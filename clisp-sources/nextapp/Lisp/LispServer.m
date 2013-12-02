#import "LispServer.h"
#import "LispText.h"
#import "Coordinator.h"
#import <mach/cthreads.h>
#import <mach/message.h>
#import <mach/port.h>
#import <sys/time.h>
#import <sys/resource.h>
#import <math.h>

port_t lispMessagePort = PORT_NULL;

@interface LispServer(Private)
- forkSubprocess:(int)argc :(char **)argv;
- noteChildExited;
@end

@implementation LispServer(Private)

extern int clisp_main(int argc, char* argv[]); // CLISP's `main'

extern int nxterminal_init(void); // Procedure to initialize connection

- forkSubprocess:(int)argc :(char **)argv
{
    // A CLISP task is started as a child process.
    // It has to be told which port to use for communication with us.
    // This is done by using the child's notify port and sending a
    // message to it that informs the child of the message port.
    // (Port numbers vary between tasks, therefore the child cannot
    // simply use the parent's variable messagePort. I have not found
    // a simpler method to achieve this. In any case, it works.)

    int pid;

    // Fork off a task
    switch(pid = vfork())
    {
      case 0: // child
      {
        port_t notifyPort;
        LispServerMsg msg;

        // Set the stack size limit to 8 MB if possible to prevent
        // crashes from machine stack overflow.
        // (If the stack is large enough, the Lisp STACK will overflow
        // first, and the error will be handled in a reasonable way.)
        { struct rlimit rl;
          long need = 0x800000; // 8 Megabyte
          getrlimit(RLIMIT_STACK, &rl);
          if (rl.rlim_max < need)
            need = rl.rlim_max;
          if (rl.rlim_cur < need)
            { rl.rlim_cur = need; setrlimit(RLIMIT_STACK,&rl); }
        }

        // Set up a notify port ...
        port_allocate(task_self(), &notifyPort);
        task_set_notify_port(task_self(), notifyPort);
        msg.h.msg_size = sizeof(LispServerMsg);
        msg.h.msg_local_port = notifyPort;
        // ... and wait for a message to transmit the LispServer's port
        // for communicating with this CLISP process
        do msg_receive((msg_header_t *)&msg, MSG_OPTION_NONE, 0);
        while(!((msg.h.msg_type == MSG_TYPE_NORMAL)
                && (msg.h.msg_id == SEND_PORT_MSG)));
        // When this was successful, remove the notify port
        task_set_notify_port(task_self(), PORT_NULL);
        port_deallocate(task_self(), (port_name_t)notifyPort);
        // Remember the communication port
        lispMessagePort = msg.h.msg_remote_port;
        if(lispMessagePort != PORT_NULL)
        {
          // Initialize the connection ...
          nxterminal_init();
          // ... and start CLISP
          clisp_main(argc, argv);
        }
        exit(0);
      }
      default: // parent
      {
        task_t child;
        port_t notifyPort = PORT_NULL;
        LispServerMsg msg;

        childPid = pid;
        // Get the child's task (port) ...
        task_by_unix_pid(task_self(), pid, &child);
        // ... and its notify port
        do task_get_notify_port(child, &notifyPort);
        while(notifyPort == PORT_NULL);
        // Set up a message to tell the child which port to use
        // for communicating with this server ...
        msg.h.msg_simple = TRUE;
        msg.h.msg_size = sizeof(LispServerMsg);
        msg.h.msg_local_port = messagePort; // this is the relevant info
        msg.h.msg_remote_port = notifyPort;
        msg.h.msg_type = MSG_TYPE_NORMAL;
        msg.h.msg_id = SEND_PORT_MSG;
        msg.t1.msg_type_name = MSG_TYPE_INTEGER_32;
        msg.t1.msg_type_size = 32;
        msg.t1.msg_type_number = 1;
        msg.t1.msg_type_inline = TRUE;
        msg.t1.msg_type_longform = FALSE;
        msg.t1.msg_type_deallocate = FALSE;
        msg.t2.msg_type_name = MSG_TYPE_CHAR;
        msg.t2.msg_type_size = 8;
        msg.t2.msg_type_number = 0;
        msg.t2.msg_type_inline = TRUE;
        msg.t2.msg_type_longform = FALSE;
        msg.t2.msg_type_deallocate = FALSE;
        // ... and send it
        msg_send((msg_header_t *)&msg, MSG_OPTION_NONE, 0);
        return self;
      }
    }
}

- noteChildExited
{
    // Do some bookkeeping and cleaning-up in case our child died
    if(childPid != 0)
    {
      childPid = 0;
      DPSRemovePort(messagePort);
      port_deallocate(task_self(), messagePort);
      port_deallocate(task_self(), exchangePort);
      [statusText setStringValue:"Process exited"];
      [stopButton setEnabled:NO];
    }
    return self;
}

@end


@implementation LispServer

static void msgHandler(LispServerMsg *msg, LispServer *self)
{
    // This function handles the different messages that CLISP may send us
    switch(msg->h.msg_id)
    {
      case WRITE_STRING_MSG:
      {
        // Insert some text into our window as Lisp output
        int len = msg->t2.msg_type_number;
        char *buffer = malloc(len*sizeof(char) + 1);
        int i;

        for(i = 0; i < len; i++) buffer[i] = msg->ch[i];
        [self->lispText writeString:buffer length:len];
        free(buffer);
        break;
      }
      case LISTEN_MSG:
      {
        // Tell CLISP if it may read some characters
        BOOL listen_p = [self->lispText listen];

        msg->h.msg_id = NOTIFY_MSG;
        msg->t2.msg_type_number = 0;
        msg->i = (int)listen_p;
        msg_send((msg_header_t *)msg, MSG_OPTION_NONE, 0);
        break;
      }
      case READ_CHAR_MSG:
      {
        // Read a character from the text the user entered, if possible
        char c = [self->lispText readChar:&msg->i];

        // Check if line length has changed
        if(self->linelength != 0)
        {
          // If so, tell CLISP about it
          LispServerMsg message = *msg;

          message.h.msg_id = CHANGE_LINELENGTH_MSG;
          message.i = self->linelength;
          self->linelength = 0;  // clear linelength
          message.t2.msg_type_number = 0;
          msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
        }

        if(c == '\0')
        {
          // Nothing ready to be read
          self->lispIsWaiting = YES;
          self->message = *msg;
          [self->statusText setStringValue:"Waiting for input"];
          [self->stopButton setEnabled:NO];
        }
        else
        {
          // Send CLISP the character
          msg->h.msg_id = NOTIFY_MSG;
          msg->t2.msg_type_number = 1;
          msg->ch[0] = c;
          msg_send((msg_header_t *)msg, MSG_OPTION_NONE, 0);
        }
        break;
      }
      case UNREAD_CHAR_MSG:
        // Back up one character
        msg->i = [self->lispText unreadChar];
        msg->h.msg_id = NOTIFY_MSG;
        msg->t2.msg_type_number = 0;
        msg_send((msg_header_t *)msg, MSG_OPTION_NONE, 0);
        break;
      case WILL_EXIT_MSG:
        [self noteChildExited];
        break;
    }
    return;
}

- initTitle:(char *)title offset:(int)off
      argCount:(int)argc argVector:(char**)argv coordinator:coord
{
    // This is the designated initializer method for LispServer objects
    [super init];

    // Set coordinator
    coordinator = coord;

    // Load window and so on
    [NXApp loadNibSection:"LispWindow.nib" owner:self withNames:NO];
    [commandScroll awake];
    [lispText awake];
    {
      // Do some inits with the window
      id window = [commandScroll window];
      NXRect theRect;

      [window getFrame:&theRect];
      [window moveTo:(NX_X(&theRect) + 20*off) :(NX_Y(&theRect) - 20*off)];
      [window setTitle:title];
      [coordinator initNewLispText:lispText];
      [window makeKeyAndOrderFront:self];
    }

    // Put a cursor at the beginning of the text
    [lispText setSel:0 :0];

    // Set up the port for communicating with CLISP
    port_allocate(task_self(), &messagePort);
    DPSAddPort(messagePort, (DPSPortProc)msgHandler, sizeof(LispServerMsg),
               self, NX_MODALRESPTHRESHOLD + 5);

    // Allocate a secondary communication port (for the completion function)
    port_allocate(task_self(), &exchangePort);

    // Start up a child process running CLISP
    [self forkSubprocess:argc :argv];
    return self;
}

- free
{
    // Do some cleaning-up
    [self killLisp:self];
    [coordinator removeMe:self];
    [[commandScroll window] close];
    return [super free];
}

- notifyLisp:sender
{
    // This method is called when the user has entered some text.
    // If CLISP is alive and waiting for input, it will get the first
    // character of the newly entered text
    task_t dummy;

    // Check if CLISP is still alive
    if(task_by_unix_pid(task_self(), childPid, &dummy) != KERN_SUCCESS)
    {
      [self noteChildExited];
      NXBeep();
    }
    if(childPid && lispIsWaiting) // alive and waiting ?
    {
      char c = [self->lispText readChar:&message.i];

      // Check if line length has changed
      if(linelength != 0)
      {
        // If so, tell CLISP about it
        message.h.msg_id = CHANGE_LINELENGTH_MSG;
        message.i = linelength;
        linelength = 0;  // clear linelength
        message.t2.msg_type_number = 0;
        msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
      }

      if(c != '\0')
      {
        // Send CLISP the first character ...
        message.h.msg_id = NOTIFY_MSG;
        message.t2.msg_type_number = 1;
        message.ch[0] = c;
        lispIsWaiting = NO;
        msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);

        // ... and reflect the change in the status line
        [statusText setStringValue:"Running"];
        [stopButton setEnabled:YES];
      }
    }
    return self;
}

- (char *)getFirstCompletion:(char *)text start:(int)s end:(int)e
                       number:(int *)n
{
    // Try to complete a symbol or function name:
    // the first e characters of text contain the string that is to be
    // completed; the name begins at an offset of s characters.
    // The number of possible completions gets stored into n
    int i, len;
    char *buffer;
    LispServerMsg msg;
    task_t dummy;

    // Check if CLISP is still alive
    if(task_by_unix_pid(task_self(), childPid, &dummy) != KERN_SUCCESS)
    {
      [self noteChildExited];
      NXBeep();
      *n = 0;
      return NULL;
    }

    // We can only do completion when CLISP is waiting for input
    // (CLISP cannot do two things at the same time!)
    if(!lispIsWaiting || e > MSG_LINE_LEN) {*n = 0; return NULL;}
    msg = message;
    msg.h.msg_local_port = exchangePort; // use the secondary port
    msg.h.msg_id = COMPLETION_MSG;
    for(i = 0; i < e; i++) msg.ch[i] = text[i];
    msg.t2.msg_type_number = e;
    msg.i = s;
    msg_rpc((msg_header_t *)&msg, MSG_OPTION_NONE, sizeof(LispServerMsg),
                                                                         0, 0);
    if((*n = msg.i) == 0) return NULL; // no completion possible
    // copy the first completion string into a buffer and return it
    len = msg.t2.msg_type_number;
    buffer = malloc((len + 1)*sizeof(char));
    for(i = 0; i < len; i++) buffer[i] = msg.ch[i];
    buffer[len] = '\0';
    return buffer;
}

- (char *)getNextCompletion:(int *)n
{
    // Get the next possible completion
    int i, len;
    LispServerMsg msg;
    char *buffer;
    task_t dummy;

    // Check if CLISP is still alive
    if(task_by_unix_pid(task_self(), childPid, &dummy) != KERN_SUCCESS)
    {
      [self noteChildExited];
      NXBeep();
      *n = 0;
      return NULL;
    }

    if(!lispIsWaiting) {*n = 0; return NULL;} // waiting ?
    // Ask CLISP for the next completion string
    msg = message;
    msg.h.msg_local_port = exchangePort;
    msg.h.msg_id = MORE_COMPLETION_MSG;
    msg.t2.msg_type_number = 0;
    msg_rpc((msg_header_t *)&msg, MSG_OPTION_NONE, sizeof(LispServerMsg),
                                                                         0, 0);
    if((*n = msg.i) == 0) return NULL;
    // copy it into a buffer and return it
    len = msg.t2.msg_type_number;
    buffer = malloc((len + 1)*sizeof(char));
    for(i = 0; i < len; i++) buffer[i] = msg.ch[i];
    buffer[len] = '\0';
    return buffer;
}

- userEntered:(char *)buffer
{
    // Insert some text simulating user typed text
    if(lispIsWaiting)
      [lispText insert:buffer];
    else
      NXBeep();
    return self;
}

- userBreak:sender
{
    // Interrupt CLISP
    if(childPid && !lispIsWaiting) kill(childPid, SIGINT);
    else NXBeep();
    return self;
}

- killLisp:sender
{
    // Kill CLISP
    if(childPid) kill(childPid, SIGKILL);
    [self noteChildExited];
    return self;
}

- coordinator
{
    return coordinator;
}

- setCoordinator:sender
{
    coordinator = sender;
    return self;
}

- lispText
{
    return lispText;
}

- setLineLength:(int)len
{
    linelength = len;
    [coordinator updateLineLength:len];
    return self;
}

- setLineLengthOfWindow:(int)len
{
    NXRect wRect, cRect, tRect;
    id window = [lispText window];

    [window getFrame:&wRect];
    [Window getContentRect:&cRect forFrameRect:&wRect style:[window style]];
    [[lispText superview] getFrame:&tRect];
    [window
      sizeWindow:ceil(cRect.size.width - tRect.size.width
                                       + len*[lispText fontWidth])
                :cRect.size.height];
    [window display];
    [self setLineLength:len];
    return self;
}


// Window Delegation

- windowDidBecomeMain:sender
{
    [coordinator makeMeCurrent:self];
    return self;
}

- windowWillMiniaturize:sender toMiniwindow:miniwindow
{
    // Make the CLISP icon appear on the miniwindow
    [sender setMiniwindowIcon:"Lisp3.tiff"];
    [coordinator removeMeFromCurrent:self];
    return self;
}

- windowDidResignMain:sender
{
    [coordinator removeMeFromCurrent:self];
    return self;
}

- windowDidResize:sender
{
    // Here CLISP has to be informed that the line length has changed.
    // Since the resizing may happen at any time, we must save
    // the information until CLISP asks for input the next time.
    int len = [lispText lispTextLineLength];

    [self setLineLength:len];
    [coordinator updateLineLength:len];

    // The linelength information will be sent to CLISP when the user
    // enters some text the next time
    return self;
}

- windowWillClose:sender
{
    // Clean up
    [self killLisp:self];
    [coordinator removeMe:self];
    [sender setDelegate:nil];
    [super free];
    return sender;
}

@end
