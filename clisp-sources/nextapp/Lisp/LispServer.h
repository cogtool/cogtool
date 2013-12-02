#import <appkit/appkit.h>
#import <mach/message.h>

#define MSG_LINE_LEN 80

// Type definition for the messages we use
typedef struct {
    msg_header_t h;
    msg_type_t t1;
    int i;                 // may contain an integer
    msg_type_t t2;
    char ch[MSG_LINE_LEN]; // and up to MSG_LINE_LEN (= 80) characters
} LispServerMsg;

// Possible kinds of messages between a LispServer and a CLISP process
#define WRITE_STRING_MSG 1
#define READ_CHAR_MSG 2
#define LISTEN_MSG 3
#define COMPLETION_MSG 4
#define MORE_COMPLETION_MSG 5
#define UNREAD_CHAR_MSG 6
#define WILL_EXIT_MSG 7
#define CHANGE_LINELENGTH_MSG 8
#define NOTIFY_MSG 10
#define SEND_PORT_MSG 99

@interface LispServer:Object
{
    id commandScroll;      // the ScrollView containing the LispText
    id lispText;           // the Text object for input to/output from Lisp
    id statusText;         // the TextField containing the status line
    id stopButton;         // the "STOP" button in the lower right corner
    id coordinator;        // the Coordinator object of Lisp.app
    LispServerMsg message; // saves the message while CLISP waits for input
    BOOL lispIsWaiting;    // flag indicating if CLISP is running or
                           //  waiting for input
    int childPid;          // process id of child process (running CLISP)
    port_t messagePort;    // main communication port  <--> CLISP
    port_t exchangePort;   // secondary communication port <--> CLISP,
                           // used for the completion function
    int linelength;        // if != 0: linelength to be set for CLISP at
                           // the next input request
}

- initTitle:(char *)title offset:(int)off
             argCount:(int)argc argVector:(char**)argv coordinator:coord;
- notifyLisp:sender;
- (char *)getFirstCompletion:(char *)text start:(int)s end:(int)e
                      number:(int *)n;
- (char *)getNextCompletion:(int *)n;
- killLisp:sender;
- userBreak:sender;
- coordinator;
- setCoordinator:sender;
- lispText;
- setLineLength:(int)len;
- setLineLengthOfWindow:(int)len;

// Window Delegation

- windowDidBecomeMain:sender;
- windowWillMiniaturize:sender toMiniwindow:miniwindow;
- windowDidResignMain:sender;
- windowDidResize:sender;
- windowWillClose:sender;

@end
