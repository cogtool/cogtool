#import "LispServer.h"
#import <mach/mach.h>
#import <mach/message.h>
#import <signal.h>
#import <sys/types.h>

extern port_t lispMessagePort;
static port_t localPort = PORT_NULL;
static LispServerMsg message;
static int buffer_count = 0;
static BOOL doingCompletion = NO;
int nxterminal_line_length = 80;

void nxterminal_send_output(void)
{
    /* Wenn wir etwas mit dem Stream machen wollen, während wir mit
       Eingabevervollständigung beschäftigt sind, müssen wir die zuerst
       sauber beenden.
    */
    if(doingCompletion)
    {
      message.i = 0;
      message.t2.msg_type_number = 0;
      msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
      doingCompletion = NO;
      buffer_count = 0; // muß eigentlich sowieso Null sein.
    }
    /* Jetzt schicken wir ab, was sich bisher angesammelt hat. */
    if(buffer_count > 0)
    {
      message.h.msg_local_port = localPort;
      message.h.msg_remote_port = lispMessagePort;
      message.h.msg_id = WRITE_STRING_MSG;
      message.t2.msg_type_number = buffer_count;
      msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
      buffer_count = 0;
    }
    return;
}

void nxterminal_exit(void)
{
    nxterminal_send_output();
    message.h.msg_local_port = localPort;
    message.h.msg_remote_port = lispMessagePort;
    message.h.msg_id = WILL_EXIT_MSG;
    msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
    return;
}

void nxterminal_write_char(char ch)
{
    message.ch[buffer_count++] = ch;
    if(buffer_count == MSG_LINE_LEN || ch == '\n')
      nxterminal_send_output();
    return;
}

void nxterminal_write_string(char *s)
{
    int len = strlen(s);
    int i;

    for(i = 0; i < len; i++) nxterminal_write_char(*s++);
    return;
}

extern char **lisp_completion(char *text, int start, int end);
extern void set_linelength_nextstep(int len);

char nxterminal_read_char(int *linepos)
{
    char **array = NULL;
    int array_pointer = 0;
    int array_len = 0;

    nxterminal_send_output();
    message.h.msg_local_port = localPort;
    message.h.msg_remote_port = lispMessagePort;
    message.h.msg_id = READ_CHAR_MSG;
    message.t2.msg_type_number = 0;
    msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
    do
    {
      message.h.msg_local_port = localPort;
      msg_receive((msg_header_t *)&message, MSG_OPTION_NONE, 0);
      switch(message.h.msg_id)
      {
        case COMPLETION_MSG:
        { /* Eingabevervollständigung gewünscht */
          int len = message.t2.msg_type_number;
          char *buffer = malloc((len + 1)*sizeof(char));
          int i;

          doingCompletion = YES;
          for(i = 0; i < len; i++) buffer[i] = message.ch[i];
          buffer[len] = '\0';
          // Erfrage die möglichen Fortsetzungen
          array = lisp_completion(buffer, message.i, len);
          array_len = 0;
          array_pointer = 0;
          if(array != NULL) while(array[array_len] != NULL) array_len++;
        }
        case MORE_COMPLETION_MSG:
          if(array_pointer == array_len)
          {
            message.i = 0;
            message.t2.msg_type_number = 0;
          }
          else
          {
            char *str = array[array_pointer];
            int len = strlen(str);
            int j;

            message.i = array_len - array_pointer;
            if(len > MSG_LINE_LEN) len = MSG_LINE_LEN;
            for(j = 0; j < len; j++) message.ch[j] = *str++;
            message.t2.msg_type_number = len;
            array_pointer++;
          }
          msg_send((msg_header_t *)&message, MSG_OPTION_NONE, 0);
          break;
        case CHANGE_LINELENGTH_MSG:
          nxterminal_line_length = message.i;
          kill(getpid(),SIGWINCH); // implizit update_linelength() aufrufen
          break;
        default:
        {
          int i;

          /* Hören die Vervollständigungsanfragen auf, geben wir den
             array wieder frei.
          */
          if(array != NULL)
          {
            for(i = 0; array[i] != NULL; i++) free(array[i]);
            free(array);
            array = NULL;
            array_len = 0;
            array_pointer = 0;
          }
        }
      }
    }
    while(message.h.msg_id != NOTIFY_MSG);
    doingCompletion = NO;
    *linepos = message.i;
    return message.ch[0];
}

int nxterminal_unread_char(void)
{
    nxterminal_send_output();
    message.h.msg_local_port = localPort;
    message.h.msg_remote_port = lispMessagePort;
    message.h.msg_id = UNREAD_CHAR_MSG;
    message.t2.msg_type_number = 0;
    msg_rpc((msg_header_t *)&message, MSG_OPTION_NONE, sizeof(LispServerMsg),
                                                                         0, 0);
    return message.i;
}

int nxterminal_listen(void)
{
    nxterminal_send_output();
    message.h.msg_local_port = localPort;
    message.h.msg_remote_port = lispMessagePort;
    message.h.msg_id = LISTEN_MSG;
    message.t2.msg_type_number = 0;
    msg_rpc((msg_header_t *)&message, MSG_OPTION_NONE, sizeof(LispServerMsg),
                                                                         0, 0);
    return message.i;
}

int nxterminal_init(void)
{
    if(localPort == PORT_NULL)
    {
      port_allocate(task_self(), &localPort);
      message.h.msg_simple = TRUE;
      message.h.msg_size = sizeof(LispServerMsg);
      message.h.msg_local_port = localPort;
      message.h.msg_remote_port = lispMessagePort;
      message.h.msg_type = MSG_TYPE_NORMAL;
      message.t1.msg_type_name = MSG_TYPE_INTEGER_32;
      message.t1.msg_type_size = 32;
      message.t1.msg_type_number = 1;
      message.t1.msg_type_inline = TRUE;        // nur ein Integer
      message.t1.msg_type_longform = FALSE;
      message.t1.msg_type_deallocate = FALSE;
      message.t2.msg_type_name = MSG_TYPE_CHAR;
      message.t2.msg_type_size = 8;
      message.t2.msg_type_number = 0; // vorläufig
      message.t2.msg_type_inline = TRUE;
      message.t2.msg_type_longform = FALSE;
      message.t2.msg_type_deallocate = FALSE;
    }
    return 0;
}

