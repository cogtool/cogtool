#import <appkit/Text.h>

typedef struct {int beg; int end;} interval;

@interface LispText:Text
{
    int userTextStart;         // Beginning of user (= editable) text
    id  machineFont, userFont; // fonts for use in text
    float fontWidth;           // the width of an 'm' in machineFont

    interval *hist;            // array containing history list
    int histLength;            // length of array hist[]
    int histIndex;             // pointer to current entry in history
    int histPtr;               // pointer to displayed entry in history
    BOOL parenTestFlag;        // flag if sytax check is to be performed
    NXColor machineColor, userColor, highlightColor, enteredColor;
                               // colors used in the text

    int currentIndex;      // pointer into text for reading and writing
    id server;             // the LispServer object managing the LispText
    id completionPanel;    // the panel showing possible completions
    id completionBrowser;  // the browser contained in this panel

    char **array;          // an array to store possible completions
    int array_len;         // its length
    int completionStart;   // pointers into the text for completion
    int completionEnd;
}

- setServer:theServer;
- setMachineFont:font;
- setUserFont:font;
- setMachineColor:sender;
- setUserColor:sender;
- setHighlightColor:sender;
- setEnteredColor:sender;
- machineFont;
- userFont;
- (int)lispTextLineLength;
- (float)fontWidth;
- (NXColor)machineColor;
- (NXColor)userColor;
- (NXColor)highlightColor;
- (NXColor)enteredColor;

- insert:(char *)buffer;
- (int)browser:sender fillMatrix:matrix inColumn:(int)column;
- cancelCompletion:sender;
- completionSelected:sender;
- doNothing:sender;

- (void)writeString:(char *)s length:(int)len;
- (char)readChar:(int *)linepos;
- (int)unreadChar;
- (BOOL)listen;

@end
