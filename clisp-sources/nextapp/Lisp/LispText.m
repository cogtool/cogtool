#import "LispText.h"
#import "CommandScroll.h"
#import "LispServer.h"
#import "Coordinator.h"
#import <math.h>

@interface LispText(Private)
- parenTest;
- (int)getOpeningParen:(int)end offset:(int*)offset;
@end

@implementation LispText(Private)

#define MAXPAREN 100

// parenTest checks the syntax of the text entered so far and highlights
// the parts of text belonging to some not-yet-closed bracketing structure.
// The following structures are checked:
// ( ... ), "...", |...|, ; ... \n, \. .
// If parenTestFlag == NO, the syntax check is skipped.
- parenTest
{
    if(parenTestFlag)
    {
      int length = [self textLength];
      int close = -1;               // position of first stray closing paren
                                    //  or -1, if there is none
      int open[MAXPAREN];           // array of positions of open parens that
                                    //  have not yet been closed
      interval intervals[MAXPAREN]; // array of correctly bracketed portions
      int oindex = 0;               // counts entries in open[]
      int iindex = 0;               // counts entries in intervals[]
      int delta = length - userTextStart;
      char *buffer = malloc(delta);
      int i;
      NXSelPt selStart;
      NXSelPt selEnd;

      // add_open(pos) remembers an opening paren at position pos
      #define add_open(pos) \
        if(oindex >= MAXPAREN-1) return self; \
        open[oindex++] = (pos);

      // add_interval(begin, end) remembers a bracketed portion of text
      // extending from begin (incl.) up to end (excl.)
      #define add_interval(b,e) \
         if(iindex >= MAXPAREN-1) return self; \
         intervals[iindex].beg = (b); \
         intervals[iindex++].end = (e);

      // Try to change the colors invisibly
      [self setAutodisplay:NO];

      // Remember selection
      [self getSel:&selStart :&selEnd];

      // Copy user's text into buffer
      [self getSubstring:buffer start:userTextStart length:delta];

      // Do the checking
      for(i = 0; i < delta; i++)
      {
        switch(buffer[i])
        {
          case '(':
          {
            add_open(i)  // note an opening paren
            break;
          }
          case ')':
          {
            if(oindex == 0)
            {
              close = i; // no corresponding opening '(' --> stray closing ')'
              goto weiter;
            }
            else
            {
              int pos = open[--oindex]; // look for corresponding opening paren
              while(iindex > 0 && intervals[iindex-1].beg > pos)
                iindex--;               // remove enclosed intervals
              add_interval(pos,i+1)     // and note the interval (...)
            }
            break;
          }
          case ';':
          {
            int j = i;
            while(++j < delta && buffer[j] != '\n'); // skip comment
            add_interval(i,j)
            i = j;
            break;
          }
          case '\\':
            if(++i == delta) {add_open(i-1)} // skip character after '\'
            break;
          case '|':
          case '"':
          {
            int j;
            // skip text up to matching '"' or '|' (or the end)
            for(j = i+1 ; j < delta && buffer[j] != buffer[i]; j++)
              if(buffer[j] == '\\') j++; // pay attention to '\' in string
            if(j >= delta) {add_open(i)}
            else {add_interval(i,j+1)}
            i = j;
            break;
          }
        }
      }
    weiter:
      free(buffer);

      // Highlight `open' text portions
      if(oindex == 0)
        // no open structures left
        if(close < 0)
        {
          // no stray opening paren:
          // no highlighting
          [self setSel:userTextStart :length];
          [self setSelColor:userColor];

          // restore selection
          [self setSel:selStart.cp :selEnd.cp];
        }
        else
        {
          // there is a stray opening paren:
          // highlight it and the text that follows
          [self setSel:userTextStart :userTextStart + close];
          [self setSelColor:userColor];
          [self setSel:userTextStart + close :length];
          [self setSelColor:highlightColor];

          // restore selection
          if(selStart.cp != userTextStart + close || selEnd.cp != length)
            [self setSel:selStart.cp :selEnd.cp];
        }
      else
      {
        // there are non-closed structures:
        // highlight text in non-closed structures, but exclude imbedded
        // closed structures
        int first = open[0];
        int i;

        [self setSel:userTextStart :userTextStart + first];
        [self setSelColor:userColor];
        for(i = 0; i < iindex; i++)
        {
          [self setSel:userTextStart + first
                      :userTextStart + intervals[i].beg];
          [self setSelColor:highlightColor];
          first = intervals[i].end;
          [self setSel:userTextStart + intervals[i].beg
                      :userTextStart + first];
          [self setSelColor:userColor];
        }

        // highlight remaining text
        [self setSel:userTextStart + first :length];
        [self setSelColor:highlightColor];

        // restore selection
        if(selStart.cp != userTextStart + first || selEnd.cp != length)
          [self setSel:selStart.cp :selEnd.cp];
      }

      // Now display the changes
      [self setAutodisplay:YES];
      [self display];
    }
    return self;
    #undef add_open
    #undef add_interval
}

// getOpeningParen:end looks for the position of the last opening paren '('
// without matching closing paren ')' before position end.
// If none is found, -1 is returned if position end
// is in a string "..." or escaped |...| or after a '\'.
// Otherwise, 0 is returned. This means that the text up to pos. end
// constitutes a sytactically valid expression.

- (int)getOpeningParen:(int)end offset:(int*)offset
{
    // cf. parenTest
    int open[MAXPAREN];
    int oindex = 0;
    int delta = end - userTextStart;
    char *buffer = malloc(delta);
    int element_count[MAXPAREN];      // counts list elements at all levels
    int last_element_pos[MAXPAREN];   // position of last relevant element
    short int data_flag[MAXPAREN];    // flag if list is probably data (1)
                                      //  or a function call (2)
    BOOL in_whitespace = NO;          // flag indicating if scanning whitespace
    BOOL newline_seen = NO;           // next element will be first in line
    int i;

#define check_element \
          if(in_whitespace) \
          {                 \
            in_whitespace = NO;  \
            if(oindex > 0)       \
            {                    \
              element_count[oindex-1]++;      \
              if(element_count[oindex-1] <= data_flag[oindex-1] \
                 || newline_seen) \
                last_element_pos[oindex-1] = i; \
            }                  \
            newline_seen = NO; \
          }

    [self getSubstring:buffer start:userTextStart length:delta];
    for(i = 0; i < delta; i++)
    {
      switch(buffer[i])
      {
        case '(':
          if(oindex >= MAXPAREN-1) return -1;
          in_whitespace = YES;
          if(oindex > 0 && element_count[oindex-1] == 0)
            data_flag[oindex-1] = 1;
          check_element
          open[oindex] = i;
          element_count[oindex] = 0;
          if(i > 0 && (buffer[i-1] == '#'
                       || (buffer[i-1] == '\''
                           && !(i > 1 && buffer[i-2] == '#'))))
            data_flag[oindex] = 1;
          else
            data_flag[oindex] = 2;
          oindex++;
          in_whitespace = YES;
          break;
        case ')':
          if(oindex == 0) return -1;
          oindex--;
          break;
        case ';':
          in_whitespace = YES;
          i++;
          while(i < delta && buffer[i] != '\n') i++;
          break;
        case '\\':
          check_element;
          i++;
          if(i >= delta) return -1;
          break;
        case '"':
          in_whitespace = NO;
        case '|':
        {
          int j;

          check_element
          for(j = i+1 ; j < delta && buffer[j] != buffer[i]; j++)
            if(buffer[j] == '\\') j++;
          if(j >= delta) return -1;
          i = j;
          break;
        }
        case '\n':
          newline_seen = YES;
        case ' ':
        case '\t':
          in_whitespace = YES;
          break;
        case '\'':
          in_whitespace = YES;
          check_element
          break;
        default:
          check_element
          break;
      }
    }
    if(oindex == 0)
    {
      *offset = userTextStart
               - [self positionFromLine:[self lineFromPosition:userTextStart]];
      return 0;
    }
    else
    {
      int open_pos = open[oindex-1];
      int text_open_pos = open_pos + userTextStart;
      int base_offset
           = text_open_pos
             - [self positionFromLine:[self lineFromPosition:text_open_pos]];

      switch(element_count[oindex-1])
      {
        case 0: // no elements in this level so far :
                //  take column after opening paren
          *offset = base_offset + 1;
          break;
        case 1: // one element: Try to figure out if it is a function call
                //  or not.
          *offset = base_offset + data_flag[oindex-1];
          break;
        default: // at least two: indent according to last element coming
                 //  first on a line
        {
          int pos = last_element_pos[oindex-1] + userTextStart;

          *offset = pos - [self positionFromLine:[self lineFromPosition:pos]];
          break;
        }
      }
      return text_open_pos;
    }
}

@end

@implementation LispText

- awake
{
    // Do some bookkeeping
    [self notifyAncestorWhenFrameChanged:YES];
    [self setHorizResizable:NO];
    [self setVertResizable:YES];
    [[self superview] setAutosizing: NX_HEIGHTSIZABLE | NX_WIDTHSIZABLE];
    [self setAutosizing:NX_WIDTHSIZABLE];
    [[self superview] setAutoresizeSubviews:YES];
    [self setMonoFont:FALSE];
    [self setOpaque:YES];
    [self setFontPanelEnabled:NO];

    // Set preliminary fonts and colors -- they will be set again
    // immediately by the coordinator
    machineFont = [Font newFont:"Ohlfs" size:12];
    userFont = [Font newFont:"Ohlfs" size:12];
    [self setFont:userFont];
    userColor = machineColor = NX_COLORBLACK;
    highlightColor = NX_COLORRED;
    enteredColor = NX_COLORBLUE;

    // Turn on syntax check by default
    parenTestFlag = YES;

    // Initialize instance variables
    histLength = 500;
    hist = malloc(histLength*sizeof(interval));
    hist[0].beg = hist[0].end = 0;
    histIndex = 0;
    currentIndex = 0;
    userTextStart = 0;
    array = NULL;
    array_len = 0;

    // Tell browser the action for double-clicking
    [completionBrowser setDoubleAction:@selector(completionSelected:)];

    return self;
}

- setServer:theServer
{
    server = theServer;
    return self;
}

- setMachineFont:font
{
    NXSelPt selStart, selEnd;
    id screenFont = [font screenFont];
    int currentLineLength
        = (fontWidth == 0.0) ? [[server coordinator] defaultLineLength]
                             : [self lispTextLineLength];
    int i, index = 0;

    machineFont = font;

    // Calculate new font width
    fontWidth = (screenFont) ? [[machineFont screenFont] getWidthOf:"m"]
                             : [machineFont getWidthOf:"m"];

    // Remember selection
    [self getSel:&selStart :&selEnd];

    // Change font of machine output produced so far
    [self setAutodisplay:NO];
    for(i = 0; i <= histIndex; i++)
    {
      [self setSel:index :hist[i].beg];
      [self setSelFont:machineFont];
      index = hist[i].end;
    }

    // restore selection
    [self setSel:selStart.cp :selEnd.cp];

    // Change window width in order to keep line length
    [server setLineLengthOfWindow:currentLineLength];

    // Now display the changes
    [self setAutodisplay:YES];
    [self display];

    return self;
}

- setUserFont:font
{
    NXSelPt selStart, selEnd;
    int i;

    userFont = font;

    // Try to change the font invisibly
    [self setAutodisplay:NO];

    // Remember selection
    [self getSel:&selStart :&selEnd];

    // Change font of previously entered text
    for(i = 0; i < histIndex; i++)
    {
      [self setSel:hist[i].beg :hist[i].end];
      [self setSelFont:userFont];
    }

    // Change font of current user text
    [self setSel:userTextStart :[self textLength]];
    [self setSelFont:userFont];

    // restore selection
    [self setSel:selStart.cp :selEnd.cp];

    // Now display the changes
    [self setAutodisplay:YES];
    [self display];

    return self;
}

- machineFont
{
    return machineFont;
}

- userFont
{
    return userFont;
}

- (int)lispTextLineLength
{
    return floor(bounds.size.width / fontWidth);
}

- (float)fontWidth
{
    return fontWidth;
}

- setMachineColor:sender
{
    machineColor = [sender color];
    return self;
}

- setUserColor:sender
{
    userColor = [sender color];
    return self;
}

- setHighlightColor:sender
{
    highlightColor = [sender color];
    return self;
}

- setEnteredColor:sender
{
    enteredColor = [sender color];
    return self;
}

- (NXColor)machineColor
{
    return machineColor;
}

- (NXColor)userColor
{
    return userColor;
}

- (NXColor)highlightColor
{
    return highlightColor;
}

- (NXColor)enteredColor
{
    return enteredColor;
}

// Override the Text class's keyDown method for our purposes
- keyDown:(NXEvent *)theEvent
{
    int flags = theEvent->flags;
    unsigned short charSet = theEvent->data.key.charSet;
    unsigned short charCode = theEvent->data.key.charCode;
    int shift = flags & NX_SHIFTMASK;
    int control = flags & NX_CONTROLMASK;
    int length = [self textLength];
    NXSelPt selStart;
    NXSelPt selEnd;

    // Hide the Completion Panel -- if user types on, she is not interested
    // in the completions
    [completionPanel close];

    // get current selection
    [self getSel:&selStart :&selEnd];

    if(selStart.cp == selEnd.cp && selStart.cp == userTextStart)
    {
      [self setSelColor:userColor];
      [self setSelFont:userFont];
    }

    // We don't want to modify text that has been entered already or was
    // output by CLISP -- change the selection if necessary
    if(selEnd.cp < userTextStart)
    {
      [self setSel:length :length];
      [self getSel:&selStart :&selEnd];
    }
    else if(selStart.cp < userTextStart)
    {
      [self setSel:userTextStart :selEnd.cp];
      [self getSel:&selStart :&selEnd];
    }

    // Now do something depending on which key was pressed
    switch(charSet)
    {
      case 0: // `normal' keys
        if(control)
          // control + key
          switch(charCode)
          {
            case 1: // ^A : move cursor to beginning of line
              goto begLine;
            case 2: // ^B : turn off syntax check
              parenTestFlag = NO;
              [self setAutodisplay:NO];
              [self setSel:userTextStart :length];
              [self setSelColor:userColor];
              [self setSel:selStart.cp :selEnd.cp];
              [self setAutodisplay:YES];
              [self display];
              break;
            case 3: // ^C : interrupt CLISP
              [server userBreak:self];
              break;
            case 4: // ^D : delete selection/character right of cursor
              goto delete;
            case 5: // ^E : move cursor to end of line
              goto endLine;
            case 20: // ^T : turn on syntax check
              parenTestFlag = YES;
              [self parenTest];
              break;
            case 21: // ^U : delete line
              [self setAutodisplay:NO];
              [self setSel:userTextStart :length];
              [self replaceSel:""];
              [self setSel:[self textLength] :[self textLength]];
              [self setAutodisplay:YES];
              [self display];
              break;
          }
        else
          switch(charCode)
          {
            case 3: // Enter : make text available to CLISP
              goto enter;
            case 9: // Tab : completion of symbol or function names
            {
              int len = selStart.cp - selStart.c1st;
                 // length from beginning of line to cursor
              char *buffer = malloc((len + 1)*sizeof(char));
              char *replacement;
              int i, start, number;

              // Copy the beginning of the line up to the cursor into buffer
              [self getSubstring:buffer start:selStart.c1st length:len];
              buffer[len] = '\0';

              // Find where the name that has to be completed begins
              start = len;
              for(i = len-1; i >= 0; i--)
                switch(buffer[i])
                {
                  case ' ':
                  case '(':
                  case '\'': // quote
                    start = i+1;
                    i = 0;
                    break;
                  case '"':
                  case ';':
                    NXBeep(); // no completion in strings or comments
                    goto end;
                }
              if(start == len) {NXBeep(); goto end;} // no name to complete

              // Ask Lisp for the first completion. If something != NULL
              // is returned, it is the longest common beginning of all
              // possible completions
              replacement = [server getFirstCompletion:buffer start:start
                                    end:len number:&number];
              free(buffer);
              if(replacement == NULL) {NXBeep(); goto end;} // no completion

              // Replace beginning of name by common beginning of completions
              start += selStart.c1st;
              completionStart = start;
              completionEnd = selStart.cp;
              [self setAutodisplay:NO];
              [self setSel:completionStart :completionEnd];
              len = strlen(replacement);
              [self replaceSel:replacement];
              completionEnd = completionStart + len;
              free(replacement);

              // number contains the number of possible completions plus one
              // (for the first completion which is the common beginning)
              if(number <= 2)
              {
                // Only one completion: add a space, set cursor after it
                [self replaceSel:" "];
                completionEnd++;
                [self setSel:completionEnd :completionEnd];
                [self setAutodisplay:YES];
                [self display];
                goto end;
              }

              // Several possible completions:
              // get them and store them into array
              {
                int i, n;

                // Free current array contents
                if(array != NULL)
                {
                  for(i = 0; i < array_len; i++) free(array[i]);
                  free(array);
                }

                // Subtract one from number to get the number of completions
                number--;

                // Fill array with all possible completions
                array = malloc(number*sizeof(char*));
                for(i = 0; i < number; i++)
                {
                  array[i] = [server getNextCompletion:&n];
                  if(n == 0) {number = i; break;}
                }
              }
              [self setSel:completionEnd :completionEnd];
              [self setAutodisplay:YES];
              [self display];
              array_len = number;

              // Put completion in browser and show browser
              [completionBrowser loadColumnZero];
              [completionPanel orderFront:self];
              goto end;
            }
            case 13: // Return
            {
              int dummy;

              if(shift || selStart.cp < length
                       || [self getOpeningParen:length offset:&dummy] != 0)
                goto newLine;
              else goto enter;
            }
            case '(':
            case ')':
            case '"':
            case ';':
            case '|':
            case '\\':
              [super keyDown:theEvent];
              [self parenTest];
              break;
            case '\177': // Backspace
              goto backspace;
            default:
            {
              char buffer[1] = " ";
              // Remember character left of cursor
              if(selStart.cp > userTextStart)
                [self getSubstring:buffer start:(selStart.cp - 1) length:1];

              // Use Text class keyDown method for inserting character
              [super keyDown:theEvent];

              // If character left of cursor was '\' or selection was not
              // empty, do a syntax check
              if(selStart.cp < selEnd.cp || buffer[0] == '\\')
                [self parenTest];
              break;
            }
          }
        break;
      case 1: // cursor keys
        switch(charCode)
        {
          case 0xAC: // Left
            if(shift)
              goto begLine; // Shift-Left : move cursor to beginnning of line
            else if(selStart.cp == selEnd.cp && selStart.cp == userTextStart)
              goto end; // at start of user text: do nothing
            else
              break;
          case 0xAD: // Up
            if(shift)
              goto histUp; // Shift-Up : move one item up in history
            else if(selStart.c1st <= userTextStart)
              goto end; // in first line of user text: do nothing
            else
              break;
          case 0xAE: // Right
            if(shift)
              goto endLine; // Shift-Right : move cursor to end of line
            else if(selStart.cp == selEnd.cp && selEnd.cp == length)
              goto end; // at end of user text: do nothing
            else
              break;
          case 0xAF: // Down
            if(shift) // Shift-Down : move down one item in history
              goto histDown;
            else
              break;
          default: goto end; // no other cursor keys are recognised
        }
        // Use usual cursor movement of Text class
        [super keyDown:theEvent];
        break;
      case 0xFE: // function keys
        switch(charCode)
        {
          case 0x2D: // Del
            goto delete;
          case 0x2E: // Home
            [self setSel:userTextStart :userTextStart];
            break;
          case 0x2F: // End
            [self setSel:length :length];
            break;
          case 0x30: // PgUp
            goto histUp;
          case 0x31: // PgDown
            goto histDown;
          default: break;
        }
        break;
    }
    goto end;

    // Here are some editing functions that may be triggered by more than
    // one key:
  enter: // Make text available to Lisp
  {
    int delta = length - userTextStart;

    if(delta > 0) // Only do something when the text is not empty
    {
      // Put text into history
      hist[histIndex].beg = userTextStart;
      hist[histIndex].end = length;

      // Add a newline
      [self setAutodisplay:NO];
      [self setSel:length :length];
      [self replaceSel:"\n" length:1];
      length++;

      // Change color to enteredColor
      [self setSel:userTextStart :length];
      [self setSelColor:enteredColor];
      [self setSel:length :length];
      [self setAutodisplay:YES];
      [self display];

      // History bookkeeping
      if(++histIndex >= histLength)
      {
        hist = realloc(hist, 2*histLength);
        histLength *= 2;
      }

      // Initialize current history entry to be empty
      hist[histIndex].beg = length;
      hist[histIndex].end = length;

      // Set pointer into history list to current entry
      histPtr = histIndex;

      // User text is empty and at end of text
      userTextStart = length;

      // Tell Lisp it may read characters now
      [server notifyLisp:self];
    }
    goto end;
  }
  newLine: // Insert a newline character and indent reasonably
  {
    int offset;
    int opening = [self getOpeningParen:selStart.cp offset:&offset];
      // get opening paren of current structure level and indentation

    if(opening >= 0)
    {
      char *buffer;
      int i;

      // Fill a buffer with a newline and enough spaces
      buffer = malloc((offset + 1)*sizeof(char));
      buffer[0] = '\n';
      for(i = 1; i <= offset; i++) buffer[i] = ' ';

      // and insert it
      [self replaceSel:buffer length:(offset + 1)];
      free(buffer);
      selStart.cp += offset + 1;
    }
    else
    {
      // In string or so: don't insert any spaces, only a newline
      char buffer[2] = "\n";

      [self replaceSel:buffer length:1];
      selStart.cp++;
    }
    [self setSel:selStart.cp :selStart.cp];
    [self parenTest];
    goto end;
  }
  begLine: // Move cursor to beginning of line
    {
      int pos = selStart.c1st;
      pos = (pos < userTextStart) ? userTextStart : pos;
      [self setSel:pos :pos];
      goto end;
    }
  endLine: // Move cursor to end of line
    {
      int lastLine = [self lineFromPosition:length];
        // position of first character of last line
      int line = [self lineFromPosition:selEnd.cp];
        // position of first character of current line
      int pos;

      if(line == lastLine)
        pos = length;
      else
        // New position = one char. before first char of next line
        pos = [self positionFromLine:(line + 1)] - 1;
      [self setSel:pos :pos];
      goto end;
    }
  backspace: // Delete selection or character left of cursor
    if(selStart.cp < selEnd.cp)
    {
      // Selection non-empty: delete it
      [self replaceSel:""];
      [self parenTest];
      goto end;
    }
    else if(selStart.cp > userTextStart)
    {
      char buffer[1];          // for character left of cursor
      char prebuffer[1] = " "; // for character before that

      [self getSubstring:buffer start:(selStart.cp - 1) length:1];
      if(selStart.cp > userTextStart + 1)
        [self getSubstring:prebuffer start:(selStart.cp - 2) length:1];

      // Use standard Text class method to delete the character
      [super keyDown:theEvent];

      // Determine if syntax check might be necessary
      switch(buffer[0])
      {
        case '(':
        case ')':
        case '"':
        case ';':
        case '\n':
        case '|':
        case '\\':
          [self parenTest];
          break;
        default:
          if(prebuffer[0] == '\\') [self parenTest];
          break;
      }
      goto end;
    }
    else
      goto end; // Do nothing when at beginning of user text
  delete: // Delete selection or character right of cursor
    if(selStart.cp < selEnd.cp)
    {
      // Selection non-empty: delete it
      [self replaceSel:""];
      [self parenTest];
    }
    else if(selStart.cp < length)
    {
      char buffer[1];          // for character right of cursor
      char prebuffer[1] = " "; // for character before that

      [self getSubstring:buffer start:selStart.cp length:1];
      if(selStart.cp > userTextStart)
        [self getSubstring:prebuffer start:(selStart.cp - 1) length:1];

      // Delete character
      [self setSel:selStart.cp :selStart.cp+1];
      [self replaceSel:""];

      // Determine if a syntax check might be necessary
      switch(buffer[0])
      {
        case '(':
        case ')':
        case '"':
        case ';':
        case '\n':
        case '|':
        case '\\':
          [self parenTest];
          break;
        default:
          if(prebuffer[0] == '\\')
            [self parenTest];
          break;
      }
    }
    [self setSel:selStart.cp :selStart.cp];
    goto end;
  histUp: // Move up one item in history
    if(--histPtr < 0) histPtr = 0;
    goto histUpDown;
  histDown: // Move down one item in history
    if(++histPtr > histIndex) histPtr = histIndex;
  histUpDown:
    {
      int beg = hist[histPtr].beg;
      int len = hist[histPtr].end - beg;
      char *buffer = malloc(len);

      // Get text referred to by history entry
      [self getSubstring:buffer start:beg length:len];

      // and replace user text by it
      [self setAutodisplay:NO];
      [self setSel:userTextStart :length];
      [self setSelColor:userColor];
      [self setSelFont:userFont];
      [self replaceSel:buffer length:len];
      free(buffer);
      length = [self textLength];
      [self setSel:length :length];
      [self setAutodisplay:YES];
      [self display];
      goto end;
    }
  end:
    [self scrollSelToVisible];
    return self;
}

// Override cut/paste/delete to prohibit changes to non-editable text
- cut:sender
{
    [self copy:sender];
    return [self delete:sender];
}

- delete:sender
{
    NXSelPt selStart;
    NXSelPt selEnd;

    [self getSel:&selStart :&selEnd];
    // Adjust selection
    if(selEnd.cp < userTextStart)
      [self setSel:[self textLength] :[self textLength]];
    else if(selStart.cp < userTextStart)
      [self setSel:userTextStart :selEnd.cp];
    [super delete:sender];
    [self parenTest];
    return self;
}

- paste:sender;
{
    NXSelPt selStart;
    NXSelPt selEnd;

    // Adjust selection
    [self getSel:&selStart :&selEnd];
    if(selEnd.cp < userTextStart)
      [self setSel:[self textLength] :[self textLength]];
    else if(selStart.cp < userTextStart)
      [self setSel:userTextStart :selEnd.cp];
    [super paste:sender];
    [self parenTest];
    return self;
}

- insert:(char *)buffer
{
    int len = strlen(buffer);
    NXSelPt selStart;
    NXSelPt selEnd;

    // Insert some text and simulate the user's having typed it in
    [self getSel:&selStart :&selEnd];

    // Insert at the point where Lisp will continue reading
    [self setAutodisplay:NO];
    [self setSel:currentIndex :currentIndex];
    [self setSelColor:enteredColor];
    [self setSelFont:userFont];
    [self replaceSel:buffer length:len];
    [self setAutodisplay:YES];
    [self display];
    [self scrollSelToVisible];

    // Update selection if necessary
    {
      int sel0 = selStart.cp;
      int selN = selEnd.cp;

      if(sel0 >= currentIndex) sel0 += len;
      if(selN >= currentIndex) selN += len;
      [self setSel:sel0 :selN];
    }

    // User text moves
    userTextStart += len;

    // Lisp may read now
    [server notifyLisp:self];
    return self;
}

// Delegate method for Completion Browser
- (int)browser:sender fillMatrix:matrix inColumn:(int)column
{
    int i;
    int count = [matrix cellCount];

    // Adjust size of matrix in browser
    if(count > array_len)
      for(i = array_len; i < count; i++)
        [matrix removeRowAt:array_len andFree:YES];
    for(i = count; i < array_len; i++) [matrix addRow];

    // Fill matrix with completion strings stored in array
    for(i = 0; i < array_len; i++)
    {
      id cell = [matrix cellAt:i :0];

      [cell setStringValue:array[i]];
      [cell setEnabled:YES];
      [cell setLeaf:YES];
      [cell setLoaded:YES];
    }
    return array_len;
}

- cancelCompletion:sender
{
    [completionPanel close];
    return self;
}

- completionSelected:sender
{
    // Insert selected completion into text
    id cell = [completionBrowser selectedCell];
    const char *replacement;
    int len;

    [completionPanel close];
    if(cell)
    {
      replacement = [cell stringValue];
      len = strlen(replacement);
      [self setSel:completionStart :completionEnd];
      [self replaceSel:replacement];
      completionEnd = completionStart + len;
      [self setSel:completionEnd :completionEnd];
      [self replaceSel:" "];
      completionEnd++;
      [self setSel:completionEnd :completionEnd];
      [self scrollSelToVisible];
    }
    return self;
}

- doNothing:sender
{
    return self;
}

// Methods for communicating with the Lisp process
- (void)writeString:(char *)s length:(int)len
{
    // Write a string as Lisp output into the text
    NXSelPt selStart;
    NXSelPt selEnd;
    char buffer[1];
    int len1 = len;
    int i;
    int nlcount = 0;
    int skipNlcount = 0;

    if(len > 0)
    {
      // Count newlines in insertion string
      for(i = 0; i < len; i++) if(s[i] == '\n') nlcount++;

      // Count newlines at position for insertion
      for(i = currentIndex; i < userTextStart && skipNlcount < nlcount; i++)
      {
        [self getSubstring:buffer start:i length:1];
        if(buffer[0] == '\n') skipNlcount++;
        else break;
      }
      len1 -= skipNlcount;

      // Save selection
      [self getSel:&selStart :&selEnd];

      // Replace skipNlcount newlines by insertion string
      // [self setAutodisplay:NO];
      [self setSel:currentIndex :(currentIndex + skipNlcount)];
      [self setSelColor:machineColor];
      [self setSelFont:machineFont];
      [self replaceSel:s length:len];

      {
        int sel0 = selStart.cp;
        int selN = selEnd.cp;

        if(currentIndex > 0)
        {
          // Update history references after insertion point
          for(i = histIndex; i >= 0 && hist[i].beg >= currentIndex; i--)
          {
            hist[i].beg += len1; hist[i].end += len1;
          }

          // Split history entry if it contains insertion point
          if(i >= 0 && hist[i].end > currentIndex)
          {
            int l = currentIndex - hist[i].beg;
            char *b = malloc(l*sizeof(char));
            BOOL flag = YES; // entry consisting of whitespace ?
            int i1;

            // Get first part of entry
            [self getSubstring:b start:hist[i].beg length:l];

            // Strip whitespace
            for(i1 = 0; i1 < l; i1++)
              if(!(b[i1] == ' ' || b[i1] == '\n'))
                {
                  flag = NO; // contains non-whitespace characters
                  break;
                }

            if(flag)
            {
              // If first part is empty or entry is the current one,
              // move entry after insertion
              hist[i].beg = currentIndex + len;
              hist[i].end += len1;
            }
            else
            {
              // Otherwise, split entry in two
              int j;

              if(histIndex + 1 == histLength)
              {
                hist = realloc(hist, 2*histLength);
                histLength *= 2;
              }
              for(j = histIndex; j > i; j--) hist[j+1] = hist[j];
              j = i + 1;
              hist[j].end = hist[i].end + len1;
              hist[j].beg = currentIndex + len;
              hist[i].end = currentIndex;
              [self getSubstring:buffer start:(currentIndex - 1) length:1];
              if(buffer[0] == '\n') hist[i].end--;
            }
            histIndex++;
            histPtr = histIndex;
          }
        }

        // Update selection and pointers into text
        if(sel0 >= currentIndex) sel0 += len1;
        if(selN >= currentIndex) selN += len1;
        currentIndex += len;
        userTextStart += len1;
        [self setSel:currentIndex :currentIndex];
        [self setSelFont:userFont];
        [self setSelColor:userColor];
        // [self setAutodisplay:YES];
        // [self display];
        [self scrollSelToVisible];
        [self setSel:sel0 :selN];
      }
    }
    return;
}

- (char)readChar:(int *)linepos
{
    // Read a character from the text
    char buffer[1];
    int linebeg;

    if(currentIndex == userTextStart) return '\0'; // EOF

    // Get the character at currentIndex
    [self getSubstring:buffer start:currentIndex length:1];
    currentIndex++;

    // Calculate its position in int line
    linebeg = [self positionFromLine:[self lineFromPosition:currentIndex]];
    *linepos = currentIndex - linebeg;
    return buffer[0];
}

- (int)unreadChar
{
    // `Put a character back', i.e. decrement currentIndex
    if(currentIndex > 0)
    {
      currentIndex--;

      // Return line position of currentIndex
      return currentIndex
              - [self positionFromLine:[self lineFromPosition:currentIndex]];
    }
    else return 0;
}

- (BOOL)listen
{
    // Determine if characters are available for reading
    return (currentIndex < userTextStart);
}

@end
