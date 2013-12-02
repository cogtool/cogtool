#import <appkit/appkit.h>

#define MAXARGS 50

@interface Coordinator:Object
{
    id  infoPanel;                  // the Info Panel on Lisp.app
    id  openPanel;                  // the Open Panel for file selection
    id  preferencesPanel;           // the Preferences Panel
    id  currentServer;              // the current LispServer object
    id  serverList;                 // a List of LispServer objects
    int count;                      // counts the CLISP processes started
    char *arguments[MAXARGS];       // arguments for CLISP
    int argCount;                   // argument count for CLISP
    int nextArgIndex;               // index into arguments for more args
    id  startFileText;              // TextField containing startup file name
    char startFilePath[MAXPATHLEN]; // the name of a possible startup file
    id  quietModeMatrix;            // the radio button matrix for quiet on/off
    BOOL quiet;                     // flag whether CLISP should run quiet
    id  colorWellMachine;           // ColorWells for the four colors used
    id  colorWellUser;              //  in the LispText object
    id  colorWellHighlight;
    id  colorWellEntered;
    NXColor defaultMachineColor;    // Defaults for the colors to use
    NXColor defaultUserColor;
    NXColor defaultHighlightColor;
    NXColor defaultEnteredColor;
    id  fontManager;
    id  machineFontText;            // TextField for displaying machine font
    id  userFontText;               // TextField for displaying user font
    id  currentMachineFont;         // Fonts in current Lisp window
    id  currentUserFont;
    id  defaultMachineFont;         // Defaults for the fonts to use
    id  defaultUserFont;
    int whichFont;                  // Which font should be set?
    id  lineLengthText;             // TextField for line length
    int defaultLineLength;
    struct _NXDefault *lispDefaults;  // for the defaults database
}

- showInfo:sender;
- showPreferences:sender;
- initNewLispText:text;
- newProcess:sender;
- newProcessWithMemFile:sender;
- killProcess:sender;
- stopProcess:sender;
- makeMeCurrent:sender;
- removeMeFromCurrent:sender;
- removeMe:sender;

- setMachineColor:sender;
- setUserColor:sender;
- setHighlightColor:sender;
- setEnteredColor:sender;
- setQuietYes:sender;
- setQuietNo:sender;
- findStartFile:sender;
- getStartFile:sender;
- setMachineFont:sender;
- setUserFont:sender;
- updateMachineFont:font;
- updateUserFont:font;
- changeFont:sender;
- setLineLength:sender;
- updateLineLength:(int)len;
- (int)defaultLineLength;

// Application Object Delegation
- appDidInit:sender;
- appWillTerminate:sender;

// Managing defaults
- setDefaults:sender;

// Loading and compiling files
- userEntered:(char *)buffer;
- loadFile:sender;
- compileFile:sender;

@end
