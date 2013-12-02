#import "Coordinator.h"
#import "LispServer.h"
#import "LispText.h"
#import <ctype.h>
#import <string.h>

#define MEMFILE "lispinit.mem"

@interface Coordinator(Private)
- (void)startNewProcess;
@end

@implementation Coordinator(Private)

- (void)startNewProcess
{
    // Start a new Lisp subprocess in its own window
    id server;
    char buffer[10];

    // If a file has to be loaded on startup, put "-i filename" into
    // the argument list
    if(startFilePath[0] != '\000')
    {
      arguments[argCount++] = "-i";
      arguments[argCount++] = startFilePath;
    }

    // If CLISP should run in `quiet' mode, put "-q" into the argument list
    if(quiet) arguments[argCount++] = "-q";
    arguments[argCount] = NULL;

    // Generate title "LISP <number>" for the new window
    sprintf(buffer, "LISP %d", count + 1);

    // Set the default values in the Preferences Panel
    [colorWellMachine setColor:defaultMachineColor];
    [colorWellUser setColor:defaultUserColor];
    [colorWellHighlight setColor:defaultHighlightColor];
    [colorWellEntered setColor:defaultEnteredColor];
    [lineLengthText setIntValue:defaultLineLength];
    [self updateMachineFont:defaultMachineFont];
    [self updateUserFont:defaultUserFont];

    // Allocate and initialize a LispServer object for the new process.
    // It will take care of the actual forking off of a Lisp task.
    server = [[LispServer alloc] initTitle:buffer
                                 offset:(count % 5)
                                 argCount:argCount
                                 argVector:arguments
                                 coordinator:self];

    // Increment the process counter
    count++;

    // Put it into our list and make it the current Lisp server
    [serverList addObject:server];
    currentServer = server;
    return;
}

@end

@implementation Coordinator

- showInfo:sender
{
    // Show the Info Panel
    if (!infoPanel)
    {
        infoPanel =
            [NXApp loadNibSection:"InfoPanel.nib" owner:NXApp withNames:NO];
    }
    [infoPanel orderFront:self];
    return self;
}

- showPreferences:sender
{
    // Show the Preferences Panel
    [preferencesPanel makeKeyAndOrderFront:self];
    return self;
}

- initNewLispText:text
{
    [text setMachineColor:colorWellMachine];
    [text setUserColor:colorWellUser];
    [text setHighlightColor:colorWellHighlight];
    [text setEnteredColor:colorWellEntered];
    [text setMachineFont:defaultMachineFont];
    [text setUserFont:defaultUserFont];
    return self;
}

- newProcess:sender
{
    // This method gets called when the user clicks "new" in the menu
    char memfilePath[MAXPATHLEN];

    // Put "-M <memfile>" into the argument list
    argCount = nextArgIndex;
    // Default memfile is lispinit.mem in the Lisp.app wrapper
    sprintf(memfilePath, "%s/%s", [[NXBundle mainBundle] directory], MEMFILE);
    arguments[argCount++] = "-M";
    arguments[argCount++] = memfilePath;
    // Start a new Lisp process
    [self startNewProcess];
    return self;
}

- newProcessWithMemFile:sender
{
    // This is the same as "- newProcess:sender", but the user may specify
    // a memory image to load (or to load no memory image)
    const char *types[2] = {"mem", NULL};
    char file[MAXPATHLEN];

    argCount = nextArgIndex;
    if(openPanel == nil)
    {
      openPanel = [OpenPanel new];
      [openPanel setDelegate:self];
      [openPanel allowMultipleFiles:NO];
    }
    // Ask the user for a file
    if([openPanel runModalForTypes:types] == NX_OKTAG)
    {
      // If user says "OK", put "-M <memfile>" into the argument list
      strcpy(file, [openPanel filename]);
      arguments[argCount++] = "-M";
      arguments[argCount++] = file;
    }
    // Start a new Lisp process
    [self startNewProcess];
    return self;
}

- killProcess:sender
{
    // Kill a Lisp process. The free method of its server does it
    if(currentServer) [currentServer free];
    return self;
}

- stopProcess:sender
{
    // Interrupt a Lisp process. This is done by the server
    if(currentServer) [currentServer userBreak:self];
    return self;
}

- makeMeCurrent:sender
{
    // sender is a LispServer that wants to become the current server.
    // Check if it is in our list
    if([serverList indexOf:sender] != NX_NOT_IN_LIST)
    {
      id text = [sender lispText];

      currentServer = sender;
      // Update the Prferences Panel to reflect the state of the new
      // current Lisp process
      [colorWellMachine setColor:[text machineColor]];
      [colorWellUser setColor:[text userColor]];
      [colorWellHighlight setColor:[text highlightColor]];
      [colorWellEntered setColor:[text enteredColor]];
      [lineLengthText setIntValue:[text lispTextLineLength]];
      [self updateMachineFont:[text machineFont]];
      [self updateUserFont:[text userFont]];
    }
    else
      currentServer = nil;
    return self;
}

- removeMeFromCurrent:sender
{
    // The LispServer sender wants to be no longer the current server
    if(sender == currentServer) currentServer = nil;
    return self;
}

- removeMe:sender
{
    // The LispServer sender wants to be removed from the list
    [self removeMeFromCurrent:sender];
    [serverList removeObject:sender];
    return self;
}

- setMachineColor:sender
{
    // Set the color for text output by the Lisp process
    // in the current Lisp window
    if(currentServer) [[currentServer lispText] setMachineColor:sender];
    else NXBeep();
    return self;
}

- setUserColor:sender
{
    // Set the color for user entered text in the current Lisp window
    if(currentServer) [[currentServer lispText] setUserColor:sender];
    else NXBeep();
    return self;
}

- setHighlightColor:sender
{
    // Set the color for highlighted text in the current Lisp window
    if(currentServer) [[currentServer lispText] setHighlightColor:sender];
    else NXBeep();
    return self;
}

- setEnteredColor:sender
{
    // Set the color for user entered text that can't be edited
    // in the current Lisp window
    if(currentServer) [[currentServer lispText] setEnteredColor:sender];
    else NXBeep();
    return self;
}

- setQuietYes:sender
{
    // Subsequent Lisp processes will start up in `quiet' mode
    quiet = YES;
    return self;
}

- setQuietNo:sender
{
    // Subsequent Lisp processes will start up in `verbose' mode
    quiet = NO;
    return self;
}

- getStartFile:sender
{
    // Read the filename of the startup file from its TextField
    strcpy(startFilePath, [startFileText stringValue]);
    return self;
}

- findStartFile:sender
{
    // Bring up an Open Panel to choose a startup file from
    const char *types[3] = {"lisp", "fas", NULL};

    if(openPanel == nil)
    {
      openPanel = [OpenPanel new];
      [openPanel setDelegate:self];
      [openPanel allowMultipleFiles:NO];
    }
    if([openPanel runModalForTypes:types] == NX_OKTAG)
    {
      // Remember the file for later processes
      strcpy(startFilePath, [openPanel filename]);
      // Update the TextField
      [startFileText setStringValue:startFilePath];
    }
    return self;
}

- setLineLength:sender
{
    // Read the desired line length from its TextField
    int len = [lineLengthText intValue];

    if(currentServer == nil) { NXBeep(); return self; }

    if(len < 40 || len > 500)
      [lineLengthText setIntValue:
                         [[currentServer lispText] lispTextLineLength]];
    else  [currentServer setLineLengthOfWindow:len];
    return self;
}

- updateLineLength:(int)len
{
    [lineLengthText setIntValue:len];
    return self;
}

- (int)defaultLineLength
{
    return defaultLineLength;
}

#define SET_MACHINE_FONT 1
#define SET_USER_FONT    2
#define SET_NO_FONT      0

- setMachineFont:sender
{
    whichFont = SET_MACHINE_FONT;
    [fontManager setSelFont:currentMachineFont isMultiple:NO];
    [fontManager orderFrontFontPanel:self];
    return self;
}

- setUserFont:sender
{
    whichFont = SET_USER_FONT;
    [fontManager setSelFont:currentUserFont isMultiple:NO];
    [fontManager orderFrontFontPanel:self];
    return self;
}

- updateMachineFont:font
{
    char name[256];

    currentMachineFont = font;
    sprintf(name, "%s %.1f Points", [font displayName], [font pointSize]);
    [machineFontText setStringValue:name];
    return self;
}

- updateUserFont:font
{
    char name[256];

    currentUserFont = font;
    sprintf(name, "%s %.1f Points", [font displayName], [font pointSize]);
    [userFontText setStringValue:name];
    return self;
}

- changeFont:sender
{
    id newFont;

    switch(whichFont)
    {
      case SET_MACHINE_FONT:
        newFont = [sender convertFont:currentMachineFont];
        if(currentServer) [[currentServer lispText] setMachineFont:newFont];
        [self updateMachineFont:newFont];
        [[sender getFontPanel:YES] close];
        break;
      case SET_USER_FONT:
        newFont = [sender convertFont:currentUserFont];
        if(currentServer) [[currentServer lispText] setUserFont:newFont];
        [self updateUserFont:newFont];
        [[sender getFontPanel:YES] close];
        break;
      default:
        NXBeep();
    }
    whichFont = SET_NO_FONT;
    return self;
}


// Application Delegation

// Some defines for the position of specific defaults in lispDefaults
#define DEFAULT_MACHINE_COLOR     0
#define DEFAULT_USER_COLOR        1
#define DEFAULT_HIGHLIGHT_COLOR   2
#define DEFAULT_ENTERED_COLOR     3
#define DEFAULT_QUIET_MODE        4
#define DEFAULT_STARTUP_FILE      5
#define DEFAULT_LINE_LENGTH       6
#define DEFAULT_MACHINE_FONT      7
#define DEFAULT_MACHINE_FONT_SIZE 8
#define DEFAULT_USER_FONT         9
#define DEFAULT_USER_FONT_SIZE    10

#define DEFAULT_OWNER  "LispStoll"

- appDidInit:sender
{
    int i;
    int languageIndex;
    const char * const *sysLanguages = [NXApp systemLanguages];
    static NXDefaultsVector defaults =
      { {"LispMachineColor",    "0.0 0.0 0.0"},
        {"LispUserColor",       "0.0 0.0 0.0"},
        {"LispHighlightColor",  "1.0 0.0 0.0"},
        {"LispEnteredColor",    "0.0 0.0 1.0"},
        {"LispQuietMode",       "No"         },
        {"LispStartupFile",     ""           },
        {"LispLineLength",      "80"         },
        {"LispMachineFont",     "Ohlfs"      },
        {"LispMachineFontSize", "12.0"       },
        {"LispUserFont",        "Ohlfs"      },
        {"LispUserFontSize",    "12.0"       },
        {NULL                                }
      };

    // Register defaults
    NXRegisterDefaults(DEFAULT_OWNER, defaults);
    lispDefaults = defaults;

    // Initialize server bookkeeping
    serverList = [[List alloc] initCount:10];
    count = 0;

    // Initialize defaults
    {
      float red = 0.0, green = 0.0, blue = 0.0;
      char *buffer;

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispMachineColor"),
             "%f %f %f", &red, &green, &blue);
      defaultMachineColor = NXConvertRGBToColor(red, green, blue);
      buffer = malloc(30*sizeof(char));
      sprintf(buffer, "%7f %7f %7f", red, green, blue);
      lispDefaults[DEFAULT_MACHINE_COLOR].value = buffer;
      [colorWellMachine setColor:defaultMachineColor];

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispUserColor"),
             "%f %f %f", &red, &green, &blue);
      defaultUserColor = NXConvertRGBToColor(red, green, blue);
      buffer = malloc(30*sizeof(char));
      sprintf(buffer, "%7f %7f %7f", red, green, blue);
      lispDefaults[DEFAULT_USER_COLOR].value = buffer;
      [colorWellUser setColor:defaultUserColor];

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispHighlightColor"),
             "%f %f %f", &red, &green, &blue);
      defaultHighlightColor = NXConvertRGBToColor(red, green, blue);
      buffer = malloc(30*sizeof(char));
      sprintf(buffer, "%7f %7f %7f", red, green, blue);
      lispDefaults[DEFAULT_HIGHLIGHT_COLOR].value = buffer;
      [colorWellHighlight setColor:defaultHighlightColor];

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispEnteredColor"),
             "%f %f %f", &red, &green, &blue);
      defaultEnteredColor = NXConvertRGBToColor(red, green, blue);
      buffer = malloc(30*sizeof(char));
      sprintf(buffer, "%7f %7f %7f", red, green, blue);
      lispDefaults[DEFAULT_ENTERED_COLOR].value = buffer;
      [colorWellEntered setColor:defaultEnteredColor];
    }

    quiet = (NXGetDefaultValue(DEFAULT_OWNER, "LispQuietMode")[0] == 'Y');
    lispDefaults[DEFAULT_QUIET_MODE].value = (quiet) ? "Yes" : "No";
    [quietModeMatrix selectCellAt:0 :(quiet) ? 1 : 0];

    strcpy(startFilePath, NXGetDefaultValue(DEFAULT_OWNER, "LispStartupFile"));
    lispDefaults[DEFAULT_STARTUP_FILE].value = startFilePath;
    [startFileText setStringValue:startFilePath];

    {
      const char *def = NXGetDefaultValue(DEFAULT_OWNER, "LispLineLength");
      char* buffer = malloc(4*sizeof(char));

      sscanf(def, "%d", &defaultLineLength);
      sprintf(buffer, "%d", defaultLineLength);
      lispDefaults[DEFAULT_LINE_LENGTH].value = buffer;
      [lineLengthText setIntValue:defaultLineLength];
    }

    {
      float size;
      char *name = malloc(256*sizeof(char));
      char *sizeString = malloc(10*sizeof(char));

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispMachineFontSize"),
             "%f", &size);
      [self updateMachineFont:
         [Font newFont:NXGetDefaultValue(DEFAULT_OWNER, "LispMachineFont")
                  size:size]];
      defaultMachineFont = currentMachineFont;
      strcpy(name, [defaultMachineFont name]);
      sprintf(sizeString, "%.1f", [defaultMachineFont pointSize]);
      lispDefaults[DEFAULT_MACHINE_FONT].value = name;
      lispDefaults[DEFAULT_MACHINE_FONT_SIZE].value = sizeString;
    }

    {
      float size;
      char *name = malloc(256*sizeof(char));
      char *sizeString = malloc(10*sizeof(char));

      sscanf(NXGetDefaultValue(DEFAULT_OWNER, "LispUserFontSize"),
             "%f", &size);
      [self updateUserFont:
         [Font newFont:NXGetDefaultValue(DEFAULT_OWNER, "LispUserFont")
                  size:size]];
      defaultUserFont = currentUserFont;
      strcpy(name, [defaultUserFont name]);
      sprintf(sizeString, "%.1f", [defaultUserFont pointSize]);
      lispDefaults[DEFAULT_USER_FONT].value = name;
      lispDefaults[DEFAULT_USER_FONT_SIZE].value = sizeString;
    }

    // Set up the arguments list for CLISP processes
    argCount = 0;
    for(i = 0; i < MAXARGS; i++) arguments[i] = NULL;
    arguments[argCount++] = "lisp"; // Name of program
    arguments[argCount++] = "-I";   // Don't treat Tabs specially
    arguments[argCount++] = "-L";
    languageIndex = argCount;
    arguments[argCount++] = "ENGLISH"; // Language for CLISP (preliminary)
    nextArgIndex = argCount;  // Index for further args

    // Check the system languages for English, German or French
    // to start up CLISP with the user's language if possible
    while(*sysLanguages)
    {
      if(!strcasecmp(*sysLanguages, "English") ||
         !strcasecmp(*sysLanguages, "German")  ||
         !strcasecmp(*sysLanguages, "French"))
        {
          int len = strlen(*sysLanguages);
          char *buffer = malloc((len+1)*sizeof(char));
          int i;

          for(i = 0; i < len; i++) buffer[i] = toupper((*sysLanguages)[i]);
          buffer[len] = '\0';
          arguments[languageIndex] = buffer;
          break;
        }
      sysLanguages++;
    }

    // Start up a Lisp process
    [self newProcess:self];
    return self;
}

- appWillTerminate:sender
{
    id server = [serverList removeLastObject];

    // Clean up all Lisp processes/servers/... that are around
    while(server)
    {
      [server free];
      server = [serverList removeLastObject];
    }
    currentServer = nil;

    // Save defaults
    NXWriteDefaults(DEFAULT_OWNER, lispDefaults);

    return self;
}

// Changing defaults

- setDefaults:sender
{
    float red, green, blue;

    defaultMachineColor = [colorWellMachine color];
    NXConvertColorToRGB(defaultMachineColor, &red, &green, &blue);
    sprintf(lispDefaults[DEFAULT_MACHINE_COLOR].value,
            "%f %f %f", red, green, blue);

    defaultUserColor = [colorWellUser color];
    NXConvertColorToRGB(defaultUserColor, &red, &green, &blue);
    sprintf(lispDefaults[DEFAULT_USER_COLOR].value,
            "%f %f %f", red, green, blue);

    defaultHighlightColor = [colorWellHighlight color];
    NXConvertColorToRGB(defaultHighlightColor, &red, &green, &blue);
    sprintf(lispDefaults[DEFAULT_HIGHLIGHT_COLOR].value,
            "%f %f %f", red, green, blue);

    defaultEnteredColor = [colorWellEntered color];
    NXConvertColorToRGB(defaultEnteredColor, &red, &green, &blue);
    sprintf(lispDefaults[DEFAULT_ENTERED_COLOR].value,
            "%f %f %f", red, green, blue);

    lispDefaults[DEFAULT_QUIET_MODE].value = (quiet) ? "Yes" : "No";

    [startFileText setStringValue:startFilePath];
    lispDefaults[DEFAULT_STARTUP_FILE].value = startFilePath;

    defaultLineLength = [lineLengthText intValue];
    sprintf(lispDefaults[DEFAULT_LINE_LENGTH].value, "%d", defaultLineLength);

    defaultMachineFont = currentMachineFont;
    strcpy(lispDefaults[DEFAULT_MACHINE_FONT].value,
           [defaultMachineFont name]);
    sprintf(lispDefaults[DEFAULT_MACHINE_FONT_SIZE].value,
            "%.1f", [defaultMachineFont pointSize]);

    defaultUserFont = currentUserFont;
    strcpy(lispDefaults[DEFAULT_USER_FONT].value,
           [defaultUserFont name]);
    sprintf(lispDefaults[DEFAULT_USER_FONT_SIZE].value,
            "%.1f", [defaultUserFont pointSize]);

    NXWriteDefaults(DEFAULT_OWNER, lispDefaults);
    return self;
}

// Loading and compiling files

- userEntered:(char *)buffer
{
    // This is used by loadFile etc. to simulate user entered commands
    if(currentServer) [currentServer userEntered:buffer]; else NXBeep();
    return self;
}

- loadFile:sender
{
    const char *types[3] = {"lisp", "fas", NULL};

    if(openPanel == nil)
    {
      openPanel = [OpenPanel new];
      [openPanel setDelegate:self];
      [openPanel allowMultipleFiles:NO];
    }
    // Ask the user for a file to load
    if([openPanel runModalForTypes:types] == NX_OKTAG)
    {
      char buffer[MAXPATHLEN+11];

      // Simulate the user's typing `(LOAD "filename")'
      sprintf(buffer, "(LOAD \"%s\")\n", [openPanel filename]);
      [self userEntered:buffer];
    }
    return self;
}

- compileFile:sender
{
    const char *types[2] = {"lisp", NULL};

    if(openPanel == nil)
    {
      openPanel = [OpenPanel new];
      [openPanel setDelegate:self];
      [openPanel allowMultipleFiles:NO];
    }
    // Ask the user for a file to compile
    if([openPanel runModalForTypes:types] == NX_OKTAG)
    {
      char buffer[MAXPATHLEN+19];

      // Simulate the user's typing `(COMPILE-FILE "filename")'
      sprintf(buffer, "(COMPILE-FILE \"%s\")\n", [openPanel filename]);
      [self userEntered:buffer];
    }
    return self;
}

@end
