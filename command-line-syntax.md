# Command Line Syntax
It is possible to do some limited actions in CogTool from the command line. This is both (a) ongoing development, so the things it is possible to do will be growing in coming weeks; and (b) very much intended for researchers, so not very robust. Regarding (b), please note that

* Errors are not handled very gracefully. For the most part if anything goes wrong, CogTool will spit out an error message to the terminal window and die. And the error messages, particularly for the syntax of the commands provided not being correct, may not be as informative as one would like.
* It is far from a general purpose command language. Not only are the possibilities limited, if you try to combine them in too complex a manner Bad Things may happen.
* The syntax is subject to change. It is currently fairly idiosyncratic using dollar signs to separate things. This may eventually change to something a little more graceful.

What can be passed to CogTool on the command line has grown over the years, and is now in its third, and most useful, iteration, which subsumes all the possibilities previously supported. However, for backward compatibility the old syntaxes are still supported. However, this note will only describe the newest, and there are no guarantees the old will continue to be supported.

In this newest version, the path to a command file is passed to CogTool on the command line. This allows more, and longer, commands than were possible when all the details were passed directly on the command line. The name of the file is passed as the value of the -f option on the command line.

How to actually run CogTool from the command line varies by platform:


### For Macintosh:
If the app is at `/Applications/CogTool.app`, type, at the command line
```
/Applications/CogTool.app/Contents/MacOS/cogtoolstart -f /path/to/command/file
```
Where `/path/to/command/file` is wherever you have put the command file you write, described below. While it doesn't necessarily have to be an absolute pathname, if relative what it is relative to is not very helpful, so it will almost certainly be best to use an absolute pathname. Note that the shell will expand things like `~/mycommands.txt` to an absolute pathname for you.

Also, the the pathname to the app must be absolute, as shown above; something like `./CogTool.app` will not work because of some pathname surgery going on inside cogtoolstart (which really isn't designed for this purpose, it's just being press-ganged into it).

### For Windows:
If the app is in it's usual place, type, at the command line
```
"C:\Program Files (x86)\CogTool\CogTool.exe" -f C:\path\to\command\file
```
Were `C:\path\to\command\file` is wherever you have put the command file you write, described below. While it doesn't necessarily have to be an absolute pathname, if relative what it is relative to is not very helpful, so it will almost certainly be best to use an absolute pathname. This has only been tested on 64 bit Windows 7, but will likely work fine in other versions, albeit possibly changing details of where the CogTool executable is located as necessary.

#### From CygWin:
It is also possible to start it from CygWin
```
/cygdrive/c/Program\ Files\ (x86)/CogToo.exe -f 'C:\path\to\command\file'
```
Note, however, that the path to the command file must still be given in Windows style, not Unix, and any pathnames within the command file must still be in Windows style.

### Contents of the command file:
The command file is a text file, which you can create with the editor of your choice, and can place wherever you like. It contains commands, one line per command. Blank lines can be added to taste. If the first non-whitespace character on any line is a number sign (\#) that line is treated as a comment, and also ignored. The file **must** use the line termination convention native to the platform on which CogTool is running, or the commands will not be correctly parsed..

A command line consists of a command name, and zero or more arguments. The arguments are separated from the command name, and one another, by dollar signs ($). This unusual choice was made to allow things like embedded whitespace, commas, colons, semicolons and other common punctuation to appear within arguments. However, all leading and trailing whitespace is removed from both command names and arguments.

For example, 
```
computeNovice $ Design 1 $ The task. $ 3 $ 600 $ Frame: initial $ true $ Frame: final
```
calls the command `computeNovice` with the seven arguments:

Design 1
The task.
3
600
Frame: initial
true
Frame: final

Where the strings that are the first, second, fifth and seventh arguments have embedded spaces, and, apart from the first, punctuation as well.

The current, as of 25 June 2013, commands possible are as follows. Where arguments are pathnames it is best to supply fully expanded, absolute paths.


#### open 
open takes one argument, the pathname of a cgt file to open; it should include the final .cgt type, if that's part of the file's name

#### import
import takes one or two arguments. The first is the pathname of an xml file to import; it should include the final .xml type, if that's part of the file's name; the contents are added to any currently open project, or if there is none, a new project is created. If a second argument is supplied, it should be the word "true" or "false" (without the surrounding quotes), and indicates whether or not any scripts should be automatically recomputed. If not supplied, the behavior is controlled, as usual, by the setting in Preferences->Research of "Compute scripts on XML import."

#### computeAllSkilled
computeAllSkilled takes no arguments, and computes all the scripts in the current project

#### generateDictionary
generateDictionary takes five arguments, and generates a dictionary for a specified design in the current project. The arguments are, in order
* the name of the design
* the name of the algorithm, such as LSA or GENSIM
* the limiting site, which is used only for the PMI-G algorithms
* the LSA Space, which is only used for the LSA or GENSIM algorithms
* the URL

```
$ theDesign $ LSA $ $ General_Reading_Up_to_1st_Year_College $ http://autocww2.colorado.edu/cgi-bin/nph-elaborate.cgi?Frequency=50&Cosine=0.5&
```

#### computeNovice
computeNovice takes seven or more arguments, and computes a CogTool Explorer task for a particular design. The arguments are, in order
* the name of the design
* the name of the task
* the number of trials, an integer
* the k value (eagerness to satisfice), an integer
* the name of the starting frame
* a boolean (true or false) saying whether or not to add the results to an existing group 
* then one or more arguments, the possible target frames

#### trace
trace takes one argument, yes or no, and indicates whether computations are to be run with ACT-R traces emitted; if no trace command is included the preference value is consulted as usual

#### exportResults
exportResults takes one argument, the pathname of a file to which to export the results from the current project as comma separated values

#### importDictionary
importDictionary takes two arguments. In order, they are
* the name of the design
* a pathname, the CSV file to import. The extension, typically .csv, should be included, and the file contents must have the correct format

#### saveAs
saveAs takes one argument, the pathname of the file to save the current project as

#### quit
quit takes no arguments, and indicates that CogTool should terminate; no further commands will be processed; if quit is not supplied the running CogTool will remain up and can be manipulated with its GUI as normal


Just before CogTool executes each line of the command file it writes to the console each command, and then each argument, one per line. This can be helpful for debugging. If for some reason it is preferred not to have this information written to the console it can be suppressed by passing the -Q (it must be capital) command line option to CogTool along with the -f option.
