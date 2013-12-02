The java and .class files of BalsamiqButtonAPIConverter are in this directory.
The .class file is in the converter directory.

If CogTool crashes while attempting the conversion, it may be due to compiling 
with different versions of Java. If this is a problem, you can compile the .java
file and save it as a .class file.

To do this create a new Project in Eclipse with the java file in the source.

Some of CogTool's classes must be exported to the user in a jar file so that the user may code the conversion.
The jar file in the directory is a jar file of CogTool's java package.

In the package explorer, Right click on the project and click Build Path->Configure Build Path
Then click Add External Jar...Select the jar file that is in this converter directory.

Now after configuring the build path, the .java should compile. After compiling, the .class
version of the file will appear in the project's bin folder.