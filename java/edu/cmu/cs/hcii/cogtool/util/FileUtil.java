/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.3, Copyright (c) 2005-2013 Carnegie Mellon University
 * This software is distributed under the terms of the FSF Lesser
 * Gnu Public License (see LGPL.txt). 
 * 
 * CogTool is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * CogTool is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with CogTool; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * CogTool makes use of several third-party components, with the 
 * following notices:
 * 
 * Eclipse SWT version 3.448
 * Eclipse GEF Draw2D version 3.2.1
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP version 2.38
 * 
 * Copyright (c) Sam Steingold, Bruno Haible 2001-2006
 * This software is distributed under the terms of the FSF Gnu Public License.
 * See COPYRIGHT file in clisp installation folder for more information.
 * 
 * ACT-R 6.0
 * 
 * Copyright (c) 1998-2007 Dan Bothell, Mike Byrne, Christian Lebiere & 
 *                         John R Anderson. 
 * This software is distributed under the terms of the FSF Lesser
 * Gnu Public License (see LGPL.txt).
 * 
 * Apache Jakarta Commons-Lang 2.1
 * 
 * This product contains software developed by the Apache Software Foundation
 * (http://www.apache.org/)
 * 
 * jopt-simple version 1.0
 * 
 * Copyright (c) 2004-2013 Paul R. Holser, Jr.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Mozilla XULRunner 1.9.0.5
 * 
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/.
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
 * License for the specific language governing rights and limitations
 * under the License.
 * 
 * The J2SE(TM) Java Runtime Environment version 5.0
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

public class FileUtil
{
    protected static final int BUFFER_SIZE = 8192;

    public static final String FILE_SEP = System.getProperty("file.separator");

    /**
     * Prevent instantiation
     */
    private FileUtil() { }

    /**
     * Copy contents of specified system resource (typically a file)
     * into the given destination file. If the destination file does not
     * exist, it is created, and otherwise is overwritten.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyResourceToFile(String resourceId, File destination)
        throws IOException
    {
        InputStream in = ClassLoader.getSystemResourceAsStream(resourceId);
        copyStreamToFile(in, destination);
    }

    /**
     * Copy contents of specified system resource (typically a file)
     * into the given OutputStream.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyResourceToStream(String resourceId, OutputStream out)
        throws IOException
    {
        InputStream in = ClassLoader.getSystemResourceAsStream(resourceId);
        copyStreamToStream(in, out);
    }

    /**
     * Copy the contents of an InputStream, up to EOF, to the given
     * OutputStream. The InputStream argument
     * is closed after having been read. The OutputStream argument is not
     * explicitly closed.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyStreamToStream(InputStream in, OutputStream out)
        throws IOException
    {
        byte[] buf = new byte[BUFFER_SIZE];
        int len;

        try {
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        }
        finally {
            in.close();
        }
    }

    public static void copyTextFileToPrintWriter(File sourceFile,
                                                 PrintWriter out)
        throws IOException
    {
        String line;
        BufferedReader br = new BufferedReader(new FileReader(sourceFile));

        while ((line = br.readLine()) != null) {
            out.println(line);
        }
    }

    public static void copyTextFileToFile(File source, File dest)
        throws IOException
    {
        PrintWriter out = null;
        try {
            out = new PrintWriter(new FileWriter(dest));
            copyTextFileToPrintWriter(source, out);
        }
        finally {
            if (out != null) {
                out.close();
            }
        }
    }

    /**
     * Copy contents of specified InputStream, up to EOF,
     * into the given destination file. If the destination file does not
     * exist, it is created, and otherwise is overwritten. The InputStream
     * argument is closed after having been read.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyStreamToFile(InputStream in, File destination)
        throws IOException
    {
        OutputStream out = null;

        try {
            out = new FileOutputStream(destination);

            copyStreamToStream(in, out);
        }
        finally {
            if (out != null) {
                out.close();
            }
        }
    }

    /**
     * Copy the contents of the specifeid LineNumberReader, up to EOF,
     * to the given PrintWriter, using local system appropriate line
     * termination.. The LineNumberReader is closed after having
     * been read.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyReaderToWriter(LineNumberReader in, PrintWriter out)
        throws IOException
    {
        try {
            for (String line = in.readLine();
                 line != null;
                 line = in.readLine())
            {
                out.println(line);
            }
        }
        finally {
            in.close();
        }
    }

    /**
     * Copy the contents of the specified resource, which should be plain
     * text in the local system's default encoding, to the given PrintWriter,
     * using local system appropriate line termination.
     *
     * @throws NullPointerException if either argument is null.
     * @throws IOException
     */
    public static void copyTextResourceToWriter(String resourceId,
                                                PrintWriter out)
        throws IOException
    {
        copyReaderToWriter(new LineNumberReader(new InputStreamReader(
                                    ClassLoader.getSystemResourceAsStream(
                                                                resourceId))),
                           out);
    }

    // TODO We might want to change the name of this method, since it might not
    //      be used to launch an editor for modifying its argument, but merely
    //      viewing it.
    /**
     * Launch, or bring to the front if already running, either the editor
     * bound to this type of file, or the user's default text editor
     * as determined from launchServices, and open
     * the specified file in it.
     * @throws NullPointerException if the argument is null.
     */
    public static void editFile(File f, boolean useDefault)
        throws IOException
    {
        String path = f.getAbsolutePath();

        if (OSUtils.WINDOWS) {
            // Commented out as this bit of magic doesn't seem to work. Seems
            // worth keeping here, though, as a possible starting point when
            // we start researching this question again.
//            // ignores useDefault
//            Runtime.getRuntime().exec(
//                 "rundll32 url.dll,FileProtocolHandler " + path);
//            return;
        }
        else if (OSUtils.MACOSX) {
            List<String> args = new ArrayList<String>(3);
            args.add("open");
            if (useDefault) {
                args.add("-t");
            }
            args.add(path);
            Runtime.getRuntime().exec(
                   args.toArray(new String[args.size()]));
            return;
        }
        // If none of the above worked, fall through to here.
        throw new UnsupportedOperationException(
            "Editing files currently is not supported on this platform ("
            + f + ")");
    }

    public static void editFile(File f) throws IOException {
        editFile(f, false);
    }

    /**
     * Copy the contents of the InputStream, up to EOF, to a temporary
     * file. Then launch the user's default text editor, as determined
     * by LaunchServices, and open that temp file in it. The temp file
     * is deleted when CogTool quits, though if the editor is still
     * running it might easily recreate it again. Returns the temp
     * file that is written. Macintosh only.
     * @throws NullPointerException if the argument is null.
     */
    public static File editStream(InputStream s, String tempFileType)
        throws IOException
    {
        if (! OSUtils.MACOSX) {
            throw new UnsupportedOperationException(
                "Editing streams currently is only support on Mac OS X");
        }

        if ((tempFileType != null) &&
            (tempFileType.length() > 0) &&
            (tempFileType.charAt(0) != '.'))
        {
            tempFileType = "." + tempFileType;
        }

        File f = File.createTempFile("cogtool", tempFileType);

        f.deleteOnExit();

        copyStreamToFile(s, f);

        editFile(f, true);

        return f;
    }

    /**
     * Creates a subdirectory of the system standard temporary directory,
     * and returns a File object specifying the created directory.
     * Warning: while deleteOnExit() does work on directories, it *only*
     *          deletes the directory if it is already empty; it does not
     *          do a recursive delete of a directory
     * @throws IOException if one occurs during the attempt to create
     *         the temporary directory
     * @throws IllegalStateException if for some mysterious reason the
     *         temporary directory cannot be created
     */
    public static File createTempDirectory(String prefix)
        throws IOException
    {
        if ((prefix == null) || (prefix.length() == 0)) {
            prefix = "tmpDir";
        }
        else if (prefix.length() < 3) {
            prefix += "Tmp";
        }

        // This is a bit profligate with resources, needlessly creating a file
        // and then deleting it, just to get a name. But given that the use
        // we're going to put it to in CogTool will dominate that cost, and how
        // infrequently it will be called, that's probably OK.
        File result = File.createTempFile(prefix, "");

        if (! result.delete()) {
            throw new IllegalStateException("Can't delete temp file " + result);
        }

        // There's a race condition here, in that someone could create the file
        // we've just deleted before we get the directory created. That's a risk
        // with which we can surely live, however.
        if (! result.mkdir()) {
            throw new IllegalStateException("Can't create temp dir " + result);
        }

        return result;
    }

    /**
     * Utility to read a "file" into a List of strings,
     * appending one per line of the file to the end of the given List.
     */
    public static void readLines(Reader r, List<String> lines)
        throws IOException
    {
        BufferedReader lineReader =
            (r instanceof BufferedReader) ? (BufferedReader) r
                                          : new BufferedReader(r);

        String line;

        while ((line = lineReader.readLine()) != null) {
            lines.add(line);
        }
    }
}
