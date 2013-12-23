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
import java.io.IOException;
import java.io.InputStreamReader;
import java.rmi.UnexpectedException;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

/**
 * Static methods for invoking programs as a subprocesses, and
 * using the output therefrom to stdout and stderr to populate linked lists of Strings,
 * one per line. The calling thread copies the subprocess's output (both stderr
 * and stdout) to its own stdout, but otherwise waits for the subprocess to
 * terminate.
 * <p>
 * This might well be better implemented as a more stateful class, containing
 * the linked lists, but that would potentially require changes to our file format.
 * So this simply factors out functionality already present to allow it to be
 * easily shared.
 */
public class Subprocess
{
    /**
     * Thrown when the subprocess set-up or execution encounters a problem.
     */
    public static class ExecuteException extends RuntimeException
    {
        public ExecuteException(String msg, Throwable ex)
        {
            super(msg, ex);
        }

        public ExecuteException(String msg)
        {
            super(msg);
        }
    }

    protected static boolean debug = false;

    private Subprocess()
    {
        // Don't allow this class to be instantiated.
    }

    public static void setDebug(boolean value)
    {
        debug = value;
    }

    /**
     * Invokes the given command as a subprocess.
     * If outLines and/or errLines are non-null, the standard output and/or
     * standard error output of the subprocess is appended to them, one String
     * being appended for each line. Both the standard output and the standard
     * error output of the subprocess are copied to this process's standard
     * output, tagged with [stdout] or [stderr] as appropriate, if the global
     * debug boolean has previously been set to true.
     * This method does not return until the subprocess has terminated.
     * @param cmd A List of Strings, the command and its commmand line arguments
     *            to be executed.
     * @param outLines if non-null, a list to which lines of the subprocess's
     *        stdout is to be appended.
     * @param errLines if non-null, a list to which lines of the subprocess's
     *        stderr is to be appended.
     * @param traceCB if non-null, the callback to invoke when each stdout
     *        or stderr line is appended.
     * @param cancelable if non-null, to test whether or not to stop
     * @return the exit status of the subprocess.
     * @throws NullPointerException if cmd is null.
     * @throws ClassCastException if the contents of command contains any Objects
     *         not castable to Strings.
     * @throws ExecuteException if the subprocess or stderr reading thread are
     *         interrupted, or if any IOException occurs.
     */
    public static int exec(List<String> cmd,
                           List<String> outLines,
                           List<String> errLines,
                           ProcessTraceCallback traceCB,
                           Cancelable cancelable)
    {
        return exec(cmd, null, null, outLines, errLines, traceCB, cancelable);
    }

    protected static boolean stillActive(Process p)
    {
        try {
            p.exitValue();
            // If we get here, p is done!
            return false;
        }
        catch (IllegalThreadStateException ex) {
            return true;    // still active!
        }
    }

    /**
     * Invokes the given command as a subprocess.
     * If outLines and/or errLines are non-null, the standard output and/or
     * standard error output of the subprocess is appended to them, one String
     * being appended for each line. Both the standard output and the standard
     * error output of the subprocess are copied to this processes standard
     * output, tagged with [stdout] or [stderr] as appropriate, if the global
     * debug boolean has previously been set to true.
     * This method does not return until the subprocess has terminated.
     * @param cmd A List of Strings, the command and its commmand line arguments
     *            to be executed.
     * @param envp A List of Strings, environmental variables required by the
     *            process
     * @param workingDir A File object representing the directory in which
     *            the process should be executed.
     * @param outLines if non-null, a list to which lines of the subprocess's
     *        stdout is to be appended.
     * @param errLines if non-null, a list to which lines of the subprocess's
     *        stderr is to be appended.
     * @param traceCB if non-null, the callback to invoke when each stdout
     *        or stderr line is appended.
     * @param cancelable if non-null, to test whether or not to stop
     * @return the exit status of the subprocess.
     * @throws NullPointerException if cmd is null.
     * @throws ClassCastException if the contents of command contains any Objects
     *         not castable to Strings.
     * @throws ExecuteException if the subprocess or stderr reading thread are
     *         interrupted, or if any IOException occurs.
     */
    public static int exec(List<String> cmd,
                           List<String> envp,
                           File workingDir,
                           final List<String> outLines,
                           final List<String> errLines,
                           final ProcessTraceCallback traceCB,
                           final Cancelable cancelable)
    {
        String[] cmdArray = cmd.toArray(new String[cmd.size()]);
        String[] envArray = null;

        if (envp != null) {
            envArray = envp.toArray(new String[cmd.size()]);
        }

        // To help in debugging, etc., print to standard out what the
        // equivalent Unix command string would be, roughly. Note that this
        // may not be a line that can just be cut and pasted into a Unix shell,
        // if it contains characters needing to be escaped.
        StringBuilder sb = new StringBuilder("CMD: ");
        for (int i = 0; i < cmdArray.length; ++i) {
            if (cmdArray[i] != null) {
                if (cmdArray[i].length() > 0 && cmdArray[i].charAt(0) == '(') {
                    sb.append("'" + cmdArray[i] + "'");
                }
                else {
                    sb.append(cmdArray[i]);
                }
                sb.append(' ');
            }
        }
        sb.append('\n');
        final String reportableCommand = sb.toString();
        if (debug) {
            System.out.println(reportableCommand);
        }

        try {
            final Process p =
                Runtime.getRuntime().exec(cmdArray, envArray, workingDir);

            // getInputStream gives us the stdout from the Process.
            final BufferedReader outReader =
                new BufferedReader(new InputStreamReader(p.getInputStream()));
            final BufferedReader errReader =
                new BufferedReader(new InputStreamReader(p.getErrorStream()));

            // The two streams must be read from separate threads, since a full
            // buffer on one stream can block the other.
            // XXX should this be using a CogTool worker thread instead of
            //     a vanilla Java Thread?
            Thread t =
                new Thread(new Runnable() {
                               public void run()
                               {
                                   if (traceCB != null) {
                                       traceCB.appendOutputLine(reportableCommand);
                                   }
                                   try {
                                       // This test will pass until
                                       // time > process death
                                       while (stillActive(p) ||
                                              outReader.ready())
                                       {
                                           if (outReader.ready()) {
                                               String output =
                                                   outReader.readLine();

                                               if (output != null) {
                                                   if (debug) {
                                                       System.out.println("[stdout] "
                                                                          + output);
                                                   }
                                                   if (outLines != null) {
                                                       outLines.add(output);
                                                   }
                                                   if (traceCB != null) {
                                                       traceCB.appendOutputLine(output);
                                                   }
                                               }
                                               else {
                                                   break;
                                               }
                                           }
                                           else {
                                               try {
                                                   Thread.sleep(50);  // milliseconds
                                               }
                                               catch (InterruptedException ex) {
                                                   // ignore
                                               }
                                           }

                                           if ((cancelable != null) &&
                                               cancelable.isCanceled())
                                           {
                                               break;
                                           }
                                       }
                                   }
                                   catch (IOException e) {
                                       throw new ExecuteException
                                           ("Subprocess output reader IOException",
                                            e);
                                   }
                               }
                           },
                           "subprocess output reader");
            t.start();

            // This test will pass until time > process death
            while (stillActive(p) || errReader.ready()) {
                if (errReader.ready()) {
                    String output = errReader.readLine();

                    if (output != null) {
                        if (debug) {
                            System.out.println("[stderr] " + output);
                        }
                        if (errLines != null) {
                            errLines.add(output);
                        }
                        if (traceCB != null) {
                            traceCB.appendErrorLine(output);
                        }
                    }
                    else {
                        break;
                    }
                }
                else {
                    try {
                        Thread.sleep(50);  // milliseconds
                    }
                    catch (InterruptedException ex) {
                        // ignore
                    }
                }

                if ((cancelable != null) && cancelable.isCanceled()) {
                    break;
                }
            }

            t.join();

            outReader.close();
            errReader.close();

            // Since the while loop above will spin or block until the Process dies,
            // and the Thread does the same, this method will not return until the
            // process is dead and both buffers are emptied.
            if ((cancelable != null) && cancelable.isCanceled()) {
                p.destroy();
            }

            return p.waitFor();
        }
        catch (IOException e) {
            throw new ExecuteException(
                  "IOException reading subprocess error stream", e);
        }
        catch (InterruptedException e) {
            throw new ExecuteException(
                  "Subprocess or stdout reading thread interrupted", e);
        }
    }

    /**
     * Invokes CLisp as a subprocess, using a specified memory image, optionally
     * loads additional files, and evaluates a form in that Lisp.
     * If outLines and/or errLines are non-null, the standard output and/or
     * standard error output of the subprocess is appended to them, one String
     * being appended for each line. Both the standard output and the standard
     * error output of the subprocess are copied to this processes standard
     * output, tagged with [stdout] or [stderr] as appropriate.
     * This method does not return until the subprocess has terminated.
     * @param memoryImageName the file name (including extension, but no
     *        path information) of the memory image to use to initialize Clisp
     * @param filesToLoad a (possibly null) List of File objects, each of which
     *        will be loaded in order from the command line when starting CLisp
     * @param initialCommand is passed on the command line to CLisp as an
     *        initial form to evaluate; it should be something that will
     *        parse as a properly formed S-expr.
     * @param outLines if non-null, a list to which lines of the subprocess's
     *        stdout is to be appended.
     * @param errLines if non-null, a list to which lines of the subprocess's
     *        stderr is to be appended.
     * @param traceCB if non-null, the callback to invoke when each stdout
     *        or stderr line is appended.
     * @param cancelable if non-null, to test whether or not to stop
     * @return the exit status of the subprocess.
     * @throws NullPointerException if memoryImage or initialCommand are null
     * @throws ClassCastException if the contents of filestoLoad contains any
     *         Objects not castable to Files.
     * @throws UnexpectedException if the subprocess or stderr reading thread are
     *         interrupted, or if any IOException occurs.
     * @see Subprocess@exec(List, List, List)
     */
    public static int execLisp(String memoryImageName,
                               List<File> filesToLoad,
                               String initialCommand,
                               List<String> outLines,
                               List<String> errLines,
                               ProcessTraceCallback traceCB,
                               Cancelable cancelable)
    {
        String lispProgName = null;
        String osName = null;

        if (OSUtils.WINDOWS) {
            lispProgName = "lisp.exe";
            osName = "win";
        }
        else if (OSUtils.MACOSX) {
            lispProgName = "lisp.run";
            if (OSUtils.isIntelMac()) {
                osName = "mac-intel";
            }
            else {
                osName = "mac-ppc";
                throw new IllegalStateException("CogTool no longers supports PowerPC");
            }
        }
        else {
            throw new IllegalStateException("Unknown Operating System");
        }

        File clispDir = new File("clisp-" + osName);

        List<String> cmdList = new ArrayList<String>();

        // the executable itself
        cmdList.add(new File(clispDir, lispProgName).getAbsolutePath());

        // make it quiet
        cmdList.add("-q");

        // Set the character encoding used. It appears that the default
        // UTF-8 is causing problems on some small number of machines.
        // Why these machines are failing to correctly launch CLisp
        // with the default encoding remains a mystery, however.
        cmdList.add("-E");
        cmdList.add("ISO-8859-1");

        // load the memory image
        cmdList.add("-M");
        cmdList.add(new File(clispDir, memoryImageName).getAbsolutePath());

        // load lisp files
        if (filesToLoad != null) {
            for (File file : filesToLoad) {
                cmdList.add("-i");
                cmdList.add(file.getPath());
            }
        }

        // what to execute
        cmdList.add("-x");
        cmdList.add(initialCommand);

        // Windows mangles command line arguments, so escape them as
        // necessary.
        if (OSUtils.WINDOWS) {
            for (ListIterator<String> it = cmdList.listIterator(); it.hasNext();)
            {
                String cmd = it.next();

                if (cmd != null) {
                    if (cmd.length() > 0 && cmd.charAt(0) == '(') {
                        it.set(LispUtil.safeString(cmd));
                    }
                }
            }
        }

        return exec(cmdList, outLines, errLines, traceCB, cancelable);
    }
}
