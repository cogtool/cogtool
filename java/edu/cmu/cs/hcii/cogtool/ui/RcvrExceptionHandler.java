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

package edu.cmu.cs.hcii.cogtool.ui;

import edu.cmu.cs.hcii.cogtool.ReportInteraction;
import edu.cmu.cs.hcii.cogtool.util.AggregateException;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.RcvrClipboardException;
import edu.cmu.cs.hcii.cogtool.util.RcvrCogModelException;
import edu.cmu.cs.hcii.cogtool.util.RcvrComputationException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOLoadException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOSaveException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOTempException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.RcvrOutOfMemoryException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUnimplementedFnException;
import edu.cmu.cs.hcii.cogtool.util.RcvrWorkThreadException;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager.IWorkThread;

/**
 * Static class that provides a single point to display RecoverableExceptions,
 * along with the requisite status messages and titles for each
 * RecoverableException subclass.
 *
 * @author jcorn
 */
public class RcvrExceptionHandler
{
    public static String IO_SAVE_EXC_TITLE =
        L10N.get("ERR.IOSaveExcTitle","File Error");

    public static String IO_SAVE_EXC_DESC =
        L10N.get("ERR.IOSaveExcDesc",
                 "CogTool could not save the file to the "
                 + "location you specfied.  Please choose "
                 + "a different file name or try again.");


    public static String IO_LOAD_EXC_TITLE =
        L10N.get("ERR.IOLoadExcTitle", "File Error");
    public static String IO_LOAD_EXC_DESC =
        L10N.get("ERR.IOLoadExcDesc",
                 "CogTool could not read the file you "
                 + "specified.  The file may have been written by a newer "
                 + "version of CogTool, or may be "
                 + "unavailable, corrupt, or simply not "
                 + "a type of file that CogTool supports. "
                 + "Please choose a different file "
                 + "or try again.");


    public static String IO_TEMP_EXC_TITLE =
        L10N.get("ERR.IOTempExcTitle",  "File Error");
    public static String IO_TEMP_EXC_DESC =
        L10N.get("ERR.IOTempExcDesc",
                 "CogTool occasionally has to create "
                 + "temporary files on your hard drive "
                 + "in order to function properly.  We "
                 + "encountered an error while working "
                 + "with these temporary files.  Please "
                 + "check your file system for problems.");

    public static String CLIPBOARD_EXC_TITLE =
        L10N.get("ERR.ClipboardExcTitle", "Clipboard Error");
    public static String CLIPBOARD_EXC_DESC =
        L10N.get("ERR.ClipboardExcDesc",
                 "CogTool could not read or write to the "
                 + "clipboard.  The data does not seem to conform  "
                 + "to proper CogTool clipboard format.");

    public static String IO_EXC_TITLE =
        L10N.get("ERR.IOExcTitle", "File Error");
    public static String IO_EXC_DESC =
        L10N.get("ERR.IOExcDesc",
                 "CogTool could not read or write to the "
                 + "file or directory you specified.  The  "
                 + "file could be missing, busy or corrupted. "
                 + "Please choose a different file or try "
                 + "again.");

    public static String UI_EXC_TITLE =
        L10N.get("ERR.UIExcTitle", "User Interface Error");
    public static String UI_EXC_DESC =
        L10N.get("ERR.UIExcDesc",
                 "CogTool's user interface components did "
                 + "not respond as expected.  Please save "
                 + "your work if possible and restart CogTool "
                 + "immediately.  If you encounter this "
                 + "problem more than once, contact the "
                 + "CogTool development team.");

    public static String UNIMP_EXC_TITLE =
        L10N.get("ERR.UnimpExcTitle","Unimplemented Functionality");
    public static String UNIMP_EXC_DESC =
        L10N.get("ERR.UnimpExcDesc",
                 "You have attempted an action that CogTool "
                 + "does not currently support, although "
                 + "we may in the future.  Please bear "
                 + "with us as we extend CogTool's "
                 + "capabilities.");

    public static String COMPUTE_EXC_TITLE =
        L10N.get("ERR.ComputeExcTitle", "Computation Error");
    public static String COMPUTE_EXC_DESC =
        L10N.get("ERR.ComputeExcDesc",
                 "CogTool encountered an error while "
                 + "attempting to compute a task time "
                 + "prediction.  This could be due to "
                 + "communication problems between CogTool "
                 + "and the currently selected cognitive "
                 + "model, or it could be the result of "
                 + "problems in your script.  Try "
                 + "redemonstrating your script and then "
                 + "recomputing.  Contact the CogTool developers "
                 + "if this problem persists.");

    public static String COG_MOD_EXC_TITLE =
        L10N.get("ERR.CogModExcTitle", "Impossible Model Error");
    public static String COG_MOD_EXC_DESC =
        L10N.get("ERR.CogModExcDesc",
                 "You have just specified an action that "
                 + "CogTool does not allow.  This could "
                 + "be due to requirements specified by "
                 + "the underlying cognitive model.  For "
                 + "example, in many cases a Menu Item "
                 + "cannot be clicked on unless a Menu "
                 + "Header has been clicked on first. "
                 + "Please specify a different action. ");

    public static String IMG_EXC_TITLE =
        L10N.get("ERR.ImageExcTitle", "Image Error");
    public static String IMG_EXC_DESC =
        L10N.get("ERR.ImageExcDesc",
                 "CogTool has encountered an image error.");

    public static String MEMORY_EXC_TITLE =
        L10N.get("ERR.MemoryExcTitle", "Out of Memory Error");
    public static String MEMORY_EXC_DESC =
        L10N.get("ERR.MemoryExcDesc",
                 "CogTool has run out of memory in attempting to perform the requested operation.");

    public static String THREAD_EXC_TITLE =
        L10N.get("ERR.ThreadExcTitle", "Thread Error(s)");
    public static String THREAD_EXC_DESC =
        L10N.get("ERR.ThreadExcDesc",
                 "CogTool has encountered one or more errors while "
                 + "executing your requested operation "
                 + "in a child thread.  "
                 + "Your data should be unaffected.  Try "
                 + "saving your data, restarting CogTool,"
                 + "and attempting the operation again.  "
                 + "If this also fails, please contact the "
                 + "CogTool development team.");


    public static String AGGREGATE_EXC_TITLE =
        L10N.get("ERR.AggregateExcTitle", "Multiple Errors");
    public static String AGGREGATE_EXC_DESC =
        L10N.get("ERR.AggregateExcDesc",
                 "CogTool has encountered one or more errors while "
                 + "executing your requested operation."
                 + "Your data should be unaffected.  Try "
                 + "saving your data, restarting CogTool,"
                 + "and attempting the operation again.  "
                 + "If this also fails, please contact the "
                 + "CogTool development team.");


    public static String RCVR_EXC_TITLE =
        L10N.get("ERR.ImageExcTitle", "Recoverable Error");
    public static String RCVR_EXC_DESC =
        L10N.get("ERR.ImageExcDesc",
                 "CogTool has encountered an error that "
                 + "caused the requested operation to fail. "
                 + "Your data should be unaffected.  Try "
                 + "saving your data, restarting CogTool, "
                 + "and attempting the operation again. "
                 + "If this fails, please contact the "
                 + "CogTool development team.");


    /**
     * Hidden constructor for static class - shouldn't be called
     */
    private RcvrExceptionHandler()
    {
        // This should never be called
    }
    /**
     * Prints the exception to stderr and then displays an exception
     * dialog box via the interaction object.
     *
     * @param ex RecoverableException to report
     * @param interaction that can create the reportException dialog
     */
    public static void recover(RecoverableException ex,
                               ReportInteraction interaction)
    {
        // Print the exception to stderr so that the stack trace still
        // appears in eclipse for debugging.
        System.err.println("Recovering...");
        ex.printStackTrace();

        // Display the appropriate message and description in the dialog
        if (ex instanceof RcvrIOSaveException) {
            interaction.reportException(IO_SAVE_EXC_TITLE,
                                        IO_SAVE_EXC_DESC,
                                        ex);
        }
        else if (ex instanceof RcvrIOLoadException) {
            interaction.reportException(IO_LOAD_EXC_TITLE,
                                        IO_LOAD_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrIOTempException) {
            interaction.reportException(IO_TEMP_EXC_TITLE,
                                        IO_TEMP_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrClipboardException) {
            interaction.reportException(CLIPBOARD_EXC_TITLE,
                                        CLIPBOARD_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrIOException) {
            interaction.reportException(IO_EXC_TITLE, IO_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrUIException) {
            interaction.reportException(UI_EXC_TITLE, UI_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrUnimplementedFnException) {
            interaction.reportException(UNIMP_EXC_TITLE, UNIMP_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrComputationException) {
            interaction.reportException(COMPUTE_EXC_TITLE,
                                        COG_MOD_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrCogModelException) {
            interaction.reportException(COG_MOD_EXC_TITLE,
                                        COG_MOD_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrImageException) {
            interaction.reportException(IMG_EXC_TITLE, IMG_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrOutOfMemoryException) {
            interaction.reportException(MEMORY_EXC_TITLE,
                                        MEMORY_EXC_DESC, ex);
        }
        else if (ex instanceof RcvrWorkThreadException) { // xyzzy
            AggregateException thrownExceptions =
                ((RcvrWorkThreadException) ex).getWorkExceptions();

            interaction.reportException(THREAD_EXC_TITLE,
                                        THREAD_EXC_DESC,
                                        thrownExceptions);
        }
        else {
            interaction.reportException(RCVR_EXC_TITLE, RCVR_EXC_DESC, ex);
        }
    }

    /**
     * Treat any exceptions thrown during the given work thread as
     * recoverable.
     */
    public static boolean recoverWorkThread(IWorkThread workThread,
                                            Interaction interaction)
    {
        AggregateException thrownExceptions = workThread.getWorkExceptions();

        if (thrownExceptions.containsExceptions()) {
            recover(new RcvrWorkThreadException(thrownExceptions),
                    interaction);

            return true;
        }

        return false;
    }
}
