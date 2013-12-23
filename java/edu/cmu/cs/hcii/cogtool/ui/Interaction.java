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

import java.io.File;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.ReportInteraction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.EnableDisable;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ProcessTraceCallback;
import edu.cmu.cs.hcii.cogtool.util.ProgressCallback;
import edu.cmu.cs.hcii.cogtool.util.StatusDisplayable;

/**
 * A collection of standard user interactions for CogTool windows.
 *
 * @author mlh
 */
public interface Interaction extends ReportInteraction, StatusDisplayable
{
    // Possible return codes for askSaveBeforeClose and protestBeingEdited

    /**
     * Indicates that the user wishes to save the CogTool project before
     * closing the project or exiting the application.
     */
    public static final int SAVE = 0;

    /**
     * Indicates that the user does not wish to save the CogTool project before
     * closing the project or exiting the application.
     */
    public static final int NO_SAVE = 1;

    /**
     * Indicates that the user wishes to cancel the closing of the project
     * or exiting the application.
     */
    public static final int CANCEL = 2;

    /**
     * Useful for status line when operation canceled.
     */
    public static final String CANCELED_MSG =
        L10N.get("II.Canceled", "Canceled");

    /**
     * Report a warning in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param msg the message reporting the warning
     * @author mlh
     */
    public void reportWarning(String title, String msg);

    /**
     * Report a set of warnings in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param messages the messages reporting the warnings
     * @author mlh
     */
    public void reportWarnings(String title, List<String> messages);

    /**
     * Report a problem in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param msg the message reporting the problem
     * @author mlh
     */
    public void reportProblem(String title, String msg);

    /**
     * Report a set of problems in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param messages the messages reporting the problems
     * @author mlh
     */
    public void reportProblems(String title, List<String> messages);

    /**
     * Report a problem in a pop-up window and allow the user to either
     * retry or cancel the affected operation; thus, buttons include
     * 'retry' and 'cancel'.
     *
     * @param title the window title
     * @param msg the message reporting the problem
     * @return true if retry was selected, false if cancel was selected
     * @author mlh
     */
    public boolean reportAndRetry(String title, String msg);

    /**
     * Request one or more files from the user in a pop-up dialog box.
     *
     * @return an array of abstract file descriptions representing the selected
     *         files
     * @author mlh
     */
    public File[] selectFileSources();

    /**
     * Request a location to save a CogTool project.
     *
     * @param projectName the name of the project being saved, which
     *                    acts as part of the initial file name
     * @return the abstract file description of where the project should
     *         be saved
     * @author mlh
     */
    public File selectFileDest(String projectName);

    /**
     * Request a location to store results.
     * Takes a default file name and extension to use
     *
     * @return the abstract file location for storing results
     * @author mlh/alex
     */
    public File selectExportLocation(String defaultFilename,
                                     String defaultExtension);

    /**
     * Protest that the specified file is currently open for a different
     * project.
     *
     * @param saveFileAttempt the file selected by the user to save a project
     *                        that happens to already be in use for a different
     *                        project being edited
     * @return NO_SAVE if retry was selected, SAVE if overwrite was selected,
     *         and CANCEL if cancel was selected
     */
    public int protestBeingEdited(File saveFileAttempt);

    /**
     * Ask the user to save a modified project having the given name
     * before actually closing it, especially during application exit.
     *
     * @param projectName the name of the project to be saved, which
     *                    acts as part of the initial file name
     * @return the code indicating the user's decision (one of
     *         <code>SAVE</code>, <code>NO_SAVE</code>, or <code>CANCEL</code>)
     * @author mlh
     */
    public int askSaveBeforeClose(String projectName);

    /**
     * The user has tried to open a file that is already open and modified, so
     * ask if they want to revert to the saved version
     *
     * @param projectName the name of the project in question
     * @return the code indicating the user's decision (either
     *         <code>SWT.OK</code> or <code>SWT.CANCEL</code>\)
     * @author rmyers
     */
    public boolean askRevertBeforeOpen(String projectName);

    /**
     * Ask the user to select a color for the widgets.
     * This should bring up the OS color wheel.
     *
     * @return new color selected by user or <code>null</code> if the user
     *         canceled
     */
    public Integer selectColor();

    /**
     * Ask the user to select a color for the widgets.
     * This should bring up the OS color wheel.
     * the parameter is the current color.
     * @param oldcolor
     * @return new color selected by user or <code>null</code> if the user
     *         canceled
     */
    public Integer selectColor(int oldcolor);

    /**
     * Ask the user to select a color for the widgets.
     * This should bring up the OS color wheel.
     * the paramaters are the current color and the
     * title of the color dialog box.
     * @param oldcolor
     * @param dialogTitle
     * @return new color selected by user or <code>null</code> if the user
     *         canceled
     */
    public Integer selectColor(int oldcolor, String dialogTitle);

    /**
     * Protest that the image file selected is invalid.
     */
    public void protestInvalidImageFile();

    /**
     * Protest that the file tried to read was unreadable
     */
    public void protestUnreadableFile();

    /**
     * Protest that the given file was not found
     */
    public void protestFileNotFound(String file);

    /**
     * Allow user to select a resource through a standard dialog
     */
    public String selectImageFile();

    /**
     * Allow user to select a .csv file through a standard dialog
     */
    public File selectCSVFile();

    /**
     * Allow user to select an .xml file through a standard dialog
     */
    public File selectXMLFile(boolean forInput, String defaultFilename);

    /**
     * Allow user to select a file with the allowed extensions through a standard dialog
     */
    public File selectFile(boolean forInput, String defaultFilename, String[] exts );


    /**
     * Allow user to select a .csv file through a standard dialog
     * to write to
     * @param projectName
     */
    public File selectCSVFileDest(String projectName);

    public File selectDirectory(String message);

    /**
     * Interface representing a visual progress bar.
     */
    public interface ProgressBar extends ProgressCallback
    {
        /**
         * Progress bar style. SMOOTH means updated by child thread.
         */
        public static final boolean SMOOTH = true;

        /**
         * Progress bar style.  INDETERMINATE means appear busy
         * since updates from the child thread may not occur.
         */
        public static final boolean INDETERMINATE = false;

        /**
         * Fetch the associated disabler; caller may use it to disable
         * (or enable) the cancel button of this dialog box.
         *
         * @return the associated disabler
         */
        public EnableDisable getDisabler();
    }

    /**
     * Interface representing a window displaying the stdout/stderr traces
     * from a work process.
     */
    public interface ITraceWindow extends ProgressBar, ProcessTraceCallback
    {
        /**
         * Fetch the associated disabler; caller may use it to disable
         * (or enable) the cancel button of this dialog box.
         *
         * @return the associated disabler
         */
        public EnableDisable getDisabler();

        public void scrollToTop();
    }

    /**
     * Provide a visual progress bar, optionally with a "Cancel" button and
     * optionally with space for status text.
     * <p>
     * IMPORTANT: Caller is responsible for invoking dispose() on the
     * returned value
     *
     * @param windowTitle the window title for the dialog box
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button; no cancel button if <code>null</code>
     * @param initialStatusText if not null, the initial text to display as
     *                          status; no status text if <code>null</code>
     * @param style true for SMOOTH and false for INDETERMINATE
     */
    public ProgressBar createProgressBar(String windowTitle,
                                          Cancelable cancelable,
                                          String initialStatusText,
                                          boolean style);

    /**
     * Provide a visual progress bar, optionally with a "Cancel" button and
     * optionally with space for status text.
     * <p>
     * IMPORTANT: Caller is responsible for invoking dispose() on the
     * returned value
     *
     * @param windowTitle the window title for the dialog box
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button; no cancel button if <code>null</code>
     * @param initialStatusText if not null, the initial text to display as
     *                          status; no status text if <code>null</code>
     * @param style true for SMOOTH and false for INDETERMINATE
     * @param ratio the ratio desired for inserting an ellipsis into the
     *              status string (see StringUtil constants)
     */
    public ProgressBar createProgressBar(String windowTitle,
                                          Cancelable cancelable,
                                          String initialStatusText,
                                          boolean style,
                                          double ratio);

    /**
     * Provide a trace window, optionally with a "Cancel" button and
     * optionally with space for status text.
     * <p>
     * IMPORTANT: Caller is responsible for invoking dispose() on the
     * returned value
     *
     * @param windowTitle the window title for the dialog box
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button; no cancel button if <code>null</code>
     * @param initialStatusText if not null, the initial text to display as
     *                          status; no status text if <code>null</code>
     */
    public ITraceWindow createTraceWindow(String windowTitle,
                                          Cancelable cancelable,
                                          String initialStatusText);

    /**
     * TODO: change the comments of this method
     * Pop a file save selection dialog to allow the user to specify a
     * destination directory to save the output of creating all the needed
     * HTML pages & images.
     * @return
     */
    public String askUserForDirectory();


    public String askUserForDirectory(String title, String message);

    /**
     * Pop a dialog box with the given project's properties (e.g., name and
     * build version).
     *
     * @param project the project whose properties to display
     */
    public void showProjectProperties(Project project);

    public void protestObsoleteWaits();

    public void protestTraceVersion(boolean tooOld);

    /**
     * Pop a dialog box to edit preferences; returns true if not canceled.
     */
    public boolean editPreferences();

    /**
     * Data needed when creating/editing a design; the device type set
     * consists of <code>DeviceType</code> instances.
     */
    public static class DesignRequestData
    {
        public String designName;
        public Set<DeviceType> deviceTypes;
    }

    /**
     * Request new information (name, device type set) about a new or
     * existing design.  The given data initializes the values for
     * the interaction.
     *
     * @param requestData the initial values for the dialog box
     * @param newProjectMode determines if the window should ask for the
     *                        project name or not
     * @param project the user's project
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author afaaborg
     */
    public boolean requestNewDesignName(DesignRequestData requestData,
                                        boolean newProjectMode,
                                        Project project);

    public boolean requestNewDesignName(DesignRequestData requestData,
                                        boolean newProjectMode,
                                        Project project, boolean changeDesignName);

    /**
     * Request new devices for the given design
     *
     * @param requestData to hold the return values for the dialog box
     * @param project the user's project
     * @param design the design to which to add devices
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     */
    public boolean requestAddDesignDevices(DesignRequestData requestData,
                                           Project project,
                                           Design design);

    /**
     * Protest that the user's selected name collides with another in the model.
     * @param type type of the object being renamed
     *
     * @return new value if one specified or <code>null</code> if user canceled
     */
    public String protestNameCollision(String type);

    /**
     * Protest that an object's name cannot be empty.
     * @param type type of the object being renamed
     *
     * @return new value if one specified or <code>null</code> if user canceled
     */
    public String protestNameCannotBeEmpty(String type);

    /**
     * Complain that one or more frames have a hidden button (Hidden hot-spot)
     *
     * @return true if the user would like to ignore hidden buttons, and continue;
     *         false if the user would like to stop the export2html process
     *
     * @author khaledziyaeen@gmail.com
     */
    public boolean protestHiddenButtons(String message);

}
