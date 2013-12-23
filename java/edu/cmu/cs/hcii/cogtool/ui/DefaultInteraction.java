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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolFileTypes;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.DisplaySubprocessTrace;
import edu.cmu.cs.hcii.cogtool.util.EnableDisable;
import edu.cmu.cs.hcii.cogtool.util.ErrorDialog;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.Pausable;
import edu.cmu.cs.hcii.cogtool.util.PausableProgessBar;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.Stoppable;
import edu.cmu.cs.hcii.cogtool.util.StoppableProgressBar;
import edu.cmu.cs.hcii.cogtool.util.ThreadProgressBar;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.PersistenceView;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * Default implementation of the standard user interactions for
 * CogTool windows.
 *
 * @author mlh
 */
public class DefaultInteraction implements Interaction
{
    /*
     * List of commonly used strings. Stored statically here for more easily
     * readable code later.
     */

    protected static final String errorTitle =
        L10N.get("DEFINT.Error",
                 "File Error");

    protected static final String genericErrorTitle =
        L10N.get("DEFINT.GenericError", "Error");

    protected static final String unreadableFileMsg =
        L10N.get("DEFINT.CannotReadFile",
                 "The selected file cannot be read.");

    protected static final String invalidImageFileMsg =
        L10N.get("DEFINT.InvalidImageFile",
                 "The selected file does not contain a valid image.");

    protected static final String fileNotFoundMsg =
        L10N.get("DEFINT.FileNotFound",
                 "File not found:") + " ";

    protected static final String selectWebPageExportDialogTitle =
        L10N.get("PM.webpageExportDialogTitle",
                 "Select a Directory to Export the Design Into");

    protected static final String selectWebPageExportDialogMessage =
        L10N.get("PM.webpageExportDialogMessage",
                 "The exported design will be added to this directory.  " +
                      "Existing files with conflicting names will be replaced.");

    protected static final String projectPropertiesTitle =
        L10N.get("DI.ProjectPropertiesTitle", "Project Properties");

    protected static final String buildVersionLabel =
        L10N.get("DI.BuildVersionLabel",
                 "Project file last written by CogTool version");

    protected static final String unknownBuildVersion =
        L10N.get("DI.UnknownBuildVersion", "Unknown build version");

    protected static final String unsavedProject =
        L10N.get("DI.UnsavedProject", "Unsaved project");

    protected static final String fileInUse =
        L10N.get("DI.FileInUse",
                 "The file selected is being used by another project being edited:");

    protected static final String containsObsoleteWait =
        L10N.get("DI.ObsoleteWaits",
                 "The design(s) contain(s) an old form of system wait that is no longer supported, and so the script(s) cannot be computed. " +
                 "Please recast the design(s) with the system waits attached to transitions.");

    protected static final String traceVersionTooOld =
        L10N.get("DI.TraceVersionTooOld",
                 "The version of CogTool used to compute this task was too old for this version to visualize. Please recompute the task.");

    protected static final String traceVersionTooNew =
        L10N.get("DI.TraceVersionTooNew",
                 "The version of CogTool used to compute this task was newer than this version, and the result cannot be visualized by this version. Please recompute the task.");

    protected static final String nameCollisionMsg =
        L10N.get("FE.WidgetNameCollision",
                 "The name you have chosen is already in use.\n" +
                 "Please choose a different name.");

    protected static final String nameEmptyMsg =
        L10N.get("FE.WidgetNameEmpty",
                 "A name cannot be empty.\n" +
                 "Please provide a name.");

    protected View view;
    protected Shell window;
    protected PersistenceView persistView;

    protected int csvNum = 1;

    /**
     * Initialize interaction with the given window that pop-up dialog
     * boxes are modal to.  Also initializes the CogTool functionality
     * for requesting file names for opening and saving projects.
     *
     * @param win target window for dialog box modality
     * @author mlh
     */
    public DefaultInteraction(View v)
    {
        view = v;
        window = v.getShell();
        persistView = new PersistenceView(window);
    }

    /**
     * Report a warning in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param msg the message reporting the warning
     * @author mlh
     */
    public void reportWarning(String title, String msg)
    {
        try {
            WindowUtil.presentWarningDialog(window, title, msg);
        }
        catch (SWTException ex) {
            throw new RcvrUIException("Creation of dialog box failed.", ex);
        }
    }

    /**
     * Report a set of warnings in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param messages the messages reporting the warnings
     * @author mlh
     */
    public void reportWarnings(String title, List<String> messages)
    {
        StringBuilder msgBuffer = new StringBuilder();
        String separator = "";

        Iterator<String> warnings = messages.iterator();

        while (warnings.hasNext()) {
            msgBuffer.append(separator);
            msgBuffer.append(warnings.next());

            separator = "\n";
        }

        reportWarning(title, msgBuffer.toString());
    }

    /**
     * Report a problem in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param msg the message reporting the problem
     * @author mlh
     */
    public void reportProblem(String title, String msg)
    {
        try {
            WindowUtil.presentErrorDialog(window, title, msg);
        }
        catch (SWTException ex) {
            throw new RcvrUIException("Creation of dialog box failed.", ex);
        }
    }

    /**
     * Report a set of problems in a pop-up window with an 'ok' button.
     *
     * @param title the window title
     * @param messages the messages reporting the problems
     * @author mlh
     */
    public void reportProblems(String title, List<String> messages)
    {
        StringBuilder msgBuffer = new StringBuilder();
        String separator = "";

        Iterator<String> problems = messages.iterator();

        while (problems.hasNext()) {
            msgBuffer.append(separator);
            msgBuffer.append(problems.next());

            separator = "\n";
        }

        reportProblem(title, msgBuffer.toString());
    }

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
    public boolean reportAndRetry(String title, String msg)
    {
        return SWT.RETRY == WindowUtil.presentErrorAbortDialog(window,
                                                               title,
                                                               msg);
    }

    /**
     * Request one or more files from the user in a pop-up dialog box.
     *
     * @return an array of abstract file descriptions representing the selected
     *         files
     * @author mlh
     */
    public File[] selectFileSources()
    {
        return persistView.selectFileSources();
    }

    /**
     * Request a location to save a CogTool project.
     *
     * @param projectName the name of the project being saved, which
     *                    acts as part of the initial file name
     * @return the abstract file description of where the project should
     *         be saved
     * @author mlh
     */
    public File selectFileDest(String projectName)
    {
        // Specify the extension for projects.
        return selectExportLocation(projectName,
                                    CogToolFileTypes.COGTOOL_FILE_EXT);
    }

    protected static String OVERWRITE = L10N.get("DI.Overwrite", "Overwrite");
    protected static String SELECT_AGAIN =
        L10N.get("DI.SelectAgain", "Select another file");
    protected static String CANCEL_SAVE = L10N.get("DI.Cancel", "Cancel");

    protected class BeingEditedDialog extends WindowUtil.SimpleDialog
    {
        protected File destination;

        public BeingEditedDialog(File saveFileAttempt)
        {
            super(window, errorTitle, SWT.PRIMARY_MODAL, SWT.DIALOG_TRIM);

            destination = saveFileAttempt;
        }

        protected void addLabel(String text, int span, int width)
        {
            Label element = new Label(dialog, SWT.NONE);

            if (text != null) {
                element.setText(text);

                if (textFont != null) {
                    element.setFont(textFont);
                }
            }

            GridData eltLayout = new GridData();

            if (span > 0) {
                eltLayout.grabExcessHorizontalSpace = true;

                eltLayout.horizontalSpan = span;
            }
            else {
                eltLayout.widthHint = width;
            }

            element.setLayoutData(eltLayout);
        }

        protected void addButton(final String label)
        {
            Button nextButton = new Button(dialog, SWT.PUSH);

            nextButton.setText(label);

            if (buttonFont != null) {
                nextButton.setFont(buttonFont);
            }

            GridData buttonLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_END);

            buttonLayout.horizontalSpan = 2;
            nextButton.setLayoutData(buttonLayout);

            nextButton.addListener(SWT.Selection,
                                   new Listener() {
                                       public void handleEvent(Event evt)
                                       {
                                           userResponse = label;
                                           dialog.close();
                                       }
                                   });
        }

        @Override
        protected void buildDialog()
        {
            // Four columns
            GridLayout layout = new GridLayout(8, false);

            layout.marginLeft = 19;
            layout.marginRight = 13;
            layout.marginTop = 7;
            layout.marginBottom = 12;

            dialog.setLayout(layout);

            addLabel(fileInUse, 8, 0);
            addLabel(null, 0, 35);
            addLabel(destination.getName(), 7, 0);
            addLabel(null, 8, 0);
            addLabel(null, 0, 35);

            // Add "Overwrite" button
            addButton(OVERWRITE);
            addButton(SELECT_AGAIN);
            addButton(CANCEL_SAVE);
        }
    }

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
    public int protestBeingEdited(File saveFileAttempt)
    {
        BeingEditedDialog dialog = new BeingEditedDialog(saveFileAttempt);

        Object response = dialog.open();

        if (response == OVERWRITE) {
            return SAVE;
        }

        if (response == SELECT_AGAIN) {
            return NO_SAVE;
        }

        return CANCEL;  // must be CANCEL_SAVE
    }

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
    public int askSaveBeforeClose(String projectName)
    {
        MessageBox saveBeforeCloseDialog =
            WindowUtil.createYesNoCancelDialog
                                (window,
                                 L10N.get("PM.SaveBeforeCloseTitle",
                                          "Save Changes?"),
                                 L10N.get("PM.SaveBeforeClose",
                                          "Save project before closing?\n"
                                              + "Unsaved changes in project:"
                                          ) + "\n    " + projectName);

        switch (saveBeforeCloseDialog.open()) {
            case SWT.YES: {
                return SAVE;
            }
            case SWT.NO: {
                return NO_SAVE;
            }
            case SWT.CANCEL: {
                return CANCEL;
            }
        }

        return CANCEL; // TODO: exception instead; should never get here
    }

    /**
     * The user has tried to open a file that is already open and modified, so
     * ask if they want to revert to the saved version
     *
     * @param projectName the name of the project in question
     * @return the code indicating the user's decision (either
     *         <code>SWT.OK</code> or <code>SWT.CANCEL</code>\)
     * @author rmyers
     */
    public boolean askRevertBeforeOpen(String projectName)
    {
        MessageBox revertBeforeOpenDialog =
            WindowUtil.createConfirmDialog
                                (window,
                                 L10N.get("PM.RevertBeforeOpenTitle",
                                          "Revert?"),
                                 L10N.get("PM.RevertBeforeOpen",
                                          projectName + " " + "is already open "
                                              + "and has been modified since "
                                              + "it was last saved.\n"
                                              + "Revert changes?"));

        return revertBeforeOpenDialog.open() == SWT.OK;
    }

    /**
     * Open up a widget color wheel with the specified title.
     * Should be platform specific.
     * the specified integer should be in the following format.
     * lowest order 8 bits red
     * middle order 8 bits green
     * highest order 8 bits blue
     * ie: xxxxxxxx xxxxxxxx  xxxxxxxx
     *     BLUE     GREEN     RED
     */
    public Integer selectColor(int oldColor, String dialogTitle)
    {
        RGB old = GraphicsUtil.getRGBFromColor(oldColor);

        // Open the platform specific color chooser
        ColorDialog dialog = new ColorDialog(window);
        dialog.setRGB(old);
        if (dialogTitle != null)
        {
            dialog.setText(dialogTitle);
        }
        RGB newColor = dialog.open();

        if (newColor != null) {
            return new Integer(GraphicsUtil.getColorFromRGB(newColor));
        }

        return null;
    }

    /**
     * Open up a widget color wheel.
     * Should be platfrom specific.
     * the specfied integer should be in the following format.
     * lowest order 8 bits red
     * middle order 8 bits green
     * highest order 8 bits blue
     * ie: xxxxxxxx xxxxxxxx  xxxxxxxx
     *     BLUE     GREEN     RED
     */
    public Integer selectColor(int oldColor)
    {
        return selectColor(oldColor, null);
    }

    /**
     * Open up a widget color wheel.
     * Should be platorm specific.
     */
    public Integer selectColor()
    {
        return selectColor(0);
    }

    /**
     * A file IO error: The selected file was unreadable
     */
    public void protestUnreadableFile()
    {
        reportProblem(errorTitle, unreadableFileMsg);
    }

    /**
     * The selected file is not a supported image type
     */
    public void protestInvalidImageFile()
    {
        reportProblem(errorTitle, invalidImageFileMsg);
    }

    public void protestFileNotFound(String file)
    {
        reportProblem(errorTitle, fileNotFoundMsg + file);
    }

    /**
     * Returns null if canceled.
     */
    protected FileDialog selectFileName(int mode,
                                        String[] filterNames,
                                        String[] extensions)
    {
        return selectFileName(mode, filterNames, extensions, null);
    }

    protected FileDialog selectFileName(int mode,
                                        String[] filterNames,
                                        String[] extensions,
                                        String def)
    {
        FileDialog dialog = new FileDialog(window, mode);

        if (def != null) {
            dialog.setFileName(def);
        }

        dialog.setFilterNames(filterNames);

        // Windows wild cards
        dialog.setFilterExtensions(extensions);

        if (dialog.open() != null) {
            return dialog;
        }

        return null;    // canceled!
    }

    /**
     * A file selection dialog for selecting images.
     * It filters the list based on JPG, GIF, PNG, BMP and ICO files
     * An option is also available to show all.
     */
    public String selectImageFile()
    {
        // TODO: provide an "All Images" option when supported by SWT
        FileDialog dialog =
            selectFileName(SWT.OPEN,
                           new String[] { "All Readable (*.jpg;*.gif;*.png;*.bmp;*.ico)",
                                          "JPG Images (*.jpg)",
                                          "GIF Images (*.gif)",
                                          "PNG Images (*.png)",
                                          "BMP Images (*.bmp)",
                                          "ICO Icon Files (*.ico)",
                                          "All Files (*.*)" },
                           new String[] { "*.jpg;*.gif;*.png;*.bmp;*.ico",
                                          "*.jpg",
                                          "*.gif",
                                          "*.png",
                                          "*.bmp",
                                          "*.ico",
                                          "*.*" });
        if (dialog != null) {
            return dialog.getFilterPath() + File.separator
                                          + dialog.getFileName();
        }

        return null;
    }

    /**
     * Selects a csv file
     *
     * @see edu.cmu.cs.hcii.cogtool.ui.Interaction#selectCSVFile(java.lang.String)
     */
    public File selectCSVFile()
    {
        FileDialog dialog =
            selectFileName(SWT.OPEN,
                           new String[] { "CSV Files (*.csv)" },
                           new String[] { "*.csv" });

        if (dialog == null) {
            return null;
        }

        File path = new File(dialog.getFilterPath());

        return new File(path, dialog.getFileName());
    }

    public File selectXMLFile(boolean forInput, String def)
    {
        FileDialog dialog =
            selectFileName(forInput ? SWT.OPEN : SWT.SAVE,
                           new String[] { "XML Files (*.xml)" },
                           new String[] { "*.xml"},
                           def);

        if (dialog == null) {
            return null;
        }

        File path = new File(dialog.getFilterPath());

        return new File(path, dialog.getFileName());
    }

    public File selectFile(boolean forInput, String def, String[] allowedExtensions)
    {
        FileDialog dialog =
            selectFileName(forInput ? SWT.OPEN : SWT.SAVE,
                           new String[] { "XML Files (*.xml)" },
                           allowedExtensions,
                           def);

        if (dialog == null) {
            return null;
        }

        File path = new File(dialog.getFilterPath());

        return new File(path, dialog.getFileName());
    }


    protected static final String exportCanceledMsg =
        L10N.get("DEFINT.ExportCanceled", "Export canceled.");

    /**
     * A file selection dialog for selecting .csv files
     * to write to.
     */
    public File selectCSVFileDest(String defaultFileName)
    {
        FileDialog dialog =
            selectFileName(SWT.SAVE,
                           new String[] { "CSV Files (*.csv)" },
                           new String[] { "*.csv" },
                           (defaultFileName + ".csv"));

        if (dialog == null) {
            setStatusMessage(exportCanceledMsg);
            return null;
        }

        File path = new File(dialog.getFilterPath());

        String name = dialog.getFileName();

        if (! name.endsWith(".csv")) {
            name += ".csv";
        }

        File dest = new File(path, name);

        if (dest.exists() && OSUtils.WINDOWS) {
            int choice = shouldReplaceFile(dest);

            if (choice == SWT.NO) {
                return selectCSVFileDest(defaultFileName);
            }

            if (choice == SWT.CANCEL) {
                setStatusMessage(exportCanceledMsg);
                return null;
            }
        }

        return dest;
    }


    /**
     * Request a location to store result trace lines, etc.
     *
     * @return the abstract file location for storing some kind of result
     * @author mlh/alex
     */
    public File selectExportLocation(String defaultFilename,
                                     String defaultExtension)
    {
        return persistView.selectFileDest(defaultFilename,
                                               defaultExtension);
    }


    protected int shouldReplaceFile(File file)
    {
        MessageBox replaceBox =
            new MessageBox(window, SWT.YES | SWT.NO | SWT.CANCEL
                                    | SWT.ICON_QUESTION | SWT.PRIMARY_MODAL);
        replaceBox.setMessage(L10N.get("DEFINT.ReplaceFile",
                                       "File exists.  Replace file?\n")
                              + file.getAbsolutePath());
        return replaceBox.open();
    }
    
    // TODO This whole mishmash of different flavors of progress bars, and
    //      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
    //      the type hierarchy to fiddle this stuff. Instead we should have a
    //      single interface for control of a background operation, subsuming
    //      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
    //      type that takes a bunch of flags indicating what buttons it should
    //      display.

    /**
     * Provide a visual progress bar, optionally with a "Cancel" button and
     * optionally with space for status text.
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
                                          boolean style)
    {
        return createProgressBar(windowTitle,
                                 null,
                                 null,
                                 cancelable,
                                 initialStatusText,
                                 style,
                                 null);
    }

    /**
     * Provide a visual progress bar, optionally with a "Cancel" button and
     * optionally with space for status text.
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
                                          double ratio)
    {
        return createProgressBar(windowTitle,
                                 null,
                                 null,
                                 cancelable,
                                 initialStatusText,
                                 style,
                                 null,
                                 ratio);
    }
    
    public ProgressBar createProgressBar(String windowTitle,
                                         Pausable pausable,
                                         Stoppable stoppable,
                                         Cancelable cancelable,
                                         String initialStatusText,
                                         boolean style,
                                         double ratio)
   {
       return createProgressBar(windowTitle,
                                pausable,
                                stoppable,
                                cancelable,
                                initialStatusText,
                                style,
                                null,
                                ratio);
   }


    /**
     * Provide a visual progress bar, optionally with a "Cancel" button and
     * optionally with space for status text.
     * <p>
     * IMPORTANT: Caller is responsible for invoking dispose() on the
     * returned value
     *
     * @param windowTitle the window title for the dialog box
     * @param pausable the "process" may be paused using the pause button;
     *                 none if <code>null</code>
     * @param cancelable the "process" that may be canceled using the cancel
     *                   button; no cancel button if <code>null</code>
     * @param initialStatusText if not null, the initial text to display as
     *                          status; no status text if <code>null</code>
     * @param style true for SMOOTH and false for INDETERMINATE
     * @param win which window (if any) to be "modal" behavior against
     */
    public ProgressBar createProgressBar(String windowTitle,
                                          Pausable pausable,
                                          Stoppable stoppable,
                                          Cancelable cancelable,
                                          String initialStatusText,
                                          boolean style,
                                          Shell win)
    {
    	return createProgressBar(windowTitle,
    			                 pausable,
    			                 stoppable,
    			                 cancelable,
    			                 initialStatusText,
    			                 style,
    			                 win,
    			                 -1.0);
    }

    public ProgressBar createProgressBar(String windowTitle,
    			                          Pausable pausable,
    			                          Stoppable stoppable,
    			                          Cancelable cancelable,
    			                          String initialStatusText,
    			                          boolean style,
    			                          Shell win,
    			                          double ratio)
    {
        if (pausable != null && stoppable != null) {
            throw new IllegalStateException(
                "ProgressBars that are both Pausable and Stoppable are not implemented");
        }
        ThreadProgressBar pb = null;
        if (pausable != null) {
            pb = new PausableProgessBar(win,
                                        windowTitle,
                                        pausable,
                                        cancelable,
                                        initialStatusText,
                                        style ? SWT.SMOOTH
                                              : SWT.INDETERMINATE);
        } else if (stoppable != null) {
            pb = new StoppableProgressBar(win,
                                          windowTitle,
                                          stoppable,
                                          cancelable,
                                          initialStatusText,
                                          style ? SWT.SMOOTH
                                                : SWT.INDETERMINATE);
        } else {
            pb = new ThreadProgressBar(win,
                                       windowTitle,
                                       cancelable,
                                       initialStatusText,
                                       style ? SWT.SMOOTH : SWT.INDETERMINATE,
                                             ratio);
        }
        final ThreadProgressBar progressBar = pb;

        progressBar.buildAndDisplay();

        return new ProgressBar()
            {
                public void dispose()
                {
                    progressBar.dispose();
                }

                public EnableDisable getDisabler()
                {
                    return progressBar.getDisabler();
                }

                public void updateProgress(double progress, String status)
                {
                    progressBar.updateProgress(progress, status);
                }
            };
    }

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
                                          String initialStatusText)
    {
        return createTraceWindow(windowTitle, cancelable, initialStatusText,
                                 null);
    }

    public ITraceWindow createTraceWindow(String windowTitle,
                                          Cancelable cancelable,
                                          String initialStatusText,
                                          Shell parentWindow)
    {
        final DisplaySubprocessTrace traceWindow =
            new DisplaySubprocessTrace(parentWindow,
                                       windowTitle,
                                       900,
                                       700,
                                       initialStatusText,
                                       cancelable);

        traceWindow.display();

        return new ITraceWindow() {

            public EnableDisable getDisabler()
            {
                return traceWindow.getDisabler();
            }

            public void updateProgress(double progress, String status)
            {
                traceWindow.updateProgress(progress, status);
            }

            public void dispose()
            {
                traceWindow.dispose();
            }

            public void appendOutputLine(String outputLine)
            {
                traceWindow.appendOutputLine(outputLine);
            }

            public void appendOutputLines(List<String> outputLines)
            {
                traceWindow.appendOutputLines(outputLines);
            }

            public void appendErrorLine(String errorLine)
            {
                traceWindow.appendErrorLine(errorLine);
            }

            public void appendErrorLines(List<String> errorLines)
            {
                traceWindow.appendErrorLines(errorLines);
            }

            public void scrollToTop()
            {
                traceWindow.scrollToTop();
            }
        };
    }

    /**
     * Creates a directory chooser dialog, with the appropriate messages
     * about what the user should be trying to do.
     */
    public String askUserForDirectory()
    {
        return askUserForDirectory(selectWebPageExportDialogTitle,selectWebPageExportDialogMessage);
    }

    /**TODO
     * Creates a directory chooser dialog, with the appropriate messages
     * about what the user should be trying to do.
     */
    public String askUserForDirectory(String title, String message)
    {
        DirectoryDialog dialog = new DirectoryDialog(window);
        dialog.setText(title);
        dialog.setMessage(message);
        return dialog.open();
    }


    /**
     * Pop a dialog box with the given project's properties (e.g., name and
     * build version).
     *
     * @param project the project whose properties to display
     */
    public void showProjectProperties(Project project)
    {
        String buildVersion = project.getBuildVersion();

        if (buildVersion == Project.NOT_YET_SAVED) {
            buildVersion = unsavedProject;
        }
        else if (buildVersion == Project.UNKNOWN_BUILD_VERSION) {
            buildVersion = unknownBuildVersion;
        }

        // TODO the following won't localize cleanly!
        WindowUtil.presentInformationDialog(window,
                                            projectPropertiesTitle
                                                + ": "
                                                + project.getName(),
                                            buildVersionLabel
                                                + ": "
                                                + buildVersion);
    }

    public void setStatusMessage(String message)
    {
        view.setStatusMessage(message);
    }

    public void setStatusMessage(String message, int duration)
    {
        view.setStatusMessage(message, duration);
    }

    public File selectDirectory(String message)
    {
        DirectoryDialog saveDialog = new DirectoryDialog(window);
        saveDialog.setMessage(message);

        String path = saveDialog.open();

        if (path != null) {
            return new File(path);
        }

        return null;
    }

    public void protestObsoleteWaits()
    {
        reportProblem(genericErrorTitle, containsObsoleteWait);
    }

    public void protestTraceVersion(boolean tooOld)
    {
        if (tooOld) {
            reportProblem(genericErrorTitle, traceVersionTooOld);
        } else {
            reportProblem(genericErrorTitle, traceVersionTooNew);
        }
    }

    /* (non-Javadoc)
     * @see edu.cmu.cs.hcii.cogtool.ui.IInteraction#reportException(java.lang.String, java.lang.String, java.lang.Exception)
     */
    public void reportException(String title, String msg, Exception e)
    {
        ErrorDialog dlog = new ErrorDialog(title, msg, e);
        dlog.open();
    }

    public boolean editPreferences()
    {
        PreferencesDialog dialog = new PreferencesDialog(window, this);
        return dialog.open() != null;
    }

    protected static final Color WHITE =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_WHITE);

    /**
     * Dialog box specific to soliciting information for a design
     * (name, device type set).
     *
     * @author afaaborg
     */
    public static class NewDesignDialog extends WindowUtil.CustomDialog
    {
        protected static final String NEW_DESIGN_FOR_TITLE =
            L10N.get("PI.NewDesignForTitle", "New Design for");

        protected static final String NEW_DESIGN_TITLE =
            L10N.get("PI.NewDesignTitle", "New Design");

        protected static final String ADD_DEVICES_TITLE =
            L10N.get("PI.AddDevicesTitle", "Add Devices to Design");

        protected static final String CALLED_UNTIL_SAVED_MSG =
            L10N.get("PI.CalledUntilSavedMsg",
                     "Until you save it, the new project will be called:");

        protected static final String PLEASE_ENTER_NAME_MSG =
            L10N.get("PI.PleaseEnterNameMsg",
                     "Because each project must have at least one design,\nplease enter the name of an initial design:");

        protected static final String NAME_FOR_NEW_DESIGN =
            L10N.get("PI.NameForNewDesign", "Name for the new design") + ":";

        protected static final String ACTION_CANNOT_BE_UNDONE =
            L10N.get("PI.ActionCannotBeUndone",
                     "Please note that adding devices cannot be undone.");

        protected static final String DESIGN_NAME_FOR_ADDED_DEVICES =
            L10N.get("PI.DesignNameForAddedDevices",
                     "Design to which to add new devices") + ":";

        protected static final String INPUT_DEVICES_TO_USE =
            L10N.get("PI.InputDevicesToUse",
                     "Input devices used in this design (at least one must be selected):");

        protected static final String INPUT_DEVICES_TO_ADD =
            L10N.get("PI.InputDevicesToAdd",
                     "Input devices to add to this design:");

        protected static final String OUTPUT_DEVICES_TO_USE =
            L10N.get("PI.OutputDevicesToUse",
                     "Output devices used in this design (at least one must be selected):");

        protected static final String OUTPUT_DEVICES_TO_ADD =
            L10N.get("PI.OutputDevicesToAdd",
                     "Output devices to add to this design:");

        protected DesignRequestData requestData;
        protected boolean newProject;
        protected Project userProject;
        protected Design existingDesign;

        //user input
        protected ManagedText designNameInput;
        protected Composite inputDeviceComposite;
        protected Composite outputDeviceComposite;

        protected String promptResponse;
        protected boolean importDialog;

        /**
         * Initialize dialog box modal to the given parent window
         * and with initial values specified by the given design request data.
         *
         * @param parentWin window the dialog box should be modal to
         * @param data the initial values for the design information
         * @author afaaborg
         */
        public NewDesignDialog(Shell parentWin,
                               DesignRequestData data,
                               boolean newProjectMode,
                               Project project)
        {
            super(parentWin,
                  newProjectMode
                      ? (NEW_DESIGN_FOR_TITLE + " " + project.getName())
                      : NEW_DESIGN_TITLE,
                  SWT.PRIMARY_MODAL);

            newProject = newProjectMode;
            userProject = project;
            existingDesign = null;
            requestData = data;

            //Set default devices:
            //Iterate through all of the designs in the project and
            //see if they have the same set of devices.  If so, default to
            //that set of devices
            List<Design> designs = userProject.getDesigns();
            Set<DeviceType> initialDevices = new HashSet<DeviceType>();
            boolean defaultDevices = true;

            for (int i = 0; i < designs.size(); i++) {
                Design design = designs.get(i);
                Set<DeviceType> devices = design.getDeviceTypes();

                // The devices set to compare future device sets against
                if (i == 0) {
                    initialDevices.addAll(devices);
                }
                else {
                    // Compare this set to the first
                    if (! devices.equals(initialDevices)) {
                        defaultDevices = false;
                    }
                }
            }

            if (defaultDevices) {
                requestData.deviceTypes = initialDevices;
            }
        }

        public NewDesignDialog(Shell parentWin,
                               DesignRequestData data,
                               boolean newProjectMode,
                               Project project, boolean changeDesignName)
        {
            super(parentWin,
                  newProjectMode
                      ? (NEW_DESIGN_FOR_TITLE + " " + project.getName())
                      : NEW_DESIGN_TITLE,
                  SWT.PRIMARY_MODAL);

            newProject = newProjectMode;
            userProject = project;
            existingDesign = null;
            requestData = data;
            importDialog = true;

        }

        public NewDesignDialog(Shell parentWin,
                               DesignRequestData data,
                               Project project,
                               Design design)
        {
            super(parentWin, ADD_DEVICES_TITLE, SWT.PRIMARY_MODAL);

            requestData = data;
            newProject = false;
            existingDesign = design;
            userProject = project;

            // The deviceTypes in requestData get the known devices from design
            requestData.deviceTypes = new HashSet<DeviceType>(design.getDeviceTypes());
            requestData.designName = design.getName();
        }

        public String getPromptResponse()
        {
            return promptResponse;
        }

        @Override
        protected void onOK()
        {
            promptResponse = designNameInput.getText();
            requestData.designName = promptResponse;

            super.onOK();
        }

        protected Composite createUI()
        {
            Composite userInterface = new Composite(dialog, SWT.NONE);
            userInterface.setLayout(new FormLayout());

            Composite newDesignUI;
            Composite newProjectUI;
            FormData formData;

            if (newProject) {
                newProjectUI = new Composite(userInterface, SWT.NONE);
                newProjectUI.setLayout(new FillLayout(SWT.VERTICAL));

                formData = new FormData();
                formData.right = new FormAttachment(100, 0);
                formData.left = new FormAttachment(0, 0);
                newProjectUI.setLayoutData(formData);

                Label initialDesignLabel = new Label(newProjectUI, SWT.WRAP);
                //initialDesignLabel.setText("This initial design will be added to your new project");
                initialDesignLabel.setText(CALLED_UNTIL_SAVED_MSG + " '"
                                              + userProject.getName()
                                              + "'");

                newDesignUI = new Composite(userInterface, SWT.NONE);
                newDesignUI.setLayout(new FormLayout());

                formData = new FormData();
                formData.bottom = new FormAttachment(100, 0);
                formData.right = new FormAttachment(100, 0);
                formData.left = new FormAttachment(0, 0);
                formData.top = new FormAttachment(newProjectUI, 30, SWT.TOP);
                newDesignUI.setLayoutData(formData);
            }
            else {
                newDesignUI = new Composite(userInterface, SWT.NONE);
                newDesignUI.setLayout(new FormLayout());

                formData = new FormData();
                formData.bottom = new FormAttachment(100, 0);
                formData.right = new FormAttachment(100, 0);
                formData.left = new FormAttachment(0, 0);
                newDesignUI.setLayoutData(formData);
            }

            Label designNameLabel = new Label(newDesignUI, SWT.NONE);
            formData = new FormData();
            formData.top = new FormAttachment(0, 0);
            formData.left = new FormAttachment(0, 0);
            designNameLabel.setLayoutData(formData);

            if (newProject) {
                designNameLabel.setText(PLEASE_ENTER_NAME_MSG);
            }
            else if (existingDesign == null) {
                designNameLabel.setText(NAME_FOR_NEW_DESIGN);
            }
            else {
                designNameLabel.setText(ACTION_CANNOT_BE_UNDONE
                                           + "\n"
                                           + DESIGN_NAME_FOR_ADDED_DEVICES);
            }

            int designNameStyle = SWT.BORDER;

            if (existingDesign != null) {
                designNameStyle |= SWT.READ_ONLY;
            }

            //Code in order to disable the input text
           /* if(importDialog && requestData.designName != null)
            {
                designNameStyle |= SWT.READ_ONLY;
            }*/
            designNameInput = new ManagedText(newDesignUI,
                                                   designNameStyle,
                                                   Keypad.FULL_KEYPAD)
            {
                @Override
                protected void onModify()
                {
                    enableOKButtonCheck();
                }

                @Override
                public void selectAll()
                {
                    if (existingDesign == null) {
                        super.selectAll();
                    }
                }
            };

            if (existingDesign != null) {
//                this.designNameInput.setEnabled(false);
                designNameInput.setBackground(WHITE);
            }

            if(importDialog && requestData.designName != null)
            {

               //this.designNameInput.setEnabled(false);
                designNameInput.setText(requestData.designName);

            }

            formData = new FormData();
            formData.right = new FormAttachment(100, -20);
            formData.top =
                new FormAttachment(designNameLabel, 10, SWT.DEFAULT);
            //formData_4.left = new FormAttachment(0, 80);
            formData.left = new FormAttachment(0, 20);
            formData.width = 200;
            designNameInput.setLayoutData(formData);

            Label devicesLabel = new Label(newDesignUI, SWT.NONE);
            devicesLabel.setText((existingDesign == null)
                                      ? INPUT_DEVICES_TO_USE
                                      : INPUT_DEVICES_TO_ADD);

            formData = new FormData();
            formData.top =
                new FormAttachment(designNameInput.getOuter(), 32, SWT.DEFAULT);
            formData.left = new FormAttachment(0, 0);
            devicesLabel.setLayoutData(formData);

            inputDeviceComposite = new Composite(newDesignUI, SWT.BORDER);
            inputDeviceComposite.setLayout(new GridLayout());

            formData = new FormData();
            formData.right = new FormAttachment(100, -20);
            formData.top = new FormAttachment(devicesLabel, 10, SWT.DEFAULT);
            formData.left = new FormAttachment(0, 20);
            inputDeviceComposite.setLayoutData(formData);

            devicesLabel = new Label(newDesignUI, SWT.NONE);
            devicesLabel.setText((existingDesign == null)
                                      ? OUTPUT_DEVICES_TO_USE
                                      : OUTPUT_DEVICES_TO_ADD);

            formData = new FormData();
            formData.top =
                new FormAttachment(inputDeviceComposite, 15, SWT.DEFAULT);
            formData.left = new FormAttachment(0, 0);
            devicesLabel.setLayoutData(formData);

            outputDeviceComposite = new Composite(newDesignUI, SWT.BORDER);
            outputDeviceComposite.setLayout(new GridLayout());

            formData = new FormData();
            formData.bottom = new FormAttachment(100, -10);
            formData.right = new FormAttachment(100, -20);
            formData.top = new FormAttachment(devicesLabel, 10, SWT.DEFAULT);
            formData.left = new FormAttachment(0, 20);
            outputDeviceComposite.setLayoutData(formData);

            GridData g = new GridData();
            g.horizontalSpan = 4;
            userInterface.setLayoutData(g);

            // Set default design name
            if (existingDesign != null) {
                designNameInput.setText(existingDesign.getName());
            }
            else if(importDialog) {
                designNameInput.setText(requestData.designName);
            }
            else {
                int designCount = userProject.getDesigns().size() + 1;
                designNameInput.setText("Design " + designCount);

                // Select the text of the design name so that it is easy to change
                designNameInput.selectAll();
            }

            return userInterface;
        } // createUI

        /**
         * Add a device checkbox to the table
         * @param table the table to add the device to
         * @param dt the device
         * @param isSelected if the device is selected
         * @param optionListener the action listener to associate with the device
         */
        protected void addDevice(DeviceType dt,
                                 boolean isSelected,
                                 SelectionListener action)
        {
            Composite parent = (dt.getNature() == DeviceType.OUTPUT_ONLY)
                                    ? outputDeviceComposite
                                    : inputDeviceComposite;
            Button button = new Button(parent, SWT.CHECK);

            button.setSelection((dt == DeviceType.Display) || isSelected);
            button.setData(dt);
            button.setText(dt.toString());
            button.addSelectionListener(action);
            if(importDialog && isSelected)
            {
                button.setEnabled(false);
            }

            if ((dt == DeviceType.Display) ||
                (isSelected && (existingDesign != null)))
            {
                button.setEnabled(false);
            }
        }

        /**
         * Maintain the request's set of selected device types;
         * toggle whether the set includes the given type.
         *
         * @param devType the device type to toggle set presence of
         */
        protected void toggleDeviceType(DeviceType devType)
        {
            if (requestData.deviceTypes.contains(devType)) {
                requestData.deviceTypes.remove(devType);
            }
            else {
                requestData.deviceTypes.add(devType);
            }

            enableOKButtonCheck();
        }

        /**
         * Populate the dialog box.
         */
        @Override
        protected void addMoreFields()
        {
            super.addMoreFields();

            createUI();

            addDevice(DeviceType.Keyboard,
                      requestData.deviceTypes.contains(DeviceType.Keyboard),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent arg0)
                          {
                              toggleDeviceType(DeviceType.Keyboard);
                          }
                      });
            addDevice(DeviceType.Mouse,
                      requestData.deviceTypes.contains(DeviceType.Mouse),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent evt)
                          {
                              toggleDeviceType(DeviceType.Mouse);
                          }
                      });
            addDevice(DeviceType.Touchscreen,
                      requestData.deviceTypes.contains(DeviceType.Touchscreen),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent evt)
                          {
                              toggleDeviceType(DeviceType.Touchscreen);
                          }
                      });
            addDevice(DeviceType.Voice,
                      requestData.deviceTypes.contains(DeviceType.Voice),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent evt)
                          {
                              toggleDeviceType(DeviceType.Voice);
                          }
                      });
//            addDevice(DeviceType.Physical,
//                      this.requestData.deviceTypes.contains(DeviceType.Physical),
//                      new SelectionAdapter() {
//                          public void widgetSelected(SelectionEvent evt)
//                          {
//                              toggleDeviceType(DeviceType.Physical);
//                          }
//                      });
            addDevice(DeviceType.Display,
                      requestData.deviceTypes.contains(DeviceType.Display),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent evt)
                          {
                              toggleDeviceType(DeviceType.Display);
                          }
                      });
            addDevice(DeviceType.Speaker,
                      requestData.deviceTypes.contains(DeviceType.Speaker),
                      new SelectionAdapter() {
                          @Override
                        public void widgetSelected(SelectionEvent evt)
                          {
                              toggleDeviceType(DeviceType.Speaker);
                          }
                      });
        }

        @Override
        protected void addMoreButtons()
        {
            super.addMoreButtons();
            enableOKButtonCheck();
        }

        /**
         * Determine if the OK button should be set enabled or disabled.
         */
        protected void enableOKButtonCheck()
        {
            if (okButton != null) {
                boolean enableOk = false;

                if (existingDesign != null) {
                    enableOk =
                        ! requestData.deviceTypes.equals(existingDesign.getDeviceTypes());
                }

                // Check that the new design name is not ""
                else if (designNameInput.getText().length() > 0) {
                    Iterator<DeviceType> requestedDevices =
                        requestData.deviceTypes.iterator();

                    // Check that at least one input device is selected
                    while (requestedDevices.hasNext()) {
                        DeviceType device = requestedDevices.next();

                        if (device.getNature() != DeviceType.OUTPUT_ONLY) {
                            enableOk = true;
                        }
                    }
                }

                okButton.setEnabled(enableOk);
            }
        }
    } // NewDesignDialog

    /**
     * Request new information (name, device type set) about a new or
     * existing design.  The given data initializes the values for
     * the interaction.
     *
     * @param requestData the initial values for the dialog box
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean requestNewDesignName(DesignRequestData requestData,
                                        boolean newProjectMode,
                                        Project project)
    {
        NewDesignDialog askName = new NewDesignDialog(window,
                                                      requestData,
                                                      newProjectMode,
                                                      project);

        Object response = askName.open();

        if ((response != null) && response.equals(WindowUtil.PromptDialog.OK))
        {
            requestData.designName = askName.getPromptResponse();

            return true;
        }

        return false;
    }

    public boolean requestNewDesignName(DesignRequestData requestData,
                                        boolean newProjectMode,
                                        Project project, boolean changeDesignName)
    {
        NewDesignDialog askName = new NewDesignDialog(window,
                                                      requestData,
                                                      newProjectMode,
                                                      project, changeDesignName);

        Object response = askName.open();

        if ((response != null) && response.equals(WindowUtil.PromptDialog.OK))
        {
            requestData.designName = askName.getPromptResponse();

            return true;
        }

        return false;
    }

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
                                           Design design)
    {
        NewDesignDialog askName = new NewDesignDialog(window,
                                                      requestData,
                                                      project,
                                                      design);

        Object response = askName.open();

        if ((response != null) && response.equals(WindowUtil.PromptDialog.OK))
        {
            requestData.designName = askName.getPromptResponse();

            return true;
        }

        return false;
    }

    /**
     * A message explaining that the new widget name collides with
     * an existing one.
     */
    public String protestNameCollision(String type)
    {
        WindowUtil.PromptDialog dialog =
            new WindowUtil.PromptDialog(window,
                                        errorTitle,
                                        SWT.PRIMARY_MODAL,
                                        type + " " + L10N.get("CT.NameLabel",
                                                              "name:"),
                                        nameCollisionMsg);

        Object response = dialog.open();

        return
            (response != null) && response.equals(WindowUtil.PromptDialog.OK)
                    ? dialog.getPromptResponse()
                    : null;
    }

    /**
     * A message explaining that the new widget name cannot be empty.
     */
    public String protestNameCannotBeEmpty(String type)
    {
        WindowUtil.PromptDialog dialog =
            new WindowUtil.PromptDialog(window,
                                        errorTitle,
                                        SWT.PRIMARY_MODAL,
                                        type + " " + L10N.get("CT.NameLabel",
                                                              "name:"),
                                        nameEmptyMsg);

        Object response = dialog.open();

        return
            (response != null) && response.equals(WindowUtil.PromptDialog.OK)
                    ? dialog.getPromptResponse()
                    : null;
    }

    /**
     * Complain that one or more frames contains a hidden button. (Hidden hot-spot)
     *
     * @author khaledziyaeen@gmail.com
     */
    public boolean protestHiddenButtons(String message)
    {
        return SWT.OK == WindowUtil.presentConfirmDialog(window,
                                                         L10N.get("CT.HiddenButtons", 
                                                                  "Hidden Buttons"),
                                                         message);
    }
    
    public Boolean confirmNoTracing() {
        switch (WindowUtil.presentYesNoCancelDialog(window,
           L10N.get("PM.confirmNoTracingTitle", "Turn Tracing On?"),
           L10N.get("PM.confirmNoTracingMsg",
                    "ACT-R tracing is currently suppressed, which means " +
                    "visualization and the generation of script steps in " +
                    "novice exploration will not work. Would you like to " +
                    "turn ACT-R tracing back on again?"))) {
            case SWT.YES:
                return Boolean.TRUE;
            case SWT.NO:
                return Boolean.FALSE;
            default:
                return null;
        }
    }
}
