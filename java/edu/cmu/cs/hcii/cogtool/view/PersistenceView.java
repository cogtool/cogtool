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

package edu.cmu.cs.hcii.cogtool.view;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolFileTypes;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;

/**
 * Provides abstract persistence functionality for CogTool.  Error checking
 * that is general across all storage formats should be provided here.
 * @author centgraf
 */
public class PersistenceView
{
    /**
     * Filter parts for CogTool project files (*.cgt)
     */
    protected static final String[] COGTOOL_FILTER_EXTS =
        new String[] { "*.cgt", "*.*" };
    protected static final String[] COGTOOL_FILTER_NAMES =
        new String[] { "CogTool Projects (*.cgt)", "All Files (*.*)" };

    /**
     * Filter parts for text files (*.txt)
     */
    protected static final String[] TEXT_FILTER_EXTS =
        new String[] { "*.txt", "*.*" };
    protected static final String[] TEXT_FILTER_NAMES =
        new String[] { "Text Files (*.txt)", "All Files (*.*)" };

    /**
     * Filter parts for Lisp files (*.lisp)
     */
    protected static final String[] LISP_FILTER_EXTS =
        new String[] { "*.lisp", "*.*" };
    protected static final String[] LISP_FILTER_NAMES =
        new String[] { "Lisp Files (*.lisp)", "All Files (*.*)" };

    /**
     * Filter parts for SQL files (*.sql)
     */
    protected static final String[] SQL_FILTER_EXTS =
        new String[] { "*.sql", "*.*" };
    protected static final String[] SQL_FILTER_NAMES =
        new String[] { "SQL Files (*.sql)", "All Files (*.*)" };

    /**
     * Filter parts for PHP files (*.php)
     */
    protected static final String[] PHP_FILTER_EXTS =
        new String[] { "*.php", "*.*" };
    protected static final String[] PHP_FILTER_NAMES =
        new String[] { "PHP Files (*.php)", "All Files (*.*)" };

    /*
     * TODO: set lastXXPath when open happens through other means (DnD or double-click)?
     */

    /**
     * "Memory" of file dialog locations -- reused across all Views.
     */
    protected static String lastOpenPath;
    protected static String lastSavePath;

    protected FileDialog openDialog;
    protected FileDialog saveDialog;
    protected MessageBox mBox;
    protected MessageBox replaceBox;

    public PersistenceView(Shell shell)
    {
        // Init dialogs
        openDialog =
            new FileDialog(shell,
                           SWT.MULTI | SWT.OPEN  | SWT.APPLICATION_MODAL);
        saveDialog = new FileDialog(shell, SWT.SAVE | SWT.PRIMARY_MODAL);

        openDialog.setFilterExtensions(COGTOOL_FILTER_EXTS);
        openDialog.setFilterNames(COGTOOL_FILTER_NAMES);

        mBox =
            new MessageBox(shell, SWT.OK | SWT.ICON_ERROR | SWT.PRIMARY_MODAL);
        replaceBox =
            new MessageBox(shell,
                           SWT.YES | SWT.NO | SWT.CANCEL
                                   | SWT.ICON_QUESTION | SWT.PRIMARY_MODAL);

        mBox.setText(L10N.get("PV.FileErrorTitle", "File Error"));
        replaceBox.setText(L10N.get("PV.ReplaceFile?", "Replace File?"));
    }

    /**
     * Prompt the user for a set of files (presumably containing Project data)
     * to open.
     * @return null if selection was cancelled or caused an error
     */
    public File[] selectFileSources()
    {
        // Ensure consistency across Views
        if (lastOpenPath != null) {
            openDialog.setFilterPath(lastOpenPath);
        }

        String result = openDialog.open();
        File[] retval;

        if (result == null) {
            retval = null;
        }

        String pathname = openDialog.getFilterPath();
        File path = new File(pathname);
        String[] sel = openDialog.getFileNames();
        List<File> files = new ArrayList<File>();

        lastOpenPath = pathname;
        String msg = L10N.get("PV.CannotReadFiles", "Cannot read file(s):");

        for (String element : sel) {
            File file = new File(path, element);

            // Make sure the file is readable, or it is useless
            if (file.canRead()) {
                files.add(file);
            }
            else {
                // XXX: more detail re. cause? permissions, doesn't exist, etc.
                msg += '\n' + file.getAbsolutePath();
            }
        }

        if (msg.length() > 22) {
            warning(msg);
        }

        retval = files.toArray(new File[files.size()]);

        return retval;
    }

    protected void setSaveDialogFilters(String fileExtension)
    {
        if (CogToolFileTypes.COGTOOL_FILE_EXT.equals(fileExtension)) {
            saveDialog.setFilterExtensions(COGTOOL_FILTER_EXTS);
            saveDialog.setFilterNames(COGTOOL_FILTER_NAMES);
        }
        else if (CogToolFileTypes.TEXT_FILE_EXT.equals(fileExtension)) {
            saveDialog.setFilterExtensions(TEXT_FILTER_EXTS);
            saveDialog.setFilterNames(TEXT_FILTER_NAMES);
        }
        else if (CogToolFileTypes.LISP_FILE_EXT.equals(fileExtension)) {
            saveDialog.setFilterExtensions(LISP_FILTER_EXTS);
            saveDialog.setFilterNames(LISP_FILTER_NAMES);
        }
        else if (CogToolFileTypes.SQL_FILE_EXT.equals(fileExtension)) {
            saveDialog.setFilterExtensions(SQL_FILTER_EXTS);
            saveDialog.setFilterNames(SQL_FILTER_NAMES);
        }
        else if (CogToolFileTypes.PHP_FILE_EXT.equals(fileExtension)) {
            saveDialog.setFilterExtensions(PHP_FILTER_EXTS);
            saveDialog.setFilterNames(PHP_FILTER_NAMES);
        }
    }

    /**
     * Prompt the user for a location to which to save a file.
     *
      * GENERALLY this is used to save an Project file.
      * However, it is also used to save any other exported file.
      *
      * @param fileName name of the file to be saved at the location being picked
      * @param fileExtension the ".xxx" extension, Use COGTOOL_FILE_EXT for Project
      * @return null if selection was canceled or caused an error
      */
    public File selectFileDest(String fileName, String fileExtension)
    {
        // Ensure consistency across ProjectViews
        if (lastSavePath != null) {
            saveDialog.setFilterPath(lastSavePath);
        }
        else if (lastOpenPath != null) {
            saveDialog.setFilterPath(lastOpenPath);
        }

        saveDialog.setFileName(fileName + fileExtension);

        setSaveDialogFilters(fileExtension);

        String result = saveDialog.open();

        if (result == null) {
            return null;
        }

        String pathname = saveDialog.getFilterPath();
        File path = new File(pathname);
        String name = saveDialog.getFileName();
        File file = new File(path, name);

        lastSavePath = pathname;

        // Check if the file exists (and would be overwritten)
        // Do this last, so that the question is never futile
        if (file.exists() && OSUtils.WINDOWS) {
            int choice = shouldReplaceFile(file);
            if (choice == SWT.NO) {
                return selectFileDest(fileName, fileExtension);
            }

            if (choice == SWT.CANCEL) {
                return null;
            }
        }
        else {
            try {
                file.createNewFile();
            }
            catch (IOException e) {
                warning(L10N.get("PV.CannotCreateFile", "Cannot create file")
                            + ":\n" + file.getAbsolutePath());
                return selectFileDest(fileName, fileExtension);
            }
        }

        // Check if the file is writable
        if (! file.canWrite()) {
            warning(L10N.get("PV.CannotWriteFile", "Cannot write to file")
                        + "\n" + file.getAbsolutePath());
            return selectFileDest(fileName, fileExtension);
        }

        return file;
    }

    /**
     * Display a warning or error message to user, blocking until the message
     * is dismissed.
      * @param msg the message to display
      */
     protected void warning(String msg)
     {
         mBox.setMessage(msg);
         mBox.open();
     }

     /**
      * Prompt user whether a file should be replaced with the Project's data.
      * @param file the file that may be replaced
      * @return true if the file should be replaced, false if it should not be
      */
     protected int shouldReplaceFile(File file)
     {
         replaceBox.setMessage(L10N.get("PV.FileExistsReplace?",
                                             "File exists.  Replace file?")
                                        + "\n" + file.getAbsolutePath());
         return replaceBox.open();
     }
}
