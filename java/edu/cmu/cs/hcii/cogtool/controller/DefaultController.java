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

package edu.cmu.cs.hcii.cogtool.controller;

import java.io.File;
import java.io.IOException;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.RcvrCannotUndoRedoException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOSaveException;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * For CogTool, this class provides default implementations for standard
 * Controller semantic actions that all windows editing CogTool model
 * objects should support (specifically excluding, for example, the
 * "invisible" root window when executing on a Mac).
 * <p>
 * Semantic actions and parameters (in addition to <code>AController</code>):
 *      SaveProject     <no parameters>
 *      SaveProjectAs   <no parameters>
 *      CloseWindow     <no parameters>
 * <p>
 * Note that CloseWindow is not in AController because one should not
 * close the "invisible" root window without explicitly exiting CogTool.
 *
 * @author mlh
 */
public abstract class DefaultController extends Controller
{
    protected Project project;

    // Note that all concrete subclasses are expected
    // to initialize undoMgr.
    protected UndoManager undoMgr = null;

    /**
     * Constructor; all CogTool model editing controllers keep track
     * of the <code>Project</code> instance that contains (or is) the
     * model object being edited.
     *
     * @param proj the Project instance that is or contains the model
     *             object being edited
     * @author mlh
     */
    public DefaultController(Project proj)
    {
        super();

        project = proj;
    }

    /**
     * Returns the <code>Project</code> instance that contains (or is) the
     * model object being edited.
     *
     * @return the Project instance that contains (or is) the
     *         model object being edited
     * @author mlh
     */
    protected Project getProject()
    {
        return project;
    }

    /**
     * Registers the set of <code>IListenerAction</code> instances
     * that implement the semantic actions that are possible.
     * <p>
     * For this class, this consists of the actions that all CogTool
     * model editing windows support.
     *
     * @author mlh
     */
    @Override
    protected void assignActions()
    {
        super.assignActions();

        UI ui = getUI();
        final Interaction interaction = getUI().getStandardInteraction();

        if (ui != null) {
            // Set "save as" action
            ui.setAction(CogToolLID.SaveProjectAs,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 return saveAs();
                             }
                         });

            // Set "save" action
            ui.setAction(CogToolLID.SaveProject,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 return forceSave();
                             }
                         });

            ui.setAction(CogToolLID.CloseWindow, createCloseWindowAction());

            ui.setAction(CogToolLID.CloseProject,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 return closeProject(project, true);
                             }
                         });

            ui.setAction(CogToolLID.Properties,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 return showProperties();
                             }
                         });

            ui.setAction(CogToolLID.SetAttribute,
                         new IListenerAction() {

                             public Class<?> getParameterClass()
                             {
                                 return UI.SetAttributeParameters.class;
                             }


                             public boolean performAction(Object prms)
                             {
                                 UI.SetAttributeParameters p =
                                     (UI.SetAttributeParameters) prms;

                                 return DefaultCmd.setAttribute(p.target,
                                                                null,
                                                                p.attrName,
                                                                p.value,
                                                                interaction,
                                                                undoMgr);
                             }
                         });
        }
    }

    /**
     * If the model has a stored save location, "Save", else "Save As...".
     *
     * @return true if file was saved, false otherwise
     */
    protected boolean forceSave()
    {
        // If the project has been saved in the past, no need to ask
        // for a location.
        if (persist.isPermanent(project)) {
            return save();
        }

        return saveAs();
    }

    /**
     * Perform a Save operation, using a stored save location.
     *
     * @return true if file was saved, false otherwise
     * @throws RcvrIOException if the save operation fails
     */
    protected boolean save()
    {
        try {
            // Save to this file's original location.
            project.setBuildVersion(CogTool.getVersion());
            persist.save(project, null);

            // Tell undo manager(s) that a save has just occurred
            try {
                UndoManager.markSavePoint(project);
            }
            catch (IllegalStateException ex) {
                throw new RcvrCannotUndoRedoException("Marking save point", ex);
            }

            return true;
        }
        catch (IOException e) {
            throw new RcvrIOSaveException("Error persisting project: "
                                                + e.getMessage(),
                                          e);
        }
    }

    /**
     * Perform a Save As... operation, prompting for a save location.
     *
     * @return true if file was saved, false otherwise
     * @throws RcvrIOException if the save operation fails
     */
    protected boolean saveAs()
    {
        Interaction stdInteraction = getUI().getStandardInteraction();

        try {
            boolean nameIsInUse;
            File saveLoc;

            do {
                // Request a new file name, using the project's current name
                // as the default.
                // TODO: It is "evil" that the test for existing files is
                //       hidden within this call; separate at some point (mlh)
                saveLoc =
                    stdInteraction.selectFileDest(project.getName());

                // If the save dialog was canceled, do nothing.
                if (saveLoc == null) {
                    return false;
                }

                // If saveLoc is already open, refuse to save there.
                nameIsInUse = persist.isRegistered(saveLoc, project);

                if (nameIsInUse) {
                    switch (stdInteraction.protestBeingEdited(saveLoc)) {
                        case Interaction.SAVE: {
                            nameIsInUse = false;
                            break;
                        }
                        case Interaction.NO_SAVE: {
                            // Simply loop to ask for a new name
                            break;
                        }
                        case Interaction.CANCEL: {
                            return false;
                        }
                    }
                }
            } while (nameIsInUse);

            CogToolPref.setRecent(saveLoc.getCanonicalPath());

            // Set the title BEFORE saving the project.
            String name = saveLoc.getName();

            // Remove the .cgt, if it is there.
            int periodIndex = name.lastIndexOf('.');
            if (periodIndex > 0) {
                name = name.substring(0, periodIndex);
            }

            project.setName(name);

            // Save to the selected location.
            project.setBuildVersion(CogTool.getVersion());
            persist.save(project, saveLoc);

            // Tell undo manager(s) that a save has just occurred
            try {
                UndoManager.markSavePoint(project);
            }
            catch (IllegalStateException ex) {
                throw new RcvrCannotUndoRedoException("Marking save point", ex);
            }

            return true;
        }
        catch (IOException e) {
            throw new RcvrIOSaveException("Error persisting project: "
                                                + e.getMessage(),
                                          e);
        }
    }
    
    public void saveAsFilename(String pathname) {
        File f = new File(pathname);
        project.setName(f.getName());
        project.setBuildVersion(CogTool.getVersion());
        try {
            persist.save(project, f);
        } catch (IOException e) {
            throw new RcvrIOSaveException("Error persisting project: "
                                                + e.getMessage(),
                                          e);
        }
        save();
    }

    /**
     * Return the primary model object this controller is responsible for
     * editing.
     *
     * @return the primary model object this controller is responsible for
     *         editing
     * @author mlh
     */
    protected abstract Object getModelObject();

    /**
     * Close the window associated with this controller.
     * <p>
     * If the associated <code>Project</code> instance or any of its
     * "children" have been modified, we may want to try to save the changes.
     * <p>
     * Two policies have been implemented: (1) if this is the last window
     * open for the associated project, then check to save; OR (2) if this
     * is the project's window (that is, a <code>ProjectController</code>),
     * then check to save.  This policy is controlled by the
     * <code>PROJECT_MANAGES_OTHERS</code> flag in <code>CogTool</code>;
     * if true, then policy (2) is used, otherwise (1) is used.
     * <p>
     * Note: Policy (2) assumes that all other open editor windows managing
     * objects belonging to the project are closed when its window is closed
     * (see <code>ProjectController.dispose()</code>).
     * <p>
     * If saving, the user is asked whether to save the associated project.
     * One response is to abort/cancel the window closing.
     * <p>
     * Sometimes, a window may be closed in the process of opening another
     * window (such as when the Frame Chooser window is closed to open the
     * corresponding Script Demonstration window).  In this case, we don't
     * want to check to save -- thus, the parameter.
     *
     * @param checkToSave whether to check to save the associated project
     * @return true if and only if the close was approved and successful
     * @throws RcvrIOSaveException if the save operation fails
     * TODO: Change the exception to one that should be caught and managed!
     * @author mlh
     */
    protected boolean closeWindow(boolean checkToSave)
    {
        // If checking to save, determine policy.  If the project manages
        // the other windows, then check if this is the project's editor
        // window.  If the policy is otherwise, then simply check whether
        // this is the last open window for objects associated with this
        // project (regardless of whether it is the ProjectController).
        if (checkToSave &&
            ((CogTool.projectManagesOthers &&
              (project == getModelObject())) || // this: ProjectController
             (CogTool.controllerNexus.getControllerCount(project) == 1)))
        {
            boolean needToAsk;

            try {
                needToAsk =
                    ! UndoManager.isAtSavePoint(project);
            }
            catch (IllegalStateException ex) {
                System.err.println("Ignoring that isAtSavePoint failed.");
                needToAsk = true;
            }

            if (needToAsk) {
                Interaction interaction = getUI().getStandardInteraction();

                // The related project and/or "children" are modified;
                // ask to save.
                switch (interaction.askSaveBeforeClose(project.getName()))
                {
                    case Interaction.SAVE: {
                        // Try to save; user may get the chance to abort/cancel
                        if (! forceSave()) {
                            return false;       // close aborted/canceled
                        }

                        // ok to close
                        break;
                    }
                    case Interaction.NO_SAVE: {
                        break;                  // ok to close
                    }
                    case Interaction.CANCEL: {
                        return false;           // aborted/canceled
                    }
                }
            }

            // Close window and recover resources.
            dispose();

            recoverManagers(project);
        }
        else {
            // No need to attempt to save (perhaps not modified).

            // Close window and recover resources.
            dispose();
        }

        return true;
    } // closeWindow

    protected IListenerAction createCloseWindowAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                return closeWindow(true);
            }
        };
    }

    protected boolean showProperties()
    {
        Interaction interaction = getUI().getStandardInteraction();

        interaction.showProjectProperties(project);

        return true;
    }

    protected void recoverManagers(Object editObject)
    {
        UndoManagerRecovery.recoverManagers(project, editObject);
    }

    /**
     * Close the associated window, recovering all associated resources.
     */
    @Override
    public void dispose()
    {
        ControllerRegistry.ONLY.removeOpenController(this);
        super.dispose();
    }
}
