/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2013 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * jopt-simpler
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.controller;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectPersister;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOLoadException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOTempException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * For CogTool, this Controller class provides default implementations for
 * standard Controller methods and the set of semantic actions that all
 * windows should support (including the "invisible" root window when
 * executing on a Mac).
 * <p>
 * This implementation assumes there is an associated view object for
 * the window (<code>AUI</code>).
 * <p>
 * Semantic actions and parameters:
 *      About           <no parameters>
 *      OpenProject     <no parameters>
 *      NewProject      <no parameters>
 *      ExitApplication <no parameters>
 *
 * @author mlh
 */
public abstract class Controller
{
    protected ObjectPersister persist;

    /**
     * Constructor for the default implementation.  For opening and creating
     * new projects, a persistence manager is required.
     * <p>
     * Generally, subclasses should invoke assignActions when they are
     * done initializing themselves.
     *
     * @author mlh
     */
    public Controller()
    {
        // At the moment, this always returns a singleton instance.
        persist = ObjectPersister.ONLY;
    }

    /**
     * Fetch the implementation of the view for this window.
     *
     * Only designated as public for testing purposes, should not be used as
     * a public method otherwise
     *
     * @return the implementation of the view for this window
     */
    public abstract UI getUI();

    /**
     * Perform the requested semantic action.
     * <p>
     * Finds the code snippet object associated with the given semantic
     * identifier and, if found, executes the application's semantic action.
     * <p>
     * If not found, this simply does nothing.  By not throwing an exception,
     * we allow for flexibility as to how to enable/disable certain
     * functionality.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @param actionParms data needed by this operation to perform the action
     * @param doCleanup whether or not to perform the cleanup operation
     *                  (non-menuHidden!)
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     * @author mlh
     */

    public boolean performAction(CogToolLID id,
                                 Object actionParms,
                                 boolean doCleanup)
    {
        UI ui = getUI();

        if (ui != null) {
            return ui.performAction(id, actionParms, doCleanup);
        }

        return false;
    }

    /**
     * Same as three-parameter performAction that requests cleanup.
     */

    public boolean performAction(CogToolLID id, Object actionParms)
    {
        return performAction(id, actionParms, true);
    }

    /**
     * Registers the set of <code>IListenerAction</code> instances
     * that implement the semantic actions that are possible.
     * <p>
     * For this class, this consists of the actions that all windows support.
     *
     * @author mlh
     */
    protected void assignActions()
    {
        UI ui = getUI();

        if (ui != null) {
            ui.setAction(CogToolLID.About, ui.popAboutBox());

            // Set "open" action
            ui.setAction(CogToolLID.OpenProject,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 // If from a menu item, prms will be
                                 // a selection state, which is useless.
                                 // open expects null in that case.
                                 if (! (prms instanceof DoublePoint)) {
                                     prms = null;
                                 }

                                 // Ask the user to select a CogTool project to open
                                 File[] openLocs =
                                     getUI().getStandardInteraction().selectFileSources();

                                 return open((DoublePoint) prms, openLocs);
                             }
                         });

            ui.setAction(CogToolLID.OpenProjectFile,
                         new IListenerAction() {

                             public Class<?> getParameterClass()
                             {
                                 return String.class;
                             }


                             public boolean performAction(Object actionParms)
                             {
                                 File[] openLoc =
                                     { new File((String) actionParms) };

                                 return open(null, openLoc);
                             }
                         });

            // Set "new project" action
            ui.setAction(CogToolLID.NewProject, createNewProjectAction());

            // Set "quit/exit" action
            ui.setAction(CogToolLID.ExitApplication, createExitAction());

            ui.setAction(CogToolLID.ClearRecent,
                         new AListenerAction() {

                             public boolean performAction(Object actionParms)
                             {
                                 CogToolPref.clearRecent();

                                 Interaction interaction =
                                     getUI().getStandardInteraction();

                                 interaction.setStatusMessage(L10N.get("AC.RecentClear",
                                                                       "Recent file data cleared."));

                                 return true;
                             }
                         });
        }
    }

    // Creating a new project
    protected IListenerAction createNewProjectAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                ProjectController c =
                    ProjectController.newProjectController();

                // Populate the empty project to avoid a "blank screen".
                c.populateProject();

                return true;
            }
        };
    }

    /**
     * Prompts for and opens a file.
     *
     * @throws RcvrIOLoadException if the open fails
     */
    protected boolean open(DoublePoint loc, File[] openLocs)
    {
        try {
            // If the open dialog was canceled, do nothing.
            if (openLocs != null) {
                Interaction interaction = getUI().getStandardInteraction();

                // Otherwise, open all of the selected projects.
                for (int i = 0; i < openLocs.length; i++) {
                    String statusMsg = null;

                    if (! openLocs[i].exists()) {
                        interaction.protestFileNotFound(openLocs[i].getName());

                        continue;
                    }

                    Project proj = null;
                    try {
                        proj = (Project) persist.isLoaded(openLocs[i]);

                        if (proj != null) {
                            if (UndoManager.isAtSavePoint(proj))
                            {
                                statusMsg = L10N.get("AC.OpenNotModified",
                                                     "Project already open and not modified");
                            }
                            else {
                                boolean revert =
                                    interaction.askRevertBeforeOpen(proj.getName());

                                if (revert) {
                                    closeProject(proj, false);
                                }
                                else {
                                    continue;
                                }
                            }
                        }

                        proj = (Project) persist.load(openLocs[i]);
                    }
                    catch (RuntimeException e) {
                        // the persistence loader signals most of its
                        // exceptions not by throwing IOExceptions but by
                        // throwing RuntimeExceptions of various sorts; note
                        // that the most common root cause of such exceptions
                        // is trying to load a file written by a newer version
                        // of CogTool, that has made a change to the persistence
                        // format
                        throw new RcvrIOLoadException(("Error loading project: "
                                                       + e.getMessage()),
                                                      e);
                    }

                    CogToolPref.setRecent(openLocs[i].getCanonicalPath());

                    // Reset the project name in case the file was renamed
                    String name = openLocs[i].getName();

                    // Remove the .cgt, if it is there.
                    int periodIndex = name.lastIndexOf('.');
                    if (periodIndex > 0) {
                        name = name.substring(0, periodIndex);
                    }

                    proj.setName(name);

                    // Create a window; the project is not yet registered
                    // and is unmodified.
                    ProjectController c =
                        ProjectController.openController(proj,
                                                                        false,
                                                                        true);
                    UI ui = getUI();

                    if ((loc != null) && (ui != null)) {
                        c.getUI().setLocation(loc);
                    }

                    if (statusMsg != null) {
                        c.interaction.setStatusMessage(statusMsg);
                    }
                }

                return true;
            }

            return false;
        }
        catch (IOException e) {
            throw new RcvrIOLoadException("Error loading project: "
                                                  + e.getMessage(),
                                          e);
        }
        catch (GraphicsUtil.ImageException e) {
            throw new RcvrImageException("Error loading image from project: "
                                                  + e.getMessage(),
                                          e);
        }
    }

    /**
     * Bring the associated window to the front and make active by
     * making the window have the focus.
     *
     * @throws RcvrUIException if there is no associated view support
     * @author mlh
     */

    public void takeFocus()
    {
        UI ui = getUI();

        if (ui != null) {
            ui.takeFocus();
        }
        else {
            throw new RcvrUIException("No ui available for taking focus.");
        }
    }

    /**
     * Request that the associated window be closed; it is possible for some
     * interaction with the user to occur which might "cancel" the operation.
     *
     * @return true if and only if the close was successful.
     * @throws RcvrUIException if there is no associated view support
     */

    public boolean requestClose()
    {
        UI ui = getUI();

        if (ui != null) {
            return ui.requestClose();
        }

        throw new RcvrUIException("No ui available for close request.");
    }

    // The action for ExitApplication
    protected IListenerAction createExitAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                // Take advantage of dual policy implementation; by
                // temporarily ensuring that "project manages others", the user
                // is presented with a ProjectController when asked to save
                // each modified and unsaved project.
                boolean oldProjectManagesOthers =
                    CogTool.projectManagesOthers;

                // Eliminate all windows where the associated
                // ProjectController still exists.
                CogTool.projectManagesOthers = true;

                Set<Project> projectSet =
                    CogTool.controllerNexus.getControllerNexuses();

                Object[] projects = projectSet.toArray();

                for (Object project : projects) {
                    Controller projectController =
                        ControllerRegistry.ONLY.findOpenController(project);

                    // Ask the shell to close (this may prompt user for save).
                    // If the shell didn't close, the exit command
                    // was canceled.
                    if ((projectController != null) &&
                        ! projectController.requestClose())
                    {
                        // Restore the original policy
                        CogTool.projectManagesOthers =
                            oldProjectManagesOthers;

                        return false;
                    }
                }

                // Restore the original policy
                CogTool.projectManagesOthers = oldProjectManagesOthers;

                // Find remaining open windows that are not ProjectControllers;
                // Note we only get here if the current policy is that
                // "projects do NOT manage others"!
                projectSet = CogTool.controllerNexus.getControllerNexuses();

                projects = projectSet.toArray();

                // Attempt to close each window, asking the user to save
                // any modified but unsaved projects.
                for (Object project2 : projects) {
                    Project project = (Project) project2;

                    if (! CogTool.controllerNexus.closeControllers(project,
                                                                   true))
                    {
                        return false;
                    }
                }

                // All the windows are closed (except possibly the "invisible"
                // root window when executing on a Mac); time to exit!
                System.exit(0);

                // Kind of superfluous (Java!)
                return true;
            }
        };
    }

    /**
     * Close the associated window, recovering all associated resources.
     */

    public void dispose()
    {
        UI ui = getUI();

        if (ui != null) {
            ui.dispose();
        }
    }

    /**
     * Sets the window to be visible or invisible
     */

    public void setVisible(boolean visible)
    {
        UI ui = getUI();

        if (ui != null) {
            ui.setVisible(visible);
        }
    }

    /**
     * Fetches the window's location
     */

    public DoublePoint getLocation()
    {
        UI ui = getUI();

        if (ui != null) {
            return ui.getLocation();
        }

        return null;
    }

    /**
     * Get the extend of the UI's window
     */

    public DoubleRectangle getExtent()
    {
        UI ui = getUI();

        if (ui != null) {
            return ui.getExtent();
        }

        return null;
    }

    /**
     * Set the top-left location of the controller's window
     */

    public void setLocation(double x, double y)
    {
        UI ui = getUI();

        if (ui != null) {
            ui.setLocation(x, y);
        }
    }

    protected void recoverManagers(Project project)
    {
        UndoManager.recoverUndoManagers(project);

        Iterator<Design> designs = project.getDesigns().iterator();

        while (designs.hasNext()) {
            DemoStateManager.removeStateManager(project, designs.next());
        }

        try {
            // Clean up project temporary/checkpoint files.
            persist.close(project);
        }
        catch (IOException ex) {
            throw new RcvrIOTempException("Error cleaning up temp files: "
                                                  + ex.getMessage(),
                                          ex);
        }
        catch (IllegalArgumentException ex) {
            throw new RcvrUIException("Error closing project: "
                                              + ex.getMessage(),
                                      ex);
        }
    }

    /**
     * Close all the project associated with this window.
     *
     * @param checkToSave whether to check to save the project
     * @return true if and only the close(s) were successful and
     *         not aborted/canceled
     */
    public boolean closeProject(Project project, boolean checkToSave)
    {
        if (CogTool.controllerNexus.closeControllers(project, checkToSave)) {
            recoverManagers(project);

            return true;
        }

        return false;
    }
}
