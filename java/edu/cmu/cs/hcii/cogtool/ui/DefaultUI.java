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

import java.util.EventObject;

import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.controller.Controller;
import edu.cmu.cs.hcii.cogtool.controller.ControllerRegistry;
import edu.cmu.cs.hcii.cogtool.controller.DesignEditorController;
import edu.cmu.cs.hcii.cogtool.controller.ProjectController;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.UndoManagerView;

/**
 * Default implementation of UI functionality shared by the views
 * for all CogTool model objects.
 *
 * @author mlh
 */
public abstract class DefaultUI extends UI
                                implements MenuFactory.ILeadItemUpdater
{
    /**
     * Specialized listener ID indicating that the window for the
     * given project or design should be restored (that is, given the focus
     * if it still exists, or re-created otherwise).
     * <p>
     * Actual restoration occurs in <code>getParameters</code>. The restored
     * window  is then returned by <code>getParameters</code> in case
     * any further semantic action might wish to operate on the window.
     * Generally, restoration of the window is all that is expected,
     * so controllers do not assign any actions to instances of
     * this listener ID subclass.
     *
     * @author mlh
     */
    public static class RestoreParentControllerLID extends ListenerIdentifier
    {
        public final Project project;
        public final Design design;   // can be null

        /**
         * Initialize the ID; if the given design is not <code>null</code>,
         * then a <code>DesignEditorController</code> will be restored.
         * Otherwise, a <code>ProjectController</code> will be restored.
         *
         * @param p the project either to restore or as the parent of the
         *          given design
         * @param d the design either to restore or <code>null</code> if
         *          restoring the project's window
         * @author mlh
         */
        public RestoreParentControllerLID(Project p, Design d)
        {
            project = p;
            design = d;
        }

        /**
         * Used by <code>getParameters</code> to restore the desired
         * window.  Returned by <code>getParameters</code> in case
         * any further semantic action might wish to operate on the window.
         * <p>
         * Generally, restoration of the window is all that is expected,
         * so controllers do not assign any actions to instances of
         * this listener ID subclass.
         *
         * @return the controller restored
         * @author mlh
         */
        public Controller restoreWindow()
        {
            Controller window = null;

            // Project controller if design is null
            if (design == null) {
                boolean notModified;

                try {
                    notModified =
                        UndoManager.isAtSavePoint(project);
                }
                catch (IllegalStateException ex) {
                    System.err.println("Ignoring that isAtSavePoint failed.");
                    notModified = false;
                }

                // Presume registered -- registering twice is problematic!
                window =
                  ProjectController.openController(project,
                                                                  false,
                                                                  notModified);
            }
            else {
                // Design controller
                window =
                  DesignEditorController.openController(design,
                                                                       project);
            }

            return window;
        }
    }

    /**
     * All UI subclasses require access to the project that contains
     * the model object being edited.
     */
    protected Project project;

    /**
     * Marker string reflecting whether the associated project
     * is modified since its last save; suitable for use in constructing
     * window titles.
     */
    protected String modificationFlag;

    /**
     * The undo manager itself. Created in the controller, held onto here.
     * Mostly used for the connection to the undoManagerview.
     */
    protected UndoManager undoManager; // needed in dispose

    protected UndoManagerView.UndoAlertHandler undoMgrViewHandler;

    /**
     * Delegates closing a window to performAction in the corresponding
     * Controller object.
     */
    protected ShellAdapter closeListener = new ShellAdapter()
    {
        /**
         * Handler for shellClosed -- delegates to performAction
         */
        @Override
        public void shellClosed(ShellEvent e)
        {
            CogTool.controllerNexus.saveWindowLocation(project,
                                                       getModelObject(),
                                                       getShell().getBounds());

            /**
             * Don't let other listeners progress if save canceled.
             */
            e.doit = performAction(CogToolLID.CloseWindow, null, false);
        }
    };;

    /**
     * Definition of the "Window" menu for this window.
     */
    protected MenuFactory.IWindowMenuData<Project> menuData = null;

    /**
     * How to react to alerts raised when save points change in any undo
     * manager.  Updates the window titles, reflecting modification status.
     * TODO: Should we reflect modification status in "Window" menu entries?
     *       If so, do here!
     */
    protected AlertHandler undoMgrTitleHandler =
        new AlertHandler() {

            public void handleAlert(EventObject evt)
            {
                UndoManager.SavePointChange chg =
                    (UndoManager.SavePointChange) evt;

                updateModificationFlag(chg.nowAtSavePoint);
                updateTitle();
            }
        };

    /**
     * How to react to alerts that a model object has been renamed.
     * Updates window titles and "Window" menu entries.
     */
    protected AlertHandler renameHandler =
        new AlertHandler() {

            public void handleAlert(EventObject evt)
            {
                updateTitle();
                updateWindowMenus();
            }
        };

    protected static final String OPEN_PROJECT_LABEL =
        L10N.get("MI.OpenProject", "Display Project");

    protected static final String OPEN_DESIGN_LABEL =
        L10N.get("MI.OpenDesign", "Display Design");

    /**
     * Support for creating the lead items for the Window menu.
     *
     * @param p the project either to restore or as the parent of the
     *          given design
     * @param d the design either to restore or <code>null</code> if
     *          restoring the project's window
     * @author mlh
     */
    protected static MenuItemDefinition[] buildLeadItems(Project p, Design d)
    {
        ListenerIdentifier openProjectLID = null;
        ListenerIdentifier openDesignLID = null;

        boolean openProjectEnabled = MenuUtil.DISABLED;
        boolean openDesignEnabled = MenuUtil.DISABLED;

        String openProjectLabel = OPEN_PROJECT_LABEL;
        String openDesignLabel = OPEN_DESIGN_LABEL;

        // Check to create standard LID's for opening design & project
        if (p != null) {
            openProjectLID = new RestoreParentControllerLID(p, null);
            openProjectEnabled = MenuUtil.ENABLED;
            openProjectLabel = openProjectLabel + ": " + p.getName();

            if (d != null) {
                openDesignLID = new RestoreParentControllerLID(p, d);
                openDesignEnabled = MenuUtil.ENABLED;
                openDesignLabel = openDesignLabel + ": " + d.getName();
            }
        }

        return new MenuItemDefinition[]
            { new SimpleMenuItemDefinition(openProjectLabel,
                                           openProjectLID,
                                           openProjectEnabled),
              new SimpleMenuItemDefinition(openDesignLabel,
                                           openDesignLID,
                                           openDesignEnabled),
              MenuUtil.SEPARATOR };
    }

    // Suitable for a Design window
    protected static MenuItemDefinition[] buildLeadItems(Project p)
    {
        return buildLeadItems(p, null);
    }

    // Suitable for a Project window
    protected static MenuItemDefinition[] buildLeadItems()
    {
        return buildLeadItems(null, null);
    }

    /**
     * All CogTool model editing views keep track of the <code>Project</code>
     * instance that contains (or is) the model object being edited.
     *
     * @param proj the Project instance that is or contains the model
     *             object being edited
     * @param windowMenuLabel the label for this window's entry in the
     *                        window menu
     * @param leadItems the lead menu items for the window menu of this window
     * @author mlh
     */
    public DefaultUI(Project proj,
                     String windowMenuLabel,
                     MenuItemDefinition[] leadItems,
                     UndoManager undoMgr)
    {
        super();

        project = proj;

        // Manage current project modification state and future changes
        // to that state and model object names.
        if (project != null) {
            try {
                updateModificationFlag(UndoManager.isAtSavePoint(project));
            }
            catch (IllegalStateException ex) {
                System.err.println("Ignoring that isAtSavePoint failed.");
                updateModificationFlag(false);
            }

            project.addHandler(this,
                                    NameChangeAlert.class,
                                    renameHandler);

            try {
                UndoManager.addSavePointChangeHandler(project,
                                                                     undoMgrTitleHandler);
            }
            catch (IllegalStateException ex) {
                System.err.println("Ignoring fact that addSavePointChangeHandler failed.");
            }
        }

        undoManager = undoMgr;

        // Create the undo manager view handler,
        // resetting the undo view handler to represent any values currently in
        // the undo manager. IE: remember undo.
        if (undoManager != null) {
            undoMgrViewHandler =
                new UndoManagerView.UndoAlertHandler(undoManager,
                                                     lIDMap,
                                                     CogToolLID.Undo,
                                                     CogToolLID.Redo);

            undoManager.addHandler(this,
                                        UndoManager.UndoRedoEvent.class,
                                        undoMgrViewHandler);
        }

        menuData = new DefaultWindowMenuData(this,
                                                  windowMenuLabel,
                                                  leadItems);
    }

    /**
     * Return the UI's Project instance
     */
    public Project getProject()
    {
        return project;
    }

    @Override
    protected int canIDCauseSelection(ListenerIdentifier lid)
    {
        if (undoManager != null) {
            IUndoableEdit edit = null;

            if (lid == CogToolLID.Undo) {
                edit = undoManager.editToBeUndone();
            }
            else if (lid == CogToolLID.Redo) {
                edit = undoManager.editToBeRedone();
            }

            if (edit != null) {
                lid = edit.getLID();
            }
            // else not undo/redo; query lid directly
        }

        return super.canIDCauseSelection(lid);
    }

    @Override
    protected boolean doesIDCommitChanges(ListenerIdentifier lid)
    {
        if (undoManager != null) {
            if (lid == CogToolLID.Undo) {
                return idCommitsChanges(undoManager.editToBeUndone().getLID());
            }

            if (lid == CogToolLID.Redo) {
                return idCommitsChanges(undoManager.editToBeRedone().getLID());
            }
        }

        if (lid == CogToolLID.Paste) {
            return CogToolClipboard.hasNonTextPasteContent();
        }

        // Not undo/redo; query lid directly
        return super.doesIDCommitChanges(lid);
    }

    protected static final String operationCanceled =
        L10N.get("DEFINT.OperationCanceled",
                 "The requested operation was canceled due to the following error:");

    @Override
    public boolean performAction(ListenerIdentifier id)
    {
        if ((undoManager != null) && doesIDCommitChanges(id)) {
            Text textWithFocus = WindowUtil.getFocusedText();

            if (textWithFocus != null) {
                if (textWithFocus instanceof ManagedText) {
                    ManagedText performText = (ManagedText) textWithFocus;

                    if (! performText.confirm(ManagedText.LOSE_FOCUS)) {
                        getStandardInteraction().setStatusMessage(operationCanceled);
                        return false;
                    }
                }
            }
        }

        return super.performAction(id);
    }

    /**
     * Return the primary model object this view/ui is responsible for
     * editing.
     *
     * @return the primary model object this view/ui is responsible for
     *         editing
     * @author mlh
     */
    protected abstract Object getModelObject();

    /**
     * Based on the given modification state, recompute the modification
     * marker string used in window titles.
     *
     * @param atSavePoint true if modification state means "saved";
     *                    false if the associated project or any of its
     *                    components were modified since the last save
     * @author mlh
     */
    protected void updateModificationFlag(boolean atSavePoint)
    {
        modificationFlag = atSavePoint ? "" : "* ";
    }

    /**
     * Update the window title for this view/ui; we expect subclasses
     * to override.
     *
     * @author mlh
     */
    protected void updateTitle()
    {
        // subclasses may override
    }

    /**
     * Subclasses should override to construct the string for the window
     * menu's item label corresponding to this window.
     */
    protected abstract String buildWindowMenuLabel();

    /**
     * Update the given lead item if necessary.
     *
     * @param leadItem the lead menu item to update
     * @param position the 0-based index of the item in the Window menu
     */

    public void updateLeadItem(MenuItem leadItem, int position)
    {
        Object itemData = leadItem.getData();

        if ((itemData != null) &&
            (itemData instanceof RestoreParentControllerLID))
        {
            RestoreParentControllerLID lid =
                (RestoreParentControllerLID) itemData;

            if (lid.design != null) {
                leadItem.setText(OPEN_DESIGN_LABEL + ": "
                                                   + lid.design.getName());
            }
            else if (lid.project != null) {
                leadItem.setText(OPEN_PROJECT_LABEL + ": "
                                                    + lid.project.getName());
            }
        }
    }

    /**
     * Updates all "Window" menus based on the current information
     * for the associated model object.
     * <p>
     * Subclasses should override to fix their IWindowMenuData,
     * then they should invoke super.updateWindowMenus()
     *
     * @author mlh
     */
    protected void updateWindowMenus()
    {
        menuData.setEntryLabel(buildWindowMenuLabel());

        // The subclass has updated the associated "Window" definition;
        // reflect those changes in all "Window" menus.
        MenuFactory.updateMenuLabels(menuData, this);
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     * <p>
     * All model editing windows support:
     *   Paste, SaveProject, SaveProjectAs, and CloseWindow
     * <p>
     * Take this opportunity to interpose a listener when the
     * associated SWT window is closed by the user without using
     * a CogTool menu item.  This listener will also save the window's
     * location in case a new window is restored for the associated
     * model object.
     *
     * @author mlh
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        if (undoMgrViewHandler != null) {
            undoMgrViewHandler.resetView(undoManager);
        }

        setEnabled(CogToolLID.Paste,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.SaveProject,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SaveProjectAs,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.CloseWindow,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.CloseProject,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.Properties,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        Shell window = getShell();

        if (forConstruction && (window != null)) {
            window.removeShellListener(closeListener);
            window.addShellListener(closeListener);
        }
    }

    /**
     * Fetches the parameters needed by any <code>performAction</code>
     * invoked for the "specialized" <code>ListenerIdentifier</code>.
     * In some cases, the determination of the parameters requires
     * information from the original "general" LID subclass instance
     * (see, for example, SEDemoLID).
     * <p>
     * If the given "general" LID is actually an instance of
     * <code>RestoreParentControllerLID</code>, then the request is actually
     * to restore the window of either the containing project or design.
     *
     * May return UNSET if no value computed; it is then the responsibility
     * of the subclass to return something valid (i.e., not UNSET).
     *
     * @param originalLID the general LID value returned from a menu command
     * @param transmutedLID the specific value representing an actual,
     *                      concrete application function returned by
     *                      a call to <code>specialize()</code>
     * @param isContextSelection true if we should parameterize based on
     *                           the current contextual selection;
     *                           false to use the standard selection
     * @return the parameters the <code>IListenerAction</code> may require
     *         to perform the requested semantic action
     * @author mlh
     */
    @Override
    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        Object parameters = super.getParameters(originalLID,
                                                transmutedLID,
                                                isContextSelection);

        if (parameters != UNSET) {
            return parameters;
        }

        if (originalLID instanceof RestoreParentControllerLID) {
            return ((RestoreParentControllerLID) originalLID).restoreWindow();
        }

        return UNSET;
    }

    /**
     * Recover any system resources being used to support the view being
     * used by this UI instance.
     * <p>
     * At this point, we must remove alert handlers and the interposed
     * listener on the associated SWT Shell object.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        if (undoManager != null) {
            undoManager.removeAllHandlers(this);
        }

        project.removeAllHandlers(this);

        try {
            UndoManager.removeSavePointChangeHandler(project,
                                                                    undoMgrTitleHandler);
        }
        catch (IllegalStateException ex) {
            System.err.println("Ignoring fact that removeSavePointChangeHandler failed.");
        }

        Shell window = getShell();

        CogTool.controllerNexus.saveWindowLocation(project,
                                                   getModelObject(),
                                                   window.getBounds());

        if (window != null) {
            if (closeListener != null) {
                window.removeShellListener(closeListener);
            }
        }

        super.dispose();
    }

    /**
     * Retrieves a saved window location for the associated model object.
     * <p>
     * Convenience function for subclasses that passes request through to
     * the ControllerNexus.
     *
     * @param model the model object being edited in the window
     * @return the location Rectangle previously saved for this model
     */
    public Rectangle getWindowLocation()
    {
        return CogTool.controllerNexus.getWindowLocation(project,
                                                         getModelObject());
    }

    /**
     * Returns a saved window zoom level for a previously-edited model object.
     * Recall that the map stores the zoom factor in a <code>Double</code>
     * instance.
     * <p>
     * Convenience function for subclasses that passes request through to
     * the ControllerNexus.
     *
     * @param model the model object being edited in the window
     * @return the zoom level previously saved for this model
     */
    public double getWindowZoom(Object model)
    {
        return CogTool.controllerNexus.getWindowZoom(project, model);
    }

    /**
     * Saves a window zoom level associated with a particular model object.
     * <p>
     * Convenience function for subclasses that passes request through to
     * the ControllerNexus.
     *
     * @param model the model object being edited in the window
     * @param loc the zoom level for the window
     */
    public void saveWindowZoom(Object model, double zoom)
    {
        CogTool.controllerNexus.saveWindowZoom(project, model, zoom);
    }

    /**
     * Close the registered open controller for the associated model object.
     * Used when the object or an ancestor has been removed from the model.
     *
     * @author mlh
     */
    protected void closeOpenController()
    {
// MLH TODO: Be nice if we didn't have to import ControllerRegistry
//           because of its interface use of DefaultController!
        Controller c =
            ControllerRegistry.ONLY.findOpenController(getModelObject());

        if (c != null) {
            c.dispose();
        }
    }
}
