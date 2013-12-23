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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolLID.ConverterFilesLID;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.EnableDisable;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.AboutView;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * Abstract class for sharing implementation for UI instances.
 * Used specifically by the "invisible" root window on Macs.
 * <p>
 * A <code>ListenerIdentifierMap</code> is used to associate a set of user
 * interface widgets (such as buttons and menu items) and specific events
 * (such as double-click or right-button-click) with a semantic application
 * operation.  Controller objects may attach specific code "listeners"
 * to react to those events on those widgets, or they may control the
 * "enabled" state and visibility of those widgets.
 *
 * @author mlh
 */
public abstract class UI implements ListenerIdentifier.ILIDTransmuter
{
    protected static final String TEXT_COPIED =
        L10N.get("AUI.TextCopied", "Text copied to the clipboard");

    protected static final String NOTHING_PASTED =
        L10N.get("AUI.NothingPasted", "Nothing pasted");

    /**
     * Any figure that is not dynamic but also does not represent directly
     * something from the model; typically used to indicate some aspect
     * of selection state.
     */
    public interface IUIFigure
    {
        public void show();
        public void update();
        public void hide();
    }

    /**
     * ListenerIdentifier subclass to handle Text selection commands.
     *
     * @author mlh
     */
    protected static class TextLID extends CogToolLID
    {
        public TextLID(String newLabel, int newPersistenceValue)
        {
            super(newLabel, newPersistenceValue);
        }

        public int getLIDCode()
        {
            return persistenceValue;
        }
    }

    public static class SetAttributeParameters
    {
        /**
         * The object whose attribute will change
         */
        public IAttributed target;

        /**
         * The name of the attribute to be changed
         */
        public String attrName;

        /**
         * The new value of the attribute
         */
        public Object value;

        public SetAttributeParameters(IAttributed attributed,
                                      String name,
                                      Object val)
        {
            target = attributed;
            attrName = name;
            value = val;
        }
    }

    protected static final int CutTextCode = 51;
    protected static final int CopyTextCode = 52;
    protected static final int PasteTextCode = 53;
    protected static final int SelectAllTextCode = 54;
    protected static final int DeselectAllTextCode = 55;

    protected static final TextLID CutText =
        new TextLID("CutText", CutTextCode);

    protected static final TextLID CopyText =
        new TextLID("CopyText", CopyTextCode);

    protected static final TextLID PasteText =
        new TextLID("PasteText", PasteTextCode);

    protected static final TextLID SelectAllText =
        new TextLID("SelectAllText", SelectAllTextCode);

    protected static final TextLID DeselectAllText =
        new TextLID("DeselectAllText", DeselectAllTextCode);

    /**
     * Specific LIDs for Text clipboard operations.
     */
    public static final Map<ListenerIdentifier, TextLID> textLIDs =
        new HashMap<ListenerIdentifier, TextLID>();
    static {
        textLIDs.put(CogToolLID.Cut, CutText);
        textLIDs.put(CogToolLID.Copy, CopyText);
        textLIDs.put(CogToolLID.Paste, PasteText);
        textLIDs.put(CogToolLID.SelectAll, SelectAllText);
        textLIDs.put(CogToolLID.DeselectAll, DeselectAllText);
    }

    /**
     * Used to associate a semantic user interface action to a corresponding
     * snippet of application Controller code.
     */
    protected ListenerIdentifierMap lIDMap = new ListenerIdentifierMap();

    public static final String WINDOW_TITLE =
    L10N.get("PM.WindowTitle", " - CogTool");

    /**
     * Initializes the shared implementation by creating a new, empty
     * <code>ListenerIdentifierMap</code> instance.
     *
     * @author mlh
     */
    public UI()
    {
        setAction(CogToolLID.Preferences, createPreferencesAction());
    }

    protected IListenerAction createPreferencesAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                Interaction interactSpt = getStandardInteraction();
                boolean result = interactSpt.editPreferences();

                if (result) {
                    MenuFactory.updateMenuDefinitions();
                    MenuFactory.rebuildAllMenus();
                    interactSpt.setStatusMessage(L10N.get("PREF.PreferencesSet",
                                                          "Preferences set."));
                }
                else {
                    interactSpt.setStatusMessage(Interaction.CANCELED_MSG);
                }

                return result;
            }
        };
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     * <p>
     * All windows support: NewProject, OpenProject, ExitApplication, About
     *
     * @author mlh
     * @param forConstruction set to true if called during UI construction,
     *                        false otherwise
     */
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        // Some items should always be enabled.
        setEnabled(CogToolLID.NewProject,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.OpenProject,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        if (! OSUtils.MACOSX) {
            // On Macintosh, SWT automagically handles the enabling of things
            // in the CogTool menu.
            setEnabled(CogToolLID.ExitApplication,
                       ListenerIdentifierMap.ALL,
                       MenuUtil.ENABLED);
            setEnabled(CogToolLID.About,
                       ListenerIdentifierMap.ALL,
                       MenuUtil.ENABLED);
            setEnabled(CogToolLID.Preferences,
                       ListenerIdentifierMap.ALL,
                       MenuUtil.ENABLED);
        }
    }

    /**
     * This method provides the View instance associated with this UI
     * instance that is used to support the shared implementation.
     *
     * @return the associated View instance
     * @author mlh
     */
    public abstract View getView();

    /**
     * Fetch the Shell being used to present the view's window.
     *
     * @return the SWT Shell instance for the view's window
     * @author mlh
     */
    public Shell getShell()
    {
        View view = getView();

        if (view != null) {
            return view.getShell();
        }

        throw new RcvrUIException("No view available for fetching the associated window.");
    }

    /**
     * Recover any system resources being used to support the view being
     * used by this UI instance.
     * <p>
     * This should be called last by overriding subclasses!
     *
     * @author mlh
     */

    public void dispose()
    {
        View view = getView();

        if (view != null) {
            view.dispose();
        }
        else {
            throw new RcvrUIException("No view available for recovering system resources.");
        }
    }

    /**
     * Set whether the associated window (and view) is visible.
     *
     * @param visible true iff the associated window/view should be visible
     * @author mlh
     */

    public void setVisible(boolean visible)
    {
        View view = getView();

        if (view != null) {
            view.setVisible(visible);
        }
        else {
            throw new RcvrUIException("No view available for setting view visibility.");
        }
    }

    /**
     * Bring the associated window to the front and make active by
     * making the window have the focus.
     *
     * @author mlh
     */

    public void takeFocus()
    {
        View view = getView();

        if (view != null) {
            view.takeFocus();
        }
        else {
            throw new RcvrUIException("No view available for taking focus.");
        }
    }

    /**
     * Request that the associated window be closed; it is possible for some
     * interaction with the user to occur which might "cancel" the operation.
     *
     * @return true if and only if the close was successful.
     */

    public boolean requestClose()
    {
        View view = getView();

        if (view != null) {
            return view.requestClose();
        }

        throw new RcvrUIException("No view available for requesting close.");
    }

    /**
     * Associate the provided semantic application code snippet with the
     * given <code>CogToolLID</code> key.
     * <p>
     * This function will replace the current code snippet object associated
     * with the identifier, if one exists.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @param action    the object representing the code snippet to be
     *                  performed for that <code>id</code>
     * @return the code snippet object previously associated with the given
     *         <code>id</code>; if none, then <code>null</code> is returned
     * @author mlh
     */

    public IListenerAction setAction(CogToolLID id, IListenerAction action)
    {
        return lIDMap.setAction(id, action);
    }

    /**
     * Remove the code snippet object associated with the given identifier,
     * if one exists.
     *
     * @param id        the key specifying the semantic nature of the
     *                  action to be performed
     * @return the code snippet object associated with the given
     *         <code>id</code>; if none, then <code>null</code> is returned
     * @author mlh
     */

    public IListenerAction removeAction(CogToolLID id)
    {
        return lIDMap.removeAction(id);
    }

    /**
     * Do any set-up before <code>performAction</code> is invoked.
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     */
    protected void setUpPerformAction(ListenerIdentifier id)
    {
        // do nothing by default; subclasses might want to override
    }

    /**
     * Perform the requested semantic action.
     * <p>
     * Finds the code snippet object associated with the given semantic
     * identifier and, if found, executes the application's semantic action.
     * <p>
     * If not found, this simply does nothing.  By not throwing an exception,
     * we allow for flexibility as to how to enable/disable certain
     * functionality.
     * <p>
     * Set-up of the actual <code>performAction</code> call only occurs
     * if cleanup is requested, since set-up might require cleanup!
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     * @param actionParms data needed by this operation to perform the action
     * @param doCleanup whether or not to perform the cleanup operation
     *                  (non-menuHidden!)
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     * @author mlh
     */

    public boolean performAction(ListenerIdentifier id,
                                 Object actionParms,
                                 boolean doCleanup)
    {
        View view = getView();

        boolean okToProceed = false;
        boolean wasPerforming = view.isPerformingAction();

        view.setStatusMessage("");
        view.setPerformingAction(true);

        try {
            if (doCleanup) {
                setUpPerformAction(id);
            }

            okToProceed = lIDMap.performAction(id, actionParms);

            if (doCleanup) {
                cleanup(okToProceed, false);    // not a context menu "hide"!
            }
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex, getStandardInteraction());
        }
        finally {
            view.setPerformingAction(wasPerforming);
        }

        return okToProceed;
    }

    /**
     * Same as three-parameter performAction that requests cleanup.
     */

    public boolean performAction(ListenerIdentifier id, Object actionParms)
    {
        return performAction(id, actionParms, true);    // request cleanup!
    }

    /**
     * Wrapper method for invoking performAction; uses this as the transmuter
     * and (!) assumes NOT caused by a context menu.  Simply invokes
     * the 2-parameter performAction with false for isContextSelection.
     *
     * @param id the general semantic LID to lookup and perform action for
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     */

    public boolean performAction(ListenerIdentifier id)
    {
        return performAction(id, false);
    }

    /**
     * Wrapper method for invoking performAction; uses this as the
     * transmuter.  ContextMenuManager is currently the only place that
     * invokes performAction as "context".
     *
     * @param id the general semantic LID to lookup and perform action for
     * @param isContextSelection true if we should specialize based on the
     *                           current contextual selection;
     *                           false to use the standard selection
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     */

    public boolean performAction(ListenerIdentifier id,
                                 boolean isContextSelection)
    {
        View view = getView();
        boolean wasPerforming = view.isPerformingAction();
        Shell s = getShell();
        Cursor oldCursor = s.getCursor();

        view.setStatusMessage("");
        view.setPerformingAction(true);
        s.setCursor(WindowUtil.BUSY_CURSOR_INSTANCE);

        // false means a normal action, not caused by a context menu selection
        try {
            return lIDMap.performAction(this, id, isContextSelection);
        }
        catch (RecoverableException ex) {
            RcvrExceptionHandler.recover(ex, getStandardInteraction());
        }
        finally {
            view.setPerformingAction(wasPerforming);
            if (! s.isDisposed()) {
                s.setCursor(oldCursor);
            }
        }

        return false;
    }

    /**
     * Helper function to enable/disable an SWT user interface widget and,
     * possibly, reset its associated label.
     *
     * @param target   the SWT user interface widget to enable/disable
     * @param enable   whether the widget should be enabled (<code>true</code>)
     *                 or disabled (<code>false</code>)
     * @param newLabel if not null, the new label for the widget (if it
     *                 supports a label)
     * @author mlh
     */
    protected void setEnabled(Object target, boolean enable, String newLabel)
    {
        if (target instanceof EnableDisable) {
            ((EnableDisable) target).setEnabled(enable);
        }
        else if (target instanceof Control) {
            ((Control) target).setEnabled(enable);

            if (newLabel != null) {
                if (target instanceof Button) {
                    ((Button) target).setText(newLabel);
                }
                else if (target instanceof Label) {
                    ((Label) target).setText(newLabel);
                }
            }
        }
        else if (target instanceof MenuItem) {
            ((MenuItem) target).setEnabled(enable);

            if (newLabel != null) {
                MenuUtil.relabelItem(((MenuItem) target), newLabel);
            }
        }
        else if (target instanceof Menu) {
            ((Menu) target).setEnabled(enable);
        }
    }

    /**
     * Change the "enabled" state of all of the user interface widgets
     * associated with the given id.
     * <p>
     * The "enabled" state of some widgets typically changes depending
     * on the state of the application, such as when parts of the
     * application state become "selected" or "de-selected".
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be affected
     * @param enable   whether the widgets associated with the given id
     *                 should become enabled (<code>true</code>) or disabled
     *                 (<code>false</code>)
     * @author mlh
     */

    public void setEnabled(CogToolLID id, Boolean availability, boolean enable)
    {
        setEnabled(id, availability, enable, null, null);
    }

    /**
     * Change the "enabled" state of all of the user interface widgets
     * associated with the given id.  The widgets' label is also modified.
     * <p>
     * The "enabled" state of some widgets typically changes depending
     * on the state of the application, such as when parts of the
     * application state become "selected" or "de-selected".
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be affected
     * @param enable   whether the widgets associated with the given id
     *                 should become enabled (<code>true</code>) or disabled
     *                 (<code>false</code>)
     * @param newLabel if not null, the new label to use for the user interface
     *                 widgets associated with the given id that support labels
     * @author mlh
     * @see ListenerIdentifierMap.getWidgets
     */

    public void setEnabled(CogToolLID id,
                           Boolean availability,
                           boolean enable,
                           String newLabel)
    {
        setEnabled(id, availability, enable, newLabel, null);
    }

   /**
     * Change both the "enabled" state and the selection state (whether or not
     * they are checked) of all of the user interface widgets
     * associated with the given id.  The widgets' label may also be modified.
     * <p>
     * The "enabled" state of some widgets typically changes depending
     * on the state of the application, such as when parts of the
     * application state become "selected" or "de-selected".
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be affected
     * @param enable   whether the widgets associated with the given id
     *                 should become enabled (<code>true</code>) or disabled
     *                 (<code>false</code>)
     * @param newLabel if not null, the new label to use for the user interface
     *                 widgets associated with the given id that support labels
     * @param selected for all associated widgets that are MenuItems set
     *                 their selection state to this
     * @see ListenerIdentifierMap.getWidgets
     */
    public void setEnabled(CogToolLID id,
                           Boolean availability,
                           boolean enable,
                           String newLabel,
                           boolean selected)
    {
        setEnabled(id, availability, enable, newLabel, Boolean.valueOf(selected));
    }

    protected void setEnabled(CogToolLID id,
                              Boolean availability,
                              boolean enable,
                              String newLabel,
                              Boolean selected)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object obj = targets.next();

            setEnabled(obj, enable, newLabel);

            if (obj instanceof MenuItem) {
                ((MenuItem) obj).setSelection((selected != null)
                                                   && selected.booleanValue());
            }
        }
    }

    /**
     * Indicate the "current" state in the user interface of the functionality
     * represented by the given id.
     * <p>
     * The functionality may be disabled even if the user interface widgets
     * associated with the semantic operation are enabled since the "parent"
     * widget(s) may be disabled (such as a menu containing the associated
     * menu item).
     * <p>
     * Clearly, this method returns <code>true</code> iff both the direct
     * user interface widgets represented by the given id and all of their
     * parents are enabled.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return whether the user can "invoke" the operation represented
     *         by the given id
     * @author mlh
     */

    public boolean isEnabled(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof EnableDisable) {
                return ((EnableDisable) target).isEnabled();
            }

            if (target instanceof Control) {
                return ((Control) target).isEnabled();
            }

            // TODO: Check if we should look at parent SWT Menu items
            if (target instanceof MenuItem) {
                return ((MenuItem) target).getEnabled();
            }

            // TODO: Check if we should look at parent SWT Menu items
            if (target instanceof Menu) {
                return ((Menu) target).getEnabled();
            }
        }

        return true;
    }

    /**
     * Indicate the "current" state of the user interface widgets associated
     * with the given id.
     * <p>
     * Unlike <code>isEnabled</code>, this method is only concerned with
     * the current state of just the widgets associated with the given id,
     * regardless of the state of any "parent" widget(s).
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return whether the widgets associated with the given id are enabled
     * @author mlh
     */

    public boolean getEnabled(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof EnableDisable) {
                return ((EnableDisable) target).isEnabled();
            }

            if (target instanceof Control) {
                return ((Control) target).getEnabled();
            }

            if (target instanceof MenuItem) {
                return ((MenuItem) target).getEnabled();
            }

            if (target instanceof Menu) {
                return ((Menu) target).getEnabled();
            }
        }

        return true;
    }

    /**
     * Similar to getEnabled, but instead of returning the enabled state, it
     * returns the string associated with the id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return the label describing the id's action
     */

    public String getLabel(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof Control) {
                if (target instanceof Button) {
                    return ((Button) target).getText();
                }

                if (target instanceof Label) {
                    return ((Label) target).getText();
                }
            }

            if (target instanceof MenuItem) {
                return ((MenuItem) target).getText();
            }
        }

        return null;
    }

    /**
     * Change the accelerator key that "invokes" the semantic operation
     * associated with the given id.
     *
     * @param id           the representation of the semantic application
     *                     operation
     * @param availability the subset of widgets that should be affected
     * @param accelerator  the new key the user may employ to "invoke" that
     *                     operation
     * @author mlh
     */

    public void setAccelerator(CogToolLID id,
                               Boolean availability,
                               int accelerator)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof MenuItem) {
                ((MenuItem) target).setAccelerator(accelerator);
            }
        }
    }

    /**
     * Indicate the accelerator key currently associated
     * with the given id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return the accelerator key the user may currently employ to "invoke"
     *         the semantic operation represented by the id; if no key
     *         is associated, the <code>SWT.NONE</code> is returned
     * @author mlh
     */

    public int getAccelerator(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof MenuItem) {
                return ((MenuItem) target).getAccelerator();
            }
        }

        return SWT.NONE;
    }

    /**
     * Set the visibility of all of the user interface widgets
     * associated with the given id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be affected
     * @param visible  whether the widgets associated with the given id
     *                 should become visible (<code>true</code>) or invisible
     *                 (<code>false</code>)
     * @author mlh
     */

    public void setVisible(CogToolLID id,
                           Boolean availability,
                           boolean visible)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof Control) {
                ((Control) target).setVisible(visible);
            }
            else if (target instanceof Menu) {
                ((Menu) target).setVisible(visible);
            }
        }
    }

    /**
     * Indicate the current visibility of the user interface widgets
     * associated with the given id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return the current visibility of the widgets associated with the
     *         given id
     * @author mlh
     */

    public boolean isVisible(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof Control) {
                return ((Control) target).isVisible();
            }
            else if (target instanceof Menu) {
                return ((Menu) target).isVisible();
            }
        }

        return true;
    }

    /**
     * Set the selected state of all of the user interface widgets
     * associated with the given id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be affected
     * @param selected whether the widgets associated with the given id
     *                 should become selected (<code>true</code>) or unselected
     *                 (<code>false</code>)
     * @author mlh
     */

    public void setSelected(CogToolLID id,
                            Boolean availability,
                            boolean selected)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof Button) {
                ((Button) target).setSelection(selected);
            }
            else if (target instanceof MenuItem) {
                ((MenuItem) target).setSelection(selected);
            }
        }
    }

    /**
     * Indicate the current selected state of the user interface widgets
     * associated with the given id.
     *
     * @param id       the representation of the semantic application operation
     * @param availability the subset of widgets that should be queried
     * @return the current selected state of the widgets associated with the
     *         given id
     * @author mlh
     */

    public boolean isSelected(CogToolLID id, Boolean availability)
    {
        Iterator<Object> targets = lIDMap.getWidgets(id, availability);

        while (targets.hasNext()) {
            Object target = targets.next();

            if (target instanceof Button) {
                return ((Button) target).getSelection();
            }
            else if (target instanceof MenuItem) {
                return ((MenuItem) target).getSelection();
            }
        }

        return true;
    }

    /**
     * Pop the About dialog box.
     *
     * @return the action for popping the About dialog box
     * @author mlh
     */

    public IListenerAction popAboutBox()
    {
        Shell window = getShell();

        if (window != null) {
            return new AboutView.PopAction(getShell());
        }

        throw new RcvrUIException("No parent window available for About.");
    }

    /**
     * Transforms an "object-oriented" <code>ListenerIdentifier</code>
     * into a more specific value representing an actual, concrete
     * application function, depending upon the internal state of the
     * application itself (such as, based on what application elements
     * are currently selected).
     * <p>
     * If there is no more specific value for the given <code>id</code>,
     * the input value should be returned unchanged.
     *
     * @param id the key specifying the semantic nature of the
     *           action to be performed
     * @param isContextSelection true if we should specialize based on the
     *                           current contextual selection;
     *                           false to use the standard selection
     * @return the specific value representing an actual, concrete
     *         application function, or, if none exists, the input value
     * @author mlh
     */

    public ListenerIdentifier transmute(ListenerIdentifier id,
                                        boolean isContextSelection)
    {
        boolean useTextLID = true;
        TextLID specificLID;

        // Check if a text field has the focus
        Text textWidgetWithFocus = WindowUtil.getFocusedText();

        if (textWidgetWithFocus != null) {
            specificLID = textLIDs.get(id);

            if (specificLID != null) {
                switch (specificLID.getLIDCode()) {
                    case CutTextCode: {
                        textWidgetWithFocus.cut();
                        break;
                    }
                    case CopyTextCode: {
                        textWidgetWithFocus.copy();
                        getStandardInteraction().setStatusMessage(TEXT_COPIED);
                        break;
                    }
                    case PasteTextCode: {
                        if (CogToolClipboard.hasCogToolObjects()) {
                            useTextLID = false;
                        }
                        else if (ClipboardUtil.hasTextData()) {
                            textWidgetWithFocus.paste();
                        }
                        else {
                            useTextLID = false; // Controller will do nothing
                            getStandardInteraction().setStatusMessage(NOTHING_PASTED);
                        }
                        break;
                    }
                    case SelectAllTextCode: {
                        textWidgetWithFocus.selectAll();
                        break;
                    }
                    case DeselectAllTextCode: {
                        int start = textWidgetWithFocus.getSelection().x;
                        textWidgetWithFocus.setSelection(start, start);
                        break;
                    }
                }

                if (useTextLID) {
                    return null;
                }
            }
        }

        // Check if a text field has the focus
        Combo comboWidgetWithFocus = WindowUtil.getFocusedCombo();

        if (comboWidgetWithFocus != null) {
            specificLID = textLIDs.get(id);

            if (specificLID != null) {
                switch (specificLID.getLIDCode()) {
                    case CutTextCode: {
                        comboWidgetWithFocus.cut();
                        break;
                    }
                    case CopyTextCode: {
                        comboWidgetWithFocus.copy();
                        break;
                    }
                    case PasteTextCode: {
                        if (CogToolClipboard.hasCogToolObjects()) {
                            useTextLID = false;
                        }
                        else if (ClipboardUtil.hasTextData()) {
                            comboWidgetWithFocus.paste();
                        }
                        else {
                            useTextLID = false; // Controller will do nothing
                        }
                        break;
                    }
                    case SelectAllTextCode: {
// When it becomes available, replace the following with: comboWidgetWithFocus.selectAll();
                        int textLen = comboWidgetWithFocus.getText().length();
                        comboWidgetWithFocus.setSelection(new Point(0, textLen));
                        break;
                    }
                    case DeselectAllTextCode: {
                        Point currentSel = comboWidgetWithFocus.getSelection();

                        currentSel.y = currentSel.x;
                        comboWidgetWithFocus.setSelection(currentSel);
                        break;
                    }
                }

                if (useTextLID) {
                    return null;
                }
            }
        }

        setUpPerformAction(id);

        if (id instanceof CogToolLID.OpenRecentLID) {
            return CogToolLID.OpenProjectFile;
        }

       if (id instanceof CogToolLID.ConverterFilesLID) {
            System.out.println("return newdesign2");
            CogToolLID.NewDesignFromImport.setClassAttribute(((ConverterFilesLID) id).getClassAttribute());
            return CogToolLID.NewDesignFromImport;
        }

        return id;
    }

    /**
     * Utility to support subclasses determining whether the given LID
     * should ensure changes to properties are committed before executing.
     *
     * @param lid the CogTool command being queried
     * @return whether the given LID should cause the commit of any
     *         property change in progress
     */
    protected static boolean idCommitsChanges(ListenerIdentifier lid)
    {
        if (lid instanceof CogToolLID) {
            return ((CogToolLID) lid).commitsPropertyChanges;
        }

        return CogToolLID.NO_COMMITS;
    }

    /**
     * Utility to support subclasses determining whether the
     * given LID can cause selection to occur (in overrides of getParameters),
     * allowing them to set the appropriate delayed work units "active".
     *
     * @param lid the CogTool command being queried
     * @return whether the given LID is a CogToolLID that
     *         can cause selection
     */
    protected static int canCauseSelection(ListenerIdentifier lid)
    {
        if (lid instanceof CogToolLID) {
            return ((CogToolLID) lid).causesSelection;
        }

        return CogToolLID.CAUSES_NO_SELECTION;
    }

    /**
     * Simple test for given flag.
     */
    protected static boolean isSelectionFlagSet(int mask, int flag)
    {
        return (mask & flag) != CogToolLID.CAUSES_NO_SELECTION;
    }

    /**
     * Utility to support subclasses determining whether the
     * given LID should commit pending property changes.
     * <p>
     * Handles CogToolLID.Undo/Redo by fetching the corresponding LID
     * from the appropriate IUndoableEdit from the given undo manager.
     *
     * @param lid the CogTool command being queried
     * @return whether the given LID should cause the commit of any
     *         property change in progress
     */
    protected boolean doesIDCommitChanges(ListenerIdentifier lid)
    {
        return idCommitsChanges(lid);
    }

    /**
     * Utility to support subclasses determining whether the
     * given LID can cause selection to occur (in overrides of getParameters),
     * allowing them to set the appropriate delayed work units "active".
     * <p>
     * Handles CogToolLID.Undo/Redo by fetching the corresponding LID
     * from the appropriate IUndoableEdit from the given undo manager.
     *
     * @param lid the CogTool command being queried
     * @param undoMgr the window's undo manager in case the given LID is
     *                CogToolLID.Undo or CogToolLID.Redo
     * @return whether the given LID is a CogToolLID that
     *         can cause selection
     */
    protected int canIDCauseSelection(ListenerIdentifier lid)
    {
        return canCauseSelection(lid);
    }

    /**
     * Fetches the parameters needed by any <code>performAction</code>
     * invoked for the "specialized" <code>ListenerIdentifier</code>.
     * In some cases, the determination of the parameters requires
     * information from the original "general" LID subclass instance
     * (see, for example, SEDemoLID).
     *
     * Returns UNSET to indicate that it is the responsibility
     * of the subclass to return something valid (i.e., not UNSET).
     *
     * @param originalLID  the general LID value returned from a menu command
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

    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        if (originalLID instanceof CogToolLID.OpenRecentLID) {
            return ((CogToolLID.OpenRecentLID) originalLID).path;
        }

       /* if (originalLID instanceof CogToolLID.ConverterFilesLID) {
            System.out.println("returning the conv class of the file");
            return ((CogToolLID.ConverterFilesLID) originalLID).converterClass;*/
       // }

        return UNSET;
    }

    /**
     * Allows the interfaces to clean up any feedback provided to the
     * user before and during a performAction.
     *
     * @param okToContinue the return value from performAction
     * @param menuHidden whether or not the context menu is dismissed
     *                   without selecting an operation to perform
     * @author mlh
     */

    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        CogTool.delayedWorkMgr.doDelayedWork(okToContinue);
    }

    /**
     * Requests the UI to show a context menu based on the
     * current selection
     */

    public void showContextMenu()
    {
        // Stub implementation that does nothing
    }

    /**
     * Requests the UI to show a context menu
     *
     * @param x x coordinate at which to show the menu
     * @param y y coordinate at which to show the menu
     */

    public void showContextMenu(int x, int y)
    {
        // Assume the default behavior is to raise the standard context menu
        showContextMenu();
    }

    /**
     * Get the location of the UI's window
     */

    public DoublePoint getLocation()
    {
        Shell window = getShell();
        if (window != null) {
            Point loc = window.getLocation();
            return new DoublePoint(loc.x, loc.y);
        }

        return null;
    }

    /**
     * Get the extend of the UI's window
     */

    public DoubleRectangle getExtent()
    {
        Shell window = getShell();
        if (window != null) {
            Rectangle loc = window.getBounds();
            return new DoubleRectangle(loc.x,
                                       loc.y,
                                       loc.width,
                                       loc.height);
        }

        return null;
    }

    /**
     * Set the location of the UI's window
     */

    public void setLocation(DoublePoint loc)
    {
        setLocation(loc.x, loc.y);
    }

    /**
     * Set the location of the UI's window
     */

    public void setLocation(double x, double y)
    {
        Shell window = getShell();
        if (window != null) {
            window.setLocation((int) x, (int) y);
        }
    }

    /**
     * Standard interaction needed by AController;
     * leaf subclasses must implement.
     *
     * @author mlh
     */
    public abstract Interaction getStandardInteraction();

}
