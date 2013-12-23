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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

/**
 * Support for creating application menus, both menu bars and pop-up menus.
 * <p>
 * It is not necessary (and therefore not allowed) to create an instance
 * of <code>MenuUtil</code>.
 * <p>
 * Menu items have both optional mnemonics (a letter in the menu item's text)
 * and optional accelerators (a keystroke).
 * <p>
 * Mnemonics are indicated by an '&' in the name field that causes the
 * next character to be the mnemonic.
 * Example: "&Quit" means that 'q' is the mnemonic.
 * <p>
 * An accelerator is the bit-wise OR of zero or more modifier masks
 * and a key.
 * <code><literal>
 * Examples:  SWT.NONE | SWT.MOD1 | SWT.MOD2 | 'T',
 *            SWT.MOD3 | SWT.F2
 *            SWT.CONTROL | SWT.SHIFT | 'T',
 *            SWT.ALT | SWT.F2.
 * </literal></code><br>
 * When using Accelerator keys,
 * "command" is standard on the Mac, "control" on Windows.
 * There is no META key as in Swing.
 * Thus, methods are provided that decide based on the operating system.
 */

public class MenuUtil
{
    /**
     * Separator for menus
     */
    final static public SimpleMenuItemDefinition SEPARATOR =
        new SimpleMenuItemDefinition();

    protected MenuUtil() { }        // Prevent instantiation

    /**
     * Utility to determine which modifier mask to use based on
     * the operating system on which the application is executing.
     *
     * @author alex
     */
    public static int platformControlKey()
    {
        if (OSUtils.MACOSX) {
            return SWT.COMMAND;
        }
        return SWT.CONTROL;
    }

    /**
     * Utility to determine the other modifier mask to use based on
     * the operating system on which the application is executing.
     *
     * @author alex
     */
    public static int notPlatformControlKey()
    {
        if (OSUtils.MACOSX) {
            return SWT.CONTROL;
        }
        return SWT.COMMAND;
    }


    /**
     * Base class for declarative definitions of menus.
     * <p>
     * Whether defining a simple menu item or a menu containing
     * other items, label components are allowed (localized text name
     * and image, both optional).
     *
     * @author alex, mlh
     */
    public static abstract class MenuItemDefinition
    {
        public String name;
        public Image image = null;

        /**
         * Initializes the label components.
         * <p>
         * This version initializes the image label component to
         * <code>null</code>.
         *
         * @param inName the localized string label component
         * @author alex, mlh
         */
        protected MenuItemDefinition(String inName)
        {
            name = inName;
        }

        /**
         * Initializes the label components.
         *
         * @param inName  the localized string label component
         * @param inImage the image label component
         * @author alex, mlh
         */
        protected MenuItemDefinition(String inName, Image inImage)
        {
            name = inName;
            image = inImage;
        }

        /**
         * Answers whether this instance represents a single menu item
         * or a cascading menu.
         *
         * @return      true iff this instance represents a single menu item
         *
         * @author alex, mlh
         */
        public abstract boolean isSingle();
    }

    /**
     * Constant indicating that menu item should be enabled initially.
     */
    public static final boolean ENABLED = true;

    /**
     * Constant indicating that menu item should be disabled initially.
     */
    public static final boolean DISABLED = false;

    /**
     * Class for declarative definitions of singleton menu items.
     * <p>
     * Each singleton menu item may have an associated accelerator key.
     * <p>
     * In addition, to facilitate separating controller from view,
     * each item can be assigned a <code>ListenerIdentifier</code>
     * that may be used by the controller to specify the actions to perform
     * when the associated menu item is selected.
     * <p>
     * A separator is an unselectable menu item that has no label
     * components.  No action may be associated with a menu separator,
     * so no <code>ListenerIdentifier</code> should be assigned.
     * See the zero-parameter constructor.
     *
     * @author alex, mlh
     */
    public static class SimpleMenuItemDefinition extends MenuItemDefinition
    {
        public int accelerator = SWT.NONE;
        public int style = SWT.PUSH;
        public ListenerIdentifier listenerID;
        public boolean enabledInitially = DISABLED;
        public boolean selectedInitially = DISABLED;

        /**
         * Specifies the separator menu item.
         *
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition()
        {
            super(null);

            style = SWT.SEPARATOR;
            listenerID = null;
        }

        /**
         * Specifies a menu item with just a text label without
         * an accelerator key.
         *
         * @param inName  the localized string label component
         * @param inLID   the listener ID for later action assignment
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID)
        {
            super(inName);

            listenerID = inLID;
        }

        /**
         * Specifies a menu item with just a text label and an initial
         * enabled state, but without an accelerator key.
         *
         * @param inName  the localized string label component
         * @param inLID   the listener ID for later action assignment
         * @param enabled whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        boolean enabled)
        {
            super(inName);

            listenerID = inLID;
            enabledInitially = enabled;
        }

        /**
         * Specifies a menu item with both a text and image label but without
         * an accelerator key.
         *
         * @param inName  the localized string label component
         * @param inImage the image label component
         * @param inLID   the listener ID for later action assignment
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID)
        {
            super(inName, inImage);

            listenerID = inLID;
        }

        /**
         * Specifies a menu item with both a text and image label, an initial
         * enabled state, but without an accelerator key.
         *
         * @param inName  the localized string label component
         * @param inImage the image label component
         * @param inLID   the listener ID for later action assignment
         * @param enabled whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        boolean enabled)
        {
            super(inName, inImage);

            listenerID = inLID;
            enabledInitially = enabled;
        }

        /**
         * Specifies a menu item with just a text label with
         * an accelerator key.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         *
         * @param inName        the localized string label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        int inAccelerator)
        {
            super(inName);

            listenerID = inLID;
            accelerator = inAccelerator;
        }

        /**
         * Specifies a menu item with just a text label with
         * an accelerator key and initial enabled state.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         *
         * @param inName        the localized string label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param enabled       whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        boolean enabled)
        {
            super(inName);

            listenerID = inLID;
            accelerator = inAccelerator;
            enabledInitially = enabled;
        }

        /**
         * Specifies a menu item with both a text and image label, with
         * an accelerator key.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         *
         * @param inName        the localized string label component
         * @param inImage       the image label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        int inAccelerator)
        {
            super(inName, inImage);

            listenerID = inLID;
            accelerator = inAccelerator;
        }

        /**
         * Specifies a menu item with both a text and image label, with
         * an accelerator key and initial enabled state.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         *
         * @param inName        the localized string label component
         * @param inImage       the image label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param enabled       whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        boolean enabled)
        {
            super(inName, inImage);

            listenerID = inLID;
            accelerator = inAccelerator;
            enabledInitially = enabled;
        }

        /**
         * Specifies a menu item with just a text label with
         * an accelerator key.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * @param inName        the localized string label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle)
        {
            super(inName);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
        }

        /**
         * Specifies a menu item with a text label, an accelerator key,
         * that is enabled,
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * @param inName        the localized string label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle,
                                        boolean inEnabled)
        {
            super(inName);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
            enabledInitially = inEnabled;
        }

        /**
         * Specifies a menu item with a text label, an accelerator key,
         * that is enabled, and selected
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * Selected
         * Used to cause checkboxes or radio buttons to appear selected
         * No effect if the SWT.Style is not Check, or Radio
         *
         * @param inName        the localized string label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @param inEnabled     is the menu item visible.
         * @param inSelected    is the menu item selected.
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle,
                                        boolean inEnabled,
                                        boolean inSelected)
        {
            super(inName);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
            enabledInitially = inEnabled;
            selectedInitially = inSelected;
        }

        /**
         * Specifies a menu item with both a text and image label, with
         * an accelerator key.  Allows setting the SWT style.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * @param inName        the localized string label component
         * @param inImage       the image label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle)
        {
            super(inName, inImage);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
        }

        /**
         * Specifies a menu item with both a text and image label, with
         * an accelerator key and initial enabled state.
         * Allows setting the SWT style.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * @param inName        the localized string label component
         * @param inImage       the image label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @param enabled       whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle,
                                        boolean enabled)
        {
            super(inName, inImage);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
            enabledInitially = enabled;
        }

        /**
         * Specifies a menu item with both a text and image label, with
         * an accelerator key, initial enabled state, and if it is selected
         * Allows setting the SWT style.
         * <p>
         * Specify <code>SWT.NONE</code> for the accelerator key
         * if not desired.
         * <p>
         * Choices for SWT style:
         * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
         *
         * Selected
         * Used to cause checkboxes or radio buttons to appear selected
         * No effect if the SWT.Style is not Check, or Radio
         *
         * @param inName        the localized string label component
         * @param inImage       the image label component
         * @param inLID         the listener ID for later action assignment
         * @param inAccelerator the accelerator's keycode
         * @param inStyle       the menu item style
         * @param enabled       whether the item should be enabled initially
         * @author alex, mlh
         */
        public SimpleMenuItemDefinition(String inName,
                                        Image inImage,
                                        ListenerIdentifier inLID,
                                        int inAccelerator,
                                        int inStyle,
                                        boolean enabled,
                                        boolean selected)
        {
            super(inName, inImage);

            listenerID = inLID;
            accelerator  = inAccelerator;
            style = inStyle;
            enabledInitially = enabled;
            selectedInitially = selected;
        }

        /**
         * Answers whether this instance represents a single menu item
         * or a cascading menu.
         * <p>
         * Indicates that this instance is a single menu item.
         *
         * @return      true
         *
         * @author alex, mlh
         */
        @Override
        public boolean isSingle()
        {
            return true;
        }
    }

    /**
     * Class for declarative definitions of cascading menus.
     *
     * @author alex, mlh
     */
    public static class CascadingMenuItemDefinition extends MenuItemDefinition
    {
        public MenuItemDefinition[] menuItems;
        public Object data;

        /**
         * Specifies a cascading menu definition with just a text label.
         *
         * @param inName      the localized string label component
         * @param inMenuItems the menu items within the cascade
         * @author alex, mlh
         */
        public CascadingMenuItemDefinition(String inName,
                                           MenuItemDefinition[] inMenuItems)
        {
            this(inName, inMenuItems, null);
        }

        public CascadingMenuItemDefinition(String inName,
                                           MenuItemDefinition[] inMenuItems,
                                           Object d)
        {
            super(inName);

            menuItems = inMenuItems;
            data = d;
        }

        /**
         * Specifies a cascading menu definition with both a text
         * and image label.
         *
         * @param inName      the localized string label component
         * @param inImage     the image label component
         * @param inMenuItems the menu items within the cascade
         * @author alex, mlh
         */
        public CascadingMenuItemDefinition(String inName,
                                           Image inImage,
                                           MenuItemDefinition[] inMenuItems)
        {
            super(inName, inImage);

            menuItems = inMenuItems;
        }

        public void setMenuItems(MenuItemDefinition[] inMenuItems)
        {
            menuItems = inMenuItems;
        }

        /**
         * Answers whether this instance represents a single menu item
         * or a cascading menu.
         * <p>
         * Indicates that this instance is a cascading menu definition.
         *
         * @return      false
         *
         * @author alex, mlh
         */
        @Override
        public boolean isSingle()
        {
            return false;
        }
    }

    /**
     * Recursive support method for navigating the tree of
     * menu item definitions to construct actual menus and menu items.
     * <p>
     * Each menu item created is associated with its
     * <code>ListenerIdentifier</code>.
     */
    protected static void createMenuStep(Shell shell,
                                         Menu parent,
                                         MenuItemDefinition defn,
                                         Boolean availability,
                                         Listener selectionListener,
                                         final ListenerIdentifierMap lIDMap)
    {
        if (defn.isSingle()) {
            SimpleMenuItemDefinition itemDef = (SimpleMenuItemDefinition) defn;
            renderShortcut(itemDef);

            final MenuItem item = addMenuItem(parent,
                                              itemDef.name,
                                              itemDef.image,
                                              itemDef.style,
                                              itemDef.accelerator,
                                              itemDef.enabledInitially,
                                              itemDef.selectedInitially);

            // pull the lid out to a local so the whole SimpleMenuItemDefinition
            // doesn't get stored in the dispose listener
            final ListenerIdentifier lid = itemDef.listenerID;

            if (lid != null) {   // avoid separator
                // Associate LID with menu item so that it can be used to find
                // the associated semantic action by the selectionListener.
                item.setData(lid);

                item.addListener(SWT.Selection, selectionListener);
                lIDMap.addWidget(lid, item, availability);

                // If this menu item gets disposed, as for example from a
                // dynamic menu, we don't want the LID still pointing at it
                item.addDisposeListener(new DisposeListener() {
                    public void widgetDisposed(DisposeEvent e)
                    {
                        lIDMap.removeWidget(lid, item);
                    }
                });
            }
        }
        else {
            CascadingMenuItemDefinition itemDef =
                (CascadingMenuItemDefinition) defn;

            MenuItem item =
                addMenuItem(parent, itemDef.name, itemDef.image, SWT.CASCADE);

            Menu cascadeParent = new Menu(shell, SWT.DROP_DOWN);

            item.setMenu(cascadeParent);
            item.setData(itemDef.data);

            if (itemDef.menuItems != null) {
                for (MenuItemDefinition menuItem : itemDef.menuItems) {
                    createMenuStep(shell,
                                   cascadeParent,
                                   menuItem,
                                   availability,
                                   selectionListener,
                                   lIDMap);
                }
            }
        }
    }

    /**
     * Create a complete menu of the specified style from the given
     * definition, associating menu items with their corresponding
     * <code>ListenerIdentifier</code>.
     * <p>
     * <code>rootStyle</code> must be one of <code>SWT.BAR</code>,
     * <code>SWT.POP_UP</code>, or <code>SWT.DROP_DOWN</code> optionally OR'ed
     * with one of <code>LEFT_TO_RIGHT</code> or <code>RIGHT_TO_LEFT</code>,
     * and optionally OR'ed with <code>NO_RADIO_GROUP</code>.
     *
     * @param shell     the application window that is to contain the menu
     * @param rootStyle the style of the base menu (such as menu bar, pop up,
     *                  or drop down)
     * @param defn      the arrangement of the menu items within the menu
     * @param availability the availability of the created MenuItem's
     * @param selectionListener the selection Listener instance for each
     *                          created MenuItem instance; this Listener
     *                          should use the ListenerIdentifier stored
     *                          in the MenuItem "data" (by calling getData())
     * @return          the constructed SWT Menu object
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.Menu
     */
    public static Menu createMenu(Shell shell,
                                  int rootStyle,
                                  MenuItemDefinition[] defn,
                                  Boolean availability,
                                  Listener selectionListener,
                                  ListenerIdentifierMap lIDMap)
    {
        // TODO: check the rootStyle to ensure it's a SWT.BAR or SWT.POPUP???
        Menu root = new Menu(shell, rootStyle);

        for (MenuItemDefinition element : defn) {
            createMenuStep(shell, root, element, availability,
                           selectionListener, lIDMap);
        }

        return root;
    }

    /**
     * Add a new item to end of the given menu; used by
     * <code>createMenu</code>.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemName  the localized text label for the
     *                      <code>MenuItem</code>
     * @param menuItemImage the (optional) icon for the constructed
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     * @param accelerator   the accelerator's keycode (<code>SWT.NONE</code>
     *                      if not desired)
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       String menuItemName,
                                       Image menuItemImage,
                                       int menuItemStyle,
                                       int accelerator,
                                       boolean enabled,
                                       boolean selected)
    {
        MenuItem mItem = new MenuItem(parentMenu, menuItemStyle);

        if (menuItemName != null) {
            mItem.setText(menuItemName);
        }

        if (menuItemImage != null) {
            mItem.setImage(menuItemImage);
        }

        mItem.setAccelerator(accelerator);

        mItem.setEnabled(enabled);

        mItem.setSelection(selected);

        return mItem;
    }

    /**
     * Add a new unselected item without an accelerator to end of the
     * given menu; used by <code>createMenu</code>.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemName  the localized text label for the
     *                      <code>MenuItem</code>
     * @param menuItemImage the (optional) icon for the constructed
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     *                      if not desired)
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       String menuItemName,
                                       Image menuItemImage,
                                       int menuItemStyle)
    {
        // 0 (ie. SWT.NONE) accelerator means no accelerator. :)
        return addMenuItem(parentMenu, menuItemName, menuItemImage,
                           menuItemStyle, SWT.NONE, true, false);
    }

    /**
     * Add a new unselected item with text label only to end of the given menu.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemName  the localized text label for the
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     * @param accelerator   the accelerator's keycode (<code>SWT.NONE</code>
     *                      if not desired)
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       String menuItemName,
                                       int menuItemStyle,
                                       int accelerator)
    {
        return addMenuItem(parentMenu, menuItemName, null,
                           menuItemStyle, accelerator, true, false);
    }

    /**
     * Add a new unselected item with text label only and no accelerator to
     * the end of the given menu.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemName  the localized text label for the
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       String menuItemName,
                                       int menuItemStyle)
    {
        // 0 (ie. SWT.NONE) accelerator means no accelerator. :)
        return addMenuItem(parentMenu, menuItemName, null,
                           menuItemStyle, SWT.NONE, true, false);
    }

    /**
     * Add a new unselected item with image only to end of the given menu.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemImage the (optional) icon for the constructed
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     * @param accelerator   the accelerator's keycode (<code>SWT.NONE</code>
     *                      if not desired)
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       Image menuItemImage,
                                       int menuItemStyle,
                                       int accelerator)
    {
        return addMenuItem(parentMenu, null, menuItemImage,
                           menuItemStyle, accelerator, true, false);
    }

    /**
     * Add a new unselected item with image only and no accelerator to end of
     * the given menu.
     * <p>
     * It is expected that the given item name text is localized
     * (that is, translated).
     * <p>
     * Choices for SWT style:
     * <code>CHECK, CASCADE, PUSH, RADIO, SEPARATOR</code>
     *
     * @param parentMenu    the menu in which the new
     *                      <code>MenuItem</code> is to reside
     * @param menuItemImage the (optional) icon for the constructed
     *                      <code>MenuItem</code>
     * @param menuItemStyle the SWT style for the new <code>MenuItem</code>
     * @return              the constructed <code>MenuItem</code>
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem addMenuItem(Menu parentMenu,
                                       Image menuItemImage,
                                       int menuItemStyle)
    {
        // 0 (ie. SWT.NONE) accelerator means no accelerator. :)
        return addMenuItem(parentMenu, null, menuItemImage,
                           menuItemStyle, SWT.NONE, true, false);
    }

    /**
     * This function takes arrays of String text labels, styles, accelerators
     * to create a set of <code>MenuItem</code> instances.
     * <p>
     * Deprecated; thought it might be useful, but hasn't come up yet.
     * <p>
     * The sizes of all arrays must match.
     *
     * @param parentMenu     the menu in which the new
     *                       <code>MenuItems</code> are to reside
     * @param menuItemNames  the localized text labels for the
     *                       <code>MenuItem</code>s
     * @param menuItemStyles the SWT styles for the new <code>MenuItem</code>s
     * @param accelerator    the accelerator keycodes (<code>SWT.NONE</code>
     *                       if not desired)
     * @return               the constructed <code>MenuItem</code>s with
     *                       the specified properties
     * @author          alex/mlh
     * @see org.eclipse.swt.widgets.MenuItem
     */
    public static MenuItem[] addMenuItemSet(Menu parentMenu,
                                            String[] menuItemNames,
                                            int[] menuItemStyles,
                                            int[] accelerators)
    {
        int count = menuItemNames.length;
        MenuItem[] result = null;

        if ((menuItemStyles.length == count) && (accelerators.length == count))
        {
            result = new MenuItem[count];

            for (int i = 0; i < count; i++) {
                // ensure each element of menuItems is a string
                result[i] = addMenuItem(parentMenu,
                                        menuItemNames[i],
                                        menuItemStyles[i],
                                        accelerators[i]);
            }
        }

        return result;
    }

    /**
     * Ensure when relabeling a menu item that the shortcut (if it exists)
     * is displayed.
     * <p>
     * Shortcut strings are separated from the normal item label by a tab.
     *
     * @param item the menu to relabel
     * @param newLabel the new label for the menu item
     */
    public static void relabelItem(MenuItem item, String newLabel)
    {
        // If running windows, shortcut might already be rendered.
        int tabIdx = item.getText().indexOf('\t');
        if ((OSUtils.WINDOWS) && (tabIdx >= 0)) {
            String shortcut = new String(item.getText().substring(tabIdx));
            item.setText(newLabel + shortcut);
        }
        else {
            item.setText(newLabel);
        }
    }

    /**
     * Ensure that shortcut(s) (if assigned) are part of the labels
     * for the menu items in the given definition.
     * <p>
     * Shortcut strings are separated from the normal item label by a tab.
     *
     * @param itemDef the item definition whose label to add the shortcut to
     */
    public static void renderShortcut(SimpleMenuItemDefinition itemDef)
    {
        // Only needed on Windows when the item's label does not already
        // have a shortcut indicator.
        if ((OSUtils.WINDOWS)
            && (itemDef.name != null)
            && (itemDef.name.indexOf('\t') == -1))
        {
            // Construct the shortcut string
            String shortcut = "\t";

            if ((itemDef.accelerator & SWT.CTRL) > 0) {
                shortcut += L10N.get("MU.Ctrl+", "Ctrl+");
            }
            if ((itemDef.accelerator & SWT.ALT) > 0) {
                shortcut += L10N.get("MU.Alt+", "Alt+");
            }
            if ((itemDef.accelerator & SWT.SHIFT) > 0) {
                shortcut += L10N.get("MU.Shift+", "Shift+");
            }

            boolean done = false;
            int key = (itemDef.accelerator & SWT.KEY_MASK);
            for (int i = 0; i < keyCodes.length; i++) {
                if (key == keyCodes[i]) {
                    shortcut += keyLabels[i];
                    done = true;
                }
            }

            if (! done) {
                shortcut += Character.toUpperCase((char) key);
            }

            itemDef.name += shortcut;
        }
    }

    // Array of special shortcut keys to determine the index
    // into the shortcut key label array.
    public static final int[] keyCodes =
    {
        SWT.ARROW_DOWN,
        SWT.ARROW_UP,
        SWT.ARROW_LEFT,
        SWT.ARROW_RIGHT,
        SWT.BREAK,
        SWT.BS,
        SWT.CR,
        SWT.DEL,
        SWT.END,
        SWT.ESC,
        SWT.F1,
        SWT.F2,
        SWT.F3,
        SWT.F4,
        SWT.F5,
        SWT.F6,
        SWT.F7,
        SWT.F8,
        SWT.F9,
        SWT.F10,
        SWT.F11,
        SWT.F12,
        SWT.F13,
        SWT.F14,
        SWT.F15,
        SWT.HELP,
        SWT.HOME,
        SWT.INSERT,
        SWT.KEYPAD_0,
        SWT.KEYPAD_1,
        SWT.KEYPAD_2,
        SWT.KEYPAD_3,
        SWT.KEYPAD_4,
        SWT.KEYPAD_5,
        SWT.KEYPAD_6,
        SWT.KEYPAD_7,
        SWT.KEYPAD_8,
        SWT.KEYPAD_9,
        SWT.KEYPAD_ADD,
        SWT.KEYPAD_CR,
        SWT.KEYPAD_DECIMAL,
        SWT.KEYPAD_DIVIDE,
        SWT.KEYPAD_EQUAL,
        SWT.KEYPAD_MULTIPLY,
        SWT.KEYPAD_SUBTRACT,
        SWT.PAGE_DOWN,
        SWT.PAGE_UP,
        SWT.PAUSE,
        SWT.PRINT_SCREEN,
        ' ',
        SWT.TAB
    };

    // Array of shortcut key labels.
    public static final String[] keyLabels =
    {
        L10N.get("MU.Down", "Down"),
        L10N.get("MU.Up", "Up"),
        L10N.get("MU.Left", "Left"),
        L10N.get("MU.Right", "Right"),
        L10N.get("MU.Break", "Break"),
        L10N.get("MU.Backspace", "Backspace"),
        L10N.get("MU.Enter", "Enter"),
        L10N.get("MU.Delete", "Delete"),
        L10N.get("MU.End", "End"),
        L10N.get("MU.Esc", "Esc"),
        L10N.get("MU.F1", "F1"),
        L10N.get("MU.F2", "F2"),
        L10N.get("MU.F3", "F3"),
        L10N.get("MU.F4", "F4"),
        L10N.get("MU.F5", "F5"),
        L10N.get("MU.F6", "F6"),
        L10N.get("MU.F7", "F7"),
        L10N.get("MU.F8", "F8"),
        L10N.get("MU.F9", "F9"),
        L10N.get("MU.F10", "F10"),
        L10N.get("MU.F11", "F11"),
        L10N.get("MU.F12", "F12"),
        L10N.get("MU.F13", "F13"),
        L10N.get("MU.F14", "F14"),
        L10N.get("MU.F15", "F15"),
        L10N.get("MU.Help", "Help"),
        L10N.get("MU.Home", "Home"),
        L10N.get("MU.Insert", "Insert"),
        L10N.get("MU.Keypad0", "Keypad0"),
        L10N.get("MU.Keypad1", "Keypad1"),
        L10N.get("MU.Keypad2", "Keypad2"),
        L10N.get("MU.Keypad3", "Keypad3"),
        L10N.get("MU.Keypad4", "Keypad4"),
        L10N.get("MU.Keypad5", "Keypad5"),
        L10N.get("MU.Keypad6", "Keypad6"),
        L10N.get("MU.Keypad7", "Keypad7"),
        L10N.get("MU.Keypad8", "Keypad8"),
        L10N.get("MU.Keypad9", "Keypad9"),
        L10N.get("MU.Keypad+", "Keypad+"),
        L10N.get("MU.KeypadEnter", "KeypadEnter"),
        L10N.get("MU.KeypadPeriod", "KeypadPeriod"),
        L10N.get("MU.Keypad/", "Keypad/"),
        L10N.get("MU.Keypad=", "Keypad="),
        L10N.get("MU.Keypad*", "Keypad*"),
        L10N.get("MU.Keypad-", "Keypad-"),
        L10N.get("MU.PageDown", "PageDown"),
        L10N.get("MU.PageUp", "PageUp"),
        L10N.get("MU.Pause", "Pause"),
        L10N.get("MU.PrintScreen", "PrintScreen"),
        L10N.get("MU.Space", "Space"),
        L10N.get("MU.Tab", "Tab")
    };
}
