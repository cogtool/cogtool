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

package edu.cmu.cs.hcii.cogtool.model;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.EnumeratedInt;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class WidgetType extends EnumeratedInt
{
    // Name will be one of the following :
    // For each skin, this group must have 9-part images:
    public static final String BUTTON_KEY = "button";
    public static final String LINK_KEY = "link";
    public static final String CHECKBOX_KEY = "checkbox";
    public static final String GRAFFITI_KEY = "graffiti";
    public static final String LISTBOXITEM_KEY = "listboxitem";
    public static final String CONTEXT_MENU_KEY = "contextmenu";
    public static final String MENU_KEY = "menu";
    public static final String MENUITEM_KEY = "menuitem";
    public static final String PULLDOWNITEM_KEY = "pulldownitem";
    public static final String TEXTBOX_KEY = "textbox";
    // This group must have 9-part images AND zoomable images:
    public static final String PULLDOWNLIST_KEY = "pulldownlist";
    public static final String SUBMENU_KEY = "submenu";
    // This group needs only a zoomable image
    public static final String RADIO_KEY = "radio";
    // Text and Noninteractive do not need images
    public static final String TEXT_KEY = "text";
    public static final String NONINTERACTIVE_KEY = "noninteractive";
    // Note that if the appropriate images are not present, the program WILL
    // crash if a graphical skin is used.

    public static final int NONINTERACTIVE_PERSISTENCE = 0;
    public static final int BUTTON_PERSISTENCE = 1;
    public static final int MENU_PERSISTENCE = 2;
    public static final int SUBMENU_PERSISTENCE = 3;
    public static final int MENUITEM_PERSISTENCE = 4;
    public static final int TEXTBOX_PERSISTENCE = 5;
    public static final int PULLDOWNLIST_PERSISTENCE = 6;
    public static final int PULLDOWNITEM_PERSISTENCE = 7;
    public static final int LISTBOXITEM_PERSISTENCE = 8;
    public static final int RADIO_PERSISTENCE = 9;
    public static final int CHECK_PERSISTENCE = 10;
    public static final int GRAFFITI_PERSISTENCE = 11;
    public static final int TEXT_PERSISTENCE = 12;
    // Text is the subset of a TextBox that just supports text selection
    public static final int LINK_PERSISTENCE = 13;
    public static final int CONTEXT_MENU_PERSISTENCE = 14;

    public static final int edu_cmu_cs_hcii_cogtool_model_WidgetType_version = 0;

    private static ObjectSaver.IDataSaver<WidgetType> SAVER =
        new ObjectSaver.ADataSaver<WidgetType>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_WidgetType_version;
            }

            @Override
            public boolean isEnum()
            {
                return true;
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(WidgetType.class.getName(), SAVER);
    }

    private static ObjectLoader.IEnumLoader LOADER =
        new ObjectLoader.AEnumLoader() {
            @Override
            public Object createEnum(String persistentValue)
            {
                switch (Integer.parseInt(persistentValue)) {
                    case NONINTERACTIVE_PERSISTENCE:
                        return WidgetType.Noninteractive;
                    case BUTTON_PERSISTENCE:
                        return WidgetType.Button;
                    case LINK_PERSISTENCE:
                        return WidgetType.Link;
                    case CONTEXT_MENU_PERSISTENCE:
                        return WidgetType.ContextMenu;
                    case MENU_PERSISTENCE:
                        return WidgetType.Menu;
                    case SUBMENU_PERSISTENCE:
                        return WidgetType.Submenu;
                    case MENUITEM_PERSISTENCE:
                        return WidgetType.MenuItem;
                    case TEXTBOX_PERSISTENCE:
                        return WidgetType.TextBox;
                    case PULLDOWNLIST_PERSISTENCE:
                        return WidgetType.PullDownList;
                    case PULLDOWNITEM_PERSISTENCE:
                        return WidgetType.PullDownItem;
                    case LISTBOXITEM_PERSISTENCE:
                        return WidgetType.ListBoxItem;
                    case RADIO_PERSISTENCE:
                        return WidgetType.Radio;
                    case CHECK_PERSISTENCE:
                        return WidgetType.Check;
                    case GRAFFITI_PERSISTENCE:
                        return WidgetType.Graffiti;
                    case TEXT_PERSISTENCE:
                        return WidgetType.Text;
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerEnumLoader(WidgetType.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_WidgetType_version,
                                        LOADER);
    }

    protected String keyname;
    protected boolean canActAsRemoteLabel;

    /**
     * The set of device types a widget of this type will work on
     * (see the persistent values in DeviceType.java).
     * TODO: Currently doesn't include DeviceType.DISPLAY_PERSISTENCE --
     *       when we do, we need to think about Display + Keyboard|Voice
     *       for most of these widget types!
     */
    protected int worksOnDeviceTypes;

    // Various widget type definitions
    public static final WidgetType Button =
        new WidgetType(L10N.get("CT.WidgetButton", "Button"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE
                           | DeviceType.PHYSICAL_PERSISTENCE,
                       BUTTON_PERSISTENCE,
                       BUTTON_KEY,
                       false);

    public static final WidgetType Link =
        new WidgetType(L10N.get("CT.WidgetLink", "Link"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       LINK_PERSISTENCE,
                       LINK_KEY,
                       true);

    public static final WidgetType ContextMenu =
        new WidgetType(L10N.get("CT.WidgetContextMenu", "Context Menu"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       CONTEXT_MENU_PERSISTENCE,
                       CONTEXT_MENU_KEY,
                       false);

    public static final WidgetType Menu =
        new WidgetType(L10N.get("CT.WidgetMenu", "Menu Header"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       MENU_PERSISTENCE,
                       MENU_KEY,
                       false);

    public static final WidgetType Submenu =
        new WidgetType(L10N.get("CT.WidgetSubmenu", "Submenu"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       SUBMENU_PERSISTENCE,
                       SUBMENU_KEY,
                       false);

    public static final WidgetType MenuItem =
        new WidgetType(L10N.get("CT.WidgetItem", "Menu Item"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       MENUITEM_PERSISTENCE,
                       MENUITEM_KEY,
                       false);

    public static final WidgetType TextBox =
        new WidgetType(L10N.get("CT.WidgetTextBox", "Text Box"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       TEXTBOX_PERSISTENCE,
                       TEXTBOX_KEY,
                       false);

    public static final WidgetType Text =
        new WidgetType(L10N.get("CT.WidgetText", "Text"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       TEXT_PERSISTENCE,
                       TEXT_KEY,
                       true);

    public static final WidgetType PullDownList =
        new WidgetType(L10N.get("CT.PullDownList", "Pull-Down List Header"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       PULLDOWNLIST_PERSISTENCE,
                       PULLDOWNLIST_KEY,
                       false);

    public static final WidgetType PullDownItem =
        new WidgetType(L10N.get("CT.PullDownItem", "Pull-Down Item"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       PULLDOWNITEM_PERSISTENCE,
                       PULLDOWNITEM_KEY,
                       false);

    public static final WidgetType ListBoxItem =
        new WidgetType(L10N.get("CT.ListBoxItem", "List Box Item"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       LISTBOXITEM_PERSISTENCE,
                       LISTBOXITEM_KEY,
                       false);

    public static final WidgetType Radio =
        new WidgetType(L10N.get("CT.WidgetRadio", "Radio Button"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       RADIO_PERSISTENCE,
                       RADIO_KEY,
                       false);

    public static final WidgetType Check =
        new WidgetType(L10N.get("CT.WidgetCheck", "Checkbox"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE,
                       CHECK_PERSISTENCE,
                       CHECKBOX_KEY,
                       true);

    public static final WidgetType Graffiti =
        new WidgetType(L10N.get("CT.Graffiti", "Graffiti\u00AE"),
                       DeviceType.TOUCHSCREEN_PERSISTENCE,
                       GRAFFITI_PERSISTENCE,
                       GRAFFITI_KEY,
                       false);
        // \u00AE is for a registered trademark

    public static final WidgetType Noninteractive =
        new WidgetType(L10N.get("CT.WidgetNonInteractive", "Non-interactive"),
                       DeviceType.MOUSE_PERSISTENCE
                           | DeviceType.TOUCHSCREEN_PERSISTENCE
                           | DeviceType.KEYBOARD_PERSISTENCE
                           | DeviceType.VOICE_PERSISTENCE
                           | DeviceType.PHYSICAL_PERSISTENCE,
                       NONINTERACTIVE_PERSISTENCE, NONINTERACTIVE_KEY,
                       true);

    // Non-interactive widgets exist outside of the ACT-R system
    // They can used for labeling purposes, or something to that effect
    // Basically, as look-at targets

    public boolean worksOn(int deviceTypes)
    {
        return DeviceType.intersects(deviceTypes, worksOnDeviceTypes);
    }

    /**
     * Indicate that this widget type requires the existence of one of
     * the returned device types.
     */
    public int requiresOneOf()
    {
        return worksOnDeviceTypes;
    }

    /**
     * Indicate whether or not this widget type can act as a remote label
     */
    public boolean canBeARemoteLabel()
    {
        return canActAsRemoteLabel;
    }

    /**
     * Display ordering
     */
    public static final WidgetType[] DISPLAY =
        { Button, Link, Check, Radio, TextBox, Text, Menu, Submenu, MenuItem,
          ContextMenu, PullDownList, PullDownItem, ListBoxItem, Graffiti,
          Noninteractive };

    // Constructor
    protected WidgetType(String lbl,
                         int deviceTypes,
                         int persistentValue,
                         String widgetname,
                         boolean actAsRemoteLabel)
    {
        super(lbl, persistentValue);
        worksOnDeviceTypes = deviceTypes;
        keyname = widgetname;
        canActAsRemoteLabel = actAsRemoteLabel;
    }

    /**
     * Return boolean value if the transitions on the old type are compatible
     * on the new type.
     *
     * @param oldType
     * @param newType
     * @return
     */
    // TODO I believe this is dead code, left over from when in the
    //      dim past we could change the type of an existing widget.
    public boolean compatibleTransitions(WidgetType oldType,
                                         WidgetType newType)
    {
        if (oldType == newType) {
            return true;
        }

        if ((oldType == Button) ||
            (oldType == Link) ||
            (oldType == ContextMenu) ||
            (oldType == Menu) ||
            (oldType == MenuItem) ||
            (oldType == PullDownList) ||
            (oldType == PullDownItem) ||
            (oldType == ListBoxItem) ||
            (oldType == Radio) ||
            (oldType == Check) ||
            (oldType == TextBox) ||
            (oldType == Text))
        {
            if ((newType == Button) ||
                (newType == Link) ||
                (newType == ContextMenu) ||
                (newType == Menu) ||
                (newType == MenuItem) ||
                (newType == PullDownList) ||
                (newType == PullDownItem) ||
                (newType == ListBoxItem) ||
                (newType == Radio) ||
                (newType == Check) ||
                (newType == TextBox) ||
                (newType == Text))
            {
                return true;
            }

            return false;
        }

        // The remaining types
        // Voice, Keyboard, Graffiti are all not compatible with any other type
        return false;
    }

    /**
     * Returns the keyname for this widget type, which is a unique string
     * defining that type.
     *
     * @return the keyname.
     */
    public String getKeyName()
    {
        return keyname;
    }

    /**
     * The set of all values to support their iteration in a specific order.
     */
    protected static final WidgetType[] ITERATOR_ORDERING =
        { Button, Link, Check, Radio, TextBox, Text, PullDownList, PullDownItem,
          ListBoxItem, ContextMenu, Menu, Submenu, MenuItem, Graffiti,
          Noninteractive };

    public static final List<WidgetType> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
