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
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolLID.ConverterFilesLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.ImportConverter;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.SEDemoLID;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.CascadingMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.RcvrImportException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;

public class MenuFactory
{
    public interface IWindowMenuData<T>
    {
        public void setView(View useView);
        public View getView();

        public MenuItemDefinition[] getWindowMenuLeadItems();

        public String getNexusLabel();
        public T getNexusData();
        public Class<T> getNexusType();

        public String getEntryLabel();

        public Class<? extends View> getViewType();
        public ListenerIdentifier getLID();

        public void setNexusLabel(String newLabel);
        public void setEntryLabel(String newLabel);

        public void setInitiallyEnabledItems();
    }

    public static class WindowListenerID extends ListenerIdentifier
    {
        protected View view;

        public WindowListenerID(View v)
        {
            view = v;
        }

        public View getView()
        {
            return view;
        }
    }

    public final static String ALIGN_LEFT_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/alignleft.gif";
    public final static String ALIGN_RIGHT_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/alignright.gif";
    public final static String ALIGN_BOTTOM_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/alignbottom.gif";
    public final static String ALIGN_TOP_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/aligntop.gif";
    public final static String ALIGN_CENTERS_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/aligncenters.gif";
    public final static String CENTER_HORIZONTALLY_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/centerhorizontally.gif";
    public final static String CENTER_VERTICALLY_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/centervertically.gif";
    public final static String SPACE_HORIZONTALLY_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/spacehorizontally.gif";
    public final static String SPACE_VERTICALLY_RESOURCE =
        "edu/cmu/cs/hcii/cogtool/resources/toolbar/spacevertically.gif";

    public static final Object RECENT_FLAG = new Object();
    public static final Object IMPORT_OTHER_FLAG = new Object();

    public static final String CUT_STRING = L10N.get("MI.Cut", "Cu&t");

    public static final String COPY_STRING = L10N.get("MI.Copy", "&Copy");
    public static final String COPY_RESULTS =
        L10N.get("MI.CopyResults", "Copy All Results");

    public static final String EDIT_STRING = L10N.get("MI.Edit", "&Edit");

    public static final String RENAME_STRING = L10N.get("MI.Rename", "Re&name");
    public static final String RELABEL_STRING =
        L10N.get("MI.Relabel", "Re&label");

    public static final String DELETE_STRING = L10N.get("MI.Delete", "&Delete");

    public static final String DUPLICATE_STRING =
        L10N.get("MI.Duplicate", "Dupl&icate");

    public static final String GROUP_STRING =
        L10N.get("MI.Group", "Group");

    public static final String UNGROUP_STRING =
        L10N.get("MI.Ungroup", "Ungroup");

    public static final String PROMOTE_STRING =
        L10N.get("MI.Promote", "Promote");

    public static final String DEMOTE_STRING =
        L10N.get("MI.Demote", "Demote");

    public static final String MOVE_EARLIER_STRING =
        L10N.get("MI.MoveEarlier", "Move Earlier");

    public static final String MOVE_LATER_STRING =
        L10N.get("MI.MoveLater", "Move Later");

    // Support for the "Window" menu for each view
    protected static List<Menu> windowMenus = new ArrayList<Menu>();

    // Platform-specific menu accelerator key
    protected static final int control = MenuUtil.platformControlKey();

    public static final SimpleMenuItemDefinition NEW_PROJECT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.NewProject",
        "&New Project"),
        CogToolLID.NewProject,
        control | 'n');
    public static final SimpleMenuItemDefinition OPEN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Open", "&Open Project..."),
                                     CogToolLID.OpenProject,
                                     control | 'o');
    public static final MenuItemDefinition OPEN_RECENT =
        new CascadingMenuItemDefinition(L10N.get("MI.PM.OpenRecent",
        "Open R&ecent"),
        null,
        RECENT_FLAG);
    public static final SimpleMenuItemDefinition CLOSE =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Close", "&Close Window"),
                                     CogToolLID.CloseWindow,
                                     control | 'w');
    public static final SimpleMenuItemDefinition CLOSE_PROJECT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.CloseProject",
        "Close P&roject"),
        CogToolLID.CloseProject);
    public static final SimpleMenuItemDefinition SAVE =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Save", "&Save Project"),
                                     CogToolLID.SaveProject,
                                     control | 's');
    public static final SimpleMenuItemDefinition SAVE_AS =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.SaveAs",
        "Save Project &as..."),
        CogToolLID.SaveProjectAs,
        control | SWT.SHIFT | 's');

    public static final SimpleMenuItemDefinition IMPORT_XML =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Import",
                                              "Import Project from CogTool XML..."),
                                     CogToolLID.ImportXML);

    public static final MenuItemDefinition IMPORT_OTHER =
        new CascadingMenuItemDefinition(L10N.get("MI.PM.Import",
        "&Import Other"),
        null,
        IMPORT_OTHER_FLAG);
    
    public static final SimpleMenuItemDefinition IMPORT_HTML =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ImportHTML",
        "Import Design from HTML..."),
        CogToolLID.ImportWebCrawl);
    public static final SimpleMenuItemDefinition CAPTURE =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Capture",
        "Capture Behavior..."),
        CogToolLID.CaptureBehavior);

    public static final SimpleMenuItemDefinition EXPORT_RESULTS_TO_CSV =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportResultsToCSV",
        "Export All Results to CSV"),
        CogToolLID.ExportResultsToCSV);
    public static final SimpleMenuItemDefinition EXPORT_DESIGN_TO_HTML =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportDesignToHTML",
        "Export Design to &HTML"),
        CogToolLID.ExportDesignToHTML);
    public static final SimpleMenuItemDefinition EXPORT_TO_XML =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportToXML",
        "Export to &XML"),
        CogToolLID.ExportToXML);
    public static final SimpleMenuItemDefinition EXPORT_SCRIPT_TO_CSV =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportScriptToCSV",
        "Export Script to &CSV"),
        CogToolLID.ExportScriptToCSV);
    public static final SimpleMenuItemDefinition EXPORT_TO_HCIPA =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportToHCIPA",
        "Export to HCI&PA..."),
        ProjectLID.ExportToHCIPA);
    public static final SimpleMenuItemDefinition EXPORT_DICTIONARY =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ExportDictionary",
        "Export Dictionary to CSV"),
        ProjectLID.ExportDictToCSV);
    public static final SimpleMenuItemDefinition IMPORT_DICTIONARY =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ImportDictionary",
        "Import Dictionary from CSV"),
        ProjectLID.ImportDict);

    // TODO: Implement printing functionality
    public static final SimpleMenuItemDefinition PRINT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.Print", "&Print..."),
                                     CogToolLID.PrintProject,
                                     control | 'p');
    public static final SimpleMenuItemDefinition PRINT_PREVIEW =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.PrintPreview",
        "Print Pre&view..."),
        CogToolLID.PrintPreviewProject);
    public static final SimpleMenuItemDefinition PAGE_SETUP =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.PageSetup",
        "Page Set&up..."),
        CogToolLID.PageSetupProject,
        control | SWT.SHIFT | 'p');
    public static final SimpleMenuItemDefinition PROPERTIES =
        new SimpleMenuItemDefinition(L10N.get("MI.Properties",
        "Propert&ies"),
        CogToolLID.Properties);

    // On Mac OS X the preferences menu item is created in MacSupport, not here
    public static final SimpleMenuItemDefinition PREFERENCES =
        (OSUtils.MACOSX
                ? null
                : new SimpleMenuItemDefinition(L10N.get("MI.PM.Preferences",
                "Pre&ferences"),
                CogToolLID.Preferences));

    // On Mac OS X the quit menu item is created in MacSupport, not here
    public static final SimpleMenuItemDefinition EXIT =
        (OSUtils.MACOSX
                ? null
                : new SimpleMenuItemDefinition(L10N.get("MI.PM.Exit", "E&xit"),
                                               CogToolLID.ExitApplication,
                                               control | 'q'));

    // New item creation stuff

    public static final SimpleMenuItemDefinition NEW_WIDGET =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NewWidget",
        "New &Widget..."),
        CogToolLID.NewWidgetJustWarn);

    public static final SimpleMenuItemDefinition NEW_FRAME =
        new SimpleMenuItemDefinition(L10N.get("MI.DE.NewFrame",
        "New Fra&me"),
        CogToolLID.NewFrame,
        control | SWT.SHIFT | 'f');

    public static final SimpleMenuItemDefinition NEW_DESIGN =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.NewDesign",
        "New &Design..."),
        CogToolLID.NewDesign,
        control | SWT.SHIFT | 'd');

    public static final SimpleMenuItemDefinition ADD_DESIGN_DEVICES =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.AddDesignDevices",
        "Add De&vices..."),
        CogToolLID.AddDesignDevices);

    public static final SimpleMenuItemDefinition NEW_TASK =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.NewTask",
        "New &Task"),
        CogToolLID.NewTask,
        control | SWT.SHIFT | 't');

    public static final SimpleMenuItemDefinition NEW_TASK_GROUP =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.NewTaskGroup",
        "New Task &Group"),
        CogToolLID.NewTaskGroup,
        control | SWT.SHIFT | 'g');

    public static final SimpleMenuItemDefinition IMPORT_BACKGROUND_IMAGES =
        new SimpleMenuItemDefinition(L10N.get("MI.DE.ImportBackgroundImages",
        "Import Images as New Frames..."),
        CogToolLID.ImportImageDirectory);

    // Frame Editor background stuff

    public static final SimpleMenuItemDefinition SET_BACKGROUND_IMAGE =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SetBackgroundImage",
        "Set Frame Back&ground Image..."),
        CogToolLID.SetBackgroundImage,
        control | 'b');

    public static final SimpleMenuItemDefinition REMOVE_BACKGROUND_IMAGE =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.RemoveBackgroundImage",
        "Remove Frame Background Image"),
        CogToolLID.RemoveBackgroundImage,
        control | SWT.SHIFT | 'b');

    public static final SimpleMenuItemDefinition SET_WIDGET_COLOR =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SetWidgetColor",
        "Set Frame's Widget Layer Color..."),
        CogToolLID.SetWidgetColor,
        control | SWT.SHIFT | 'c');

    // Frame Editor Widget stuff
    public static final SimpleMenuItemDefinition SET_WIDGET_IMAGE =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SetWidgetImage",
        "Set Widget Image..."),
        FrameEditorLID.SetImageProperty);

    public static final SimpleMenuItemDefinition REMOVE_WIDGET_IMAGE =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.RemoveWidgetImage",
        "Remove Widget Image"),
        FrameEditorLID.RemoveImageProperty);

    public static final SimpleMenuItemDefinition CAPTURE_WIDGET_IMAGE =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.CaptureWidgetImage",
        "Capture Widget Image"),
        FrameEditorLID.CaptureImageProperty);

    public static final String RENDER_WIDGET_SKIN_LABEL =
        L10N.get("MI.FE.RenderWidgetSkin", "Render Widget Skin");
    public static final SimpleMenuItemDefinition RENDER_WIDGET_SKIN =
        new SimpleMenuItemDefinition(RENDER_WIDGET_SKIN_LABEL,
                                     FrameEditorLID.ToggleRenderSkin,
                                     SWT.NONE,
                                     SWT.CHECK);

    static public final SimpleMenuItemDefinition RENDER_ALL =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.RenderAll",
        "Render All Design's Widgets"),
        CogToolLID.RenderAll);

    static public final SimpleMenuItemDefinition UN_RENDER =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.UnRender",
        "Un-Render All Design's Widgets"),
        CogToolLID.UnRender);

    // FE/DE layering stuff

    public static final SimpleMenuItemDefinition BRING_TO_FRONT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.BringToFront",
        "Bring to Front"),
        CogToolLID.BringToFront,
        control | SWT.SHIFT | ']');

    public static final SimpleMenuItemDefinition BRING_FORWARD =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.BringForward",
        "Bring Forward"),
        CogToolLID.BringForward,
        control | ']');

    public static final SimpleMenuItemDefinition SEND_BACKWARD =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SendBackward",
        "Send Backward"),
        CogToolLID.SendBackward,
        control | '[');

    public static final SimpleMenuItemDefinition SEND_TO_BACK =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SendToBack",
        "Send to Back"),
        CogToolLID.SendToBack,
        control | SWT.SHIFT | '[');

    static public final MenuItemDefinition[] LAYERING_ITEMS =
        new MenuItemDefinition[] {
        BRING_TO_FRONT,
        BRING_FORWARD,
        SEND_BACKWARD,
        SEND_TO_BACK
    };

    static public final MenuItemDefinition LAYERING_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.FE.LayeringCascade",
        "Widget Layering"),
        LAYERING_ITEMS);

    // FE/DE alignment stuff

    public static final SimpleMenuItemDefinition ALIGN_TOP =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignTop",
        "Align Top"),
        GraphicsUtil.getImageFromResource(ALIGN_TOP_RESOURCE),
        CogToolLID.AlignTop);

    public static final SimpleMenuItemDefinition ALIGN_BOTTOM =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignBottom",
        "Align Bottom"),
        GraphicsUtil.getImageFromResource(ALIGN_BOTTOM_RESOURCE),
        CogToolLID.AlignBottom);

    public static final SimpleMenuItemDefinition ALIGN_LEFT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignLeft",
        "Align Left"),
        GraphicsUtil.getImageFromResource(ALIGN_LEFT_RESOURCE),
        CogToolLID.AlignLeft);

    public static final SimpleMenuItemDefinition ALIGN_RIGHT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignRight",
        "Align Right"),
        GraphicsUtil.getImageFromResource(ALIGN_RIGHT_RESOURCE),
        CogToolLID.AlignRight);

    public static final SimpleMenuItemDefinition ALIGN_CENTERS =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignCenter",
        "Align Centers"),
        GraphicsUtil.getImageFromResource(ALIGN_CENTERS_RESOURCE),
        CogToolLID.AlignCenter);

    public static final SimpleMenuItemDefinition ALIGN_HORIZ_CENTER =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignHorizCenter",
        "Align Horizontal Center"),
        GraphicsUtil.getImageFromResource(CENTER_HORIZONTALLY_RESOURCE),
        CogToolLID.AlignHorizCenter);

    public static final SimpleMenuItemDefinition ALIGN_VERT_CENTER =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.AlignVertCenter",
        "Align Vertical Center"),
        GraphicsUtil.getImageFromResource(CENTER_VERTICALLY_RESOURCE),
        CogToolLID.AlignVertCenter);

    // FE/DE spacing stuff

    public static final SimpleMenuItemDefinition SPACE_VERTICALLY =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SpaceVertically",
        "Space Vertically"),
        GraphicsUtil.getImageFromResource(SPACE_VERTICALLY_RESOURCE),
        CogToolLID.SpaceVertically);

    public static final SimpleMenuItemDefinition SPACE_HORIZONTALLY =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SpaceHorizontally",
        "Space Horizontally"),
        GraphicsUtil.getImageFromResource(SPACE_HORIZONTALLY_RESOURCE),
        CogToolLID.SpaceHorizontally);

    static public final MenuItemDefinition[] ALIGNMENT_ITEMS =
        new MenuItemDefinition[] {
        ALIGN_TOP,
        ALIGN_VERT_CENTER,
        ALIGN_BOTTOM,
        MenuUtil.SEPARATOR,
        ALIGN_LEFT,
        ALIGN_HORIZ_CENTER,
        ALIGN_RIGHT,
        MenuUtil.SEPARATOR,
        ALIGN_CENTERS,
        SPACE_VERTICALLY,
        SPACE_HORIZONTALLY
    };

    public static final MenuItemDefinition WIDGET_ALIGNMENT_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.FE.WidgetAlignmentCascade",
        "Widget Alignment"),
        ALIGNMENT_ITEMS);

    public static final MenuItemDefinition FRAME_ALIGNMENT_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.FE.FrameAlignmentCascade",
        "Frame Alignment"),
        ALIGNMENT_ITEMS);

    // Nudging
    public static final SimpleMenuItemDefinition NUDGE_UP =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NudgeUp", "Nudge &Up"),
                                     CogToolLID.NudgeUp,
                                     control | SWT.ARROW_UP);

    public static final SimpleMenuItemDefinition NUDGE_DOWN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NudgeDown",
        "Nudge &Down"),
        CogToolLID.NudgeDown,
        control | SWT.ARROW_DOWN);

    public static final SimpleMenuItemDefinition NUDGE_LEFT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NudgeLeft",
        "Nudge &Left"),
        CogToolLID.NudgeLeft,
        control | SWT.ARROW_LEFT);

    public static final SimpleMenuItemDefinition NUDGE_RIGHT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NudgeRight",
        "Nudge &Right"),
        CogToolLID.NudgeRight,
        control | SWT.ARROW_RIGHT);

    static public final MenuItemDefinition[] NUDGE_ITEMS =
        new MenuItemDefinition[] {
        NUDGE_UP,
        NUDGE_DOWN,
        NUDGE_LEFT,
        NUDGE_RIGHT
    };

    static public final MenuItemDefinition NUDGE_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.NudgeCascade",
        "Nudge"),
        NUDGE_ITEMS);

    // Global clipboard stuff

    public static final SimpleMenuItemDefinition CUT =
        new SimpleMenuItemDefinition(CUT_STRING,
                                     CogToolLID.Cut,
                                     control | 'x');

    public static final SimpleMenuItemDefinition COPY =
        new SimpleMenuItemDefinition(COPY_STRING,
                                     CogToolLID.Copy,
                                     control | 'c');

    public static final SimpleMenuItemDefinition COPY_PROJECT_RESULTS =
        new SimpleMenuItemDefinition(COPY_RESULTS,
                                     ProjectLID.CopyResultsToClipboard);

    public static final SimpleMenuItemDefinition PASTE =
        new SimpleMenuItemDefinition(L10N.get("MI.Paste", "&Paste"),
                                     CogToolLID.Paste,
                                     control | 'v');

    public static final SimpleMenuItemDefinition SET_FRAME_TPL =
        new SimpleMenuItemDefinition(L10N.get("MI.SetFrameTemplate",
        "Set Frame &Template"),
        CogToolLID.SetFrameTemplate);

    public static final SimpleMenuItemDefinition CLEAR_FRAME_TPL =
        new SimpleMenuItemDefinition(L10N.get("MI.ClearFrameTemplate",
        "Clear Frame Template"),
        CogToolLID.ClearFrameTemplate);

    public static final SimpleMenuItemDefinition SELECT_ALL =
        new SimpleMenuItemDefinition(L10N.get("MI.SelectAll", "Select &all"),
                                     CogToolLID.SelectAll,
                                     control | 'a');


    // Global editing stuff

    public static final SimpleMenuItemDefinition UNDO =
        new SimpleMenuItemDefinition(L10N.get("MI.Undo", "&Undo"),
                                     CogToolLID.Undo,
                                     control | 'z');

    public static final SimpleMenuItemDefinition REDO =
        new SimpleMenuItemDefinition(L10N.get("MI.Redo", "&Redo"),
                                     CogToolLID.Redo,
                                     control | 'y');

    public static final SimpleMenuItemDefinition EDIT =
        new SimpleMenuItemDefinition(EDIT_STRING,
                                     CogToolLID.Edit,
                                     control | 'e');

    public static final SimpleMenuItemDefinition RENAME =
        new SimpleMenuItemDefinition(RENAME_STRING,
                                     CogToolLID.Rename,
                                     control | 'r');

    public static final SimpleMenuItemDefinition RELABEL =
        new SimpleMenuItemDefinition(RELABEL_STRING,
                                     FrameEditorLID.Relabel,
                                     control | 'l');

    public static final SimpleMenuItemDefinition DELETE =
        new SimpleMenuItemDefinition(DELETE_STRING,
                                     CogToolLID.Delete,
                                     control | ((OSUtils.MACOSX) ? SWT.BS
                                                                 : SWT.DEL));

    public static final SimpleMenuItemDefinition DUPLICATE =
        new SimpleMenuItemDefinition(DUPLICATE_STRING,
                                     CogToolLID.Duplicate,
                                     control | 'd');

    public static final SimpleMenuItemDefinition GROUP =
        new SimpleMenuItemDefinition(GROUP_STRING,
                                     CogToolLID.Group,
                                     control | 'g');

    // Used in project editor as well as widget group stuff
    public static final SimpleMenuItemDefinition UNGROUP =
        new SimpleMenuItemDefinition(UNGROUP_STRING,
                                     CogToolLID.Ungroup,
                                     SWT.SHIFT | control | 'g');

    // Project editing stuff
    public static final SimpleMenuItemDefinition PROMOTE_TASK =
        new SimpleMenuItemDefinition(PROMOTE_STRING,
                                     ProjectLID.PromoteTask,
                                     SWT.SHIFT | SWT.TAB);
    public static final SimpleMenuItemDefinition DEMOTE_TASK =
        new SimpleMenuItemDefinition(DEMOTE_STRING,
                                     ProjectLID.DemoteTask,
                                     SWT.TAB);
    public static final SimpleMenuItemDefinition MOVE_EARLIER =
        new SimpleMenuItemDefinition(MOVE_EARLIER_STRING,
                                     ProjectLID.MoveTaskEarlier,
                                     SWT.SHIFT | SWT.ARROW_UP);
    public static final SimpleMenuItemDefinition MOVE_LATER =
        new SimpleMenuItemDefinition(MOVE_LATER_STRING,
                                     ProjectLID.MoveTaskLater,
                                     SWT.SHIFT | SWT.ARROW_DOWN);

    // Currently unused
    static public final MenuItemDefinition[] MOVE_TASK_ITEMS =
        new MenuItemDefinition[] {
        PROMOTE_TASK,
        DEMOTE_TASK,
        MOVE_EARLIER,
        MOVE_LATER
    };


    public static final MenuItemDefinition MOVE_TASK_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.PV.MoveTaskCascade",
        "Move Task(s)"),
        MOVE_TASK_ITEMS);

    // FE/DE zooming stuff

    public static final SimpleMenuItemDefinition ZOOM_IN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.ZoomIn", "Zoo&m In"),
                                     CogToolLID.ZoomIn,
                                     control | '+');

    public static final SimpleMenuItemDefinition ZOOM_OUT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.ZoomOut", "Zoom &Out"),
                                     CogToolLID.ZoomOut,
                                     control | '-');

    public static final SimpleMenuItemDefinition ZOOM_NORMAL =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.NormalZoom",
        "&Normal Zoom"),
        CogToolLID.ZoomNormal,
        control | '0');

    public static final SimpleMenuItemDefinition ZOOM_FIT =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.ZoomToFit",
        "Zoom to &Fit"),
        CogToolLID.ZoomToFit,
        control | '/');

    public static final SimpleMenuItemDefinition RECOMPUTE_SCRIPT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.RecomputeScript(s)",
        "Recompute Script(s)"),
        CogToolLID.RecomputeScript);


    public static final SimpleMenuItemDefinition EDIT_SCRIPT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.EditScript",
        "Edit Script"),
        ProjectLID.EditScript);

    // Skin changing stuff.

    public static final SimpleMenuItemDefinition WIRE_SKIN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SK.WireFrame",
        "Wire Frame"),
        CogToolLID.SkinWireFrame,
        SWT.NONE,
        SWT.RADIO);

    public static final SimpleMenuItemDefinition MACX_SKIN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SK.MacOSX",
        "MacOSX"),
        CogToolLID.SkinMacOSX,
        SWT.NONE,
        SWT.RADIO);

    public static final SimpleMenuItemDefinition WINXP_SKIN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SK.WinXPSkin",
        "Windows XP"),
        CogToolLID.SkinWinXP,
        SWT.NONE,
        SWT.RADIO);

    public static final SimpleMenuItemDefinition PALM_SKIN =
        new SimpleMenuItemDefinition(L10N.get("MI.FE.SK.PalmSkin",
        "Palm"),
        CogToolLID.SkinPalm,
        SWT.NONE,
        SWT.RADIO);

    static public final MenuItemDefinition[] SKIN_ITEMS =
        new MenuItemDefinition[] {
        WIRE_SKIN,
        MACX_SKIN,
        WINXP_SKIN/*,
            PALM_SKIN*/
    };

    public static final MenuItemDefinition SKIN_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.FE.SetSkin",
        "Set Design's Widget Skin"),
        SKIN_ITEMS);


    // Script editing modification operations

    public static final SimpleMenuItemDefinition RECOMPUTE =
        new SimpleMenuItemDefinition(L10N.get("MI.SE.Recompute",
        "&Recompute"),
        CogToolLID.RecomputeScript);

    public static final SimpleMenuItemDefinition REGENERATE =
        new SimpleMenuItemDefinition(L10N.get("MI.SE.RegenerateScript",
        "Re&generate Script(s)"),
        CogToolLID.RegenerateScript);

    public static final SimpleMenuItemDefinition SHOW_VISUALIZATION =
        new SimpleMenuItemDefinition(L10N.get("MI.SE.ShowVisualization",
        "Show &Visualization..."),
        SEDemoLID.ShowModelVisualization);

    public static final SimpleMenuItemDefinition CHANGE_THINK_DURATION =
        new SimpleMenuItemDefinition(L10N.get("MI.SE.ChangeThinkProperties",
        "Change &Think Properties"),
        CogToolLID.ChangeThinkProperties);

    public static final SimpleMenuItemDefinition CHANGE_WAIT_DURATION =
        new SimpleMenuItemDefinition(L10N.get("MI.SE.ChangeWaitProperties",
        "Change &Wait Properties"),
        CogToolLID.ChangeWaitProperties);


    // Default Algorithm operations
    // Project defaults for algo execution
    //  Execution Style
    public static final SimpleMenuItemDefinition DEFAULT_ALG_IN_BACKGROUND =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.BackgroundRun",
        "Execute in Background"),
        ProjectLID.SetProjExecBackground,
        SWT.NONE,
        SWT.RADIO,
        MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition DEFAULT_ALG_IN_FOREGROUND =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.ForegroundRun",
        "Execute in Foreground"),
        ProjectLID.SetProjExecForeground,
        SWT.NONE,
        SWT.RADIO,
        MenuUtil.ENABLED);


    static public final MenuItemDefinition[] DEFAULT_EXECUTE_ITEMS =
        new MenuItemDefinition[] {
        DEFAULT_ALG_IN_FOREGROUND,
        DEFAULT_ALG_IN_BACKGROUND
    };


    public static final MenuItemDefinition DEFAULT_EXECUTE_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.PV.Execute",
        "Default Algorithm Execution"),
        DEFAULT_EXECUTE_ITEMS);


    //  Project defaults for algo selection
    //  Execution Style
    public static final SimpleMenuItemDefinition DEFAULT_ALG_ACTR =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DefaultACTR",
        "ACT-R"), //BEJohn 25mar2011: this doesn't seem to name the menu item, ProjectView.java seems to name the menu - maybe if the default is set somehow?
        ProjectLID.SetProjDefaultAlgoACTR,
        SWT.NONE,
        SWT.RADIO,
        MenuUtil.ENABLED);

    public static final SimpleMenuItemDefinition DEFAULT_ALG_SNIFACT =
        new SimpleMenuItemDefinition(L10N.get("MI.PM.DefaultSNIFACT",
        "SNIF-ACT"), //BEJohn 25mar2011: this doesn't seem to name the menu item, ProjectView.java seems to name the menu - maybe if the default is set somehow?
        ProjectLID.SetProjDefaultAlgoSNIFACT,
        SWT.NONE,
        SWT.RADIO,
        MenuUtil.ENABLED);

    public static final MenuItemDefinition[] DEFAULT_ALGO_ITEMS =
        new MenuItemDefinition[] {
        DEFAULT_ALG_ACTR,
        DEFAULT_ALG_SNIFACT
    };


    public static final MenuItemDefinition DEFAULT_ALGO_CASCADE =
        new CascadingMenuItemDefinition(L10N.get("MI.PV.Execute",
        "Default Algorithm Type"),
        DEFAULT_ALGO_ITEMS);

    // Window menu, empty indicator

    protected static final ListenerIdentifier NO_WINDOWS_SENTINEL =
        new ListenerIdentifier();

    public static final SimpleMenuItemDefinition NO_WINDOWS =
        new SimpleMenuItemDefinition(L10N.get("MI.NONE", "NONE"),
                                     NO_WINDOWS_SENTINEL);
    // initially disabled!

    /**
     * Represents a root menu (eg. in a menu bar)
     */
    public static class MenuType implements Comparable<MenuType>
    {
        protected static int nextOrdering = 0;

        private final transient String label;
        private final transient int ordering;
        private final int persistenceValue;

        protected MenuType(String newLabel, int newPersistenceValue)
        {
            label = newLabel;
            persistenceValue = newPersistenceValue;
            ordering = nextOrdering++;
        }

        public static final MenuType FileMenu = new MenuType("File", 0);
        public static final MenuType EditMenu = new MenuType("Edit", 1);
        public static final MenuType FrameEditMenu =
            new MenuType("FrameModify", 2);
        public static final MenuType CreateMenu = new MenuType("Create", 3);
        public static final MenuType ProjectModifyMenu =
            new MenuType("ProjectModify", 4);
        public static final MenuType FrameModifyMenu =
            new MenuType("FrameModify", 5);
        public static final MenuType ZoomModifyMenu =
            new MenuType("ZoomModify", 6);
        public static final MenuType ScriptModifyMenu =
            new MenuType("ScriptModify", 7);
        public static final MenuType ScriptMenu = new MenuType("Script", 8);
        public static final MenuType WindowMenu = new MenuType("Window", 9);
        public static final MenuType HelpMenu = new MenuType("Help", 10);
        public static final MenuType DictionaryEditMenu =
            new MenuType("Edit", 11);
        public static final MenuType DesignModifyMenu =
            new MenuType("DesignModify", 12);

        @Override
        public String toString()
        {
            return label;
        }


        public int compareTo(MenuType other)
        {
            return ordering - other.ordering;
        }

        protected boolean valueEquals(MenuType other)
        {
            return (other != null) && (ordering == other.ordering);
        }

        @Override
        public boolean equals(Object other)
        {
            return (other != null) &&
            (other.getClass() == MenuType.class) &&
            valueEquals((MenuType) other);
        }

        public int getOrdering()
        {
            return ordering;
        }

        protected static final MenuType[] PERSISTENCE_ORDERING =
        { FileMenu, EditMenu, ProjectModifyMenu, FrameModifyMenu, ZoomModifyMenu,
            ScriptModifyMenu, CreateMenu, ScriptMenu, WindowMenu, HelpMenu,
            DictionaryEditMenu, DesignModifyMenu };

        public static final MenuType[] VALUES =
        { FileMenu, EditMenu, ProjectModifyMenu, FrameModifyMenu, ZoomModifyMenu,
            ScriptModifyMenu, CreateMenu, ScriptMenu, WindowMenu, HelpMenu,
            DictionaryEditMenu, DesignModifyMenu };

        private Object readResolve()
        {
            return PERSISTENCE_ORDERING[persistenceValue];
        }
    }

    // Mnemonics:                   Accelerators:
    //      a       Save as           shift-^s
    //      c       Close                   ^w
    //      e       Export            shift-^e
    //      i       Properties
    //      n       New                     ^n
    //      o       Open                    ^o
    //      p       Print                   ^p
    //      s       Save                    ^s
    //      u       Page Setup        shift-^p
    //      x       Exit                    ^q
    protected static MenuItemDefinition[] createFileMenuDefn(boolean research,
                                                             boolean hcipa)
    {
        List<MenuItemDefinition> result = new ArrayList<MenuItemDefinition>();

        result.add(NEW_PROJECT);
        result.add(OPEN);
        result.add(OPEN_RECENT);
        result.add(MenuUtil.SEPARATOR);
        result.add(CLOSE);
        result.add(CLOSE_PROJECT);
        result.add(SAVE);
        result.add(SAVE_AS);

        result.add(MenuUtil.SEPARATOR);
        if (research) {
            result.add(IMPORT_XML);
        }
        result.add(IMPORT_HTML);
        if (research) {
            result.add(IMPORT_DICTIONARY);
            String convDir = CogToolPref.CONVERTER_DIRECTORY.getString();
            if (convDir != null && !convDir.equals("")) {
                result.add(IMPORT_OTHER);
            }
        }

        result.add(MenuUtil.SEPARATOR);
        if (research) {
            result.add(EXPORT_TO_XML);
            result.add(EXPORT_DESIGN_TO_HTML);
        }
        result.add(EXPORT_SCRIPT_TO_CSV);
        result.add(EXPORT_RESULTS_TO_CSV);
        if (research) {
            result.add(EXPORT_DICTIONARY);
        }
        if (hcipa) {
            result.add(EXPORT_TO_HCIPA);
        }

        result.add(MenuUtil.SEPARATOR);
        result.add(PROPERTIES);

        if (! OSUtils.MACOSX) {
            result.add(MenuUtil.SEPARATOR);
            result.add(PREFERENCES);
            result.add(EXIT);
        }

        return result.toArray(new MenuItemDefinition[result.size()]);
    } // createFileMenuDefn

    // Edit menu items
    // Mnemonics:                   Accelerators:
    //      a       Select all              ^a
    //      c       Copy                    ^c
    //      d       Delete                  DEL
    //      e       Edit                    ^e
    //      f       Find                    ^f
    //      g       Find again              ^g
    //      i       Duplicate               ^d
    //      l       Deselect all            ESC
    //      n       Rename                  ^r
    //      p       Paste                   ^v
    //      r       Redo                    ^y
    //      t       Cut                     ^x
    //      u       Undo                    ^z
    protected static MenuItemDefinition[] createEditMenuDefn()
    {
        return new SimpleMenuItemDefinition[]
                                            { UNDO,
                REDO,
                MenuUtil.SEPARATOR,
                CUT,
                COPY,
                COPY_PROJECT_RESULTS,
                PASTE,
                DELETE,
                DUPLICATE,
                SELECT_ALL,
                MenuUtil.SEPARATOR,
                EDIT,
                RENAME,
                //              MenuUtil.SEPARATOR,
                //              new SimpleMenuItemDefinition(L10N.get("MI.Find", "&Find"),
                //                                           CogToolLID.Find,
                //                                           control | 'f'),
                //              new SimpleMenuItemDefinition(L10N.get("MI.FindAgain",
                //                                                    "Find a&gain"),
                //                                           CogToolLID.FindAgain,
                //                                           control | 'g')
                                            };
    } // createEditMenuDefn

    // View menu items for drawing editor views
    // Mnemonics:                   Accelerators:
    protected static MenuItemDefinition[] createFrameEditMenuDefn()
    {
        return new SimpleMenuItemDefinition[]
                                            { UNDO,
                REDO,
                MenuUtil.SEPARATOR,
                CUT,
                COPY,
                COPY_PROJECT_RESULTS,
                PASTE,
                DELETE,
                DUPLICATE,
                SELECT_ALL,
                MenuUtil.SEPARATOR,
                EDIT,
                RENAME,
                RELABEL
                                            };
    } // createFrameEditMenuDefn

    protected static MenuItemDefinition[] createDictionaryEditMenuDefn()
    {
        return new MenuItemDefinition[]
                                      {
                UNDO,
                REDO,
                MenuUtil.SEPARATOR,
                DELETE,
                SELECT_ALL
                                      };
    }

    // Create menu items
    // Mnemonics:                   Accelerators:
    //      t       New Task
    //      g       New Task Group...
    //      d       New Design
    //      f       New Frame
    //      w       New Widget
    //      t       New Transition
    protected static MenuItemDefinition[] createCreateMenuDefn(boolean isHCIPA)
    {
        return isHCIPA
        ? new SimpleMenuItemDefinition[]
                                       { NEW_TASK,
                MenuUtil.SEPARATOR,
                NEW_DESIGN,
                NEW_FRAME,
                NEW_WIDGET
                                       }
        : new SimpleMenuItemDefinition[]
                                       { NEW_TASK,
                                               NEW_TASK_GROUP,
                                               MenuUtil.SEPARATOR,
                                               NEW_DESIGN,
                                               NEW_FRAME,
                                               NEW_WIDGET
                                       };
    } // createCreateMenuDefn

    // Script menu items
    // Mnemonics:                   Accelerators:
    //             Demonstrate Task
    //             Show Timeline(s)
    //             Show Driving Replay
    //             Normal Mode
    //             Advanced Mode
    //             Insert Mental Delay
    //             Insert Visual Delay
    //             Insert System Delay
    protected static MenuItemDefinition[] createScriptMenuDefn()
    {
        return new SimpleMenuItemDefinition[]
                                            {  };
    } // createScriptMenuDefn

    // View menu items
    // Mnemonics:                   Accelerators:
    //             Show Min
    //             Show Max
    //             Show Mean
    //             Show Sum
    //             Show Standalone Time
    //             Show User Time
    //             Show Driving Time
    //             Show Avg Lane Deviation
    //             Show Max Lane Deviation
    //             Show Avg Reaction Time
    //             Show Max Reaction Time
    protected static MenuItemDefinition[] createProjectModifyMenuDefn(boolean research)
    {
        List<MenuItemDefinition> result = new ArrayList<MenuItemDefinition>();

        result.add(RECOMPUTE_SCRIPT);
        result.add(MenuUtil.SEPARATOR);
        result.add(ADD_DESIGN_DEVICES);
        result.add(MenuUtil.SEPARATOR);
        result.add(new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowSum",
        "Show &Sum"),
        CogToolLID.ShowSum,
        SWT.NONE,
        SWT.RADIO, false, true));
        result.add(new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMean",
        "Show &Mean"),
        CogToolLID.ShowMean,
        SWT.NONE,
        SWT.RADIO));
        result.add(new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMin",
        "Show Mi&nimum"),
        CogToolLID.ShowMin,
        SWT.NONE,
        SWT.RADIO));
        result.add(new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMax",
        "Show Ma&ximum"),
        CogToolLID.ShowMax,
        SWT.NONE,
        SWT.RADIO));

        // TODO clean up the detritus from this no longer used functionality
//        if (research) {
//            result.add(MenuUtil.SEPARATOR);
//            result.add(DEFAULT_ALGO_CASCADE);
//            result.add(DEFAULT_EXECUTE_CASCADE);
//        }
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowStandaloneTime",
        //                                                    "Show Standalone Time"),
        //                                           CogToolLID.ShowStandaloneTime,
        //                                           SWT.NONE,
        //                                           SWT.RADIO, false, true),
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowUserTime",
        //                                                    "Show User Time"),
        //                                           CogToolLID.ShowUserTime,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowDrivingTime",
        //                                                    "Show Driving Time"),
        //                                           CogToolLID.ShowDrivingTime,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        ////              MenuUtil.SEPARATOR,
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowAvgLane",
        //                                                    "Show Avg Lane Deviation"),
        //                                           CogToolLID.ShowAvgLaneDev,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMaxLane",
        //                                                    "Show Max Lane Deviation"),
        //                                           CogToolLID.ShowMaxLaneDev,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowAvgReaction",
        //                                                    "Show Avg Reaction Time"),
        //                                           CogToolLID.ShowAvgReaction,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        //              new SimpleMenuItemDefinition(L10N.get("MI.PM.ShowMaxReaction",
        //                                                    "Show Max Reaction Time"),
        //                                           CogToolLID.ShowMaxReaction,
        //                                           SWT.NONE,
        //                                           SWT.RADIO),
        result.add(MenuUtil.SEPARATOR);
        result.add(UNGROUP);
        result.add(MenuUtil.SEPARATOR);
        result.add(PROMOTE_TASK);
        result.add(DEMOTE_TASK);
        result.add(MOVE_EARLIER);
        result.add(MOVE_LATER);

        return result.toArray(new MenuItemDefinition[result.size()]);
    } // createProjectModifyMenuDefn

    // View menu items for drawing editor views
    // Mnemonics:                   Accelerators:
    //      d       Move Frame Down       ARROW_DOWN
    //      i       Import
    //      g       Set Background Image
    //      f       Zoom to Fit           ^/
    //      l       Move Frame Left       ARROW_LEFT
    //      m       Zoom In               ^+
    //      n       Normal zoom           ^=
    //      o       Zoom Out              ^-
    //      r       Move Frame Right      ARROW_RIGHT
    //      u       Move Frame Up         ARROW_UP
    protected static MenuItemDefinition[] createDesignModifyMenuDefn(boolean research)
    {
        List<MenuItemDefinition> result = new ArrayList<MenuItemDefinition>(16);
        result.add(IMPORT_BACKGROUND_IMAGES);
        result.add(SKIN_CASCADE);
        result.add(RENDER_ALL);
        result.add(UN_RENDER);
        result.add(ADD_DESIGN_DEVICES);
        result.add(MenuUtil.SEPARATOR);
        result.add(SET_BACKGROUND_IMAGE);
        result.add(REMOVE_BACKGROUND_IMAGE);
        result.add(SET_WIDGET_COLOR);
        result.add(CLEAR_FRAME_TPL);
        result.add(MenuUtil.SEPARATOR);
        result.add(NUDGE_CASCADE);
        result.add(FRAME_ALIGNMENT_CASCADE);
        result.add(MenuUtil.SEPARATOR);
        result.add(ZOOM_IN);
        result.add(ZOOM_OUT);
        result.add(ZOOM_NORMAL);
        result.add(ZOOM_FIT);
        return result.toArray(new MenuItemDefinition[result.size()]);
    } // createDesignModifyMenuDefn

    // View menu items for drawing editor views
    // Mnemonics:                   Accelerators:
    //      d       Move Widget Down      ARROW_DOWN
    //      i       Import
    //      g       Set Background Image
    //      f       Zoom to Fit           ^/
    //      l       Move Widget Left      ARROW_LEFT
    //      m       Zoom In               ^+
    //      n       Normal zoom           ^=
    //      o       Zoom Out              ^-
    //      r       Move Widget Right     ARROW_RIGHT
    //      u       Move Widget Up        ARROW_UP
    protected static MenuItemDefinition[] createFrameModifyMenuDefn(boolean research)
    {
        List<MenuItemDefinition> result = new ArrayList<MenuItemDefinition>(21);
        result.add(SKIN_CASCADE);
        result.add(RENDER_ALL);
        result.add(UN_RENDER);
        result.add(ADD_DESIGN_DEVICES);
        result.add(MenuUtil.SEPARATOR);
        result.add(SET_BACKGROUND_IMAGE);
        result.add(REMOVE_BACKGROUND_IMAGE);
        result.add(SET_WIDGET_COLOR);
        result.add(MenuUtil.SEPARATOR);
        result.add(SET_FRAME_TPL);
        result.add(CLEAR_FRAME_TPL);
        result.add(MenuUtil.SEPARATOR);
        result.add(SET_WIDGET_IMAGE);
        result.add(REMOVE_WIDGET_IMAGE);
        result.add(CAPTURE_WIDGET_IMAGE);
        result.add(MenuUtil.SEPARATOR);
        result.add(NUDGE_CASCADE);
        result.add(LAYERING_CASCADE);
        result.add(WIDGET_ALIGNMENT_CASCADE);
        if (research) {
            result.add(MenuUtil.SEPARATOR);
            result.add(GROUP);
            result.add(UNGROUP);
        }
        result.add(MenuUtil.SEPARATOR);
        result.add(ZOOM_IN);
        result.add(ZOOM_OUT);
        result.add(ZOOM_NORMAL);
        result.add(ZOOM_FIT);
        return result.toArray(new MenuItemDefinition[result.size()]);
    } // createFrameModifyMenuDefn

    // View menu items for non-editable drawing views
    // Mnemonics:                   Accelerators:
    //      f       Zoom to Fit           ^/
    //      m       Zoom In               ^+
    //      n       Normal zoom           ^=
    //      o       Zoom Out              ^-
    protected static MenuItemDefinition[] createZoomModifyMenuDefn()
    {
        return new MenuItemDefinition[]
                                      {
                ZOOM_IN,
                ZOOM_OUT,
                ZOOM_NORMAL,
                ZOOM_FIT
                                      };
    } // createZoomModifyMenuDefn

    // View menu items for non-editable drawing views
    // Mnemonics:                   Accelerators:
    //      r       Recompute
    //      t       Change Think Time
    //      w       Change Wait Time
    //      f       Zoom to Fit           ^/
    //      m       Zoom In               ^+
    //      n       Normal zoom           ^=
    //      o       Zoom Out              ^-
    protected static MenuItemDefinition[] createScriptModifyMenuDefn()
    {
        return new MenuItemDefinition[]
                                      {
                RECOMPUTE,
                SHOW_VISUALIZATION,
                MenuUtil.SEPARATOR,
                ZOOM_IN,
                ZOOM_OUT,
                ZOOM_NORMAL,
                ZOOM_FIT
                                      };
    } // createScriptModifyMenuDefn

    // Help menu items
    // Mnemonics:                   Accelerators:
    //      a       About
    //      h       Help                    F1
    protected static MenuItemDefinition[] createHelpMenuDefn()
    {
        int controlKey = MenuUtil.platformControlKey();

        // On Mac OS X the about menu item is created in MacSupport, not here
        if (OSUtils.MACOSX) {
            return new SimpleMenuItemDefinition[]
                                                { new SimpleMenuItemDefinition(L10N.get("MI.Help", "&Help"),
                                                                               CogToolLID.Help,
                                                                               controlKey | '?')
                                                };
        }
        else {
            return new SimpleMenuItemDefinition[]
                                                { new SimpleMenuItemDefinition(L10N.get("MI.Help", "&Help"),
                                                                               CogToolLID.Help,
                                                                               SWT.F1),
                                                                               new SimpleMenuItemDefinition(L10N.get("MI.About",
                                                                               "&About CogTool"),
                                                                               CogToolLID.About)
                                                };
        }
    } // createHelpMenuDefn

    //    protected static final MenuUtil.CascadingMenuItemDefinition[] menuDefnsNormal =
    //        { new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.File",
    //                                                            "&File"),
    //                                                   createFileMenuDefn(false)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Edit",
    //                                                            "&Edit"),
    //                                                   createEditMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Create",
    //                                                           "&Create"),
    //                                                   createCreateMenuDefn(true)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ProjectModify",
    //                                                            "&Modify"),
    //                                                   createProjectModifyMenuDefn(false)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.DefaultModify",
    //                                                            "&Modify"),
    //                                                   createDefaultModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ZoomModify",
    //                                                            "&Modify"),
    //                                                   createZoomModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ScriptModify",
    //                                                            "&Modify"),
    //                                                   createScriptModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Script",
    //                                                            "&Script"),
    //                                                   createScriptMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Window",
    //                                                            "&Window"),
    //                                                   null),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Help",
    //                                                            "&Help"),
    //                                                   createHelpMenuDefn()) };
    //
    //    protected static final MenuUtil.CascadingMenuItemDefinition[] menuDefnsResearch =
    //        { new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.File",
    //                                                            "&File"),
    //                                                   createFileMenuDefn(true)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Edit",
    //                                                            "&Edit"),
    //                                                   createEditMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Create",
    //                                                           "&Create"),
    //                                                   createCreateMenuDefn(true)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ProjectModify",
    //                                                            "&Modify"),
    //                                                   createProjectModifyMenuDefn(true)),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.DefaultModify",
    //                                                            "&Modify"),
    //                                                   createDefaultModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ZoomModify",
    //                                                            "&Modify"),
    //                                                   createZoomModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ScriptModify",
    //                                                            "&Modify"),
    //                                                   createScriptModifyMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Script",
    //                                                            "&Script"),
    //                                                   createScriptMenuDefn()),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Window",
    //                                                            "&Window"),
    //                                                   null),
    //          new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Help",
    //                                                            "&Help"),
    //                                                   createHelpMenuDefn()) };

    public static MenuUtil.CascadingMenuItemDefinition[] menuDefns =
    { new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.File",
    "&File"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Edit",
    "&Edit"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.FrameEdit",
    "&Edit"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Create",
    "&Create"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ProjectModify",
    "&Modify"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.FrameModify",
    "&Modify"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ZoomModify",
    "&Modify"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ScriptModify",
    "&Modify"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Script",
    "&Script"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Window",
    "&Window"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Help",
    "&Help"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.Edit",
    "&Edit"),
    null),
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.DesignModify",
    "&Modify"),
    null) ,
    new MenuUtil.CascadingMenuItemDefinition(L10N.get("MI.ImportXML",
    "&Import XML"),
    null),};
    static {
        updateMenuDefinitions();
    }

    public static void updateMenuDefinitions()
    {
        //        menuDefns =
        //            (CogToolPrefs.isResearch() ? menuDefnsResearch : menuDefnsNormal);
        buildMenuDefinitions(menuDefns,
                             CogToolPref.RESEARCH.getBoolean(),
                             CogToolPref.HCIPA.getBoolean());
    }

    protected static void buildMenuDefinitions(CascadingMenuItemDefinition[] menuDefns,
                                               boolean research,
                                               boolean hcipa)
    {
        menuDefns[0].setMenuItems(createFileMenuDefn(research, hcipa));
        menuDefns[1].setMenuItems(createEditMenuDefn());
        menuDefns[2].setMenuItems(createFrameEditMenuDefn());
        menuDefns[3].setMenuItems(createCreateMenuDefn(hcipa));
        menuDefns[4].setMenuItems(createProjectModifyMenuDefn(research));
        menuDefns[5].setMenuItems(createFrameModifyMenuDefn(research));
        menuDefns[6].setMenuItems(createZoomModifyMenuDefn());
        menuDefns[7].setMenuItems(createScriptModifyMenuDefn());
        menuDefns[8].setMenuItems(createScriptMenuDefn());
        menuDefns[9].setMenuItems(null);
        menuDefns[10].setMenuItems(createHelpMenuDefn());
        menuDefns[11].setMenuItems(createDictionaryEditMenuDefn());
        menuDefns[12].setMenuItems(createDesignModifyMenuDefn(research));
    }

    protected static int findNexusCascade(MenuItem[] nexusItems,
                                          Object nexusData,
                                          ILeadItemUpdater leadItemUpdater)
    {
        int i = 0;

        while ((i < nexusItems.length) &&
                (nexusData != nexusItems[i].getData()))
        {
            if (leadItemUpdater != null) {
                leadItemUpdater.updateLeadItem(nexusItems[i], i);
            }

            i++;
        }

        if (i == nexusItems.length) {
            return -1;
        }

        return i;
    }

    protected static void updateNexusCascadeItem(Menu inNexusMenu,
                                                 IWindowMenuData<?> menuData)
    {
        ListenerIdentifier itemLID = menuData.getLID();

        MenuItem[] windowMenuItems = inNexusMenu.getItems();
        int originalItemCount = windowMenuItems.length;

        for (int i = 0; i < originalItemCount; i++) {
            if (windowMenuItems[i].getData() == itemLID) {
                windowMenuItems[i].setText(menuData.getEntryLabel());
                return;
            }
        }

        throw new RcvrUIException("Cannot find window item");
    }

    protected static void updateMenuLabels(Menu inWindowMenu,
                                           IWindowMenuData<?> menuData,
                                           ILeadItemUpdater leadItemUpdater)
    {
        // Find nexus cascade
        MenuItem[] nexusItems = inWindowMenu.getItems();

        int i = findNexusCascade(nexusItems,
                                 menuData.getNexusData(),
                                 leadItemUpdater);

        if (i >= 0) {
            nexusItems[i].setText(menuData.getNexusLabel());

            updateNexusCascadeItem(nexusItems[i].getMenu(), menuData);
        }
    }

    /**
     * Support for updating any lead items of the Window menu.
     */
    public interface ILeadItemUpdater
    {
        /**
         * Update the given lead item if necessary.
         *
         * @param leadItem the lead menu item to update
         * @param position the 0-based index of the item in the Window menu
         */
        public void updateLeadItem(MenuItem leadItem, int position);
    }

    public static void updateMenuLabels(IWindowMenuData<?> menuData,
                                        ILeadItemUpdater leadItemUpdater)
    {
        Iterator<Menu> menus = windowMenus.iterator();

        while (menus.hasNext()) {
            Menu windowMenu = menus.next();

            updateMenuLabels(windowMenu, menuData, leadItemUpdater);
        }
    }

    // Returns true if it removed the last item from the cascade
    protected static boolean removeNexusCascadeItem(Menu fromNexusMenu,
                                                    IWindowMenuData<?> menuData)
    {
        ListenerIdentifier itemLID = menuData.getLID();

        boolean precedingWasSeparator = false;

        MenuItem[] windowMenuItems = fromNexusMenu.getItems();
        int originalItemCount = windowMenuItems.length;

        int i;

        for (i = 0; i < originalItemCount; i++) {
            MenuItem windowItem = windowMenuItems[i];
            Object itemData = windowItem.getData();

            if (itemData == null) {     // indicates a SEPARATOR
                precedingWasSeparator = true;
            }
            else if (itemData == itemLID) {
                windowItem.dispose();
                break;
            }
            else {
                precedingWasSeparator = false;
            }
        }

        if (i == originalItemCount) {
            throw new RcvrUIException("Cannot find window item");
        }

        // Check if we need to remove a SEPARATOR
        if (i == 0) {
            if ((originalItemCount > 1) &&
                    (windowMenuItems[1].getData() == null))
            {
                // Removed first and second was a SEPARATOR; remove it
                windowMenuItems[1].dispose();
            }
        }
        else if (precedingWasSeparator) {
            // Preceding was a SEPARATOR.  Check if we removed last or if the
            // following item is also a SEPARATOR; if so, remove preceding.
            if (((i + 1) == originalItemCount) ||
                    (windowMenuItems[i + 1].getData() == null))
            {
                windowMenuItems[i - 1].dispose();
            }
        }

        // Return true if no items remain
        return fromNexusMenu.getItems().length == 0;
    }

    protected static void removeWindowMenuItem(Menu fromWindowMenu,
                                               IWindowMenuData<?> menuData)
    {
        // Find nexus cascade
        MenuItem[] nexusItems = fromWindowMenu.getItems();

        int i = findNexusCascade(nexusItems, menuData.getNexusData(), null);

        // If we removed the last item in the cascade, remove the nexus' item
        if ((i >= 0) &&
                removeNexusCascadeItem(nexusItems[i].getMenu(), menuData))
        {
            nexusItems[i].dispose();

            // TODO: stupid test for now since RootView's window menu
            // has no leading items
            if (fromWindowMenu.getItems().length == 0) {
                MenuItem noWindowsItem =
                    new MenuItem(fromWindowMenu, SWT.PUSH);

                noWindowsItem.setText(NO_WINDOWS.name);
                noWindowsItem.setData(NO_WINDOWS_SENTINEL);
                noWindowsItem.setEnabled(false);
            }
        }
    }

    protected static void removeWindowMenu(Menu windowMenuToRemove)
    {
        // First, remove this Window menu from the registry
        // (no point in updating it; it is in the process of being disposed)
        windowMenus.remove(windowMenuToRemove);

        IWindowMenuData<?> menuData =
            (IWindowMenuData<?>) windowMenuToRemove.getData();

        // Update other menus
        if (windowMenus.size() > 0) {
            Iterator<Menu> menus = windowMenus.iterator();

            while (menus.hasNext()) {
                Menu windowMenu = menus.next();

                removeWindowMenuItem(windowMenu, menuData);
            }
        }
    }

    protected static Listener windowItemSelectionListener =
        new Listener() {

        public void handleEvent(Event evt)
        {
            WindowListenerID id = (WindowListenerID) evt.widget.getData();

            id.getView().takeFocus();
        }
    };

    protected static void copyExistingNexusWindows(MenuItem existingNexusItem,
                                                   Menu newWindowMenu)
    {
        MenuItem newNexusItem = new MenuItem(newWindowMenu, SWT.CASCADE);

        newNexusItem.setText(existingNexusItem.getText());
        newNexusItem.setImage(existingNexusItem.getImage());
        newNexusItem.setData(existingNexusItem.getData());  // the nexus!

        Menu newNexusMenu = new Menu(newWindowMenu.getShell(), SWT.DROP_DOWN);

        newNexusItem.setMenu(newNexusMenu);

        MenuItem[] nestedNexusItems = existingNexusItem.getMenu().getItems();

        for (MenuItem nestedNexusItem : nestedNexusItems) {
            MenuItem newNestedItem =
                new MenuItem(newNexusMenu, nestedNexusItem.getStyle());

            newNestedItem.setText(nestedNexusItem.getText());
            newNestedItem.setImage(nestedNexusItem.getImage());

            Object windowLID = nestedNexusItem.getData();

            if (windowLID != null) {
                newNestedItem.setData(windowLID);

                newNestedItem.addListener(SWT.Selection,
                                          windowItemSelectionListener);
            }
        }
    } // copyExistingNexusWindows

    // Recursive step; since nexus cascades are the bottom group, we can work
    // from the bottom up, but copy from the top of the nexus cascades down.
    protected static void copyExistingNexusCascades(MenuItem[] existingItems,
                                                    int itemIndex,
                                                    Menu newWindowMenu)
    {
        if (itemIndex >= 0) {
            MenuItem existingItem = existingItems[itemIndex];

            if (existingItem.getStyle() == SWT.CASCADE) {
                copyExistingNexusCascades(existingItems,
                                          itemIndex - 1,
                                          newWindowMenu);

                copyExistingNexusWindows(existingItem, newWindowMenu);
            }
        }
    }

    protected static void copyExistingWindows(Menu existingWindowMenu,
                                              Menu newWindowMenu)
    {
        MenuItem[] existingItems = existingWindowMenu.getItems();

        copyExistingNexusCascades(existingItems,
                                  existingItems.length - 1,
                                  newWindowMenu);
    } // copyExistingWindows

    protected static Menu createNexusCascade(Menu windowMenu,
                                             int index,
                                             IWindowMenuData<?> menuData)
    {
        MenuItem newNexusItem =
            (index != -1) ? new MenuItem(windowMenu, SWT.CASCADE, index)
        : new MenuItem(windowMenu, SWT.CASCADE);

            Menu cascadeParent = new Menu(windowMenu.getShell(), SWT.DROP_DOWN);

            newNexusItem.setMenu(cascadeParent);

            newNexusItem.setText(menuData.getNexusLabel());
            newNexusItem.setData(menuData.getNexusData());  // the nexus!
            // ... setAccelerator ??

            return cascadeParent;
    } // createNexusCascade

    protected static Menu ensureNexusCascade(Menu windowMenu,
                                             MenuItem[] windowMenuItems,
                                             IWindowMenuData<?> menuData)
    {
        String nexusLabel = menuData.getNexusLabel();
        Class<?> nexusType = menuData.getNexusType();
        Object nexusData = menuData.getNexusData();

        int i;

        for (i = windowMenuItems.length; i > 0; i--) {
            MenuItem windowItem = windowMenuItems[i - 1];

            if ((windowItem.getStyle() == SWT.CASCADE) &&
                    (nexusType != null) &&
                    nexusType.isInstance(windowItem.getData()))
            {
                if (windowItem.getData() == nexusData) {
                    return windowItem.getMenu();
                }

                // Check if the current nexus item comes after nexusLabel
                if (windowItem.getText().compareToIgnoreCase(nexusLabel) < 0) {
                    break;
                }
            }
            else {
                break;          // no longer within the group of nexus cascades
            }
        }

        return createNexusCascade(windowMenu, i, menuData);
    } // ensureNexusCascade

    protected static final int IN_PROJECT_ZONE = 0;
    protected static final int IN_DESIGN_ZONE = 1;
    protected static final int IN_FRAME_ZONE = 2;
    protected static final int IN_SCRIPT_ZONE = 3;
    protected static final int IN_VISUALIZATION_ZONE = 4;
    protected static final int IN_RBAUDITOR_ZONE = 5;
    protected static final int LAST_ZONE = 5;

    protected static Class<?>[] zoneClasses =
        new Class<?>[] { ProjectView.class,
        DesignEditorView.class,
        FrameEditorView.class,
        ScriptView.class,
        PERTChartView.class
        //,RBAuditorView.class
    };

    protected static int determineZone(Class<? extends View> viewClass)
    {
        int zone = 0;

        while (zone <= LAST_ZONE) {
            if (zoneClasses[zone].isAssignableFrom(viewClass)) {
                return zone;
            }

            zone++;
        }

        throw new RcvrUIException("Unknown zone class");
    }

    protected static void addToCascade(Menu nexusCascade,
                                       IWindowMenuData<?> menuData)
    {
        boolean needSeparator = true;
        String newLabel = menuData.getEntryLabel();
        int desiredZone = determineZone(menuData.getViewType());
        int currentZone = IN_PROJECT_ZONE;

        MenuItem[] nexusItems = nexusCascade.getItems();

        int i = 0;

        while (i < nexusItems.length) {
            MenuItem nexusItem = nexusItems[i];
            Object viewLID = nexusItem.getData();

            if (viewLID != null) {
                currentZone =
                    determineZone(((WindowListenerID)
                            viewLID).getView().getClass());

                if (desiredZone == currentZone) {
                    needSeparator = false;

                    // Check if new item comes before current item
                    if (nexusItem.getText().compareToIgnoreCase(newLabel) > 0)
                    {
                        break;  // insert here! (see (1) below)
                    }
                }
                else if (! needSeparator) {
                    break;      // reached end of proper zone; insert!
                    // Actually, should never reach here!
                }
                else if (desiredZone < currentZone) {
                    break;      // found first entry of a later zone (3)
                }
            }
            else if (! needSeparator) {
                break;          // reached end of proper zone; insert! (2)
            }

            i++;
        }

        // Here due to one of:
        // (1) Found zone and found proper location within the group
        // (2) Found zone and proper location is at end of the group
        // (3) Reached the first entry of a later zone; needs a separator after
        // (4) Reached the end of the list w/o finding desired zone;
        //     needs a separator before only if the list is non-empty

        // Check for (4) and need a separator before
        if (needSeparator && (i == nexusItems.length)) {
            if (i > 0) {                // not the first item to be added
                new MenuItem(nexusCascade, SWT.SEPARATOR, i++);
            }

            needSeparator = false;      // simplify later test
        }

        MenuItem newItem = new MenuItem(nexusCascade, SWT.PUSH, i);

        newItem.setText(newLabel);
        newItem.setData(menuData.getLID());

        newItem.addListener(SWT.Selection, windowItemSelectionListener);

        // Check for (3)
        if (needSeparator) {
            new MenuItem(nexusCascade, SWT.SEPARATOR, i + 1);
        }
    } // addToCascade

    protected static void addWindowMenu(Menu newWindowMenu,
                                        IWindowMenuData<?> menuData)
    {
        if (windowMenus.size() > 0) {
            copyExistingWindows(windowMenus.get(0), newWindowMenu);
        }

        // Add to global registry
        windowMenus.add(newWindowMenu);
        newWindowMenu.setData(menuData);

        // Must have data to add view with Window menu to all Window menus
        // i.e. the RootView will not be added!
        if (menuData.getNexusData() != null) {
            Iterator<Menu> menus = windowMenus.iterator();

            while (menus.hasNext()) {
                Menu windowMenu = menus.next();

                Menu nexusCascade = null;

                // Remove the last item if it is the NO_WINDOWS_SENTINEL
                MenuItem[] nexusItems = windowMenu.getItems();

                if (nexusItems.length > 0) {
                    MenuItem lastItem = nexusItems[nexusItems.length - 1];

                    if (lastItem.getData() == NO_WINDOWS_SENTINEL) {
                        lastItem.dispose();

                        nexusCascade =
                            createNexusCascade(windowMenu, -1, menuData);
                    }
                }

                if (nexusCascade == null) {
                    nexusCascade =
                        ensureNexusCascade(windowMenu, nexusItems, menuData);
                }

                addToCascade(nexusCascade, menuData);
            }

            newWindowMenu.addDisposeListener(new DisposeListener()
            {

                public void widgetDisposed(DisposeEvent e)
                {
                    removeWindowMenu((Menu) e.getSource());
                }
            });
        }
        else {
            newWindowMenu.addDisposeListener(new DisposeListener()
            {

                public void widgetDisposed(DisposeEvent e)
                {
                    windowMenus.remove(e.getSource());
                }
            });
        }
    } // addWindowMenu

    public static void buildMenu(MenuType[] neededMenus,
                                 Shell viewShell,
                                 final Listener selectionListener,
                                 ListenerIdentifierMap lIDMap,
                                 IWindowMenuData<?> menuData)
    {
        int windowMenuIndex = -1;
        int fileMenuIndex = -1;

        MenuUtil.CascadingMenuItemDefinition[] defn =
            new MenuUtil.CascadingMenuItemDefinition[neededMenus.length];

        for (int i = 0; i < neededMenus.length; i++) {
            defn[i] = menuDefns[neededMenus[i].getOrdering()];

            if (neededMenus[i] == MenuFactory.MenuType.FileMenu) {
                fileMenuIndex = i;
            }
            else if (neededMenus[i] == MenuFactory.MenuType.WindowMenu) {
                windowMenuIndex = i;
                defn[i].menuItems = menuData.getWindowMenuLeadItems();
            }
        }

        Menu newMenuBar = MenuUtil.createMenu(viewShell,
                                              SWT.BAR | SWT.LEFT_TO_RIGHT,
                                              defn,
                                              ListenerIdentifierMap.NORMAL,
                                              selectionListener,
                                              lIDMap);

        if (fileMenuIndex != -1) {
            final Menu fileMenu = newMenuBar.getItem(fileMenuIndex).getMenu();

            fileMenu.addMenuListener(new MenuAdapter() {
                @Override
                public void menuShown(MenuEvent evt)
                {
                    for (MenuItem item : fileMenu.getItems()) {
                        //This menu item corresponds to the Open Recent submenu
                        if (item.getData() == RECENT_FLAG) {
                            Menu cascade = item.getMenu();
                            for (MenuItem subItem : cascade.getItems()) {
                                subItem.dispose();
                            }

                            char recentIndex = '0';

                            for (String pathName : CogToolPref.getRecent()) {
                                if (! (MenuFactory.UNSET_FILE.equals(pathName)))
                                {
                                    if (recentIndex != 0) {
                                        if (recentIndex != '9') {
                                            recentIndex++;
                                        }
                                        else {
                                            recentIndex = ' ';
                                        }
                                    }

                                    String safePathName =
                                        "&" + recentIndex
                                        + " "
                                        + pathName.replaceAll("&", "&&");
                                    MenuItem mi =
                                        MenuUtil.addMenuItem(cascade,
                                                             safePathName,
                                                             SWT.PUSH);

                                    CogToolLID lid =
                                        new CogToolLID.OpenRecentLID("OpenRecent",
                                                                     pathName);

                                    mi.addListener(SWT.Selection,
                                                   selectionListener);
                                    mi.setData(lid);
                                }
                            }

                            boolean hasRecent = CogToolPref.hasRecent();

                            if (hasRecent) {
                                MenuUtil.addMenuItem(cascade, "", SWT.SEPARATOR);
                            }

                            MenuItem clearItem =
                                MenuUtil.addMenuItem(cascade,
                                                     L10N.get("MI.ClearItems",
                                                     "Clear items"),
                                                     SWT.PUSH);

                            clearItem.addListener(SWT.Selection,
                                                  selectionListener);
                            clearItem.setData(CogToolLID.ClearRecent);

                            clearItem.setEnabled(hasRecent);

                            //break;
                        }

                        //This menu item corresponds to the Import submenu
                        // TODO this is a mess and needs to be tidied up
                        else if (item.getData() == IMPORT_OTHER_FLAG) {
                            Menu cascade = item.getMenu();
                            for (MenuItem subItem : cascade.getItems()) {
                                subItem.dispose();
                            }

                            File directory = null;
                            String directoryName = CogToolPref.CONVERTER_DIRECTORY.getString();
                            boolean researchMode = CogToolPref.RESEARCH.getBoolean();

                            if (directoryName != null && ! directoryName.equals(""))
                            {
                                directory = new File(directoryName);

                                URL[] urls = null;
                                try {
                                     // TODO: fix this deprecated method
                                    URL url= directory.toURL();
                                    urls = new URL[]{url};
                                }
                                catch (MalformedURLException e1) {
                                    // TODO Auto-generated catch block
                                    e1.printStackTrace();
                                }
                                if(directory.exists()){
                                    URLClassLoader classLoader = new URLClassLoader(urls);

                                    String[] children = directory.list();
                                    boolean firstMenuItem = true;

                                    for (String resource : children) {
                                        System.out.println("Resource " + resource);
                                        resource = (resource.lastIndexOf(".") == -1)?
                                                                                     resource: resource.substring(0, resource.lastIndexOf('.'));
                                        try {

                                            Class<ImportConverter> translatorClass = (Class<ImportConverter>) classLoader.loadClass(resource);

                                            try{
                                                Object converter = null;
                                                try{
                                                    converter = translatorClass.newInstance();
                                                    Class[] nameMethodParameters = new Class[0];
                                                    Method method = translatorClass.getMethod("name", nameMethodParameters);
                                                    String name =  (String) method.invoke(converter);
                                                    if(! name.endsWith("...")){
                                                        name = name + "...";
                                                    }

                                                    if(firstMenuItem){
                                                        MenuUtil.addMenuItem(cascade, "", SWT.SEPARATOR);
                                                        firstMenuItem = false;
                                                    }
                                                    String menuItemName = "Import Designs from " + name;
                                                    MenuItem mi =
                                                        MenuUtil.addMenuItem(cascade,
                                                                             menuItemName,
                                                                             SWT.PUSH);

                                                    CogToolLID lid = new CogToolLID.ConverterFilesLID("NewDesignFromImport");
                                                    ((ConverterFilesLID) lid).setClassAttribute(translatorClass);
                                                    mi.setData(lid);
                                                    mi.addListener(SWT.Selection,
                                                                   selectionListener);
                                                }
                                                catch(Exception ex){
                                                    throw new RcvrImportException("The file " + resource + " can not be loaded as a class.");
                                                }
                                                //TODO: catch the specific error and give a more detailed message
                                                //Interact with the user and display the message.
                                                catch(Error er){
                                                    System.out.println("Error was thrown!");
                                                    //TODO: How to throw this recoverable exception but move on?
                                                    //throw new RcvrImportException("The file " + resource + " can not be loaded as a class.");

                                                }
                                            }
                                            catch( Exception ex){
                                                throw new RcvrImportException("The file " + resource + " is not a valid converter file.");
                                            }
                                            catch(Error er){
                                                System.out.println("Error was thrown2!");
                                                //TODO: How to throw this recoverable exception but move on?
                                                //throw new RcvrImportException("The file " + resource + " can not be loaded as a class.");

                                            }
                                        }
                                        catch (Exception ex){
                                            throw new RcvrImportException("The file " + resource + " cannot be loaded as a class.");
                                        }
                                        catch(Error er){
                                            System.out.println("Error was thrown3!");
                                            //TODO: How to throw this recoverable exception but move on?
                                            //throw new RcvrImportException("The file " + resource + " can not be loaded as a class.");

                                        }

                                    }
                                    break;
                                }
                            }

                        }
                    }

                }
            });
        }

        if (windowMenuIndex != -1) {
            defn[windowMenuIndex].menuItems = null;     // reset!

            addWindowMenu(newMenuBar.getItem(windowMenuIndex).getMenu(),
                          menuData);
        }

        viewShell.setMenuBar(newMenuBar);
    }

    public static void rebuildAllMenus()
    {
        Menu[] menus =
            windowMenus.toArray(new Menu[windowMenus.size()]);

        for (Menu menu : menus) {
            IWindowMenuData<?> menuData =
                (IWindowMenuData<?>) menu.getData();

            menuData.getView().rebuildMenus();
            menuData.setInitiallyEnabledItems();
        }
    }

    public static final String UNSET_FILE = "";
}
