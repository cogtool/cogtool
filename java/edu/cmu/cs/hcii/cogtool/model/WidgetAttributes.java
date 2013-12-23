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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.IAttributed;

/**
 * CAUTION: NEVER CHANGE THE VALUE OF ANY _ATTR STRING OR ANY CONSTANT.
 * They are saved in .cgt files!  In particular, anything in a string that
 * starts "WidgetType." must remain that way, even if we change the name of
 * this class again!  Or any other string, for that matter!
 */
public class WidgetAttributes
{
    private WidgetAttributes() { }  // prevent instantiation

    public static final Set<String> obsoletingAttributeRegistry =
        new HashSet<String>();

    public static boolean isObsoleting(String attrName)
    {
        return obsoletingAttributeRegistry.contains(attrName);
    }

    // Selected is three-valued
    public static final Boolean IS_SELECTED = Boolean.TRUE;
    public static final Boolean NOT_SELECTED = Boolean.FALSE;
    public static final Boolean TOGGLE_SELECTION = null;

    /**
     * Used to reflect the selection status of a widget.
     * Applies to an IWidget or an AScriptStep (as an override).
     * Value reflects whether to set selected (see IS_SELECTED
     *    and NOT_SELECTED) or to toggle its current selection state
     *    (see TOGGLE_SELECTION)
     *
     * Used to specify which Radio button widget is selected
     *    by RadioButtonGroup.
     * Used to toggle a Check or Button in SEDemoController
     *    for a Tap or Left-Click self transition.
     * Used by DesignExportToHTML and RendererSupport to determine
     *    the display state of a checkbox or radio button widget.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String IS_SELECTED_ATTR = "WidgetType.isSelected";

    // A widget can be toggleable or not
    public static final Boolean IS_TOGGLEABLE = Boolean.TRUE;
    public static final Boolean NOT_TOGGLEABLE = Boolean.FALSE;

    /**
     * Applies to a Button IWidget
     * Value reflects whether the widget may toggle
     *
     * Used by SEDemoMouseState to determine if a self-transition is
     *    warranted on a click.
     * Used by GraphicalWidget to create the proper button renderer.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String IS_TOGGLEABLE_ATTR = "WidgetType.isTogglable";

    // A widget can be standard or custom
    public static final Boolean IS_STANDARD = Boolean.TRUE;
    public static final Boolean IS_CUSTOM = Boolean.FALSE;

    /**
     * Applies to an IWidget
     * Value reflects whether the widget exhibits standard
     *    behavior and appearance; the CogTool user is responsible
     *    for specifying these explicitly.
     *
     * Set by createWidget in FrameEditorController
     */
    public static final String IS_STANDARD_ATTR = "WidgetType.isAutomatic";

    // Reflects that no widget is selected.
    public static final IWidget NONE_SELECTED = null;

    /**
     * Applies to an AParentWidget, an SimpleWidgetGroup that contains
     *    radio button widgets, or an AScriptStep (as an override).
     * Reflects which is the selected widget.
     *
     * Used by RadioButtonGroup to control which child widget should
     *    have its IS_SELECTED_ATTR attribute set.
     * Used by (DesignExportToHTML--SHOULD!) and RendererSupport to determine
     *    the display state of a pull-down header or radio button widget.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     *
     * NOTE: If an object that has this attribute set is duplicated,
     * the value of this attribute must be duplicated as well.
     */
    public static final String SELECTION_ATTR = "WidgetType.currentSelection";

    // Reflects that there is no remote label.
    public static final IWidget NO_WIDGET = null;

    /**
     * The remote label for a widget or frame element group, which is
     * a custom IWidget
     */
    public static final String REMOTE_LABEL_ATTR = "WidgetType.remoteLabel";

    // Reflects that there is no remote label owner.
    public static final FrameElement NO_ELEMENT = null;

    /**
     * The owner of a remote label, which is an IFrameElement.
     */
    public static final String REMOTE_LABEL_OWNER_ATTR =
        "WidgetType.remoteLabelOwner";

    // A TextBox (or Text?) widget may support one or multiple lines
    public static final Boolean IS_MULTI_LINE = Boolean.TRUE;
    public static final Boolean IS_SINGLE_LINE = Boolean.FALSE;

    /**
     * Currently unused; intended to reflect whether or not a
     *    TextBox widget has multiple lines or not.
     */
    public static final String IS_MULTILINE_ATTR = "WidgetType.isMultiLine";

    // Default number of entries in a List box that are visible.
    public static final Integer DEFAULT_VISIBLE = new Integer(5);

    /**
     * Currently unused; intended to reflect the number of items
     * in a List box that are visible.  Will be needed when
     * scrollable List boxes are implemented.
     */
    public static final String NUM_VISIBLE_ATTR = "WidgetType.numVisible";


    // Default first visible entry in the list box (0-relative)
    public static final Integer DEFAULT_FIRST_VISIBLE = new Integer(0);

    /**
     * Currently unused; intended to reflect which List box item
     * is scrolled to the top of the visible set.  Will be needed when
     * scrollable List boxes are implemented.
     *
     * Set on the ListBox's containing SimpleWidgetGroup
     *
     * NOTE: If an object that has this attribute set is duplicated,
     * the value of this attribute must be duplicated as well.
     */
    public static final String FIRST_VISIBLE_ATTR = "WidgetType.firstVisible";

    // A widget may be rendered or not
    public static final Boolean IS_RENDERED = Boolean.TRUE;
    public static final Boolean NOT_RENDERED = Boolean.FALSE;

    /**
     * Applies to an IWidget or a widget group; indicates whether or not
     * the widget (or contained widgets) should be rendered using the
     * enclosing Design's skin.
     *
     * Used by the internal widget group in an AParentWidget or
     *    by the group for radio buttons or check boxes
     */
    public static final String IS_RENDERED_ATTR = "WidgetType.isRendered";


    // A MenuItem, PullDownItem, or ListItem widget may represent a separator
    public static final Boolean IS_SEPARATOR = Boolean.TRUE;
    public static final Boolean NON_SEPARATOR = Boolean.FALSE;

    /**
     * Applies to a MenuItem, PullDownItem, or ListItem IWidget.
     *
     * Used when creating a new widget or modifying its title.
     *    If modifying, the the affected widget cannot be selected.
     * Used by anything that needs to compute the size of the widget,
     *    such as when duplicating a widget or resizing widgets
     *    in FrameEditorController (see also AParentWidget.setWidgetSize).
     *    See also FrameEditorMouseState.setDividerBounds and
     *    FrameEditorUI.setFigureBounds.
     * Used by FrameEditorUI.initiateRetitleFigure to prevent re-titling
     *    of a separator (??? why? ???)
     * Used by SEDemoMouseState and SEDemoUI to prevent a self-transition
     *    on a separator.
     * Used by SEDemoController to avoid setting a separator as the
     *    selected PullDownItem for a PullDownHeader.
     * Used by GraphicalMenuItem, GraphicalWidget, and ItemWireRenderer
     *    to determine traversal and for rendering.
     * Used by StructureViewUIModel to prevent separators from acting
     *    as a transition source.
     * FrameEditorView and FrameUIModel set the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String IS_SEPARATOR_ATTR = "WidgetType.isSeparator";

    // Appended text to a TextBox may be empty
    public static final String EMPTY_TEXT = "";

    /**
     * Applies as the override value in an AScriptStep for keyboard
     *    self-transitions of a TextBox widget;
     *
     * Used by SEDemoController to "override" the current TextBox value.
     * Used by the TextActionSegment ctor to support duplication (verify?).
     */
    public static final String APPENDED_TEXT_ATTR = "WidgetType.textContents";

    // Standard submenu transitions can use any of the following actions.
    public static final Integer HOVER_SUBMENU_ACTION = new Integer(0);
    public static final Integer CLICK_SUBMENU_ACTION = new Integer(1);
    public static final Integer TAP_SUBMENU_ACTION = new Integer(2);

    /**
     * Applies in an AMenuWidget as the action to use when generating script
     *    steps for Standard submenu access during demonstration.
     *
     * Used by KLMCognitiveGenerator to generate proper script steps.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String SUBMENU_ACTION_ATTR = "WidgetType.submenuAction";

    // Standard submenu delays when the submenu action is HOVER_SUBMENU_ACTION
    public static final Double NO_SUBMENU_DELAY = new Double(0.0);
    public static final Double PC_SUBMENU_DELAY = new Double(0.5);

    /**
     * Applies in an AMenuWidget as the delay to use when generating script
     *    steps for Standard submenu access during demonstration
     *    (if the associated action is a Hover).
     *
     * Used by KLMCognitiveGenerator to generate proper script steps.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String SUBMENU_DELAY_ATTR = "WidgetType.submenuDelay";

    // The actions that may be used to make a Context Menu visible
    public static final Integer RIGHT_CLICK = new Integer(0);
    public static final Integer CTRL_LEFT_CLICK = new Integer(1);
    public static final Integer TAP_HOLD = new Integer(2);
    public static final Integer MENU_KEY_PRESS = new Integer(3);

    /**
     * Applies in an ContextMenu as the action to use when generating script
     *    steps for Standard context menu access during demonstration.
     *
     * Used by KLMCognitiveGenerator to generate proper script steps.
     * FrameEditorView sets the value based on interaction with
     *    the selected widget's property panel elements.
     */
    public static final String CONTEXT_MENU_ACTION_ATTR =
        "WidgetType.contextMenuAction";

    // Default value is to have no image path.
    public static final String NO_IMAGE = "";

    /**
     * Applies to either an IWidget or an Frame to reflect the
     *    file path of the object's background image, if set.
     *
     * Used by DesignEditorController and FrameEditorController when
     *    modifying the object's background image.  (also HCIPACmd)
     * Used by ImportCogTool
     * Used by FrameEditorView and FramePropertiesPane to display
     *    the current value.
     */
    public static final String IMAGE_PATH_ATTR = "WidgetType.imagePath";

    public static final String NO_FUNCTION_NAME = "";
    public static final String HCIPA_FUNCTION_ATTR = "WidgetType.functionName";

    /**
     * Applies to an Design to hold the list of URLs that had yet
     *    to be crawled during an import of a web site.
     *
     * Used by ImportWebCrawlThread and ProjectController to manage
     *    that list.
     * Used by WebCrawlImportDialog to control which parts of the dialog box
     *    should be enabled (by the presence or absence of remaining URLs).
     */
    public static final String PAUSED_WEB_CRAWL_ATTR =
        "DesignType.pausedWebCrawl";

    public static final ISimilarityDictionary NO_DICTIONARY =
        null;

    // If an object that has this attribute set is duplicated, the value of
    // this attribute must be duplicated as well.
    public static final String DICTIONARY_ATTR = "DesignType.dictionary";

    public static final SNIFACTExecContext NO_CONTEXT = null;

    // If an object that has this attribute set is duplicated, the value of
    // this attribute must be duplicated as well.
    public static final String SNIFACT_CONTEXT_ATTR = "ProjectType.snifActParms";

    /**
     * Must explicitly initialize; seems that some JVM's won't perform
     * the static initialization sections properly (sigh).
     */
    public static void registerAttributes()
    {
        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_SELECTED_ATTR,
                                                           Boolean.class,
                                                           NOT_SELECTED);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_TOGGLEABLE_ATTR,
                                                           Boolean.class,
                                                           NOT_TOGGLEABLE);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_STANDARD_ATTR,
                                                           Boolean.class,
                                                           IS_STANDARD);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(SELECTION_ATTR,
                                                           IWidget.class,
                                                           NONE_SELECTED);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(REMOTE_LABEL_ATTR,
                                                           IWidget.class,
                                                           NO_WIDGET);
        obsoletingAttributeRegistry.add(REMOTE_LABEL_ATTR);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(REMOTE_LABEL_OWNER_ATTR,
                                                           FrameElement.class,
                                                           NO_ELEMENT);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_MULTILINE_ATTR,
                                                           Boolean.class,
                                                           IS_SINGLE_LINE);
        IAttributed.AttributeRegistry.ONLY.defineAttribute(NUM_VISIBLE_ATTR,
                                                           Integer.class,
                                                           DEFAULT_VISIBLE);
        obsoletingAttributeRegistry.add(NUM_VISIBLE_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(FIRST_VISIBLE_ATTR,
                                                           IWidget.class,
                                                           null);
        obsoletingAttributeRegistry.add(FIRST_VISIBLE_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_RENDERED_ATTR,
                                                           Boolean.class,
                                                           NOT_RENDERED);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(IS_SEPARATOR_ATTR,
                                                           Boolean.class,
                                                           NON_SEPARATOR);
        obsoletingAttributeRegistry.add(IS_SEPARATOR_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(APPENDED_TEXT_ATTR,
                                                           String.class,
                                                           EMPTY_TEXT);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(SUBMENU_ACTION_ATTR,
                                                           Integer.class,
                                                           HOVER_SUBMENU_ACTION);
        obsoletingAttributeRegistry.add(SUBMENU_ACTION_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(SUBMENU_DELAY_ATTR,
                                                           Double.class,
                                                           NO_SUBMENU_DELAY);
        obsoletingAttributeRegistry.add(SUBMENU_DELAY_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(CONTEXT_MENU_ACTION_ATTR,
                                                           Integer.class,
                                                           RIGHT_CLICK);
        obsoletingAttributeRegistry.add(CONTEXT_MENU_ACTION_ATTR);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(IMAGE_PATH_ATTR,
                                                           String.class,
                                                           NO_IMAGE);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(HCIPA_FUNCTION_ATTR,
                                                           String.class,
                                                           NO_FUNCTION_NAME);

        @SuppressWarnings("unchecked")
        IAttributed.AttributeDefinition<?> defn =
            new IAttributed.AttributeDefinition<List>(PAUSED_WEB_CRAWL_ATTR,
                                                      List.class,
                                                      null)
            {
                @Override
                public Object createAggregate(int count)
                {
                    return new ArrayList<URLCrawlEntry>(count);
                }
            };

        IAttributed.AttributeRegistry.ONLY.registerAttribute(defn);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(DICTIONARY_ATTR,
                                                           ISimilarityDictionary.class,
                                                           NO_DICTIONARY);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(SNIFACT_CONTEXT_ATTR,
                                                           SNIFACTExecContext.class,
                                                           NO_CONTEXT);
    }
}
