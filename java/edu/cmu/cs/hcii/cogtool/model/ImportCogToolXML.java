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
 * jopt-simple
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

package edu.cmu.cs.hcii.cogtool.model;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.xerces.parsers.DOMParser;
import org.eclipse.ecf.core.util.Base64;
import org.eclipse.swt.graphics.Image;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;

/**
 * Imports designs and demonstrations from an XML File
 * @author AlexF
 * Edited by Brett Harris with Importing from an BMML File
 */
public class ImportCogToolXML
{
    public static final String CURRENT_VERSION = "1";

    public static final String COGTOOL_IMPORT_ELT = "cogtoolimport";
    public static final String DESIGN_ELT = "design";
    public static final String DEVICE_ELT = "device";
    public static final String FRAME_ELT = "frame";
    public static final String CONTROLS_ELT = "controls";
    public static final String CONTROL_ELT = "control";
    public static final String HREF_ELT = "href";
    public static final String BALSAMIQ_TEXT_ELT = "text";

    public static final String MOCKUP_ELT = "mockup";
    public static final String DEMONSTRATION_ELT = "demonstration";
    public static final String BKG_IMAGE_PATH_ELT = "backgroundImagePath";
    public static final String BKG_IMAGE_DATA_ELT = "backgroundImageData";
    public static final String ORIGIN_ELT = "topLeftOrigin";
    public static final String SPEAKER_TEXT_ELT = "speakerText";
    public static final String LISTEN_TIME_SECS_ELT = "listenTimeSecs";
    public static final String WIDGET_ELT = "widget";
    public static final String ELTGROUP_ELT = "eltGroup";
    public static final String ELTNAME_ELT = "elementName";
    public static final String KEYBOARD_TRANSITIONS_ELT = "keyboardTransitions";
    public static final String VOICE_TRANSITIONS_ELT = "voiceTransitions";
    public static final String DISPLAY_LABEL_ELT = "displayLabel";
    public static final String AUX_TEXT_ELT = "auxText";
    public static final String EXTENT_ELT = "extent";
    public static final String TRANSITION_ELT = "transition";
    public static final String ACTION_ELT = "action";
    public static final String MOUSE_ACTION_ELT = "mouseAction";
    public static final String TOUCHSCREEN_ACTION_ELT = "touchscreenAction";
    public static final String GRAFFITI_ACTION_ELT = "graffitiAction";
    public static final String KEYBOARD_ACTION_ELT = "keyboardAction";
    public static final String VOICE_ACTION_ELT = "voiceAction";
    public static final String KBD_MODIFIER_ELT = "modifier";
    public static final String GESTURES_ELT = "gestures";
    public static final String TEXT_ELT = "text";
    public static final String DEMO_STEP_ELT = "demonstrationStep";
    public static final String ACTION_STEP_ELT = "actionStep";
    public static final String KEYBOARD_STEP_ELT = "keyboardActionStep";
    public static final String VOICE_STEP_ELT = "voiceActionStep";
    public static final String THINK_STEP_ELT = "thinkStep";
    public static final String SYS_DELAY_STEP_ELT = "systemDelayStep";
    public static final String LOOK_AT_STEP_ELT = "lookAtWidgetStep";
    public static final String START_EYE_LOC_ELT = "startingEyePosition";
    public static final String START_MOUSE_LOC_ELT = "startingMousePosition";
    public static final String START_LEFT_POS_ELT = "startingLeftHandPosition";
    public static final String START_RIGHT_POS_ELT =
        "startingRightHandPosition";
    public static final String TASK_ELT = "task";
    public static final String TASK_GROUP_ELT = "taskGroup";
    
    public static final String VERSION_ATTR = "version";
    public static final String NAME_ATTR = "name";
    public static final String TYPE_ATTR = "type";
    public static final String PARENT_ATTR = "parent";
    public static final String GROUP_ATTR = "group";
    public static final String REMOTE_LABEL_ATTR = "remoteLabel";
    public static final String CONTROL_TYPE_ATTR = "controlTypeID";
    public static final String CONTROL_ID_ATTR = "controlID";
    public static final String TASK_GROUP_ID_ATTR = "taskGroupID";

    public static final String X_ATTR = "x";
    public static final String Y_ATTR = "y";
    public static final String WIDTH_ATTR = "width";
    public static final String HEIGHT_ATTR = "height";
    public static final String MEASURED_WIDTH_ATTR = "measuredW";
    public static final String MEASURED_HEIGHT_ATTR = "measuredH";
    public static final String SHAPE_ATTR = "shape";
    public static final String DEST_FRAME_NAME_ATTR = "destinationFrameName";
    public static final String BUTTON_ATTR = "button";
    public static final String ACTION_ATTR = "action";
    public static final String IS_CMD_ATTR = "is-command";
    public static final String TASK_NAME_ATTR = "taskName";
    public static final String START_FRAME_NAME_ATTR = "startFrameName";
    public static final String HANDEDNESS_ATTR = "handedness";
    public static final String TARGET_WIDGET_NAME_ATTR = "targetWidgetName";
    public static final String LOOKAT_WIDGET_NAME_ATTR = "lookAtWidgetName";
    public static final String DURATION_ATTR = "durationInSecs";
    public static final String THINK_LABEL_ATTR = "thinkLabel";
    public static final String DELAY_LABEL_ATTR = "delayLabel";
    public static final String GROUP_NATURE_ATTR = "displayedGroupSummary";

    // General WidgetAttributes attribute names and values
    // REMEMBER TO ENTER INTO THE REGISTRIES BELOW!!!!
    public static final String TRUE_VALUE = "true";
    public static final String FALSE_VALUE = "false";
    public static final String IS_SELECTED_ATTR = "w-is-selected";
    public static final String IS_SELECTED_VALUE = TRUE_VALUE;
    public static final String NOT_SELECTED_VALUE = FALSE_VALUE;
    public static final String TOGGLE_VALUE = "toggle";
    public static final String IS_TOGGLEABLE_ATTR = "w-is-toggleable";
    public static final String IS_STANDARD_ATTR = "w-is-standard";
    public static final String SELECTION_ATTR = "w-selected-name";
    public static final String IS_RENDERED_ATTR = "w-is-rendered";
    public static final String IS_SEPARATOR_ATTR = "w-is-separator";
    public static final String APPENDED_TEXT_ATTR = "w-appended-text";
    public static final String SUBMENU_ACTION_ATTR = "w-submenu-action";
    public static final String TAP_VALUE = "tap";
    public static final String CLICK_VALUE = "click";
    public static final String HOVER_VALUE = "hover";
    public static final String SUBMENU_DELAY_ATTR = "w-submenu-delay";
    public static final String PC_DELAY_VALUE = "pc-delay";
    public static final String NO_DELAY_VALUE = "no-delay";
    public static final String CONTEXT_MENU_ACTION_ATTR = "w-menu-action";
    public static final String CTRL_LEFT_VALUE = "ctrl-left-click";
    public static final String TAP_HOLD_VALUE = "tap-hold";
    public static final String MENU_KEY_VALUE = "menu-key";
    public static final String RIGHT_CLICK_VALUE = "right-click";

    public static final String KEYBOARD_DEVICE = "keyboard";
    public static final String MOUSE_DEVICE = "mouse";
    public static final String TOUCHSCREEN_DEVICE = "touchscreen";
    public static final String MICROPHONE_DEVICE = "microphone";
    public static final String DISPLAY_DEVICE = "display";
    public static final String SPEAKER_DEVICE = "speaker";

    public static final String BUTTON_WIDGETTYPE = "button";
    public static final String LINK_WIDGETTYPE = "link";
    public static final String CHECKBOX_WIDGETTYPE = "check box";
    public static final String RADIO_WIDGETTYPE = "radio button";
    public static final String TEXTBOX_WIDGETTYPE = "text box";
    public static final String TEXT_WIDGETTYPE = "text";
    public static final String PULLDOWNLIST_WIDGETTYPE = "pull-down list";
    public static final String PULLDOWNITEM_WIDGETTYPE = "pull-down item";
    public static final String LISTBOXITEM_WIDGETTYPE = "list box item";
    public static final String CONTEXTMENU_WIDGETTYPE = "context menu";
    public static final String MENUHEADER_WIDGETTYPE = "menu";
    public static final String SUBMENU_WIDGETTYPE = "submenu";
    public static final String MENUITEM_WIDGETTYPE = "menu item";
    public static final String GRAFFITI_WIDGETTYPE = "graffiti";
    public static final String NONINTERACTIVE_WIDGETTYPE = "non-interactive";

    public static final String RECTANGLE_SHAPE = "rectangle";
    public static final String ELLIPSE_SHAPE = "ellipse";
    public static final String ROUND_RECT_SHAPE = "rounded rectangle";

    public static final String LEFT_BUTTON = "left";
    public static final String MIDDLE_BUTTON = "middle";
    public static final String RIGHT_BUTTON = "right";

    public static final String DOWNUP_ACTION = "downUp";
    public static final String TAP_ACTION = "tap";
    public static final String PRESS_ACTION = "press";
    public static final String DOUBLE_ACTION = "double";
    public static final String TRIPLE_ACTION = "triple";
    public static final String DOWN_ACTION = "down";
    public static final String UP_ACTION = "up";
    public static final String HOVER_ACTION = "hover";

    public static final String SHIFT_MODIFIER = "SHIFT";
    public static final String CTRL_MODIFIER = "CTRL";
    public static final String ALT_MODIFIER = "ALT";
    public static final String COMMAND_MODIFIER = "COMMAND";
    public static final String FUNCTION_MODIFIER = "FUNCTION";

    public static final String RIGHT_HAND = "right";
    public static final String LEFT_HAND = "left";

    public static final String FINAL_FRAME_NAME = "FINAL FRAME";

    /**
     * Thrown when the import cannot complete.
     */
    public static class ImportFailedException extends RuntimeException
    {
        public ImportFailedException(String msg, Throwable ex)
        {
            super(msg, ex);
        }

        public ImportFailedException(String msg)
        {
            super(msg);
        }
    }

    private static final Map<String,
                               IAttributed.AttributeDefinition<?>> ATTRIBUTE_REGISTRY =
        new HashMap<String, IAttributed.AttributeDefinition<?>>();

    private static final Map<String, Object> VALUE_REGISTRY =
        new HashMap<String, Object>();

    private static IAttributed.AttributeDefinition<?> getAttrDefn(String attr)
    {
        return IAttributed.AttributeRegistry.ONLY.getAttributeDefn(attr);
    }

    private static void registerAttributes()
    {
        ATTRIBUTE_REGISTRY.put(IS_SELECTED_ATTR,
                               getAttrDefn(WidgetAttributes.IS_SELECTED_ATTR));
        ATTRIBUTE_REGISTRY.put(IS_TOGGLEABLE_ATTR,
                               getAttrDefn(WidgetAttributes.IS_TOGGLEABLE_ATTR));
        ATTRIBUTE_REGISTRY.put(IS_STANDARD_ATTR,
                               getAttrDefn(WidgetAttributes.IS_STANDARD_ATTR));
        ATTRIBUTE_REGISTRY.put(SELECTION_ATTR,
                               getAttrDefn(WidgetAttributes.SELECTION_ATTR));
        ATTRIBUTE_REGISTRY.put(IS_RENDERED_ATTR,
                               getAttrDefn(WidgetAttributes.IS_RENDERED_ATTR));
        ATTRIBUTE_REGISTRY.put(IS_SEPARATOR_ATTR,
                               getAttrDefn(WidgetAttributes.IS_SEPARATOR_ATTR));
        ATTRIBUTE_REGISTRY.put(APPENDED_TEXT_ATTR,
                               getAttrDefn(WidgetAttributes.APPENDED_TEXT_ATTR));
        ATTRIBUTE_REGISTRY.put(SUBMENU_ACTION_ATTR,
                               getAttrDefn(WidgetAttributes.SUBMENU_ACTION_ATTR));
        ATTRIBUTE_REGISTRY.put(SUBMENU_DELAY_ATTR,
                               getAttrDefn(WidgetAttributes.SUBMENU_DELAY_ATTR));
        ATTRIBUTE_REGISTRY.put(CONTEXT_MENU_ACTION_ATTR,
                               getAttrDefn(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR));

        VALUE_REGISTRY.put(TRUE_VALUE, Boolean.TRUE);
        VALUE_REGISTRY.put(FALSE_VALUE, Boolean.FALSE);
        VALUE_REGISTRY.put(TOGGLE_VALUE, WidgetAttributes.TOGGLE_SELECTION);

        VALUE_REGISTRY.put(TAP_VALUE, WidgetAttributes.TAP_SUBMENU_ACTION);
        VALUE_REGISTRY.put(CLICK_VALUE, WidgetAttributes.CLICK_SUBMENU_ACTION);
        VALUE_REGISTRY.put(HOVER_VALUE, WidgetAttributes.HOVER_SUBMENU_ACTION);

        VALUE_REGISTRY.put(PC_DELAY_VALUE, WidgetAttributes.PC_SUBMENU_DELAY);
        VALUE_REGISTRY.put(NO_DELAY_VALUE, WidgetAttributes.NO_SUBMENU_DELAY);

        VALUE_REGISTRY.put(CTRL_LEFT_VALUE, WidgetAttributes.CTRL_LEFT_CLICK);
        VALUE_REGISTRY.put(TAP_HOLD_VALUE, WidgetAttributes.TAP_HOLD);
        VALUE_REGISTRY.put(MENU_KEY_VALUE, WidgetAttributes.MENU_KEY_PRESS);
        VALUE_REGISTRY.put(RIGHT_CLICK_VALUE, WidgetAttributes.RIGHT_CLICK);
    }

    private static boolean notInitialized = true;

    private static void addAttributes(IAttributed attributed,
                                        Node node)
    {
        addAttributes(attributed, node, null);
    }

    private static void addAttributes(IAttributed attributed,
                                        Node node,
                                        Map<IAttributed, String> pendingAttrSets)
    {
        NamedNodeMap attributes = node.getAttributes();

        if (attributes != null) {
            int numAttributes = attributes.getLength();

            if (numAttributes > 0) {
                if (notInitialized) {
                    registerAttributes();
                }

                for (int i = 0; i < numAttributes; i++) {
                    Node attributeNode = attributes.item(i);

                    // Should never be null; sanity check
                    if (attributeNode != null) {
                        String attribute = attributeNode.getNodeName();
                        IAttributed.AttributeDefinition<?> attrDefn =
                            ATTRIBUTE_REGISTRY.get(attribute);

                        if (attrDefn != null) {
                            String attrName = attrDefn.attrName;
                            String attrNodeValue = attributeNode.getNodeValue();

                            if (WidgetAttributes.SELECTION_ATTR.equals(attrName))
                            {
                                // attrNodeValue is name of the selected widget,
                                // but the widget may not exist yet.
                                pendingAttrSets.put(attributed, attrNodeValue);
                            }
                            else if (VALUE_REGISTRY.containsKey(attrNodeValue))
                            {
                                Object attrValue =
                                    VALUE_REGISTRY.get(attrNodeValue);

                                if (! NullSafe.equals(attrValue,
                                                      attrDefn.defaultValue))
                                {
                                    attributed.setAttribute(attrName,
                                                            attrValue);
                                }
                            }
                            else {
                                // Assume string value (eg, APPENDED_TEXT_ATTR)
                                attributed.setAttribute(attrName,
                                                        attrNodeValue);
                            }
                        }
                    }
                }
            }
        }
    }

    private String directoryPath = "";
    private ObjectLoader objLoader = new ObjectLoader();

    // Maps Design to List of Demonstration
    private Map<Design, Collection<Demonstration>> designs =
        new LinkedHashMap<Design, Collection<Demonstration>>();
    
    private Set<AUndertaking> newUndertakings = 
        new LinkedHashSet<AUndertaking>();
    
    private Map<String, TaskGroup> taskGroups = 
        new HashMap<String, TaskGroup>();

    private List<String> failedObjectErrors = new ArrayList<String>();
    private List<String> failedImages = new ArrayList<String>();
    private List<String> klmWarnings = new ArrayList<String>();

    private CognitiveModelGenerator modelGenerator = null;
    private String dtdVersion = "0";
    private DefaultModelGeneratorState currentState = null;

    // Maps name to newly created Task
    private Map<String, List<Task>> createdTaskRegistry =
        new HashMap<String, List<Task>>();

    // Maps file name to image data (byte[])
    private Map<String, byte[]> imageRegistry = new HashMap<String, byte[]>();

    private Map<String, SimpleWidgetGroup> groupRegistry =
        new HashMap<String, SimpleWidgetGroup>();

    // Image data cache; maps image name to data (byte[])
    private Map<String, byte[]> cachedImages = new HashMap<String, byte[]>();

    private static ObjectLoader.IObjectLoader<Design> designLoader =
        Design.getImportLoader();
    private static ObjectLoader.IObjectLoader<Frame> frameLoader =
        Frame.getImportLoader();
    private static ObjectLoader.IObjectLoader<Widget> widgetLoader =
        Widget.getImportLoader();
    private static ObjectLoader.IObjectLoader<FrameElementGroup> groupLoader =
        FrameElementGroup.getImportLoader();
    
    private static boolean inImportFromXML = false;

    public boolean importXML(File inputFile,
                             TaskParent parent,
                             CognitiveModelGenerator modelGen)
                   throws IOException, SAXException, SecurityException
    {
        InputStream fis = null;
        fis = new FileInputStream(inputFile);
        boolean result = false;

        try {
            Reader input = new InputStreamReader(fis, "UTF-8");

            try {
                result = importXML(input,
                                   inputFile.getParent() + File.separator,
                                   parent,
                                   modelGen);

            }
            finally {
                input.close();
            }
        }
        finally {
            fis.close();
        }

        return result;
    }

    public boolean importXML(Reader input,
                             String imageDirPath,
                             TaskParent taskParent,
                             CognitiveModelGenerator modelGen)
        throws IOException, SAXException
    {
        modelGenerator = modelGen;

        // Create a Xerces DOM Parser
        DOMParser parser = new DOMParser();

        // Set the path for loading images
        directoryPath = imageDirPath;

        // Parse the Document and traverse the DOM
        parser.parse(new InputSource(input));


        Document document = parser.getDocument();
        parseFile(document, taskParent);

        if (failedImages.size() > 0) {
            String failedImageString = "Failed to load the following images:";
            Iterator<String> fIter = failedImages.iterator();

            while (fIter.hasNext()) {
                failedImageString +=
                    System.getProperty("file.separator") + fIter.next();
            }

            throw new GraphicsUtil.ImageException(failedImageString);
        }

        return true;
    }



    public Map<Design, Collection<Demonstration>> getDesigns()
    {
        return designs;
    }
    
    public Set<AUndertaking> getNewUndertakings() 
    {
        return newUndertakings;
    }

    public List<String> getObjectFailures()
    {
        return failedObjectErrors;
    }

    public List<String> getGenerationWarnings()
    {
        return klmWarnings;
    }

    private String getAttributeValue(Node node, String attr)
    {
        NamedNodeMap attributes = node.getAttributes();

        if (attributes != null) {
            Node attributeNode = attributes.getNamedItem(attr);

            if (attributeNode != null) {
                return attributeNode.getNodeValue();
            }
        }

        return null;
    }

    private String getElementText(Node node)
    {
        if (node.getFirstChild() != null) {
            return node.getFirstChild().getNodeValue().trim();
        }

        return "";
    }

    /**
     * Imports an XML file containing a design
     * and possibly a demonstration
     * @param node
     */
    private void parseFile(Node node, TaskParent taskParent)
        throws IOException
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            if (node.getNodeName().equalsIgnoreCase(COGTOOL_IMPORT_ELT)) {
                dtdVersion = getAttributeValue(node, VERSION_ATTR);

                for (int i = 0; i < children.getLength(); i++) {
                    Node child = children.item(i);
                    String nodeName = child.getNodeName();
                    if (nodeName.equalsIgnoreCase(TASK_GROUP_ELT)) {
                        parseTaskGroup(child, null);
                    } else if (nodeName.equalsIgnoreCase(TASK_ELT)) {
                        parseTask(child, taskParent);
                    }
                }

                for (int i = 0; i < children.getLength(); i++) {
                    Node child = children.item(i);
                    String nodeName = child.getNodeName();
                    if (nodeName.equalsIgnoreCase(DESIGN_ELT)) {
                        parseDesign(child, taskParent);
                    }
                }
            }
            else {
                for (int i = 0; i < children.getLength(); i++) {
                    parseFile(children.item(i), taskParent);
                }
            }
        }
    }
    
    private void parseTaskGroup(Node node, TaskParent taskParent) {
        TaskGroup taskGroup = 
            new TaskGroup(getAttributeValue(node, NAME_ATTR),
                          GroupNature.fromString(
                             getAttributeValue(node, GROUP_NATURE_ATTR)));
        if (taskParent != null) {
            taskParent.addUndertaking(taskGroup);
        } else {
            newUndertakings.add(taskGroup);
        }
        taskGroups.put(getAttributeValue(node, TASK_GROUP_ID_ATTR), taskGroup);
        NodeList children = node.getChildNodes();
        for (int i = 0; i < children.getLength(); ++i) {
            Node child = children.item(i);
            String childName = child.getNodeName();
            if (childName.equalsIgnoreCase(TASK_GROUP_ELT)) {
                parseTaskGroup(child, taskGroup);
            } else if (childName.equalsIgnoreCase(TASK_ELT)) {
                parseTask(child, taskGroup);
            }
        }
    }

    private void parseTask(Node node, TaskParent taskParent) {
        getTask(getAttributeValue(node, NAME_ATTR), null, taskParent);
    }

    /**
     * Imports a design
     * @param node
     */
    private void parseDesign(Node node, TaskParent taskParent)
        throws IOException
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            Design design = designLoader.createObject();
            List<Demonstration> demonstrations =
                new ArrayList<Demonstration>();

            designLoader.set(design,
                             Design.nameVAR,
                             getAttributeValue(node, NAME_ATTR));
            addAttributes(design, node);

            @SuppressWarnings("unchecked")
            Collection<DeviceType> deviceTypes = (Collection<DeviceType>)
                designLoader.createCollection(design, Design.deviceTypesVAR, 1);
            Collection<?> frames =
                designLoader.createCollection(design, Design.framesVAR, 1);


            ObjectLoader.IAggregateLoader deviceTypesLoader =
                designLoader.getLoader(Design.deviceTypesVAR);

            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(DEVICE_ELT)) {
                    String device = getElementText(child);

                    if (device.equalsIgnoreCase(KEYBOARD_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Keyboard);
                    }
                    else if (device.equalsIgnoreCase(MOUSE_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Mouse);
                    }
                    else if (device.equalsIgnoreCase(TOUCHSCREEN_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Touchscreen);
                    }
                    else if (device.equalsIgnoreCase(MICROPHONE_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Voice);
                    }
                    else if (device.equalsIgnoreCase(DISPLAY_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Display);
                    }
                    else if (device.equalsIgnoreCase(SPEAKER_DEVICE)) {
                        deviceTypesLoader.addToCollection(objLoader,
                                                          deviceTypes,
                                                          DeviceType.Speaker);
                    }
                    else {
                        // "unknown device"
                        failedObjectErrors.add("Unknown Design device: "
                                                        + device);
                    }
                }
                else if (nodeName.equalsIgnoreCase(FRAME_ELT)) {
                    parseFrame(design, child);
                    // No need to add frame to the design here; done already!
                }
                else if (nodeName.equalsIgnoreCase(DEMONSTRATION_ELT)) {
                    Demonstration demo = parseDemonstration(design, 
                                                            child,
                                                            taskParent);

                    if (demo != null) {
                        demonstrations.add(demo);
                    }
                }
            }

            if ((design.getName() == null) ||
                (deviceTypes.size() == 0) ||
                (frames.size() == 0))
            {
                throw new ImportFailedException("No design found");
            }

            designs.put(design, demonstrations);
        }
    }




    private byte[] loadImage(String relativePath)
        throws IOException
    {
        byte[] loadedImageData = imageRegistry.get(relativePath);

        if (loadedImageData == null) {
            loadedImageData =
                GraphicsUtil.loadImageFromFile(directoryPath
                                                               + relativePath);

            if (loadedImageData != null) {
                imageRegistry.put(relativePath, loadedImageData);
            }
        }

        return loadedImageData;
    }

    /**
     * Imports a frame
     * @param node
     */
    private Frame parseFrame(Design design, Node node)
        throws IOException
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            String frameName = getAttributeValue(node, NAME_ATTR);

            if ((frameName == null) || frameName.equals("")) {
                failedObjectErrors.add("Cannot create a frame with an empty name.");
                return null;
            }

            // This adds the created frame to the design
            Frame frame = getFrame(design, frameName);
            addAttributes(frame, node);




            Frame.setFrameDevices(frame, design.getDeviceTypes());

            TransitionSource keyboardDevice =
                frame.getInputDevice(DeviceType.Keyboard);
            TransitionSource voiceDevice =
                frame.getInputDevice(DeviceType.Voice);

            // Some widgets have parents; so as not to require that
            // all widgets of a frame occur in a particular order, we must
            // resolve the parent names after all widgets have been parsed.
            // Maps the child widget to the name of its parent
            Map<ChildWidget, String> pendingParentSets =
                new LinkedHashMap<ChildWidget, String>();

            // Some attributes refer to widget names; must resolve these
            // after all widgets have been created.
            // Currently, the only such attribute that applies to widgets
            // is WidgetAttributes.SELECTION_ATTR
            // Maps the attributed object to the widget name that is
            // the value of the WidgetAttributes.SELECTION_ATTR attribute
            Map<IAttributed, String> pendingAttrSets =
                new HashMap<IAttributed, String>();

            // Some element groups may be referenced as members of other
            // groups before being defined; this map will hold them
            Map<String, FrameElementGroup> pendingGrps =
                new HashMap<String, FrameElementGroup>();

            // Some remote labels may not be defined before they're referenced
            // so keep track of those cases.  Maps the owner object to
            // the name of the remote label
            Map<FrameElement, String> pendingRemoteLabels =
                new HashMap<FrameElement, String>();

            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(BKG_IMAGE_PATH_ELT)) {
                    String backgroundImagePath = getElementText(child);
                    byte[] image = loadImage(backgroundImagePath);

                    if (image != null) {
                        frameLoader.set(frame, Frame.backgroundVAR, image);
                        frame.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                           backgroundImagePath);
                    }
                }
                else if (nodeName.equalsIgnoreCase(BKG_IMAGE_DATA_ELT)) {
                    String backgroundImageData = getElementText(child);
                    String imageName = getAttributeValue(child, NAME_ATTR);
                    byte[] image = null;

                    if (backgroundImageData != "") {
                        image = Base64.decode(backgroundImageData);

                        if ((imageName != null) && ! imageName.equals("")) {
                            cachedImages.put(imageName, image);
                        }
                    }
                    else if ((imageName != null) && ! imageName.equals("")) {
                        // If imageName specified but there is no data, trust and
                        // try to find the last image data associated with that
                        // name in the cache.
                        image = cachedImages.get(imageName);
                    }

                    if (image != null) {
                        frameLoader.set(frame, Frame.backgroundVAR, image);

                        if ((imageName != null) && ! imageName.equals("")) {
                            frame.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                               imageName);
                        }
                    }
                }
                else if (nodeName.equalsIgnoreCase(ORIGIN_ELT)) {
                    double x =
                        Double.parseDouble(getAttributeValue(child, X_ATTR));
                    double y =
                        Double.parseDouble(getAttributeValue(child, Y_ATTR));
                    DoublePoint origin = new DoublePoint(x, y);

                    frameLoader.set(frame, Frame.originVAR, origin);
                }
                else if (nodeName.equalsIgnoreCase(SPEAKER_TEXT_ELT)) {
                    frameLoader.set(frame,
                                    Frame.speakerTextVAR,
                                    getElementText(child));
                }
                else if (nodeName.equalsIgnoreCase(LISTEN_TIME_SECS_ELT)) {
                    frameLoader.set(frame,
                                    Frame.listenTimeVAR,
                                    Double.parseDouble(getElementText(child)));
                }
                else if (nodeName.equalsIgnoreCase(WIDGET_ELT)) {
                    IWidget w = parseWidget(design,
                                            frame,
                                            pendingParentSets,
                                            pendingAttrSets,
                                            pendingRemoteLabels,
                                            child);

                    if (w != null) {
                        frame.addWidget(w);
                    }
                    else
                    {
                        w = new Widget(null, WidgetType.Noninteractive);
                        Image wImage = GraphicsUtil.getImageFromResource("edu/cmu/cs/hcii/cogtool/resources/warning.jpg");
                        //w.setImage(wImage.getBytes());
                        frame.addWidget(w);
                    }
                }
                else if (nodeName.equalsIgnoreCase(ELTGROUP_ELT)) {
                    FrameElementGroup g =
                        parseEltGroup(design, frame, pendingGrps, child);

                    if (g != null) {
                        String eltGrpName = g.getName();

                        pendingGrps.remove(eltGrpName);

                        eltGrpName =
                            NamedObjectUtil.makeNameUnique(eltGrpName,
                                                           frame.getEltGroups());
                        g.setName(eltGrpName);
                        frame.addEltGroup(g);
                    }
                }
                else if (nodeName.equalsIgnoreCase(KEYBOARD_TRANSITIONS_ELT)) {
                    if (keyboardDevice != null) {
                        parseTransitions(design, keyboardDevice, child);
                    }
                    else {
                        failedObjectErrors.add("Keyboard transitions require that Design have a Keyboard device");
                    }
                }
                else if (nodeName.equalsIgnoreCase(VOICE_TRANSITIONS_ELT)) {
                    if (voiceDevice != null) {
                        parseTransitions(design, voiceDevice, child);
                    }
                    else {
                        failedObjectErrors.add("Voice transitions require that Design have a Voice device");
                    }
                }
            }

            if (frame.getName() != null) {
                // Handle any forward references for remote labels
                Iterator<Map.Entry<FrameElement, String>> labelRefs =
                    pendingRemoteLabels.entrySet().iterator();

                while (labelRefs.hasNext()) {
                    Map.Entry<FrameElement, String> labelRef =
                        labelRefs.next();

                    setRemoteLabel(frame,
                                   labelRef.getValue(),
                                   labelRef.getKey(),
                                   null);
                }

                // If any "pending" element groups still exist, then there
                // is an error -- an element group that didn't exist!
                Iterator<FrameElementGroup> missingGrps =
                    pendingGrps.values().iterator();
                StringBuilder errorMsg = new StringBuilder();

                while (missingGrps.hasNext()) {
                    FrameElementGroup missingGrp = missingGrps.next();

                    errorMsg.append("Missing widget or group, named: ");
                    errorMsg.append(missingGrp.getName());
                    errorMsg.append(" as member of the following groups: ");

                    Iterator<FrameElementGroup> inGrps =
                        missingGrp.getEltGroups().iterator();
                    String separator = "";

                    while (inGrps.hasNext()) {
                        errorMsg.append(separator + inGrps.next().getName());
                        separator = ", ";
                    }

                    failedObjectErrors.add(errorMsg.toString());
                    errorMsg.delete(0, errorMsg.length());
                }

                Iterator<Map.Entry<ChildWidget, String>> childToParentSet =
                    pendingParentSets.entrySet().iterator();

                // Now that all widgets have been created, set the parent-child
                // relationships
                while (childToParentSet.hasNext()) {
                    Map.Entry<ChildWidget, String> childToParent =
                        childToParentSet.next();
                    String parentName = childToParent.getValue();

                    if (! "".equals(parentName)) {
                        ChildWidget child = childToParent.getKey();
                        AParentWidget parent =
                            (AParentWidget) frame.getWidget(parentName);

                        parent.addItem(child);
                        child.setParent(parent);
                    }
                }

                Iterator<Map.Entry<IAttributed, String>> selnAttrToSet =
                    pendingAttrSets.entrySet().iterator();

                // Now that all widgets have been created, set the attributes
                // that used widget names as values.
                while (selnAttrToSet.hasNext()) {
                    Map.Entry<IAttributed, String> selnAttr =
                        selnAttrToSet.next();
                    String widgetName = selnAttr.getValue();
                    IWidget attrValue =
                        "".equals(widgetName) ? null
                                              : frame.getWidget(widgetName);

                    // At the moment, all occurrences that use pendingAttrSets
                    // are instances of PullDownHeader for
                    // WidgetAttributes.SELECTION_ATTR
                    selnAttr.getKey().setAttribute(WidgetAttributes.SELECTION_ATTR,
                                                   attrValue);
                }

                return frame;
            }
        }

        return null;
    } // parseFrame


    /** Helper function used by parseFrame.
     * This method is used to return the corresponding Frame object for a
     * given frame name
     *
     * @ param  frameName the specified frame name
     */
    private Frame getFrame(Design design, String frameName)
    {
        if ((frameName != null) &&
            ! frameName.equals("") &&
            ! frameName.equals(FINAL_FRAME_NAME))
        {
            // Fail-fast -- right now, we have only one implementation of
            // Frame, so this cannot fail.
            Frame frame = design.getFrame(frameName);

            if (frame == null) {
                frame = frameLoader.createObject();
                frame.setName(frameName);
                design.addFrame(frame);
            }

            return frame;
        }

        return null;
    }

    private WidgetType getWidgetType(String widgetType)
    {
        if ((widgetType == null) || // TODO: note error, return null on this case?
            widgetType.equalsIgnoreCase(BUTTON_WIDGETTYPE))
        {
            return WidgetType.Button;
        }
        if (widgetType.equalsIgnoreCase(LINK_WIDGETTYPE)) {
            return WidgetType.Link;
        }
        if (widgetType.equalsIgnoreCase(CHECKBOX_WIDGETTYPE)) {
            return WidgetType.Check;
        }
        if (widgetType.equalsIgnoreCase(RADIO_WIDGETTYPE)) {
            return WidgetType.Radio;
        }
        if (widgetType.equalsIgnoreCase(TEXTBOX_WIDGETTYPE)) {
            return WidgetType.TextBox;
        }
        if (widgetType.equalsIgnoreCase(TEXT_WIDGETTYPE)) {
            return WidgetType.Text;
        }
        if (widgetType.equalsIgnoreCase(PULLDOWNLIST_WIDGETTYPE)) {
            return WidgetType.PullDownList;
        }
        if (widgetType.equalsIgnoreCase(PULLDOWNITEM_WIDGETTYPE)) {
            return WidgetType.PullDownItem;
        }
        if (widgetType.equalsIgnoreCase(LISTBOXITEM_WIDGETTYPE)) {
            return WidgetType.ListBoxItem;
        }
        if (widgetType.equalsIgnoreCase(CONTEXTMENU_WIDGETTYPE)) {
            return WidgetType.ContextMenu;
        }
        if (widgetType.equalsIgnoreCase(MENUHEADER_WIDGETTYPE)) {
            return WidgetType.Menu;
        }
        if (widgetType.equalsIgnoreCase(SUBMENU_WIDGETTYPE)) {
            return WidgetType.Submenu;
        }
        if (widgetType.equalsIgnoreCase(MENUITEM_WIDGETTYPE)) {
            return WidgetType.MenuItem;
        }
        if (widgetType.equalsIgnoreCase(GRAFFITI_WIDGETTYPE)) {
            return WidgetType.Graffiti;
        }
        if (widgetType.equalsIgnoreCase(NONINTERACTIVE_WIDGETTYPE)) {
            return WidgetType.Noninteractive;
        }

        failedObjectErrors.add("Unknown widget type: " + widgetType);
        return null;
    }




    /**
     * Imports a widget
     * @param node
     */
    private IWidget parseWidget(Design design,
                                  Frame frame,
                                  Map<ChildWidget, String> pendingParentSets,
                                  Map<IAttributed, String> pendingAttrSets,
                                  Map<FrameElement, String> pendingRemoteLabels,
                                  Node node)
        throws IOException
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            String isStandardValue = getAttributeValue(node, IS_STANDARD_ATTR);
            boolean isStandard;

            if (isStandardValue != null) {
                Boolean isStandardAttrValue =
                    (Boolean) VALUE_REGISTRY.get(isStandardValue);

                if (isStandardAttrValue != null) {
                    isStandard = isStandardAttrValue.booleanValue();
                }
                else {
                    isStandard = false;
                }
            }
            else if (CURRENT_VERSION.equals(dtdVersion)) {
                isStandard = false;
            }
            else {
                isStandard = true;
            }

            WidgetType widgetType =
                getWidgetType(getAttributeValue(node, TYPE_ATTR));

            Widget widget = null;

            if (widgetType != null) {
                if (isStandard) {
                    if (widgetType == WidgetType.ContextMenu) {
                        // Using this constructor sets up childItems and
                        // childLocation to the only value currently in use
                        widget = new ContextMenu(ContextMenu.FOR_DUPLICATION);
                    }
                    else if (widgetType == WidgetType.Menu) {
                        // Using this constructor sets up childItems and
                        // childLocation to the only value currently in use
                        widget = new MenuHeader(MenuHeader.FOR_DUPLICATION);

                        // Must also find its "anonymous" parentGroup
                        String grpName = getAttributeValue(node, GROUP_ATTR);

                        if ((grpName != null) && ! "".equals(grpName)) {
                            SimpleWidgetGroup menuHdrGroup =
                                groupRegistry.get(grpName);

                            if (menuHdrGroup == null) {
                                menuHdrGroup =
                                    new SimpleWidgetGroup(SimpleWidgetGroup.HORIZONTAL);
                                menuHdrGroup.setName(grpName);
                                groupRegistry.put(grpName, menuHdrGroup);
                            }

                            menuHdrGroup.add(widget);
                        }
                    }
                    else if ((widgetType == WidgetType.Submenu) ||
                             (widgetType == WidgetType.MenuItem))
                    {
                        // Must also assign its parent, which must come earlier
                        String parentName =
                            getAttributeValue(node, PARENT_ATTR);

                        if (parentName != null) {
                            MenuItem menuItem = new MenuItem();

                            // If a submenu, set up childItems and childLocation
                            menuItem.setSubmenu(widgetType == WidgetType.Submenu);

                            pendingParentSets.put(menuItem, parentName);

                            widget = menuItem;
                        }
                        else {
                            widget = widgetLoader.createObject();
                        }
                    }
                    else if (widgetType == WidgetType.PullDownList) {
                        // Using this constructor sets up childItems and
                        // childLocation to the only value currently in use
                        widget =
                            new PullDownHeader(PullDownHeader.FOR_DUPLICATION);
                    }
                    else if (widgetType == WidgetType.PullDownItem) {
                        // Must assign its parent, which must come earlier
                        String parentName =
                            getAttributeValue(node, PARENT_ATTR);

                        if (parentName != null) {
                            PullDownItem pullDownItem = new PullDownItem();

                            pendingParentSets.put(pullDownItem, parentName);

                            widget = pullDownItem;
                        }
                        else {
                            widget = widgetLoader.createObject();
                        }
                    }
                    else if (widgetType == WidgetType.ListBoxItem) {
                        widget = new ListItem();

                        // Must also find its "anonymous" parentGroup
                        String grpName = getAttributeValue(node, GROUP_ATTR);

                        if ((grpName != null) && ! "".equals(grpName)) {
                            SimpleWidgetGroup listItemGroup =
                                groupRegistry.get(grpName);

                            if (listItemGroup == null) {
                                listItemGroup =
                                    new SimpleWidgetGroup(SimpleWidgetGroup.VERTICAL);
                                listItemGroup.setName(grpName);
                                groupRegistry.put(grpName, listItemGroup);
                            }

                            listItemGroup.add(widget);
                        }
                    }
                    else if ((widgetType == WidgetType.Radio) ||
                             (widgetType == WidgetType.Check))
                    {
                        GridButton gridWidget;

                        if (widgetType == WidgetType.Radio) {
                            gridWidget = new RadioButton();
                        }
                        else {
                            gridWidget = new CheckBox();
                        }

                        // Fetch the horizontal/vertical spacing attributes
                        double horizontal =
                            Double.parseDouble(getAttributeValue(node, X_ATTR));
                        double vertical =
                            Double.parseDouble(getAttributeValue(node, Y_ATTR));
                        gridWidget.setHorizSpace(horizontal);
                        gridWidget.setVertSpace(vertical);

                        // Must also find its "anonymous" parentGroup
                        String grpName = getAttributeValue(node, GROUP_ATTR);

                        if ((grpName != null) && ! "".equals(grpName)) {
                            SimpleWidgetGroup gridWidgetGrp =
                                groupRegistry.get(grpName);

                            if (gridWidgetGrp == null) {
                                if (widgetType == WidgetType.Radio) {
                                    gridWidgetGrp = new RadioButtonGroup();
                                }
                                else {
                                    gridWidgetGrp = new GridButtonGroup();
                                }

                                gridWidgetGrp.setName(grpName);
                                groupRegistry.put(grpName, gridWidgetGrp);
                            }

                            gridWidgetGrp.add(gridWidget);
                        }

                        widget = gridWidget;
                    }
                    else {
                        widget = widgetLoader.createObject();
                    }
                }
                else {
                    widget = widgetLoader.createObject();
                }

                widgetLoader.set(widget, Widget.widgetTypeVAR, widgetType);
            }
            else {
                widget = widgetLoader.createObject();
            }

            String widgetName = getAttributeValue(node, NAME_ATTR);

            widget.setName(widgetName);

            String remoteLabelAttr = getAttributeValue(node, REMOTE_LABEL_ATTR);

            if ((remoteLabelAttr != null) && ! remoteLabelAttr.equals("")) {
                FrameElement remoteLabelOwner = widget.getRemoteLabelOwner();

                if (remoteLabelOwner == null) {
                    failedObjectErrors.add("Attempting to set a remote label on the wrong widget type for: "
                                                + widgetName);
                }
                else {
                    IWidget existingLabel =
                        (IWidget)
                            remoteLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

                    if (existingLabel == null) {
                        setRemoteLabel(frame, remoteLabelAttr, remoteLabelOwner,
                                       pendingRemoteLabels);
                    }
                    else if (! remoteLabelAttr.equals(existingLabel.getName()))
                    {
                        failedObjectErrors.add("Attempting to set a second remote label for the widget: "
                                                    + widgetName);
                    }
                }
            }

            // For the case that the widget is a RadioButton that
            // has an is-selected attribute, the setting of that
            // attribute in this call will, per the override of setAttribute()
            // in RadioButton, set the proper selected-widget attribute
            // on the containing widget group.
            addAttributes(widget, node, pendingAttrSets);

            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(DISPLAY_LABEL_ELT)) {
                    widgetLoader.set(widget,
                                     Widget.titleVAR,
                                     getElementText(child));
                }
                else if (nodeName.equalsIgnoreCase(AUX_TEXT_ELT)) {
                    widgetLoader.set(widget,
                                     Widget.auxTextVAR,
                                     getElementText(child));
                }
                else if (nodeName.equalsIgnoreCase(EXTENT_ELT)) {
                    double x =
                        Double.parseDouble(getAttributeValue(child, X_ATTR));
                    double y =
                        Double.parseDouble(getAttributeValue(child, Y_ATTR));
                    double width =
                        Double.parseDouble(getAttributeValue(child,
                                                             WIDTH_ATTR));
                    double height =
                        Double.parseDouble(getAttributeValue(child,
                                                             HEIGHT_ATTR));
                    DoubleRectangle extent =
                        new DoubleRectangle(x, y, width, height);

                    AShape widgetShape = null;

                    String shapeType = getAttributeValue(node, SHAPE_ATTR);

                    if ((shapeType == null) ||
                        shapeType.equalsIgnoreCase(RECTANGLE_SHAPE))
                    {
                        widgetShape = new ShapeRectangle(extent);
                    }
                    else if (shapeType.equalsIgnoreCase(ELLIPSE_SHAPE)) {
                        widgetShape = new ShapeOval(extent);
                    }
                    else if (shapeType.equalsIgnoreCase(ROUND_RECT_SHAPE)) {
                        widgetShape = new ShapeRoundedRectangle(extent);
                    }
                    else {
                        failedObjectErrors.add("Unknown shape type: "
                                                        + shapeType);
                        widgetShape = new ShapeRectangle(extent);
                    }

                    if (widgetShape != null) {
                        widgetLoader.set(widget, Widget.shapeVAR, widgetShape);
                    }
                }
                else if (nodeName.equalsIgnoreCase(BKG_IMAGE_PATH_ELT)) {
                    String backgroundImagePath = getElementText(child);
                    byte[] image = loadImage(backgroundImagePath);

                    if (image != null) {
                        widgetLoader.set(widget, Widget.widgetImageVAR, image);
                        widget.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                            backgroundImagePath);
                    }
                }
                else if (nodeName.equalsIgnoreCase(BKG_IMAGE_DATA_ELT)) {
                    String backgroundImageData = getElementText(child);

                    if (backgroundImageData != "") {
                        byte[] image = Base64.decode(backgroundImageData);

                        widgetLoader.set(widget, Widget.widgetImageVAR, image);

                        String imageName = getAttributeValue(child, NAME_ATTR);

                        if ((imageName != null) && ! imageName.equals("")) {
                            widget.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                                imageName);
                        }
                    }
                }
                else if (nodeName.equalsIgnoreCase(TRANSITION_ELT)) {
                    Transition t = parseTransition(design, widget, child);

                    if (t != null) {
                        // TODO Because of the setting of parents is deferred,
                        //      this is executed when there may not be any
                        //      parents. This results in the curves not being
                        //      assigned to the transitions, and is a bug,
                        //      ticket #775. The fix is probably to also defer
                        //      adding transitions until later?
                        widget.addTransition(t);
                    }
                }
            }

            if ((widget.getName() != null) &&
                (widget.getWidgetType() != null) &&
                (widget.getShape() != null))
            {
                return widget;
            }
        }

        return null;
    } // parseWidget

    private void setRemoteLabel(Frame frame,
                                  String remoteLabelName,
                                  FrameElement labelOwner,
                                  Map<FrameElement, String> pendingRemoteLabels)
    {
        IWidget remoteLabel = frame.getWidget(remoteLabelName);

        if (remoteLabel == null) {
            if (pendingRemoteLabels == null) {
                failedObjectErrors.add("Missing remote label widget ("
                                            + remoteLabelName
                                            + ") for element group: "
                                            + labelOwner.getName());
            }
            else {
                pendingRemoteLabels.put(labelOwner, remoteLabelName);
            }
        }
        else if (! remoteLabel.getWidgetType().canBeARemoteLabel()) {
            failedObjectErrors.add("Disallowed type for remote label: "
                                        + remoteLabelName);
        }
        else if (remoteLabel instanceof GridButton) {
            failedObjectErrors.add("Disallowed class for remote label: "
                                        + remoteLabelName);
        }
        else {
            labelOwner.setAttribute(WidgetAttributes.REMOTE_LABEL_ATTR,
                                    remoteLabel);
            remoteLabel.setAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR,
                                     labelOwner);
            remoteLabel.setAttribute(WidgetAttributes.IS_STANDARD_ATTR,
                                     WidgetAttributes.IS_CUSTOM);
        }
    }

    private static final String DEFAULT_GROUP_PREFIX =
        L10N.get("ICT.GroupNamePrefix", "Group");

    private FrameElementGroup parseEltGroup(Design design,
                                               Frame frame,
                                               Map<String,
                                                   FrameElementGroup> pendingGrps,
                                               Node node)
    {
        String nameAttr = getAttributeValue(node, NAME_ATTR);
        if (nameAttr == null) {
            nameAttr = DEFAULT_GROUP_PREFIX + " [1]";
        }
        FrameElementGroup eltGroup = pendingGrps.get(nameAttr);
        if (eltGroup == null) {
            eltGroup = new FrameElementGroup();
        }
        eltGroup.setName(nameAttr);
        String remoteLabelAttr = getAttributeValue(node, REMOTE_LABEL_ATTR);
        if ((remoteLabelAttr != null) && ! remoteLabelAttr.equals("")) {
            setRemoteLabel(frame, remoteLabelAttr, eltGroup, null);
        }
        addAttributes(eltGroup, node);
        NodeList children = node.getChildNodes();
        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();
                if (nodeName.equalsIgnoreCase(ELTNAME_ELT)) {
                    String eltName = getElementText(child);
                    IWidget widget = frame.getWidget(eltName);
                    FrameElement elt = null;
                    if (widget != null) {
                        elt = widget.getRootElement();
                    }
                    else {
                        SimpleWidgetGroup anonymousParentGroup =
                            groupRegistry.get(eltName);
                        if (anonymousParentGroup != null) {
                            elt = anonymousParentGroup;
                        }
                        else {
                            FrameElementGroup grp = frame.getEltGroup(eltName);

                            // If not found, assume the group will show up later!
                            if (grp == null) {
                                grp = pendingGrps.get(eltName);
                                if (grp == null) {
                                    grp = new FrameElementGroup();
                                    grp.setName(eltName);
                                    pendingGrps.put(eltName, grp);
                                }
                            }
                            elt = grp;
                        }
                    }
                    eltGroup.add(elt);
                } else if (nodeName.equalsIgnoreCase(AUX_TEXT_ELT)) {
                    groupLoader.set(eltGroup,
                                     Widget.auxTextVAR,
                                     getElementText(child));
                }

            }
        }

        return eltGroup;
    }

    /**
     * Imports a transition
     * @param node
     */
    private void parseTransitions(Design design,
                                    TransitionSource source,
                                    Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                if (child.getNodeName().equalsIgnoreCase(TRANSITION_ELT)) {
                    Transition t = parseTransition(design, source, child);

                    if (t != null) {
                        source.addTransition(t);
                    }
                }
            }
        }
    }

    private Transition parseTransition(Design design,
                                          TransitionSource source,
                                          Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            String frameName = getAttributeValue(node, DEST_FRAME_NAME_ATTR);
            Frame destination =
                (frameName != null) ? getFrame(design, frameName) : null;

            if (destination != null) {
                AAction action = null;

                for (int i = 0; i < children.getLength(); i++) {
                    Node child = children.item(i);
                    String nodeName = child.getNodeName();

                    if (nodeName.equalsIgnoreCase(ACTION_ELT)) {
                        action = parseAction(child);
                    }
                }

                if (action != null) {
                    Transition t = new Transition(source, destination, action);

                    String delayDurationAttr =
                        getAttributeValue(node, DURATION_ATTR);

                    if (delayDurationAttr != null) {
                        t.setDelayInfo(Double.parseDouble(delayDurationAttr),
                                       getAttributeValue(node, DELAY_LABEL_ATTR));
                    }

                    addAttributes(t, node);

                    return t;
                }
            }
        }

        return null;
    } // parseTransition

    /**
     * Imports an action
     * @param node
     */
    private AAction parseAction(Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(MOUSE_ACTION_ELT)) {
                    return parseMouseAction(child);
                }
                if (nodeName.equalsIgnoreCase(TOUCHSCREEN_ACTION_ELT)) {
                    return parseTouchscreenAction(child);
                }
                if (nodeName.equalsIgnoreCase(GRAFFITI_ACTION_ELT)) {
                    return parseGraffitiAction(child);
                }
                if (nodeName.equalsIgnoreCase(KEYBOARD_ACTION_ELT)) {
                    return parseKeyboardAction(child);
                }
                if (nodeName.equalsIgnoreCase(VOICE_ACTION_ELT)) {
                    return parseVoiceAction(child);
                }
            }
        }

        return null;
    }

    private AAction parseMouseAction(Node node)
    {
        int modifiers = AAction.NONE;

        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                if (child.getNodeName().equalsIgnoreCase(KBD_MODIFIER_ELT)) {
                    modifiers =
                        addKeyboardModifier(modifiers, getElementText(child));
                }
            }
        }

        String buttonAttr = getAttributeValue(node, BUTTON_ATTR);
        String actionAttr = getAttributeValue(node, ACTION_ATTR);

        MouseButtonState button = null;
        MousePressType mouseAction = null;

        if ((actionAttr == null) ||
            actionAttr.equalsIgnoreCase(DOWNUP_ACTION))
        {
            mouseAction = MousePressType.Click;
        }
        else if (actionAttr.equalsIgnoreCase(DOUBLE_ACTION)) {
            mouseAction = MousePressType.Double;
        }
        else if (actionAttr.equalsIgnoreCase(TRIPLE_ACTION)) {
            mouseAction = MousePressType.Triple;
        }
        else if (actionAttr.equalsIgnoreCase(DOWN_ACTION)) {
            mouseAction = MousePressType.Down;
        }
        else if (actionAttr.equalsIgnoreCase(UP_ACTION)) {
            mouseAction = MousePressType.Up;
        }
        else if (actionAttr.equalsIgnoreCase(HOVER_ACTION)) {
            mouseAction = MousePressType.Hover;
        }
        else {
            failedObjectErrors.add("Unknown button action: " + actionAttr);
            mouseAction = MousePressType.Click;
        }

        if (buttonAttr == null) {
            if (mouseAction != MousePressType.Hover) {
                button = MouseButtonState.Left;
            }
        }
        else if (buttonAttr.equalsIgnoreCase(LEFT_BUTTON)) {
            button = MouseButtonState.Left;
        }
        else if (buttonAttr.equalsIgnoreCase(MIDDLE_BUTTON)) {
            button = MouseButtonState.Middle;
        }
        else if (buttonAttr.equalsIgnoreCase(RIGHT_BUTTON)) {
            button = MouseButtonState.Right;
        }
        else {
            failedObjectErrors.add("Unknown button: " + buttonAttr);
            button = MouseButtonState.Left;
        }

        AAction action = new ButtonAction(button, mouseAction, modifiers);

        addAttributes(action, node);

        return action;
    }

    private AAction parseTouchscreenAction(Node node)
    {
        int modifiers = AAction.NONE;

        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                if (child.getNodeName().equalsIgnoreCase(KBD_MODIFIER_ELT)) {
                    modifiers =
                        addKeyboardModifier(modifiers, getElementText(child));
                }
            }
        }
        // TODO: currently, we have no place to put the modifiers; fix TapAction!

        String actionAttr = getAttributeValue(node, ACTION_ATTR);

        TapPressType tapAction = null;

        if ((actionAttr == null) ||
            actionAttr.equalsIgnoreCase(TAP_ACTION) ||
            actionAttr.equalsIgnoreCase(DOWNUP_ACTION))
        {
            tapAction = TapPressType.Tap;
        }
        else if (actionAttr.equalsIgnoreCase(DOUBLE_ACTION)) {
            tapAction = TapPressType.DoubleTap;
        }
        else if (actionAttr.equalsIgnoreCase(TRIPLE_ACTION)) {
            tapAction = TapPressType.TripleTap;
        }
        else if (actionAttr.equalsIgnoreCase(DOWN_ACTION)) {
            tapAction = TapPressType.Down;
        }
        else if (actionAttr.equalsIgnoreCase(UP_ACTION)) {
            tapAction = TapPressType.Up;
        }
        else if (actionAttr.equalsIgnoreCase(HOVER_ACTION)) {
            tapAction = TapPressType.Hover;
        }
        else {
            failedObjectErrors.add("Unknown tap action: " + actionAttr);
            tapAction = TapPressType.Tap;
        }

        AAction action = new TapAction(tapAction);

        addAttributes(action, node);

        return action;
    }

    private boolean isAttributeTRUE(String isAttr, boolean defaultsTRUE)
    {
        if (isAttr == null) {
            return defaultsTRUE;
        }

        return isAttr.equalsIgnoreCase("true") ||
               isAttr.equalsIgnoreCase("t") ||
               isAttr.equals("1");
    }

    private AAction parseGraffitiAction(Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                if (child.getNodeName().equalsIgnoreCase(GESTURES_ELT)) {
                    boolean isCommand =
                        isAttributeTRUE(getAttributeValue(node, IS_CMD_ATTR),
                                        true);

                    AAction action =
                        new GraffitiAction(getElementText(child), isCommand);

                    addAttributes(action, node);

                    return action;
                }
            }
        }

        return null;
    }

    private KeyPressType getPressType(String type)
    {
        if (type != null) {
            if (type.equalsIgnoreCase(PRESS_ACTION)) {
                return KeyPressType.Stroke;
            }
            if (type.equalsIgnoreCase(UP_ACTION)) {
                return KeyPressType.Up;
            }
            if (type.equalsIgnoreCase(DOWN_ACTION)) {
                return KeyPressType.Down;
            }

            failedObjectErrors.add("Unknown key press type: " + type);
        }

        return KeyPressType.Stroke;
    }

    private AAction parseKeyboardAction(Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            String text = null;
            int modifiers = AAction.NONE;
            KeyPressType pressType =
                getPressType(getAttributeValue(node, TYPE_ATTR));

            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(TEXT_ELT)) {
                    text = getElementText(child);
                }
                // No need to handle modifiers any more;
                // SPECIAL CHARACTERS ARE USED INSTEAD
            }

            if (text != null) {
                boolean isCommand =
                    isAttributeTRUE(getAttributeValue(node, IS_CMD_ATTR),
                                    true);

                if ((text.length() == 1) && isCommand) {
                    return new KeyAction(text.charAt(0), pressType, modifiers);
                }

                AAction action = new KeyAction(text, isCommand, modifiers);

                addAttributes(action, node);

                return action;
            }
        }

        return null;
    }

    private int addKeyboardModifier(int modifiers, String addModifier)
    {
        if (addModifier != null) {
            if (addModifier.equalsIgnoreCase(SHIFT_MODIFIER)) {
                return modifiers | AAction.SHIFT;
            }
            if (addModifier.equalsIgnoreCase(CTRL_MODIFIER)) {
                return modifiers | AAction.CTRL;
            }
            if (addModifier.equalsIgnoreCase(ALT_MODIFIER)) {
                return modifiers | AAction.ALT;
            }
            if (addModifier.equalsIgnoreCase(COMMAND_MODIFIER)) {
                return modifiers | AAction.COMMAND;
            }
            if (addModifier.equalsIgnoreCase(FUNCTION_MODIFIER)) {
                return modifiers | AAction.FUNCTION;
            }

            failedObjectErrors.add("Unknown keyboard modifier: "
                                            + addModifier);
        }
        return modifiers;
    }

    private AAction parseVoiceAction(Node node)
    {
        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                if (child.getNodeName().equalsIgnoreCase(TEXT_ELT)) {
                    boolean isCommand =
                        isAttributeTRUE(getAttributeValue(node, IS_CMD_ATTR),
                                        true);

                    AAction action =
                        new VoiceAction(getElementText(child), isCommand);

                    addAttributes(action, node);

                    return action;
                }
            }
        }

        return null;
    }

    private Task getTask(String taskName, 
                         String taskGroupID,
                         TaskParent parent)
    {
        if (taskName != null) {
            AUndertaking undertaking = null;
            if (taskGroupID == null) {
                undertaking = parent.getUndertaking(taskName);
            }
            if (undertaking == null) {
                List<Task> createdTasks =
                    createdTaskRegistry.get(taskName);
                if (taskGroupID != null) {
                    parent = taskGroups.get(taskGroupID);
                }
                if (createdTasks != null) {
                    for (Task t : createdTasks) {
                        TaskGroup tg = t.getTaskGroup();
                        if (tg == parent || 
                                (tg == null && taskGroupID == null)) {
                            return t;
                        }
                    }
                } else {
                    createdTasks = new ArrayList<Task>(1);
                    createdTaskRegistry.put(taskName, createdTasks);
                }
                Task ct = new Task(taskName);
                if (parent instanceof TaskGroup) {
                    parent.addUndertaking(ct);
                } else {
                    newUndertakings.add(ct);
                }
                createdTasks.add(ct);
                return ct;
            }
            if (undertaking instanceof Task) {
                return (Task) undertaking;
            }
        }
        return null;
    }
    
    /**
     * Imports a demonstration
     * @param node
     */
    private Demonstration parseDemonstration(Design design, 
                                               Node node,
                                               TaskParent taskParent)
    {
        if (design == null) {
            return null;
        }

        NodeList children = node.getChildNodes();

        if (children == null) {
            return null;
        }

        Task task = getTask(getAttributeValue(node, TASK_NAME_ATTR), 
                            getAttributeValue(node, TASK_GROUP_ID_ATTR),
                            taskParent);

        if (task == null) {
            failedObjectErrors.add("Cannot create demonstration without a valid task.");
            return null;
        }

        Frame startFrame =
            getFrame(design, getAttributeValue(node, START_FRAME_NAME_ATTR));

        if (startFrame == null) {
            failedObjectErrors.add("Cannot create a demonstration without a start frame");
            return null;
        }

        Frame currentFrame = startFrame;
        TaskApplication taskApp = new TaskApplication(task, design);
        Demonstration demo = taskApp.getDemonstration();

        demo.setStartFrame(startFrame);
        demo.setStartFrameChosen(true);

        Script genScript = new Script(demo, modelGenerator);
        taskApp.setScript(modelGenerator, genScript);
        // DemoStateManager.trackEdits, adding new tasks to the project,
        // etc. are the responsibility of the caller.

        String handAttr = getAttributeValue(node, HANDEDNESS_ATTR);
        boolean mouseHand =
            (handAttr == null) || handAttr.equalsIgnoreCase(RIGHT_HAND);

        demo.setMouseHand(mouseHand);

        addAttributes(demo, node);

        // Call this after setting the mouseHand!
        currentState = demo.getInitialState();

        // Actual location for mouse hand might change via child elements
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            String nodeName = child.getNodeName();

            if (nodeName.equalsIgnoreCase(START_RIGHT_POS_ELT)) {
                // Assume touchscreen is akin to mouse for now
                if (getElementText(child).equalsIgnoreCase(KEYBOARD_DEVICE)) {
                    currentState.setHandLocation(HandLocation.RIGHT_HAND,
                                                      HandLocation.OnKeyboard);
                }
                else {
                    currentState.setHandLocation(HandLocation.RIGHT_HAND,
                                                      HandLocation.OnMouse);
                }
            }
            else if (nodeName.equalsIgnoreCase(START_LEFT_POS_ELT))
            {
                // Assume touchscreen is akin to mouse for now
                if (getElementText(child).equalsIgnoreCase(KEYBOARD_DEVICE)) {
                    currentState.setHandLocation(HandLocation.LEFT_HAND,
                                                      HandLocation.OnKeyboard);
                }
                else {
                    currentState.setHandLocation(HandLocation.LEFT_HAND,
                                                      HandLocation.OnMouse);
                }
            }
            else if (nodeName.equalsIgnoreCase(START_MOUSE_LOC_ELT)) {
                String attrValue = getAttributeValue(child, X_ATTR);
                attrValue = getAttributeValue(child, Y_ATTR);
            }
            else if (nodeName.equalsIgnoreCase(START_EYE_LOC_ELT)) {
                String attrValue = getAttributeValue(child, X_ATTR);
                attrValue = getAttributeValue(child, Y_ATTR);
            }
            else if (nodeName.equalsIgnoreCase(DEMO_STEP_ELT)) {
                AScriptStep demoStep =
                    parseDemoStep(currentFrame, genScript, child);

                if (demoStep != null) {
                    addAttributes(demoStep, child);
                    demo.appendStep(demoStep);
                    currentFrame = demoStep.getDestinationFrame();
                }
            }
        }

        return demo;
    } // parseDemonstration

    private void generateAndAppend(AScriptStep step, Script genScript)
    {
        try {
            inImportFromXML = true;
            currentState =
                    modelGenerator.generateScriptSteps(step,
                                                       currentState,
                                                       klmWarnings,
                                                       genScript.getStepStates());
        } finally {
            inImportFromXML = false;
        }
    }
    
    public static boolean isInImportFromXML() {
        return inImportFromXML;
    }

    private AScriptStep buildActionStep(TransitionSource src,
                                          AAction action,
                                          Node node)
    {
        if (action != null) {
            if (src != null) {
                Transition transition = src.getTransition(action);

                if (transition != null) {
                    return new TransitionScriptStep(transition);
                }
            }

            ActionScriptStep step = new ActionScriptStep(action, src);

            String delayDurationAttr = getAttributeValue(node, DURATION_ATTR);
            double delayDuration = 0.0;

            if (delayDurationAttr != null) {
                delayDuration = Double.parseDouble(delayDurationAttr);
            }

            step.setDelay(delayDuration,
                          getAttributeValue(node, DELAY_LABEL_ATTR));

            return step;
        }

        return null;
    }

    private AScriptStep parseActionStep(TransitionSource src, Node child)
    {
        AAction action = parseAction(child);

        return buildActionStep(src, action, child);
    }

    private AScriptStep parseDemoStep(Frame currentFrame,
                                        Script genScript,
                                        Node node)
    {
        AScriptStep newStep = null;

        NodeList children = node.getChildNodes();

        if (children != null) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);
                String nodeName = child.getNodeName();

                if (nodeName.equalsIgnoreCase(ACTION_STEP_ELT)) {
                    String targetWidgetName =
                        getAttributeValue(child, TARGET_WIDGET_NAME_ATTR);

                    if (targetWidgetName != null) {
                        IWidget actionFocus =
                            currentFrame.getWidget(targetWidgetName);

                        newStep = parseActionStep(actionFocus, child);
                    }
                    else {
                        newStep = parseActionStep(null, child);
                    }
                }
                else if (nodeName.equalsIgnoreCase(KEYBOARD_STEP_ELT)) {
                    InputDevice kbDevice =
                        currentFrame.getInputDevice(DeviceType.Keyboard);

                    newStep = buildActionStep(kbDevice,
                                              parseKeyboardAction(child),
                                              child);
                }
                else if (nodeName.equalsIgnoreCase(VOICE_STEP_ELT)) {
                    InputDevice voiceDevice =
                        currentFrame.getInputDevice(DeviceType.Voice);

                    newStep = buildActionStep(voiceDevice,
                                              parseVoiceAction(child),
                                              child);
                }
                else if (nodeName.equalsIgnoreCase(THINK_STEP_ELT)) {
                    String duration =
                        getAttributeValue(child, DURATION_ATTR);
                    String thinkLabel =
                        getAttributeValue(child, THINK_LABEL_ATTR);

                    if (thinkLabel == null) {
                        thinkLabel = ThinkScriptStep.DEFAULT_THINK_LABEL;
                    }

                    if (duration != null) {
                        newStep =
                            new ThinkScriptStep(currentFrame,
                                                Double.parseDouble(duration),
                                                thinkLabel);
                    }
                }
                else if (nodeName.equalsIgnoreCase(SYS_DELAY_STEP_ELT)) {
                    String duration =
                        getAttributeValue(child, DURATION_ATTR);
                    String label =
                        getAttributeValue(child, DELAY_LABEL_ATTR);

                    if (label == null) {
                        label = DelayScriptStep.DEFAULT_DELAY_LABEL;
                    }

                    if (duration != null) {
                        newStep =
                            new DelayScriptStep(currentFrame,
                                                Double.parseDouble(duration),
                                                label);
                    }
                }
                else if (nodeName.equalsIgnoreCase(LOOK_AT_STEP_ELT)) {
                    String widgetName =
                        getAttributeValue(child, LOOKAT_WIDGET_NAME_ATTR);

                    if (widgetName != null) {
                        IWidget widget = currentFrame.getWidget(widgetName);

                        if (widget != null) {
                            newStep = new LookAtScriptStep(widget);
                        }
                    }
                }

                if (newStep != null) {
                    generateAndAppend(newStep, genScript);

                    return newStep;
                }
            }
        }

        return null;
    }
}
