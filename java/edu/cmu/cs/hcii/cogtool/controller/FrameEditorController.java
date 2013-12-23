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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.controller.DemoStateManager.IDesignUndoableEdit;
import edu.cmu.cs.hcii.cogtool.model.AMenuWidget;
import edu.cmu.cs.hcii.cogtool.model.AParentWidget;
import edu.cmu.cs.hcii.cogtool.model.Association;
import edu.cmu.cs.hcii.cogtool.model.CheckBox;
import edu.cmu.cs.hcii.cogtool.model.ChildWidget;
import edu.cmu.cs.hcii.cogtool.model.ContextMenu;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.GridButton;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.ListItem;
import edu.cmu.cs.hcii.cogtool.model.MenuHeader;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.PullDownHeader;
import edu.cmu.cs.hcii.cogtool.model.PullDownItem;
import edu.cmu.cs.hcii.cogtool.model.RadioButton;
import edu.cmu.cs.hcii.cogtool.model.RadioButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.ShapeType;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.TraversableWidget;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorInteraction;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.EmptyIterator;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.RcvrClipboardException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.ReadOnlyList;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

//TODO: Use exit point in a mouse out, as a mouse drag.
//TODO: Use entry point in a mouse in, as a mouse drag.

/**
 * @author alexeiser
 * Controls the various functions for the frame view.
 *
 */
public class FrameEditorController extends ZoomableController
{
    /**
     * Frame editor UI for this controller.
     */
    private FrameEditorUI ui;

    /**
     * Demonstration state manager (manages invalidating/obsoleting
     * state changes to demonstration and script steps based on
     * frame and widget edits).
     */
    private DemoStateManager demoStateMgr;

    /**
     * Local reference to the interaction held by the UI.
     */
    private FrameEditorInteraction interaction;

    /**
     * The model object for this controller. A Frame.
     */
    private Frame model;

    /**
     * The parent object for this model. the design.
     * Used in order to preserve the relationships and to invalidating scripts.
     */
    private Design design;

    private static final String DEFAULT_WIDGET_PREFIX =
        L10N.get("FE.WidgetNamePrefix", "Widget");

    private static final String DEFAULT_GROUP_PREFIX =
        L10N.get("FE.GroupNamePrefix", "Group");

    /**
     * Holds the current suffix to auto insert on new widgets
     */
    private int widgetNameSuffix = 1;

    private static final String PASTE = DesignEditorCmd.PASTE;

    private static final String WIDGET_COPIED =
        L10N.get("FE.WidgetCopied", "Widget copied to the clipboard");

    private static final String WIDGETS_COPIED =
        L10N.get("FE.WidgetsCopied", "Widgets copied to the clipboard");

    private static final String SELECT_WIDGET_COLOR =
        L10N.get("FRAME.WIDGET.selectColor", "Select Widget Color");

    private static final String BRING_TO_FRONT =
        L10N.get("UNDO.FE.BringToFront", "Bring to Front");

    private static final String BRING_FORWARD =
        L10N.get("UNDO.FE.BringForward", "Bring Forward");

    private static final String SEND_BACKWARD =
        L10N.get("UNDO.FE.SendBackward", "Send Backward");

    private static final String SEND_TO_BACK =
        L10N.get("UNDO.FE.SendToBack", "Send to Back");

    private static final String CHG_DISPLAYED_LABEL =
        L10N.get("UNDO.FE.ChangeWidgetTitle", "Change Displayed Label");

    private static final String CHG_AUX_TEXT =
        L10N.get("UNDO.FE.ChangeWidgetAuxText", "Change Auxiliary Text");

    private static final String SET_REMOTE_LABEL =
        L10N.get("UNDO.FE.SetRemoteLabel", "Set Remote Label");

    private static final String CAPTURE_BKG_IMG =
        L10N.get("UNDO.FE.CaptureBackgroundImage", "Capture Background");

    private static final String CHG_WIDGET_SHAPE =
        L10N.get("UNDO.FE.ChangeWidgetShape", "Change Widget Shape");

    private static final String CHG_WIDGET_TYPE =
        L10N.get("UNDO.FE.ChangeWidgetType", "Change Widget Type");

    private static final String CHG_WIDGET_RENDERED =
        L10N.get("UNDO.FE.ChangeRenderSkin", "Change Widget Rendered");

    private static final String SET_WIDGET_COLOR =
        L10N.get("UNDO.FE.SetWidgetColor", "Set Widget Color");

    private static final String REMOVE_BKG_IMG =
        L10N.get("UNDO.FE.RemoveBackgroundImage",
                 "Remove Frame Background Image");

    private static final String SET_BKG_IMG =
        L10N.get("UNDO.FE.SetBackgroundImage", "Set Frame Background Image");

    private static final String NEW_WIDGET =
        L10N.get("UNDO.FE.NewWidget", "New Widget");

    private static final String DELETE_WIDGETS =
        L10N.get("UNDO.FE.DeleteWidgets", "Delete Widgets");

    private static final String DELETE_WIDGET =
        L10N.get("UNDO.FE.DeleteWidget", "Delete Widget");

    private static final String DELETE_GROUP =
        L10N.get("UNDO.FE.DeleteGroup", "Delete Group");

    private static final String RESIZE_WIDGETS =
        L10N.get("UNDO.FE.ResizeWidgets", "Resize Widgets");

    private static final String RESIZE_WIDGET =
        L10N.get("UNDO.FE.ResizeWidget", "Resize Widget");

    private static final String MOVE_WIDGETS =
        L10N.get("UNDO.FE.MoveWidgets", "Move Widgets");

    private static final String MOVE_WIDGET =
        L10N.get("UNDO.FE.MoveWidget", "Move Widget");

    private static final String REORDER_WIDGET =
        L10N.get("UNDO.FE.ReorderWidget", "Reorder Widget");

    private static final String CHG_WIDGET_NAME =
        L10N.get("UNDO.FE.ChangeWidgetName", "Change Widget Name");

    private static final String CHG_FRAME_NAME =
        L10N.get("UNDO.FE.ChangeFrameName", "Change Frame Name");

    private static final String DUPLICATE_WIDGET =
        L10N.get("UNDO.FE.DuplicateWidget", "Duplicate Widget");

    private static final String DUPLICATE_WIDGETS =
        L10N.get("UNDO.FE.DuplicateWidgets", "Duplicate Widgets");

    private static final String REMOVE_WIDGET_IMG =
        L10N.get("UNDO.FE.RemoveWidgetImage", "Remove Widget Image");

    private static final String SET_WIDGET_IMG =
        L10N.get("UNDO.FE.SetWidgetImage", "Set Widget Image");

    private static final String CHANGE_SKIN =
        L10N.get("UNDO.DE.ChangeSkin", "Change Skin");

    private static final String RENDER_ALL =
        L10N.get("UNDO.DE.RenderAll", "Render Design's Widgets");

    private static final String UN_RENDER =
        L10N.get("UNDO.DE.UnRender", "UnRender Design's Widgets");

    private static final String CHG_WIDGET_LEVEL =
        L10N.get("UNDO.FE.ChangeWidgetLevel", "Level Change");

    private static final String SET_SPEAKER_TEXT =
        L10N.get("UNDO.FE.SetSpeakerText", "Set Speaker Text");

    private static final String SET_SPEAKER_DURATION =
        L10N.get("UNDO.FE.SetSpeakerDuration", "Set Speaker Duration");

    private static final String GROUP_ELEMENTS =
        L10N.get("UNDO.FE.GroupElements", "Group");

    private static final String UNGROUP_ELEMENTS =
        L10N.get("UNDO.FE.UngroupElements", "Ungroup");

    private static final String nothingPasted =
        L10N.get("FE.NothingPasted", "Nothing pasted");

    private static final String pasteComplete =
        L10N.get("FE.PasteComplete", "object(s) pasted");

    private static final String templateCreated =
        L10N.get("FE.TemplateCreated", "Frame Template created");

    private static final String noTemplateWidgets =
        L10N.get("FE.NoTemplateWidgets",
                 "No widgets are available to define Frame Template");

    private static final String frameTemplateComplete =
        L10N.get("FE.FrameTemplateComplete",
                 "widget(s) comprise the new Frame Template");

    /**
     * "Duplicator" support for duplicating widgets; simply returns the given
     * frame.
     */
    private static Frame.IFrameDuplicator lookupFrameDuplicator =
        new Frame.IFrameDuplicator()
        {

            public Frame getOrDuplicate(Frame frameToCopy)
            {
                return frameToCopy;
            }


            public void recordDuplicateFrame(Frame originalFrame,
                                             Frame frameDuplicate)
            {
                // Do nothing
            }
        };

    protected static class ElementAllWidgetIterator implements Iterator<IWidget>
    {
        private Stack<Iterator<? extends FrameElement>> mbrIterators =
            new Stack<Iterator<? extends FrameElement>>();
        private IWidget nextWidget;

        public ElementAllWidgetIterator(Association<?> association)
        {
            mbrIterators.push(association.iterator());
            nextWidget = null;
        }

        public ElementAllWidgetIterator(IWidget widget)
        {
            nextWidget = widget;
            pushParentWidgetIterator();
        }

        private void pushParentWidgetIterator()
        {
            if (nextWidget instanceof AParentWidget) {
                SimpleWidgetGroup groupChildren =
                    ((AParentWidget) nextWidget).getChildren();

                if (groupChildren != null) {
                    mbrIterators.push(groupChildren.iterator());
                }
            }
        }


        public boolean hasNext()
        {
            if (nextWidget == null) {
                while (mbrIterators.size() > 0) {
                    if (mbrIterators.peek().hasNext()) {
                        FrameElement nextMbr = mbrIterators.peek().next();

                        if (nextMbr instanceof IWidget) {
                            nextWidget = (IWidget) nextMbr;
                            pushParentWidgetIterator();

                            return true;
                        }

                        if (nextMbr instanceof Association<?>) {
                            Iterator<? extends FrameElement> members =
                                ((Association<?>) nextMbr).iterator();

                            mbrIterators.push(members);
                        }
                        // else unknown member type (currently none exist!)
                    }
                    else {
                        mbrIterators.pop();
                    }
                }

                return false;
            }

            return true;
        }


        public IWidget next()
        {
            if (hasNext()) {
                IWidget result = nextWidget;

                nextWidget = null;

                return result;
            }

            throw new NoSuchElementException();
        }


        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }

    protected static class ElementAllWidgets implements Iterable<IWidget>
    {
        private FrameElement elt;

        public ElementAllWidgets(FrameElement element)
        {
            elt = element;
        }


        public Iterator<IWidget> iterator()
        {
            if (elt instanceof IWidget) {
                return new ElementAllWidgetIterator((IWidget) elt);
            }

            if (elt instanceof Association<?>) {
                return new ElementAllWidgetIterator((Association<?>) elt);
            }

            return new EmptyIterator<IWidget>();
        }
    }

    /**
     * Combines a noteWidgetEdit and a check for automatic regeneration
     */
    private void noteEditCheckRegenerate(DemoStateManager.ObsoletingEdit edit)
    {
        demoStateMgr.noteFrameEdit(model, edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                                  design,
                                                  interaction);
        }
    }

    private void noteEditCheckRegenerate(FrameElement elt,
                                           DemoStateManager.ObsoletingEdit edit)
    {
        demoStateMgr.noteWidgetsEdit(new ElementAllWidgets(elt), edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                                  design,
                                                  interaction);
        }
    }

    private void noteEditCheckRegenerate(IWidget widget,
                                           DemoStateManager.ObsoletingEdit edit)
    {
        demoStateMgr.noteWidgetEdit(widget, edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                                  design,
                                                  interaction);
        }
    }

    private void noteEditCheckRegenerate(Iterable<? extends IWidget> widgets,
                                           DemoStateManager.ObsoletingEdit edit)
    {
        demoStateMgr.noteWidgetsEdit(widgets, edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                                  design,
                                                  interaction);
        }
    }

    private void noteEditCheckRegenerate(SimpleWidgetGroup group1,
                                           SimpleWidgetGroup group2,
                                           DemoStateManager.ObsoletingEdit edit)
    {
        demoStateMgr.noteReorderEdit(group1, group2, edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                                  design,
                                                  interaction);
        }
    }

    /**
     * Undo/redo edit for moving a widget to a new origin
     */
    protected class WidgetMoveUndoRedo extends DemoStateManager.ObsoletingEdit
    {
        private String presentationLabel;
        private IWidget widget;
        private double newX;
        private double newY;
        private double oldX;
        private double oldY;

        public WidgetMoveUndoRedo(ListenerIdentifier listenerID,
                                  String presentation,
                                  IWidget w,
                                  double nx,
                                  double ny,
                                  double ox,
                                  double oy)
        {
            super(listenerID, demoStateMgr);

            presentationLabel = presentation;
            widget = w;
            newX = nx;
            newY = ny;
            oldX = ox;
            oldY = oy;
        }

        @Override
        public String getPresentationName()
        {
            return presentationLabel;
        }

        @Override
        public void redo()
        {
            super.redo();

            widget.setWidgetOrigin(newX, newY);

            noteEditCheckRegenerate(widget, this);
        }

        @Override
        public void undo()
        {
            super.undo();

            widget.setWidgetOrigin(oldX, oldY);

            noteEditCheckRegenerate(widget, this);
        }
    }

    /**
     * The listenerAction to handle setting alignment of frame elements
     *
     * Subclass of listener action, which all alignment actions use.
     * TODO: generalize and move to external utility class.
     *
     * @author alexeiser
     *
     */
    protected class ElementAlignmentAction extends AlignmentAction
    {
        protected class ElementAlignmentUndoRedo
                                        extends DemoStateManager.ObsoletingEdit
        {
            private FrameElement elt;
            private double deltaX;
            private double deltaY;

            public ElementAlignmentUndoRedo(ListenerIdentifier listenerID,
                                            FrameElement e,
                                            double dx,
                                            double dy)
            {
                super(listenerID, demoStateMgr);

                elt = e;
                deltaX = dx;
                deltaY = dy;
            }

            @Override
            public String getPresentationName()
            {
                return getAlignmentName();
            }

            @Override
            public void redo()
            {
                super.redo();

                // Move
                elt.moveElement(deltaX, deltaY);

                noteEditCheckRegenerate(elt, this);
            }

            @Override
            public void undo()
            {
                super.undo();

                // move to old location
                elt.moveElement(- deltaX, - deltaY);

                noteEditCheckRegenerate(elt, this);
            }
        }

        public ElementAlignmentAction(int alignAction)
        {
            super(alignAction);
        }

        /**
         * Set the expected parameter class.
         * use Selection. (Expects multi select)
         */

        public Class<?> getParameterClass()
        {
            return FrameEditorSelectionState.class;
        }


        public boolean performAction(Object prms)
        {
            FrameEditorSelectionState selection =
                (FrameEditorSelectionState) prms;

            // Reset for new computation
            reset();

            // Check that there are two or more items to align.
            // Items that can align are elements that are not descendants
            //   of a selected FrameElementGroup, such as:
            // (1) A selected widget that doesn't have a parent group
            //     (thus, by definition, not an IChildWidget)
            // (2) The parent group of a selected non-IChildWidget widget
            // (3) A selected FrameElementGroup
            // For optimization, keep track of the bounds for each item
            // that will be aligned.
            Map<FrameElement, DoubleRectangle> allowedToAlign =
                new HashMap<FrameElement, DoubleRectangle>();

            Iterator<FrameElement> selectedElts =
                selection.getSelectedElementsIterator();

            // Determine which elements can participate and the
            // reference location
            while (selectedElts.hasNext()) {
                FrameElement elt = selectedElts.next();

                if ((! (elt instanceof ChildWidget)) &&
                    ! isMemberOfSelectedGroup(elt, selection))
                {
                    DoubleRectangle bds;

                    if (elt instanceof IWidget) {
                        SimpleWidgetGroup parentGroup =
                            ((IWidget) elt).getParentGroup();

                        if (parentGroup == null) {
                            bds = elt.getEltBounds();
                            allowedToAlign.put(elt, bds);
                        }
                        else {
                            bds = parentGroup.getEltBounds();
                            allowedToAlign.put(parentGroup, bds);
                        }
                    }
                    else {
                        bds = elt.getEltBounds();
                        allowedToAlign.put(elt, bds);
                    }

                    computeReference(bds);
                }
            }

            // Complain if not at least two participants
            if (allowedToAlign.size() < 2) {
                interaction.protestTooFewElements();
                return false;
            }

            CompoundUndoableEdit editSequence =
                new CompoundUndoableEdit(getAlignmentName(), LIDS[action]);

            // Adjust all the elements relative to the reference location
            Iterator<Map.Entry<FrameElement, DoubleRectangle>> alignEntries =
                allowedToAlign.entrySet().iterator();

            while (alignEntries.hasNext()) {
                Map.Entry<FrameElement, DoubleRectangle> alignEntry =
                    alignEntries.next();

                // Get the bounds
                DoubleRectangle bounds = alignEntry.getValue();

                // Compute where the bounds should be based on the reference;
                // the new origin is available in this.newX and this.newY
                computeNewOrigin(bounds);

                FrameElement elt = alignEntry.getKey();

                // Move to the new location
                double dx = newX - bounds.x;
                double dy = newY - bounds.y;

                elt.moveElement(dx, dy);

                DemoStateManager.ObsoletingEdit edit =
                    new ElementAlignmentUndoRedo(LIDS[action],
                                                 elt, dx, dy);

                noteEditCheckRegenerate(elt, edit);

                editSequence.addEdit(edit);
            }

            editSequence.end();

            // Only add this edit if it is significant
            if (editSequence.isSignificant()) {
                undoMgr.addEdit(editSequence);
            }

            interaction.setStatusMessage(getAlignmentName());

            return true;
        } // performAction
    }

    /**
     * Constructor for the FrameEditorController.
     * Takes a Frame as the input model.
     *
     * Creates the FrameEditorUI and the view.
     */
    public FrameEditorController(Frame f, Design d, Project p)
    {
        super(p);

        model = f;
        design = d;

        // Get the Undo Manager from the factory.
        // Opening a new window for this controller will keep the undo from the
        // previous one.
        undoMgr = UndoManager.getUndoManager(model,
                                                                 project);

        demoStateMgr =
            DemoStateManager.getStateManager(project, design);

        // Generate the UI Model. This uses a factory, but it's not really
        // necessary. The design is passed in mostly for renaming.
        // Similar with project.
        ui = new FrameEditorUI(model, design, project, undoMgr);

        // Get the UI's interaction support.
        interaction = ui.getInteraction();

        // Set up the list of Perform Action operations.
        assignActions();

        // Set the view as visible, and let the dogs fly! :D
        ui.setVisible(true);
    }

    /**
     * Generic accessor method from default controller.
     * Accessor method
     * @return model object.
     */
    @Override
    protected Object getModelObject()
    {
        return getModel();
    }

    /**
     * Accessor for the specific model
     * @return the Frame model being managed by this controller
     */
    public Frame getModel()
    {
        return model;
    }

    /**
     * Add the listeners for Menu Items, and other LID listeners
     */
    @Override
    public void assignActions()
    {
        // get the default actions from Default controller
        super.assignActions();

        // Enable undo & redo
        ui.setAction(FrameEditorLID.Undo,
                          new UndoController.UndoAction(undoMgr,
                                                        interaction));

        ui.setAction(FrameEditorLID.Redo,
                          new UndoController.RedoAction(undoMgr,
                                                        interaction));

        // Enable cut copy and paste
        ui.setAction(FrameEditorLID.Paste, createPasteAction());
        ui.setAction(FrameEditorLID.SetFrameTemplate,
                          createSetFrameTemplateAction());
        ui.setAction(FrameEditorLID.ClearFrameTemplate,
                          new AListenerAction() {

                              public boolean performAction(Object prms)
                              {
                                  FrameTemplateSupport.clearFrameTemplate(design);
                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.Copy, createCopyWidgetAction());
        ui.setAction(FrameEditorLID.Cut, createCutWidgetAction());

        ui.setAction(CogToolLID.CopyPath,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState seln =
                                      (FrameEditorSelectionState) prms;

                                  if (seln.getWidgetSelectionCount() != 1) {
                                      return false;
                                  }

                                  IWidget w = seln.getSelectedIWidgets()[0];

                                  Object pathObj =
                                      w.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);
                                  if (! NullSafe.equals(WidgetAttributes.NO_IMAGE,
                                                        pathObj))
                                  {
                                      ClipboardUtil.copyTextData((String) pathObj);
                                  }

                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.Rename,
                          createInitiateRenameAction());

        ui.setAction(FrameEditorLID.Relabel,
                          createInitiateRelabelAction());

        // Select all.
        ui.setAction(FrameEditorLID.SelectAll,
                          new AListenerAction() {

                              public boolean performAction(Object prms)
                              {
                                  ui.selectAllWidgets();

                                  return true;
                              }
                          });

        // Creating a new widget
        // May be called from the menu item, or from mouseState
        ui.setAction(FrameEditorLID.NewWidget, createNewWidgetAction());

        ui.setAction(FrameEditorLID.NewWidgetJustWarn,
                          createNewWidgetExplanationAction());

        // Delete an item.
        // Requires a selection state
        ui.setAction(FrameEditorLID.Delete,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Delete the item / add undo
                                  return deleteElements(selection);
                              }
                          });

        // Adjust image/color properties
        ui.setAction(FrameEditorLID.SetBackgroundImage,
                          createSetBackgroundImageAction());

        // Clear the background of an image.
        ui.setAction(FrameEditorLID.RemoveBackgroundImage,
                          new AListenerAction() {

                              public boolean performAction(Object prms)
                              {
                                  // Clear background, by saying use
                                  // "no" image.
                                  setBackgroundImage(null,
                                                     WidgetAttributes.NO_IMAGE);
                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.CopyImageAsBackground,
                          createCopyImageAsBkgAction());

        ui.setAction(FrameEditorLID.PasteBackgroundImage,
                          createPasteBackgroundImageAction());

        // Set the color of the widgets.
        ui.setAction(FrameEditorLID.SetWidgetColor,
                          newSetWidgetColorAction());

        // Skins!
        ui.setAction(FrameEditorLID.SkinWireFrame,
                          createSetSkinAction(SkinType.WireFrame,
                                              FrameEditorLID.SkinWireFrame));
        ui.setAction(FrameEditorLID.SkinMacOSX,
                          createSetSkinAction(SkinType.MacOSX,
                                              FrameEditorLID.SkinMacOSX));
        ui.setAction(FrameEditorLID.SkinWinXP,
                          createSetSkinAction(SkinType.WinXP,
                                              FrameEditorLID.SkinWinXP));
        ui.setAction(FrameEditorLID.SkinPalm,
                          createSetSkinAction(SkinType.Palm,
                                              FrameEditorLID.SkinPalm));

        ui.setAction(CogToolLID.RenderAll,
                     createRenderAllAction(true, CogToolLID.RenderAll));
        ui.setAction(CogToolLID.UnRender,
                     createRenderAllAction(false, CogToolLID.UnRender));

        // Nudge selected widget(s)
        // requires selection
        ui.setAction(FrameEditorLID.NudgeLeft,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Displace left by 1 pixel scaled by
                                  // the zoom
                                  double dx = -1.0 / ui.getZoom();

                                  // Move by the point
                                  return moveElements(selection, dx, 0.0);
                              }
                          });

        ui.setAction(FrameEditorLID.NudgeRight,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Move by 1 pixel scaled by zoom right.
                                  double dx = 1.0 / ui.getZoom();

                                  return moveElements(selection, dx, 0.0);
                              }
                          });

        ui.setAction(FrameEditorLID.NudgeUp,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Move up 1 pixel scaled by zoom
                                  double dy = -1.0 / ui.getZoom();

                                  return moveElements(selection, 0.0, dy);
                              }
                          });

        ui.setAction(FrameEditorLID.NudgeDown,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // move down by 1 pixel scaled by zoom
                                  double dy = 1.0 / ui.getZoom();

                                  return moveElements(selection, 0.0, dy);
                              }
                          });

        // Align selected widgets
        ui.setAction(FrameEditorLID.AlignTop,
                          new ElementAlignmentAction(AlignmentAction.TOP));

        ui.setAction(FrameEditorLID.AlignBottom,
                          new ElementAlignmentAction(AlignmentAction.BOTTOM));

        ui.setAction(FrameEditorLID.AlignLeft,
                          new ElementAlignmentAction(AlignmentAction.LEFT));

        ui.setAction(FrameEditorLID.AlignRight,
                          new ElementAlignmentAction(AlignmentAction.RIGHT));

        ui.setAction(FrameEditorLID.AlignCenter,
                          new ElementAlignmentAction(AlignmentAction.CENTER));

        ui.setAction(FrameEditorLID.AlignHorizCenter,
                          new ElementAlignmentAction(AlignmentAction.HORIZ_CENTER));

        ui.setAction(FrameEditorLID.AlignVertCenter,
                          new ElementAlignmentAction(AlignmentAction.VERT_CENTER));

        // Space selected widgets equally
        ui.setAction(FrameEditorLID.SpaceVertically,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Equally space the widgets in the
                                  // vertical axis.
                                  final boolean VERTICAL = true;

                                  return spaceElementsEqually(selection,
                                                              VERTICAL);
                              }
                          });

        ui.setAction(FrameEditorLID.SpaceHorizontally,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Equally space the widgets in the
                                  // horizontal axis.
                                  final boolean HORIZONTAL = false;

                                  return spaceElementsEqually(selection,
                                                              HORIZONTAL);
                              }
                          });

        // Adjust ordering of selected widget(s)
        ui.setAction(FrameEditorLID.BringToFront,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  CompoundUndoableEdit editSequence =
                                      new CompoundUndoableEdit(BRING_TO_FRONT,
                                                               FrameEditorLID.BringToFront);

                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Get the list of selected widgets
                                  // Keep them in the same level ordering
                                  // they started in
                                  IWidget[] selected =
                                      getSelectedWidgets(selection,
                                                         Widget.WidgetLevelComparator.ONLY);

                                  // Now, iterate through all widgets and
                                  // set the level.
                                  for (IWidget element : selected) {
                                      // use MAX Value here
                                      // adjustWidgetLevel will tell the frame
                                      // and it will change MAX_VALUE to
                                      // be the correct number
                                      adjustWidgetLevel(Integer.MAX_VALUE,
                                                        element,
                                                        editSequence,
                                                        editSequence.getLID());
                                  }

                                  editSequence.end();

                                  // Only add this edit if it is significant
                                  if (editSequence.isSignificant()) {
                                      undoMgr.addEdit(editSequence);
                                  }

                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.BringForward,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  CompoundUndoableEdit editSequence =
                                      new CompoundUndoableEdit(BRING_FORWARD,
                                                               FrameEditorLID.BringForward);

                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Get the list of selected widgets
                                  // Keep them in the same level ordering
                                  // they started in
                                  IWidget[] selected =
                                      getSelectedWidgets(selection,
                                                         Widget.WidgetLevelComparator.ONLY);

                                  // Fix corner case where all the widgets are
                                  // stacked downward from the top
                                  // IE: Prevent the current top most item
                                  // from moving.
                                  int maxLevel = model.getWidgets().size() - 1;

                                  // Traverse the list sorted by level in reverse order
                                  for (int i = selected.length - 1;
                                       i >= 0;
                                       i--, maxLevel--)
                                  {
                                      IWidget w = selected[i];
                                      int widgetLevel = w.getLevel();

                                      // If the level is less then the max,
                                      // try to increase it one.
                                      if (widgetLevel < maxLevel) {
                                          adjustWidgetLevel(widgetLevel + 1,
                                                            w,
                                                            editSequence,
                                                            editSequence.getLID());
                                      }
                                  }

                                  editSequence.end();

                                  // Only add this edit if it is significant
                                  if (editSequence.isSignificant()) {
                                      undoMgr.addEdit(editSequence);
                                  }

                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.SendBackward,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  CompoundUndoableEdit editSequence =
                                      new CompoundUndoableEdit(SEND_BACKWARD,
                                                               FrameEditorLID.SendBackward);

                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Get the list of selected widgets
                                  // Keep them in the same level ordering
                                  // they started in
                                  IWidget[] selected =
                                      getSelectedWidgets(selection,
                                                         Widget.WidgetLevelComparator.ONLY);

                                  // Fix corner case where all the widgets are
                                  // stacked upward from level 0
                                  int minLevel = 0;

                                  for (int i = 0;
                                       i < selected.length;
                                       i++, minLevel++)
                                  {
                                      IWidget w = selected[i];
                                      int widgetLevel = w.getLevel();

                                      // move the level down by 1
                                      if (widgetLevel > minLevel) {
                                          adjustWidgetLevel(widgetLevel - 1,
                                                            w,
                                                            editSequence,
                                                            editSequence.getLID());
                                      }
                                  }

                                  editSequence.end();

                                  // Only add this edit if it is significant
                                  if (editSequence.isSignificant()) {
                                      undoMgr.addEdit(editSequence);
                                  }

                                  return true;
                              }
                          });

        ui.setAction(FrameEditorLID.SendToBack,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  CompoundUndoableEdit editSequence =
                                      new CompoundUndoableEdit(SEND_TO_BACK,
                                                               FrameEditorLID.SendToBack);

                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Get the list of selected widgets
                                  // Keep them in the same level ordering
                                  // they started in
                                  IWidget[] selected =
                                      getSelectedWidgets(selection,
                                                         Widget.WidgetLevelComparator.ONLY);

                                  // Traverse in reverse so they get set to 0
                                  // in the correct order
                                  for (int i = selected.length - 1; i >= 0; i--)
                                  {
                                      // try to move all towards 0.
                                      adjustWidgetLevel(0,
                                                        selected[i],
                                                        editSequence,
                                                        editSequence.getLID());
                                  }

                                  editSequence.end();

                                  // Only add this edit if it is significant
                                  if (editSequence.isSignificant()) {
                                      undoMgr.addEdit(editSequence);
                                  }

                                  return true;
                              }
                          });

        // Mouse operations on widgets
        ui.setAction(FrameEditorLID.MoveWidgets,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.MoveParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  // Get the move parameters.
                                  FrameEditorUI.MoveParameters movePrms =
                                      (FrameEditorUI.MoveParameters) prms;

                                  if (movePrms != null) {
                                      return moveElements(movePrms.selection,
                                                          movePrms.moveByX,
                                                          movePrms.moveByY,
                                                          movePrms.moveAsGroup);
                                  }

                                  return false;
                              }
                          });

        ui.setAction(FrameEditorLID.Reorder,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.ReorderWidgetParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  // Get the parameters.
                                  FrameEditorUI.ReorderWidgetParameters rPrms =
                                      (FrameEditorUI.ReorderWidgetParameters) prms;

                                  if (rPrms != null) {
                                      if (rPrms.parent == null) {
                                          return reorderWidget(rPrms.reorderWidget,
                                                               rPrms.widgetGroup,
                                                               rPrms.insertIndex);
                                      }

                                      return reorderChildWidget((ChildWidget)
                                                                   rPrms.reorderWidget,
                                                                rPrms.widgetGroup,
                                                                rPrms.insertIndex,
                                                                rPrms.parent);
                                  }

                                  return false;
                              }
                          });

        ui.setAction(FrameEditorLID.InsertDuplicate,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.InsertDuplicateParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  // Get the parameters.
                                  FrameEditorUI.InsertDuplicateParameters iPrms =
                                      (FrameEditorUI.InsertDuplicateParameters) prms;

                                  if (iPrms != null) {
                                      return insertDuplicateWidget(iPrms.reorderWidget,
                                                                   iPrms.widgetGroup,
                                                                   iPrms.insertIndex,
                                                                   iPrms.parent,
                                                                   iPrms.moveByX,
                                                                   iPrms.moveByY);
                                  }

                                  return false;
                              }
                          });

        ui.setAction(FrameEditorLID.ResizeWidgets,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.ResizeParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorUI.ResizeParameters resizePrms =
                                      (FrameEditorUI.ResizeParameters) prms;

                                  if (prms != null) {
                                      // Resize the frame elements based on prms
                                      return resizeElements(resizePrms.oldX,
                                                            resizePrms.oldY,
                                                            resizePrms.newX,
                                                            resizePrms.newY,
                                                            resizePrms.ratioX,
                                                            resizePrms.ratioY,
                                                            resizePrms.selection);
                                  }

                                  return false;
                              }
                          });

        // Change properties of selected widget(s)
        ui.setAction(FrameEditorLID.ChangeShapeProperty,
                          createChangeShapeAction());

        // Change the widget title.
        // The title is non unique.
        ui.setAction(FrameEditorLID.ChangeTitleProperty,
                          createChangeTitlePropertyAction());

        ui.setAction(FrameEditorLID.ChangeAuxTextProperty,
                          createChangeAuxTextPropertyAction());

        // Change the name. This requires a UNIQUE name.
        // Only one selected widget is expected.
        ui.setAction(DesignEditorLID.RenameFrame,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return DesignEditorUI.FrameRenameParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  DesignEditorUI.FrameRenameParameters p =
                                      (DesignEditorUI.FrameRenameParameters) prms;

                                  return updateFrameName(p.frame, p.newName);
                              }
                          });

        // Change the name. This requires a UNIQUE name.
        // Only one selected widget is expected.
        ui.setAction(FrameEditorLID.ChangeNameProperty,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.ActionStringParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorUI.ActionStringParameters p =
                                      (FrameEditorUI.ActionStringParameters) prms;

                                  // Check selection count
                                  int numWidgets =
                                      p.selection.getWidgetSelectionCount();

                                  if (numWidgets == 0) {
                                      interaction.protestNoSelection();
                                  }
                                  else if (numWidgets > 1) {
                                      interaction.protestTooManyWidgets();
                                  }
                                  else {
                                      IWidget[] selectedWidget =
                                          p.selection.getSelectedIWidgets();

                                      // Update widget's name
                                      return updateWidgetName(selectedWidget[0],
                                                              p.newString);
                                  }

                                  return false;
                              }
                          });

        // Change the type of the widget.
        // TODO: this needs to be extended to invalidate transitions (e.g., if changed to Noninteractive)
        ui.setAction(FrameEditorLID.ChangeTypeProperty,
                          createChangeTypeAction());

        ui.setAction(FrameEditorLID.SetRenderSkin,
                          createSetRenderSkinAction());

        // Set the widget image property.
        ui.setAction(FrameEditorLID.SetImageProperty,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                   return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  // Apply to all selected items.
                                  Iterator<IWidget> selected =
                                      selection.getSelectedWidgetsIterator();

                                  // Prompt user for the new file
                                  String imageURL =
                                      interaction.selectImageFile();

                                  // Check if the user canceled the dialog
                                  if (imageURL != null) {
                                      try {
                                          byte[] imageData =
                                              GraphicsUtil.loadImageFromFile(imageURL);

                                          setWidgetImages(selected,
                                                          imageData,
                                                          imageURL);

                                          return true;
                                      }
                                      catch (IOException e) {
                                          // Tell the user if there was a
                                          // problem reading the file.
                                          interaction.protestUnreadableFile();
                                      }
                                  }

                                  return false;
                              }
                          });

        // Clear the image stored on a widget.
        ui.setAction(FrameEditorLID.RemoveImageProperty,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorSelectionState.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorSelectionState selection =
                                      (FrameEditorSelectionState) prms;

                                  Iterator<IWidget> selected =
                                      selection.getSelectedWidgetsIterator();

                                  // Remove all the images
                                  setWidgetImages(selected,
                                                  null,
                                                  WidgetAttributes.NO_IMAGE);

                                  return true;
                              }
                          });

        // capture the image under a widget..
        // IE: get its background.
        ui.setAction(FrameEditorLID.CaptureImageProperty,
                          captureImageAction());

        ui.setAction(FrameEditorLID.Duplicate, duplicateWidgetsAction());

        ui.setAction(CogToolLID.SetAttribute,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return UI.SetAttributeParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  UI.SetAttributeParameters saprms =
                                      (UI.SetAttributeParameters) prms;

                                  return frameSetAttribute(saprms.target,
                                                           saprms.attrName,
                                                           saprms.value,
                                                           undoMgr);
                              }
                          });

        ui.setAction(FrameEditorLID.SetSpeakerText,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return String.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  return setSpeakerText((String) prms);
                              }
                          });

        ui.setAction(FrameEditorLID.SetSpeakerTime,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return Double.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  double newTime =
                                      ((Double) prms).doubleValue();
                                  return setSpeakerDuration(newTime);
                              }
                          });

        ui.setAction(FrameEditorLID.AddDesignDevices,
                          new AListenerAction() {

                              public boolean performAction(Object prms)
                              {
                                  return DesignCmd.addDevices(project,
                                                              design,
                                                              interaction);
                              }
                          });

        ui.setAction(FrameEditorLID.Group, createGroupElementsAction());
        ui.setAction(FrameEditorLID.Ungroup, createUngroupElementsAction());

        ui.setAction(FrameEditorLID.RenameEltGroup,
                          new IListenerAction() {

                              public Class<?> getParameterClass()
                              {
                                  return FrameEditorUI.EltGroupRenameParameters.class;
                              }


                              public boolean performAction(Object prms)
                              {
                                  FrameEditorUI.EltGroupRenameParameters p =
                                      (FrameEditorUI.EltGroupRenameParameters) prms;

                                  return renameEltGroup(p.eltGroup, p.newName);
                              }
                          });

        ui.setAction(FrameEditorLID.SetRemoteLabelText,
                          createSetRemoteLabelTextAction());

        ui.setAction(FrameEditorLID.SetRemoteLabelType,
                          createSetRemoteLabelTypeAction());
    } // assignActions

    private boolean setAsSeparator(final IWidget widget,
                                     final Object value,
                                     final ChildWidget oldSelectedItem,
                                     IUndoableEditSequence editSeq)
    {
        final String IS_SEP_ATTR = WidgetAttributes.IS_SEPARATOR_ATTR;
        final Object oldValue = widget.getAttribute(IS_SEP_ATTR);

        final DoubleRectangle bounds = widget.getEltBounds();
        final double oldHeight = bounds.height;
        final double newHeight =
            WidgetAttributes.IS_SEPARATOR.equals(value)
                ? (oldHeight / FrameEditorUI.SEPARATOR_RATIO)
                : (oldHeight * FrameEditorUI.SEPARATOR_RATIO);

        final boolean isAuto = widget.isStandard();

        final SimpleWidgetGroup group = widget.getParentGroup();
        final AParentWidget pullDown =
            (oldSelectedItem != null) ? oldSelectedItem.getParent() : null;

        if (widget.equals(oldSelectedItem)) {
            // the currently selected widget in the pull down
            // has changed to a separator; deselect it
            pullDown.setAttribute(WidgetAttributes.SELECTION_ATTR,
                                  WidgetAttributes.NONE_SELECTED);
        }

        widget.setAttribute(IS_SEP_ATTR, value);
        if (isAuto) {
            widget.setWidgetSize(bounds.width, newHeight);
            DesignEditorCmd.repositionChildren(widget.getParentGroup());
        }

        if (editSeq != null) {
            DemoStateManager.IDesignUndoableEdit edit =
                new DemoStateManager.InvalidatingEdit(CogToolLID.SetAttribute,
                                                      demoStateMgr)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return DefaultCmd.SET_ATTRIBUTE;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        if (widget.equals(oldSelectedItem)) {
                            pullDown.setAttribute(WidgetAttributes.SELECTION_ATTR,
                                                  WidgetAttributes.NONE_SELECTED);
                        }

                        widget.setAttribute(IS_SEP_ATTR, value);
                        if (isAuto) {
                            widget.setWidgetSize(bounds.width, newHeight);
                            DesignEditorCmd.repositionChildren(group);
                        }

                        stateMgr.noteWidgetEdit(widget, this);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        if (widget.equals(oldSelectedItem)) {
                            pullDown.setAttribute(WidgetAttributes.SELECTION_ATTR,
                                                  oldSelectedItem);
                        }

                        widget.setAttribute(IS_SEP_ATTR, oldValue);
                        if (isAuto) {
                            widget.setWidgetSize(bounds.width, oldHeight);
                            DesignEditorCmd.repositionChildren(group);
                        }

                        stateMgr.noteWidgetEdit(widget, this);
                    }
                };

            demoStateMgr.noteWidgetEdit(widget, edit);
            editSeq.addEdit(edit);
        }

        return true;
    } // setAsSeparator

    // xxy for remote label, do we set both directions (here??)
    private boolean frameSetAttribute(IAttributed target,
                                        final String attrName,
                                        final Object value,
                                        IUndoableEditSequence editSeq)
    {
        if (attrName.equals(WidgetAttributes.IS_SEPARATOR_ATTR)) {
            ChildWidget selectedItem = null;

            if (target instanceof PullDownItem) {
                AParentWidget pullDown = ((PullDownItem) target).getParent();

                selectedItem =
                    (ChildWidget)
                        pullDown.getAttribute(WidgetAttributes.SELECTION_ATTR);
            }

            return setAsSeparator((IWidget) target,
                                  value,
                                  selectedItem,
                                  editSeq);
        }

        return DefaultCmd.setAttribute(target,
                                       demoStateMgr, attrName, value,
                                       interaction, editSeq);
    }

    private void insertDuplicateEdit(final IWidget duplicatedWidget,
                                       final ReadOnlyList<? extends IWidget> widgetCopies,
                                       final SimpleWidgetGroup group,
                                       final int index,
                                       final AParentWidget parent,
                                       final double startPosX,
                                       final double startPosY,
                                       IUndoableEditSequence editSeq)
    {
        DemoStateManager.IDesignUndoableEdit edit =
            new DemoStateManager.InvalidatingEdit(FrameEditorLID.Duplicate,
                                                  demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return DUPLICATE_WIDGET;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    if (duplicatedWidget instanceof ChildWidget) {
                        ChildWidget child = (ChildWidget) duplicatedWidget;
                        AParentWidget curParent = child.getParent();

                        curParent.addItem(index, child);
                    }
                    else if (index >= 0) {
                        group.add(index, duplicatedWidget);
                    }
                    else {
                        group.add(duplicatedWidget);
                    }

                    model.addWidget(duplicatedWidget);

                    stateMgr.noteWidgetsEdit(widgetCopies, this);

                    if (parent != null) {
                        DesignEditorCmd.repositionChildren(parent);
                    }
                    else if (group != null) {
                        DesignEditorCmd.repositionChildren(group, startPosX, startPosY);
                    }
                }

                @Override
                public void undo()
                {
                    super.undo();

                    if (duplicatedWidget instanceof ChildWidget) {
                        ChildWidget child = (ChildWidget) duplicatedWidget;
                        AParentWidget curParent = child.getParent();
                        curParent.removeItem(child);
                    }
                    else if (duplicatedWidget.getParentGroup() != null) {
                        SimpleWidgetGroup parentGroup =
                            duplicatedWidget.getParentGroup();
                        parentGroup.remove(duplicatedWidget);
                    }

                    model.removeWidget(duplicatedWidget);

                    stateMgr.noteWidgetsEdit(widgetCopies, this);

                    if (parent != null) {
                        DesignEditorCmd.repositionChildren(parent);
                    }
                    else if (group != null) {
                        DesignEditorCmd.repositionChildren(group, startPosX, startPosY);
                    }
                }
            };

        editSeq.addEdit(edit);
    }

    /**
     * If group or parent is non-null, duplicate the widget within the given
     * group.  If they are both null, the widget was dragged to empty space, so
     * give it a new group.
     */
    private boolean insertDuplicateWidget(IWidget widget,
                                            SimpleWidgetGroup group,
                                            int index,
                                            AParentWidget parent,
                                            double moveByX,
                                            double moveByY)
    {
        Map<IWidget, IWidget> widgetCopies =
        	new LinkedHashMap<IWidget, IWidget>();
        double startPosX = 0.0;
        double startPosY = 0.0;
        DoubleSize newSize;

        if (parent != null) {
            newSize = getNewWidgetSize(parent);

            newSize.height *= getHeightFactor(widget, parent.getChildren());
        }
        else if ((group == null) || (group.size() == 0)) {
            newSize = widget.getShape().getSize();
        }
        else {
            DoublePoint startPos = group.get(0).getShape().getOrigin();

            startPosX = startPos.x;
            startPosY = startPos.y;

            newSize = group.get(0).getShape().getSize();

            if (widget instanceof ListItem) {
                newSize.height *= getHeightFactor(widget, group);
            }
        }

        widgetSituator.reset(widgetCopies, null);

        IWidget newWidget = null;

        if (parent != null) {
            if (widget instanceof ChildWidget) {
                newWidget =
                    ((ChildWidget) widget).duplicate(parent,
                                                      lookupFrameDuplicator,
                                                      widgetSituator,
                                                      index);

                newWidget.setWidgetSize(newSize.width, newSize.height);
                if (newWidget instanceof AParentWidget) {
                    resizeChildren(newWidget);
                    DesignEditorCmd.repositionChildren((AParentWidget) newWidget);
                }

                DesignEditorCmd.repositionChildren(parent);
            }
        }
        else if (group != null) {
            if (widget instanceof MenuHeader) {
                newWidget =
                    ((MenuHeader) widget).duplicate(group,
                                                     lookupFrameDuplicator,
                                                     widgetSituator,
                                                     index);
            }
            else if (widget instanceof ListItem) {
                newWidget =
                    ((ListItem) widget).duplicate(group,
                                                   lookupFrameDuplicator,
                                                   index);
            }

            newWidget.setWidgetSize(newSize.width, newSize.height);
            resizeChildren(newWidget);
            widgetSituator.placeInContext(widget, newWidget);
            DesignEditorCmd.repositionChildren(group, startPosX, startPosY);
        }
        else {
            // Duplicating into space
            if ((widget instanceof MenuHeader) ||
                (widget instanceof ListItem))
            {
                SimpleWidgetGroup newGroup = null;

                if (widget instanceof MenuHeader) {
                    newGroup = new SimpleWidgetGroup(SimpleWidgetGroup.HORIZONTAL);
                    newWidget =
                        ((MenuHeader) widget).duplicate(newGroup,
                                                         lookupFrameDuplicator,
                                                         widgetSituator);
                }
                else { // (widget instanceof ListItem)
                    newGroup = new SimpleWidgetGroup(SimpleWidgetGroup.VERTICAL);
                    newWidget =
                        ((ListItem) widget).duplicate(newGroup,
                                                       lookupFrameDuplicator);
                }

                group = newGroup;

                widgetSituator.placeInContext(widget, newWidget);
                newWidget.moveElement(moveByX, moveByY);

                group.setAttribute(WidgetAttributes.IS_RENDERED_ATTR,
                                   Boolean.valueOf(widget.isRendered()));
            }
        }

        widgetSituator.completeWork();

        Collection<IWidget> duplicateWidgets = widgetCopies.values();
        Iterator<IWidget> copies = duplicateWidgets.iterator();

        while (copies.hasNext()) {
            IWidget widgetCopy = copies.next();

            // Warning: it is important that each widget be added
            // to the frame *before* we make the next widget name
            // unique, or we can end up with non-unique names.
            makeWidgetNameUnique(widgetCopy);
            model.addWidget(widgetCopy);
        }

        SimpleWidgetGroup newGroup = (group != null) ? group
                                                : newWidget.getParentGroup();
        Object rendered = newGroup.getAttribute(WidgetAttributes.IS_RENDERED_ATTR);
        boolean groupRendered = ((Boolean) rendered).booleanValue();
        boolean widgetRendered = widget.isRendered();

        if (groupRendered != widgetRendered) {
            newWidget.setRendered(groupRendered);
        }

        insertDuplicateEdit(newWidget,
                            new ReadOnlyList<IWidget>(new ArrayList<IWidget>(duplicateWidgets)),
                            group,
                            index,
                            parent,
                            startPosX,
                            startPosY,
                            undoMgr);

        return true;
    }

    private void reorderGroupWidget(IWidget widget,
                                      double newWidth,
                                      double newHeight,
                                      int index,
                                      SimpleWidgetGroup prevGroup,
                                      SimpleWidgetGroup newGroup,
                                      double prevGroupStartX,
                                      double prevGroupStartY,
                                      double newGroupStartX,
                                      double newGroupStartY)
    {
        widget.setWidgetSize(newWidth, newHeight);
        resizeChildren(widget);

        prevGroup.remove(widget);
        newGroup.add(index, widget);
        DesignEditorCmd.repositionChildren(newGroup,
                                           newGroupStartX,
                                           newGroupStartY);

        if (prevGroup != newGroup) {
            DesignEditorCmd.repositionChildren(prevGroup,
                                               prevGroupStartX,
                                               prevGroupStartY);
        }
    }

    private boolean reorderWidget(final IWidget widget,
                                    final SimpleWidgetGroup newGroup,
                                    final int newIndex)
    {
        final SimpleWidgetGroup prevGroup = widget.getParentGroup();
        final int prevIndex = prevGroup.indexOf(widget);

        int index = newIndex;
        if (prevGroup == newGroup) {
            if (prevIndex < newIndex) {
                index--;
            }

            if (index == prevIndex) {
                return true;
            }
        }

        DoublePoint prevGroupStartPos =
            prevGroup.get(0).getShape().getOrigin();
        final double prevGroupStartX = prevGroupStartPos.x;
        final double prevGroupStartY = prevGroupStartPos.y;

        DoubleSize prevSize = widget.getShape().getSize();
        final double prevWidth = prevSize.width;
        final double prevHeight = prevSize.height;

        DoubleRectangle newBds = newGroup.get(0).getEltBounds();
        double heightFactor = (widget instanceof ListItem)
                                  ? getHeightFactor(widget, newGroup)
                                  : 1.0;
        final double newWidth = newBds.width;
        final double newHeight = newBds.height * heightFactor;
        final double newGroupStartX = newBds.x;
        final double newGroupStartY = newBds.y;

        Object rendered =
            newGroup.getAttribute(WidgetAttributes.IS_RENDERED_ATTR);
        final boolean groupRendered = ((Boolean) rendered).booleanValue();
        final boolean widgetRendered = widget.isRendered();

        reorderGroupWidget(widget,
                           newWidth, newHeight,
                           index, prevGroup, newGroup,
                           prevGroupStartX, prevGroupStartY,
                           newGroupStartX, newGroupStartY);

        if (widgetRendered != groupRendered) {
            widget.setRendered(groupRendered);
        }

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(FrameEditorLID.Reorder,
                                                demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return REORDER_WIDGET;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    int index = newIndex;

                    if (prevGroup == newGroup) {
                        if (prevIndex < newIndex) {
                            index--;
                        }
                    }

                    reorderGroupWidget(widget,
                                       newWidth, newHeight,
                                       index, prevGroup, newGroup,
                                       prevGroupStartX, prevGroupStartY,
                                       newGroupStartX, newGroupStartY);

                    if (widgetRendered != groupRendered) {
                        widget.setRendered(groupRendered);
                    }

                    noteEditCheckRegenerate(prevGroup, newGroup, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    reorderGroupWidget(widget,
                                       prevWidth, prevHeight,
                                       prevIndex, newGroup, prevGroup,
                                       newGroupStartX, newGroupStartY,
                                       prevGroupStartX, prevGroupStartY);

                    if (widgetRendered != groupRendered) {
                        widget.setRendered(widgetRendered);
                    }

                    noteEditCheckRegenerate(newGroup, prevGroup, this);
                }
            };

        noteEditCheckRegenerate(prevGroup, newGroup, edit);
        undoMgr.addEdit(edit);

        return true;
    }

    private void reorderChildWidget(ChildWidget child,
                                      double newWidth,
                                      double newHeight,
                                      int index,
                                      AParentWidget prevParent,
                                      AParentWidget newParent)
    {
        child.setWidgetSize(newWidth, newHeight);
        resizeChildren(child);

        prevParent.removeItem(child);
        newParent.addItem(index, child);
        DesignEditorCmd.repositionChildren(prevParent);

        if (newParent != prevParent) {
            child.setParent(newParent);
            DesignEditorCmd.repositionChildren(newParent);
        }
    }

    private boolean reorderChildWidget(final ChildWidget widget,
                                         final SimpleWidgetGroup newGroup,
                                         final int newIndex,
                                         final AParentWidget newParent)
    {
        final SimpleWidgetGroup prevGroup = widget.getParentGroup();
        final int prevIndex = prevGroup.indexOf(widget);

        int index = newIndex;
        if (prevGroup == newGroup) {
            if (prevIndex < newIndex) {
                index--;
            }

            if (index == prevIndex) {
                return true;
            }
        }

        final AParentWidget prevParent = widget.getParent();

        DoubleSize prevSize = widget.getShape().getSize();
        final double prevWidth = prevSize.width;
        final double prevHeight = prevSize.height;

        DoubleSize newSize = getNewWidgetSize(newParent);
        final double newWidth = newSize.width;
        final double newHeight = newSize.height * getHeightFactor(widget,
                                                                  newGroup);
        final boolean widgetRendered = widget.isRendered();

        reorderChildWidget(widget,
                           newWidth, newHeight,
                           index, prevParent, newParent);

        Object rendered =
            widget.getParentGroup().getAttribute(WidgetAttributes.IS_RENDERED_ATTR);
        final boolean groupRendered = ((Boolean) rendered).booleanValue();

        if (widgetRendered != groupRendered) {
            widget.setRendered(groupRendered);
        }

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(FrameEditorLID.Reorder,
                                                demoStateMgr)
            {
                private ChildWidget child = widget;

                @Override
                public String getPresentationName()
                {
                    return REORDER_WIDGET;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    int index = newIndex;

                    if (prevGroup == newGroup) {
                        if (prevIndex < newIndex) {
                            index--;
                        }
                    }

                    reorderChildWidget(child,
                                       newWidth, newHeight,
                                       index, prevParent, newParent);

                    if (widgetRendered != groupRendered) {
                        widget.setRendered(groupRendered);
                    }

                    noteEditCheckRegenerate(prevParent.getChildren(),
                                            newParent.getChildren(),
                                            this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    reorderChildWidget(child,
                                       prevWidth, prevHeight,
                                       prevIndex, newParent, prevParent);

                    if (widgetRendered != groupRendered) {
                        widget.setRendered(widgetRendered);
                    }

                    noteEditCheckRegenerate(newParent.getChildren(),
                                            prevParent.getChildren(),
                                            this);
                }
            };

        noteEditCheckRegenerate(prevParent.getChildren(),
                                newParent.getChildren(),
                                edit);
        undoMgr.addEdit(edit);

        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
            DemoScriptCmd.regenerateDesignScripts(project,
                                               design,
                                               interaction);
        }

        return true;
    }

    private double getHeightFactor(IWidget widget,
                                     SimpleWidgetGroup newGroup)
    {
        double heightFactor = 1.0;
        IWidget newWidget = null;

        if (newGroup != null) {
            if (newGroup.size() > 0) {
                newWidget = newGroup.get(0);
            }

            if (newWidget != null){
                Object isSep =
                    newWidget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
                    heightFactor *= FrameEditorUI.SEPARATOR_RATIO;
                }
            }
        }

        Object isSep = widget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

        if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
            heightFactor *= 1.0 / FrameEditorUI.SEPARATOR_RATIO;
        }

        return heightFactor;
    }

    private void resizeChildren(IWidget widget)
    {
        if (widget instanceof AParentWidget) {
            AParentWidget parent = (AParentWidget) widget;
            DoubleSize size = parent.getShape().getSize();

            if (parent instanceof MenuHeader) {
                size.width *= FrameEditorUI.MENU_ITEM_RATIO;
            }

            int numItems = parent.itemCount();

            for (int i = 0; i < numItems; i++) {
                ChildWidget child = parent.getItem(i);
                double h = size.height;

                Object isSep =
                    child.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
                    h /= FrameEditorUI.SEPARATOR_RATIO;
                }

                child.setWidgetSize(size.width, h);
                resizeChildren(child);
            }
        }
    }

    private DoubleSize getNewWidgetSize(AParentWidget newParent)
    {
        if (newParent.hasChildren()) {
            return newParent.getItem(0).getShape().getSize();
        }

        DoubleSize size = newParent.getShape().getSize();

        if (newParent instanceof MenuHeader) {
            size.width *= 1.3;
        }

        return size;
    }

    /**
     * Create a ListenerAction to handle creating a new Widget.
     *
     */
    private IListenerAction createNewWidgetAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                // TODO: Should we provide more input to this default widget?
                // Dialog box with option?
                // Current/last palette setting

                // TODO: Bonnie wanted to have new widgets show up under the
                // mouse.  To do that we need to do something like
                // getCursorLocation() (from display)
                // offset based on the (0,0) of the window
                // Might want to get that based on GetCursorControl().
                // If working with the control, you need to walk the tree to
                // get the actual offset from 0,0, in the display.
                //
                // Alternatively, in specialize, get the mouse pointer position
                // from mousestate (but that would require tracking the mouse
                // state -- on hover at least).

                // Instantiate the appropriate widget. If the class was passed
                // a prms, use that to dictate what to do.
                Widget widget = null;
                CompoundUndoableEdit editSequence =
                    new CompoundUndoableEdit(NEW_WIDGET,
                                             FrameEditorLID.NewWidget);

                if (prms instanceof FrameEditorUI.NewWidgetParameters) {
                    FrameEditorUI.NewWidgetParameters nwp = (FrameEditorUI.NewWidgetParameters) prms;

                    // If parent is non-null, the new widget will be a child
                    // widget
                    if (nwp.parent != null) {
                        if (nwp.type == WidgetType.MenuItem){
                            // Create menu item; may become a submenu through
                            // user interaction later
                            widget = new MenuItem((AMenuWidget) nwp.parent,
                                                  nwp.bounds,
                                                  nwp.widgetTitle);
                        }
                        else if (nwp.type == WidgetType.PullDownItem) {
                            widget =
                                new PullDownItem((PullDownHeader) nwp.parent,
                                                  nwp.bounds,
                                                  nwp.widgetTitle);

                            boolean rendered = nwp.parent.isRendered();
                            SimpleWidgetGroup group = widget.getParentGroup();

                            group.setAttribute(WidgetAttributes.IS_RENDERED_ATTR,
                                               Boolean.valueOf(rendered));
                        }
                    }
                    // If a parent group is specified, the new widget will be
                    // a menu header, list item, or radio button; these are the
                    // only widgets that are created as part of a free-standing
                    // widget group
                    else if (nwp.parentGroup != null) {
                        if (nwp.type == WidgetType.Menu) {
                            widget = new MenuHeader(nwp.parentGroup,
                                                    nwp.bounds,
                                                    nwp.widgetTitle);
                        }
                        else if (nwp.type == WidgetType.ListBoxItem) {
                            widget = new ListItem(nwp.parentGroup,
                                                  nwp.bounds,
                                                  nwp.widgetTitle);
                        }
                        else if (nwp.type == WidgetType.Radio) {
                            widget =
                                new RadioButton((RadioButtonGroup) nwp.parentGroup,
                                                nwp.bounds,
                                                nwp.widgetTitle);
                        }
                        else if (nwp.type == WidgetType.Check) {
                            widget =
                                new CheckBox((GridButtonGroup) nwp.parentGroup,
                                             nwp.bounds,
                                             nwp.widgetTitle);
                        }
                    }
                    else {
                        widget = createWidget(nwp.type,
                                              nwp.bounds,
                                              nwp.widgetTitle,
                                              nwp.isAutomatic);
                    }

                    if (nwp.isSeparator) {
                        frameSetAttribute(widget,
                                          WidgetAttributes.IS_SEPARATOR_ATTR,
                                          WidgetAttributes.IS_SEPARATOR,
                                          editSequence);
                    }
                }

//                if (widget.getWidgetType() == WidgetType.TextBox) {
//                    widget.setAttribute(WidgetAttributes.IS_STANDARD_ATTR,
//                                        WidgetAttributes.IS_CUSTOM);
//                }

                // Auto-generate a unique name for the widget
                widget.setName(generateUniqueWidgetName());

                // Build the widget: check for uniqueness and add to the frame
                boolean result = addCreatedWidget(widget, editSequence);

                editSequence.end();

                undoMgr.addEdit(editSequence);

                return result;
            }
        };
    }

    private IListenerAction createNewWidgetExplanationAction()
    {
        return new AListenerAction() {
            public boolean performAction(Object prms)
            {
                interaction.newWidgetExplanation();
                return false;
            }
        };
    }

    private Widget createWidget(WidgetType defaultType,
                                  DoubleRectangle bounds,
                                  String widgetTitle,
                                  boolean isStandard)
    {
        Widget widget;

        // The given parameter bounds must be valid, thus
        // the user specified actual bounds interactively
        if (isStandard) {
            if (defaultType == WidgetType.Menu) {
                SimpleWidgetGroup newMenuHeaderGroup =
                    new SimpleWidgetGroup(SimpleWidgetGroup.HORIZONTAL);

                widget = new MenuHeader(newMenuHeaderGroup,
                                        bounds,
                                        widgetTitle);
            }
            else if (defaultType == WidgetType.PullDownList) {
                widget = new PullDownHeader(bounds, widgetTitle);
            }
            else if (defaultType == WidgetType.ContextMenu) {
                widget = new ContextMenu(bounds, widgetTitle);

                // The default value for CONTEXT_MENU_ACTION_ATTR
                // is RIGHT_CLICK; must change to TAP_HOLD if the devices
                // contain a Touchscreen but not a Mouse
                Set<DeviceType> deviceTypes = design.getDeviceTypes();

                if (deviceTypes.contains(DeviceType.Touchscreen) &&
                    ! deviceTypes.contains(DeviceType.Mouse))
                {
                    widget.setAttribute(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR,
                                        WidgetAttributes.TAP_HOLD);
                }
            }
            else if (defaultType == WidgetType.ListBoxItem) {
                SimpleWidgetGroup newListItemGroup =
                    new SimpleWidgetGroup(SimpleWidgetGroup.VERTICAL);

                widget = new ListItem(newListItemGroup, bounds, widgetTitle);

                newListItemGroup.setAttribute(WidgetAttributes.FIRST_VISIBLE_ATTR,
                                              widget);
            }
            else if (defaultType == WidgetType.Radio) {
                RadioButtonGroup newRadioGroup = new RadioButtonGroup();

                widget = new RadioButton(newRadioGroup, bounds, widgetTitle);
            }
            else if (defaultType == WidgetType.Check) {
                GridButtonGroup newCheckGroup = new GridButtonGroup();

                widget = new CheckBox(newCheckGroup, bounds, widgetTitle);
            }
            else {
                // Create new widget in specified location
                // Note: could be a child widget;
                //       if so, the user is managing the hierarchy!
                widget = new Widget(bounds, defaultType);
            }

            widget.setAttribute(WidgetAttributes.IS_STANDARD_ATTR,
                                WidgetAttributes.IS_STANDARD);
        }
        else {
            // Create new widget in specified location
            // Note: could be a child widget;
            //       if so, the user is managing the hierarchy!
            widget = new Widget(bounds, defaultType);

            widget.setAttribute(WidgetAttributes.IS_STANDARD_ATTR,
                                WidgetAttributes.IS_CUSTOM);
        }

        return widget;
    }

    /**
     * Create a ListenerAction to handle setting frame background image
     *
     */
    private IListenerAction createSetBackgroundImageAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                // Pull up interaction to select a file
                String imageURL = interaction.selectImageFile();

                // Check if the dialog was cancelled
                if (imageURL != null) {
                    try {
                        // Set the background to new image
                        setBackgroundImage(GraphicsUtil.loadImageFromFile(imageURL),
                                           imageURL);

                        return true;
                    }
                    catch (IOException e) {
                        interaction.protestUnreadableFile();
                    }
                }

                return false;
            }
        };
    }

    private IListenerAction createPasteBackgroundImageAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.PasteBackgroundImageParms.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.PasteBackgroundImageParms p =
                    (FrameEditorUI.PasteBackgroundImageParms) prms;

                if (p.selection.getWidgetSelectionCount() > 0) {
                    setWidgetImages(p.selection.getSelectedWidgetsIterator(),
                                    p.imageData,
                                    WidgetAttributes.NO_IMAGE);
                }
                else {
                    // Set background image with clipboard data
                    setBackgroundImage(p.imageData,
                                       WidgetAttributes.NO_IMAGE);
                }

                return true;
            }
        };
    }

    private void changeTitleProperty(ListenerIdentifier lid,
                                       final String presentation,
                                       final IWidget widget,
                                       final String newTitle,
                                       boolean isSeparator,
                                       IUndoableEditSequence editSequence)
    {
        final String oldTitle = widget.getTitle();

        if (! oldTitle.equals(newTitle)) {
            if (isSeparator &&
                ((widget instanceof MenuItem) ||
                 (widget instanceof ListItem) ||
                 (widget instanceof PullDownItem)))
            {
                frameSetAttribute(widget,
                                  WidgetAttributes.IS_SEPARATOR_ATTR,
                                  WidgetAttributes.IS_SEPARATOR,
                                  editSequence);
            }

            widget.setTitle(newTitle);

            DemoStateManager.ObsoletingEdit edit =
                new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return presentation;
                    }

                    @Override
                    public void redo()
                    {
                        // Redo the set title
                        super.redo();
                        widget.setTitle(newTitle);

                        noteEditCheckRegenerate(widget, this);
                    }

                    @Override
                    public void undo()
                    {
                        // Go back to old title
                        super.undo();
                        widget.setTitle(oldTitle);

                        noteEditCheckRegenerate(widget, this);
                    }
                };

            noteEditCheckRegenerate(widget, edit);
            editSequence.addEdit(edit);
        }
    } // changeTitleProperty

    private IListenerAction createChangeTitlePropertyAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.ActionStringParameters.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.ActionStringParameters p = (FrameEditorUI.ActionStringParameters) prms;

                // While the UI should suppress
                // multiple selection for setting
                // titles, the selection supports it
                Iterator<IWidget> selected =
                	p.selection.getSelectedWidgetsIterator();

                CompoundUndoableEdit editSequence =
                    new CompoundUndoableEdit(CHG_DISPLAYED_LABEL,
                                             FrameEditorLID.ChangeTitleProperty);

                String newTitle = p.newString;

                // Loop through each item and set the title
                while (selected.hasNext()) {
                    IWidget widget = selected.next();

                    changeTitleProperty(FrameEditorLID.ChangeTitleProperty,
                                        CHG_DISPLAYED_LABEL,
                                        widget,
                                        newTitle,
                                        p.isSeparator,
                                        editSequence);
                }

                editSequence.end();

                // Don't add empty edits!
                if (editSequence.isSignificant()) {
                    undoMgr.addEdit(editSequence);
                }

                return true;
            }
        };
    } // createChangeTitlePropertyAction

    private void changeAuxTextProperty(ListenerIdentifier lid,
                                         final String presentation,
                                         FrameElement elt,
                                         final String newText,
                                         IUndoableEditSequence editSequence)
    {
        if (elt instanceof IWidget) {
            final IWidget widget = (IWidget) elt;

            final String oldText = widget.getAuxiliaryText();

            if (! oldText.equals(newText)) {
                widget.setAuxiliaryText(newText);

                DemoStateManager.ObsoletingEdit edit =
                    new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return presentation;
                        }

                        @Override
                        public void redo()
                        {
                            // Redo the set auxiliary text
                            super.redo();
                            widget.setAuxiliaryText(newText);

                            noteEditCheckRegenerate(widget, this);
                        }

                        @Override
                        public void undo()
                        {
                            // Go back to old auxiliary text
                            super.undo();
                            widget.setAuxiliaryText(oldText);

                            noteEditCheckRegenerate(widget, this);
                        }
                    };

                noteEditCheckRegenerate(widget, edit);
                editSequence.addEdit(edit);
            }
        }
        else if (elt instanceof FrameElementGroup) {
            final FrameElementGroup eltGroup = (FrameElementGroup) elt;

            final String oldText = eltGroup.getAuxiliaryText();

            if (! oldText.equals(newText)) {
                eltGroup.setAuxiliaryText(newText);

                IUndoableEdit edit =
                    new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return presentation;
                        }

                        @Override
                        public void redo()
                        {
                            // Redo the set auxiliary text
                            super.redo();
                            eltGroup.setAuxiliaryText(newText);
                        }

                        @Override
                        public void undo()
                        {
                            // Go back to old auxiliary text
                            super.undo();
                            eltGroup.setAuxiliaryText(oldText);
                        }
                    };

                editSequence.addEdit(edit);
            }
        }
    } // changeAuxTextProperty

    private IListenerAction createChangeAuxTextPropertyAction()
    {
        return new IListenerAction() {

          public Class<?> getParameterClass()
          {
              return FrameEditorUI.ActionStringParameters.class;
          }


          public boolean performAction(Object prms)
          {
              FrameEditorUI.ActionStringParameters p = (FrameEditorUI.ActionStringParameters) prms;

              // While the UI should suppress
              // multiple selection for setting
              // titles, the selection supports it
              Iterator<FrameElement> selected =
                  p.selection.getSelectedElementsIterator();

              CompoundUndoableEdit editSequence =
                  new CompoundUndoableEdit(CHG_AUX_TEXT,
                                           FrameEditorLID.ChangeAuxTextProperty);

              String newTitle = p.newString;

              // Loop through each item and set the title
              while (selected.hasNext()) {
                  FrameElement elt = selected.next();

                  changeAuxTextProperty(FrameEditorLID.ChangeAuxTextProperty,
                                        CHG_AUX_TEXT,
                                        elt,
                                        newTitle,
                                        editSequence);
              }

              editSequence.end();

              // Don't add empty edits!
              if (editSequence.isSignificant()) {
                  undoMgr.addEdit(editSequence);
              }

              return true;
          }
      };
    } // createChangeAuxTextPropertyAction

    /**
     * Create a ListenerAction which will handle capturing a background image.
     * @return
     */
    private IListenerAction captureImageAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                CompoundUndoableEdit editSequence =
                    new CompoundUndoableEdit(CAPTURE_BKG_IMG,
                                             FrameEditorLID.CaptureImageProperty);

                // Get the selection.
                FrameEditorSelectionState selection =
                    (FrameEditorSelectionState) prms;

                Iterator<IWidget> selected =
                	selection.getSelectedWidgetsIterator();

                // Iterate over every selected widget
                while (selected.hasNext()) {
                    final IWidget w = selected.next();

                    DoubleRectangle bounds = w.getEltBounds();

                    // Get the image from the background, and crop to shape
                    final byte[] bg =
                        GraphicsUtil.cropImage(model.getBackgroundImage(),
                                               bounds.x,
                                               bounds.y,
                                               bounds.width,
                                               bounds.height);

                    // Get the old image, could be null.
                    final byte[] old = w.getImage();
                    final String previousImagePath =
                        (String) w.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);

                    w.setImage(bg);
                    w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                   WidgetAttributes.NO_IMAGE);

                    editSequence.addEdit(new AUndoableEdit(FrameEditorLID.CaptureImageProperty)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return CAPTURE_BKG_IMG;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();
                            w.setImage(bg);
                            w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                           WidgetAttributes.NO_IMAGE);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();
                            w.setImage(old);
                            w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                           previousImagePath);
                        }
                    });
                }

                editSequence.end();

                // Only add this edit if it is significant
                if (editSequence.isSignificant()) {
                    undoMgr.addEdit(editSequence);
                }

                return true;
            }
        };
    }

    /**
     * Create a listener for changing the shape of a widget.
     *
     * @return
     */
    private IListenerAction createChangeShapeAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.ShapeChangeParameters.class;
            }


            public boolean performAction(Object prms)
            {
                // Get the parameters to change the shape into
                final FrameEditorUI.ShapeChangeParameters p = (FrameEditorUI.ShapeChangeParameters) prms;

                Iterator<IWidget> selected =
                	p.selection.getSelectedWidgetsIterator();

                CompoundUndoableEdit editSequence =
                    new CompoundUndoableEdit(CHG_WIDGET_SHAPE,
                                             FrameEditorLID.ChangeShapeProperty);

                // Loop through all selected widgets.
                while (selected.hasNext()) {
                    final IWidget w = selected.next();

                    final ShapeType oldShapeType = w.getShape().getShapeType();
                    final ShapeType newShapeType = p.newShapeType;

                    // Don't make a non-changing edit!
                    if (! oldShapeType.equals(newShapeType)) {
                        w.setShapeType(newShapeType);

                        DemoStateManager.ObsoletingEdit edit =
                            new DemoStateManager.ObsoletingEdit(FrameEditorLID.ChangeShapeProperty,
                                                                demoStateMgr)
                            {
                                @Override
                                public String getPresentationName()
                                {
                                    return CHG_WIDGET_SHAPE;
                                }

                                @Override
                                public void redo()
                                {
                                    super.redo();

                                    w.setShapeType(p.newShapeType);

                                    noteEditCheckRegenerate(w, this);
                                }

                                @Override
                                public void undo()
                                {
                                    super.undo();

                                    w.setShapeType(oldShapeType);

                                    noteEditCheckRegenerate(w, this);
                                }
                            };

                        noteEditCheckRegenerate(w, edit);
                        editSequence.addEdit(edit);
                    }
                }

                editSequence.end();

                // Don't add empty edits!
                if (editSequence.isSignificant()) {
                    undoMgr.addEdit(editSequence);
                }

                return true;
            }
        };
    }

    private void changeWidgetType(ListenerIdentifier lid,
                                    final String presentation,
                                    final IWidget widget,
                                    final WidgetType newWidgetType,
                                    IUndoableEditSequence editSequence)
    {
        final WidgetType oldWidgetType = widget.getWidgetType();

        // Don't make a non-changing edit!
        if (! oldWidgetType.equals(newWidgetType)) {
            // Set the new widget type
            widget.setWidgetType(newWidgetType);

            DemoStateManager.ObsoletingEdit edit =
                new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return presentation;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        widget.setWidgetType(newWidgetType);

                        noteEditCheckRegenerate(widget, this);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        widget.setWidgetType(oldWidgetType);

                        noteEditCheckRegenerate(widget, this);
                    }
                };

            noteEditCheckRegenerate(widget, edit);
            editSequence.addEdit(edit);
        }
    }

    /**
     * Create a listener action for changing the type.
     * Changing the type can be a dangerous option.
     * If you change the type and thus change what kind of
     * transition you can use, it may play havoc on scripts, and design view.
     * @return
     */
    private IListenerAction createChangeTypeAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.TypeChangeParameters.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.TypeChangeParameters p = (FrameEditorUI.TypeChangeParameters) prms;

                Iterator<IWidget> selected =
                	p.selection.getSelectedWidgetsIterator();

                CompoundUndoableEdit editSequence =
                    new CompoundUndoableEdit(CHG_WIDGET_TYPE,
                                             FrameEditorLID.ChangeTypeProperty);

                while (selected.hasNext()) {
                    IWidget widget = selected.next();

                    // Check if the widget types match
                    // TODO: deal with the following
                    // WidgetType.compatibleTransitions(oldWidgetType,
                    //                                  p.newWidgetType)
                    // if it returns false, show interaction..
                    // auto delete transition?

                    changeWidgetType(FrameEditorLID.ChangeTypeProperty,
                                     CHG_WIDGET_TYPE,
                                     widget,
                                     p.newWidgetType,
                                     editSequence);
                }

                editSequence.end();

                // Don't add empty edits!
                if (editSequence.isSignificant()) {
                    undoMgr.addEdit(editSequence);
                }

                return true;
            }
        };
    }

    private IListenerAction createSetRenderSkinAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.SetRenderSkinParameters.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.SetRenderSkinParameters p = (FrameEditorUI.SetRenderSkinParameters) prms;

                // Iterate through selected objects.
                Iterator<IWidget> selected =
                    p.selection.getSelectedWidgetsIterator();

                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(CHG_WIDGET_RENDERED,
                                             FrameEditorLID.SetRenderSkin);

                while (selected.hasNext()) {
                    IWidget w = selected.next();

                    if (w instanceof TraversableWidget) {
                        AParentWidget parent = null;

                        if (w instanceof MenuItem) {
                            parent = ((MenuItem) w).getTopHeader();

                            if (parent == null) {
                                // parent is a context menu
                                parent = ((MenuItem) w).getParent();
                            }
                        }
                        else if (w instanceof ChildWidget) {
                            parent = ((ChildWidget) w).getParent();
                        }
                        else if (w instanceof AParentWidget) {
                            parent = (AParentWidget) w;
                        }

                        if (parent != null) {
                            SimpleWidgetGroup group = parent.getParentGroup();

                            if (group != null) {
                                //menu header
                                renderGroup(group,
                                            p.rendered,
                                            parent.isRendered(),
                                            editSeq);
                            }
                            else {
                                //pull down header
                                renderWidget(parent,
                                             p.rendered,
                                             parent.isRendered(),
                                             editSeq);
                                renderChildren(parent,
                                               p.rendered,
                                               parent.isRendered(),
                                               editSeq);
                            }
                        }
                        else if (w.getParentGroup() != null) {
                            //list box item or radio button
                            renderGroup(w.getParentGroup(),
                                        p.rendered,
                                        w.isRendered(),
                                        editSeq);
                        }
                    }
                    else {
                        renderWidget(w, p.rendered, w.isRendered(), editSeq);
                    }
                }

                editSeq.end();

                // Only add this edit if it is significant
                if (editSeq.isSignificant()) {
                    undoMgr.addEdit(editSeq);
                }

                return true;
            }
        };
    } // createSetRenderSkinAction

    public static void renderWidget(final IWidget w,
                                    final boolean rendered,
                                    final boolean oldRendered,
                                    CompoundUndoableEdit edit)
    {
        w.setRendered(rendered);

        edit.addEdit(new AUndoableEdit(FrameEditorLID.SetRenderSkin)
        {
            @Override
            public String getPresentationName()
            {
                return CHG_WIDGET_RENDERED;
            }

            @Override
            public void redo()
            {
                super.redo();
                w.setRendered(rendered);
            }

            @Override
            public void undo()
            {
                super.undo();
                w.setRendered(oldRendered);
            }
        });
    }

    public static void renderUnRenderAll(Design d,
                                         boolean render,
                                         CogToolLID lid,
                                         UndoManager mgr)
    {
        CompoundUndoableEdit edits =
            new CompoundUndoableEdit((render ? RENDER_ALL : UN_RENDER), lid);
        for (Frame f : d.getFrames()) {
            for (IWidget w : f.getWidgets()) {
                renderWidget(w, render, w.isRendered(), edits);
            }
        }
        edits.end();
        if (edits.isSignificant()) {
            mgr.addEdit(edits);
        }

    }

    private void renderGroup(SimpleWidgetGroup group,
                               boolean rendered,
                               boolean oldRendered,
                               CompoundUndoableEdit edit)
    {
        Iterator<IWidget> children = group.iterator();

        while (children.hasNext()) {
            IWidget w = children.next();

            if (w instanceof AParentWidget) {
                renderChildren((AParentWidget) w, rendered, oldRendered, edit);
            }

            renderWidget(w, rendered, oldRendered, edit);
        }
    }

    private void renderChildren(AParentWidget parent,
                                  boolean rendered,
                                  boolean oldRendered,
                                  CompoundUndoableEdit edit)
    {
        if (parent.hasChildren()) {
            Iterator<IWidget> children = parent.getChildren().iterator();

            while (children.hasNext()) {
                IWidget w = children.next();

                if (w instanceof AParentWidget) {
                    renderChildren((AParentWidget) w,
                                   rendered,
                                   oldRendered,
                                   edit);
                }

                renderWidget(w, rendered, oldRendered, edit);
            }
        }
    }

    private IListenerAction createRenderAllAction(final boolean render,
                                                    final CogToolLID lid)
    {
        return new AListenerAction() {
            public boolean performAction(Object prms)
            {
                renderUnRenderAll(design,
                                  render,
                                  lid,
                                  FrameEditorController.this.undoMgr);
                return true;
            }
        };
    }

    /**
     * Set up the COPY widget code.
     * Uses the persistence store to generate XML for the copy action.
     * @return
     */
    private IListenerAction createCopyWidgetAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState seln =
                    (FrameEditorSelectionState) prms;

                // If selection is non-zero, copy frame elements
                int elementCount = seln.getElementSelectionCount();

                if (elementCount > 0) {
                    copyElements(seln, DesignEditorCmd.SAVE_TO_CLIPBOARD);

                    if (elementCount == 1) {
                        interaction.setStatusMessage(WIDGET_COPIED);
                    }
                    else {
                        interaction.setStatusMessage(WIDGETS_COPIED);
                    }

                    return true;
                }

                // tell user to select something.
                interaction.protestNoSelection();

                return false;
            }
        };
    }

    /**
     * Actually perform the copy widgets operation with the selection
     * Use persistence to generate the needed XML.
     * Saves either to the clipboard or to the given Design's Frame template.
     */
    private void copyElements(FrameEditorSelectionState seln,
                                boolean saveToClipboard)
    {
        // Sort by level so pasted widgets share level relationships of
        // the originals
        Set<FrameElement> sortedSelection =
            getSelectedElements(seln, elementLevelComparator);

        DesignEditorCmd.copyElements(design,
                                     seln.getSelectedIFrameElements(),
                                     sortedSelection.iterator(),
                                     saveToClipboard);
    }

    /**
     * Set up cut action, tests to ensure a cut is valid, and then
     * calls cut method.
     * @return
     */
    private IListenerAction createCutWidgetAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState seln =
                    (FrameEditorSelectionState) prms;

                // if non zero selected items copy them, then delete.
                if (seln.getElementSelectionCount() > 0) {
                    // Copy the widgets, then delete them to perform a cut
                    copyElements(seln, DesignEditorCmd.SAVE_TO_CLIPBOARD);
                    return deleteElements(seln);
                }

                // Tell the user nothing was selected
                interaction.protestNoSelection();

                return false;
            }
        };
    }

    private IListenerAction createInitiateRenameAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState selection =
                    (FrameEditorSelectionState) prms;

                int selectedWidgetCount = selection.getWidgetSelectionCount();

                if (selectedWidgetCount == 1) {
                    IWidget w = selection.getSelectedIWidgets()[0];
                    ui.initiateWidgetRename(w);
                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    }

    private IListenerAction createInitiateRelabelAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState selection =
                    (FrameEditorSelectionState) prms;

                int selectedWidgetCount = selection.getWidgetSelectionCount();

                if (selectedWidgetCount == 1) {
                    IWidget w = selection.getSelectedIWidgets()[0];
                    ui.initiateWidgetRetitle(w);
                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    }

    /**
     * Ensure that the new name specified is UNIQUE.
     * Do this by adding a [X] on the end. mostly used in copy and paste.
     *
     * See bug report #133
     *
     * @param widget
     */
    private void makeWidgetNameUnique(IWidget widget)
    {
        widget.setName(NamedObjectUtil.makeNameUnique(widget.getName(),
                                                      model.getWidgets()));
    }

    private void makeEltGroupNameUnique(FrameElementGroup eltGroup,
                                          String initialName)
    {
        eltGroup.setName(NamedObjectUtil.makeNameUnique(initialName,
                                                        model.getEltGroups()));
    }

    private void makeEltGroupNameUnique(FrameElementGroup eltGroup)
    {
        makeEltGroupNameUnique(eltGroup, eltGroup.getName());
    }

    private void assignUniqueEltGroupName(FrameElementGroup eltGroup)
    {
        makeEltGroupNameUnique(eltGroup, DEFAULT_GROUP_PREFIX + " [1]");
    }

    /**
     * Ensure that the new name specified is UNIQUE.
     * Do this by adding a [X] on the end. mostly used in copy and paste.
     *
     * See bug report #133
     *
     * @param widget
     */
    private void makeFrameNameUnique(Frame frame)
    {
        frame.setName(NamedObjectUtil.makeNameUnique(frame.getName(),
                                                     model.getDesign().getFrames()));
    }

    private void addChildWidgets(SimpleWidgetGroup widgetGroup,
                                   IUndoableEditSequence editSeq)
    {
        if (widgetGroup != null) {
            Iterator<IWidget> children = widgetGroup.iterator();

            while (children.hasNext()) {
                IWidget child = children.next();

                makeWidgetNameUnique(child);
                editSeq.addEdit(addWidget(child));

                if (child instanceof MenuItem) {
                    MenuItem item = (MenuItem) child;

                    if (item.isSubmenu()) {
                        addChildWidgets(item.getChildren(), editSeq);
                    }
                }
            }
        }
    }

    /**
     * Record the Widgets in the clipboard as the template to use
     * when creating new Frames.
     */
    private IListenerAction createSetFrameTemplateAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState seln =
                    (FrameEditorSelectionState) prms;

                // If selection is non zero, copy widgets
                if (seln.getElementSelectionCount() > 0) {
                    copyElements(seln, DesignEditorCmd.SAVE_TO_TEMPLATE);
                    interaction.setStatusMessage(templateCreated);

                    return true;
                }

                // tell user to select something.
                interaction.setStatusMessage(noTemplateWidgets);
                interaction.protestNoSelection();

                return false;
            }
        };
    } // createSetFrameTemplateAction

    /**
     * The paste action for inserting copied information.
     * Currently no checks are made to ensure that the paste is valid XML.
     * @return
     */
    private IListenerAction createPasteAction()
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                try {
                    if (CogToolClipboard.hasCogToolObjects()) {
                        // Get the XML text from the clipboard
                        Collection<Object> objects =
                            CogToolClipboard.fetchCogToolObjects();

                        if ((objects != null) && (objects.size() > 0)) {
                            CompoundUndoableEdit editSequence =
                                new CompoundUndoableEdit(PASTE,
                                                         FrameEditorLID.Paste);
                            int numPasted =
                                DesignEditorCmd.pasteElements(design,
                                                              model,
                                                              objects,
                                                              demoStateMgr,
                                                              editSequence);

                            if (numPasted > 0) {
                                editSequence.end();
                                undoMgr.addEdit(editSequence);
                                interaction.setStatusMessage(numPasted + " "
                                                              + pasteComplete);
                            }
                            else {
                                interaction.setStatusMessage(nothingPasted);
                            }
                        }

                        return true;
                    }
                    else {
                        interaction.setStatusMessage(nothingPasted);
                    }
                }
                catch (IOException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (ParserConfigurationException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (SAXException e) {
                    throw new RcvrClipboardException(e);
                }
                catch (ClipboardUtil.ClipboardException e) {
                    throw new RcvrClipboardException(e);
                }

                return false;
            }
        };
    } // createPasteAction

    private IListenerAction createCopyImageAsBkgAction()
    {
        return new IListenerAction()
        {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.CopyImageAsBackgroundParms.class;
            }


            public boolean performAction(Object actionParms)
            {
                FrameEditorUI.CopyImageAsBackgroundParms prms =
                    (FrameEditorUI.CopyImageAsBackgroundParms) actionParms;

                if (prms.selectedWidget != null) {
                    setWidgetImage(prms.selectedWidget,
                                   prms.imageData,
                                   WidgetAttributes.NO_IMAGE,
                                   FrameEditorLID.CopyImageAsBackground,
                                   SET_WIDGET_IMG,
                                   undoMgr);
                }
                else {
                    // Set background image with clipboard data
                    setBackgroundImage(prms.imageData,
                                       WidgetAttributes.NO_IMAGE);
                }

                return true;
            }
        };
    }

    /**
     * Returns a new anonymous IListenerAction for widget color changes.
     * @return
     */
    private IListenerAction newSetWidgetColorAction()
    {
        return new AListenerAction()
        {

            public boolean performAction(Object prms)
            {
                final int oldColor = model.getWidgetColor();

                // Ask the user to get the color.
                Integer newColorChoice =
                    interaction.selectColor(oldColor, SELECT_WIDGET_COLOR);

                // if the color is not the flag value, set it and add the undo.
                if (newColorChoice != null) {
                    final int newColor = newColorChoice.intValue();

                    model.setWidgetColor(newColor);

                    undoMgr.addEdit(new AUndoableEdit(FrameEditorLID.SetWidgetColor)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return SET_WIDGET_COLOR;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();
                            model.setWidgetColor(newColor);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();
                            model.setWidgetColor(oldColor);
                        }
                    });

                    return true;
                }

                return false;
            }
        };
    }

    /**
     * Sets the background image for the frame
     *
     * @param imageData the new image, or null if none
     */
    private void setBackgroundImage(final byte[] imageData,
                                      final String imagePath)
    {
        try {
            // compute the size of the new image.
            final DoubleRectangle imageSize =
                GraphicsUtil.getImageBounds(imageData);

            // Get existing image
            final byte[] previousImageData = model.getBackgroundImage();
            final DoubleRectangle previmageSize = model.getBackgroundBounds();
            final String oldPath =
                (String) model.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);

            // Perform operation
            model.setBackgroundImage(imageData, imageSize);

            model.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR, imagePath);

            CogToolLID lid = (imageData == null)
                                ? FrameEditorLID.RemoveBackgroundImage
                                : FrameEditorLID.SetBackgroundImage;

            // Add the undo edit
            IUndoableEdit edit = new AUndoableEdit(lid)
            {
                @Override
                public String getPresentationName()
                {
                    return (imageData == null) ? REMOVE_BKG_IMG
                                               : SET_BKG_IMG;
                }

                @Override
                public void redo()
                {
                    super.redo();
                    try {
                        model.setBackgroundImage(imageData, imageSize);
                        model.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                           imagePath);
                    }
                    catch (GraphicsUtil.ImageException ex) {
                        throw new RcvrImageException("Redo set background image failed",
                                                     ex);
                    }
                }

                @Override
                public void undo()
                {
                    super.undo();
                    try {
                        model.setBackgroundImage(previousImageData,
                                                 previmageSize);
                        model.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                                           oldPath);
                    }
                    catch (GraphicsUtil.ImageException ex) {
                        throw new RcvrImageException("Undo set background image failed",
                                                     ex);
                    }
                }
            };

            undoMgr.addEdit(edit);
        }
        catch (GraphicsUtil.ImageException e) {
            interaction.protestInvalidImageFile();
        }
    }

    /**
     * Add a widget to the frame.
     * Returns an UndoableEdit which can later be undone.
     * @param w
     * @return
     */
    private IUndoableEdit addWidget(IWidget widget)
    {
        model.addWidget(widget);

        IDesignUndoableEdit edit =
            DesignEditorCmd.addWidgetUndoableEdit(model,
                                                  widget,
                                                  demoStateMgr);

        demoStateMgr.noteWidgetEdit(widget, edit);

        return edit;
    }

    /**
     * Take a newly created widget and add it to the model, and then select it.
     * Prompts the user if the widget's name matches that of an existing widget.
     *
     * Currently this method is called in a single place, and the provided name
     * is guaranteed unique.
     *
     * @param w new widget
     */
    private boolean addCreatedWidget(final IWidget w,
                                       IUndoableEditSequence seq)
    {
        // If we have a name collision, we prompt the user to enter a new name
        // until we find one that does not collide.
        String name = w.getName();

        if (model.isWidgetNameTaken(name)) {
            do {
                // Ask until the new widget name is specified.
                name =
                    interaction.askWidgetName(name,
                                                   generateUniqueWidgetName());

                if (name == null) {
                    return false;
                }
            } while (model.isWidgetNameTaken(name));

            // Once the name is unique, set it.
            w.setName(name);
        }

        // Add the new widget to the undo system and to the model
        seq.addEdit(addWidget(w));

        return true;
    }

    /**
     * Delete currently selected widgets.
     */
    protected boolean deleteElements(FrameEditorSelectionState selection)
    {
        String editLabel = (selection.getElementSelectionCount() > 1)
                                ? DELETE_WIDGETS
                                : DELETE_WIDGET;

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(editLabel, FrameEditorLID.Delete);

        FrameElement[] selectedElts = selection.getSelectedIFrameElements();

        // Check to see if any items are selected
        if (selectedElts.length > 0) {
            if (interaction.confirmDeleteElements(selectedElts)) {
                Set<IWidget> frameWidgets = model.getWidgets();
                Set<FrameElementGroup> frameAssocs = model.getEltGroups();

                for (FrameElement selectedElt : selectedElts) {
                    if (selectedElt instanceof FrameElementGroup) {
                        // We need to check that the current selected group
                        // hasn't already been deleted because it is a
                        // component of another group
                        if (frameAssocs.contains(selectedElt)) {
                            deleteFrameEltGroup((FrameElementGroup) selectedElt,
                                                null,
                                                editSequence);
                        }
                    }
                    else if (selectedElt instanceof IWidget) {
                        // We need to check that the current selected widget
                        // hasn't already been deleted because it is a component
                        // of another widget (e.g., menu item part of a menu)
                        if (frameWidgets.contains(selectedElt)) {
                            deleteWidget((IWidget) selectedElt,
                                         true,
                                         null,
                                         editSequence);
                        }
                    }
                }

                editSequence.end();

                undoMgr.addEdit(editSequence);

                return true;
            }
        }
        else {
            interaction.protestNoSelection();
        }

        return false;
    } // deleteWidgets

    /**
     * This class is just for delete widget edits. It's not an anonymous class
     * due to the two abstract functions, undoHelper and redoHelper.
     * @author cmj
     */
    protected class DeleteWidgetUndoableEdit
                                      extends DemoStateManager.InvalidatingEdit
    {
        protected IWidget widget;
        protected SimpleWidgetGroup parentGroup;
        protected int atIndex;
        protected double deltaX;
        protected double deltaY;

        public DeleteWidgetUndoableEdit(IWidget w,
                                        SimpleWidgetGroup pg,
                                        int index,
                                        double dx,
                                        double dy)
        {
            super(FrameEditorLID.Delete, demoStateMgr);
            widget = w;
            parentGroup = pg;
            atIndex = index;
            deltaX = dx;
            deltaY = dy;
        }

        @Override
        public String getPresentationName()
        {
            return DELETE_WIDGET;
        }

        /**
         * This function should be overridden to remove the widget from its
         * parent group.
         */
        protected void redoHelper()
        { /* empty */ }

        /**
         * This function should be overridden to add the widget to its
         * parent group, at the appropriate index level.
         */
        protected void undoHelper()
        { /* empty */ }

        @Override
        public void redo()
        {
            super.redo();

            if (parentGroup != null) {
                int numWidgets = parentGroup.size();

                for (int i = atIndex + 1; i < numWidgets; i++) {
                    IWidget curWidget = parentGroup.get(i);
                    curWidget.moveElement(deltaX, deltaY);
                }

                redoHelper();
            }

            model.removeWidget(widget);

            stateMgr.noteWidgetEdit(widget, this);
        }

        @Override
        public void undo()
        {
            super.undo();

            if (parentGroup != null) {
                undoHelper();

                int numWidgets = parentGroup.size();

                for (int i = atIndex + 1; i < numWidgets; i++) {
                    IWidget curWidget = parentGroup.get(i);
                    curWidget.moveElement(- deltaX, - deltaY);
                }
            }

            model.addWidget(widget);

            stateMgr.noteWidgetEdit(widget, this);
        }
    }

    private void deleteGroupMember(FrameElement elt,
                                     FrameElementGroup fromGroup,
                                     IUndoableEditSequence editSequence)
    {
        if (elt instanceof IWidget) {
            deleteWidget((IWidget) elt, false, fromGroup, editSequence);
        }
        else if (elt instanceof FrameElementGroup) {
            deleteFrameEltGroup((FrameElementGroup) elt,
                                fromGroup,
                                editSequence);
        }
        else if (elt instanceof SimpleWidgetGroup) {
            SimpleWidgetGroup parentGroup = (SimpleWidgetGroup) elt;

            while (parentGroup.size() > 0) {
                IWidget child = parentGroup.get(0);

                // If the widget group is itself a remote label owner,
                // its remote label will be deleted in the first call here.
                deleteWidget(child, false, fromGroup, editSequence);
            }
        }
    }

    private void deleteFrameEltGroup(final FrameElementGroup grp,
                                       FrameElementGroup fromGroup,
                                       IUndoableEditSequence editSequence)
    {
        // Check if this element group is a remote label owner; if so, delete
        // the remote label as well
        deleteRemoteLabel(grp, editSequence);

        Iterator<FrameElement> members = grp.iterator();

        while (members.hasNext()) {
            deleteGroupMember(members.next(), grp, editSequence);
        }

        model.removeEltGroup(grp);

        removeRootElement(DELETE_GROUP, grp, fromGroup, editSequence);

        DemoStateManager.IDesignUndoableEdit edit =
            new DemoStateManager.InvalidatingEdit(FrameEditorLID.Delete,
                                                  demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return DELETE_GROUP;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    model.removeEltGroup(grp);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    model.addEltGroup(grp);
                }
            };

        editSequence.addEdit(edit);
    }

    private void removeRootElement(final String presentationName,
                                     final FrameElement rootElt,
                                     final FrameElementGroup fromGrp,
                                     IUndoableEditSequence editSequence)
    {
        Set<FrameElementGroup> rootEltGrps = rootElt.getEltGroups();
        int numGrps = rootEltGrps.size();

        if (numGrps > 0) {
            final List<FrameElementGroup> deleteGrps =
                new ArrayList<FrameElementGroup>(rootEltGrps);
            final int[] atIndexes = new int[numGrps];

            Iterator<FrameElementGroup> eltGrps = deleteGrps.iterator();
            int i = 0;

            while (eltGrps.hasNext()) {
                FrameElementGroup grp = eltGrps.next();
                atIndexes[i++] = grp.indexOf(rootElt);

                if (grp != fromGrp) {
                    grp.remove(rootElt);

                    if (grp.size() == 1) {
                        ungroup(grp, editSequence);
                        removeRootElement(presentationName, grp, fromGrp,
                                          editSequence);
                    }
                }
            }

            if (editSequence != null) {
                DemoStateManager.IDesignUndoableEdit edit =
                    new DemoStateManager.InvalidatingEdit(FrameEditorLID.Delete,
                                                          demoStateMgr)
                    {
                        @Override
                        public String getPresentationName()
                        {
                            return presentationName;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            Iterator<FrameElementGroup> eltGrps =
                                deleteGrps.iterator();

                            while (eltGrps.hasNext()) {
                                FrameElementGroup grp = eltGrps.next();

                                if (grp != fromGrp) {
                                    grp.remove(rootElt);

                                    if (grp.size() == 1) {
                                        ungroup(grp, null);
                                    }
                                }
                            }
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            Iterator<FrameElementGroup> eltGrps =
                                deleteGrps.iterator();
                            int i = 0;

                            while (eltGrps.hasNext()) {
                                FrameElementGroup grp = eltGrps.next();

                                if (grp.size() == 1) {
                                    regroup(grp);
                                }

                                grp.add(atIndexes[i++], rootElt);
                            }
                        }
                    };

                editSequence.addEdit(edit);
            }
        }
    }

    /**
     * Delete the associated remote label if this element has one.
     */
    private void deleteRemoteLabel(FrameElement elt,
                                     IUndoableEditSequence editSequence)
    {
        FrameElement asLabelOwner = elt.getRemoteLabelOwner();

        if (asLabelOwner != null) {
            IWidget remoteLabel =
                (IWidget)
                    asLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

            // If this element has a remote label, delete it
            if (remoteLabel != null) {
                deleteWidget(remoteLabel, false, null, editSequence);
            }
        }
    }

    /**
     * This deletes a widget from the model and adds the action to the undo list.
     *
     * @param w is the widget to delete.
     * @param moveSiblings is a flag - if it is true, the other widgets in the
     * group will be moved to close up the space left by the deleted widget.
     * @param editSequence is the compound edit to add this delete operation to.
     */
    private void deleteWidget(IWidget w,
                                boolean moveSiblings,
                                FrameElementGroup fromGroup,
                                IUndoableEditSequence editSequence)
    {
        // If this is a remote label, delete the attribute indicating so
        // from the point of view of its owner.
        FrameElement remoteLabelOwner =
            (FrameElement)
                w.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

        if (remoteLabelOwner != null) {
            DefaultCmd.unsetAttribute(remoteLabelOwner,
                                      demoStateMgr,
                                      WidgetAttributes.REMOTE_LABEL_ATTR,
                                      interaction,
                                      editSequence);
        }
        else {
            // Check if this widget is a remote label owner; if so, delete
            // the remote label as well
            deleteRemoteLabel(w, editSequence);
        }

        SimpleWidgetGroup parentGroup = w.getParentGroup();
        int atIndex = (parentGroup != null) ? parentGroup.indexOf(w) : -1;
        double deltaX = 0;
        double deltaY = 0;

        // If a parent widget with children (for example, submenu or menu
        // header), delete those children too. The moveSiblings flag is
        // false because there is no need to shuffle the child menus around
        // since we know every item in them will be deleted.
        if (w instanceof AParentWidget) {
            AParentWidget pw = (AParentWidget) w;

            while (pw.itemCount() > 0) {
                deleteWidget(pw.getItem(0), false, fromGroup, editSequence);
            }
        }

        // If the widget is the last object of the its root element,
        // then the root element must be removed from any containing groups
        // and, if the root element is the second to last member of any
        // group, then the group should be removed as well.
        // This must be done before the removeWidget call.
        FrameElement rootElt = w.getRootElement();

        // If the widget is its own root element, remove it from
        // containing associations
        if (rootElt == w) {
            removeRootElement(DELETE_WIDGET, rootElt, fromGroup, editSequence);
        }
        else if (rootElt instanceof SimpleWidgetGroup) {
            // Otherwise, need to do the same only if the root element
            // is a widget group that will become empty.
            SimpleWidgetGroup rootGroup = (SimpleWidgetGroup) rootElt;

            // If the widget group will become empty,
            // remove it from containing associations
            if (rootGroup.size() == 1) {
                removeRootElement(DELETE_WIDGET, rootElt, fromGroup,
                                  editSequence);
            }
        }

        model.removeWidget(w);

        if (parentGroup != null) {
            // Check if this parent group is a remote label owner; if so, delete
            // the remote label as well
            deleteRemoteLabel(parentGroup, editSequence);

            if (moveSiblings) {
                int myGroupNum = parentGroup.size();

                if (parentGroup.getOrientation() == SimpleWidgetGroup.HORIZONTAL) {
                    deltaX = - w.getEltBounds().width;
                }
                else if (parentGroup.getOrientation() == SimpleWidgetGroup.VERTICAL)
                {
                    deltaY = - w.getEltBounds().height;
                }

                // Move all widgets later than this one in the group.
                // This loop will be ineffective for grid buttons
                for (int i = atIndex + 1; i < myGroupNum; i++) {
                    IWidget curWidget = parentGroup.get(i);
                    curWidget.moveElement(deltaX, deltaY);
                }
            }

            // If it is a child widget, remove this item from its parent.
            // Otherwise, remove it from its parent group.
            if (w instanceof ChildWidget) {
                ChildWidget child = (ChildWidget) w;
                AParentWidget itemParent = child.getParent();
                itemParent.removeItem(child);
            }
            else {
                parentGroup.remove(w);
            }

            if (parentGroup instanceof GridButtonGroup) {
                GridButtonGroup gbg = (GridButtonGroup) parentGroup;
                GridButton gb = (GridButton) w;
                DoubleRectangle b = w.getEltBounds();
                double x = b.x + b.width;
                double y = b.y + b.height;
                double newHoriz = gb.getHorizSpace();
                double newVert = gb.getVertSpace();

                ReadOnlyList<GridButton> movedButtons = null;
                GridButton top = null;
                double dx = 0.0;
                double dy = 0.0;

                if (gbg.getColumn(gb).size() == 0) {
                    // w was the only widget in the column; need to move next
                    // column over
                    movedButtons = gbg.getMovedButtons(false, x, b.y);

                    if (movedButtons.size() > 0) {
                        top = movedButtons.get(0);
                        DoublePoint p = top.getShape().getOrigin();
                        dx = b.x - p.x;
                    }
                }
                else {
                    // need to move lower widgets up to fill the hole
                    movedButtons = gbg.getMovedButtons(true, b.x, y);

                    if (movedButtons.size() > 0) {
                        top = movedButtons.get(0);
                        DoublePoint p = top.getShape().getOrigin();
                        dy = b.y - p.y;
                    }
                }

                if (top != null) {
                    moveGridButton(FrameEditorLID.Delete, DELETE_WIDGET,
                                   top, dx, dy, newHoriz, newVert, editSequence);
                }
            }
        }

        DemoStateManager.IDesignUndoableEdit edit;

        // Add the deletion to the undoable history
        if (w instanceof ChildWidget) {
            ChildWidget child = (ChildWidget) w;
            final AParentWidget itemParent = child.getParent();

            edit = new DeleteWidgetUndoableEdit(w,
                                                parentGroup,
                                                atIndex,
                                                deltaX,
                                                deltaY)
                   {
                       @Override
                       public void redoHelper()
                       {
                           itemParent.removeItem((ChildWidget) widget);
                       }

                       @Override
                       public void undoHelper()
                       {
                           itemParent.addItem(atIndex,
                                              (ChildWidget) widget);
                       }
                   };
        }
        else {
            edit =
                new DeleteWidgetUndoableEdit(w,
                                             parentGroup,
                                             atIndex,
                                             deltaX,
                                             deltaY)
                {
                    @Override
                    public void redoHelper()
                    {
                        parentGroup.remove(widget);
                    }

                    @Override
                    public void undoHelper()
                    {
                        parentGroup.add(atIndex, widget);
                    }
                };
        }

        demoStateMgr.noteWidgetEdit(w, edit);
        editSequence.addEdit(edit);
    } // deleteWidget

    private void resizeElement(FrameElement elt,
                                 double oldResizeX,
                                 double oldResizeY,
                                 double newResizeX,
                                 double newResizeY,
                                 double ratioX,
                                 double ratioY,
                                 Set<SimpleWidgetGroup> resizedGroups,
                                 boolean childrenToo,
                                 CompoundUndoableEdit editSequence)
    {
        if (elt instanceof IWidget) {
            IWidget widget = (IWidget) elt;
            SimpleWidgetGroup group = widget.getParentGroup();

            if ((widget instanceof TraversableWidget) && (group != null)) {
                if (! resizedGroups.contains(group)) {
                    resizeGroup(group, oldResizeX, oldResizeY,
                                newResizeX, newResizeY, ratioX, ratioY,
                                childrenToo, editSequence);

                    resizedGroups.add(group);
                }
            }
            else {
                // Resize the actual widget.
                resizeWidget(widget, oldResizeX, oldResizeY,
                             newResizeX, newResizeY, ratioX, ratioY,
                             childrenToo, editSequence);
            }
        }
        else if (elt instanceof SimpleWidgetGroup) {
            Iterator<IWidget> widgets =
                ((SimpleWidgetGroup) elt).iterator();

            while (widgets.hasNext()) {
                resizeElement(widgets.next(), oldResizeX, oldResizeY,
                              newResizeX, newResizeY, ratioX, ratioY,
                              resizedGroups, childrenToo, editSequence);
            }
        }
        else if (elt instanceof FrameElementGroup) {
            Iterator<FrameElement> members =
                ((FrameElementGroup) elt).iterator();

            while (members.hasNext()) {
                resizeElement(members.next(), oldResizeX, oldResizeY,
                              newResizeX, newResizeY, ratioX, ratioY,
                              resizedGroups, true, editSequence);
            }
        }
    }

    /**
     * Resize the selected set of elements based on where the mouse was released
     * and the fixed point in the resize.
     *
     * While it supports multiple selection, its behavior is correct if called
     * with more then one selected widget.
     */
    private boolean resizeElements(double oldResizeX,
                                     double oldResizeY,
                                     double newResizeX,
                                     double newResizeY,
                                     double ratioX,
                                     double ratioY,
                                     FrameEditorSelectionState selection)
    {
        Iterator<FrameElement> selected =
            selection.getSelectedElementsIterator();

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit((selection.getWidgetSelectionCount() != 1)
                                         ? RESIZE_WIDGETS
                                         : RESIZE_WIDGET,
                                     FrameEditorLID.ResizeWidgets);

        Set<SimpleWidgetGroup> resizedGroups = new HashSet<SimpleWidgetGroup>();

        // Loop through selected widgets
        while (selected.hasNext()) {
            FrameElement elt = selected.next();

            resizeElement(elt, oldResizeX, oldResizeY,
                          newResizeX, newResizeY, ratioX, ratioY,
                          resizedGroups, false, editSequence);
        }

        editSequence.end();

        // Only add this edit if it is significant
        if (editSequence.isSignificant()) {
            undoMgr.addEdit(editSequence);
        }

        return true;
    }

    private void resizeGroup(SimpleWidgetGroup group,
                               double oldResizeX,
                               double oldResizeY,
                               double newResizeX,
                               double newResizeY,
                               double ratioX,
                               double ratioY,
                               boolean childrenToo,
                               CompoundUndoableEdit editSequence)
    {
        Iterator<IWidget> groupElts = group.iterator();

        while (groupElts.hasNext()) {
            IWidget groupElt = groupElts.next();

            resizeWidget(groupElt, oldResizeX, oldResizeY,
                         newResizeX, newResizeY, ratioX, ratioY,
                         childrenToo, editSequence);
        }
    }

    /**
     * Implementation on a per widget bases for resize.
     * Calculates the new size be the difference between the fixed point and the
     * released point.
     *
     * @param widget The widget to resize
     * @param mouseReleased The point where the mouse was let up
     * @param fixedPoint The point which will not move after resize
     * @param edit The Compound undoable edit
     */
    private void resizeWidget(final IWidget widget,
                                double oldResizeX,
                                double oldResizeY,
                                double newResizeX,
                                double newResizeY,
                                final double ratioX,
                                final double ratioY,
                                final boolean childrenToo,
                                IUndoableEditSequence editSequence)
    {
        // Get the old shape bounds for the undo
        DoubleRectangle oldBounds = widget.getEltBounds();

        final double oldWidgetX = oldBounds.x;
        final double oldWidgetY = oldBounds.y;
        final double oldWidth = oldBounds.width;
        final double oldHeight = oldBounds.height;

        final double newWidgetX =
            ratioX * (oldBounds.x - oldResizeX) + newResizeX;
        final double newWidgetY =
            ratioY * (oldBounds.y - oldResizeY) + newResizeY;
        final double newWidth = Math.max(ratioX * oldBounds.width, 1.0);
        final double newHeight = Math.max(ratioY * oldBounds.height, 1.0);

        final double oldHoriz = (widget instanceof GridButton)
                                     ? ((GridButton) widget).getHorizSpace()
                                     : 0.0;
        final double oldVert = (widget instanceof GridButton)
                                     ? ((GridButton) widget).getVertSpace()
                                     : 0.0;

        // Actually make the changes to the model.
        widget.setWidgetOrigin(newWidgetX, newWidgetY);
        widget.setWidgetSize(newWidth, newHeight);

        if (widget instanceof GridButton) {
            GridButton gb = (GridButton) widget;
            gb.setHorizSpace(oldHoriz * ratioX);
            gb.setVertSpace(oldVert * ratioY);
        }
        else if (widget instanceof AParentWidget) {
            if (childrenToo) {
                resizeChildren(widget);
            }

            DesignEditorCmd.repositionChildren((AParentWidget) widget);
        }

        // Add the undo support
        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(FrameEditorLID.ResizeWidgets,
                                                demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return RESIZE_WIDGET;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    widget.setWidgetOrigin(newWidgetX, newWidgetY);
                    widget.setWidgetSize(newWidth, newHeight);

                    if (widget instanceof GridButton) {
                        GridButton gb = (GridButton) widget;
                        gb.setHorizSpace(oldHoriz * ratioX);
                        gb.setVertSpace(oldVert * ratioY);
                    }
                    else if (widget instanceof AParentWidget) {
                        if (childrenToo) {
                            resizeChildren(widget);
                        }

                        DesignEditorCmd.repositionChildren((AParentWidget) widget);
                    }

                    noteEditCheckRegenerate(widget, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    widget.setWidgetOrigin(oldWidgetX, oldWidgetY);
                    widget.setWidgetSize(oldWidth, oldHeight);

                    if (widget instanceof GridButton) {
                        GridButton gb = (GridButton) widget;
                        gb.setHorizSpace(oldHoriz);
                        gb.setVertSpace(oldVert);
                    }
                    else if (widget instanceof AParentWidget) {
                        if (childrenToo) { // technically, this won't restore the sizes if they were changed manuallythes
                            resizeChildren(widget);
                        }

                        DesignEditorCmd.repositionChildren((AParentWidget) widget);
                    }

                    noteEditCheckRegenerate(widget, this);
                }
            };

        noteEditCheckRegenerate(widget, edit);
        editSequence.addEdit(edit);
    } // resizeWidget

    /**
     * Move the list of selected Widgets and set up undo support for that.
     * Uses a compound Undo
     *
     * @param selection Iterator of items to move
     */
    private boolean moveElements(FrameEditorSelectionState selection,
                                   double widgetMoveByX,
                                   double widgetMoveByY)
    {
        return moveElements(selection, widgetMoveByX, widgetMoveByY, true);
    }

    private void moveWidget(ListenerIdentifier lid,
                              String presentationLabel,
                              IWidget widget,
                              double widgetMoveByX,
                              double widgetMoveByY,
                              boolean moveAsGroup,
                              Set<SimpleWidgetGroup> movedGroups,
                              Set<IWidget> movedWidgets,
                              IUndoableEditSequence editSequence)
    {
        if (movedWidgets.contains(widget)) {
            return;
        }
        movedWidgets.add(widget);
        SimpleWidgetGroup parentGroup = widget.getParentGroup();

        if ((parentGroup == null) || (! moveAsGroup)) {
            if (widget instanceof GridButton) {
                GridButton gb = (GridButton) widget;
                moveGridButton(lid, presentationLabel,
                               gb, widgetMoveByX, widgetMoveByY,
                               gb.getHorizSpace() + widgetMoveByX,
                               gb.getVertSpace() + widgetMoveByY,
                               editSequence);
            }
            else {
                moveWidget(lid, presentationLabel,
                           widget, widgetMoveByX, widgetMoveByY,
                           editSequence);
            }
        }
        else if (! (widget instanceof ChildWidget)) {
            // If widget is part of a group that hasn't been moved yet,
            // move entire group
            if (! movedGroups.contains(parentGroup)) {
                moveWidgetGroup(lid, presentationLabel,
                                parentGroup, widgetMoveByX, widgetMoveByY,
                                editSequence);

                movedGroups.add(parentGroup);
            }
        }
    }

    private void moveElement(ListenerIdentifier lid,
                               String presentationLabel, FrameElement frameElt,
                               double widgetMoveByX,
                               double widgetMoveByY,
                               boolean moveAsGroup,
                               Set<SimpleWidgetGroup> movedGroups,
                               Set<IWidget> movedWidgets,
                               IUndoableEditSequence editSequence)
    {
        if (frameElt instanceof IWidget) {
            moveWidget(lid, presentationLabel,
                       (IWidget) frameElt, widgetMoveByX, widgetMoveByY,
                       moveAsGroup, movedGroups, movedWidgets,
                       editSequence);
        }
        else if (frameElt instanceof SimpleWidgetGroup) {
            for (IWidget w : (SimpleWidgetGroup)frameElt) {
                moveWidget(lid, presentationLabel,
                           w, widgetMoveByX, widgetMoveByY,
                           moveAsGroup, movedGroups, movedWidgets,
                           editSequence);
            }
        }
        else if (frameElt instanceof FrameElementGroup) {
            for (FrameElement fe : (FrameElementGroup)frameElt) {
                moveElement(lid, presentationLabel,
                            fe, widgetMoveByX, widgetMoveByY,
                            moveAsGroup, movedGroups, movedWidgets,
                            editSequence);
            }
        }
    }

    private boolean isMemberOfSelectedGroup(FrameElement elt,
                                              FrameEditorSelectionState seln)
    {
        FrameElement rootElt = elt.getRootElement();
        Iterator<FrameElementGroup> grps = rootElt.getEltGroups().iterator();

        // If a selected item is a member of an FrameElementGroup
        // that is also selected, skip it
        while (grps.hasNext()) {
            if (seln.isElementSelected(grps.next())) {
                return true;
            }
        }

        return false;
    }

    private boolean moveElements(FrameEditorSelectionState selection,
                                   double moveByX,
                                   double moveByY,
                                   boolean moveAsGroup)
    {
        String editLabel;

        if (selection.getWidgetSelectionCount() == 1) {
            editLabel = MOVE_WIDGET;
        }
        else {
            editLabel = MOVE_WIDGETS;
        }

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(editLabel,
                                     FrameEditorLID.MoveWidgets);

        FrameElement[] selected = selection.getSelectedIFrameElements();

        // Avoid moving a group more than once
        Set<SimpleWidgetGroup> movedGroups = new HashSet<SimpleWidgetGroup>();
        Set<IWidget> movedWidgets = new HashSet<IWidget>();

        // Move all selected widgets by the specified amount
        for (FrameElement eltToMove : selected) {
            if (! isMemberOfSelectedGroup(eltToMove, selection)) {
                moveElement(FrameEditorLID.MoveWidgets, editLabel,
                            eltToMove, moveByX, moveByY,
                            moveAsGroup, movedGroups, movedWidgets,
                            editSequence);
            }
        }

        editSequence.end();

        // Only add this edit if it is significant
        if (editSequence.isSignificant()) {
            undoMgr.addEdit(editSequence);
        }

        return true;
    }

    /**
     * Special case of moveWidget; moves all buttons below or to the right of
     * gb in its group and updates the horizontal and vertical offsets as
     * necessary.
     */
    private void moveGridButton(ListenerIdentifier lid,
                                  String presentationLabel,
                                  GridButton gb,
                                  double widgetMoveByX,
                                  double widgetMoveByY,
                                  double newHoriz,
                                  double newVert,
                                  IUndoableEditSequence editSequence)
    {
        // move the list of buttons affected
        DoublePoint oldLocation = gb.getShape().getOrigin();
        double oldX = oldLocation.x;
        double oldY = oldLocation.y;
        GridButtonGroup gbg = (GridButtonGroup) gb.getParentGroup();
        boolean vertical = (widgetMoveByX == 0);
        double oldHoriz = gb.getHorizSpace();
        double oldVert = gb.getVertSpace();

        ReadOnlyList<? extends GridButton> movedButtons =
            gbg.getMovedButtons(vertical, oldX, oldY);

        for (int i = 0; i < movedButtons.size(); i++) {
            GridButton g = movedButtons.get(i);

            DoublePoint p = g.getShape().getOrigin();

            double tempX = Math.max(p.x + widgetMoveByX, 0.0);
            double tempY = Math.max(p.y + widgetMoveByY, 0.0);

            g.setWidgetOrigin(tempX, tempY);
        }

        if (vertical) {
            gb.setVertSpace(newVert);
        }
        else {
            List<GridButton> column = gbg.getColumn(gb);

            for (int i = 0; i < column.size(); i++) {
                GridButton g = column.get(i);
                g.setHorizSpace(newHoriz);
            }
        }

        DemoStateManager.IDesignUndoableEdit edit =
            moveGridButtonsEdit(lid, presentationLabel,
                                movedButtons, widgetMoveByX, widgetMoveByY,
                                oldHoriz, oldVert, newHoriz, newVert, gb);

        editSequence.addEdit(edit);
    }

    /**
     * Move a single widget.
     * Add the move to the list of undoable edits using compound edit passed in
     *
     * Enforce that the move does not move the origin past 0,0
     *
     * @param w
     * @param e
     */
    private void moveWidget(ListenerIdentifier lid,
                              String presentationLabel,
                              IWidget w,
                              double widgetMoveByX,
                              double widgetMoveByY,
                              IUndoableEditSequence editSequence)
    {
        // Keep track of old and new locations, ensuring not less than zero
        DoublePoint oldLocation = w.getShape().getOrigin();
        double oldX = oldLocation.x;
        double oldY = oldLocation.y;
        double newX = Math.max(oldX + widgetMoveByX, 0.0);
        double newY = Math.max(oldY + widgetMoveByY, 0.0);

        // Change model and create undo.
        w.setWidgetOrigin(newX, newY);

        DemoStateManager.ObsoletingEdit edit =
            new WidgetMoveUndoRedo(lid, presentationLabel,
                                   w, newX, newY, oldX, oldY);

        noteEditCheckRegenerate(w, edit);
        editSequence.addEdit(edit);
    } // moveWidget

    private void moveWidgetGroup(ListenerIdentifier lid,
                                   final String presentationLabel,
                                   final SimpleWidgetGroup group,
                                   double widgetMoveByX,
                                   double widgetMoveByY,
                                   IUndoableEditSequence editSequence)
    {
        // Keep track of old and new locations, ensuring not less than zero
        IWidget firstChildWidget = group.get(0);
        DoublePoint oldLocation = firstChildWidget.getShape().getOrigin();
        final double deltaX = Math.max(widgetMoveByX, - oldLocation.x);
        final double deltaY = Math.max(widgetMoveByY, - oldLocation.y);
        final int numWidgets = group.size();
        List<IWidget> groupWidgets = new ArrayList<IWidget>();

        // Change model and create undo.
        for (int i = 0; i < numWidgets; i++) {
            IWidget widget = group.get(i);
            groupWidgets.add(widget);
            widget.moveElement(deltaX, deltaY);
        }

        final ReadOnlyList<? extends IWidget> roGroupWidgets =
            new ReadOnlyList<IWidget>(groupWidgets);

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return presentationLabel;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    for (int i = 0; i < numWidgets; i++) {
                        IWidget w = roGroupWidgets.get(i);

                        w.moveElement(deltaX, deltaY);
                    }

                    noteEditCheckRegenerate(roGroupWidgets, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    for (int i = 0; i < numWidgets; i++) {
                        IWidget w = roGroupWidgets.get(i);

                        w.moveElement(- deltaX, - deltaY);
                    }

                    noteEditCheckRegenerate(roGroupWidgets, this);
                }
            };

        noteEditCheckRegenerate(roGroupWidgets, edit);
        editSequence.addEdit(edit);
    } // moveWidgetGroup

    private IDesignUndoableEdit moveGridButtonsEdit(ListenerIdentifier lid,
                                                      final String presentationLabel,
                                                      final ReadOnlyList<? extends GridButton> movedButtons,
                                                      final double widgetMoveByX,
                                                      final double widgetMoveByY,
                                                      final double oldHoriz,
                                                      final double oldVert,
                                                      final double newHoriz,
                                                      final double newVert,
                                                      final GridButton gb)
    {
        final GridButtonGroup gbg = (GridButtonGroup) gb.getParentGroup();

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return presentationLabel;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    for (int i = 0; i < movedButtons.size(); i++) {
                        GridButton g = movedButtons.get(i);

                        DoublePoint p = g.getShape().getOrigin();

                        double tempX = Math.max(p.x + widgetMoveByX, 0.0);
                        double tempY = Math.max(p.y + widgetMoveByY, 0.0);

                        g.setWidgetOrigin(tempX, tempY);
                    }

                    if (widgetMoveByX == 0) {
                        gb.setVertSpace(newVert);
                    }
                    else {
                        List<GridButton> column = gbg.getColumn(gb);

                        for (int i = 0; i < column.size(); i++) {
                            GridButton g = column.get(i);
                            g.setHorizSpace(newHoriz);
                        }
                    }

                    noteEditCheckRegenerate(movedButtons, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    for (int i = 0; i < movedButtons.size(); i++) {
                        GridButton g = movedButtons.get(i);

                        DoublePoint p = g.getShape().getOrigin();

                        double tempX = Math.max(p.x - widgetMoveByX, 0.0);
                        double tempY = Math.max(p.y - widgetMoveByY, 0.0);

                        g.setWidgetOrigin(tempX, tempY);
                    }

                    if (widgetMoveByX == 0) {
                        gb.setVertSpace(oldVert);
                    }
                    else {
                        List<GridButton> column = gbg.getColumn(gb);

                        for (int i = 0; i < column.size(); i++) {
                            GridButton g = column.get(i);
                            g.setHorizSpace(oldHoriz);
                        }
                    }

                    noteEditCheckRegenerate(movedButtons, this);
                }
            };

        noteEditCheckRegenerate(movedButtons, edit);

        return edit;
    }

    /**
     * Changes the level of the selected widget
     *
     * @param newLevel the desired level for the widget
     */
    private void adjustWidgetLevel(final int newLevel,
                                     final IWidget widget,
                                     IUndoableEditSequence editSequence,
                                     ListenerIdentifier lid)
    {
        // Get the old widget level
        final int oldLevel = widget.getLevel();

        // Update the model
        model.setWidgetLevel(newLevel, widget);

        // add undo support
        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(lid, demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return CHG_WIDGET_LEVEL;
                }

                @Override
                public void redo()
                {
                    super.redo();
                    model.setWidgetLevel(newLevel, widget);
                    noteEditCheckRegenerate(widget, this);
                }

                @Override
                public void undo()
                {
                    super.undo();
                    model.setWidgetLevel(oldLevel, widget);
                    noteEditCheckRegenerate(widget, this);
                }
            };

        noteEditCheckRegenerate(widget, edit);
        editSequence.addEdit(edit);
    }

    /**
     * Used to create a set of selected frame elements sorted by
     * "widget" level, where each frame element group is assigned the maximum
     * level of the widgets it contains.
     */
    private static Comparator<FrameElement> elementLevelComparator =
        new Comparator<FrameElement>() {
            protected int getMaxLevel(FrameElementGroup g)
            {
                int level = 0;

                Iterator<FrameElement> elements = g.iterator();

                while (elements.hasNext()) {
                    FrameElement elt = elements.next();
                    int compareLevel = 0;

                    if (elt instanceof IWidget) {
                        compareLevel = ((IWidget) elt).getLevel();
                    }
                    else if (elt instanceof SimpleWidgetGroup) {
                        SimpleWidgetGroup wg = (SimpleWidgetGroup) elt;

                        if (wg.size() > 0) {
                            compareLevel = wg.get(0).getLevel();
                        }
                    }
                    else if (elt instanceof FrameElementGroup) {
                        compareLevel = getMaxLevel((FrameElementGroup) elt);
                    }

                    if (level < compareLevel) {
                        level = compareLevel;
                    }
                }

                return level;
            }


            public int compare(FrameElement l, FrameElement r)
            {
                if (l instanceof IWidget) {
                    IWidget lw = (IWidget) l;

                    if (r instanceof IWidget) {
                        IWidget rw = (IWidget) r;

                        return Widget.WidgetLevelComparator.ONLY.compare(lw, rw);
                    }

                    if (r instanceof FrameElementGroup) {
                        return lw.getLevel() - getMaxLevel((FrameElementGroup) r);
                    }
                }
                else if (l instanceof FrameElementGroup) {
                    int lLevel = getMaxLevel((FrameElementGroup) l);

                    if (r instanceof IWidget) {
                        return lLevel - ((IWidget) r).getLevel();
                    }

                    if (r instanceof FrameElementGroup) {
                        return lLevel - getMaxLevel((FrameElementGroup) r);
                    }
                }

                return 0;   // shouldn't get here, but what else to do?
            }
        };

    /**
     * Used to determine the horizontal relationship of 2 widgets
     * This is used in the calculation of alignment, and spacing.
     * Widget1 to the left of Widget2 returns a < 0 number
     */
    private static Comparator<FrameElement> elementHorizontalComparator =
        new Comparator<FrameElement>() {

            public int compare(FrameElement l, FrameElement r)
            {
                // Compare the left & right for ordering
                DoubleRectangle left = l.getEltBounds();
                DoubleRectangle right = r.getEltBounds();

                double diff = left.x - right.x;

                return (diff < 0) ? -1 : ((diff > 0) ? 1 : 0);
            }
        };

    /**
     * Used to determine the Vertical relationship of 2 widgets
     * This is used in the calculation of alignment, and spacing.
     * Widget1 above Widget2 returns a < 0 number
     */
    private static Comparator<FrameElement> elementVerticalComparator =
        new Comparator<FrameElement>() {

            public int compare(FrameElement u, FrameElement l)
            {
                // Compare up and down for ordering.
                DoubleRectangle upper = u.getEltBounds();
                DoubleRectangle lower = l.getEltBounds();

                double diff = upper.y - lower.y;

                return (diff < 0) ? -1 : ((diff > 0) ? 1 : 0);
            }
        };

    /**
     * Used to sort group members by location (by column and row).
     */
    private static Comparator<FrameElement> groupMemberComparator =
        new Comparator<FrameElement>() {

            public int compare(FrameElement lhs, FrameElement rhs)
            {
                if (lhs == rhs) {
                    return 0;
                }

                DoubleRectangle lhsBds = lhs.getEltBounds();
                DoubleRectangle rhsBds = rhs.getEltBounds();

                // lhs comes before rhs if its column is to the left OR
                // it is in the same column and it comes above
                if ((lhsBds.x < rhsBds.x) ||
                    ((lhsBds.y < rhsBds.y) &&
                     PrecisionUtilities.withinEpsilon(lhsBds.x,
                                                      rhsBds.x,
                                                      GridButtonGroup.PIXEL_EPSILON)))
                {
                    return -1;
                }

                return 1;
            }
        };

    /**
     * Spaces the selected frame elements equally along a certain axis
     * @param vertical true for vertical axis; false for horizontal
     */
    private boolean spaceElementsEqually(FrameEditorSelectionState selection,
                                           boolean vertical)
    {
        // Order the widgets according to location
        // (either from the left or from the top)
        Comparator<FrameElement> c = vertical ? elementVerticalComparator
                                               : elementHorizontalComparator;
        Set<FrameElement> elements = getSelectedElements(selection, c);

        if (elements.size() <= 2) {
            interaction.protestTooFewElements();
            return false;
        }

        // Calculate the spacing between widgets
        double sum = 0;
        double min = Double.MAX_VALUE;
        double max = Double.MIN_VALUE;

        // Go through each element that is selected
        // Determine the size, min & max of the region.
        // this can then be used to do spacing.
        Iterator<FrameElement> eltIter = elements.iterator();

        while (eltIter.hasNext()) {
            DoubleRectangle bounds = eltIter.next().getEltBounds();

            double size = vertical ? bounds.height : bounds.width;
            double position = vertical ? bounds.y : bounds.x;

            sum += size;
            min = Math.min(min, position);
            max = Math.max(max, size + position);
        }

        // Get the spacing to use between each item.
        double spacing = ((max - min) - sum) / (elements.size() - 1);

        String undoRedoLabel =
            vertical ? SPACE_VERTICALLY : SPACE_HORIZONTALLY;

        CogToolLID lid = vertical ? FrameEditorLID.SpaceVertically
                                  : FrameEditorLID.SpaceHorizontally;

        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(undoRedoLabel, lid);

        // Avoid moving a group more than once
        Set<SimpleWidgetGroup> movedGroups = new HashSet<SimpleWidgetGroup>();
        Set<IWidget> movedWidgets = new HashSet<IWidget>();

        // Adjust the spacings to the correct values and go through
        // each element performing the appropriate move.
        eltIter = elements.iterator();

        while (eltIter.hasNext()) {
            FrameElement elt = eltIter.next();
            DoubleRectangle bounds = elt.getEltBounds();

            // Determine the amount to move each element
            double deltaX = vertical ? 0.0 : (min - bounds.x);
            double deltaY = vertical ? (min - bounds.y) : 0.0;

            // Set the new location, adding the undoable edit
            moveElement(lid, undoRedoLabel,
                        elt, deltaX, deltaY,
                        true, movedGroups, movedWidgets,
                        editSequence);

            // Advance the pointer to the next location
            min += spacing + (vertical ? bounds.height : bounds.width);
        }

        editSequence.end();

        // Only add this edit if it is significant
        if (editSequence.isSignificant()) {
            undoMgr.addEdit(editSequence);
        }

        return true;
    }

    /**
     * Update the widget's name.
     * The name must be unique.
     *
     * @param widget
     * @param newName
     * @return
     */
    private boolean updateWidgetName(final IWidget widget, String tryName)
    {
        final String oldName = widget.getName();

        boolean notDone = true;

        do {
            if (tryName.length() == 0) {
                tryName =
                    interaction.protestNameCannotBeEmpty(DEFAULT_WIDGET_PREFIX);

                // If canceled, indicate so; otherwise, test again
                if (tryName == null) {
                    return false;
                }
            }
            else {
                IWidget widgetForName = model.getWidget(tryName);

                // If the widget for the given name is the same,
                // then no change is necessary!
                if (widgetForName == widget) {
                    notDone = false;
                }
                else if (widgetForName != null) {
                    // A non-null widget for the tryName indicates a collision
                    tryName =
                        interaction.protestNameCollision(DEFAULT_WIDGET_PREFIX);

                    // If canceled, indicate so; otherwise, test again
                    if (tryName == null) {
                        return false;
                    }
                }
                else {
                    // Not canceled, not empty, and not a collision
                    notDone = false;

                    final String newName = tryName;

                    model.setWidgetName(newName, widget);

                    DemoStateManager.ObsoletingEdit edit =
                        new DemoStateManager.ObsoletingEdit(FrameEditorLID.ChangeNameProperty,
                                                            demoStateMgr)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return CHG_WIDGET_NAME;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();
                                model.setWidgetName(newName, widget);
                                noteEditCheckRegenerate(widget, this);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();
                                model.setWidgetName(oldName, widget);
                                noteEditCheckRegenerate(widget, this);
                            }
                        };

                    noteEditCheckRegenerate(widget, edit);
                    undoMgr.addEdit(edit);
                }
            }
        } while (notDone);

        return true;
    }

    private boolean renameEltGroup(final FrameElementGroup eltGroup,
                                     String tryName)
    {
        final String oldName = eltGroup.getName();

        boolean notDone = true;

        do {
            if (tryName.length() == 0) {
                tryName =
                    interaction.protestNameCannotBeEmpty(DEFAULT_GROUP_PREFIX);

                // If canceled, indicate so; otherwise, test again
                if (tryName == null) {
                    return false;
                }
            }
            else {
                FrameElementGroup groupForName =
                    model.getEltGroup(tryName);

                // If the group for the given name is the same,
                // then no change is necessary!
                if (groupForName == eltGroup) {
                    notDone = false;
                }
                else if (groupForName != null) {
                    // A non-null widget for the tryName indicates a collision
                    tryName =
                        interaction.protestNameCollision(DEFAULT_GROUP_PREFIX);

                    // If canceled, indicate so; otherwise, test again
                    if (tryName == null) {
                        return false;
                    }
                }
                else {
                    // Not canceled, not empty, and not a collision
                    notDone = false;

                    final String newName = tryName;

                    model.setEltGroupName(newName, eltGroup);

                    IUndoableEdit edit =
                        new AUndoableEdit(FrameEditorLID.ChangeNameProperty)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return CHG_WIDGET_NAME;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();
                                model.setEltGroupName(newName, eltGroup);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();
                                model.setEltGroupName(oldName, eltGroup);
                            }
                        };

                    undoMgr.addEdit(edit);
                }
            }
        } while (notDone);

        return true;
    } // renameEltGroup

    /**
     * Update the frame's name.
     * The name must be unique.
     *
     * @param frame
     * @param newName
     * @return
     */
    private boolean updateFrameName(final Frame frame, String tryName)
    {
        final String oldName = frame.getName();

        boolean notDone = true;

        do {
            if (tryName.length() == 0) {
                tryName =
                    interaction.protestNameCannotBeEmpty("Frame");

                // If canceled, indicate so; otherwise, test again
                if (tryName == null) {
                    return false;
                }
            }
            else {
                Frame frameForName = model.getDesign().getFrame(tryName);

                // If the widget for the given name is the same,
                // then no change is necessary!
                if (frameForName == frame) {
                    notDone = false;
                }
                else if (frameForName != null) {
                    // A non-null widget for the tryName indicates a collision
                    tryName =
                        interaction.protestNameCollision("Frame");

                    // If canceled, indicate so; otherwise, test again
                    if (tryName == null) {
                        return false;
                    }
                }
                else {
                    // Not canceled, not empty, and not a collision
                    notDone = false;

                    final String newName = tryName;

                    frame.setName(newName);

                    IUndoableEdit edit =
                        new AUndoableEdit(DesignEditorLID.RenameFrame)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return CHG_FRAME_NAME;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();
                                Frame testFrame = design.getFrame(newName);
                                frame.setName(newName);

                                if (testFrame != null) {
                                	makeFrameNameUnique(frame);
                                }
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();
                                Frame testFrame = design.getFrame(oldName);
                                frame.setName(oldName);

                                if (testFrame != null) {
                                	makeFrameNameUnique(frame);
                                }
                            }
                        };

                    undoMgr.addEdit(edit);
                }
            }
        } while (notDone);

        return true;
    }

    protected static class DuplicateWidgetSituator
                                       extends SimpleWidgetGroup.AWidgetDuplicator
    {
        protected Map<IWidget, IWidget> widgetCopies;
        protected Map<FrameElementGroup, FrameElementGroup> groupCopies;


        public void placeInContext(IWidget origWidget, IWidget widgetCopy)
        {
            widgetCopies.put(origWidget, widgetCopy);
        }


        public IWidget getDuplicate(IWidget origWidget)
        {
            return widgetCopies.get(origWidget);
        }

        public void reset(Map<IWidget, IWidget> wCopies,
                          Map<FrameElementGroup, FrameElementGroup> gCopies)
        {
            reset();
            widgetCopies = wCopies;
            groupCopies = gCopies;
        }

        public void setGroupDuplicate(FrameElementGroup original,
                                      FrameElementGroup copy)
        {
            groupCopies.put(original, copy);
        }

        @Override
        public void completeWork()
        {
            super.completeWork();

            // Avoid leak
            widgetSituator.reset(null, null);
        }
    }

    private static DuplicateWidgetSituator widgetSituator =
        new DuplicateWidgetSituator();

    private Iterator<IWidget> sortSelectedMbrs(SimpleWidgetGroup group,
                                                 FrameEditorSelectionState s)
    {
        Set<IWidget> sortingSet = new TreeSet<IWidget>(groupMemberComparator);

        Iterator<IWidget> groupMbrs = group.iterator();

        while (groupMbrs.hasNext()) {
            IWidget member = groupMbrs.next();

            if ((s == null) || s.isElementSelected(member)) {
                sortingSet.add(member);
            }
        }

        return sortingSet.iterator();
    }

    // Assumes that widget is an instance of MenuHeader, ListItem, or
    // GridButton
    private IWidget duplicateGroupMember(IWidget widget,
                                           FrameEditorSelectionState selection,
                                           double moveByX,
                                           double moveByY)
    {
        SimpleWidgetGroup existingGroup = widget.getParentGroup();
        SimpleWidgetGroup newGroup = widgetSituator.getGroup(existingGroup);

        // If the first time this group has been seen, populate.
        if (newGroup.size() == 0) {
            Iterator<IWidget> widgets =
                sortSelectedMbrs(existingGroup, selection);

            while (widgets.hasNext()) {
                Frame.duplicateWidget(widgets.next(),
                                      lookupFrameDuplicator,
                                      widgetSituator,
                                      moveByX,
                                      moveByY);
            }

            DesignEditorCmd.repositionChildren(newGroup);
        }

        return widgetSituator.getDuplicate(widget);
    } // duplicateGroupMember

    private IWidget duplicateWidget(IWidget widget,
                                      FrameEditorSelectionState selection,
                                      double moveByX,
                                      double moveByY)
    {
        // If already copied, simply return the copy.
        IWidget widgetCopy = widgetSituator.getDuplicate(widget);

        if (widgetCopy != null) {
            return widgetCopy;
        }

        // Avoid children
        // IChildWidget returns null; this is ok for now since the only
        // invocation that cares about the return value is the recursive
        // call for the remote "label" in duplicateRemoteLabel().
        if (widget instanceof ChildWidget) {
            return null;
        }

        // Test this first to catch MenuHeader
        // (also gets GridButton and ListItem)
        if (widget.getParentGroup() != null) {
            widgetCopy =
                duplicateGroupMember(widget, selection, moveByX, moveByY);
        }
        else {
            // This gets PullDownHeader and ContextMenu (but not
            // MenuHeader/Item) and all else but MenuItem and PullDownItem
            // (i.e., children)
            // TODO: Is there any reasonable semantics to
            //       duplicating menu/pull-down items?
            widgetCopy = Frame.duplicateWidget(widget,
                                               lookupFrameDuplicator,
                                               widgetSituator,
                                               moveByX,
                                               moveByY);
        }

        // If this is a remote label, then check if the owner is also being
        // duplicated; if so, keep the remote label as a remote label.
        // If not, remote the attribute on the copy.
        FrameElement remoteLabelOwner =
            (FrameElement)
                widget.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

        if (remoteLabelOwner != null) {
            if ((selection != null) &&
                ! selection.isElementSelected(remoteLabelOwner))
            {
                // Remove "label"-ness
                widgetCopy.unsetAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);
            }
            // otherwise, when the owner gets duplicated, it will replace
            // the owner of this label with its copy; see duplicateRemoteLabel!
        }
        else {
            Frame.duplicateRemoteLabel(widget, widgetCopy,
                                       lookupFrameDuplicator, widgetSituator,
                                       moveByX, moveByY);
        }

        return widgetCopy;
    } // duplicateWidget

    private FrameElementGroup duplicateFrameEltGroup(FrameElementGroup grp,
                                                        double moveByX,
                                                        double moveByY)
    {
        // Temporarily assign the same name; when added to the Frame
        // we will ensure the name is then unique
        FrameElementGroup newEltGrp = grp.twin();

        Iterator<FrameElement> eltsToDup = grp.iterator();

        while (eltsToDup.hasNext()) {
            FrameElement elt = eltsToDup.next();
            FrameElement eltCopy = null;

            if (elt instanceof IWidget) {
                IWidget widget = (IWidget) elt;

                duplicateWidget(widget, null, moveByX, moveByY);

                eltCopy = widgetSituator.getDuplicate(widget);
            }
            else if (elt instanceof SimpleWidgetGroup) {
                SimpleWidgetGroup group = (SimpleWidgetGroup) elt;
                SimpleWidgetGroup groupCopy = widgetSituator.getGroup(group);
                Iterator<IWidget> groupWidgets = group.iterator();

                while (groupWidgets.hasNext()) {
                    duplicateGroupMember(groupWidgets.next(),
                                         null, moveByX, moveByY);
                }

                Frame.duplicateRemoteLabel(group, groupCopy,
                                           lookupFrameDuplicator,
                                           widgetSituator,
                                           moveByX, moveByY);

                eltCopy = groupCopy;
            }
            else if (elt instanceof FrameElementGroup) {
                eltCopy = duplicateFrameEltGroup((FrameElementGroup) elt,
                                                 moveByX, moveByY);
            }

            if (eltCopy != null) {
                newEltGrp.add(eltCopy);
            }
        }

        widgetSituator.setGroupDuplicate(grp, newEltGrp);

        Frame.duplicateRemoteLabel(grp, newEltGrp,
                                   lookupFrameDuplicator, widgetSituator,
                                   moveByX, moveByY);

        return newEltGrp;
    } // duplicateFrameEltGroup

    private IListenerAction duplicateWidgetsAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.DuplicateParameters.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.DuplicateParameters parameters = (FrameEditorUI.DuplicateParameters) prms;

                int elementCount =
                    parameters.selection.getElementSelectionCount();

                // If selection is non zero, copy widgets
                if (elementCount > 0) {
                    Map<IWidget, IWidget> widgetCopies =
                    	new LinkedHashMap<IWidget, IWidget>();
                    final Map<FrameElementGroup, FrameElementGroup> groupCopies =
                        new LinkedHashMap<FrameElementGroup, FrameElementGroup>();

                    widgetSituator.reset(widgetCopies, groupCopies);

                    Set<FrameElement> selectedElements =
                        getSelectedElements(parameters.selection,
                                            elementLevelComparator);
                    Iterator<FrameElement> elements =
                        selectedElements.iterator();

                    // Iterate through the widgets and duplicate.
                    while (elements.hasNext()) {
                        FrameElement elt = elements.next();

                        if (elt instanceof IWidget) {
                            duplicateWidget((IWidget) elt,
                                            parameters.selection,
                                            parameters.moveByX,
                                            parameters.moveByY);
                        }
                        else if (elt instanceof FrameElementGroup) {
                            duplicateFrameEltGroup((FrameElementGroup) elt,
                                                   parameters.moveByX,
                                                   parameters.moveByY);
                        }
                    }

                    widgetSituator.completeWork();

                    final Iterable<? extends IWidget> widgetDuplicates =
                        new ReadOnlyList<IWidget>(new ArrayList<IWidget>(widgetCopies.values()));
                    Iterator<? extends IWidget> copiedWidgets =
                        widgetDuplicates.iterator();

                    Set<GridButtonGroup> dupGridGroups =
                    	new HashSet<GridButtonGroup>();

                    while (copiedWidgets.hasNext()) {
                        IWidget widgetCopy = copiedWidgets.next();

                        // Warning: it is important that each widget be added
                        // to the frame *before* we make the next widget name
                        // unique or we can end up with non-unique names.
                        makeWidgetNameUnique(widgetCopy);
                        model.addWidget(widgetCopy);

                        if (widgetCopy instanceof GridButton) {
                            GridButtonGroup gbg =
                                (GridButtonGroup) widgetCopy.getParentGroup();

                            // Avoid recalculating offsets more than once
                            // for each grid button group
                            if (! dupGridGroups.contains(gbg)) {
                                gbg.recalculateOffsets();
                                dupGridGroups.add(gbg);
                            }
                        }
                    }

                    Iterator<FrameElementGroup> copiedGroups =
                        groupCopies.values().iterator();

                    while (copiedGroups.hasNext()) {
                        FrameElementGroup copiedGroup = copiedGroups.next();

                        // Ensure name is unique, then add to frame immediately
                        // Otherwise, it would be possible to generate presumed
                        // unique names that weren't.
                        makeEltGroupNameUnique(copiedGroup);

                        model.addEltGroup(copiedGroup);
                    }

                    DemoStateManager.IDesignUndoableEdit edit =
                        new DemoStateManager.InvalidatingEdit(FrameEditorLID.Duplicate,
                                                              demoStateMgr)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return DUPLICATE_WIDGETS;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                Iterator<? extends IWidget> addCopies =
                                    widgetDuplicates.iterator();

                                while (addCopies.hasNext()) {
                                    IWidget widgetCopy = addCopies.next();

                                    model.addWidget(widgetCopy);
                                }

                                Iterator<FrameElementGroup> copiedGroups =
                                    groupCopies.values().iterator();

                                while (copiedGroups.hasNext()) {
                                    model.addEltGroup(copiedGroups.next());
                                }

                                demoStateMgr.noteWidgetsEdit(widgetDuplicates, this);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                Iterator<? extends IWidget> removeCopies =
                                	widgetDuplicates.iterator();

                                while (removeCopies.hasNext()) {
                                    IWidget widgetCopy = removeCopies.next();

                                    model.removeWidget(widgetCopy);
                                }

                                Iterator<FrameElementGroup> copiedGroups =
                                    groupCopies.values().iterator();

                                while (copiedGroups.hasNext()) {
                                    model.removeEltGroup(copiedGroups.next());
                                }

                                demoStateMgr.noteWidgetsEdit(widgetDuplicates, this);
                            }
                        };

                    demoStateMgr.noteWidgetsEdit(widgetDuplicates, edit);

                    undoMgr.addEdit(edit);

                    return true;
                }

                // tell user to select something.
                interaction.protestNoSelection();

                return false;
            }
        };
    }

    /**
     * Get the UI.
     * In this case it's a FrameEditorUI
     */
    @Override
    public UI getUI()
    {
        return ui;
    }

    @Override
    protected ZoomableUI getZoomableUI()
    {
        return ui;
    }

    /**
     * Helper method which returns a list of models instead of the
     * SelectionState returned Figures
     * @param selection
     * @return
     */
    private Set<FrameElement> getSelectedElements(FrameEditorSelectionState seln,
                                                     Comparator<? super FrameElement> c)
    {
        Set<FrameElement> elements = new TreeSet<FrameElement>(c);
        Iterator<FrameElement> selectedElts =
            seln.getSelectedElementsIterator();

        // Determine which elements can participate
        while (selectedElts.hasNext()) {
            FrameElement elt = selectedElts.next();

            if (! isMemberOfSelectedGroup(elt, seln)) {
                elements.add(elt);
            }
        }

        return elements;
    }

    /**
     * Fetch all widgets referenced by the given selection sorted by
     * the given comparison function.
     */
    private IWidget[] getSelectedWidgets(FrameEditorSelectionState seln,
                                           Comparator<? super IWidget> c)
    {
        IWidget[] widgets;

        Set<IWidget> allWidgets = new HashSet<IWidget>();
        Iterator<FrameElement> s = seln.getSelectedElementsIterator();

        while (s.hasNext()) {
            FrameElement elt = s.next();

            if (elt instanceof IWidget) {
                IWidget w = (IWidget) elt;
                SimpleWidgetGroup group = w.getParentGroup();

                if (group != null) {
                    Iterator<IWidget> mbrs = group.iterator();

                    while (mbrs.hasNext()) {
                        allWidgets.add(mbrs.next());
                    }
                }
                else {
                    allWidgets.add(w);
                }
            }
            else if (elt instanceof FrameElementGroup) {
                Iterator<IWidget> eltGrpWidgets =
                    new ElementAllWidgetIterator((FrameElementGroup) elt);

                while (eltGrpWidgets.hasNext()) {
                    allWidgets.add(eltGrpWidgets.next());
                }
            }
        }

        widgets = new IWidget[allWidgets.size()];

        allWidgets.toArray(widgets);

        Arrays.sort(widgets, c);

        return widgets;
    }

    /**
     * Generates a unique widget name
     *
     * See bug report #133
     *
     * @return the generated name
     */
    private String generateUniqueWidgetName()
    {
        String name = null;
        do {
            name = DEFAULT_WIDGET_PREFIX + " " + widgetNameSuffix++;

        } while (model.isWidgetNameTaken(name));

        return name;
    }

    private void setWidgetImage(final IWidget w,
                                  final byte[] imageData,
                                  final String imageURL,
                                  CogToolLID lid,
                                  final String undoRedoLabel,
                                  IUndoableEditSequence editSequence)
    {
        // Get existing image
        final byte[] previousImageData = w.getImage();
        final String previousImagePath =
            (String) w.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);

        // Perform operation
        w.setImage(imageData);
        w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR, imageURL);

        // Add the undo edit
        editSequence.addEdit(new AUndoableEdit(lid)
        {
            @Override
            public String getPresentationName()
            {
                return undoRedoLabel;
            }

            @Override
            public void redo()
            {
                super.redo();
                w.setImage(imageData);
                w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR, imageURL);
            }

            @Override
            public void undo()
            {
                super.undo();
                w.setImage(previousImageData);
                w.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
                               previousImagePath);
            }
        });
    }

    /**
     * Sets the images on a bunch of widgets
     * @param selected an iterator containing the IWidgets
     * @param imageData the new image, or null of none
     */
    private void setWidgetImages(Iterator<IWidget> selected,
                                   final byte[] imageData,
                                   final String imageURL)
    {
        String undoRedoLabel =
            (imageData == null) ? REMOVE_WIDGET_IMG : SET_WIDGET_IMG;

        CogToolLID lid =
            (imageData == null) ? FrameEditorLID.RemoveImageProperty
                                : FrameEditorLID.SetImageProperty;

        // Create the compound undo
        CompoundUndoableEdit editSequence =
            new CompoundUndoableEdit(undoRedoLabel, lid);

        // Change the images on all the selected widgets
        while (selected.hasNext()) {
            setWidgetImage(selected.next(),
                           imageData,
                           imageURL,
                           lid,
                           undoRedoLabel,
                           editSequence);
        }

        // Commit the edit
        editSequence.end();

        // Only add this edit if it is significant
        if (editSequence.isSignificant()) {
            undoMgr.addEdit(editSequence);
        }
    }

    private AListenerAction createSetSkinAction(final SkinType newSkin,
                                                  final CogToolLID lid)
    {
        return new AListenerAction() {

            public boolean performAction(Object prms)
            {
                final SkinType oldSkin = design.getSkin();

                design.setSkin(newSkin);

                IUndoableEdit edit = new AUndoableEdit(lid)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return CHANGE_SKIN;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        design.setSkin(newSkin);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        design.setSkin(oldSkin);
                    }
                };

                UndoManager designUndoMgr =
                    UndoManager.getUndoManager(design, project);
                designUndoMgr.addEdit(edit);
                undoMgr.addEdit(edit);

                return true;
            }
        };
    }

    private boolean setSpeakerText(final String newText)
    {
        final String oldText = model.getSpeakerText();

        if (oldText.equals(newText)) {
            return true;
        }

        model.setSpeakerText(newText);

        DemoScriptCmd.resetComputations(project, design);

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(FrameEditorLID.SetSpeakerText,
                                                demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return SET_SPEAKER_TEXT;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    model.setSpeakerText(newText);

//                    DemoScriptCmd.redoAllChanges(computeUndoRedos);
                    noteEditCheckRegenerate(this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    model.setSpeakerText(oldText);

//                    DemoScriptCmd.undoAllChanges(computeUndoRedos);
                    noteEditCheckRegenerate(this);
                }
            };

        noteEditCheckRegenerate(edit);
        undoMgr.addEdit(edit);

        return true;
    }

    private boolean setSpeakerDuration(final double newTimeInSecs)
    {
        final double oldTimeInSecs = model.getListenTimeInSecs();

        if (oldTimeInSecs == newTimeInSecs) {
            return true;
        }

        model.setListenTimeInSecs(newTimeInSecs);

        DemoScriptCmd.resetComputations(project, design);

        DemoStateManager.ObsoletingEdit edit =
            new DemoStateManager.ObsoletingEdit(FrameEditorLID.SetSpeakerTime,
                                                demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return SET_SPEAKER_DURATION;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    model.setListenTimeInSecs(newTimeInSecs);

//                    DemoScriptCmd.redoAllChanges(computeUndoRedos);
                    noteEditCheckRegenerate(this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    model.setListenTimeInSecs(oldTimeInSecs);

//                    DemoScriptCmd.undoAllChanges(computeUndoRedos);
                    noteEditCheckRegenerate(this);
                }
            };

        noteEditCheckRegenerate(edit);
        undoMgr.addEdit(edit);

        return true;
    }

    private IListenerAction createGroupElementsAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState selection =
                    (FrameEditorSelectionState) prms;

                int selectedEltCount = selection.getElementSelectionCount();

                if (selectedEltCount > 1) {
                    final FrameElementGroup newGroup = new FrameElementGroup();

                    assignUniqueEltGroupName(newGroup);

                    Iterator<FrameElement> selectedElts =
                        selection.getSelectedElementsIterator();

                    while (selectedElts.hasNext()) {
                        FrameElement rootElt =
                            selectedElts.next().getRootElement();

                        // Add element to the group, which also records
                        // that the element is a member of the group
                        if (! newGroup.contains(rootElt)) {
                            newGroup.add(rootElt);
                        }
                    }

                    model.addEltGroup(newGroup);

                    IUndoableEdit edit =
                        new AUndoableEdit(FrameEditorLID.Group)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return GROUP_ELEMENTS;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                Iterator<FrameElement> groupElts =
                                    newGroup.iterator();

                                // Since undo simply removed the group as an
                                // association from the element, we only need
                                // to add it back before adding the association
                                // back to the frame
                                while (groupElts.hasNext()) {
                                    groupElts.next().addToEltGroup(newGroup);
                                }

                                model.addEltGroup(newGroup);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                model.removeEltGroup(newGroup);

                                Iterator<FrameElement> groupElts =
                                    newGroup.iterator();

                                // Since the group is no longer part of the
                                // frame, no need to physically remove its
                                // members; just remove the group as an
                                // association from the element.
                                while (groupElts.hasNext()) {
                                    groupElts.next().removeFromEltGroup(newGroup);
                                }
                            }
                        };

                    undoMgr.addEdit(edit);

                    return true;
                }

                if (selectedEltCount == 1) {
                    interaction.protestTooFewWidgets();
                }
                else {
                    interaction.protestNoSelection();
                }

                return false;
            }
        };
    } // createGroupElementsAction

    private void regroup(FrameElementGroup group)
    {
        Iterator<? extends FrameElement> groupElts = group.iterator();

        while (groupElts.hasNext()) {
            groupElts.next().addToEltGroup(group);
        }

        model.addEltGroup(group);
    }

    private void ungroup(FrameElementGroup group,
                           IUndoableEditSequence editSequence)
    {
        model.removeEltGroup(group);

        Iterator<? extends FrameElement> groupElts = group.iterator();

        while (groupElts.hasNext()) {
            groupElts.next().removeFromEltGroup(group);
        }

        deleteRemoteLabel(group, editSequence);
    }

    private IListenerAction createUngroupElementsAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorSelectionState.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorSelectionState selection =
                    (FrameEditorSelectionState) prms;

                Iterator<FrameElement> selectedElts =
                    selection.getSelectedElementsIterator();
                final Set<FrameElementGroup> selectedGroups =
                    new HashSet<FrameElementGroup>();

                while (selectedElts.hasNext()) {
                    FrameElement elt = selectedElts.next();

                    if (elt instanceof FrameElementGroup) {
                        selectedGroups.add((FrameElementGroup) elt);
                    }
                    else {
                        selectedGroups.addAll(elt.getRootElement().getEltGroups());
                    }
                }

                if (selectedGroups.size() > 0) {
                    CompoundUndoableEdit editSequence =
                        new CompoundUndoableEdit(UNGROUP_ELEMENTS,
                                                 FrameEditorLID.Ungroup);

                    Iterator<FrameElementGroup> groups =
                        selectedGroups.iterator();

                    while (groups.hasNext()) {
                        FrameElementGroup group = groups.next();

                        ungroup(group, editSequence);

                        removeRootElement(UNGROUP_ELEMENTS,
                                          group,
                                          null,
                                          editSequence);
                    }

                    IUndoableEdit edit =
                        new AUndoableEdit(FrameEditorLID.Ungroup)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return UNGROUP_ELEMENTS;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                Iterator<FrameElementGroup> groups =
                                    selectedGroups.iterator();

                                while (groups.hasNext()) {
                                    FrameElementGroup group = groups.next();

                                    ungroup(group, null);
                                }
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                Iterator<FrameElementGroup> groups =
                                    selectedGroups.iterator();

                                while (groups.hasNext()) {
                                    FrameElementGroup group = groups.next();

                                    regroup(group);
                                }
                            }
                        };

                    editSequence.addEdit(edit);

                    // Commit the edit
                    editSequence.end();

                    // Only add this edit if it is significant
                    if (editSequence.isSignificant()) {
                        undoMgr.addEdit(editSequence);
                    }

                    return true;
                }

                interaction.protestNoSelection();

                return false;
            }
        };
    } // createUngroupElementsAction

    private IListenerAction createSetRemoteLabelTextAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.SetRemoteLabelTextParms.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.SetRemoteLabelTextParms p =
                    (FrameEditorUI.SetRemoteLabelTextParms) prms;

                // Check p.selectedElement; must be an FrameElementGroup
                // or an IWidget of WidgetType Button, PullDownList, Graffiti,
                // or TextBox (whether IS_STANDARD or IS_CUSTOM) or an IWidget
                // of WidgetType Radio, Check, or ListBoxItem if IS_STANDARD
                // (in which case, the label belongs to the parent SimpleWidgetGroup)
                FrameElement owningElt =
                    p.selectedElement.getRemoteLabelOwner();

                if (owningElt == null) {
                    interaction.protestUnsupportedLabelOwnerType();
                    return false;
                }

                IWidget remoteLabel =
                    (IWidget)
                        owningElt.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

                // Already has a label; simply update
                if (remoteLabel != null) {
                    changeTitleProperty(FrameEditorLID.SetRemoteLabelText,
                                        SET_REMOTE_LABEL,
                                        remoteLabel,
                                        p.newText,
                                        false,  // not a separator!
                                        undoMgr);
                }
                else if ((p.newText != null) && ! p.newText.equals("")) {
                    CompoundUndoableEdit editSequence =
                        new CompoundUndoableEdit(SET_REMOTE_LABEL,
                                                 FrameEditorLID.SetRemoteLabelText);

                    final double INITIAL_WIDTH = 100.0;
                    final double INITIAL_HEIGHT = 16.0;
                    final double MIN_MARGIN = 5.0;
                    final double EXTRA_MARGIN = 10.0;

                    DoubleRectangle eltBds = owningElt.getEltBounds();
                    double maxY = eltBds.y - INITIAL_HEIGHT - MIN_MARGIN;

                    DoubleRectangle labelBds;

                    if (0.0 <= maxY) {
                        labelBds =
                            new DoubleRectangle(eltBds.x,
                                                Math.max(0.0,
                                                         maxY - EXTRA_MARGIN),
                                                INITIAL_WIDTH,
                                                INITIAL_HEIGHT);
                    }
                    else {
                        double maxX =
                            Math.max(0.0, eltBds.x - INITIAL_WIDTH - MIN_MARGIN);

                        labelBds =
                            new DoubleRectangle(Math.max(0.0,
                                                         maxX - EXTRA_MARGIN),
                                                eltBds.y,
                                                INITIAL_WIDTH,
                                                INITIAL_HEIGHT);
                    }

                    remoteLabel = new Widget(labelBds, WidgetType.Text);

                    DefaultCmd.setAttribute(owningElt,
                                            demoStateMgr,
                                            WidgetAttributes.REMOTE_LABEL_ATTR,
                                            remoteLabel,
                                            interaction,
                                            editSequence);

                    remoteLabel.setAttribute(WidgetAttributes.IS_STANDARD_ATTR,
                                             WidgetAttributes.IS_CUSTOM);
                    remoteLabel.setAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR,
                                             owningElt);

                    remoteLabel.setName(generateUniqueWidgetName());
                    remoteLabel.setTitle(p.newText);

                    // Add the new widget to the undo system and to the model
                    editSequence.addEdit(addWidget(remoteLabel));

                    // Commit the edit
                    editSequence.end();

                    // Only add this edit if it is significant
                    if (editSequence.isSignificant()) {
                        undoMgr.addEdit(editSequence);
                    }
                }

                return true;
            }
        };
    } // createSetRemoteLabelTextAction

    private IListenerAction createSetRemoteLabelTypeAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return FrameEditorUI.SetRemoteLabelTypeParms.class;
            }


            public boolean performAction(Object prms)
            {
                FrameEditorUI.SetRemoteLabelTypeParms p =
                    (FrameEditorUI.SetRemoteLabelTypeParms) prms;

                if (! p.newType.canBeARemoteLabel()) {
                    interaction.protestUnsupportedRemoteLabelType();
                    return false;
                }

                // TODO: xxy If changed to Noninteractive, remove transitions(!?)
                changeWidgetType(FrameEditorLID.SetRemoteLabelType,
                                 CHG_WIDGET_TYPE,
                                 p.selectedRemoteLabel,
                                 p.newType,
                                 undoMgr);

                return true;
            }
        };
    } // createSetRemoteLabelTypeAction

    /**
     * Creates a new FrameEditorController instance for editing an existing
     * Frame instance.
     *
     * @param frame the Frame instance to edit.
     * @param design the Design containing the frame
     * @param project the Project containing the design
     * @return the Controller instance for editing the given Frame instance
     * @author mlh
     */
    public static FrameEditorController openController(Frame frame,
                                                Design design,
                                                Project project)
    {
        FrameEditorController controller =
            (FrameEditorController)
                ControllerRegistry.ONLY.findOpenController(frame);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            controller = new FrameEditorController(frame, design, project);

            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }
}
