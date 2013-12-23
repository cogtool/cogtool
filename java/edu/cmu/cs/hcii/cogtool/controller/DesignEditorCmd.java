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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.controller.DemoStateManager.IDesignUndoableEdit;
import edu.cmu.cs.hcii.cogtool.model.AParentWidget;
import edu.cmu.cs.hcii.cogtool.model.AShape;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.ChildWidget;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.MenuItem;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.RcvrClipboardException;
import edu.cmu.cs.hcii.cogtool.util.RcvrOutOfMemoryException;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

public class DesignEditorCmd
{
    public static final String PASTE = L10N.get("UNDO.Paste", "Paste");

    private static final String NEW_WIDGET =
        L10N.get("UNDO.DE.NewWidget", "New Widget");

    public static final String NEW_FRAME =
        L10N.get("UNDO.DE.NewFrame", "New Frame");

    public static final String USE_TEMPLATE =
        L10N.get("UNDO.DE.UseTemplate", "Populate New Frame");

    private static final String CHANGE_TARGET_FRAME =
        L10N.get("UNDO.DE.ChangeTarget", "Change Target Frame");

    public static final String NEW_TRANSITION =
        L10N.get("UNDO.DE.NewTransition", "New Transition");

    private DesignEditorCmd() { } // prevent instantiation

    public static final boolean SAVE_TO_CLIPBOARD =
        CogToolClipboard.SAVE_TO_CLIPBOARD;
    public static final boolean SAVE_TO_TEMPLATE =
        CogToolClipboard.SAVE_STRING_ONLY;

    public static void copyElements(Design design,
                                    FrameElement[] selectedElts,
                                    Iterator<FrameElement> eltsToCopy,
                                    boolean saveToClipboard)
    {
        try {
            // Set up a clipboard saver.  Indicate that no transitions
            // should be copied.
            CogToolClipboard.ClipboardClassSaver s =
                CogToolClipboard.startClipboardSave(CogToolClipboard.CopyWidgets,
                                                    selectedElts,
                                                    saveToClipboard);

            // Iterate through the widgets and save the selected items.
            while (eltsToCopy.hasNext()) {
                FrameElement elt = eltsToCopy.next();

                // TODO: Fix this when we can limit where menu/pull-down items
                //       get pasted.
                if (! (elt instanceof ChildWidget)) {
                    s.saveObject(elt);
                }
            }

            s.finish();

            if (! saveToClipboard) {
                FrameTemplateSupport.setFrameTemplate(design,
                                                      s.getSavedString());
            }
        }
        catch (IOException e) {
            throw new RcvrClipboardException(e);
        }
        catch (OutOfMemoryError error) {
            throw new RcvrOutOfMemoryException("Copying Widgets", error);
        }
    }

    private static void makeWidgetNameUnique(IWidget widget, Frame frame)
    {
        widget.setName(NamedObjectUtil.makeNameUnique(widget.getName(),
                                                      frame.getWidgets()));
    }

    private static void makeEltGroupNameUnique(FrameElementGroup eltGroup,
                                                 Frame frame)
    {
        eltGroup.setName(NamedObjectUtil.makeNameUnique(eltGroup.getName(),
                                                        frame.getEltGroups()));
    }

    private static void addChildWidgets(SimpleWidgetGroup widgetGroup, Frame f)
    {
        if (widgetGroup != null) {
            Iterator<IWidget> children = widgetGroup.iterator();

            while (children.hasNext()) {
                IWidget child = children.next();

                makeWidgetNameUnique(child, f);
                f.addWidget(child);

                if (child instanceof MenuItem) {
                    MenuItem item = (MenuItem) child;

                    if (item.isSubmenu()) {
                        addChildWidgets(item.getChildren(), f);
                    }
                }
            }
        }
    }

    private static DoublePoint getFirstChildPosition(AParentWidget parent)
    {
        AShape parentShape = parent.getShape();
        DoublePoint pos = parentShape.getOrigin();
        DoubleSize extent = parentShape.getSize();

        switch (parent.getChildrenLocation()) {
            case AParentWidget.CHILDREN_BELOW: {
                pos.y += extent.height;
                break;
            }
            case AParentWidget.CHILDREN_RIGHT: {
                pos.x += extent.width;
                break;
            }
            case AParentWidget.CHILDREN_CENTER: {
                pos.x += extent.width / 2.0;
                pos.y += extent.height / 2.0;
                break;
            }
        }

        return pos;
    }

    public static void repositionChildren(AParentWidget parent)
    {
        if (parent.hasChildren()) {
            DoublePoint startPos = getFirstChildPosition(parent);
            repositionChildren(parent.getChildren(), startPos.x, startPos.y);
        }
    }

    /**
     * TODO: If we ever allow children above or left of a parent,
     * we need to pass in childrenLocation here in addition to the group.
     */
    public static void repositionChildren(SimpleWidgetGroup group,
                                          double x,
                                          double y)
    {
        int orientation = group.getOrientation();

        if (orientation == SimpleWidgetGroup.FREEFORM) {
            return;
        }

        Iterator<IWidget> groupIter = group.iterator();

        while (groupIter.hasNext()) {
            IWidget widget = groupIter.next();

            widget.setWidgetOrigin(x, y);

            if (orientation == SimpleWidgetGroup.HORIZONTAL) {
                x += widget.getEltBounds().width;
            }
            else if (orientation == SimpleWidgetGroup.VERTICAL) {
                y += widget.getEltBounds().height;
            }

            if (widget instanceof AParentWidget) {
                repositionChildren((AParentWidget) widget);
            }
        }
    }

    public static void repositionChildren(SimpleWidgetGroup group)
    {
        if (group.size() > 0) {
            DoublePoint startPos = group.get(0).getShape().getOrigin();
            repositionChildren(group, startPos.x, startPos.y);
        }
    }

    private abstract static class AddWidgetUndoableEdit
                                      extends DemoStateManager.InvalidatingEdit
    {
        protected DemoStateManager demoStateMgr;
        protected Frame frame;
        protected IWidget widget;

        public AddWidgetUndoableEdit(Frame f, IWidget w, DemoStateManager mgr)
        {
            super(FrameEditorLID.NewWidget, mgr);
            frame = f;
            widget = w;
            demoStateMgr = mgr;
        }

        @Override
        public String getPresentationName()
        {
            return NEW_WIDGET;
        }

        protected abstract void redoHelper();
        protected abstract void undoHelper();

        @Override
        public void redo()
        {
            super.redo();
            redoHelper();
            frame.addWidget(widget);

            demoStateMgr.noteWidgetEdit(widget, this);
        }

        @Override
        public void undo()
        {
            super.undo();
            undoHelper();
            frame.removeWidget(widget);

            demoStateMgr.noteWidgetEdit(widget, this);
        }
    }

    public static IDesignUndoableEdit addWidgetUndoableEdit(Frame frame,
                                                            IWidget widget,
                                                            DemoStateManager mgr)
    {
        if (widget instanceof ChildWidget) {
            ChildWidget childWidget = (ChildWidget) widget;
            final AParentWidget itemParent = childWidget.getParent();
            final int atIndex = itemParent.indexOf(childWidget);

            return new AddWidgetUndoableEdit(frame, widget, mgr) {
                @Override
                protected void redoHelper()
                {
                    itemParent.addItem(atIndex, (ChildWidget) widget);
                }

                @Override
                protected void undoHelper()
                {
                    itemParent.removeItem((ChildWidget) widget);
                }
            };
        }

        final SimpleWidgetGroup parentGroup = widget.getParentGroup();
        final int atIndex =
            (parentGroup != null) ? parentGroup.indexOf(widget) : -1;

        return new AddWidgetUndoableEdit(frame, widget, mgr)
            {
                @Override
                protected void redoHelper()
                {
                    if (parentGroup != null) {
                        parentGroup.add(atIndex, widget);
                        // TODO:mlh reposition items/headers following!
                    }
                }

                @Override
                protected void undoHelper()
                {
                    if (parentGroup != null) {
                        parentGroup.remove(widget);
                        // TODO:mlh reposition items/headers following!
                    }
                }
            };
    }

    private static void addChildWidgets(Frame frame,
                                          SimpleWidgetGroup widgetGroup,
                                          DemoStateManager mgr,
                                          IUndoableEditSequence editSeq)
    {
        if (widgetGroup != null) {
            Iterator<IWidget> children = widgetGroup.iterator();

            while (children.hasNext()) {
                IWidget child = children.next();

                makeWidgetNameUnique(child, frame);
                frame.addWidget(child);

                if (editSeq != null) {
                    editSeq.addEdit(addWidgetUndoableEdit(frame,
                                                          child,
                                                          mgr));
                }

                if (child instanceof MenuItem) {
                    MenuItem item = (MenuItem) child;

                    if (item.isSubmenu()) {
                        addChildWidgets(frame,
                                        item.getChildren(),
                                        mgr,
                                        editSeq);
                    }
                }
            }
        }
    }

    private static void checkForRemoteLabel(FrameElement elt,
                                              Set<IWidget> addedRemoteLabels)
    {
        FrameElement owner = elt.getRemoteLabelOwner();

        if (owner != null) {
            IWidget remoteLabel =
                (IWidget) owner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

            if (remoteLabel != null) {
                addedRemoteLabels.add(remoteLabel);
            }
        }
    }

    private static int pasteWidget(IWidget widget,
                                     Design design,
                                     int currentDeviceTypes,
                                     Frame frame,
                                     DemoStateManager mgr,
                                     IUndoableEditSequence editSequence,
                                     Set<FrameElementGroup> groups,
                                     Set<IWidget> addedRemoteLabels)
    {
        if (frame.getWidgets().contains(widget)) {
            return 0;
        }
        
        int requiresOneOf = widget.getWidgetType().requiresOneOf();

        if (! DeviceType.intersects(requiresOneOf, currentDeviceTypes)) {
            // Missing either Mouse or Touchscreen
            if (DeviceType.Touchscreen.isMember(requiresOneOf)) {
                DesignCmd.addDevice(design,
                                    DeviceType.Touchscreen);
            }
            else if (DeviceType.Mouse.isMember(requiresOneOf)) {
                DesignCmd.addDevice(design,
                                    DeviceType.Mouse);
            }
        }

        makeWidgetNameUnique(widget, frame);
        frame.addWidget(widget);

        IDesignUndoableEdit edit = addWidgetUndoableEdit(frame, widget, mgr);

        mgr.noteWidgetEdit(widget, edit);
        editSequence.addEdit(edit);

        if (widget instanceof AParentWidget) {
            AParentWidget parent = (AParentWidget) widget;

            addChildWidgets(frame, parent.getChildren(), mgr, editSequence);
        }

        groups.addAll(widget.getEltGroups());
        checkForRemoteLabel(widget, addedRemoteLabels);

        return 1;
    }

    private static int pasteWidgetGroup(SimpleWidgetGroup group,
                                          Design design,
                                          int currentDeviceTypes,
                                          Frame frame,
                                          DemoStateManager mgr,
                                          IUndoableEditSequence editSequence,
                                          Set<FrameElementGroup> groups,
                                          Set<IWidget> addedRemoteLabels)
    {
        int numPasted = 0;

        Iterator<IWidget> groupWidgets = group.iterator();

        while (groupWidgets.hasNext()) {
            numPasted += pasteWidget(groupWidgets.next(),
                                     design,
                                     currentDeviceTypes,
                                     frame,
                                     mgr,
                                     editSequence,
                                     groups,
                                     addedRemoteLabels);
        }

        groups.addAll(group.getEltGroups());
        checkForRemoteLabel(group, addedRemoteLabels);

        return numPasted;
    }

    private static int pasteFrameElementGroup(FrameElementGroup eltGroup,
                                                Design design,
                                                int currentDeviceTypes,
                                                Frame frame,
                                                DemoStateManager mgr,
                                                IUndoableEditSequence editSeq,
                                                Set<FrameElementGroup> groups,
                                                Set<IWidget> addedRemoteLabels)
    {
        int numPasted = 0;

        makeEltGroupNameUnique(eltGroup, frame);

        groups.add(eltGroup);
        groups.addAll(eltGroup.getEltGroups());
        checkForRemoteLabel(eltGroup, addedRemoteLabels);

        Iterator<FrameElement> elementsToAdd = eltGroup.iterator();

        while (elementsToAdd.hasNext()) {
            FrameElement eltToAdd = elementsToAdd.next();

            if (eltToAdd instanceof IWidget) {
                numPasted += pasteWidget((IWidget) eltToAdd,
                                         design,
                                         currentDeviceTypes,
                                         frame,
                                         mgr,
                                         editSeq,
                                         groups,
                                         addedRemoteLabels);
            }
            else if (eltToAdd instanceof SimpleWidgetGroup) {
                numPasted += pasteWidgetGroup((SimpleWidgetGroup) eltToAdd,
                                              design,
                                              currentDeviceTypes,
                                              frame,
                                              mgr,
                                              editSeq,
                                              groups,
                                              addedRemoteLabels);
            }
            else if (eltToAdd instanceof FrameElementGroup) {
                numPasted +=
                    pasteFrameElementGroup((FrameElementGroup) eltToAdd,
                                           design,
                                           currentDeviceTypes,
                                           frame,
                                           mgr,
                                           editSeq,
                                           groups,
                                           addedRemoteLabels);
            }
        }

        return numPasted;
    } // pasteFrameElementGroup

    public static int pasteElements(Design design,
                                    final Frame frame,
                                    Collection<Object> objects,
                                    DemoStateManager mgr,
                                    IUndoableEditSequence editSequence)
    {
        // If given objects contain widgets, insert into the given frame
        if ((objects != null) && (objects.size() > 0)) {
            Iterator<Object> objIt = objects.iterator();
            Set<SimpleWidgetGroup> addedGroups = new HashSet<SimpleWidgetGroup>();
            final Set<IWidget> addedRemoteLabels = new HashSet<IWidget>();
            final Set<FrameElementGroup> addedEltGroups =
                new HashSet<FrameElementGroup>();
            int numPasted = 0;

            // May need to add a device
            int currentDeviceTypes =
                DeviceType.buildDeviceSet(design.getDeviceTypes());

            // Loop through all objects and, if widgets,
            // create them.
            while (objIt.hasNext()) {
                Object o = objIt.next();

                if (o instanceof IWidget) {
                    IWidget widget = (IWidget) o;

                    numPasted += pasteWidget(widget,
                                             design,
                                             currentDeviceTypes,
                                             frame,
                                             mgr,
                                             editSequence,
                                             addedEltGroups,
                                             addedRemoteLabels);

                    SimpleWidgetGroup group = widget.getParentGroup();

                    if (group != null) {
                        addedGroups.add(group);
                    }
                }
                else if (o instanceof FrameElementGroup) {
                    numPasted += pasteFrameElementGroup((FrameElementGroup) o,
                                                        design,
                                                        currentDeviceTypes,
                                                        frame,
                                                        mgr,
                                                        editSequence,
                                                        addedEltGroups,
                                                        addedRemoteLabels);
                }
            }

            Iterator<SimpleWidgetGroup> groupsIter = addedGroups.iterator();

            while (groupsIter.hasNext()) {
                SimpleWidgetGroup group = groupsIter.next();

                repositionChildren(group);

                if (group instanceof GridButtonGroup) {
                    ((GridButtonGroup) group).recalculateOffsets();
                }

                addedEltGroups.addAll(group.getEltGroups());
            }

            Iterator<FrameElementGroup> eltGroups = addedEltGroups.iterator();

            while (eltGroups.hasNext()) {
                frame.addEltGroup(eltGroups.next());
            }

            Iterator<IWidget> remoteLabels = addedRemoteLabels.iterator();

            while (remoteLabels.hasNext()) {
                IWidget remoteLabel = remoteLabels.next();

                if (! frame.containsWidget(remoteLabel)) {
                    String uniqueName =
                        NamedObjectUtil.makeNameUnique(remoteLabel.getName(),
                                                       frame.getWidgets());

                    remoteLabel.setName(uniqueName);
                    frame.addWidget(remoteLabel);
                }
            }

            IUndoableEdit edit =
                new AUndoableEdit(CogToolLID.Paste)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return PASTE;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        Iterator<FrameElementGroup> eltGroups =
                            addedEltGroups.iterator();

                        while (eltGroups.hasNext()) {
                            frame.addEltGroup(eltGroups.next());
                        }

                        Iterator<IWidget> remoteLabels =
                            addedRemoteLabels.iterator();

                        while (remoteLabels.hasNext()) {
                            IWidget remoteLabel = remoteLabels.next();

                            if (! frame.containsWidget(remoteLabel)) {
                                frame.addWidget(remoteLabel);
                            }
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        Iterator<FrameElementGroup> eltGroups =
                            addedEltGroups.iterator();

                        while (eltGroups.hasNext()) {
                            frame.removeEltGroup(eltGroups.next());
                        }

                        Iterator<IWidget> remoteLabels =
                            addedRemoteLabels.iterator();

                        while (remoteLabels.hasNext()) {
                            IWidget remoteLabel = remoteLabels.next();

                            if (frame.containsWidget(remoteLabel)) {
                                frame.removeWidget(remoteLabel);
                            }
                        }
                    }
                };

            editSequence.addEdit(edit);

            return numPasted;
        }

        return 0;
    } // pasteElements

    public static void addFrame(final Project project,
                                final Design design,
                                final DemoStateManager demoStateMgr,
                                final Frame frame,
                                IUndoableEditSequence editSequence)
    {
        design.addFrame(frame);

        Collection<Object> objects =
            FrameTemplateSupport.getFrameTemplate(design);

        if ((objects != null) && (objects.size() > 0)) {
            CompoundUndoableEdit tplEdit =
                new CompoundUndoableEdit(USE_TEMPLATE, null);

            pasteElements(design, frame, objects, demoStateMgr, tplEdit);

            UndoManager undoMgr =
                UndoManager.getUndoManager(frame, project);

            tplEdit.end();
            undoMgr.addEdit(tplEdit);
        }

        IUndoableEdit edit =
            new DemoStateManager.InvalidatingEdit(DesignEditorLID.NewFrame,
                                                  demoStateMgr)
            {
                private boolean recoverMgr = false;

                @Override
                public String getPresentationName()
                {
                    return NEW_FRAME;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgr = false;

                    design.addFrame(frame);

                    demoStateMgr.noteFrameEdit(frame, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgr = true;

                    design.removeFrame(frame);

                    demoStateMgr.noteFrameEdit(frame, this);
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgr) {
                        UndoManagerRecovery.recoverManagers(project, frame);
                    }
                }
           };

       editSequence.addEdit(edit);
    }

    public static void addIncidentTransitions(Transition[] transitions)
    {
        // Maps Transition to the IWidget that is its source.
        for (Transition transition : transitions) {
            TransitionSource sourceWidget = transition.getSource();

            sourceWidget.addTransition(transition);
        }
    }

    public static void deleteFrame(final Project project,
                                   final Design design,
                                   final DemoStateManager demoStateMgr,
                                   final Frame frame,
                                   CogToolLID lid,
                                   IUndoableEditSequence editSequence)
    {
        final Transition[] incidentTransitions =
            frame.removeIncidentTransitions();

        design.removeFrame(frame);

        DemoStateManager.IDesignUndoableEdit edit =
            new DemoStateManager.InvalidatingEdit(lid, demoStateMgr)
            {
                private boolean recoverMgr = true;

                @Override
                public void redo()
                {
                    super.redo();

                    recoverMgr = true;

                    frame.removeIncidentTransitions();

                    design.removeFrame(frame);

                    demoStateMgr.noteFrameEdit(frame, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    recoverMgr = false;

                    design.addFrame(frame);

                    addIncidentTransitions(incidentTransitions);

                    demoStateMgr.noteFrameEdit(frame, this);
                }

                @Override
                public void die()
                {
                    super.die();

                    if (recoverMgr) {
                        UndoManagerRecovery.recoverManagers(project, frame);
                    }
                }
            };

        demoStateMgr.noteFrameEdit(frame, edit);

        editSequence.addEdit(edit);
    }

    public static IUndoableEdit addTransition(final DemoStateManager demoStateMgr,
                                              final Transition transition)
    {
        transition.getSource().addTransition(transition);

        IDesignUndoableEdit edit =
            new DemoStateManager.InvalidatingEdit(DesignEditorLID.NewTransition,
                                                  demoStateMgr)
            {
                @Override
                public String getPresentationName()
                {
                    return NEW_TRANSITION;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    transition.getSource().addTransition(transition);

                    demoStateMgr.noteTransitionEdit(transition, this);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    transition.getSource().removeTransition(transition);

                    demoStateMgr.noteTransitionEdit(transition, this);
                }
            };

        demoStateMgr.noteTransitionEdit(transition, edit);

        return edit;
    }

    public static void changeTransitionTarget(final DemoStateManager demoStateMgr,
                                              final Transition transition,
                                              final Frame newDestination,
                                              IUndoableEditSequence editSeq)
    {
        final Frame oldDestination = transition.getDestination();

        if (newDestination != oldDestination) {
            transition.setDestination(newDestination);

            DemoStateManager.IDesignUndoableEdit edit =
                new DemoStateManager.InvalidatingEdit(DesignEditorLID.ChangeTarget,
                                                      demoStateMgr)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return CHANGE_TARGET_FRAME;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        transition.setDestination(newDestination);

                        demoStateMgr.noteTransitionEdit(transition, this);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        transition.setDestination(oldDestination);

                        demoStateMgr.noteTransitionEdit(transition, this);
                    }
                };

            demoStateMgr.noteTransitionEdit(transition, edit);
            editSeq.addEdit(edit);
        }
    } // changeTransitionTarget
}
