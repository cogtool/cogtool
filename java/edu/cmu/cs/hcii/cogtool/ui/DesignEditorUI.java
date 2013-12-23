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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.ActionType;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GraffitiAction;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.KeyAction;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.TextAction;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.VoiceAction;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorSelectionState.FrameSelectionChange;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorSelectionState.TransitionSelectionChange;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorTransition;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.StructureViewUIModel;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTTextEditor;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.ActionPropertySet;
import edu.cmu.cs.hcii.cogtool.view.ActionSet;
import edu.cmu.cs.hcii.cogtool.view.DesignEditorView;
import edu.cmu.cs.hcii.cogtool.view.InteractionDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.ResizeThumb;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.View;

public class DesignEditorUI extends ZoomableUI
{
    protected class FrameNameEditor extends SWTTextEditor
    {
        public FrameNameEditor()
        {
            super(view.getEditor().getSWTEditorSubstrate(), CENTER_TEXT);
        }

        @Override
        public boolean confirm(int focusRule)
        {
            return confirmRenameFrame();
        }

        @Override
        protected int computeHeight(Rectangle bounds, double scale, int offset)
        {
            DesignEditorFrame frameFig = (DesignEditorFrame) getData();

            return PrecisionUtilities.round(frameFig.getLabelHeight() * scale);
        }

        public void editFrameName(DesignEditorFrame frameFigure)
        {
            Frame frame = frameFigure.getFrame();

            editingInProgress = false;

            editText(frame.getName(), frameFigure, structureView.getZoom());
        }

        @Override
        protected Font getFontToUse()
        {
            return ((DesignEditorFrame) getData()).getLabel().getFont();
        }
    }

    /**
     * Parameters for MoveFrames operation
     *
     * @author mlh
     */
    public static class MoveParameters
    {
        public double dx;
        public double dy;
        public FrameSelectionState selection;

        public MoveParameters(double deltaX, double deltaY,
                              FrameSelectionState selnState)
        {
            dx = deltaX;
            dy = deltaY;
            selection = selnState;
        }
    }

    /**
     * Parameters for DuplicateFrames operation
     *
     * @author mlh
     */
    public static class DuplicateParameters
    {
        public double dx;
        public double dy;
        public DesignEditorSelectionState selection;

        public DuplicateParameters(double deltaX, double deltaY,
                                   DesignEditorSelectionState selnState)
        {
            dx = deltaX;
            dy = deltaY;
            selection = selnState;
        }
    }

    /**
     * Parameters for CreateTransition operation
     *
     * @author mlh
     */
    public static class NewTransitionParameters
    {
        public TransitionSource source;
        public Frame target;
        public double x;
        public double y;

        // Ok for target to be null -- means that a new Frame should be built
        public NewTransitionParameters(TransitionSource transitionSource,
                                       Frame targetFrame,
                                       double atX,
                                       double atY)
        {
            source = transitionSource;
            target = targetFrame;
            x = atX;
            y = atY;
        }
    }

    public static class EditTransitionParameters
    {
        public DesignEditorSelectionState selection;
        public int useWhichParts;

        public EditTransitionParameters(DesignEditorSelectionState seln,
                                        int whichParts)
        {
            selection = seln;
            useWhichParts = whichParts;
        }
    }

    public static class DesignRenameParameters
    {
        public Design design;
        public String newText;

        public DesignRenameParameters(Design d, String text)
        {
            design = d;
            newText = text;
        }

    }

    /**
     * Parameters for ChangeTarget operation
     *
     * @author mlh
     */
    public static class ChangeTargetParameters
    {
        public Transition transition;
        public Frame newDestination;

        public ChangeTargetParameters(Transition t, Frame f)
        {
            transition = t;
            newDestination = f;
        }
    }

    /**
     * Parameters for ChangeSource operation
     *
     * @author mlh
     */
    public static class ChangeSourceParameters
    {
        public Transition transition;
        public TransitionSource newSource;

        public ChangeSourceParameters(Transition t, TransitionSource s)
        {
            transition = t;
            newSource = s;
        }
    }

    /**
     * Parameters for ChangeWidgetAction/ChangeDeviceAction operations
     *
     * @author mlh
     */
    public static class ChangeActionParameters
    {
        public DesignEditorSelectionState selection;
        public ActionProperties properties;

        public ChangeActionParameters(ActionProperties p,
                                      DesignEditorSelectionState s)
        {
            properties = p;
            selection = s;
        }
    }

    public static class ChangeDelayParameters
    {
        public DesignEditorSelectionState selection;
        public double delayInSecs;
        public String delayLabel;

        public ChangeDelayParameters(double duration,
                                     String label,
                                     DesignEditorSelectionState s)
        {
            delayInSecs = duration;
            delayLabel = label;
            selection = s;
        }
    }

    public static class PasteBackgroundImageParms
    {
        public FrameSelectionState selection;
        public byte[] imageData;

        public PasteBackgroundImageParms(FrameSelectionState seln,
                                         byte[] imgData)
        {
            selection = seln;
            imageData = imgData;
        }
    }

    public static class CopyBackgroundImageParms
    {
        public byte[] imageData;
        public Frame selectedFrame;

        public CopyBackgroundImageParms(byte[] imgData, Frame frame)
        {
            imageData = imgData;
            selectedFrame = frame;
        }
    }

    // For renaming a Frame
    public static class FrameRenameParameters
    {
        public Frame frame;
        public String newName;

        public FrameRenameParameters(Frame frameToRename,
                                     String newFrameName)
        {
            frame = frameToRename;
            newName = newFrameName;
        }
    }

    // Constants for DesignEditorTransition resize thumb positions
    // (used by DesignEditorMouseState)
    public static final int SOURCE = DesignEditorTransition.SOURCE;
    public static final int TARGET = DesignEditorTransition.TARGET;

    protected DesignEditorView view;

    protected DesignEditorSelectionState selection;
    protected DesignEditorSelectionState contextSelection;

    protected static final DesignEditorSelectionState emptySelection =
        new DesignEditorSelectionState();

    protected DesignEditorInteraction interaction;

    protected FrameNameEditor editor = null;
    protected boolean editingInProgress = false;

    protected DesignEditorUIModel uiModel;
    protected StructureViewUIModel structureView;      // cached from uiModel!

    protected DelayedSelection delayedFrameSelection;
    protected DelayedSelection delayedTransitionSelection;

    // Constants for delayedRepainting; powers of 2!
    protected static final int REPAINT_SELECT_HANDLES = 1;
    protected static final int REPAINT_EDITOR = 2;

    protected static final int PERFORM_LWS_UPDATE = 4;
//    protected static final int REPAINT_VIEW = 8;

    protected static final int ZOOM_REPAINT =
        REPAINT_EDITOR | REPAINT_SELECT_HANDLES;

    protected static final int UPDATE_REPAINT =
        REPAINT_SELECT_HANDLES | PERFORM_LWS_UPDATE;

    protected static final int REPAINT_ALL = DelayedRepaint.REPAINT_ALL;

    protected DelayedRepaint delayedRepainting;

    protected DesignEditorUI.EditTransitionParameters editTransitionParms;

    protected Design design;

    protected static final String DESIGN_PREFIX =
        L10N.get("WT.DesignPrefix", "Design");

    protected static final String FRAME_LABEL =
        L10N.get("WT.FrameLabel", "Frame");
    protected static final String FRAMES_LABEL =
        L10N.get("WT.FramesLabel", "Frames");
    protected static final String TRANSITION_LABEL =
        L10N.get("WT.TransitionLabel", "Transition");
    protected static final String TRANSITIONS_LABEL =
        L10N.get("WT.TransitionsLabel", "Transitions");
    protected static final String SELECT_ALL_FRAMES =
        L10N.get("DE.SelectAllFrames", "Select All Frames");
    protected static final String SET_FRAME_BKG_IMAGE =
        L10N.get("DE.SetFrameBkgImage", "Set Background Image...");
    protected static final String REMOVE_FRAME_BKG_IMAGE =
        L10N.get("DE.RemoveFrameBkgImage", "Remove Background Image");

    protected static String buildWindowMenuLabel(Design design)
    {
        String designName =
            SWTStringUtil.insertEllipsis(design.getName(),
                                         StringUtil.EQUAL,
                                         SWTStringUtil.DEFAULT_FONT);
        return DESIGN_PREFIX + ": " + designName;
    }

    public DesignEditorUI(final Design designToEdit,
                          Project designProject,
                          UndoManager undoMgr)
    {
        super(designProject,
              buildWindowMenuLabel(designToEdit),
              buildLeadItems(designProject),
              undoMgr);

        design = designToEdit;

        selection = new DesignEditorSelectionState();
        contextSelection = new DesignEditorSelectionState();

        delayedFrameSelection =
            new DelayedSelection(selection) {
                @Override
                protected void selectItem(Object item)
                {
                    selection.selectFrame((DesignEditorFrame) item);
                }
            };

        delayedTransitionSelection =
            new DelayedSelection(selection) {
                @Override
                protected void selectItem(Object item)
                {
                    selection.selectTransition((DesignEditorTransition) item);
                }
            };

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                protected void performRepaint()
                {
                    InteractionDrawingEditor viewEditor = view.getEditor();

                    if (isRepaintNeeded(REPAINT_EDITOR)) {
                        repaintEditor();
                    }

                    // Don't know why, but this must come
                    // *before* repaintSelectHandles!
                    if (isRepaintNeeded(PERFORM_LWS_UPDATE)) {
                        viewEditor.getLWS().getUpdateManager().performUpdate();
                    }

                    if (isRepaintNeeded(REPAINT_SELECT_HANDLES)) {
                        repaintSelectHandles();
                    }

                    updateView(false);
//                    if (isRepaintNeeded(REPAINT_VIEW)) {
//                        viewEditor.repaint();
//                    }
                }

                @Override
                public void doWork()
                {
                    // Must do Draw2D changes before requesting refresh
                    uiModel.resetHiddenTransitionSources();

                    super.doWork();

                    undoMgrViewHandler.resetView(undoManager);

                    // Update the enabled items selection state.
                    setViewEnabledState(selection,
                                        ListenerIdentifierMap.NORMAL);
                }

                @Override
                public void reset(boolean notCanceled)
                {
                    // TODO: for now, reset text box regardless of what
                    // action was actually canceled

                    if (! notCanceled) {
                        if (selection.getSelectedTransitionCount() == 1) {
                            Transition t =
                                selection.getSelectedTransitions()[0];
                            AAction a = t.getAction();
                            ActionPropertySet aps =
                                view.getActionPropertySet();

                            if (t.getSource() instanceof IWidget) {
                                aps.resetMode(a);
                            }

                            if (a instanceof TextAction) {
                                String newString = ((TextAction) a).getText();

                                if (a instanceof KeyAction) {
                                    aps.setKeyboardString(newString);
                                }
                                else if (a instanceof GraffitiAction) {
                                    aps.setGraffitiString(newString);
                                }
                                else if (a instanceof VoiceAction) {
                                    aps.setVoiceString(newString);
                                }
                            }
                        }
                    }
                }
            };

        CogTool.selectionPhase.addDelayedWork(delayedFrameSelection);
        CogTool.selectionPhase.addDelayedWork(delayedTransitionSelection);

        CogTool.repaintPhase.addDelayedWork(delayedRepainting);

        uiModel =
            new DesignEditorUIModel(design,
                                    designProject,
                                    new AlertHandler()
                                    {

                                        public void handleAlert(EventObject a)
                                        {
                                            delayedRepainting.requestRepaint(UPDATE_REPAINT);
                                        }
                                    },
                                    new AlertHandler()
                                    {

                                        public void handleAlert(EventObject a)
                                        {
                                            updateView(false);
                                        }
                                    });

        // Cache the structure view since it's used so often here
        structureView = uiModel.getStructureView();

        final DesignEditorMouseState mouseState =
            new DesignEditorMouseState(this);

        int deviceTypes =
            DeviceType.buildDeviceSet(design.getDeviceTypes());

        editTransitionParms =
            new DesignEditorUI.EditTransitionParameters(selection,
                                         ActionProperties.UNSET);

        view = new DesignEditorView(deviceTypes,
                                         lIDMap,
                                         this,
                                         editTransitionParms,
                                         menuData,
                                         structureView.getContents(),
                                         mouseState,
                                         mouseState,
                                         this,
                                         getWindowLocation());
//... check out deleteKeyL and other addKeyL calls in FrameEditorUIModel

        view.getActionPropertySet().updateEmptyComposite(design,
                                                              true);

        SelectionListener treeListener =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    boolean needToDeselect = true;

                    Tree t = (Tree) evt.getSource();

                    TreeItem[] items = t.getSelection();

                    for (TreeItem item : items) {
                        Object data = item.getData();

                        if (data instanceof Frame) {
                            if (needToDeselect) {
                                selection.deselectAll();
                                needToDeselect = false;
                            }

                            DesignEditorFrame frameFig =
                                structureView.getFrameFigure((Frame) data);
                            selection.selectFrame(frameFig);
                            centerSelectedRegion();
                        }
                        else if (data instanceof Transition) {
                            if (needToDeselect) {
                                selection.deselectAll();
                                needToDeselect = false;
                            }

                            DesignEditorTransition transFig =
                                structureView.getTransitionFigure((Transition) data);
                            selection.selectTransition(transFig);
                            centerSelectedRegion();
                        }
                    }

                    mouseState.cleanup();
                }
            };

        view.getActionPropertySet().setTreeListener(treeListener);

        // Add them to the contents.
        InteractionDrawingEditor drawingEditor = view.getEditor();
        setZoomEditor(drawingEditor);

        // Let mouseState handle delete key events
        drawingEditor.getInteractionFigure().addKeyListener(mouseState);

        Canvas editorSubstrate = drawingEditor.getSWTEditorSubstrate();

        //TODO: This code is used in a number of places. Should it be moved to
        //      interaction drawing editor?
        editorSubstrate.addListener(SWT.MenuDetect, mouseState);
        setUpDropImageSupport(editorSubstrate);

        updateTitle();

        interaction = new DesignEditorInteraction(view);

        editorSubstrate.addMouseListener(new MouseAdapter()
            {
                @Override
                public void mouseDoubleClick(MouseEvent e)
                {
                    if (editingInProgress) {
                        confirmRenameFrame();
                    }
                    else {
                        editingInProgress = true;
                    }
                }

                @Override
                public void mouseDown(MouseEvent e)
                {
                    if (editingInProgress) {
                        confirmRenameFrame();
                    }
                    else {
                        editingInProgress = true;
                    }
                }
            });

        // Restore zoom level
        restoreZoom();

        selection.addHandler(this,
                                  FrameSelectionChange.class,
                                  createFrameSelectionHandler());

        selection.addHandler(this,
                                  TransitionSelectionChange.class,
                                  createTransitionSelectionHandler());

        structureView.addHandler(this,
                                      StructureViewUIModel.FrameAdd.class,
                                      createFrameAddHandler());

        structureView.addHandler(this,
                                      StructureViewUIModel.FrameShapeChange.class,
                                      createFrameShapeChangeHandler());

        structureView.addHandler(this,
                                      StructureViewUIModel.FrameNameChange.class,
                                      createFrameNameChangeHandler());

        structureView.addHandler(this,
                                      StructureViewUIModel.FrameRecovery.class,
                                      createFrameRecoveryHandler());

        structureView.addHandler(this,
                                      StructureViewUIModel.TransitionAddRemove.class,
                                      createTransitionAddRemoveHandler());

        project.addHandler(this,
                                Project.DesignChange.class,
                                new AlertHandler()
                                {

                                    public void handleAlert(EventObject alert)
                                    {
                                        Project.DesignChange chg =
                                            (Project.DesignChange) alert;

                                        if ((! chg.isAdd) &&
                                            (chg.element == designToEdit))
                                        {
                                            closeOpenController();
                                        }
                                    }
                                });

        setFrameChangeHandlers(design);

        // Some items should always be enabled.
        setInitiallyEnabled(true);
    } // ctor

    protected void setUpDropImageSupport(final Canvas editorSubstrate)
    {
        setUpDropImage(editorSubstrate,
                       new ExternalImageDropTarget()
        {
            protected DesignEditorFrame dropTargetFrame = null;

            @Override
            public void dragOperationChanged(DropTargetEvent evt)
            {
                if (dropTargetFrame == null) {
                    evt.detail = DND.DROP_NONE;
                }
                else {
                    super.dragOperationChanged(evt);
                }
            }

            @Override
            public void dragOver(DropTargetEvent evt)
            {
                super.dragOver(evt);

                if (dropTargetFrame == null) {
                    evt.detail = DND.DROP_NONE;
                }
            }

            @Override
            protected void cancelHighlight(DropTargetEvent evt)
            {
                if (dropTargetFrame != null) {
                    dropTargetFrame.dynamicHighlight(false);
                }
            }

            @Override
            protected void highlight(DropTargetEvent evt)
            {
                org.eclipse.swt.graphics.Point pt =
                    editorSubstrate.toControl(evt.x, evt.y);
                DesignEditorFrame frameFig =
                    structureView.getFrameAtXY(pt.x, pt.y);

                if (dropTargetFrame != frameFig) {
                    cancelHighlight(evt);

                    if (frameFig != null) {
                        frameFig.dynamicHighlight(true);
                    }

                    dropTargetFrame = frameFig;
                }
            }

            @Override
            protected Object buildParameters(DropTargetEvent evt,
                                             byte[] imgData)
            {
                org.eclipse.swt.graphics.Point pt =
                    editorSubstrate.toControl(evt.x, evt.y);
                DesignEditorFrame frameFig =
                    structureView.getFrameAtXY(pt.x, pt.y);

                if (frameFig != null) {
                    return new DesignEditorUI.CopyBackgroundImageParms(imgData,
                                                        frameFig.getFrame());
                }

                return null;
            }
        });
    }

    /**
     * Support for resetting transition source anchors for those transitions
     * whose sources may be "hidden".
     */
    public void resetHiddenTransitionSources()
    {
        uiModel.resetHiddenTransitionSources();
        performRepaintUpdates();
    }

    @Override
    protected void updateTitle()
    {
        view.setWindowTitle(modificationFlag
                                 + DESIGN_PREFIX
                                 + ": "
                                 + project.getName()
                                 + " > "
                                 + uiModel.getDesign().getName()
                                 + ((OSUtils.MACOSX) ? "" : UI.WINDOW_TITLE));
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return buildWindowMenuLabel(uiModel.getDesign());
    }

    /**
     * Recover any system resources being used to support this window/view.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        if (editor != null) {
            editor.dispose();
        }

        CogTool.selectionPhase.removeDelayedWork(delayedFrameSelection);
        CogTool.selectionPhase.removeDelayedWork(delayedTransitionSelection);

        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        structureView.removeAllHandlers(this);
        uiModel.getDesign().removeAllHandlers(this);

        uiModel.dispose();

        selection.removeAllHandlers(this);

        // Need to remove structure view before disposing the display, since
        // the structure view dispose will try and deselect all before
        // disposing its self.
        super.dispose();
    }

    @Override
    protected Object getModelObject()
    {
        return uiModel.getDesign();
    }


    public DesignEditorInteraction getInteraction()
    {
        return interaction;
    }

    /**
     * Standard interaction needed by AController;
     * leaf subclasses must implement.
     *
     * @author mlh
     */

    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    /*
     * Selection
     */

    public DesignEditorSelectionState getSelection()
    {
        return selection;
    }


    public void selectAllFrames()
    {
        // deselect any selected transitions
        selection.deselectAllTransitions();

        Iterator<DesignEditorFrame> frameFigures =
            structureView.getAllFrameFigures();

        while (frameFigures.hasNext()) {
            DesignEditorFrame frameFigure = frameFigures.next();

            selection.selectFrame(frameFigure);
        }
    }


    public DesignEditorSelectionState getSelectionState()
    {
        return selection;
    }

    @Override
    public View getView()
    {
        return view;
    }

    // Really want a keyword for "package" visibility!
    protected InteractionDrawingEditor getViewEditor()
    {
        return view.getEditor();
    }

    // For use solely by DesignEditorControllerTest!
    public DesignEditorUIModel getUIModel()
    {
        return uiModel;
    }

    protected AlertHandler createFrameSelectionHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                FrameSelectionChange evt = (FrameSelectionChange) alert;

                ActionPropertySet actionProps = view.getActionPropertySet();

                if (evt != null) {
                    if (evt.changedFrameFigure != null) {
                        evt.changedFrameFigure.setSelected(evt.selected);

                        // Handle property sheet selection
                        Frame[] selectedFrames = selection.getSelectedFrames();
                        int selectedFrameCount = selectedFrames.length;

                        if (selectedFrameCount > 0) {
                            actionProps.setComposite(ActionPropertySet.FRAME);

                            if (selectedFrameCount == 1) {
                                actionProps.setFrameName(selectedFrames[0]);
                            }
                        }
                        else {
                            actionProps.setComposite(ActionSet.USE_NONE);
                        }

                        actionProps.enableFrameName(selectedFrameCount == 1);
                    }
                    else {
                        actionProps.setComposite(ActionSet.USE_NONE);

                        Iterator<DesignEditorFrame> frameFigures =
                            selection.getSelectedFrameFigures();

                        while (frameFigures.hasNext()) {
                            DesignEditorFrame frameFigure = frameFigures.next();

                            frameFigure.setSelected(evt.selected);
                        }
                    }

                    // Repaint the frame contents
                    delayedRepainting.requestRepaint(REPAINT_ALL);
                }
            }
        };
    } // createFrameSelectionHandler

    protected AlertHandler createTransitionSelectionHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                TransitionSelectionChange evt =
                    (TransitionSelectionChange) alert;

                if (evt != null) {
                    InteractionDrawingEditor editor = view.getEditor();

                    if (evt.changedTransitionFigure != null) {
                        evt.changedTransitionFigure.setSelected(editor,
                                                                evt.selected);
                    }
                    else {
                        Iterator<DesignEditorTransition> transitionFigures =
                            selection.getSelectedTransitionFigures();

                        while (transitionFigures.hasNext()) {
                            DesignEditorTransition transitionFigure =
                                transitionFigures.next();

                            transitionFigure.setSelected(editor, evt.selected);
                        }
                    }

                    // Repaint the contents
                    delayedRepainting.requestRepaint(REPAINT_ALL);
                    updateView(evt.changedTransitionFigure == null);
                }
            }
        };
    } // createTransitionSelectionHandler

    public void resetVisibleArea()
    {
        StandardDrawingEditor e = view.getEditor();
        e.getLWS().getUpdateManager().performUpdate();

        DoubleSize extent = structureView.getPreferredSize();

        e.setMinVisibleArea(PrecisionUtilities.round(extent.width),
                            PrecisionUtilities.round(extent.height),
                            false);
    }

    protected void ensureRectIsVisible(Rectangle frameBounds)
    {
        resetVisibleArea();

        StandardDrawingEditor e = view.getEditor();
        org.eclipse.swt.graphics.Rectangle visibleBounds = e.getVisibleBounds();

        double scale = structureView.getZoom();

        double right = frameBounds.x + frameBounds.width;
        double bottom = frameBounds.y + frameBounds.height;

        int frameX = PrecisionUtilities.round(frameBounds.x * scale);
        int frameY = PrecisionUtilities.round(frameBounds.y * scale);
        int frameWidth = PrecisionUtilities.round(right * scale) - frameX;
        int frameHeight = PrecisionUtilities.round(bottom * scale) - frameY;

        int newOriginX;
        int newOriginY;

        // If possible, move the origin to contain the frame
        if (visibleBounds.width >= frameWidth) {
            if (frameX < visibleBounds.x) {
                newOriginX = frameX;
            }
            else {
                newOriginX = visibleBounds.x;

                int frameRight = frameX + frameWidth;
                int visibleRight =
                    visibleBounds.x + visibleBounds.width;

                if (frameRight > visibleRight) {
                    newOriginX += (frameRight - visibleRight);
                }
            }
        }
        else {
            // Otherwise, center the frame horizontally
            newOriginX =
                frameX + (frameWidth / 2) - (visibleBounds.width / 2);
        }

        // If possible, move the origin to contain the frame
        if (visibleBounds.height >= frameHeight) {
            if (frameY < visibleBounds.y) {
                newOriginY = frameY;
            }
            else {
                newOriginY = visibleBounds.y;

                int frameBottom = frameY + frameHeight;
                int visibleBottom =
                    visibleBounds.y + visibleBounds.height;

                if (frameBottom > visibleBottom) {
                    newOriginY += (frameBottom - visibleBottom);
                }
            }
        }
        else {
            // Otherwise, use the top of the frame
            newOriginY = frameY;
        }

        e.setScrollOrigin(newOriginX, newOriginY);
    } // ensureRectIsVisible

    protected AlertHandler createFrameAddHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                StructureViewUIModel.FrameAdd evt = (StructureViewUIModel.FrameAdd) alert;

                ensureRectIsVisible(evt.frameFigure.getBounds());
            }
        };
    }

    protected AlertHandler createFrameShapeChangeHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                StructureViewUIModel.FrameShapeChange evt = (StructureViewUIModel.FrameShapeChange) alert;

                resetVisibleArea();

                if ((editor != null) &&
                    editor.getVisible() &&
                    (evt.getFrameFigure() == (DesignEditorFrame)
                                                             editor.getData()))
                {
                    delayedRepainting.requestRepaint(REPAINT_EDITOR);
                }

                delayedRepainting.requestRepaint(REPAINT_SELECT_HANDLES);

                // Ensure that the frame is still visible.
                ensureRectIsVisible(evt.getFrameFigure().getBounds());
            }
        };
    }

    protected AlertHandler createFrameNameChangeHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                StructureViewUIModel.FrameNameChange evt = (StructureViewUIModel.FrameNameChange) alert;
                Frame f = evt.getFrame();

                if (selection.isFrameSelected(f)) {
                    view.getActionPropertySet().setFrameName(f);
                }
            }
        };
    }
    protected AlertHandler createFrameRecoveryHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                StructureViewUIModel.FrameRecovery evt = (StructureViewUIModel.FrameRecovery) alert;

                if (evt != null) {
                    DesignEditorFrame frameFigure = evt.getFrameFigure();

                    selection.deselectFrame(frameFigure);

                    // If removing the DesignEditorFrame for the
                    // rename text editor, cancel the rename
                    // operation and clean up.
                    cleanupFrameEditor(frameFigure);
                }
            }
        };
    }

    protected AlertHandler createTransitionAddRemoveHandler()
    {
        return new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                StructureViewUIModel.TransitionAddRemove evt = (StructureViewUIModel.TransitionAddRemove) alert;

                if (evt != null) {
                    DesignEditorTransition transitionFigure =
                        evt.getTransitionFigure();

                    if (evt.isAdd) {
                        delayedTransitionSelection.addToSelection(transitionFigure.getTransition(),
                                                                  transitionFigure);
                    }
                    else if (transitionFigure.isSelected()) {
                        selection.deselectTransition(transitionFigure);
                        delayedTransitionSelection.removeFromSelection(transitionFigure.getTransition());
                    }
                }
            }
        };
    }

    protected void setFrameChangeHandlers(final Design design)
    {
        design.addHandler(this, NameChangeAlert.class, renameHandler);

        AlertHandler frameChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design.FrameChange chg = (Design.FrameChange) alert;

                    if (chg != null) {
                        updateView(false);

                        Frame frame = (Frame) chg.element;

                        if (chg.isAdd) {
                            DesignEditorFrame frameFigure =
                                structureView.getFrameFigure(frame);

                            // A newly created frame should be selected.
                            delayedFrameSelection.addToSelection(frame,
                                                                 frameFigure);
                        }
                        else {
                            delayedFrameSelection.removeFromSelection(frame);
                        }

                        delayedRepainting.requestRepaint(REPAINT_ALL);
                    }
                }
            };

        design.addHandler(this, Design.FrameChange.class, frameChangeHandler);

        frameChangeHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    Design.FrameSetChange chg =
                        (Design.FrameSetChange) alert;

                    if (chg != null) {
                        updateView(false);

                        if (chg.isAdd) {
                            for (Frame frame : chg.frames) {
                                DesignEditorFrame frameFigure =
                                    structureView.getFrameFigure(frame);

                                // A newly created frame should be selected.
                                delayedFrameSelection.addToSelection(frameFigure.getFrame(),
                                                                     frameFigure);
                            }

                            delayedRepainting.requestRepaint(REPAINT_ALL);
                        }
                        else {
                            for (Frame frame : chg.frames) {
                                delayedFrameSelection.removeFromSelection(frame);
                            }
                        }
                    }
                }
            };

        design.addHandler(this,
                          Design.FrameSetChange.class,
                          frameChangeHandler);

        design.addHandler(this,
                          Design.DeviceTypeChange.class,
                          new AlertHandler() {

                              public void handleAlert(EventObject alert)
                              {
                                  Set<DeviceType> deviceTypeSet =
                                      design.getDeviceTypes();
                                  int deviceTypes =
                                      DeviceType.buildDeviceSet(deviceTypeSet);

                                  view.resetDeviceTypes(deviceTypes);
                              }
                          });
    } // setFrameChangeHandlers

    @Override
    public void setZoom(double scale)
    {
        super.setZoom(scale);
        delayedRepainting.requestRepaint(ZOOM_REPAINT);
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     *
     * @author mlh
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setEnabled(CogToolLID.NewFrame,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.AddDesignDevices,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomNormal,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomIn,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomOut,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomToFit,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.SkinNone,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinWireFrame,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinWinXP,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinMacOSX,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.SkinPalm,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.RenderAll,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.UnRender,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setEnabled(CogToolLID.ExportDesignToHTML,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ExportToXML,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED,
                   L10N.get("DE.ExportXMLLabel", "Export Design to XML"));
        setEnabled(DesignEditorLID.ImportImageDirectory,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setViewEnabledState(selection,
                            ListenerIdentifierMap.NORMAL);
    }


    public void setLIDEnabledState()
    {
        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }

    /**
     * Enables or disables LIDs as appropriate
     * @param sel the selection state on which to base enabling/disabling
     * @param availability NORMAL or CONTEXT
     * @see ListenerIdentifierMap
     */
    protected void setViewEnabledState(DesignEditorSelectionState sel,
                                       Boolean availability)
    {
        String label = "";
        int frameCount = sel.getSelectedFrameCount();
        if (frameCount > 0) {
            if (frameCount > 1) {
                label = " " + FRAMES_LABEL;
            }
            else {
                label = " " + FRAME_LABEL;
            }
        }
        int transitionCount = sel.getSelectedTransitionCount();
        if (transitionCount > 0) {
            if (transitionCount > 1) {
                label = " " + TRANSITIONS_LABEL;
            }
            else {
                label = " " + TRANSITION_LABEL;
            }
        }

        Text t = WindowUtil.getFocusedText();
        boolean editing = ((editor != null) && editor.getVisible());
        String cutCopyLabel = (editing || (t != null)) ? "" : label;

        boolean enabled = (frameCount > 0) || (transitionCount == 1);

        String editString = MenuFactory.EDIT_STRING;
        if (enabled) {
            editString += label;
        }
        setEnabled(CogToolLID.Edit, availability, enabled, editString);

        enabled = (frameCount > 0) ||
                  (transitionCount > 0);

        String deleteString = MenuFactory.DELETE_STRING + label;
        setEnabled(CogToolLID.Delete, availability, enabled, deleteString);

        // Enable move/reorder items if any frames are selected
        enabled = (frameCount > 0);

        String cutString = MenuFactory.CUT_STRING;
        if (enabled) {
            cutString += cutCopyLabel;
        }
        setEnabled(CogToolLID.Cut, availability, enabled, cutString);
        String copyString = MenuFactory.COPY_STRING;
        if (enabled) {
            copyString += cutCopyLabel;
        }
        setEnabled(CogToolLID.Copy, availability, enabled, copyString);

        setEnabled(CogToolLID.NudgeLeft, availability, enabled);
        setEnabled(CogToolLID.NudgeRight, availability, enabled);
        setEnabled(CogToolLID.NudgeDown, availability, enabled);
        setEnabled(CogToolLID.NudgeUp, availability, enabled);

        // TODO: For now, don't allow duplication of transitions
        String dupString = MenuFactory.DUPLICATE_STRING;
        if (enabled) {
            dupString += label;
        }
        setEnabled(CogToolLID.Duplicate, availability, enabled, dupString);

        // the following 3 menu items involve code snippets copied from
        // FrameEditor classes
        setEnabled(CogToolLID.SetBackgroundImage, availability, enabled,
                   SET_FRAME_BKG_IMAGE);
        setEnabled(CogToolLID.SetWidgetColor, availability, enabled);

        // only enable RemoveBackgroundImage if there are selected frames
        if (enabled) {
            // and at least one of those frames has a background image
            boolean foundBackgroundImage = false;
            Frame[] selFrames = sel.getSelectedFrames();

            for (Frame selFrame : selFrames) {
                if (selFrame.getBackgroundImage() != null) {
                    foundBackgroundImage = true;
                    break;
                }
            }

            setEnabled(CogToolLID.RemoveBackgroundImage,
                       availability,
                       foundBackgroundImage,
                       REMOVE_FRAME_BKG_IMAGE);
        }
        else {
            setEnabled(CogToolLID.RemoveBackgroundImage, availability, enabled,
                       REMOVE_FRAME_BKG_IMAGE);
        }

//        setEnabled(CogToolLID.BringToFront, availability, enabled);
//        setEnabled(CogToolLID.BringForward, availability, enabled);
//        setEnabled(CogToolLID.SendBackward, availability, enabled);
//        setEnabled(CogToolLID.SendToBack, availability, enabled);

        // Enable alignment items if multiple frames are selected
        enabled = (frameCount > 1);

        setEnabled(CogToolLID.AlignTop, availability, enabled);
        setEnabled(CogToolLID.AlignBottom, availability, enabled);
        setEnabled(CogToolLID.AlignLeft, availability, enabled);
        setEnabled(CogToolLID.AlignRight, availability, enabled);
        setEnabled(CogToolLID.AlignCenter, availability, enabled);
        setEnabled(CogToolLID.AlignHorizCenter, availability, enabled);
        setEnabled(CogToolLID.AlignVertCenter, availability, enabled);

        // Enable spacing items if at least 3 frames are selected
        enabled = (frameCount >= 3);

        setEnabled(CogToolLID.SpaceVertically, availability, enabled);
        setEnabled(CogToolLID.SpaceHorizontally, availability, enabled);

        // Edit and Rename enabled if a single selection
        enabled = (frameCount == 1);

        String renameString = MenuFactory.RENAME_STRING;
        if (enabled) {
            renameString += label;
        }
        setEnabled(CogToolLID.Rename, availability, enabled, renameString);

        Design modelDesign = uiModel.getDesign();
        enabled = (modelDesign.getFrames().size() > 0);

        setEnabled(CogToolLID.SelectAll, availability, enabled,
                   SELECT_ALL_FRAMES);

        // Draws the dot to indicate that the correct skin type is selected
        setSelected(CogToolLID.SkinNone, availability, false);
        setSelected(CogToolLID.SkinWireFrame, availability, false);
        setSelected(CogToolLID.SkinMacOSX, availability, false);
        setSelected(CogToolLID.SkinWinXP, availability, false);
        setSelected(CogToolLID.SkinPalm, availability, false);

        SkinType skin = modelDesign.getSkin();
        CogToolLID id = null;

        if (skin == SkinType.None) {
            id = CogToolLID.SkinNone;
        }
        else if (skin == SkinType.WireFrame) {
            id = CogToolLID.SkinWireFrame;
        }
        else if (skin == SkinType.MacOSX) {
            id = CogToolLID.SkinMacOSX;
        }
        else if (skin == SkinType.WinXP) {
            id = CogToolLID.SkinWinXP;
        }
        else if (skin == SkinType.Palm) {
            id = CogToolLID.SkinPalm;
        }

        if (id != null) {
            setSelected(id, availability, true);
        }

        setEnabled(CogToolLID.ClearFrameTemplate,
                   ListenerIdentifierMap.ALL,
                   FrameTemplateSupport.hasFrameTemplate(design));
    }

    // LID Transmuter Stuff
    @Override
    public ListenerIdentifier transmute(ListenerIdentifier id,
                                        boolean isContextSelection)
    {
        ListenerIdentifier specificLID =
            super.transmute(id, isContextSelection);

        // Check if super has already specialized this
        if (specificLID != id) {
            return specificLID;
        }

        DesignEditorSelectionState seln =
            isContextSelection ? contextSelection : selection;

        // Give priority to frame stuff
        if (seln.getSelectedFrameCount() > 0) {
            if ((id == CogToolLID.Paste) && ClipboardUtil.hasImageData()) {
                return CogToolLID.PasteBackgroundImage;
            }

            specificLID = DesignEditorLID.frameLIDs.get(id);
        }

        // Fallback to undertaking stuff if necessary
        else {
            specificLID = DesignEditorLID.transitionLIDs.get(id);
        }

        return (specificLID != null) ? specificLID : id;
    }

    /**
     * Do any set-up before <code>performAction</code> is invoked.
     *
     * @param id the transmuted key specifying the semantic nature of the
     *           action to be performed
     */
    @Override
    protected void setUpPerformAction(ListenerIdentifier id)
    {
        super.setUpPerformAction(id);

        int selectionMask = canIDCauseSelection(id);

        if (isSelectionFlagSet(selectionMask,
                               DesignEditorLID.CAUSES_FRAME_SELECTION))
        {
            delayedFrameSelection.setActive(true);
        }

        if (isSelectionFlagSet(selectionMask,
                               DesignEditorLID.CAUSES_TRANSITION_SELECTION))
        {
            delayedTransitionSelection.setActive(true);
        }
    }

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

        setUpPerformAction(transmutedLID);

        DesignEditorSelectionState selnStateToUse =
            isContextSelection ? contextSelection : selection;

        if (transmutedLID == CogToolLID.PasteBackgroundImage) {
            return new DesignEditorUI.PasteBackgroundImageParms(selnStateToUse,
                                                 ClipboardUtil.fetchImageData());
        }

        // Merged paths for editing widgets, since GRAFFITI crosses both
        if ((transmutedLID == DesignEditorLID.ChangeWidgetAction) ||
            (transmutedLID == DesignEditorLID.ChangeDeviceAction))
        {
            return new DesignEditorUI.ChangeActionParameters(view.getActionProperties(),
                                              selnStateToUse);
        }

        if (transmutedLID == DesignEditorLID.ChangeDelay) {
            ActionProperties properties = view.getActionProperties();

            return new DesignEditorUI.ChangeDelayParameters(properties.delayInSecs,
                                             properties.delayLabel,
                                             selnStateToUse);
        }

        if (transmutedLID == DesignEditorLID.DuplicateFrame) {
            return new DesignEditorUI.DuplicateParameters(16.0, 16.0, selnStateToUse);
        }

        if (transmutedLID == DesignEditorLID.EditTransition) {
            return new DesignEditorUI.EditTransitionParameters(selnStateToUse,
                                                ActionProperties.UNSET);
        }

        if (transmutedLID == DesignEditorLID.AlignTop ||
            transmutedLID == DesignEditorLID.AlignBottom ||
            transmutedLID == DesignEditorLID.AlignLeft ||
            transmutedLID == DesignEditorLID.AlignRight ||
            transmutedLID == DesignEditorLID.AlignCenter ||
            transmutedLID == DesignEditorLID.AlignHorizCenter ||
            transmutedLID == DesignEditorLID.AlignVertCenter ||
            transmutedLID == DesignEditorLID.SpaceHorizontally ||
            transmutedLID == DesignEditorLID.SpaceVertically)
        {
            Frame[] frames = selection.getSelectedFrames();
            Map<Frame, DoubleRectangle> frameMap =
                new HashMap<Frame, DoubleRectangle>();

            for (Frame frame : frames) {
                DesignEditorFrame figure =
                    structureView.getFrameFigure(frame);

                Rectangle bounds = figure.getBounds();
                frameMap.put(frame,
                             new DoubleRectangle(bounds.x,
                                                 bounds.y,
                                                 bounds.width,
                                                 bounds.height));
            }

            return frameMap;
        }


        return selnStateToUse;
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
    @Override
    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        if (menuHidden) {
            Iterator<DesignEditorFrame> frameFigs;

            if (selection.getSelectedFrameCount() > 0) {
                frameFigs = selection.getSelectedFrameFigures();

                while (frameFigs.hasNext()) {
                    DesignEditorFrame fig = frameFigs.next();

                    fig.dynamicHighlight(false);
                }
            }

            if (contextSelection.getSelectedFrameCount() > 0) {
                frameFigs = contextSelection.getSelectedFrameFigures();

                while (frameFigs.hasNext()) {
                    DesignEditorFrame fig = frameFigs.next();

                    fig.dynamicHighlight(false);
                }
            }
        }

        super.cleanup(okToContinue, menuHidden);
    }

    /**
     * Triggers the frame-renaming functionality in the UI on the given
     * frame.
     */

    public void initiateFrameRename(Frame frameToRename)
    {
        DesignEditorFrame renameFrameFigure =
            structureView.getFrameFigure(frameToRename);

        initiateFrameRename(renameFrameFigure);
    }

    /**
     * Triggers the frame-renaming functionality in the UI on the given
     * selection.
     */

    public void initiateFrameRename(FrameSelectionState frameToRename)
    {
        // Can progress only if one frame is selected
        if (frameToRename.getSelectedFrameCount() == 1) {
            // Rename the selected frame
            initiateFrameRename(frameToRename.getSelectedFrames()[0]);
        }
    }

    public void initiateFrameRename(DesignEditorFrame frameFigure)
    {
        // The editor control must be a child of an SWT object (the canvas)
        if (editor == null) {
            editor = new FrameNameEditor();
        }

        if (frameFigure != null) {
            editor.editFrameName(frameFigure);
        }

        setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
    }

    protected void repaintEditor()
    {
        if (editor != null) {
            editor.repaintTextEditor(structureView.getZoom());
        }
    }

    protected boolean confirmRenameFrame()
    {
        boolean success = true;

        if ((editor != null) && editor.inUse()) {
            String newName = editor.getText();
            DesignEditorFrame frameFigure =
                (DesignEditorFrame) editor.getData();

            Frame frameToRename = frameFigure.getFrame();

            cleanupFrameEditor(frameFigure);

            success = performAction(DesignEditorLID.RenameFrame,
                                    new DesignEditorUI.FrameRenameParameters(frameToRename,
                                                              newName),
                                    false);
        }

        return success;
    }

    /**
     * Removes stale Text control and selection listener, but only
     * if the editor is for the given DesignEditorFrame; if the given
     * frame figure is <code>null</code>, simply clean up.
     */
    protected void cleanupFrameEditor(DesignEditorFrame frameFig)
    {
        // Prevent access to the frame name Text editor
        // frameFig is null or is editing the given frame's name
        if ((editor != null) &&
            editor.inUse() &&
            (frameFig == editor.getData()))
        {
            editor.cleanup();

            setViewEnabledState(selection, ListenerIdentifierMap.NORMAL);
        }
    }

    /**
     * Removes stale Text control and selection listener.
     */
    protected void cleanupFrameEditor()
    {
        if (editor != null) {
            editor.cleanup();
        }
    }

    protected void showContextMenu(DesignEditorSelectionState seln,
                                   boolean context)
    {
        setViewEnabledState(seln, ListenerIdentifierMap.CONTEXT);

        if (seln.getSelectedFrameCount() > 0) {
            view.showFrameMenu(context);
        }
        else if (seln.getSelectedTransitionCount() > 0) {
            view.showTransitionMenu(context);
        }
        else {
            view.showStandardMenu();
        }
    }

    @Override
    public void showContextMenu()
    {
        showContextMenu(selection, View.SELECTION);
    }

    // Context menu stuff
    @Override
    public void showContextMenu(int x, int y)
    {
        // Check which region was hit
        IFigure fig =
            structureView.getFigureAtXY(x, y,
                                             StructureViewUIModel.NO_SOURCE);

        // Invocation in empty space
        if (fig == null) {
            contextSelection.deselectAll();
            showContextMenu(contextSelection,
                            View.CONTEXT);
        }

        // Invocation on a Transition
        else if (fig instanceof DesignEditorTransition) {
            DesignEditorTransition transitionFig =
                (DesignEditorTransition) fig;
//fig.setToolTip(null);
            if (transitionFig.isSelected()) {
                showContextMenu();
            }
            else {
                // Populate the context selection
                contextSelection.setSelectedTransition(transitionFig);
                showContextMenu(contextSelection,
                                View.CONTEXT);
            }
        }

        // Invocation on a Frame
        else {    // fig instanceof DesignEditorFrame
            DesignEditorFrame frameFig = (DesignEditorFrame) fig;

            if (frameFig.isSelected()) {
                // Indicate the selection to be used visually
                Iterator<DesignEditorFrame> figs =
                    selection.getSelectedFrameFigures();

                while (figs.hasNext()) {
                    frameFig = figs.next();

                    frameFig.dynamicHighlight(true);
                }

                showContextMenu();
            }
            else {
                // Indicate the context selection visually
                frameFig.dynamicHighlight(true);

                // Populate the context selection
                contextSelection.setSelectedFrame(frameFig);

                showContextMenu(contextSelection,
                                View.CONTEXT);
            }

            view.getEditor().getLWS().getUpdateManager().performUpdate();
        }
    } // showContextMenu

    protected void setFigureOrigin(IFigure f, double x, double y)
    {
        f.setLocation(new Point(PrecisionUtilities.round(x),
                                PrecisionUtilities.round(y)));
    }

    protected void repaintSelectHandles()
    {
        Iterator<DesignEditorTransition> selectedTransitions =
            selection.getSelectedTransitionFigures();

        while (selectedTransitions.hasNext()) {
            DesignEditorTransition selectedTransition =
                selectedTransitions.next();

            // May be in the midst of deselecting; check!
            if (selectedTransition.isSelected()) {
                selectedTransition.repaintSelection(view.getEditor());
            }
        }
    }

    /**
     * Support for refresh during dynamic operations
     * (e.g., DesignEditorMouseState).
     */
    protected void performRepaintUpdates()
    {
        view.getEditor().getLWS().getUpdateManager().performUpdate();
        repaintSelectHandles();
    }

    /**
     * Support for centering selection when zooming
     */
    @Override
    protected Rectangle getSelectedRegion()
    {
        Iterator<DesignEditorFrame> selectedFigs =
            selection.getSelectedFrameFigures();

        Rectangle r = computeUnion(selectedFigs);

        if (r != null) {
            return r;
        }

        if (selection.getSelectedTransitionCount() > 0) {
            Iterator<DesignEditorTransition> transitionFigs =
                selection.getSelectedTransitionFigures();

            return computeUnion(transitionFigs);
        }

        return super.getSelectedRegion();
    }

    protected void updateView(boolean deselectAll)
    {
        if (view.isDisposed()) {
            return;
        }

        ActionPropertySet actionProps = view.getActionPropertySet();

        int selectedTransitionCount =
            selection.getSelectedTransitionCount();

        int selectedFrameCount = selection.getSelectedFrameCount();

        if (deselectAll || (selectedFrameCount + selectedTransitionCount == 0))
        {
            actionProps.useParameters(ActionSet.USE_NONE);
        }
        else if (selectedTransitionCount == 1) {
            Transition transition =
                selection.getSelectedTransitions()[0];

            AAction action = transition.getAction();
            DeviceType type = action.getDefaultDeviceType();
            int device = ActionSet.USE_NONE;

            if (action instanceof GraffitiAction) {
                device = ActionSet.USE_GRAFFITI_WIDGET;
            }
            else if (type == DeviceType.Mouse) {
                device = ActionSet.USE_MOUSE;
            }
            else if (type == DeviceType.Touchscreen) {
                device = ActionSet.USE_TOUCHSCREEN;
            }
            else if (type == DeviceType.Keyboard) {
                device = ActionSet.USE_KEYBOARD;
            }
            else if (type == DeviceType.Voice) {
                device = ActionSet.USE_VOICE;
            }
            else if (type == null) {
                // Generally, a hover action; pick whichever is active
                if (actionProps.isMouseSelected()) {
                    device = ActionSet.USE_MOUSE;
                }
                else if (actionProps.isTouchSelected()) {
                    device = ActionSet.USE_TOUCHSCREEN;
                }
            }

            actionProps.setComposite(device);

            ActionProperties properties = view.getActionProperties();

            view.getDefaultProperties(properties);

            properties.updateProperties(transition,
                                        transition.getAction(),
                                        transition.getSource());

            int limitMode =
                ActionProperties.determineChangeActionMode(transition.getSource());

            actionProps.setLimitMode(limitMode, properties.useWhichParts);
            actionProps.setProperties(properties, properties.useWhichParts);
        }
        else if (selectedTransitionCount > 1) {
            actionProps.useParameters(ActionPropertySet.MULT_TRANS);
        }

        if (selectedFrameCount == 1) {
            actionProps.updateFrameComposite(selection.getSelectedFrames()[0]);
        }
        else if ((selectedFrameCount == 0) && (selectedTransitionCount == 0)) {
            actionProps.updateEmptyComposite(design, false);
        }
    }

    // TODO: same as widgetResizeUnderXY in FrameEditorUI!
    public ResizeThumb getResizeAtXY(int x, int y)
    {
        IFigure f =
            view.getEditor().getInteractionFigure().findFigureAt(x, y);

        if (f instanceof ResizeThumb) {
            return (ResizeThumb) f;
        }

        return null;
    }


    public ActionType getCurrentActionType()
    {
        return view.getActionType();
    }


    public void getDefaultProperties(TransitionSource source,
                                     ActionProperties properties)
    {
        view.getDefaultProperties(properties);
        ActionType transitionType = getCurrentActionType();
        Set<DeviceType> deviceTypes = uiModel.getDesign().getDeviceTypes();

        properties.setInitialActionType(source, transitionType, deviceTypes);
    }


    public DoubleRectangle getFrameDisplayBounds(Frame f)
    {
        double zoom = structureView.getZoom();
        DesignEditorFrame figure = structureView.getFrameFigure(f);
        Rectangle r = figure.getBounds();

        return new DoubleRectangle(r.x * zoom,
                                   r.y * zoom,
                                   r.width * zoom,
                                   r.height * zoom);
    }
}
