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
import java.util.Iterator;
import java.util.Set;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Color;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.uimodel.StructureViewUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.SEFrameChooserUIModel;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.view.SEFrameChooserView;
import edu.cmu.cs.hcii.cogtool.view.ScriptView;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.View;

public class SEFrameChooserUI extends SEDefaultUI
{
    protected static final String seChooseFrameWindowTitle =
        L10N.get("SE.ChooseFrameWindowTitle", "Choose Start Frame - ");

    protected SEFrameChooserView view;
    protected SEFrameChooserSelectionState selection;
    protected SEFrameChooserInteraction interaction;

    protected TaskApplication taskApp;

    protected SEFrameChooserUIModel uiModel;
    protected StructureViewUIModel structureView; // cached from uiModel!

    protected DelayedRepaint delayedRepainting;

    protected AlertHandler initialStateChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                Demonstration.InitialStateChange chg = (Demonstration.InitialStateChange) alert;

                Demonstration demo = taskApp.getDemonstration();
                DefaultModelGeneratorState initialState = demo.getInitialState();
                boolean mouseHand = initialState.getMouseHand();

                if (chg.involvesChange(Demonstration.InitialStateChange.START_FRAME)) {
                    selection.setSelectedFrame(demo.getStartFrame());
                }
                if (chg.involvesChange(Demonstration.InitialStateChange.MOUSE_HAND)) {
                    view.setUserMouseHand(mouseHand);
                }
                if (chg.involvesChange(Demonstration.InitialStateChange.GENERATOR_STATE)) {
                    view.setStartMouseHandLocation(initialState.getHandLocation(mouseHand));
                }
            }
        };

    protected DesignEditorFrame currentFrameFig = null;

    protected Color unselectedColor = null;

    /**
     * Create an interactive FrameEditor View. Creates a window.
     */
    public SEFrameChooserUI(TaskApplication ta,
                            Project scriptProject,
                            UndoManager undoMgr)
    {
        super(ta.getTask(), ta.getDesign(), scriptProject, undoMgr, false);

        taskApp = ta;

        selection = new SEFrameChooserSelectionState();

        uiModel = new SEFrameChooserUIModel(ta, scriptProject);

        structureView = uiModel.getStructureView();

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                public void doWork()
                {
                    uiModel.resetHiddenTransitionSources();

                    super.doWork();

                    if (selection.selectedFrame != null) {
                        updateSelection(selection.selectedFrame);
                    }

                    undoMgrViewHandler.resetView(undoManager);
                }
            };

        CogTool.repaintPhase.addDelayedWork(delayedRepainting);

        AlertHandler frameOriginChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    StructureViewUIModel.FrameShapeChange evt = (StructureViewUIModel.FrameShapeChange) alert;
                    Frame frame = evt.getFrameFigure().getFrame();

                    if (frame == selection.selectedFrame) {
                        // Update selection area.
                        updateSelection(frame);
                    }

                    resetVisibleArea();
                }
            };

        structureView.addHandler(this,
                                      StructureViewUIModel.FrameShapeChange.class,
                                      frameOriginChangeHandler);

        int deviceTypes =
            DeviceType.buildDeviceSet(ta.getDesign().getDeviceTypes());

        view =
            new SEFrameChooserView(deviceTypes,
                                   lIDMap,
                                   this,
                                   menuData,
                                   structureView.getContents(),
                                   null, // no motion listener
                                   new SEFrameChooserMouseState(this),
                                   uiModel.getRowRenderer(),
                                   this,
                                   getWindowLocation());

        setZoomEditor(view.getEditor());

        interaction = new SEFrameChooserInteraction(getView());

        updateTitle();

        AlertHandler designFrameChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    Design.FrameChange evt = (Design.FrameChange) alert;

                    // No matter what the change is, just rebuild the list.
                    updateView();

                    Frame frame = (Frame) evt.element;

                    // If it's a delete, we need to update selection in case
                    // the selected item was deleted.
                    if ((! evt.isAdd) && (frame == selection.selectedFrame)) {
                        // clear selection
                        selection.deselectAll();
                    }

                    addRemoveFrameNameChgHandler(frame, evt.isAdd);
                }
            };

        design.addHandler(this,
                               Design.FrameChange.class,
                               designFrameChangeHandler);
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

        updateView();

        AlertHandler selectionChangeHandler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    SEFrameChooserSelectionState.SelectionChange change =
                        (SEFrameChooserSelectionState.SelectionChange) alert;
                    updateSelection(change.selected);
                }
            };

        selection.addHandler(this,
                                  SEFrameChooserSelectionState.SelectionChange.class,
                                  selectionChangeHandler);

        // Restore zoom level
        setZoom(getWindowZoom(ta.getDesign()));

        Demonstration demo = taskApp.getDemonstration();
        DefaultModelGeneratorState initialState = demo.getInitialState();
        boolean mouseHand = initialState.getMouseHand();

        updateSelection(demo.getStartFrame());
        view.setUserMouseHand(mouseHand);
        view.setStartMouseHandLocation(initialState.getHandLocation(mouseHand));

        demo.addHandler(this,
                        Demonstration.InitialStateChange.class,
                        initialStateChangeHandler);

        setInitiallyEnabled(true);
    } // ctor


    @Override
    public void resetVisibleArea()
    {
        StandardDrawingEditor e = view.getEditor();
        e.getLWS().getUpdateManager().performUpdate();

        DoubleSize extent = structureView.getPreferredSize();

        e.setMinVisibleArea(PrecisionUtilities.round(extent.width),
                            PrecisionUtilities.round(extent.height),
                            false);
    }

    /**
     * Recover any system resources being used to support this window/view.
     *
     * @author mlh
     */
    @Override
    public void dispose()
    {
        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        taskApp.getDemonstration().removeAllHandlers(this);

        structureView.removeAllHandlers(this);

        selection.removeAllHandlers(this);

        super.dispose();
    }

    /**
     * @return Interaction Object
     */

    public SEFrameChooserInteraction getInteraction()
    {
        return interaction;
    }


    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    @Override
    public View getView()
    {
        return view;
    }

    /**
     * Supports shared implementation of editor-based operations
     * between SEFrameChooser and SEDemo
     */
    protected ScriptView getScriptView()
    {
        return view;
    }


    /**
     * Support for centering selection when zooming
     */
    @Override
    protected Rectangle getSelectedRegion()
    {
        Frame frame = selection.getSelectedFrame();

        if (frame != null) {
            DesignEditorFrame frameFig =
                structureView.getFrameFigure(frame);

            return frameFig.getBounds();
        }

        return super.getSelectedRegion();
    }

    /**
     * Update the view's contents.
     * Get the list of frames sorted alphabetically.
     */
    @Override
    protected void updateView()
    {
        Iterator<Frame> sortedFrames =
            NamedObjectUtil.getSortedList(design.getFrames()).iterator();
        // Set up the view contents
        view.getFrameList().setListContents(sortedFrames);
    }

    /**
     * Resize the internal items to support the new size.
     */
    public void resizeContents(int width, int height, boolean resizeMin)
    {
        view.getEditor().resizeContents(width, height, resizeMin);
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        // Currently, nothing more to enable/disable
    }

    /**
     * Set design's zoom setting
     */
    @Override
    public void setZoom(double zoom)
    {
        // Save zoom setting for design, not script (mlh: why?)
        view.getEditor().setZoomSetting(zoom);
        saveWindowZoom(design, zoom);
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

        return isContextSelection ? null : selection;
    }

    @Override
    public void showContextMenu(int x, int y)
    {
        DesignEditorFrame f = structureView.getFrameAtXY(x, y);

        if (f != null) {
            performAction(CogToolLID.SetStartFrame, f.getFrame());
//            this.selection.setSelectedFrame(f.getFrame());
            setViewEnabledState(ListenerIdentifierMap.ALL);
            view.showPositionalMenu();
        }
        else {
            view.showStandardMenu();
        }
    }

    @Override
    public void showContextMenu()
    {
        setViewEnabledState(ListenerIdentifierMap.CONTEXT);

        if (selection.getSelectedFrame() != null) {
            view.showPositionalMenu();
        }
        else {
            view.showStandardMenu();
        }
    }

    /**
     * Updates the selection in the view.
     * Highlights area in Structure View,
     */
    protected void updateSelection(Frame f)
    {
        if (currentFrameFig != null) {
            currentFrameFig.setBackgroundColor(unselectedColor);
        }

        if (f == null) {
            view.getFrameList().deselectAll();
        }
        else {
            // Use the frame object itself as the key
            view.getFrameList().setListItemHighlighted(f);

            currentFrameFig = structureView.getFrameFigure(f);

            if (currentFrameFig != null) {
                unselectedColor = currentFrameFig.getBackgroundColor();
                currentFrameFig.setBackgroundColor(ColorConstants.orange);
            }
        }

        setViewEnabledState(ListenerIdentifierMap.ALL);
    }


    public void setLIDEnabledState()
    {
        setViewEnabledState(ListenerIdentifierMap.NORMAL);
    }

    /**
     * Update the view and enable and disable buttons as needed based on
     * selection
     * @param select
     */
    protected void setViewEnabledState(Boolean availability)
    {
        setEnabled(SEFrameChooserLID.OpenScriptEditor,
                   availability,
                   taskApp.getDemonstration().getStartFrame() != null);
    }

    public SEFrameChooserUIModel getUIModel()
    {
        return uiModel;
    }

    @Override
    protected Object getModelObject()
    {
        return taskApp;
    }
}
