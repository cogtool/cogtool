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

import java.util.Collection;
import java.util.EventObject;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.widgets.Label;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoubleSize;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Script;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.ListChange;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.DefaultSEUIModel;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.view.DefaultSEView;
import edu.cmu.cs.hcii.cogtool.view.StandardDrawingEditor;

public abstract class SEDefaultUI extends ZoomableUI
{
    protected AUndertaking task;        // cached from uiModel!
    protected Design design;           // cached from uiModel!

    protected DefaultSEView view = null;
    protected DefaultSEUIModel uiModel = null;

    protected MouseListener frameLabelListener =
        new MouseAdapter()
        {
            @Override
            public void mouseDoubleClick(MouseEvent evt)
            {
                Label frameNameLabel = (Label) evt.getSource();
                Frame f = design.getFrame(frameNameLabel.getText());

                if (f != null) {
                    performAction(DesignEditorLID.EditFrame, f);
                }
            }
        };

    protected DefaultSEUIModel getDefaultUIModel()
    {
        // Overridden by SEDemoUI and ScriptViewerUI
        return null;
    }

    protected AlertHandler frameNameChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert) {
                // This should be just a name change, so rebuild the list.
                updateView();
            }
        };

    // Used by SEDemoUI and ScriptViewerUI to handle new input devices
    protected AlertHandler inputDeviceChangeHandler =
        new AlertHandler()
        {

            public void handleAlert(EventObject alert)
            {
                Frame.InputDeviceChange deviceChange =
                      (Frame.InputDeviceChange) alert;

                view.addInputDevice(deviceChange.newDevice);
                uiModel.updateDeviceDisplay();
            }
        };

    protected static final String SCRIPT_PREFIX =
        L10N.get("WT.ScriptPrefix", "Script");

    protected static String buildWindowMenuLabel(Design design,
                                                 AUndertaking task)
    {
        String designName = SWTStringUtil.insertEllipsis(design.getName(),
                                                         StringUtil.EQUAL,
                                                         SWTStringUtil.DEFAULT_FONT);
        String taskName = SWTStringUtil.insertEllipsis(task.getName(),
                                                       StringUtil.EQUAL,
                                                       SWTStringUtil.DEFAULT_FONT);

        return SCRIPT_PREFIX +  ": " + designName + " > " + taskName;
    }

    public SEDefaultUI(AUndertaking t,
                       Design d,
                       Project scriptProject,
                       UndoManager undoMgr,
                       boolean supportSpeaker)
    {
        super(scriptProject,
              buildWindowMenuLabel(d, t),
              buildLeadItems(scriptProject, d),
              undoMgr);

        task = t;
        design = d;

        task.addHandler(this, NameChangeAlert.class, renameHandler);
        design.addHandler(this,
                               NameChangeAlert.class,
                               renameHandler);

        if (supportSpeaker) {
            design.addHandler(this,
                                   Design.DeviceTypeChange.class,
                                   new AlertHandler() {

                                       public void handleAlert(EventObject alert)
                                       {
                                           Set<DeviceType> dts =
                                               design.getDeviceTypes();
                                           int deviceTypes =
                                               DeviceType.buildDeviceSet(dts);

                                           view.addSpeakerDevice(deviceTypes);
                                       }
                                   });
        }

        Iterator<Frame> iter = design.getFrames().iterator();

        while (iter.hasNext()) {
            Frame frame = iter.next();

            frame.addHandler(this,
                             NameChangeAlert.class,
                             frameNameChangeHandler);
        }

        project.addHandler(this,
                                Project.DesignChange.class,
                                new AlertHandler()
                                {

                                    public void handleAlert(EventObject alert)
                                    {
                                        Project.DesignChange chg =
                                            (Project.DesignChange) alert;

                                        if ((! chg.isAdd) &&
                                            (chg.element == design))
                                        {
                                            closeOpenController();
                                        }
                                    }
                                });

        project.addHandler(this,
                                Project.TaskChange.class,
                                new AlertHandler()
                                {

                                    public void handleAlert(EventObject alert)
                                    {
                                        Project.TaskChange chg =
                                            (Project.TaskChange) alert;

                                        if ((! chg.isAdd) &&
                                            (chg.element == task))
                                        {
                                            closeOpenController();
                                        }
                                    }
                                });

        design.addHandler(this,
                               TaskApplication.TaskApplicationResultChange.class,
                               new AlertHandler()
                               {

                                   public void handleAlert(EventObject alert)
                                   {
                                       TaskApplication.TaskApplicationResultChange chg =
                                           (TaskApplication.TaskApplicationResultChange) alert;

                                       TaskApplication taskApp =
                                           project.getTaskApplication(chg.task,
                                                                      design);

                                       // If not in the project, must be removed!
                                       if (taskApp == null) {
                                           closeOpenController();
                                       }
                                   }
                               });

        AlertHandler closeIfContextRemovedHandler =
            new AlertHandler()
            {

                public void handleAlert(EventObject alert)
                {
                    ListChange chg = (ListChange) alert;

                    if ((! chg.isAdd) &&
                        ((chg.element == design) || (chg.element == task)))
                    {
                        closeOpenController();
                    }
                }
            };

        project.addHandler(this,
                                Project.DesignChange.class,
                                closeIfContextRemovedHandler);

        project.addHandler(this,
                                Project.TaskChange.class,
                                closeIfContextRemovedHandler);

        addCloseIfTaskRemoved(project.getUndertakings().iterator(),
                              closeIfContextRemovedHandler);
    }

    protected AlertHandler handleWidgetShapeChange =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                resetVisibleArea();
            }
        };

    public void resetVisibleArea()
    {
        StandardDrawingEditor e = view.getEditor();
        e.getLWS().getUpdateManager().performUpdate();

        DoubleSize extent = uiModel.getCurrentFrame().getPreferredSize();

        e.setMinVisibleArea(PrecisionUtilities.round(extent.width),
                            PrecisionUtilities.round(extent.height),
                            false);
    }

    protected void setViewUIModel(DefaultSEView v, DefaultSEUIModel uim)
    {
        view = v;
        uiModel = uim;
    }

    protected abstract void updateView();

    protected void addRemoveFrameNameChgHandler(Frame frame, boolean add)
    {
        if (add) {
            // Add a handler for name change events.
            frame.addHandler(this,
                             NameChangeAlert.class,
                             frameNameChangeHandler);
        }
        else {
            // Remove the handler for name change events.
            frame.removeHandler(NameChangeAlert.class,
                                frameNameChangeHandler);
        }
    }

    protected boolean determineNextSelection(Script script,
                                             Collection<AScriptStep> scriptSteps,
                                             int action,
                                             int stepIndex,
                                             DelayedSelection seln)
    {

        Demonstration demo = script.getDemonstration();
        int numDemoSteps = demo.getStepCount();
        boolean computableChange;

        // This works for Demonstration.ScriptStepChange.REMOVE_STEP
        // since the removed object's index should be the next item
        // to select.

        if (action == Demonstration.ScriptStepChange.ADD_STEP) {
            // Select the next item after those that were added
            if (scriptSteps != null) {
                int numStepsAdded = scriptSteps.size();

                stepIndex += numStepsAdded;
                computableChange = (numDemoSteps == numStepsAdded);
            }
            else {
                stepIndex++;
                computableChange = (numDemoSteps == 1);
            }
        }
        else {
            computableChange = (numDemoSteps == 0);
        }

        AScriptStep stepToSelect = demo.getStepAt(stepIndex);

        seln.addToSelection(stepToSelect);

        return computableChange;
    }

    protected void addCloseIfTaskRemoved(Iterator<AUndertaking> undertakings,
                                         AlertHandler closeIfTaskRemovedHandler)
    {
        while (undertakings.hasNext()) {
            AUndertaking undertaking = undertakings.next();

            if (undertaking.isTaskGroup()) {
                TaskGroup taskGroup = (TaskGroup) undertaking;

                taskGroup.addHandler(this,
                                     TaskGroup.TaskChange.class,
                                     closeIfTaskRemovedHandler);

                Iterator<AUndertaking> childUndertakings =
                    taskGroup.getUndertakings().iterator();

                addCloseIfTaskRemoved(childUndertakings,
                                      closeIfTaskRemovedHandler);
            }
        }
    }

    @Override
    protected void updateTitle()
    {
        String taskName = SWTStringUtil.insertEllipsis(task.getName(),
                                                       StringUtil.EQUAL,
                                                       SWTStringUtil.DEFAULT_FONT);
        getView().setWindowTitle(modificationFlag
                                       + SCRIPT_PREFIX
                                       + ": "
                                       + project.getName()
                                       + " > "
                                       + design.getName()
                                       + " > "
                                       + taskName
                                       + (OSUtils.MACOSX ? "" : UI.WINDOW_TITLE));
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return buildWindowMenuLabel(design, task);
    }

    /**
     * Sets the "always-enabled" widgets;
     * call this at the end of the subclass constructor!
     */
    @Override
    protected void setInitiallyEnabled(boolean forConstruction)
    {
        super.setInitiallyEnabled(forConstruction);

        setEnabled(CogToolLID.ZoomIn,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomOut,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomNormal,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(CogToolLID.ZoomToFit,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        // Turn off paste
        setEnabled(CogToolLID.Paste,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.DISABLED);
    }

    protected void removeTaskGroupHandlers(Iterator<AUndertaking> undertakings)
    {
        while (undertakings.hasNext()) {
            AUndertaking undertaking = undertakings.next();

            if (undertaking.isTaskGroup()) {
                ((TaskGroup) undertaking).removeAllHandlers(this);
            }
        }
    }

    @Override
    public void dispose()
    {
        removeTaskGroupHandlers(project.getUndertakings().iterator());

        task.removeAllHandlers(this);
        design.removeAllHandlers(this);

        Iterator<Frame> iter = design.getFrames().iterator();

        while (iter.hasNext()) {
            Frame frame = iter.next();
            frame.removeAllHandlers(this);
        }

        super.dispose();
    }
}