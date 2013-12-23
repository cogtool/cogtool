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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTGroupParameters;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTParameters;
import edu.cmu.cs.hcii.cogtool.uimodel.DictionaryEditorUIModel;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.PersistenceView;

/**
 * Creates a dialog box that allows users to send parameters to the SNIF-ACT
 * algorithm.  These parameters are: goal string (which is uneditable here; it
 * is the name of the task), number of trials to generate, k value (which
 * reflects the algorithm's willingness to match labels), the name of the
 * start frame, and a list of strings representing target frames that indicate
 * that the goal has been reached.  If the user is recomputing from an existing
 * SNIF-ACT task group, they have the option to add the new tasks to the same
 * group (if the parameters are the same) or to create a new one.  Also, if
 * more than one algorithm was used to compute the set of entries in the
 * dictionary, the user has the option to limit the computation to an individual
 * algorithm.
 */
public class SNIFACTDialog extends WindowUtil.PromptDialog
{
    protected static final String SNIF_ACT_TITLE =
        L10N.get("WCI.SNIFACTTitle", "Set SNIF-ACT parameters");

    public static final int NONE = 0;
    public static final int ENABLED = 1;
    public static final int DISABLED = 2;

    public static final int WIDTH = 200;

    /**
     * A list of the frame names in the relevant design, sorted alphabetically
     */
    protected java.util.List<Frame> sortedFrames;

    protected Spinner numRunsSpinner;
    protected Spinner kValueSpinner;
    protected Combo startFrameCombo;
    protected List targetFrameList;
    protected Button addToGroup = null;
    protected Combo algCombo;
    protected Button exportOnly;

    protected int addGroupMode;
    protected SNIFACTParameters defaultParameters;
    protected SNIFACTGroupParameters parameters;
    protected Set<ITermSimilarity> algorithmSet;
    protected ITermSimilarity[] algArray;

    public SNIFACTDialog(Shell parentWin,
                         java.util.List<Frame> frames,
                         SNIFACTParameters defaultParms,
                         Set<ITermSimilarity> algSet,
                         int mode)
    {
        super(parentWin,
              SNIF_ACT_TITLE,
              SWT.PRIMARY_MODAL,
              WIDTH,
              L10N.get("PM.TaskName", "Task Name") + ": ",
              "",
              defaultParms.taskName);

        sortedFrames = frames;
        defaultParameters = defaultParms;
        algorithmSet = algSet;
        addGroupMode = mode;
        algArray = new ITermSimilarity[algorithmSet.size() + 1];
    }

    @Override
    protected void addMoreFields()
    {
        super.addMoreFields();

        responseBox.setEditable(false);

        Label lbl = new Label(dialog, SWT.NONE);

        lbl.setText(L10N.get("PM.NumTrials", "Number of Trials") + ": ");

        GridData lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

        lbl.setLayoutData(lblLayout);

        numRunsSpinner = new Spinner(dialog, SWT.NONE);
        numRunsSpinner.setMinimum(1);
        numRunsSpinner.setMaximum(1000);
        numRunsSpinner.setSelection(defaultParameters.numRuns);

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        lblLayout.grabExcessHorizontalSpace = true;
        lblLayout.horizontalSpan = 3;

        numRunsSpinner.setLayoutData(lblLayout);

        lbl = new Label(dialog, SWT.NONE);

        lbl.setText(L10N.get("PM.KValue", "Eagerness to satisfy (smaller is more eager)") + ": ");

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        lbl.setLayoutData(lblLayout);

        kValueSpinner = new Spinner(dialog, SWT.NONE);
        kValueSpinner.setMinimum(1);
        kValueSpinner.setMaximum(999);
        kValueSpinner.setSelection(defaultParameters.kValue);
        kValueSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                if (addGroupMode == ENABLED) {
                    boolean equal = kValueSpinner.getSelection() ==
                                    defaultParameters.kValue;
                    addToGroup.setEnabled(equal);

                    if (! equal) {
                        addToGroup.setSelection(false);
                    }
                }
            }
        });

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        lblLayout.grabExcessHorizontalSpace = true;
        lblLayout.horizontalSpan = 3;

        kValueSpinner.setLayoutData(lblLayout);

        lbl = new Label(dialog, SWT.NONE);

        lbl.setText(L10N.get("PM.StartFrame", "Start Frame") + ": ");

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

        lbl.setLayoutData(lblLayout);

        startFrameCombo =
            new ComboWithEnableFix(dialog, SWT.DROP_DOWN | SWT.READ_ONLY);

        startFrameCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                if (addGroupMode == ENABLED) {
                    int selIndex = startFrameCombo.getSelectionIndex();
                    String frameName = startFrameCombo.getItem(selIndex);
                    String defaultName = defaultParameters.startFrame;
                    boolean equal = frameName.equals(defaultName);

                    addToGroup.setEnabled(equal);

                    if (! equal) {
                        addToGroup.setSelection(false);
                    }
                }
            }
        });

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        lblLayout.grabExcessHorizontalSpace = true;
        lblLayout.horizontalSpan = 3;

        startFrameCombo.setLayoutData(lblLayout);

        lbl = new Label(dialog, SWT.NONE);

        lbl.setText(L10N.get("PM.TargetFrames", "Target Frames") + ": ");

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

        lbl.setLayoutData(lblLayout);

        targetFrameList =
            new org.eclipse.swt.widgets.List(dialog,
                                             SWT.MULTI | SWT.V_SCROLL | SWT.BORDER);

        targetFrameList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                okButton.setEnabled(targetFrameList.getSelectionCount() > 0);

                if (addGroupMode == ENABLED) {
                    // Need to test whether the list of selected frame names
                    // is identical (except for order) to the list of frame
                    // names specified in defaultParameters.targetFrames, so
                    // check if they are the same size and then test whether
                    // any names in one list don't appear in the other
                    String[] targets = targetFrameList.getSelection();
                    java.util.List<String> defaultTargets =
                        defaultParameters.targetFrames;

                    boolean equal = targets.length == defaultTargets.size();

                    if (equal) {
                        for (int i = 0; i < targets.length; i++) {
                            String curDefault = defaultTargets.get(i);

                            if (! (curDefault.equals(targets[i]))) {
                                equal = false;
                            }
                        }
                    }

                    addToGroup.setEnabled(equal);

                    if (! equal) {
                        addToGroup.setSelection(false);
                    }
                }
            }
        });

        for (int i = 0; i < sortedFrames.size(); i++) {
            String name = sortedFrames.get(i).getName();
            String display = SWTStringUtil.insertEllipsis(name,
                                                          WIDTH,
                                                          StringUtil.NO_FRONT,
                                                          SWTStringUtil.DEFAULT_FONT);

            startFrameCombo.add(display);
            targetFrameList.add(display);

            if (name.equals(defaultParameters.startFrame)) {
                startFrameCombo.select(i);
            }

            if (defaultParameters.targetFrames.contains(name)) {
                targetFrameList.select(i);
            }
        }

        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

        lblLayout.grabExcessHorizontalSpace = true;
        lblLayout.horizontalSpan = 3;
        lblLayout.heightHint = 100;

        targetFrameList.setLayoutData(lblLayout);

        if (algorithmSet.size() > 1) {
            lbl = new Label(dialog, SWT.NONE);

            lbl.setText(L10N.get("PM.Algorithm", "Algorithm") + ": ");

            lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

            lbl.setLayoutData(lblLayout);

            algCombo = new ComboWithEnableFix(dialog,
                                                   SWT.DROP_DOWN | SWT.READ_ONLY);
            algCombo.add(L10N.get("PM.All", "All"));
            int i = 0;
            algArray[i++] = null;
            int selectIndex = 0;

            Iterator<ITermSimilarity> algs = algorithmSet.iterator();

            while (algs.hasNext()) {
                ITermSimilarity alg = algs.next();
                algArray[i] = alg;
                algCombo.add(DictionaryEditorUIModel.getAlgLabel(alg));

                if (alg.getClass().isInstance(defaultParameters.algorithm)) {
                    selectIndex = i;
                }

                i++;
            }

            algCombo.select(selectIndex);

            lblLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            lblLayout.grabExcessHorizontalSpace = true;
            lblLayout.horizontalSpan = 3;

            algCombo.setLayoutData(lblLayout);
        }

        numRunsSpinner.forceFocus();

        if (addGroupMode != NONE) {
            addToGroup = new Button(dialog, SWT.CHECK);
            addToGroup.setText(L10N.get("PM.AddToGroup", "Add scripts to existing task group"));

            addToGroup.setEnabled(true);
            addToGroup.setSelection(true);

            lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            lblLayout.grabExcessHorizontalSpace = true;
            lblLayout.horizontalSpan = 3;

            addToGroup.setLayoutData(lblLayout);
        }
        
        exportOnly = new Button(dialog, SWT.CHECK);
        exportOnly.setText(L10N.get("PM.ExportOnly", "Only export model, do not run it"));
        exportOnly.setEnabled(true);
        exportOnly.setSelection(false);
        lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        lblLayout.grabExcessHorizontalSpace = true;
        lblLayout.horizontalSpan = 3;
        exportOnly.setLayoutData(lblLayout);
    }

    @Override
    protected void onOK()
    {
        String task = responseBox.getText();
        int runs = numRunsSpinner.getSelection();
        int k = kValueSpinner.getSelection();
        String frameName =
            sortedFrames.get(startFrameCombo.getSelectionIndex()).getName();
        int[] targetInds = targetFrameList.getSelectionIndices();
        java.util.List<String> targets = new ArrayList<String>();
        for (int targetInd : targetInds) {
            targets.add(sortedFrames.get(targetInd).getName());
        }
        int algIndex = (algCombo == null) ? 0 : algCombo.getSelectionIndex();
        boolean add = (addToGroup == null) ? false
                                                : addToGroup.getSelection();

        parameters = new SNIFACTGroupParameters(task,
                                                     runs,
                                                     k,
                                                     frameName,
                                                     targets,
                                                     algArray[algIndex],
                                                     add);
        
        if (exportOnly.getSelection()) {
            SNIFACTPredictionAlgo.exportCTEModelFile = 
                    (new PersistenceView(parent)).selectFileDest("Exported CT-E Model", ".lisp");
        } else {
            SNIFACTPredictionAlgo.exportCTEModelFile = null;
        }            

        super.onOK();
    }

    public SNIFACTGroupParameters getParameters()
    {
        return parameters;
    }
}