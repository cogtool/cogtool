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

import java.util.EnumSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.util.IntegerEntry;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.StatusBar;

public class PreferencesDialog extends WindowUtil.SimpleDialog
{
    private Interaction interaction;
    private Boolean researchNewState = null;
    private Button researchButton;
    private IntegerEntry minFrameWidthEntry;
    private IntegerEntry framesPerRowEntry;
    private Button klmResultRangeCheckbox;
    private Combo displayDigitsCombo; 
    private boolean researchChanged = false;
    public static final int DEFAULT_MIN_FRAME_WIDTH =
    // large enough to hold both the bottom widgets (voice, keyboard)
    2 * (DesignUtil.DEVICE_WIDTH + DesignUtil.DEVICE_SPACING) + 1;
    public static final int MAX_MIN_FRAME_WIDTH = 500;

    public PreferencesDialog(Shell parentWin, Interaction interact)
    {
        super(parentWin,
              L10N.get("PREFDG.Preferences", "CogTool Preferences"),
              SWT.APPLICATION_MODAL,
              SWT.DIALOG_TRIM);
        interaction = interact;
    }

    @Override
    public void buildDialog()
    {
        FormLayout lo = new FormLayout();
        lo.marginWidth = 28;
        dialog.setLayout(lo);
        dialog.setMinimumSize(550, 180);

        Button okButton = new Button(dialog, SWT.PUSH);
        okButton.setText(L10N.get("PREFDG.OK", "OK"));
        dialog.setDefaultButton(okButton);
        okButton.addListener(SWT.Selection,
                             new Listener() {
                                 public void handleEvent(Event e) {
                                     // TODO: updateValues() can throw IllegalStateException!
                                     userResponse = (updateValues() ?
                                                         Boolean.TRUE :
                                                         null);
                                     dialog.close();
                                 }
                             });

        Button cancelButton = new Button(dialog, SWT.PUSH);
        cancelButton.setText(L10N.get("PREFDG.Cancel", "Cancel"));
        cancelButton.addListener(SWT.Selection,
                                 new Listener() {
                                    public void handleEvent(Event e) {
                                        userResponse = (researchChanged ?
                                                             Boolean.TRUE :
                                                             null);
                                        dialog.close();
                                    }
                                 });

        researchButton = new Button(dialog, SWT.PUSH);
        setResearchButtonText();
        researchButton.addListener(SWT.Selection,
                                 new Listener() {
                                    public void handleEvent(Event e) {
                                        if (researchNewState != null) {
                                            CogToolPref.RESEARCH.setBoolean(researchNewState);
                                            researchNewState = null;
                                        }
                                        researchChanged |=
                                            ((new ResearchDialog(parent, interaction)).open() != null);
                                        if (researchChanged) {
                                            setResearchButtonText();
                                        }
                                    }
                                 });
        
        Button restoreDefaultsButton = new Button(dialog, SWT.PUSH);
        restoreDefaultsButton.setText(L10N.get("PREFDG.RestoreDefaultsButton",
                                               "Reset to Default Values"));
        restoreDefaultsButton.addListener(SWT.Selection,
                                          new Listener() {
                                             public void handleEvent(Event e) {
                                                 restoreDefaults();
                                             }
                                          });

        Label minFrameWidthLabel = new Label(dialog, SWT.NONE);
        minFrameWidthLabel.setText(L10N.get("PREFDG.MINFRAMEWID",
                                            "Minimum frame width:"));
        minFrameWidthEntry = new IntegerEntry(dialog, SWT.BORDER);
        minFrameWidthEntry.setAllowNegative(false);
        minFrameWidthEntry.setValue(CogToolPref.MIN_FRAME_WIDTH.getInt());
        Label minFrameWidthUnitsLabel = new Label(dialog, SWT.NONE);
        minFrameWidthUnitsLabel.setText(L10N.get("PREFDG.PIXELS", "pixels"));

        Label framesPerRowLabel = new Label(dialog, SWT.NONE);
        framesPerRowLabel.setText(L10N.get("PREFDG.FRAMESPERROW",
                                 "Frames per row (on import):"));
        framesPerRowEntry = new IntegerEntry(dialog, SWT.BORDER);
        framesPerRowEntry.setAllowNegative(false);
        framesPerRowEntry.setValue(CogToolPref.FRAMES_PER_ROW.getInt());
        klmResultRangeCheckbox = new Button(dialog, SWT.CHECK);
        klmResultRangeCheckbox.setText(L10N.get("PREFDG.KLMRESULTRANGE",
                                            "Display range of predicted skilled execution time instead of a single value"));
        klmResultRangeCheckbox.setSelection(CogToolPref.KLM_RESULT_RANGE.getBoolean());

        Label displayDigitsLabel = new Label(dialog, SWT.NONE);
        displayDigitsLabel.setText(L10N.get("PREFDG.DISPDIG",
                "Number of decimal places displayed in results:"));
        displayDigitsCombo = new Combo(dialog, SWT.READ_ONLY);
        displayDigitsCombo.add("0");
        displayDigitsCombo.add("1");
        displayDigitsCombo.add("2");
        displayDigitsCombo.add("3");
        displayDigitsCombo.select(CogToolPref.DISPLAY_DIGITS.getInt());
        
        FormData fd = new FormData();

        Button rightMostButton;
        if (OSUtils.MACOSX) {
            rightMostButton = okButton;
            
            fd.right = new FormAttachment(100, 0);
            fd.bottom = new FormAttachment(100, -10);
            fd.top = new FormAttachment(klmResultRangeCheckbox, 22);
            okButton.setLayoutData(fd);

            fd = new FormData();
            fd.right = new FormAttachment(okButton, -8);
            fd.bottom = new FormAttachment(100, -10);
            cancelButton.setLayoutData(fd);

            fd = new FormData();
            fd.right = new FormAttachment(cancelButton, -28);
            fd.bottom = new FormAttachment(100, -10);
            researchButton.setLayoutData(fd);
        }
        else {
            rightMostButton = cancelButton;
            
            fd.left = new FormAttachment(50, -20);
            fd.bottom = new FormAttachment(100, -10);
            fd.top = new FormAttachment(klmResultRangeCheckbox, 22);
            okButton.setLayoutData(fd);

            fd = new FormData();
            fd.left = new FormAttachment(okButton, 20);
            fd.bottom = new FormAttachment(100, -10);
            cancelButton.setLayoutData(fd);

            fd = new FormData();
            fd.right = new FormAttachment(okButton, -28);
            fd.bottom = new FormAttachment(100, -10);
            researchButton.setLayoutData(fd);
        }
        
        fd = new FormData();
        fd.right = new FormAttachment(researchButton, -28);
        fd.bottom = new FormAttachment(100, -10);
        restoreDefaultsButton.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(0, 20);
        fd.right = new FormAttachment(100, -120);
        minFrameWidthUnitsLabel.setLayoutData(fd);
        fd = new FormData();
        fd.top = new FormAttachment(minFrameWidthUnitsLabel, 0, SWT.CENTER);
        fd.right = new FormAttachment(minFrameWidthUnitsLabel, -5);
        fd.left = new FormAttachment(minFrameWidthUnitsLabel, -60, SWT.LEFT);
        minFrameWidthEntry.getOuter().setLayoutData(fd);
        fd = new FormData();
        fd.top = new FormAttachment(minFrameWidthUnitsLabel, 0, SWT.CENTER);
        fd.right = new FormAttachment(minFrameWidthEntry.getOuter(), -5);
        minFrameWidthLabel.setLayoutData(fd);

        fd = new FormData();
        fd.right = new FormAttachment(minFrameWidthEntry.getOuter(), 0, SWT.RIGHT);
        fd.left = new FormAttachment(minFrameWidthEntry.getOuter(), 0, SWT.LEFT);
        fd.top = new FormAttachment(minFrameWidthEntry.getOuter(), 0);
        framesPerRowEntry.getOuter().setLayoutData(fd);
        fd = new FormData();
        fd.top = new FormAttachment(framesPerRowEntry, 0, SWT.CENTER);
        fd.right = new FormAttachment(framesPerRowEntry.getOuter(), -5);
        framesPerRowLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(framesPerRowEntry.getOuter(), 5);
        fd.right = new FormAttachment(framesPerRowLabel, 0, SWT.RIGHT);
        displayDigitsLabel.setLayoutData(fd);
        fd = new FormData();
        fd.top = new FormAttachment(displayDigitsLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(displayDigitsLabel, 5);
        displayDigitsCombo.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(displayDigitsLabel, 10);
        fd.left = new FormAttachment(displayDigitsLabel, 25, SWT.LEFT);
        klmResultRangeCheckbox.setLayoutData(fd);
    }

    private void setResearchButtonText() {
        boolean state = CogToolPref.RESEARCH.getBoolean();
        if (researchNewState != null) {
            state = researchNewState;
        }
        researchButton.setText(String.format(L10N.get("PREFDG.ResesarchButton", "Research Commands (%s) ..."),
                                               (state ? L10N.get("PREFDG.On", "On") :
                                                        L10N.get("FREFDG.Off", "Off"))));
    }
    
    // returns true iff something has actually been changed
    private boolean updateValues()
    {
        Set<CogToolPref> changed = EnumSet.noneOf(CogToolPref.class);
        if (CogToolPref.MIN_FRAME_WIDTH.setInt(minFrameWidthEntry.getValue())) {
            changed.add(CogToolPref.MIN_FRAME_WIDTH);
        }
        if (CogToolPref.FRAMES_PER_ROW.setInt(framesPerRowEntry.getValue())) {
            changed.add(CogToolPref.FRAMES_PER_ROW);
        }
        if (CogToolPref.DISPLAY_DIGITS.setInt(displayDigitsCombo.getSelectionIndex())) {
            changed.add(CogToolPref.DISPLAY_DIGITS);
        }
        if (CogToolPref.KLM_RESULT_RANGE.setBoolean(klmResultRangeCheckbox.getSelection())) {
            changed.add(CogToolPref.KLM_RESULT_RANGE);
        }
        if (researchNewState != null && CogToolPref.RESEARCH.setBoolean(researchNewState)) {
            changed.add(CogToolPref.RESEARCH);
        }
        if (changed.isEmpty() && !researchChanged) {
            return false;
        }
        CogToolPref.flush();
        StatusBar.updateStatusBars();
        if (!changed.isEmpty()) {
            CogToolPref.ALERTER.raiseAlert(new CogToolPref.PreferencesChange(changed));
        }
        return true;
    }
    
    private void restoreDefaults() {
        minFrameWidthEntry.setValue(CogToolPref.MIN_FRAME_WIDTH.getIntDefault());
        framesPerRowEntry.setValue(CogToolPref.FRAMES_PER_ROW.getIntDefault());
        klmResultRangeCheckbox.setSelection(CogToolPref.KLM_RESULT_RANGE.getBooleanDefault());
        displayDigitsCombo.select(CogToolPref.DISPLAY_DIGITS.getIntDefault());
        if (CogToolPref.RESEARCH.getBoolean() != CogToolPref.RESEARCH.getBooleanDefault()) {
            researchNewState = CogToolPref.RESEARCH.getBooleanDefault();
            researchChanged = true;
        }
        setResearchButtonText();
    }

}

