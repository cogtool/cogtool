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
import edu.cmu.cs.hcii.cogtool.model.CachedGoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.IntegerEntry;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public class ResearchDialog extends WindowUtil.SimpleDialog
{
    private static final String BACK_BUTTON_HELP_TEXT =
        L10N.get("PREFDG.BKBUTHELP",
          "This is a place holder for some text describing the various " +
          "settings associated with the back button in CogTool-Explorer. " +
          "The pop-up distinguishes three possible behaviors: just stopping and " +
          "giving up when a search bottoms out; clicking on a distinguished " +
          "back button when we bottom out, following a transition from that " +
          "button, if any, and otherwise going back via history; or just going " +
          "back magically using the history without clicking anything. " +
          "The text for a back button can be set; multiple possible names for the " +
          "back button can be set by providing one per line in the relevant " +
          "text box.");
    
    private Interaction interaction;
    private Button allowResearchCmds;
    private Button useKeypad;
    private Button hcipaCheckbox;
    private Button systemWaitVisionOnlyCheckbox;
    private Button enableComputeScriptsCheckbox; 
    private Button generateThinksOnImportCheckbox;
    private Button enableTracingCheckbox;
    private Button useEMMACheckbox;
    private Combo actrDebugLevelCombo;
    private IntegerEntry actrTimeoutEntry;
    private DoubleEntry pmiGSizeEntry;
    private Button cteSuppressNoiseCheckbox;
    private Button cteSuppressNoninteractiveCheckbox;
    private ManagedText converterDirectoryEntry;
    private Button alternativeParametersCheckbox;
    private IntegerEntry visualAttentionEntry;
    private IntegerEntry motorInitiationEntry;
    private IntegerEntry peckFittsCoeffEntry;
    private IntegerEntry actrDATEntry;
    private Combo cteBackButtonSemantics;
    private ManagedText cteBackButtonEntry;
    private Button enableLoggingCheckbox;
    private ManagedText logDirectoryEntry;
    
    public ResearchDialog(Shell parentWin, Interaction interact)
    {
        super(parentWin,
              L10N.get("PREFDG.Research", "CogTool Research Commands"),
              SWT.APPLICATION_MODAL,
              SWT.DIALOG_TRIM);
        interaction = interact;
    }

    @Override
    public void buildDialog()
    {
        dialog.setLayout(new FormLayout());

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
                                        userResponse = null;
                                        dialog.close();
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

        allowResearchCmds = new Button(dialog, SWT.CHECK);
        allowResearchCmds.setText(L10N.get("PREFDG.Research",
                                               "Enable research commands"));
        allowResearchCmds.setSelection(CogToolPref.RESEARCH.getBoolean());
        allowResearchCmds.addListener(SWT.Selection,
                                           new Listener() {
                                              public void handleEvent(Event e) {
                                                  updateEnabled();
                                              }
                                           });

        useKeypad = new Button(dialog, SWT.CHECK);
        useKeypad.setText(L10N.get("FREFDG.UseKeypad",
                                        "Allow screen keyboard for text entry"));
        useKeypad.setSelection(CogToolPref.USE_KEYPAD.getBoolean());

        hcipaCheckbox = new Button(dialog, SWT.CHECK);
        hcipaCheckbox.setText(L10N.get("PREFDG.HCIPA",
                                            "HCIPA"));
        hcipaCheckbox.setSelection(CogToolPref.HCIPA.getBoolean());

        systemWaitVisionOnlyCheckbox = new Button(dialog, SWT.CHECK);
        systemWaitVisionOnlyCheckbox.setText(L10N.get("PREFDG.SYSWVO",
                                                           "Mobile phone timings"));
        systemWaitVisionOnlyCheckbox.setSelection(CogToolPref.SYSWVO.getBoolean());
        
        enableComputeScriptsCheckbox = new Button(dialog, SWT.CHECK);
        enableComputeScriptsCheckbox.setText(L10N.get("PREFDG.COMPSCR", 
                                                      "Compute scripts on XML import")); 
        enableComputeScriptsCheckbox.setSelection(CogToolPref.COMPSCR.getBoolean());
        
        generateThinksOnImportCheckbox = new Button(dialog, SWT.CHECK);
        generateThinksOnImportCheckbox.setText(L10N.get("PREFDG.GENSTEPS",
                                                          "Generate implied Thinks on Import from XML"));
        generateThinksOnImportCheckbox.setSelection(CogToolPref.GENERATE_THINKS_ON_IMPORT.getBoolean());
        
        enableTracingCheckbox = new Button(dialog, SWT.CHECK);
        enableTracingCheckbox.setText(L10N.get("PREFDG.TRACE",
                                               "Emit ACT-R Traces"));
        enableTracingCheckbox.setSelection(CogToolPref.IS_TRACING.getBoolean());
        
        enableLoggingCheckbox = new Button(dialog, SWT.CHECK);
        enableLoggingCheckbox.setText(L10N.get("PREFDG.ENABLELOG",
                                               "Emit detailed log file"));
        enableLoggingCheckbox.setSelection(CogToolPref.IS_LOGGING.getBoolean());
        
        Label logDirectoryLabel = new Label(dialog, SWT.NONE);
        logDirectoryLabel.setText(L10N.get("PREFDG.LogDirectory", "Log file Directory:"));
        logDirectoryEntry = new ManagedText(dialog, SWT.BORDER, Keypad.FULL_KEYPAD);
        String directory = CogToolPref.LOG_DIRECTORY.getString();
        if( directory != null){
            logDirectoryEntry.setText(directory);
        }
        Button logBrowseButton = new Button(dialog, SWT.PUSH);
        logBrowseButton.setText(L10N.get("PREFDG.ChooseLog", "Choose..."));
        logBrowseButton.addListener(SWT.Selection,
                                   new Listener() {
                                      public void handleEvent(Event e) {
                                         String dir = interaction.askUserForDirectory("Log Files","Choose the directory into which to write log files." );

                                         if (dir != null)
                                         {
                                             logDirectoryEntry.setText(dir);
                                         }
                                      }
                                   });
        
        useEMMACheckbox = new Button(dialog, SWT.CHECK);
        useEMMACheckbox.setText(L10N.get("PREFDG.EMMA",
                                         "Use EMMA to model vision"));
        useEMMACheckbox.setSelection(CogToolPref.USE_EMMA.getBoolean());
        
        Label actrDebugLevelLabel = new Label(dialog, SWT.NONE);
        actrDebugLevelLabel.setText(L10N.get("PREFDG.DEBUGLVL",
                                             "ACT-R debug level:"));
        
        actrDebugLevelCombo = new Combo(dialog, SWT.READ_ONLY);
        actrDebugLevelCombo.add("0");
        actrDebugLevelCombo.add("1");
        actrDebugLevelCombo.add("2");
        actrDebugLevelCombo.add("3");
        actrDebugLevelCombo.select(CogToolPref.ACTR_DEBUG_LEVEL.getInt());
        
        Label actrTimeoutLabel = new Label(dialog, SWT.NONE);
        actrTimeoutLabel.setText(L10N.get("PREFDG.ACTRTIMEOUT",
                                          "ACT-R timeout:"));
        actrTimeoutEntry = new IntegerEntry(dialog, SWT.BORDER);
        actrTimeoutEntry.setAllowNegative(false);
        actrTimeoutEntry.setValue(CogToolPref.ACTR_TIMEOUT.getInt());
        Label actrTimeoutUnitsLabel = new Label(dialog, SWT.NONE);
        actrTimeoutUnitsLabel.setText("msec");
        
        alternativeParametersCheckbox = new Button(dialog, SWT.CHECK);
        alternativeParametersCheckbox.setText(L10N.get("PREFDG.ALT_PARM",
                                               "Use alternative ACT-R parameters"));
        alternativeParametersCheckbox.setSelection(CogToolPref.ACTR_ALTERNATIVE_PARAMETERS.getBoolean());
        alternativeParametersCheckbox.addListener(SWT.Selection,
                                                      new Listener() {
                                                         public void handleEvent(Event e) {
                                                             updateEnabled();
                                                         }
                                                      });
        
        Label visualAttentionLabel = new Label(dialog, SWT.NONE);
        visualAttentionLabel.setText(L10N.get("PREFDG.VISATTN",
                                              "ACT-R Visual Attention:"));
        
        visualAttentionEntry = new IntegerEntry(dialog, SWT.BORDER);
        visualAttentionEntry.setAllowNegative(false);
        visualAttentionEntry.setValue(CogToolPref.VISUAL_ATTENTION.getInt());
        Label visualAttentionUnitsLabel = new Label(dialog, SWT.NONE);
        visualAttentionUnitsLabel.setText(String.format("msec (default %d)",
                                                        CogToolPref.VISUAL_ATTENTION.getIntDefault()));
        
        Label motorInitiationLabel = new Label(dialog, SWT.NONE);
        motorInitiationLabel.setText(L10N.get("PREFDG.MOTORINIT",
                                              "ACT-R Motor Initiation:"));
        motorInitiationEntry = new IntegerEntry(dialog, SWT.BORDER);
        motorInitiationEntry.setAllowNegative(false);
        motorInitiationEntry.setValue(CogToolPref.MOTOR_INITIATION.getInt());
        
        Label motorInitiationUnitsLabel = new Label(dialog, SWT.NONE);
        motorInitiationUnitsLabel.setText(String.format("msec (default %d)",
                                                        CogToolPref.MOTOR_INITIATION.getIntDefault()));
        Label peckFittsCoeffLabel = new Label(dialog, SWT.NONE);
        peckFittsCoeffLabel.setText(L10N.get("PREFDG.PECKFITTSCOEFF",
                                              "ACT-R peck Fitts coefficient:"));
        peckFittsCoeffEntry = new IntegerEntry(dialog, SWT.BORDER);
        peckFittsCoeffEntry.setAllowNegative(false);
        peckFittsCoeffEntry.setValue(CogToolPref.PECK_FITTS_COEFF.getInt());
        
        Label peckFittsCoeffUnitsLabel = new Label(dialog, SWT.NONE);
        peckFittsCoeffUnitsLabel.setText(String.format("msec (default %d)",
                                                        CogToolPref.PECK_FITTS_COEFF.getIntDefault()));
       
        Label actrDATLabel = new Label(dialog, SWT.NONE);
        actrDATLabel.setText(L10N.get("PREFDG.DAT", "ACT-R dat:"));
        actrDATEntry = new IntegerEntry(dialog, SWT.BORDER);
        actrDATEntry.setAllowNegative(false);
        actrDATEntry.setValue(CogToolPref.ACTR_DAT.getInt());
        Label actrDATUnitsLabel = new Label(dialog, SWT.NONE);
        actrDATUnitsLabel.setText(String.format("msec (default %d)",
                                                        CogToolPref.ACTR_DAT.getIntDefault()));
        
        cteSuppressNoiseCheckbox = new Button(dialog, SWT.CHECK);
        cteSuppressNoiseCheckbox.setText(L10N.get("PREFDG.CTE_SUPPRESS_NOISE",
                                                  "Suppress noise in CogTool Explorer"));
        cteSuppressNoiseCheckbox.setSelection(CogToolPref.CTE_SUPPRESS_NOISE.getBoolean());
        
        cteSuppressNoninteractiveCheckbox = new Button(dialog, SWT.CHECK);
        cteSuppressNoninteractiveCheckbox.setText(L10N.get("PREFDG.CTE_SUPPRESS_NONINTERACTIVE",
                                                           "Ignore non-interactive widgets with no display or auxilliary text in CogTool Explorer"));
        cteSuppressNoninteractiveCheckbox.setSelection(CogToolPref.CTE_SUPPRESS_NONINTERACTIVE.getBoolean());
        
        cteBackButtonSemantics = new Combo(dialog, SWT.READ_ONLY);
        cteBackButtonSemantics.add("Never go back");
        cteBackButtonSemantics.add("Use back button to go back");
        cteBackButtonSemantics.add("Go back implicitly");
        cteBackButtonSemantics.select(CogToolPref.CTE_BACK_BUTTON_SEMANTICS.getInt());
        Button cteBackButtonHelp = new Button(dialog, SWT.PUSH);
        cteBackButtonHelp.setText("?");
        cteBackButtonHelp.addListener(SWT.Selection,
                                      new Listener() {
                                     public void handleEvent(Event e) {
                                         WindowUtil.presentInformationDialog(dialog,
                                             L10N.get("PREFDG.CTE_BACK_BUTTON_HELP_TITLE",
                                                      "Back Button Help"),
                                             BACK_BUTTON_HELP_TEXT);                                                                                
                                     }
                                 });
        Label cteBackButtonEntryLabel = new Label(dialog, SWT.NONE);
        cteBackButtonEntryLabel.setText(L10N.get("PREFDG.CTEBACKLABEL", "Back button label:"));
        cteBackButtonEntry = new ManagedText(dialog, (SWT.BORDER | SWT.MULTI | SWT.LEFT), Keypad.FULL_KEYPAD);
        cteBackButtonEntry.setText(CogToolPref.CTE_DEFAULT_BACK_LABEL.getString());
        
        Label pmiGSizeLabel = new Label(dialog, SWT.NONE);
        pmiGSizeLabel.setText(L10N.get("PREFDG.PMIGSIZE",
                                       "PMI-G size:"));
        pmiGSizeEntry = new DoubleEntry(dialog, SWT.BORDER);
        pmiGSizeEntry.setAllowNegative(false);
        double pmiGSize = CogToolPref.PMI_G_SIZE.getDouble();
        if (pmiGSize != CachedGoogleSimilarity.PMI_G_SIZE_AUTOMATIC) {
            pmiGSizeEntry.setValue((int)pmiGSize);
        }
        
        Label converterDirectoryLabel = new Label(dialog, SWT.NONE);
        converterDirectoryLabel.setText(L10N.get("PREFDG.ConverterDirectory", "Converter Directory:"));
        converterDirectoryEntry = new ManagedText(dialog, SWT.BORDER, Keypad.FULL_KEYPAD);
        //Set the text entry to be equal to the current specified directory
        directory = CogToolPref.CONVERTER_DIRECTORY.getString();
        if( directory != null){
            converterDirectoryEntry.setText(directory);
        }
        Button convertBrowseButton = new Button(dialog, SWT.PUSH);
        convertBrowseButton.setText(L10N.get("PREFDG.ChooseConverter", "Choose..."));
        convertBrowseButton.addListener(SWT.Selection,
                                   new Listener() {
                                      public void handleEvent(Event e) {
                                         String dir = interaction.askUserForDirectory("Import Converter Files","Choose the directory that contains the converter files." );

                                         if(dir != null)
                                         {
                                             converterDirectoryEntry.setText(dir);
                                         }
                                      }
                                   });

        updateEnabled();

        FormData fd = new FormData();

        if (OSUtils.MACOSX) {
            fd.right = new FormAttachment(100, -18);
            fd.bottom = new FormAttachment(100, -10);
            okButton.setLayoutData(fd);

            fd = new FormData();
            fd.right = new FormAttachment(okButton, -8);
            fd.bottom = new FormAttachment(100, -10);
            cancelButton.setLayoutData(fd);
        }
        else {
            fd.left = new FormAttachment(50, -40);
            fd.bottom = new FormAttachment(100, -10);
            okButton.setLayoutData(fd);

            fd = new FormData();
            fd.left = new FormAttachment(okButton, 10);
            fd.bottom = new FormAttachment(100, -10);
            cancelButton.setLayoutData(fd);
        }
        
        fd = new FormData();
        fd.right = new FormAttachment(cancelButton, -108);
        fd.bottom = new FormAttachment(100, -10);
        restoreDefaultsButton.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(0, 20);
        fd.left = new FormAttachment(0, 20);
        allowResearchCmds.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(allowResearchCmds, 10);
        fd.left = new FormAttachment(0, 20);
        fd.right = new FormAttachment(100, -20);
        useKeypad.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(useKeypad, 10);
        fd.left = new FormAttachment(0, 20);
        hcipaCheckbox.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(hcipaCheckbox, 10);
        fd.left = new FormAttachment(0, 20);
        systemWaitVisionOnlyCheckbox.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(systemWaitVisionOnlyCheckbox, 15);
        fd.left = new FormAttachment(systemWaitVisionOnlyCheckbox, 0, SWT.LEFT);
        enableComputeScriptsCheckbox.setLayoutData(fd);
        
        fd = new FormData(); 
        fd.top = new FormAttachment(enableComputeScriptsCheckbox, 15);
        fd.left = new FormAttachment(enableComputeScriptsCheckbox, 0, SWT.LEFT);
        generateThinksOnImportCheckbox.setLayoutData(fd);

        fd = new FormData(); 
        fd.top = new FormAttachment(generateThinksOnImportCheckbox, 15);
        fd.left = new FormAttachment(generateThinksOnImportCheckbox, 0, SWT.LEFT);
        enableTracingCheckbox.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(enableTracingCheckbox, 15);
        fd.left = new FormAttachment(enableTracingCheckbox, 0, SWT.LEFT);
        enableLoggingCheckbox.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(enableLoggingCheckbox, 15);
        fd.left = new FormAttachment(enableLoggingCheckbox, 0, SWT.LEFT);
        logDirectoryLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(logDirectoryLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(logDirectoryLabel, 5, SWT.RIGHT);
        fd.right = new FormAttachment(logDirectoryLabel, 305, SWT.RIGHT);
        logDirectoryEntry.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(logDirectoryEntry, 0, SWT.CENTER);
        fd.left = new FormAttachment(logDirectoryEntry, 5, SWT.RIGHT);
        logBrowseButton.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(logDirectoryLabel, 15);
        fd.left = new FormAttachment(logDirectoryLabel, 0, SWT.LEFT);
        useEMMACheckbox.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(useEMMACheckbox, 15);
        fd.left = new FormAttachment(useEMMACheckbox, 0, SWT.LEFT);
        actrDebugLevelLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrDebugLevelLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(actrDebugLevelLabel, 5);
        actrDebugLevelCombo.setLayoutData(fd);
 
        fd = new FormData();
        fd.top = new FormAttachment(actrDebugLevelLabel, 18);
        fd.left = new FormAttachment(actrDebugLevelLabel, 0, SWT.LEFT);
        actrTimeoutLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrTimeoutLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(actrTimeoutLabel, 5);
        fd.right = new FormAttachment(actrTimeoutLabel, 80, SWT.RIGHT);
        actrTimeoutEntry.getOuter().setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(actrTimeoutLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(actrTimeoutEntry.getOuter(), 5);
        actrTimeoutUnitsLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrTimeoutLabel, 18);
        fd.left = new FormAttachment(actrTimeoutLabel, 0, SWT.LEFT);
        alternativeParametersCheckbox.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(alternativeParametersCheckbox, 8);
        fd.left = new FormAttachment(alternativeParametersCheckbox, 215, SWT.LEFT);
        fd.right = new FormAttachment(alternativeParametersCheckbox, 265, SWT.LEFT);
        visualAttentionEntry.getOuter().setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(visualAttentionEntry.getOuter(), 0, SWT.CENTER);
        fd.right = new FormAttachment(visualAttentionEntry.getOuter(), -5, SWT.LEFT);
        visualAttentionLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(visualAttentionEntry.getOuter(), 0, SWT.CENTER);
        fd.left = new FormAttachment(visualAttentionEntry.getOuter(), 5, SWT.RIGHT);
        visualAttentionUnitsLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(visualAttentionEntry.getOuter(), 4);
        fd.left = new FormAttachment(visualAttentionEntry.getOuter(), 0, SWT.LEFT);
        fd.right = new FormAttachment(visualAttentionEntry.getOuter(), 0, SWT.RIGHT);
        motorInitiationEntry.getOuter().setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(motorInitiationEntry.getOuter(), 0, SWT.CENTER);
        fd.right = new FormAttachment(motorInitiationEntry.getOuter(), -5, SWT.LEFT);
        motorInitiationLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(motorInitiationEntry.getOuter(), 0, SWT.CENTER);
        fd.left = new FormAttachment(motorInitiationEntry.getOuter(), 5, SWT.RIGHT);
        motorInitiationUnitsLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(motorInitiationEntry.getOuter(), 4);
        fd.left = new FormAttachment(motorInitiationEntry.getOuter(), 0, SWT.LEFT);
        fd.right = new FormAttachment(motorInitiationEntry.getOuter(), 0, SWT.RIGHT);
        peckFittsCoeffEntry.getOuter().setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(peckFittsCoeffEntry.getOuter(), 0, SWT.CENTER);
        fd.right = new FormAttachment(peckFittsCoeffEntry.getOuter(), -5, SWT.LEFT);
        peckFittsCoeffLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(peckFittsCoeffEntry.getOuter(), 0, SWT.CENTER);
        fd.left = new FormAttachment(peckFittsCoeffEntry.getOuter(), 5, SWT.RIGHT);
        peckFittsCoeffUnitsLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(peckFittsCoeffEntry.getOuter(), 4);
        fd.left = new FormAttachment(peckFittsCoeffEntry.getOuter(), 0, SWT.LEFT);
        fd.right = new FormAttachment(peckFittsCoeffEntry.getOuter(), 0, SWT.RIGHT);
        actrDATEntry.getOuter().setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrDATEntry.getOuter(), 0, SWT.CENTER);
        fd.right = new FormAttachment(actrDATEntry.getOuter(), -5, SWT.LEFT);
        actrDATLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrDATEntry.getOuter(), 0, SWT.CENTER);
        fd.left = new FormAttachment(actrDATEntry.getOuter(), 5, SWT.RIGHT);
        actrDATUnitsLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(actrDATEntry.getOuter(), 18);
        fd.left = new FormAttachment(alternativeParametersCheckbox, 0, SWT.LEFT);
        cteSuppressNoiseCheckbox.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(cteSuppressNoiseCheckbox, 18);
        fd.left = new FormAttachment(cteSuppressNoiseCheckbox, 0, SWT.LEFT);
        fd.right = new FormAttachment(100, -35);
        cteSuppressNoninteractiveCheckbox.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(cteSuppressNoninteractiveCheckbox, 18);
        fd.left = new FormAttachment(cteSuppressNoninteractiveCheckbox, 0, SWT.LEFT);
        cteBackButtonSemantics.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(cteBackButtonSemantics, 8);
        fd.left = new FormAttachment(cteBackButtonEntryLabel, 5);
        fd.right = new FormAttachment(cteBackButtonEntryLabel, 180, SWT.RIGHT);
        fd.bottom = new FormAttachment(cteBackButtonSemantics, 110);
        cteBackButtonEntry.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(cteBackButtonEntry, 0, SWT.CENTER);
        fd.left = new FormAttachment(cteBackButtonSemantics, 30, SWT.LEFT);
        cteBackButtonEntryLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(cteBackButtonSemantics, 0, SWT.CENTER);
        fd.left = new FormAttachment(cteBackButtonSemantics, 15, SWT.RIGHT);
        cteBackButtonHelp.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(cteBackButtonEntry, 24);
        fd.left = new FormAttachment(cteSuppressNoiseCheckbox, 0, SWT.LEFT);
        pmiGSizeLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.top = new FormAttachment(pmiGSizeLabel, 0, SWT.CENTER);
        fd.left = new FormAttachment(pmiGSizeLabel, 5);
        fd.right = new FormAttachment(pmiGSizeLabel, 140, SWT.RIGHT);
        pmiGSizeEntry.getOuter().setLayoutData(fd);
        
        fd = new FormData();
        fd.left = new FormAttachment(pmiGSizeLabel, 0, SWT.LEFT);
        fd.top = new FormAttachment(pmiGSizeLabel, 18);
        converterDirectoryLabel.setLayoutData(fd);
        
        fd = new FormData();
        fd.left = new FormAttachment(converterDirectoryLabel, 5, SWT.RIGHT);
        fd.right = new FormAttachment(converterDirectoryLabel, 305, SWT.RIGHT);
        fd.top = new FormAttachment(converterDirectoryLabel, 0, SWT.CENTER);
        converterDirectoryEntry.setLayoutData(fd);

        fd = new FormData();
        fd.top = new FormAttachment(converterDirectoryEntry, 0, SWT.CENTER);
        fd.left = new FormAttachment(converterDirectoryEntry, 5, SWT.RIGHT);
        fd.right = new FormAttachment(okButton, 0, SWT.RIGHT);
        fd.bottom = new FormAttachment(okButton, -30);
        convertBrowseButton.setLayoutData(fd);
    }

    protected void updateEnabled() {
        boolean resrch = allowResearchCmds.getSelection();
        boolean alt = alternativeParametersCheckbox.getSelection();
        useKeypad.setEnabled(resrch);
        hcipaCheckbox.setEnabled(resrch);
        systemWaitVisionOnlyCheckbox.setEnabled(resrch);
        enableComputeScriptsCheckbox.setEnabled(resrch);
        generateThinksOnImportCheckbox.setEnabled(resrch);
        enableTracingCheckbox.setEnabled(resrch);
        useEMMACheckbox.setEnabled(resrch);
        actrDebugLevelCombo.setEnabled(resrch);
        actrTimeoutEntry.getOuter().setEnabled(resrch);
        alternativeParametersCheckbox.setEnabled(resrch);
        visualAttentionEntry.getOuter().setEnabled(resrch && alt);
        motorInitiationEntry.getOuter().setEnabled(resrch && alt);
        peckFittsCoeffEntry.getOuter().setEnabled(resrch && alt);
        actrDATEntry.getOuter().setEnabled(resrch && alt);
        pmiGSizeEntry.getOuter().setEnabled(resrch);
        cteSuppressNoiseCheckbox.setEnabled(resrch);
        cteSuppressNoninteractiveCheckbox.setEnabled(resrch);
        cteBackButtonSemantics.setEnabled(resrch);
        cteBackButtonEntry.setEnabled(resrch);
    }

    // returns true iff something has actually been changed
    protected boolean updateValues()
    {
    	Set<CogToolPref> changed = EnumSet.noneOf(CogToolPref.class);
        if (CogToolPref.RESEARCH.setBoolean(allowResearchCmds.getSelection())) {
            changed.add(CogToolPref.RESEARCH);
        }
        if (CogToolPref.USE_KEYPAD.setBoolean(useKeypad.getSelection())) {
            changed.add(CogToolPref.USE_KEYPAD);
        }
        if (CogToolPref.HCIPA.setBoolean(hcipaCheckbox.getSelection())) {
            changed.add(CogToolPref.HCIPA);
        }
        if (CogToolPref.SYSWVO.setBoolean(systemWaitVisionOnlyCheckbox.getSelection())) {
            changed.add(CogToolPref.SYSWVO);
        }
        if (CogToolPref.COMPSCR.setBoolean(enableComputeScriptsCheckbox.getSelection())) {
            changed.add(CogToolPref.COMPSCR);
        }
        if (CogToolPref.GENERATE_THINKS_ON_IMPORT.setBoolean(generateThinksOnImportCheckbox.getSelection())) {
            changed.add(CogToolPref.GENERATE_THINKS_ON_IMPORT);
        }
        if (CogToolPref.IS_TRACING.setBoolean(enableTracingCheckbox.getSelection())) {
            changed.add(CogToolPref.IS_TRACING);
        }
        if (CogToolPref.USE_EMMA.setBoolean(useEMMACheckbox.getSelection())) {
            changed.add(CogToolPref.USE_EMMA);
        }
        if (CogToolPref.ACTR_DEBUG_LEVEL.setInt(actrDebugLevelCombo.getSelectionIndex())) {
            changed.add(CogToolPref.ACTR_DEBUG_LEVEL);
        }
        if (CogToolPref.ACTR_TIMEOUT.setInt(actrTimeoutEntry.getValue())) {
            changed.add(CogToolPref.ACTR_TIMEOUT);
        }
        if (CogToolPref.ACTR_ALTERNATIVE_PARAMETERS.setBoolean(alternativeParametersCheckbox.getSelection())) {
            changed.add(CogToolPref.ACTR_ALTERNATIVE_PARAMETERS);
        }
        if (CogToolPref.VISUAL_ATTENTION.setInt(visualAttentionEntry.getValue())) {
            changed.add(CogToolPref.VISUAL_ATTENTION);
        }
        if (CogToolPref.MOTOR_INITIATION.setInt(motorInitiationEntry.getValue())) {
            changed.add(CogToolPref.MOTOR_INITIATION);
        }
        if (CogToolPref.PECK_FITTS_COEFF.setInt(peckFittsCoeffEntry.getValue())) {
            changed.add(CogToolPref.PECK_FITTS_COEFF);
        }
        if (CogToolPref.ACTR_DAT.setInt(actrDATEntry.getValue())) {
            changed.add(CogToolPref.ACTR_DAT);
        }
        if (CogToolPref.CTE_SUPPRESS_NOISE.setBoolean(cteSuppressNoiseCheckbox.getSelection())) {
            changed.add(CogToolPref.CTE_SUPPRESS_NOISE);
        }
        if (CogToolPref.CTE_SUPPRESS_NONINTERACTIVE.setBoolean(cteSuppressNoninteractiveCheckbox.getSelection())) {
            changed.add(CogToolPref.CTE_SUPPRESS_NONINTERACTIVE);
        }
        if (CogToolPref.PMI_G_SIZE.setDouble(pmiGSizeEntry.getDoubleValue())) {
            changed.add(CogToolPref.PMI_G_SIZE);
        }
        if (CogToolPref.CONVERTER_DIRECTORY.setString(converterDirectoryEntry.getText())) {
            changed.add(CogToolPref.CONVERTER_DIRECTORY);
        }
        if (CogToolPref.CTE_BACK_BUTTON_SEMANTICS.setInt(cteBackButtonSemantics.getSelectionIndex())) {
            changed.add(CogToolPref.CTE_BACK_BUTTON_SEMANTICS);
        }
        if (CogToolPref.CTE_DEFAULT_BACK_LABEL.setString(cteBackButtonEntry.getText())) {
            changed.add(CogToolPref.CTE_DEFAULT_BACK_LABEL);
        }
        if (CogToolPref.IS_LOGGING.setBoolean(enableTracingCheckbox.getSelection())) {
            changed.add(CogToolPref.IS_LOGGING);
        }
        if (CogToolPref.LOG_DIRECTORY.setString(logDirectoryEntry.getText())) {
            changed.add(CogToolPref.LOG_DIRECTORY);
        }
        if (changed.isEmpty()) {
            return false;
        }
        CogToolPref.flush();
        CogToolPref.ALERTER.raiseAlert(new CogToolPref.PreferencesChange(changed));
        return true;
    }

    private void restoreDefaults() {
        useKeypad.setSelection(CogToolPref.USE_KEYPAD.getBooleanDefault());
        hcipaCheckbox.setSelection(CogToolPref.HCIPA.getBooleanDefault());
        systemWaitVisionOnlyCheckbox.setSelection(CogToolPref.SYSWVO.getBooleanDefault());
        enableComputeScriptsCheckbox.setSelection(CogToolPref.COMPSCR.getBooleanDefault());
        generateThinksOnImportCheckbox.setSelection(CogToolPref.GENERATE_THINKS_ON_IMPORT.getBooleanDefault());
        enableTracingCheckbox.setSelection(CogToolPref.IS_TRACING.getBooleanDefault());
        useEMMACheckbox.setSelection(CogToolPref.USE_EMMA.getBooleanDefault());
        actrDebugLevelCombo.select(CogToolPref.ACTR_DEBUG_LEVEL.getIntDefault());
        actrTimeoutEntry.setValue(CogToolPref.ACTR_TIMEOUT.getIntDefault());
        alternativeParametersCheckbox.setSelection(CogToolPref.ACTR_ALTERNATIVE_PARAMETERS.getBooleanDefault());
        visualAttentionEntry.setValue(CogToolPref.VISUAL_ATTENTION.getIntDefault());
        motorInitiationEntry.setValue(CogToolPref.MOTOR_INITIATION.getIntDefault());
        peckFittsCoeffEntry.setValue(CogToolPref.PECK_FITTS_COEFF.getIntDefault());
        actrDATEntry.setValue(CogToolPref.ACTR_DAT.getIntDefault());
        cteSuppressNoiseCheckbox.setSelection(CogToolPref.CTE_SUPPRESS_NOISE.getBooleanDefault());
        cteSuppressNoninteractiveCheckbox.setSelection(CogToolPref.CTE_SUPPRESS_NONINTERACTIVE.getBooleanDefault());
        cteBackButtonSemantics.select(CogToolPref.CTE_BACK_BUTTON_SEMANTICS.getIntDefault());
        cteBackButtonEntry.setText(CogToolPref.CTE_DEFAULT_BACK_LABEL.getStringDefault());
        pmiGSizeEntry.setValue((int)CogToolPref.PMI_G_SIZE.getDoubleDefault());
        String dir = CogToolPref.CONVERTER_DIRECTORY.getStringDefault();
        if (dir == null) {
            dir = "";
        }
        converterDirectoryEntry.setText(dir);
    }

}

