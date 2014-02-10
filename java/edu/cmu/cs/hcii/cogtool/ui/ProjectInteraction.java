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

import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.CachedGoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.GoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.GroupNature;
import edu.cmu.cs.hcii.cogtool.model.ISitedTermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.LSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.GensimLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTGroupParameters;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTParameters;
import edu.cmu.cs.hcii.cogtool.model.TaskParent;
import edu.cmu.cs.hcii.cogtool.model.URLCrawlEntry;
import edu.cmu.cs.hcii.cogtool.uimodel.DictionaryEditorUIModel;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil.PromptDialog;
import edu.cmu.cs.hcii.cogtool.view.AboutView;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * Implementation of the user interaction that is specific to the
 * Project Editor interface.
 *
 * @author mlh
 */
public class ProjectInteraction extends DefaultInteraction
{
    protected static final String needSelectionMsg =
        L10N.get("PM.NeedSelection",
                 "You must select a design or task first; click on a design's column header or a task's row.");

    protected static final String needTaskApplicationMsg =
        L10N.get("PM.NeedTaskApplication",
                 "You must select a cell that has a valid script and/or result.");

    protected static final String tooManySelectedTasks =
        L10N.get("PM.TooManySelectedTasks",
                 "There are too many tasks selected for this operation.");

    protected static final String cannotMoveTaskEarlier =
        L10N.get("PM.CannotMoveTaskEarlier",
                 "Cannot move the first child task earlier");

    protected static final String cannotMoveTaskLater =
        L10N.get("PM.CannotMoveTaskLater",
                 "Cannot move the last child task later");

    protected static final String confirmDeleteDesignMsg =
        L10N.get("PM.ConfirmDeleteDesign",
                 "Please confirm the deletion of the following Design:");

    protected static final String confirmOverwriteScriptMsg =
        L10N.get("PM.ConfirmDeleteScript",
                 "The script in this cell will be deleted. Would you like to continue?");

    protected static final String designNotUniqueMsg =
        L10N.get("PM.NotUniqueDesignName",
                 "Design names must be unique; would you like to try again?");

    protected static final String designEmptyNameMsg =
        L10N.get("PM.EmptyDesignName",
                 "Design names must not be empty; would you like to try again?");

    protected static final String noDeviceTypesMsg =
        L10N.get("PM.NoDeviceTypes",
                 "At least one device type must be chosen; would you like to try again?");

    protected static final String confirmDeleteTasksMsg =
        L10N.get("PM.ConfirmDeleteTask",
                 "Please confirm the deletion of the following Task(s):");

    protected static final String confirmDeleteTasksChildrenMsg =
        L10N.get("PM.ConfirmDeleteTask",
                 "Please confirm the deletion of the following Task(s) and Task Group(s), plus any group members:");

    protected static final String taskNotUniqueMsg =
        L10N.get("PM.NotUniqueTaskName",
                 "Task names must be unique within the containing group; would you like to try again?");

    protected static final String taskEmptyNameMsg =
        L10N.get("PM.EmptyTaskName",
                 "Task names must not be empty; would you like to try again?");

    protected static final String pasteTaskReuseTitle =
        L10N.get("PM.PasteTaskReuseTitle", "Paste Design");

    protected static final String pasteTaskReuseMsg =
        L10N.get("PM.PasteTaskReuseMsg",
                 "Should paste re-use the following existing task\n('Yes' to re-use, 'No' to create a new task)?");

    protected static final String errorTitle =
        L10N.get("PM.ErrorTitle", "Project Error");

    protected static final String confirmTitle =
        L10N.get("PM.ConfirmTitle", "Confirm Deletion");

    protected static final String recomputeTitle =
        L10N.get("PC.Recompute", "Recompute");

    protected static final String scriptRegenerationRequired =
        L10N.get("PC.NeedRegeneration",
                 "Some demonstrated scripts require regeneration before recomputation; regenerate and then compute?");

    /**
     * Dialog box specific to soliciting information for a task
     * (name, nature, position).
     *
     * @author mlh
     */
    public static class NewTaskDialog extends WindowUtil.PromptDialog
    {
        protected ProjectInteraction.TaskRequestData requestData;

        protected Button meanOption;
        protected Button sumOption;
        protected Button maxOption;
        protected Button minOption;

        /**
         * Initialize dialog box modal to the given parent window
         * and with initial values specified by the given task request data.
         *
         * @param parentWin window the dialog box should be modal to
         * @param data the initial values for the tsk information
         * @author mlh
         */
        public NewTaskDialog(Shell parentWin, ProjectInteraction.TaskRequestData data)
        {
            super(parentWin,
                  L10N.get("PM.NewTaskTitle", "New Task"),
                  SWT.PRIMARY_MODAL,
                  L10N.get("CT.TaskNameLabel", "Task name:"),
                  L10N.get("PM.AskNewTaskName",
                           "What is the new task's name?"),
                  data.taskName);

            requestData = data;
        }

        /**
         * Region for the radio buttons representing the nature and
         * positioning choices.
         *
         * @return region for the radio buttons representing the nature and
         *         positioning choices
         */
        protected Composite addRadioSet()
        {
            Composite radioSet = new Composite(dialog, SWT.NONE);

            radioSet.setLayout(new FillLayout());

            GridData radioSetLayout = new GridData();

            radioSetLayout.horizontalSpan = 2;

            radioSet.setLayoutData(radioSetLayout);

            return radioSet;
        }

        /**
         * Add radio button to given region.
         *
         * @param radioSet the given region to hold the radio button
         * @param optionText the label for the radio button
         * @param isSelected whether the box is initially selected
         * @param isEnabled whether the user can actually choose this option
         * @param optionListener how to react to a click on the radio button
         */
        protected Button addOption(Composite radioSet,
                                   String optionText,
                                   boolean isSelected,
                                   boolean isEnabled,
                                   SelectionListener optionListener)
        {
            Button option = new Button(radioSet, SWT.RADIO);

            option.setSelection(isSelected);
            option.setEnabled(isEnabled);

            option.setText(optionText);
            option.addSelectionListener(optionListener);

            return option;
        }

        /**
         * Add the given text as a label to the dialog box.
         *
         * @param lblText the text to add as a label
         * @return the SWT label object
         */
        protected Label addLabel(String lblText)
        {
            // Label in left-most column, right-justified
            Label lbl = new Label(dialog, SWT.NONE);

            lbl.setText(lblText);

            GridData lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

            lbl.setLayoutData(lblLayout);

            return lbl;
        }

        /**
         * Add an empty cell as padding in the layout grid.
         */
        protected void addEmptyCell()
        {
            Label emptyCell = new Label(dialog, SWT.NONE);

            GridData emptyCellLayout = new GridData();

            emptyCellLayout.grabExcessHorizontalSpace = true;

            emptyCell.setLayoutData(emptyCellLayout);
        }

        /**
         * Populate the dialog box.
         */
        @Override
        protected void addMoreFields()
        {
            super.addMoreFields();

            // If asking for before/after placement, add radio button set
            if ((requestData.flags & ProjectInteraction.TaskRequestData.ASK_IF_BEFORE) != 0)
            {
                addLabel(L10N.get("PM.PlaceNewTask", "Place:"));

                Composite radioSet = addRadioSet();

                addOption(radioSet,
                          L10N.get("RADIO.After", "after"),
                          requestData.addAfter,
                          true,
                          new SelectionAdapter() {
                              @Override
                              public void widgetSelected(SelectionEvent evt)
                              {
                                  requestData.addAfter = true;
                              }
                          });

                addOption(radioSet,
                          L10N.get("RADIO.Before", "before"),
                          ! requestData.addAfter,
                          true,
                          new SelectionAdapter() {
                              @Override
                              public void widgetSelected(SelectionEvent evt)
                              {
                                  requestData.addAfter = false;
                              }
                          });

                addEmptyCell();
            }

            // If asking if new task is a group, add radio button set
            if ((requestData.flags & ProjectInteraction.TaskRequestData.ASK_IF_GROUP)
                    == ProjectInteraction.TaskRequestData.ASK_IF_GROUP)
            {
                addLabel(L10N.get("PM.IsNewTaskGroup", "As a group:"));

                Composite radioSet = addRadioSet();

                addOption(radioSet,
                          L10N.get("RADIO.SingleTask", "a single task"),
                          requestData.addAsTask,
                          true,
                          new SelectionAdapter() {
                              @Override
                              public void widgetSelected(SelectionEvent evt)
                              {
                                  requestData.addAsTask = true;
                                  meanOption.setEnabled(false);
                                  sumOption.setEnabled(false);
                                  maxOption.setEnabled(false);
                                  minOption.setEnabled(false);
                              }
                          });

                addOption(radioSet,
                          L10N.get("RADIO.TaskGroup",
                                   "groups other tasks"),
                          ! requestData.addAsTask,
                          true,
                          new SelectionAdapter() {
                              @Override
                              public void widgetSelected(SelectionEvent evt)
                              {
                                  requestData.addAsTask = false;
                                  meanOption.setEnabled(true);
                                  sumOption.setEnabled(true);
                                  maxOption.setEnabled(true);
                                  minOption.setEnabled(true);
                              }
                          });

                addEmptyCell();
            }

            // Add nature button set
            if ((requestData.flags & ProjectInteraction.TaskRequestData.ASK_NATURE) != 0) {
                addLabel(L10N.get("PM.NewTaskNature", "Nature:"));

                Composite radioSet = addRadioSet();

                boolean isEnabled =
                    (! requestData.addAsTask) ||
                    ((requestData.flags & ProjectInteraction.TaskRequestData.ASK_IF_GROUP)
                            != ProjectInteraction.TaskRequestData.ASK_IF_GROUP);

                meanOption =
                    addOption(radioSet,
                              L10N.get("RADIO.MEAN", "MEAN"),
                              requestData.nature == GroupNature.MEAN,
                              isEnabled,
                              new SelectionAdapter() {
                                  @Override
                                  public void widgetSelected(SelectionEvent evt)
                                  {
                                      requestData.nature = GroupNature.MEAN;
                                  }
                              });

                sumOption =
                    addOption(radioSet,
                              L10N.get("RADIO.SUM", "SUM"),
                              requestData.nature == GroupNature.SUM,
                              isEnabled,
                              new SelectionAdapter() {
                                  @Override
                                  public void widgetSelected(SelectionEvent evt)
                                  {
                                      requestData.nature = GroupNature.SUM;
                                  }
                              });

                maxOption =
                    addOption(radioSet,
                              L10N.get("RADIO.MAX", "MAX"),
                              requestData.nature == GroupNature.MAX,
                              isEnabled,
                              new SelectionAdapter() {
                                  @Override
                                  public void widgetSelected(SelectionEvent evt)
                                  {
                                      requestData.nature = GroupNature.MAX;
                                  }
                              });

                minOption =
                    addOption(radioSet,
                              L10N.get("RADIO.MIN", "MIN"),
                              requestData.nature == GroupNature.MIN,
                              isEnabled,
                              new SelectionAdapter() {
                                  @Override
                                  public void widgetSelected(SelectionEvent evt)
                                  {
                                      requestData.nature = GroupNature.MIN;
                                  }
                              });

                addEmptyCell();
            }
        } // addMoreFields
    }

    public static class GenerateDictEntriesDialog extends PromptDialog
    {
        protected Combo algCombo;
        protected String algString;
        protected boolean computeAll;
        protected Combo spaceCombo;
        protected String spaceString;
        protected Text urlText;
        protected String urlString;
        protected Button regenerate;
        protected Button recompute;
        protected boolean hasDict;
        protected ITermSimilarity defaultAlg;

        public GenerateDictEntriesDialog(Shell parentWin,
                                         String requestQuestion,
                                         ITermSimilarity alg,
                                         String defaultSite,
                                         String defaultSpace,
                                         String defaultURL,
                                         boolean hasDictionary)
        {
            super(parentWin,
                  L10N.get("PM.GenerateParms", "Generation Details"),
                  SWT.PRIMARY_MODAL,
                  L10N.get("PM.Site", "Limiting Site") + ": ",
                  requestQuestion,
                  defaultSite);

            spaceString = defaultSpace;
            urlString = defaultURL;

            disableOK = false;
            hasDict = hasDictionary;
            defaultAlg = alg;
        }

        /**
         * What to do when the OK button is selected by the user.
         *
         * @author mlh
         */
        @Override
        protected void onOK()
        {
            int algIndex = algCombo.getSelectionIndex();
            algString = algCombo.getItem(algIndex);
            spaceString = spaceCombo.getText();
            urlString = urlText.getText();
            computeAll = (! hasDict || recompute.getSelection());
            super.onOK();
        }

        public ITermSimilarity getAlgorithm()
        {
            return DictionaryEditorUIModel.computeAlgorithm(algString, urlString, spaceString, promptResponse);
        }

        public ProjectInteraction.GenerateEntriesData getData()
        {
            return new ProjectInteraction.GenerateEntriesData(getAlgorithm(), computeAll);
        }

        /**
         * Populate the dialog box.
         */
        @Override
        protected void addMoreFields()
        {
            Label lbl = new Label(dialog, SWT.NONE);

            lbl.setText(L10N.get("PC.GenerateEntries",
                                 "Parameters for dictionary entry generation"));

            GridData lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
            lblLayout.grabExcessHorizontalSpace = true;
            lblLayout.horizontalAlignment = SWT.FILL;
            lblLayout.horizontalSpan = 4;
            lbl.setLayoutData(lblLayout);

            super.addMoreFields();

            responseBox.setEnabled(defaultAlg instanceof GoogleSimilarity ||
                                        defaultAlg instanceof CachedGoogleSimilarity);

            lbl = new Label(dialog, SWT.NONE);

            lbl.setText(L10N.get("PM.Algorithm", "Algorithm") + ": ");

            lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

            lbl.setLayoutData(lblLayout);

            SelectionListener listener = new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    Combo c = (Combo) evt.widget;

                    int index = c.getSelectionIndex();

                    responseBox.setEnabled((index == DictionaryEditorUIModel.GOOGLE_WORD_INDEX) ||
                                           (index == DictionaryEditorUIModel.GOOGLE_PHRASE_INDEX));
                    spaceCombo.setEnabled((index == DictionaryEditorUIModel.LSA_INDEX) ||
                                          (index == DictionaryEditorUIModel.GENSIM_LSA_INDEX));
                    urlText.setEnabled((index == DictionaryEditorUIModel.LSA_INDEX) ||
                                       (index == DictionaryEditorUIModel.GENSIM_LSA_INDEX));
                   
                    if (index == DictionaryEditorUIModel.GENSIM_LSA_INDEX){
                        spaceCombo.setItems(GensimLSASimilarity.KNOWN_SPACES);
                        spaceCombo.setText(GensimLSASimilarity.DEFAULT_SPACE);
                        urlText.setText(GensimLSASimilarity.DEFAULT_LSA_URL);
                        responseBox.setText("");
                    }
                    else {
                        spaceCombo.setItems(LSASimilarity.KNOWN_SPACES);
                        spaceCombo.setText(LSASimilarity.DEFAULT_SPACE);
                        urlText.setText(LSASimilarity.DEFAULT_LSA_URL);
                        responseBox.setText("");
                    }
                }
            };
            algCombo =
                DictionaryEditorUIModel.createAlgCombo(dialog,
                                                       defaultAlg,
                                                       listener);

            GridData itemLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            itemLayout.grabExcessHorizontalSpace = true;
            itemLayout.horizontalSpan = 3;

            algCombo.setLayoutData(itemLayout);

            lbl = new Label(dialog, SWT.NONE);
            lbl.setText(L10N.get("PM.LSASpace", "LSA Space") + ": ");
            lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
            lbl.setLayoutData(lblLayout);
            
            SelectionListener spaceListener = new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent evt)
                {
                    Combo c = (Combo) evt.widget;
                    int algIndex = algCombo.getSelectionIndex();
                    String alg = algCombo.getItem(algIndex);
                    int index = c.getSelectionIndex();
                    
                    if (algIndex == DictionaryEditorUIModel.GENSIM_LSA_INDEX){
                        if (index == 1){
                            urlText.setText(GensimLSASimilarity.GENSIM_STRIPPED_LSA_URL);
                        }
                        else if (index == 2){
                            urlText.setText(GensimLSASimilarity.GENSIM_SECTIONS_LSA_URL);
                        }
                        else if (index == 3){
                            urlText.setText(GensimLSASimilarity.GENSIM_PARAGRAPHS_LSA_URL);
                        }
                        else if (index == 4){
                            urlText.setText(GensimLSASimilarity.GENSIM_SIMPLE_LSA_URL);
                        }
                        else {
                            urlText.setText(GensimLSASimilarity.DEFAULT_LSA_URL);
                        }
                    }
                }
            };

            spaceCombo =
                new ComboWithEnableFix(dialog, SWT.DROP_DOWN | SWT.BORDER);
            spaceCombo.addSelectionListener(spaceListener);
            //spaceCombo.setItems(LSASimilarity.KNOWN_SPACES);
            int algIndex = algCombo.getSelectionIndex();
            if (algIndex == DictionaryEditorUIModel.GENSIM_LSA_INDEX){
                spaceCombo.setItems(GensimLSASimilarity.KNOWN_SPACES);
            }
            else {
                spaceCombo.setItems(LSASimilarity.KNOWN_SPACES);
            }
            spaceCombo.setText(spaceString);

            itemLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            itemLayout.grabExcessHorizontalSpace = true;
            itemLayout.horizontalSpan = 3;

            spaceCombo.setLayoutData(itemLayout);

            lbl = new Label(dialog, SWT.NONE);
            lbl.setText(L10N.get("PM.LSA_URL", "LSA URL") + ": ");
            lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
            lbl.setLayoutData(lblLayout);

            urlText =
                new TextWithEnableFix(dialog, SWT.SINGLE | SWT.BORDER);
            urlText.setText(urlString);

            itemLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            itemLayout.grabExcessHorizontalSpace = true;
            itemLayout.horizontalSpan = 3;

            urlText.setLayoutData(itemLayout);

            if (hasDict) {
                Group useSelectionGroup = new Group(dialog, SWT.SHADOW_NONE);
                useSelectionGroup.setLayout(new GridLayout());

                regenerate = new Button(useSelectionGroup, SWT.RADIO);
                regenerate.setText(L10N.get("PM.Regenerate",
                                                 "Recompute only entries affected by changes to the model"));
                regenerate.setSelection(true);

                recompute = new Button(useSelectionGroup, SWT.RADIO);
                recompute.setText(L10N.get("PM.ComputeAll",
                                                "Recompute similarity for all entries in the dictionary"));

                GridData groupLayout =
                    new GridData(GridData.HORIZONTAL_ALIGN_FILL);

                groupLayout.grabExcessHorizontalSpace = true;
                groupLayout.horizontalSpan = 4;
                useSelectionGroup.setLayoutData(groupLayout);

                recompute.setLayoutData(new GridData());
                regenerate.setLayoutData(new GridData());
            }
        }
    }


    /**
     * The return value of <code>open</code> when the user selects "Create".
     */
    public static final String CREATE = new String("CREATE");

    /**
     * The return value of <code>open</code> when the user selects "Open".
     */
    public static final String OPEN = new String("OPEN");

    /**
     * Initialize interaction with the given window that pop-up dialog
     * boxes are modal to.
     *
     * @param v target window for dialog box modality
     * @author mlh
     */
    public ProjectInteraction(View v)
    {
        super(v);
    }

    /**
     * Complain that no design or task has been selected.
     *
     * @author mlh
     */

    public void protestNoSelection()
    {
        reportProblem(errorTitle, needSelectionMsg);
    }

    /**
     * Complain that the operation applies only to a single task.
     */

    public void protestTooManySelectedTasks()
    {
        reportProblem(errorTitle, tooManySelectedTasks);
    }

    /**
     * Complain that no task application exists.
     *
     * @author mlh
     */

    public void protestNoTaskApplication()
    {
        reportProblem(errorTitle, needTaskApplicationMsg);
    }

    /**
     * Complain that the given task cannot be earlier relative to its siblings
     */

    public void protestCannotMoveEarlier()
    {
        reportProblem(errorTitle, cannotMoveTaskEarlier);
    }

    /**
     * Complain that the given task cannot be later relative to its siblings
     */

    public void protestCannotMoveLater()
    {
        reportProblem(errorTitle, cannotMoveTaskLater);
    }

    /**
     * Request new information (name, device type set) about a new or
     * existing dictionary.
     */

    public ProjectInteraction.GenerateEntriesData requestGenerateDictionaryParms(String request,
                                                              ITermSimilarity defaultAlg,
                                                              boolean hasDict)
    {
        String defaultSite = "";
        String defaultSpace = "";
        String defaultURL = "";

        if (defaultAlg instanceof ISitedTermSimilarity) {
            defaultSite =
                ((ISitedTermSimilarity) defaultAlg).getContextSite();
        }
        else if (defaultAlg instanceof LSASimilarity) {
            LSASimilarity algLSA = (LSASimilarity) defaultAlg;

            defaultSpace = algLSA.getSpace();
            defaultURL = algLSA.getURL();
        }
        else if (defaultAlg instanceof GensimLSASimilarity) {
            GensimLSASimilarity algLSA = (GensimLSASimilarity) defaultAlg;

            defaultSpace = algLSA.getSpace();
            defaultURL = algLSA.getURL();
        }

        GenerateDictEntriesDialog dictDialog =
            new GenerateDictEntriesDialog(window,
                                          request,
                                          defaultAlg,
                                          defaultSite,
                                          defaultSpace,
                                          defaultURL,
                                          hasDict);

        ProjectInteraction.GenerateEntriesData data = null;
        boolean notDone = true;

        while (notDone) {
            Object response = dictDialog.open();

            if ((response != null) &&
                response.equals(WindowUtil.PromptDialog.OK))
            {
                data = dictDialog.getData();
                notDone = false;
            }
            else {
                notDone = false;
            }
        }

        return data;
    }

    /**
     * Request a new design name.  The given string provides the
     * initial value for the dialog box.
     *
     * @param oldDesignName the initial value for the dialog box
     * @return the new name if not canceled; <code>null</code> otherwise
     * @author mlh
     */

    public String requestDesignRename(String oldDesignName)
    {
        WindowUtil.PromptDialog askName =
            new WindowUtil.PromptDialog
                                (window,
                                 L10N.get("PM.RenameDesignTitle",
                                          "Rename Design"),
                                 SWT.PRIMARY_MODAL,
                                 L10N.get("CT.DesignNameLabel",
                                          "Design name:"),
                                 L10N.get("PM.AskNewDesignName",
                                          "What is the design's new name?"),
                                 oldDesignName);

        Object response = askName.open();

        return
            (response != null) && response.equals(WindowUtil.PromptDialog.OK)
                    ? askName.getPromptResponse()
                    : null;
    }

    /**
     * Complain that the specified design is not unique;
     * allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */

    public boolean protestNotUniqueDesignName()
    {
        return reportAndRetry(errorTitle, designNotUniqueMsg);
    }

    /**
     * Complain that the design name is empty; allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */

    public boolean protestEmptyDesignName()
    {
        return reportAndRetry(errorTitle, designEmptyNameMsg);
    }

    /**
     * Complain that the specified device set is empty;
     * allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */

    public boolean protestNoDeviceTypes()
    {
        return reportAndRetry(errorTitle, noDeviceTypesMsg);
    }

    /**
     * Complain that the attempt to recompute was prevented by scripts
     * that require regeneration.  Ask whether to regenerate then compute
     * or to cancel.  Returns <code>true</code> if the user indicated
     * to regenerate then compute.
     *
     * @author mlh
     */

    public boolean reportRegenerationRequired()
    {
        return SWT.OK == WindowUtil.presentMessageDialog(window,
                                                         recomputeTitle,
                                                         scriptRegenerationRequired,
                                                         SWT.OK | SWT.CANCEL,
                                                         SWT.ICON_WARNING,
                                                         SWT.PRIMARY_MODAL);
    }

    /**
     * Ask the user to confirm that the given design may be deleted.
     *
     * @param design the design that will be deleted
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */

    public boolean confirmDeleteDesign(Design design)
    {
        String msg = confirmDeleteDesignMsg + "\n    " + design.getName();

        return (SWT.OK == WindowUtil.presentConfirmDialog(window,
                                                          confirmTitle,
                                                          msg));
    }

    /**
     * Request new information (name, group nature) about a new or
     * existing task.  The given data initializes the values for
     * the interaction.
     *
     * @param requestData the initial values for the dialog box
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */

    public boolean requestNewTaskName(ProjectInteraction.TaskRequestData requestData)
    {
        NewTaskDialog askName = new NewTaskDialog(window, requestData);

        Object response = askName.open();

        if ((response != null) && response.equals(WindowUtil.PromptDialog.OK))
        {
            requestData.taskName = askName.getPromptResponse();

            return true;
        }

        return false;
    }

    /**
     * Ask the user to confirm that the given tasks may be deleted.
     *
     * @param selectedTasks the tasks that will be deleted
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */

    public boolean confirmDeleteTasks(AUndertaking[] selectedTasks)
    {
        String msg = confirmDeleteTasksMsg;

        // Specialize the message depending on whether any task group
        // is in the selected set to be deleted.
        for (int i = 0; i < selectedTasks.length; ++i)
        {
            if (selectedTasks[i].isTaskGroup()) {
                msg = confirmDeleteTasksChildrenMsg;
                break;
            }
        }

        return (SWT.OK == WindowUtil.presentConfirmItemsDialog(window,
                                                               confirmTitle,
                                                               msg,
                                                               selectedTasks));
    }

    /**
     * Complain that the specified task name is not unique;
     * allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */

    public boolean protestNotUniqueTaskName()
    {
        return reportAndRetry(errorTitle, taskNotUniqueMsg);
    }

    /**
     * Complain that the specified task name is empty; allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */

    public boolean protestEmptyTaskName()
    {
        return reportAndRetry(errorTitle, taskEmptyNameMsg);
    }

    /**
     * Ask whether the user wants to use an existing corresponding task
     * (i.e., one with the same name as the given task in the given taskParent)
     * when pasting a design between projects.
     */

    public Boolean askToReuseTaskOnPaste(AUndertaking task,
                                         TaskParent taskParent)
    {
        String msg = pasteTaskReuseMsg + "\n    " + task.getName();

        if (WindowUtil.presentYesNoQuestionDialog(window,
                                                  pasteTaskReuseTitle,
                                                  msg) == SWT.YES)
        {
            return Boolean.TRUE;
        }

        return Boolean.FALSE;
    }

    /**
     * Opening question to the user: Create-new vs. open existing project?
     */
    public static class StartUpDialog extends AboutView
    {
        // Available for alignment
        protected Combo openRecent = null;
        protected Button createButton;
        protected Button openButton;
        protected ShellListener modalFaker = null;

        public StartUpDialog(Shell parentWin)
        {
            super(parentWin,
                  L10N.get("CGTL.StartUp", "CogTool Start-up"),
                  OSUtils.MACOSX ? SWT.MODELESS : SWT.PRIMARY_MODAL);
        }

        /**
         * Construct the contents of the dialog window.
         * <p>
         * First, the question/instructions, if specified.<br>
         *
         * @author          mlh
         */

        @Override
        protected void addButtons()
        {
            Label msgCell = new Label(dialog, SWT.NONE);
            Font fnt = msgCell.getFont();
            msgCell.setFont(FontUtils.getAdjustedFont(fnt, 14, SWT.BOLD));
            msgCell.setText(L10N.get("CGTL.CreateOrOpen",
                                     "Create a new project or open an existing project?"));

            GridData msgCellLayout = new GridData();
            msgCellLayout.horizontalSpan = 4;
            msgCellLayout.grabExcessHorizontalSpace = true;
            msgCellLayout.verticalIndent = 25;
            msgCell.setLayoutData(msgCellLayout);

            Label emptyCell = new Label(dialog, SWT.NONE);
            GridData emptyCellLayout = new GridData();
            emptyCellLayout.grabExcessHorizontalSpace = true;
            emptyCell.setLayoutData(emptyCellLayout);

            if (CogToolPref.hasRecent()) {
                openRecent = new Combo(dialog, SWT.DROP_DOWN | SWT.READ_ONLY);
                openRecent.add(L10N.get("B.RECENT", "Open Recent"));
                for (String pathName : CogToolPref.getRecent()) {
                    if (! MenuFactory.UNSET_FILE.equals(pathName)) {
                        openRecent.add(pathName);
                    }
                }
                openRecent.select(0);
            }
            openButton = new Button(dialog, SWT.PUSH);
            createButton = new Button(dialog, SWT.PUSH);

            if (buttonFont != null) {
                createButton.setFont(buttonFont);
                openButton.setFont(buttonFont);
            }

            openButton.setText(L10N.get("B.OPEN", "Open..."));
            createButton.setText(L10N.get("B.CREATE", "Create"));

            dialog.setDefaultButton(createButton);

            GridData recentLayout = null;
            GridData cancelLayout = null;
            GridData okLayout = null;
            if (OSUtils.MACOSX) {
                if (openRecent != null) {
                    recentLayout = new GridData();
                    recentLayout.widthHint = 130;
                }
                cancelLayout = new GridData();
                okLayout = new GridData();
                // See Apple HIGs:
                // http://developer.apple.com/documentation/UserExperience/Conceptual/OSXHIGuidelines/XHIGControls/chapter_18_section_2.html#//apple_ref/doc/uid/TP30000359-TPXREF186
                cancelLayout.widthHint = 82;
                okLayout.widthHint = 82;
            }
            else {
                if (openRecent != null) {
                    recentLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
                }
                okLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
                cancelLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
            }

            okLayout.verticalIndent = 7;
            cancelLayout.verticalIndent = okLayout.verticalIndent;
            if (recentLayout != null) {
                recentLayout.verticalIndent = 4;
            }
            if (openRecent != null) {
                openRecent.setLayoutData(recentLayout);
            }
            openButton.setLayoutData(cancelLayout);
            createButton.setLayoutData(okLayout);

            if (openRecent != null) {
                openRecent.addSelectionListener(new SelectionAdapter()
                {
                    @Override
                    public void widgetSelected(SelectionEvent e)
                    {
                        int i = openRecent.getSelectionIndex();
                        if (i > 0) {
                            userResponse = openRecent.getItem(i);
                            dialog.close();
                        }
                    }
                });
            }

            // Dismiss the dialog box when OK button selected, with success
            createButton.addSelectionListener(new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = ProjectInteraction.CREATE;
                    dialog.close();
                }
            });

            // Dismiss the dialog box when Cancel button selected, with failure
            openButton.addSelectionListener(new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = ProjectInteraction.OPEN;
                    dialog.close();
                }
            });

            // SWT in its infinite wisdom disables the Quit menu item whenever
            // a modal dialog is up on Macintosh. We really want to be able to
            // quit when this guy is up, though, so this dialog is actually
            // modeless, but we fake making it modal by refusing to let its
            // parent become active.
            //
            // Sigh. The following used to work fine. But as of the update
            // to SWT in October 2007 it no longer does -- for thoroughly
            // mysterious reasons, now whenever you do this forceActive() it
            // causes a further activate event on the parent to be queued. The
            // end result is essentially an infinite recursion, but through
            // the event posting mechanism, not through this method so we
            // can't even get a decent handle on it (that is, the following
            // code returns fine; it's just that in executing it the event
            // processing code arranges that it will be called again Real Soon.
            // Allowing the parent window to become active isn't too bad as
            // it still can't come to the front, and even if manipulated bad
            // things don't actually seem to happen. So just comment this
            // stuff out for now. I'm going to file a bug report with the SWT
            // people and see if maybe it can be fixed in the future, though.
//            if (OSUtils.MACOSX) {
//                this.modalFaker = new ShellAdapter() {
//                    public void shellActivated(ShellEvent evt)
//                    {
//                        if (! StartUpDialog.this.dialog.isDisposed()) {
//                            StartUpDialog.this.dialog.forceActive();
//                        }
//                    }
//                };
//
//                this.parent.addShellListener(this.modalFaker);
//            }
        }

        public void removeModalFaker()
        {
            if (OSUtils.MACOSX && (modalFaker != null)) {
                parent.removeShellListener(modalFaker);
                modalFaker = null;
            }
        }

    }

    /**
     * Data needed when creating/editing a task or task group.
     * The flags control whether creating/editing a task or a task group,
     * and whether to ask for how to place the new instance.
     */
    public static class TaskRequestData
    {
        // Flags that may be OR'ed together
        public static final int ASK_NOTHING = 0;
        public static final int ASK_NATURE = 1;
        public static final int ASK_IF_GROUP = 3;   // subsumes ASK_NATURE
        public static final int ASK_IF_BEFORE = 4;

        public int flags;

        public String taskName;
        public GroupNature nature;
        public boolean addAfter = true;
        public boolean addAsTask = true;
    }

    public static class GenerateEntriesData
    {
        public ITermSimilarity algorithm;
        public boolean computeAll;

        public GenerateEntriesData(ITermSimilarity alg, boolean recompute)
        {
            algorithm = alg;
            computeAll = recompute;
        }
    }

    /**
     * Data returned by requesting a web crawl import.
     */
    public static interface IWebCrawlImport
    {
        /**
         * Value returned from getMaxPages() and getDefaultDepth()
         * when the system default should be used.
         */
        public static final int USE_SYSTEM_DEFAULT = -1;

        /**
         * Returns the name of the design into which to import web pages as
         * frames.  If null returned, a new design should be created.
         */
        public String getDesignName();

        /**
         * Returns the maximum number of pages that should be crawled; if
         * USE_SYSTEM_DEFAULT, then the system's default count should be used.
         */
        public int getMaxPages();

        /**
         * Returns the default depth to which initially specified URLs
         * that do not set their own depth should be crawled; if
         * USE_SYSTEM_DEFAULT, then the system's default depth should be used.
         */
        public int getDefaultDepth();

        /**
         * Returns a list of URLs to crawl.  If not null, the returned
         * List will consist of WebCrawler.URLCrawlEntry instances, each
         * of which contains a URL string and a depth integer.
         *
         */
        public List<URLCrawlEntry> getURLsToCrawl();
        /**
         * Returns whether to prune the web crawl when a URL is encountered
         * that the Design into which the web pages are imported as Frames
         * contains a Frame with the same name as the URL (keeping the
         * Frame as it currently is).  The other choice is to replace the Frame
         * with the new page as crawled and continue crawling its links
         * if the depth specifier allows.
         *
         * Please note that, if pruning the web crawl, it only applies
         * to Frames that exist in the Design prior to the web crawl.  Thus,
         * this option only has an effect if resuming the web crawl for an
         * existing Design.
         */
        public boolean pruneSameURLs();

        /**
         * Returns whether or not to capture web page images
         */
        public boolean captureImages();

        /**
         * Returns the desired width of the browser page.
         */
        public int getBrowserWidth();

        /**
         * Returns the desired height of the browser page.
         */
        public int getBrowserHeight();
    }

    /**
     * Request whether to start the application by creating a new project
     * or opening an existing project.
     *
     * @return CREATE if a new project is desired, OPEN if
     *         the user wishes to open an existing project,
     *         <code>null</code> if the user cancels.
     * @author mlh
     */

    public String createNewOrOpenExisting()
    {
        StartUpDialog dialog = null;
        try {
            dialog = new StartUpDialog(window);
            return (String) dialog.open();
        }
        finally {
            if (OSUtils.MACOSX) {
                dialog.removeModalFaker();
            }
        }
    }


    public ProjectInteraction.IWebCrawlImport requestWebCrawlParms(Project project,
                                                int defaultMaxToCrawl)
    {
        WebCrawlImportDialog dialog =
            new WebCrawlImportDialog(window, project, defaultMaxToCrawl);

        Object notCanceled = dialog.open();

        if (notCanceled != null) {
            return dialog;
        }

        return null;
    }


    public SNIFACTGroupParameters requestSNIFACTParameters(boolean hasScript,
                                                           List<Frame> sortedFrames,
                                                           SNIFACTParameters parms,
                                                           Set<ITermSimilarity> algSet,
                                                           int addGroupMode)
    {
        if (hasScript) {
            String msg = confirmOverwriteScriptMsg;

            if (SWT.OK != WindowUtil.presentConfirmDialog(window,
                                                          confirmTitle,
                                                          msg))
            {
                return null;
            }
        }

        SNIFACTDialog dialog =
            new SNIFACTDialog(window, sortedFrames, parms, algSet, addGroupMode);

        Object notCanceled = dialog.open();

        if (notCanceled != null) {
            return dialog.getParameters();
        }

        return null;
    }
    
}
