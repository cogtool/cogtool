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

import java.util.Arrays;
import java.util.EventObject;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.GensimLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.EntryChange;
import edu.cmu.cs.hcii.cogtool.model.ISitedTermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.LSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.DictionaryEditorUIModel;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.view.DictionaryEditorView;
import edu.cmu.cs.hcii.cogtool.view.View;

public class DictionaryEditorUI extends DefaultUI
{
    /**
     * Parameters for setting either the goal or search string for an
     * existing dictionary entry
     */
    public static class SetStringParms
    {
        public String string;
        public int rowIndex;

        public SetStringParms(String s, int index)
        {
            string = s;
            rowIndex = index;
        }
    }

    public static class AddStringParms
    {
        public String newString;
        public boolean isGoal;

        public AddStringParms(String s, boolean goal)
        {
            super();
            newString = s;
            isGoal = goal;
        }
    }

    public static class SetAlgorithmParms
    {
        public ITermSimilarity algorithm;
        public int[] rowIndices;

        public SetAlgorithmParms(ITermSimilarity alg, int[] indices)
        {
            algorithm = alg;
            rowIndices = indices;
        }
    }

    public static class SetSimilarityParms
    {
        public double similarity;
        public int rowIndex;

        public SetSimilarityParms(double simil,
                                  int index)
        {
            similarity = simil;
            rowIndex = index;
        }
    }

    public static class ComputeSimilarityParms
    {
        public String goalString;
        public String searchString;
        public ITermSimilarity algorithm;
        public int rowIndex;

        public ComputeSimilarityParms(String goal,
                                      String search,
                                      ITermSimilarity alg,
                                      int index)
        {
            goalString = goal;
            searchString = search;
            algorithm = alg;
            rowIndex = index;
        }
    }

    protected ISimilarityDictionary dictionary;
    protected Design design;

    protected DictionaryEditorView view;
    protected TableEditor editor;

    protected DictionaryEditorUIModel uiModel;

    protected DelayedRepaint delayedRepainting;

    protected Interaction interaction;

    protected int modifiedRow = -1;

    protected static final String DICT_PREFIX =
        L10N.get("WT.DictPrefix", "Dictionary");
    
    protected Combo spaceCombo;

    protected SelectionListener algListener = new SelectionAdapter()
    {
        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            Combo c = (Combo) evt.widget;

            TableItem row = (TableItem) c.getData();
            int index = c.getSelectionIndex();
            String algString = c.getItem(index);
            boolean isLSA = (index == DictionaryEditorUIModel.LSA_INDEX);
            boolean isGENSIM = (index == DictionaryEditorUIModel.GENSIM_LSA_INDEX);
            
            view.setURLEnabled((index == DictionaryEditorUIModel.GOOGLE_WORD_INDEX)
                                || (index == DictionaryEditorUIModel.GOOGLE_PHRASE_INDEX)
                                || isLSA);
            view.setURLEnabled(isLSA || isGENSIM);
            
            
            //Where should these get updated versus just using what is already there??
            //It appears that the URL is just used from the previous rather than being set here
            if (isGENSIM) {
                //view.setItems(GensimLSASimilarity.KNOWN_SPACES);
                view.setSpace(GensimLSASimilarity.DEFAULT_SPACE);
                view.setURL(GensimLSASimilarity.GENSIM_SIMPLE_LSA_URL);
            }
            else { //LSA
                //view.setItems(LSASimilarity.KNOWN_SPACES);
                view.setSpace(LSASimilarity.DEFAULT_SPACE);
                view.setURL(LSASimilarity.DEFAULT_LSA_URL);
            }

            Table dictTable = view.getDictTable();
            modifiedRow = dictTable.indexOf(row);
            dictTable.setSelection(modifiedRow);

            ITermSimilarity alg = getAlgorithm(algString);

            DictionaryEditorUI.SetAlgorithmParms parms =
                new DictionaryEditorUI.SetAlgorithmParms(alg, dictTable.getSelectionIndices());

            performAction(DictionaryEditorLID.SetAlgorithm, parms);

            int manualIndex = DictionaryEditorUIModel.MANUAL_INDEX;

            if ((index == manualIndex) && (dictTable.getSelectionCount() == 1))
            {
                initiateEdit(row, DictionaryEditorUIModel.SIMIL_COL);
            }
        }
    };

    public DictionaryEditorUI(ISimilarityDictionary dict,
                              Design d,
                              PendingDictEntry pendingEntry,
                              Project project,
                              UndoManager undoMgr)
    {
        super(project,
              "",
              buildLeadItems(project),
              undoMgr);

        dictionary = dict;
        design = d;

        view = new DictionaryEditorView(lIDMap,
                                             this,
                                             menuData,
                                             getWindowLocation());

        updateTitle();

        final Table dictTable = view.getDictTable();

        dictTable.addMouseListener(new MouseAdapter()
        {
            @Override
            public void mouseDoubleClick(MouseEvent evt)
            {
                TableItem row = dictTable.getItem(new Point(evt.x, evt.y));

                if (row != null) {
                    int column = evt.x / DictionaryEditorView.COL_WIDTH;

                    if (column <= DictionaryEditorUIModel.SIMIL_COL) {
                        initiateEdit(row, column);
                    }
                }
            }
        });

        dictTable.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent evt)
            {
                updateView();
                setViewEnabledState(ListenerIdentifierMap.NORMAL);
            }
        });

        dictTable.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent evt)
            {
                if ((evt.keyCode == SWT.DEL) ||
                    (evt.keyCode == SWT.BS))
                {
                    int[] indices = dictTable.getSelectionIndices();
                    Arrays.sort(indices);
                    performAction(DictionaryEditorLID.Delete,
                                  indices);
                }
            }
        });

        editor = new TableEditor(dictTable);
        editor.grabHorizontal = true;

        interaction = new DictionaryEditorInteraction(view);

        uiModel = new DictionaryEditorUIModel(dictionary,
                                                   pendingEntry,
                                                   view.getDictTable(),
                                                   algListener,
                                                   this.project);

        delayedRepainting =
            new DelayedRepaint() {
                @Override
                protected void performRepaint()
                {
                    updateView();
                }

                @Override
                public void doWork()
                {
                    super.doWork();

                    undoMgrViewHandler.resetView(undoManager);

                    // Update the enabled items selection state.
                    setViewEnabledState(ListenerIdentifierMap.NORMAL);

                    if (modifiedRow >= 0) {
                        uiModel.updateRow(modifiedRow);
                        modifiedRow = -1;
                    }

                    view.getDictTable().update();
                }
            };

        CogTool.repaintPhase.addDelayedWork(delayedRepainting);

        AlertHandler handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                EntryChange change =
                    (EntryChange) alert;

                DictEntry entry = dictionary.getEntry(change.rowIndex);

                if (entry.algorithm instanceof ISitedTermSimilarity) {
                    String newSite =
                        ((ISitedTermSimilarity) entry.algorithm).getContextSite();

                    if (newSite != null) {
                        view.setURL(newSite);
                    }
                }
            }
        };

        dictionary.addHandler(this,
                                   EntryChange.class,
                                   handler);

        design.addHandler(this,
                               NameChangeAlert.class,
                               renameHandler);

        this.project.addHandler(this,
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

        setInitiallyEnabled(true);
    }

    protected void updateView()
    {
        Table dictTable = view.getDictTable();
        int selectionCount = 0;
        try {
            selectionCount = dictTable.getSelectionCount();
        } catch (SWTException e) {
            return;
        }

        if (selectionCount == 1) {
            int index = dictTable.getSelectionIndex();
            DictEntry entry = dictionary.getEntry(index);

            if (entry != null) {
                String url = null;
                String space = null;

                if (entry.algorithm instanceof ISitedTermSimilarity) {
                    url =
                        ((ISitedTermSimilarity) entry.algorithm).getContextSite();

                    view.setURLEnabled(true);
                    view.setSpaceEnabled(false);
                }
                else if (entry.algorithm instanceof LSASimilarity) {
                    LSASimilarity lsa = (LSASimilarity) entry.algorithm;

                    space = lsa.getSpace();
                    url = lsa.getURL();

                    if (url == null) {
                        url = LSASimilarity.DEFAULT_LSA_URL;
                    }

                    view.setURLEnabled(true);
                    view.setSpaceEnabled(true);
                }
                else if (entry.algorithm instanceof GensimLSASimilarity) {
                    GensimLSASimilarity lsa = (GensimLSASimilarity) entry.algorithm;

                    space = lsa.getSpace();
                    url = lsa.getURL();

                    if (url == null) {
                        url = GensimLSASimilarity.DEFAULT_LSA_URL;
                    }

                    view.setURLEnabled(true);
                    view.setSpaceEnabled(true);
                }
                else {
                    view.setURLEnabled(false);
                    view.setSpaceEnabled(false);
                }

                view.setURL(url);
                view.setSpace(space);
            }
            else {
                TableItem row = dictTable.getItem(index);
                Combo c = (Combo) row.getData();
                int seln = c.getSelectionIndex();
                boolean isLSA = (seln == DictionaryEditorUIModel.LSA_INDEX);

                view.setURLEnabled((seln == DictionaryEditorUIModel.GOOGLE_WORD_INDEX)
                                         || (seln == DictionaryEditorUIModel.GOOGLE_PHRASE_INDEX)
                                         || isLSA);
                view.setSpaceEnabled(isLSA);
            }
        }
        else {
            // disable if 0 or more than 1 are selected
            view.setURL(null);
            view.setSpace(null);
            view.setURLEnabled(false);
            view.setSpaceEnabled(false);
        }
    }

    @Override
    protected String buildWindowMenuLabel()
    {
        return "";
    }

    @Override
    protected Object getModelObject()
    {
        return dictionary;
    }

    @Override
    public View getView()
    {
        return view;
    }

    @Override
    public Interaction getStandardInteraction()
    {
        return interaction;
    }

    public double parseDouble(String s)
    {
        if (DictionaryEditorUIModel.UNRELATED.equals(s)) {
            return ITermSimilarity.UNKNOWN;
        }

        return Double.parseDouble(s);
    }

    public void initiateEdit(final TableItem row, int column)
    {
        Table dictTable = view.getDictTable();
        editor.horizontalAlignment = SWT.LEFT;

        if (column < 3) {
            Text actualEditor;

            if (column == DictionaryEditorUIModel.SIMIL_COL) {
                actualEditor = new DoubleEntry(dictTable, SWT.NONE)
                {
                    @Override
                    public boolean confirm(int focusRule)
                    {
                        return commitEdit();
                    }

                    @Override
                    public void cancel()
                    {
                        cleanupEditor();
                    }
                };
            }
            else {
                actualEditor = new View.PerformActionText(dictTable,
                                                           SWT.SINGLE | SWT.LEFT)
                {
                    @Override
                    protected boolean doChangeAction()
                    {
                        return commitEdit();
                    }

                    @Override
                    public void cancel()
                    {
                        cleanupEditor();
                    }
                };
            }

            String rowText = row.getText(column);

            if (DictionaryEditorUIModel.UNRELATED.equals(rowText)) {
                rowText = "";
            }

            actualEditor.setText(rowText);
            actualEditor.setFont(dictTable.getFont());

            actualEditor.setFocus();
            actualEditor.selectAll();

            Object data = new Integer(column);

            actualEditor.setData(data);

            editor.setEditor(actualEditor, row, column);
        }

        dictTable.setSelection(row);
    }

    @Override
    public Object getParameters(ListenerIdentifier originalLID,
                                ListenerIdentifier transmutedLID,
                                boolean isContextSelection)
    {
        // Check the general LID, if it is to restore the parent, then
        // return the super's get parameters
        Object parameters = super.getParameters(originalLID,
                                                transmutedLID,
                                                isContextSelection);
        if (parameters != UNSET) {
            return parameters;
        }

        Table dictTable = view.getDictTable();
        int rowIndex = dictTable.getSelectionIndex();

        if (transmutedLID == DictionaryEditorLID.Delete) {
            int[] indices = dictTable.getSelectionIndices();
            Arrays.sort(indices);
            return indices;
        }

        if ((transmutedLID == DictionaryEditorLID.ComputeSimilarity) ||
            (transmutedLID == DictionaryEditorLID.CreateNewEntry))
        {
            if ((0 <= rowIndex) && (rowIndex < dictTable.getItemCount())) {
                TableItem row = dictTable.getItem(rowIndex);
                Combo c = (Combo) row.getData();
                int ind = c.getSelectionIndex();
                return new DictionaryEditorUI.ComputeSimilarityParms(uiModel.getText(rowIndex,
                                                                       DictionaryEditorUIModel.GOAL_COL),
                                                  uiModel.getText(rowIndex,
                                                                       DictionaryEditorUIModel.SEARCH_COL),
                                                  getAlgorithm(c.getItem(ind)),
                                                  rowIndex);
            }
        }

        if (transmutedLID == DictionaryEditorLID.SetAlgorithm) {
            if ((0 <= rowIndex) && (rowIndex < dictTable.getItemCount())) {
                TableItem row = dictTable.getItem(rowIndex);
                Combo c = (Combo) row.getData();
                int ind = c.getSelectionIndex();
                return new DictionaryEditorUI.SetAlgorithmParms(getAlgorithm(c.getItem(ind)),
                                             dictTable.getSelectionIndices());
            }
        }

        return null;
    }

    protected ITermSimilarity getAlgorithm(String algString)
    {
        return DictionaryEditorUIModel.getAlgorithm(algString,
                                                    view.getURL(),
                                                    view.getSpace());
    }

    @Override
    protected void updateTitle()
    {
        String designName = design.getName();
        view.setWindowTitle(modificationFlag
                                 + DICT_PREFIX
                                 + ": "
                                 + project.getName()
                                 + " > "
                                 + designName
                                 + ((OSUtils.MACOSX) ? "" : UI.WINDOW_TITLE));

        view.updateTitle(designName);
    }

    /**
     * Removes stale Text control and selection listener.
     */
    protected void cleanupEditor()
    {
        // Remove and dispose the text box
        Control oldEditor = editor.getEditor();

        if (oldEditor != null) {
            oldEditor.dispose();

            editor.setEditor(null);
        }
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

        setEnabled(ProjectLID.ExportDictToCSV,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(ProjectLID.ImportDict,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);
        setEnabled(ProjectLID.SelectAll,
                   ListenerIdentifierMap.ALL,
                   MenuUtil.ENABLED);

        setViewEnabledState(ListenerIdentifierMap.NORMAL);
    }

    @Override
    public void cleanup(boolean okToContinue, boolean menuHidden)
    {
        super.cleanup(okToContinue, menuHidden);

        setViewEnabledState(ListenerIdentifierMap.NORMAL);

        updateView();
    }

    public void setLIDEnabledState()
    {
        setViewEnabledState(ListenerIdentifierMap.NORMAL);
    }

    protected void setViewEnabledState(Boolean context)
    {
        try {
            Table dictTable = view.getDictTable();
            int selectionCount = dictTable.getSelectionCount();
            boolean pendingSelected =
                    dictTable.isSelected(dictTable.getItemCount() - 1);
            boolean enabled = ! pendingSelected && (selectionCount >= 1);

            setEnabled(CogToolLID.Delete, context, enabled);

            enabled = selectionCount == 1;

            setEnabled(DictionaryEditorLID.SetSimilarity, context, enabled);
            setEnabled(DictionaryEditorLID.SetGoalString, context, enabled);
            setEnabled(DictionaryEditorLID.SetSearchString, context, enabled);
        } catch (SWTException e) {
            // ignore
        }
    }

    protected boolean commitEdit()
    {
        Text actualEditor = (Text) editor.getEditor();

        if (actualEditor != null) {
            Table dictTable = view.getDictTable();
            modifiedRow = dictTable.getSelectionIndex();

            if (modifiedRow < 0) {
                cleanupEditor();
                return true;
            }

            Integer editCol = (Integer) actualEditor.getData();
            int col = editCol.intValue();
            String value = actualEditor.getText();
            Object editParms;

            if (! ("".equals(value))) {
                DictEntry entry = dictionary.getEntry(modifiedRow);

                if (col == DictionaryEditorUIModel.SIMIL_COL) {
                    // check this first because we set this regardless of
                    // whether there's an entry for this row already
                    double simil = parseDouble(value);
                    editParms = new DictionaryEditorUI.SetSimilarityParms(simil, modifiedRow);
                    performAction(DictionaryEditorLID.SetSimilarity, editParms);
                }
                else if (entry == null) {
                    addTerm(col, value, modifiedRow);
                }
                else if (col == DictionaryEditorUIModel.GOAL_COL) {
                    editParms = new DictionaryEditorUI.SetStringParms(value, modifiedRow);
                    performAction(DictionaryEditorLID.SetGoalString, editParms);
                }
                else if (col == DictionaryEditorUIModel.SEARCH_COL) {
                    editParms = new DictionaryEditorUI.SetStringParms(value, modifiedRow);
                    performAction(DictionaryEditorLID.SetSearchString, editParms);
                }
            }
            else {
                // text is empty, set similarity to UNKNOWN if applicable
                if (col == DictionaryEditorUIModel.SIMIL_COL) {
                    double simil = ITermSimilarity.UNKNOWN;
                    editParms = new DictionaryEditorUI.SetSimilarityParms(simil, modifiedRow);
                    performAction(DictionaryEditorLID.SetSimilarity, editParms);
                }
            }
        }

        cleanupEditor();
        return true;
    }

    protected void addTerm(int column, String newText, int row)
    {
        String otherText = uiModel.getText(row, (column + 1) % 2);
        boolean createEntry = ! ("".equals(otherText));

        if (createEntry) {
            // both strings are filled in, so add an entry to the dictionary
            uiModel.setText(row, column, newText);
            if (! performAction(DictionaryEditorLID.CreateNewEntry)) {
                // Adding the new entry failed, so undo the change to the
                // pending entry
                uiModel.setText(row, column, "");
            }
        }
        else {
            // only one string is not empty, so don't save anything to the model
            performAction(DictionaryEditorLID.StartNewEntry,
                          new DictionaryEditorUI.AddStringParms(newText,
                                             column == DictionaryEditorUIModel.GOAL_COL));
        }
    }

    @Override
    protected void updateWindowMenus()
    {
        // do nothing
    }

    @Override
    public void dispose()
    {
        if (editor != null) {
            editor.dispose();
        }

        uiModel.dispose();

        CogTool.repaintPhase.removeDelayedWork(delayedRepainting);

        dictionary.removeAllHandlers(this);
        design.removeAllHandlers(this);
        project.removeAllHandlers(this);

        super.dispose();
    }

    public void selectAll()
    {
        Table dictTable = view.getDictTable();
        int size = dictTable.getItemCount();

        // Don't select the pending entry row
        dictTable.setSelection(0, size - 2);
    }
}