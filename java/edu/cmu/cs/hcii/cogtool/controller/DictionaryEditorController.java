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

import java.util.ArrayList;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictValue;
import edu.cmu.cs.hcii.cogtool.ui.DictionaryEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.DictionaryEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.PendingDictEntry;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

public class DictionaryEditorController extends DefaultController
{
    protected static final String ADD_TERM =
        L10N.get("DEC.AddTerm", "Add Term");
    protected static final String MODIFY_TERM =
        L10N.get("DEC.ModifyTerm", "Modify Term");
    protected static final String SET_SIMILARITY =
        L10N.get("DEC.SetSimilarity", "Set Similarity");
    protected static final String SET_ALGORITHM =
        L10N.get("DEC.SetAlgorithm", "Set Algorithm");
    protected static final String CREATE_ROW =
        L10N.get("DEC.CreateRow", "Create Row");
    protected static final String DELETE_ROW =
        L10N.get("DEC.DeleteRow", "Delete Row");
    protected static final String DELETE_ROWS =
        L10N.get("DEC.DeleteRows", "Delete Rows");
    protected static final String ALG_UNCHANGED =
        L10N.get("DEC.UnchangedAlg", "Algorithm was not changed.");
    protected static final String ALG_CHANGED =
        L10N.get("DEC.ChangedAlg", "Algorithm changed successfully.");
    protected static final String SIMIL_UNCHANGED =
        L10N.get("DEC.UnchangedSimil", "Similarity was not changed.");
    protected static final String STRING_UNCHANGED =
        L10N.get("DEC.UnchangedString", "String was not changed.");
    protected static final String ERROR_TITLE =
        L10N.get("DEC.ErrorTitle", "Dictionary Error");
    protected static final String NO_ENTRY =
        L10N.get("DEC.NoEntry", "Cannot edit a nonexistant entry.");
    protected static final String NO_SELECTION =
        L10N.get("DEC.NoSelection", "Nothing is selected.");
    protected static final String ENTRY_EXISTS =
        L10N.get("DEC.EntryExists",
                 "This entry already exists in the dictionary.");

    protected DictionaryEditorUI ui;
    protected ISimilarityDictionary dictionary;
    protected Interaction interaction;

    protected PendingDictEntry pendingEntry;

    protected Design design;

    public DictionaryEditorController(ISimilarityDictionary dict,
                                      Design d,
                                      Project proj)
    {
        super(proj);

        dictionary = dict;
        design = d;

        undoMgr =
            UndoManager.getUndoManager(dictionary,
                                                      project);

        pendingEntry = new PendingDictEntry(dictionary);

        ui = new DictionaryEditorUI(dictionary, design, pendingEntry, project, undoMgr);

        interaction = ui.getStandardInteraction();

        assignActions();

        ui.setVisible(true);
    }

    @Override
    public void assignActions()
    {
        super.assignActions();

        // Enable undo & redo
        ui.setAction(FrameEditorLID.Undo,
                          new UndoController.UndoAction(undoMgr,
                                                        interaction));

        ui.setAction(FrameEditorLID.Redo,
                          new UndoController.RedoAction(undoMgr,
                                                        interaction));

        ui.setAction(DictionaryEditorLID.StartNewEntry,
                          createAddNewTermAction());

        ui.setAction(DictionaryEditorLID.SetGoalString,
                          createSetStringAction(0));
        ui.setAction(DictionaryEditorLID.SetSearchString,
                          createSetStringAction(1));
        ui.setAction(DictionaryEditorLID.CreateNewEntry,
                          createAddNewEntryAction());

        ui.setAction(DictionaryEditorLID.SetSimilarity,
                          createSetSimilarityAction());
        ui.setAction(DictionaryEditorLID.ComputeSimilarity,
                          createComputeSimilarityAction());

        ui.setAction(DictionaryEditorLID.SetAlgorithm,
                          createSetAlgorithmAction());

        ui.setAction(DictionaryEditorLID.Delete,
                          createDeleteEntryAction());

        ui.setAction(ProjectLID.ExportDictToCSV,
                          new AListenerAction() {
                              public boolean performAction(Object actionParms)
                              {
                                  return DictionaryEditorCmd.exportDictionaryToCSV(dictionary,
                                                                                   interaction);
                              }
                          });

        ui.setAction(ProjectLID.ImportDict,
                          new AListenerAction() {
                              public boolean performAction(Object actionParms)
                              {
                                  return DictionaryEditorCmd.importDictionary(design,
                                                                              dictionary,
                                                                              WidgetAttributes.NO_DICTIONARY,
                                                                              interaction,
                                                                              undoMgr, null);
                              }
                          });

        ui.setAction(CogToolLID.SelectAll,
                          new AListenerAction() {
                              public boolean performAction(Object actionParms)
                              {
                                  ui.selectAll();
                                  return true;
                              }
                          });
    }

    protected double computeSimilarity(String goal,
                                       String search,
                                       ITermSimilarity alg)
    {
        List<String> errors = new ArrayList<String>();

        double similarity = alg.determineSimilarity(goal,
                                                    search,
                                                    errors,
                                                    null);

        if (errors.size() > 0) {
            interaction.reportProblems("Encountered:", errors);
        }

        return similarity;
    }

    protected IListenerAction createAddNewTermAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.AddStringParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                final DictionaryEditorUI.AddStringParms parms = (DictionaryEditorUI.AddStringParms) actionParms;

                final String oldString;

                if (parms.isGoal) {
                    oldString = pendingEntry.getDictEntry().goalWord;

                    if (parms.newString.equals(oldString)) {
                        interaction.setStatusMessage(STRING_UNCHANGED);
                        return true;
                    }

                    pendingEntry.setGoal(parms.newString);
                }
                else {
                    oldString = pendingEntry.getDictEntry().searchWord;

                    if (parms.newString.equals(oldString)) {
                        interaction.setStatusMessage(STRING_UNCHANGED);
                        return true;
                    }

                    pendingEntry.setSearch(parms.newString);
                }

                undoMgr.addEdit(new AUndoableEdit(DictionaryEditorLID.StartNewEntry)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return ADD_TERM;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        if (parms.isGoal) {
                            pendingEntry.setGoal(parms.newString);
                        }
                        else {
                            pendingEntry.setSearch(parms.newString);
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        if (parms.isGoal) {
                            pendingEntry.setGoal(oldString);
                        }
                        else {
                            pendingEntry.setSearch(oldString);
                        }
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createDeleteEntryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return int[].class;
            }

            public boolean performAction(Object actionParms)
            {
                final int[] rows = (int[]) actionParms;
                final DictEntry[] deletedEntries = new DictEntry[rows.length];
                final DictValue[] deletedValues = new DictValue[rows.length];

                for (int i = rows.length - 1; i >= 0; i--) {
                    deletedEntries[i] = dictionary.getEntry(rows[i]);
                    if (deletedEntries[i] == null) {
                        interaction.reportProblem(ERROR_TITLE, NO_ENTRY);
                        continue;
                    }

                    deletedValues[i] = dictionary.getValue(deletedEntries[i]);
                    dictionary.removeEntry(rows[i]);
                }

                undoMgr.addEdit(new AUndoableEdit(DictionaryEditorLID.Delete)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return (rows.length == 1) ? DELETE_ROW : DELETE_ROWS;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        for (int i = rows.length - 1; i >= 0; i--) {
                            dictionary.removeEntry(rows[i]);
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        for (int i = 0; i < rows.length; i++) {
                            dictionary.insertEntry(deletedEntries[i],
                                                   deletedValues[i],
                                                   rows[i]);
                        }
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createAddNewEntryAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.ComputeSimilarityParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                final DictionaryEditorUI.ComputeSimilarityParms parms =
                    (DictionaryEditorUI.ComputeSimilarityParms) actionParms;
                final ITermSimilarity oldAlg =
                    pendingEntry.getDictEntry().algorithm;
                final double oldSimilarity = pendingEntry.getSimilarity();
                final double similarity;

                if (checkNewEntry(parms.goalString,
                                  parms.searchString,
                                  parms.algorithm))
                {
                    interaction.reportProblem(ERROR_TITLE, ENTRY_EXISTS);
                    return false;
                }

                if (parms.algorithm == ITermSimilarity.MANUAL) {
                    similarity = oldSimilarity;
                }
                else {
                    similarity = computeSimilarity(parms.goalString,
                                                   parms.searchString,
                                                   parms.algorithm);
                }

                final String oldGoal = pendingEntry.getDictEntry().goalWord;
                final String oldSearch = pendingEntry.getDictEntry().searchWord;

                pendingEntry.clear();

                final DictValue value = new DictValue(similarity);
                dictionary.setSimilarity(parms.goalString,
                                         parms.searchString,
                                         parms.algorithm,
                                         value,
                                         parms.rowIndex);

                undoMgr.addEdit(new AUndoableEdit(DictionaryEditorLID.CreateNewEntry) {
                    @Override
                    public String getPresentationName()
                    {
                        return CREATE_ROW;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        pendingEntry.clear();

                        dictionary.setSimilarity(parms.goalString,
                                                 parms.searchString,
                                                 parms.algorithm,
                                                 value,
                                                 parms.rowIndex);
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        dictionary.removeEntry(parms.rowIndex);

                        pendingEntry.setGoal(oldGoal);
                        pendingEntry.setSearch(oldSearch);
                        pendingEntry.setAlgorithm(oldAlg);
                        pendingEntry.setSimilarity(oldSimilarity);
                    }
                });

                return true;
            }
        };
    }

    protected IListenerAction createSetAlgorithmAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.SetAlgorithmParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                DictionaryEditorUI.SetAlgorithmParms parms =
                    (DictionaryEditorUI.SetAlgorithmParms) actionParms;

                double similarity;

                if (parms.rowIndices.length == 0) {
                    interaction.reportProblem(ERROR_TITLE, NO_SELECTION);
                    return false;
                }

                CompoundUndoableEdit editSeq =
                    new CompoundUndoableEdit(SET_ALGORITHM,
                                             DictionaryEditorLID.SetAlgorithm);

                for (int rowIndice : parms.rowIndices) {
                    DictEntry entry = dictionary.getEntry(rowIndice);

                    if (entry == null) {
                        final ITermSimilarity oldAlg =
                            pendingEntry.getDictEntry().algorithm;
                        final double oldSimilarity =
                            pendingEntry.getSimilarity();
                        final ITermSimilarity newAlg = parms.algorithm;

                        boolean equal = (newAlg == null) ? oldAlg == null
                                                         : newAlg.equals(oldAlg);

                        if (equal) {
                            interaction.setStatusMessage(ALG_UNCHANGED);
                            continue;
                        }

                        pendingEntry.setAlgorithm(newAlg);
                        pendingEntry.setSimilarity(ITermSimilarity.UNKNOWN);

                        editSeq.addEdit(new AUndoableEdit(DictionaryEditorLID.SetAlgorithm) {
                            @Override
                            public String getPresentationName()
                            {
                                return SET_ALGORITHM;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                pendingEntry.setAlgorithm(newAlg);
                                pendingEntry.setSimilarity(ITermSimilarity.UNKNOWN);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                pendingEntry.setAlgorithm(oldAlg);
                                pendingEntry.setSimilarity(oldSimilarity);
                            }
                        });

                        interaction.setStatusMessage(ALG_CHANGED);
                        continue;
                    }
                    if (parms.algorithm.equals(entry.algorithm)) {
                        interaction.setStatusMessage(ALG_UNCHANGED);
                        continue;
                    }

                    if (checkNewEntry(entry.goalWord,
                                      entry.searchWord,
                                      parms.algorithm))
                    {
                        interaction.reportProblem(ERROR_TITLE, ENTRY_EXISTS);
                        continue;
                    }

                    if (parms.algorithm != ITermSimilarity.MANUAL) {

                        similarity = computeSimilarity(entry.goalWord,
                                                       entry.searchWord,
                                                       parms.algorithm);
                    }
                    else {
                        similarity = dictionary.getValue(entry).similarity;
                    }

                    setSimilarity(rowIndice,
                                  similarity,
                                  parms.algorithm,
                                  editSeq);
                }

                editSeq.end();

                if (editSeq.isSignificant()) {
                    undoMgr.addEdit(editSeq);
                    interaction.setStatusMessage(ALG_CHANGED);
                }

                return true;
            }
        };
    }

    /**
     * Return true if an entry with these components already exists
     * (Don't allow an entry to be modified into a duplicate of another existing
     * entry.)
     */
    protected boolean checkNewEntry(String goalWord,
                                    String searchWord,
                                    ITermSimilarity algorithm)
    {
        DictEntry testEntry = new DictEntry(goalWord, searchWord, algorithm);

        return dictionary.containsEntry(testEntry);
    }

    protected IListenerAction createComputeSimilarityAction()
    {
        return new IListenerAction() {

            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.ComputeSimilarityParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                DictionaryEditorUI.ComputeSimilarityParms parms =
                    (DictionaryEditorUI.ComputeSimilarityParms) actionParms;

                double similarity;

                if ("".equals(parms.goalString) ||
                    "".equals(parms.searchString))
                {
                    interaction.reportProblem(ERROR_TITLE, NO_ENTRY);
                    return true;
                }

                similarity = computeSimilarity(parms.goalString,
                                               parms.searchString,
                                               parms.algorithm);

                setSimilarity(parms.rowIndex, similarity, parms.algorithm,
                              undoMgr);

                return true;
            }
        };
    }

    protected IListenerAction createSetSimilarityAction()
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.SetSimilarityParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                DictionaryEditorUI.SetSimilarityParms parms = (DictionaryEditorUI.SetSimilarityParms) actionParms;

                DictEntry entry = dictionary.getEntry(parms.rowIndex);
                if (entry == null) {
                    final double oldSimil = pendingEntry.getSimilarity();
                    final double newSimil = parms.similarity;

                    if (PrecisionUtilities.withinEpsilon(oldSimil,
                                                         newSimil,
                                                         0.001))
                    {
                        interaction.setStatusMessage(SIMIL_UNCHANGED);
                        return true;
                    }

                    final ITermSimilarity oldAlg =
                        pendingEntry.getDictEntry().algorithm;

                    pendingEntry.setSimilarity(newSimil);
                    pendingEntry.setAlgorithm(ITermSimilarity.MANUAL);

                    undoMgr.addEdit(new AUndoableEdit(DictionaryEditorLID.SetSimilarity) {
                        @Override
                        public String getPresentationName()
                        {
                            return SET_SIMILARITY;
                        }

                        @Override
                        public void redo()
                        {
                            super.redo();

                            pendingEntry.setSimilarity(newSimil);
                            pendingEntry.setAlgorithm(ITermSimilarity.MANUAL);
                        }

                        @Override
                        public void undo()
                        {
                            super.undo();

                            pendingEntry.setSimilarity(oldSimil);
                            pendingEntry.setAlgorithm(oldAlg);
                        }
                    });

                    return true;
                }

                if (PrecisionUtilities.withinEpsilon(dictionary.getValue(entry).similarity,
                                                     parms.similarity,
                                                     0.001))
                {
                    interaction.setStatusMessage(SIMIL_UNCHANGED);
                    return true;
                }

                setSimilarity(parms.rowIndex,
                              parms.similarity,
                              ITermSimilarity.MANUAL,
                              undoMgr);

                return true;
            }
        };
    }

    protected void setSimilarity(final int rowIndex,
                                 final double similarity,
                                 final ITermSimilarity algorithm,
                                 IUndoableEditSequence editSeq)
    {
        DictEntry entry = dictionary.getEntry(rowIndex);
        final String goal = entry.goalWord;
        final String search = entry.searchWord;
        DictValue oldV = dictionary.getValue(entry);
        final DictValue oldValue = new DictValue(oldV.similarity,
                                                 oldV.editedDate);
        final DictValue newValue = new DictValue(similarity);

        if (PrecisionUtilities.withinEpsilon(oldValue.similarity,
                                             similarity,
                                             0.001))
        {
            interaction.setStatusMessage(SIMIL_UNCHANGED);
            return;
        }

        final ITermSimilarity oldAlg = entry.algorithm;

        dictionary.setSimilarity(goal, search, algorithm,
                                      newValue, rowIndex);

        editSeq.addEdit(new AUndoableEdit(DictionaryEditorLID.SetSimilarity) {
            @Override
            public String getPresentationName()
            {
                return SET_SIMILARITY;
            }

            @Override
            public void redo()
            {
                super.redo();

                dictionary.setSimilarity(goal, search, algorithm,
                                         newValue, rowIndex);
            }

            @Override
            public void undo()
            {
                super.undo();

                dictionary.setSimilarity(goal, search, oldAlg,
                                         oldValue, rowIndex);
            }
        });
    }

    protected IListenerAction createSetStringAction(final int column)
    {
        return new IListenerAction() {
            public Class<?> getParameterClass()
            {
                return DictionaryEditorUI.SetStringParms.class;
            }

            public boolean performAction(Object actionParms)
            {
                DictionaryEditorUI.SetStringParms p = (DictionaryEditorUI.SetStringParms) actionParms;

                if (p != null) {
                    final int rowIndex = p.rowIndex;
                    final String newString = p.string;

                    IUndoableEdit edit = null;

                    DictEntry entry = dictionary.getEntry(rowIndex);

                    if (entry == null) {
                        interaction.reportProblem(ERROR_TITLE, NO_ENTRY);
                        return true;
                    }

                    CogToolLID lid;
                    final String goal;
                    final String search;
                    final String oldGoal;
                    final String oldSearch;

                    if (column == 0) {
                        goal = newString;
                        oldGoal = entry.goalWord;

                        if (goal.equals(oldGoal)) {
                            interaction.setStatusMessage(STRING_UNCHANGED);
                            return true;
                        }

                        search = entry.searchWord;
                        oldSearch = search;
                        lid = DictionaryEditorLID.SetGoalString;
                    }
                    else {
                        goal = entry.goalWord;
                        oldGoal = goal;
                        search = newString;
                        oldSearch = entry.searchWord;

                        if (search.equals(oldSearch)) {
                            interaction.setStatusMessage(STRING_UNCHANGED);
                            return true;
                        }

                        lid = DictionaryEditorLID.SetSearchString;
                    }

                    if (checkNewEntry(goal, search, entry.algorithm)) {
                        interaction.reportProblem(ERROR_TITLE, ENTRY_EXISTS);
                        return false;
                    }

                    dictionary.updateEntry(rowIndex, goal, search);

                    CompoundUndoableEdit editSeq =
                        new CompoundUndoableEdit(MODIFY_TERM, lid);

                    edit =
                        new AUndoableEdit(lid)
                        {
                            @Override
                            public String getPresentationName()
                            {
                                return MODIFY_TERM;
                            }

                            @Override
                            public void redo()
                            {
                                super.redo();

                                dictionary.updateEntry(rowIndex,
                                                       goal,
                                                       search);
                            }

                            @Override
                            public void undo()
                            {
                                super.undo();

                                dictionary.updateEntry(rowIndex,
                                                       oldGoal,
                                                       oldSearch);
                            }
                        };

                    editSeq.addEdit(edit);

                    if (entry.algorithm != ITermSimilarity.MANUAL) {
                        double similarity = computeSimilarity(goal,
                                                              search,
                                                              entry.algorithm);

                        setSimilarity(rowIndex,
                                      similarity,
                                      entry.algorithm,
                                      editSeq);
                    }

                    editSeq.end();

                    undoMgr.addEdit(editSeq);
                }

                return true;
            }
        };
    }

    @Override
    protected Object getModelObject()
    {
        return dictionary;
    }

    @Override
    public UI getUI()
    {
        return ui;
    }

    public void setCurrentAlgorithm(ITermSimilarity alg)
    {
        pendingEntry.setAlgorithm(alg);
    }

    public ITermSimilarity getCurrentAlgorithm()
    {
        return pendingEntry.getDictEntry().algorithm;
    }

    /**
     * Creates a new DictionaryEditorController instance for viewing the scripts
     * in a task group
     * @param d
     * @return the Controller instance for the given DictionaryEditorController
     */
    public static DictionaryEditorController openController(ISimilarityDictionary dict,
                                                           Design d,
                                                           Project taProj)
    {
        // Check whether this project is already open
        DictionaryEditorController controller =
            (DictionaryEditorController)
                ControllerRegistry.ONLY.findOpenController(dict);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            // if this project isn't open, create a new controller
            controller = new DictionaryEditorController(dict, d, taProj);
            ControllerRegistry.ONLY.addOpenController(controller);
        }

        controller.setCurrentAlgorithm(dict.getCurrentAlgorithm());

        return controller;
    }
}
