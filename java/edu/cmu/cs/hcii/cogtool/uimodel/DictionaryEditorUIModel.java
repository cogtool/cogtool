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

package edu.cmu.cs.hcii.cogtool.uimodel;

import java.text.NumberFormat;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.model.CachedGoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.GLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.GoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.LSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.GensimLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.MSRSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictValue;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictionaryChange;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.EntryChange;
import edu.cmu.cs.hcii.cogtool.ui.PendingDictEntry;
import edu.cmu.cs.hcii.cogtool.ui.PendingDictEntry.PendingEntryChange;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.L10N;

public class DictionaryEditorUIModel extends DefaultUIModel
{
    public static final int GOAL_COL = 0;
    public static final int SEARCH_COL = 1;
    public static final int SIMIL_COL = 2;
    public static final int DATE_COL = 4;

    public static final String UNRELATED =
        L10N.get("DEUIM.Unrelated", "Unrelated");

    protected ISimilarityDictionary dictionary;
    protected PendingDictEntry pendingEntry;
    protected Table dictTable;
    protected SelectionListener algListener;

    public static final int LSA_INDEX = 0;
    public static final int MSR_INDEX = 1;
    public static final int GLSA_INDEX = 2;
    public static final int GOOGLE_WORD_INDEX = 3;
    public static final int GOOGLE_PHRASE_INDEX = 4;
    public static final int GENSIM_LSA_INDEX = 5;

    public static final String DEFAULT_ALGORITHM = "LSA";

    public static final String[] ALGORITHMS =
        { DEFAULT_ALGORITHM,
          "RPI", "GLSA", "PMI-G (Word)", "PMI-G (Phrase)", "GENSIM", "Manual" };

    public static final int MANUAL_INDEX = ALGORITHMS.length - 1;

    public DictionaryEditorUIModel(ISimilarityDictionary dict,
                                   PendingDictEntry pendingDEntry,
                                   Table dTable,
                                   SelectionListener listener,
                                   Project proj)
    {
        super(proj);

        dictionary = dict;
        pendingEntry = pendingDEntry;
        dictTable = dTable;
        algListener = listener;

        AlertHandler handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                DictionaryChange change = (DictionaryChange) alert;

                if (change.isAdd) {
                    insertRow(change.rowIndex);
                }
                else {
                    TableItem row = dictTable.getItem(change.rowIndex);
                    ((Combo) row.getData()).dispose();
                    row.dispose();
                }
            }
        };

        dictionary.addHandler(this,
                                   DictionaryChange.class,
                                   handler);

        handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                fillRow(pendingEntry.getDictEntry(),
                        new DictValue(pendingEntry.getSimilarity(), null));
            }
        };

        pendingEntry.addHandler(this, PendingEntryChange.class, handler);

        handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                EntryChange change =
                    (EntryChange) alert;

                DictEntry entry = dictionary.getEntry(change.rowIndex);

                fillRow(change.rowIndex, entry, change.value);
            }
        };

        dictionary.addHandler(this,
                                   EntryChange.class,
                                   handler);

        fillTable();
    }

    public void setText(int row, int column, String newText)
    {
        dictTable.getItem(row).setText(column, newText);
    }

    public String getText(int row, int column)
    {
        return dictTable.getItem(row).getText(column);
    }

    /**
     * Creates an empty table, or loads values from an existing dictionary
     */
    public void fillTable()
    {
        List<DictEntry> entries = dictionary.getEntries();

        if (entries.size() > 0) {
            Iterator<DictEntry> entryIter = entries.iterator();

            while (entryIter.hasNext()) {
                DictEntry entry = entryIter.next();
                DictValue dValue = dictionary.getValue(entry);
                TableItem row = createEmptyRow();

                fillRow(row, entry, dValue);
            }
        }

        createEmptyRow();
    }

    public void fillRow(DictEntry entry, DictValue value)
    {
        TableItem lastRow =
            dictTable.getItem(dictTable.getItemCount() - 1);

        fillRow(lastRow, entry, value);
    }

    public void fillRow(int row, DictEntry entry, DictValue value)
    {
        fillRow(dictTable.getItem(row), entry, value);
    }

    public void fillRow(TableItem row, DictEntry entry, DictValue value)
    {
        row.setText(GOAL_COL, entry.goalWord);
        row.setText(SEARCH_COL, entry.searchWord);

        String simil;
        if (value.similarity == ITermSimilarity.UNKNOWN) {
            if (("".equals(entry.goalWord)) || ("".equals(entry.searchWord))) {
                simil = "";
            }
            else {
                simil = UNRELATED;
            }
        }
        else {
            NumberFormat nFmtr = NumberFormat.getInstance(Locale.US);

            nFmtr.setMaximumFractionDigits(3);
            nFmtr.setMinimumFractionDigits(3);

            simil =
                nFmtr.format(Double.parseDouble(Double.toString(value.similarity)));
        }

        row.setText(SIMIL_COL, simil);

        Combo c = (Combo) row.getData();
        int index = getAlgIndex(entry.algorithm);

        c.select(index);
        row.setText(DATE_COL,
                    (value.editedDate == null) ? ""
                                               : value.editedDate.toString());

    }

    public static int getAlgIndex(ITermSimilarity algorithm)
    {
        int index = MANUAL_INDEX;

        if (GLSASimilarity.ONLY.equals(algorithm)) {
            index = GLSA_INDEX;
        }
        else if (MSRSimilarity.ONLY.equals(algorithm)) {
            index = MSR_INDEX;
        }
        else if (algorithm instanceof LSASimilarity) {
            index = LSA_INDEX;
        }
        else if (algorithm instanceof GensimLSASimilarity) {
            index = GENSIM_LSA_INDEX;
        }
        else if (algorithm instanceof CachedGoogleSimilarity) {
            index = GOOGLE_WORD_INDEX;
        }
        else if (algorithm instanceof GoogleSimilarity) {
            index = GOOGLE_PHRASE_INDEX;
        }

        return index;
    }

    public static String getAlgLabel(ITermSimilarity alg)
    {
        return ALGORITHMS[getAlgIndex(alg)];
    }

    public void insertRow(int rowIndex)
    {
        DictEntry entry = dictionary.getEntry(rowIndex);

        if (entry != null) {
            fillRow(createEmptyRow(rowIndex),
                    entry,
                    dictionary.getValue(entry));
        }
    }

    public TableItem createEmptyRow()
    {
        return createEmptyRow(dictTable.getItemCount());
    }

    public TableItem createEmptyRow(int index)
    {
        final TableItem emptyRow =
            new TableItem(dictTable, SWT.NONE, index);

        emptyRow.setText(GOAL_COL, "");
        emptyRow.setText(SEARCH_COL, "");
        emptyRow.setText(SIMIL_COL, "");
        emptyRow.setText(DATE_COL, "");

        TableEditor editor = new TableEditor(dictTable);
        editor.grabHorizontal = true;

        Combo algCombo = createAlgCombo(dictTable,
                                        dictionary.getCurrentAlgorithm(),
                                        algListener);

        algCombo.setData(emptyRow);
        emptyRow.setData(algCombo);

        editor.setEditor(algCombo, emptyRow, 3);

        return emptyRow;
    }

    @Override
    public void dispose()
    {
        dictionary.removeAllHandlers(this);
        pendingEntry.removeAllHandlers(this);

        super.dispose();
    }

    public static Combo createAlgCombo(Composite parent,
                                       ITermSimilarity defaultAlg,
                                       SelectionListener listener)
    {
        Combo algCombo = new ComboWithEnableFix(parent,
                                                SWT.DROP_DOWN | SWT.READ_ONLY);

        for (String element : ALGORITHMS) {
            algCombo.add(element);
        }

        algCombo.select(getAlgIndex(defaultAlg));

        algCombo.addSelectionListener(listener);

        algCombo.setVisibleItemCount(ALGORITHMS.length);

        return algCombo;
    }

    public static ITermSimilarity getAlgorithm(String algSeln,
                                               String site,
                                               String space)
    {
        ITermSimilarity computeSimilarity = ITermSimilarity.MANUAL;

        if (ALGORITHMS[GLSA_INDEX].equals(algSeln)) {
            computeSimilarity = GLSASimilarity.ONLY;
        }
        else if (ALGORITHMS[MSR_INDEX].equals(algSeln)) {
            computeSimilarity = MSRSimilarity.ONLY;
        }
        else if (ALGORITHMS[LSA_INDEX].equals(algSeln)) {
            computeSimilarity = LSASimilarity.create(space, site);
        }
        else if (ALGORITHMS[GENSIM_LSA_INDEX].equals(algSeln)) {
            computeSimilarity = GensimLSASimilarity.create(space, site);
        }
        else {
            if ((site != null) && site.equals("")) {
                site = null;
            }

            if (ALGORITHMS[GOOGLE_WORD_INDEX].equals(algSeln)) {
                computeSimilarity = CachedGoogleSimilarity.create(site);
            }
            else if (ALGORITHMS[GOOGLE_PHRASE_INDEX].equals(algSeln)) {
                computeSimilarity = GoogleSimilarity.create(site);
            }
        }

        return computeSimilarity;
    }

    public void updateRow(int modifiedRow)
    {
        DictEntry entry = dictionary.getEntry(modifiedRow);

        if (entry != null) {
            TableItem row = dictTable.getItem(modifiedRow);
            row.setText(DictionaryEditorUIModel.GOAL_COL, entry.goalWord);
            row.setText(DictionaryEditorUIModel.SEARCH_COL, entry.searchWord);

            Combo c = (Combo) row.getData();
            c.select(DictionaryEditorUIModel.getAlgIndex(entry.algorithm));
        }
    }
    
    public static ITermSimilarity computeAlgorithm(String algString, String urlString, String spaceString, String limitingSite)
    {
        String useURL;
        if (DEFAULT_ALGORITHM.equals(algString)) {
            useURL = urlString;
        }
        else if (ALGORITHMS[GENSIM_LSA_INDEX].equals(algString) ) {
            useURL = urlString;
        }
        else 
            useURL = limitingSite;
        return getAlgorithm(algString, useURL, spaceString);
    }
}
