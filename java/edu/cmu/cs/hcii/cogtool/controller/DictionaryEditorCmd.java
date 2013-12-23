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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.model.CachedGoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.CachedTermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.GLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.GoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ISitedTermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.LSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.GensimLSASimilarity;
import edu.cmu.cs.hcii.cogtool.model.MSRSimilarity;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictValue;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.CSVSupport;
import edu.cmu.cs.hcii.cogtool.util.FileUtil;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.PersistentDate;
import edu.cmu.cs.hcii.cogtool.util.RcvrParsingException;

public class DictionaryEditorCmd
{
    protected static final String DEFAULT_DICT_PREFIX =
        L10N.get("PM.NewDictName", "Dictionary");
    protected static int nextNewDictSuffix = 1;

    protected static final String IMPORT_DICTIONARY =
        L10N.get("PC.ImportDictionary", "Import Dictionary");

    protected static final String DICT_CSV_VERSION_0 = "0";

    protected static String getAlgorithmName(ITermSimilarity algorithm)
    {
        String result = "Manual";

        if (GLSASimilarity.ONLY.equals(algorithm)) {
            result = "GLSA";
        }
        else if (MSRSimilarity.ONLY.equals(algorithm)) {
            result = "MSR";
        }
        else if (algorithm instanceof LSASimilarity) {
            result = "LSA";
        }
        else if (algorithm instanceof GensimLSASimilarity) {
            result = "GENSIM";
        }
        else if (algorithm instanceof CachedGoogleSimilarity) {
            result = "Old Google";
        }
        else if (algorithm instanceof GoogleSimilarity) {
            result = "Google";
        }

        return result;
    }

    protected static ITermSimilarity getNameAlgorithm(String name,
                                                      String site,
                                                      String space)
    {
        ITermSimilarity result = ITermSimilarity.MANUAL;

        if ("GLSA".equals(name)) {
            result = GLSASimilarity.ONLY;
        }
        else if ("MSR".equals(name)) {
            result = MSRSimilarity.ONLY;
        }
        else if ("LSA".equals(name)) {
            result = LSASimilarity.create(space, site);
        }
        else if ("GENSIM".equals(name)) {
            result = GensimLSASimilarity.create(site);
        }
        else if ("Old Google".equals(name)) {
            result = CachedGoogleSimilarity.create(site);
        }
        else if ("Google".equals(name)) {
            result = GoogleSimilarity.create(site);
        }

        return result;
    }

    protected static String toString(double similarity)
    {
        return CachedTermSimilarity.toString(similarity);
    }

    public static boolean exportDictionaryToCSV(ISimilarityDictionary dictionary,
                                                Interaction interaction)
    {
        File dest = interaction.selectCSVFileDest(DEFAULT_DICT_PREFIX + "" +
                                                  nextNewDictSuffix++);
        if (dest == null) {
            return false;
        }

        if (dictionary.size() == 0) {
            interaction.reportProblem("Export Dictionary",
                                      "Selected dictionary is empty");
            return true;
        }

        FileWriter fw = null;
        BufferedWriter buffer = null;

        try {
            fw = new FileWriter(dest);
            buffer = new BufferedWriter(fw);

            // first write out header, version, and column titles
            CSVSupport.writeCell(ISimilarityDictionary.COGTOOL_DICT_HEADER,
                                 buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell(DICT_CSV_VERSION_0, buffer);
            CSVSupport.addLineEnding(buffer);

            CSVSupport.writeCell("Term", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell("Display Labels", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell("Algorithm", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell("Limiting URL", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell("Similarity", buffer);
            CSVSupport.addSeparator(buffer);
            CSVSupport.writeCell("Date computed", buffer);
            CSVSupport.addLineEnding(buffer);

            Iterator<DictEntry> values = dictionary.getEntries().iterator();

            while (values.hasNext()) {
                DictEntry entry = values.next();
                DictValue value = dictionary.getValue(entry);
                String url = "";
                if (entry.algorithm instanceof ISitedTermSimilarity) {
                    url = ((ISitedTermSimilarity) entry.algorithm).getContextSite();

                    if (url == null) {
                        url = "";
                    }
                }
                String simil = toString(value.similarity);

                CSVSupport.writeCell(entry.goalWord, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(entry.searchWord, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(getAlgorithmName(entry.algorithm), buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(url, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(simil, buffer);
                CSVSupport.addSeparator(buffer);
                CSVSupport.writeCell(value.editedDate.toString(), buffer);
                CSVSupport.addLineEnding(buffer);
            }
        }
        catch (IOException e) {
            interaction.reportProblem("File I/O Error", e.getMessage());
            return false;
        }
        finally {
            try {
                if (buffer != null) {
                    buffer.close();
                }
                if (fw != null) {
                    fw.close();
                }
            }
            catch (IOException e) {
                interaction.reportProblem("File I/O Error on Close",
                                          e.getMessage());
                return false;
            }
        }

        interaction.setStatusMessage("Export successful");
        return true;
    }

    protected static double parseDouble(String simil)
    {
        try {
            return Double.parseDouble(simil);
        }
        catch (NumberFormatException ex) {
            return ITermSimilarity.UNKNOWN;
        }
    }

    /**
     * Replace prevDict with dict after reading the entries from the file.
     * @param csvFile TODO
     */
    // I (dfm) believe the above is a lie. I think prevDict was already replaced 
    // by dict by the caller, and prevDict is simply a vestigal bit of data that
    // is passed around and encapsulated in undo and redo forms for no reason
    // at all. But I'm not sufficiently confident of that diagnosis to zap it
    // just yet....
    public static boolean importDictionary(final Design design,
                                           final ISimilarityDictionary dict,
                                           final ISimilarityDictionary prevDict,
                                           Interaction interaction,
                                           IUndoableEditSequence editSeq, 
                                           String csvFile)
    {
        File dataFile = (csvFile != null ? new File(csvFile) : interaction.selectCSVFile()); 

        if (dataFile == null) {
            return false;
        }

        FileReader reader = null;
        List<String> outputLines = new ArrayList<String>();

        try {
            reader = new FileReader(dataFile);
            FileUtil.readLines(reader, outputLines);
        }
        catch (IOException e) {
            interaction.reportProblem("File I/O Error", e.getMessage());
            return false;
        }
        finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            }
            catch (IOException e) {
                interaction.reportProblem("File I/O Error on Close",
                                               e.getMessage());
                return false;
            }
        }

        if (outputLines.size() == 0) {
            interaction.reportProblem("Empty file",
                                      "Requested .csv file is empty");
            return false;
        }

        if (outputLines.size() < 3) {
            interaction.reportProblem("Invalid file",
                                      "Requested file is not a valid dictionary file");
            return false;
        }

        String[] headerCol = CSVSupport.getCells(outputLines.get(0));

        if ((headerCol.length != 2) ||
            ! (ISimilarityDictionary.COGTOOL_DICT_HEADER.equals(headerCol[0])) ||
            ! headerCol[1].equals(DICT_CSV_VERSION_0))
        {
            interaction.reportProblem("Invalid file",
                                      "Requested file is not a valid dictionary file");
            return false;
        }

        final Map<DictEntry, DictValue> newEntries =
        	new LinkedHashMap<DictEntry, DictValue>();

        for (int i = 2; i < outputLines.size(); i++) {
            String newLine = outputLines.get(i);
            String[] cols = CSVSupport.getCells(newLine);
            // [required] 0 is goal string
            // [required] 1 is search string
            // [required] 2 is algorithm name
            // [optional] 3 is site (for Google algs) or url (for LSA)
            // [optional] 4 is term space (for LSA)
            String site = (cols.length > 3) ? cols[3] : "";
            String space = (cols.length > 4) ? cols[4] : "";

            DictEntry entry =
                new DictEntry(cols[0],
                              cols[1],
                              getNameAlgorithm(cols[2], site, space));
            double similarity;
            Date editedDate;

            try {
                similarity = parseDouble(cols[4]);
                editedDate =
                    new PersistentDate(PersistentDate.DATE_FORMAT.parse(cols[5]));
            }
            catch (Exception e)
            {
                throw new RcvrParsingException("Parsing error", e);
            }

            DictValue value = new DictValue(similarity, editedDate);

            newEntries.put(entry, value);
        }

        dict.insertEntries(newEntries, false);

        if (editSeq != null) {
            editSeq.addEdit(new AUndoableEdit(ProjectLID.ImportDict) {
                @Override
                public String getPresentationName()
                {
                    return IMPORT_DICTIONARY;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    if (! NullSafe.equals(prevDict, WidgetAttributes.NO_DICTIONARY)) {
                        design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, dict);
                    }
                    else {
                        dict.insertEntries(newEntries, false);
                    }
                }

                @Override
                public void undo()
                {
                    super.undo();

                    if (! NullSafe.equals(prevDict, WidgetAttributes.NO_DICTIONARY)) {
                        design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, prevDict);
                    }
                    else {
                        dict.removeEntries(newEntries);
                    }
                }
            });
        }
        if (interaction != null) {
            interaction.setStatusMessage("Import successful");
        }
        return true;
    }
}
