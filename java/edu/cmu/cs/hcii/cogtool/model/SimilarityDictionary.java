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

package edu.cmu.cs.hcii.cogtool.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class SimilarityDictionary extends GlobalAttributed
                                  implements ISimilarityDictionary
{
    public static final int edu_cmu_cs_hcii_cogtool_model_SimilarityDictionary_version = 0;

    protected static final String similaritiesVAR = "values";
    protected static final String entriesVAR = "entries";
    protected static final String similarityAlgVAR = "similarityAlg";

    private static ObjectSaver.IDataSaver<SimilarityDictionary> SAVER =
        new ObjectSaver.ADataSaver<SimilarityDictionary>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_SimilarityDictionary_version;
            }

            @Override
            public void saveData(SimilarityDictionary v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.similarityAlg, similarityAlgVAR);
                saver.saveObject(v.dictMap, similaritiesVAR);
                saver.saveObject(v.dictEntries, entriesVAR);
            }
        };

    public static void registerSaver()
    {
        DictEntry.registerSaver();
        DictValue.registerSaver();
        ObjectSaver.registerSaver(SimilarityDictionary.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<GlobalAttributed> GLOBAL_ATTR_LOADER =
        new GlobalAttributedLoader() {
            @Override
            protected void setAttribute(GlobalAttributed v,
                                        String prefixedAttribute,
                                        Object value)
            {
                if ((v instanceof ISimilarityDictionary) &&
                    (attributePREFIX + "DesignType.currentAlg").equals(prefixedAttribute))
                {
                    ISimilarityDictionary d = (ISimilarityDictionary) v;

                    if (value == null) {
                        value = ITermSimilarity.MANUAL;
                    }

                    d.setCurrentAlgorithm((ITermSimilarity) value);
                }
                else {
                    super.setAttribute(v, prefixedAttribute, value);
                }
            }
        };

    private static ObjectLoader.IObjectLoader<SimilarityDictionary> LOADER =
        new ObjectLoader.AObjectLoader<SimilarityDictionary>() {
            @Override
            public SimilarityDictionary createObject()
            {
                return new SimilarityDictionary();
            }

            @Override
            public void set(SimilarityDictionary target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(similarityAlgVAR)) {
                        target.similarityAlg = (ITermSimilarity) value;
                    }
                }
            }

            @Override
            public Collection<?> createCollection(SimilarityDictionary target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(entriesVAR)) {
                        return target.dictEntries;
                    }
                }

                return null;
            }

            @Override
            public Map<?, ?> createMap(SimilarityDictionary target,
                                       String variable,
                                       int size)
            {
                if (variable != null) {
                    if (variable.equals(similaritiesVAR)) {
                        return target.dictMap;
                    }
                }

                return null;
            }
        };

    public static void registerLoader()
    {
        DictEntry.registerLoader();
        DictValue.registerLoader();
        ObjectLoader.registerLoader(SimilarityDictionary.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_SimilarityDictionary_version,
                                    LOADER);

        ObjectLoader.registerLoader(GlobalAttributed.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_GlobalAttributed_version,
                                    GLOBAL_ATTR_LOADER);
    }

    protected Map<DictEntry, DictValue> dictMap =
        new HashMap<DictEntry, DictValue>();

    /**
     * A list of entries so that they can be accessed by row index
     */
    protected List<DictEntry> dictEntries = new ArrayList<DictEntry>();

    /**
     * Current algorithm to use for computing similarity values for entries
     */
    protected ITermSimilarity similarityAlg = DEFAULT_ALG;

    public SimilarityDictionary()
    {
        super();
    }

    public ITermSimilarity getCurrentAlgorithm()
    {
        return similarityAlg;
    }

    public void setCurrentAlgorithm(ITermSimilarity alg)
    {
        similarityAlg = alg;
    }

    public void setSimilarity(String goalTerm,
                              String searchTerm,
                              ITermSimilarity algorithm,
                              DictValue value,
                              int rowIndex)
    {
        if (rowIndex < dictEntries.size()) {
            DictEntry entry = dictEntries.get(rowIndex);
            DictValue curValue = dictMap.get(entry);
            curValue.similarity = value.similarity;
            curValue.editedDate = value.editedDate;

            if (entry != null) {
                updateEntry(entry, rowIndex, goalTerm, searchTerm, algorithm,
                            curValue);
            }
        }
        else {
            DictEntry newEntry =
                new DictEntry(goalTerm, searchTerm, algorithm);

            insertEntry(newEntry, value, rowIndex);
        }
    }

    public List<DictEntry> getEntries()
    {
        return dictEntries;
    }

    public DictEntry getEntry(int index)
    {
        return (dictEntries.size() > index)
            ? dictEntries.get(index)
            : null;
    }

    protected void updateEntry(DictEntry entry,
                               int rowIndex,
                               String goal,
                               String search,
                               ITermSimilarity algorithm,
                               DictValue value)
    {
        dictMap.remove(entry);

        entry.goalWord = goal;
        entry.searchWord = search;
        entry.algorithm = algorithm;

        dictMap.put(entry, value);

        raiseAlert(new EntryChange(this, value, rowIndex));
    }

    public void updateEntry(int rowIndex,
                            String goal,
                            String search)
    {
        if (dictEntries.size() > rowIndex) {
            DictEntry entry = dictEntries.get(rowIndex);

            if (entry != null) {
                DictValue value = dictMap.get(entry);

                updateEntry(entry,
                            rowIndex,
                            goal,
                            search,
                            entry.algorithm,
                            value);
            }
        }
    }

    public DictValue getValue(DictEntry entry)
    {
        return dictMap.get(entry);
    }

    public void removeEntry(DictEntry entry)
    {
        if (dictEntries.contains(entry)) {
            int index = dictEntries.indexOf(entry);
            dictEntries.remove(entry);
            dictMap.remove(entry);

            raiseAlert(new DictionaryChange(this, false, index));
        }
    }

    public void removeEntry(int index)
    {
        if (dictEntries.size() > index) {
            dictMap.remove(dictEntries.remove(index));

            raiseAlert(new DictionaryChange(this, false, index));
        }
    }

    public void insertEntry(DictEntry entry, DictValue value, int index)
    {
        if (! containsEntry(entry)) {
            dictEntries.add(index, entry);

            dictMap.put(entry, value);

            raiseAlert(new DictionaryChange(this, true, index));
        }
    }

    public void insertEntries(Map<DictEntry, DictValue> entries,
                              boolean recompute)
    {
        Iterator<Map.Entry<DictEntry, DictValue>> iter =
            entries.entrySet().iterator();

        while (iter.hasNext()) {
            Map.Entry<DictEntry, DictValue> mapEntry = iter.next();
            DictEntry entry = mapEntry.getKey();
            DictValue value = mapEntry.getValue();

            if (containsEntry(entry)) {
                if (recompute) {
                    setSimilarity(entry.goalWord,
                                  entry.searchWord,
                                  entry.algorithm,
                                  value,
                                  dictEntries.indexOf(entry));
                }
            }
            else {
                insertEntry(entry, value, dictMap.size());
            }
        }
    }

    public void removeEntries(Map<DictEntry, DictValue> entries)
    {
        Iterator<DictEntry> iter = entries.keySet().iterator();

        while (iter.hasNext()) {
            DictEntry entry = iter.next();

            if (containsEntry(entry)) {
                int index = dictEntries.indexOf(entry);
                removeEntry(index);
            }
        }
    }

    public boolean containsEntry(DictEntry entry)
    {
        return dictMap.containsKey(entry);
    }

    public int size()
    {
        return dictMap.size();
    }

    /**
     * Returns the set of ITermSimilarity objects that were used to generate
     * entries in this dictionary.
     */
    public Set<ITermSimilarity> getAlgorithmsInUse()
    {
        Set<ITermSimilarity> result = new HashSet<ITermSimilarity>();
        Iterator<DictEntry> values = getEntries().iterator();

        while (values.hasNext()) {
            DictEntry entry = values.next();
            result.add(entry.algorithm);
        }

        return result;
    }

    public ISimilarityDictionary duplicate()
    {
        ISimilarityDictionary newDict = new SimilarityDictionary();

        newDict.setCurrentAlgorithm(similarityAlg.duplicate());

        Iterator<DictEntry> values = getEntries().iterator();

        while (values.hasNext()) {
            DictEntry entry = values.next();
            DictValue value = getValue(entry);
            int index = dictEntries.indexOf(entry);
            // algorithm should never be null
            ITermSimilarity newAlg = entry.algorithm.duplicate();

            DictEntry newEntry = new DictEntry(entry.goalWord,
                                               entry.searchWord,
                                               newAlg);
            DictValue newValue = new DictValue(value.similarity,
                                               value.editedDate);

            newDict.insertEntry(newEntry, newValue, index);
        }

        newDict.copyAttributes(this);

        return newDict;
    }
}