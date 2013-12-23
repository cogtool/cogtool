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

import java.util.Date;
import java.util.EventObject;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.model.CachedTermSimilarity.WordPair;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.PersistentDate;

public interface ISimilarityDictionary extends IAttributed
{
    public static final String COGTOOL_DICT_HEADER = "CTSD";

    public static class DictEntry extends WordPair
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictEntry_version = 0;

        protected static final String algorithmVAR = "algorithm";

        private static ObjectSaver.IDataSaver<DictEntry> SAVER =
            new ObjectSaver.ADataSaver<DictEntry>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictEntry_version;
                }

                @Override
                public void saveData(DictEntry v, ObjectSaver saver)
                    throws java.io.IOException
                {
                    saver.saveObject(v.algorithm, algorithmVAR);
                }
            };

        public static void registerSaver()
        {
            WordPair.registerSaver();
            ObjectSaver.registerSaver(DictEntry.class.getName(), SAVER);
        }

        private static ObjectLoader.IObjectLoader<DictEntry> LOADER =
            new ObjectLoader.AObjectLoader<DictEntry>() {
                @Override
                public DictEntry createObject()
                {
                    return new DictEntry();
                }

                @Override
                public void set(DictEntry target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(algorithmVAR)) {
                            if (value == null) {
                                value = ITermSimilarity.MANUAL;
                            }

                            target.algorithm = (ITermSimilarity) value;
                        }
                    }
                }

                @Override
                public void evolve(DictEntry target)
                {
                    ITermSimilarity registered =
                        ITermSimilarity.AlgorithmRegistry.ONLY.register(target.algorithm);

                    // If null, then the registry accepted the entry's
                    // current algorithm.  If not null, then the registry had
                    // a version for this algorithm's parameters, so use it
                    // instead.
                    if (registered != null) {
                        target.algorithm = registered;
                    }
                }
            };

        public static void registerLoader()
        {
            WordPair.registerLoader();
            ObjectLoader.registerLoader(DictEntry.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictEntry_version,
                                        LOADER);
        }

        public ITermSimilarity algorithm;

        protected DictEntry() { } // for loading

        public DictEntry(String goal,
                         String search,
                         ITermSimilarity alg)
        {
            super(goal, search);

            algorithm = alg;
        }

        protected boolean valueEquals(DictEntry other)
        {
            return super.valueEquals(other) &&
                   NullSafe.equals(algorithm, other.algorithm);
        }

        @Override
        public boolean equals(Object other)
        {
            return (other != null) &&
                   (other.getClass() == DictEntry.class) &&
                   valueEquals((DictEntry) other);
        }

        @Override
        public int hashCode()
        {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this: 173, 215
            HashCodeBuilder hcb = new HashCodeBuilder(173, 215);
            hcb.append(goalWord.hashCode());
            hcb.append(searchWord.hashCode());

            if (algorithm != null) {
                hcb.append(algorithm.hashCode());
            }

            return hcb.toHashCode();
        }
    }

    public static class DictValue
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictValue_version = 0;

        protected static final String similarityVAR = "similarity";
        protected static final String dateVAR = "date";

        private static ObjectSaver.IDataSaver<DictValue> SAVER =
            new ObjectSaver.ADataSaver<DictValue>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictValue_version;
                }

                @Override
                public void saveData(DictValue v, ObjectSaver saver)
                    throws java.io.IOException
                {
                    saver.saveDouble(v.similarity, similarityVAR);
                    saver.saveObject(v.editedDate, dateVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(DictValue.class.getName(), SAVER);
        }

        private static ObjectLoader.IObjectLoader<DictValue> LOADER =
            new ObjectLoader.AObjectLoader<DictValue>() {
                @Override
                public DictValue createObject()
                {
                    return new DictValue();
                }

                @Override
                public void set(DictValue target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(dateVAR)) {
                            target.editedDate = (Date) value;
                        }
                    }
                }

                @Override
                public void set(DictValue target, String variable, double value)
                {
                    if (variable != null) {
                        if (variable.equals(similarityVAR)) {
                            target.similarity = value;
                        }
                    }
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(DictValue.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_ISimilarityDictionary$DictValue_version,
                                        LOADER);
        }

        public double similarity;
        public Date editedDate;

        protected DictValue() { } // for loading

        public DictValue(double simil, Date date)
        {
            super();
            similarity = simil;
            editedDate = date;
        }

        public DictValue(double simil)
        {
            this(simil, new PersistentDate());
        }
    }

    public static class EntryChange extends EventObject
    {
        public DictValue value;
        public int rowIndex;

        public EntryChange(ISimilarityDictionary dict,
                           DictValue v,
                           int index)
        {
            super(dict);
            value = v;
            rowIndex = index;
        }
    }

    public static class DictionaryChange extends EventObject
    {
        public boolean isAdd;
        public int rowIndex;

        public DictionaryChange(ISimilarityDictionary dict,
                                boolean add,
                                int index)
        {
            super(dict);
            isAdd = add;
            rowIndex = index;
        }
    }

    public static final ITermSimilarity DEFAULT_ALG = LSASimilarity.create();

    public ITermSimilarity getCurrentAlgorithm();
    public void setCurrentAlgorithm(ITermSimilarity alg);

    public void setSimilarity(String goalTerm,
                              String searchTerm,
                              ITermSimilarity algorithm,
                              DictValue value,
                              int rowIndex);

    public void updateEntry(int rowIndex,
                            String goal,
                            String search);

    public List<DictEntry> getEntries();

    public DictEntry getEntry(int index);

    public DictValue getValue(DictEntry entry);

    public void removeEntry(int index);

    public void insertEntry(DictEntry entry, DictValue value, int index);

    public void insertEntries(Map<DictEntry, DictValue> entries, boolean recompute);
    public void removeEntries(Map<DictEntry, DictValue> entries);

    public boolean containsEntry(DictEntry entry);

    public int size();

    public ISimilarityDictionary duplicate();

    /**
     * Returns the set of ITermSimilarity objects that were used to generate
     * entries in this dictionary.
     */
    public Set<ITermSimilarity> getAlgorithmsInUse();
}