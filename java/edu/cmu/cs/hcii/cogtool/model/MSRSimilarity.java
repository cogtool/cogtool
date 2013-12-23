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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.FetchURLUtil;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

//The names of classes and interfaces around this are terrible, but we can't
//change them without breaking old .cgt files, since our persistence
//mechanism leaks the implementation detail of our class names into the
//abstraction of our file format.
//Note that the only implementer of this interface is the abstract calls
//CachedTermSimilarity. All concrete classes are subclasses of that, and there
//is no such thing as an un-cached TermSimilarity.
//Both GoogleSimilarity and CachedGoogleSimilarity are direct subsclasses
//of CachedTermSimilarity, and the one with cached in its name does not
//inherit from the one without. All very confusing.
//TODO once we augment our persistence mechanism in such a way that we
//   can actually rename persistable classes, we should tidy up these names.

/**
 * Corresponds to computeMSRSimilarity in Leonghwee's code.
 */
public class MSRSimilarity extends CachedTermSimilarity
{
    public static MSRSimilarity ONLY = new MSRSimilarity();

    public static final int edu_cmu_cs_hcii_cogtool_model_MSRSimilarity_version = 0;

    private static ObjectSaver.IDataSaver<MSRSimilarity> SAVER =
        new ObjectSaver.ADataSaver<MSRSimilarity>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_MSRSimilarity_version;
            }

            @Override
            public void saveData(MSRSimilarity v, ObjectSaver saver)
            {
                // Nothing to save; it's an ONLY!
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(MSRSimilarity.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<MSRSimilarity> LOADER =
        new ObjectLoader.AObjectLoader<MSRSimilarity>() {
            @Override
            public MSRSimilarity createObject()
            {
                return ONLY;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(MSRSimilarity.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_MSRSimilarity_version,
                                    LOADER);
    }

    public MSRSimilarity()
    {
    }

    @Override
    protected IWordFrequencyParser getWordFreqParser(String word, List<String> errors)
    {
        GoogleSimilarity.termFreqParser.reset(word, errors);

        return GoogleSimilarity.termFreqParser;
    } // getWordFreqParser

    protected static class ProcessGoalSimilarity
                                             extends FetchURLUtil.AURLProcessor
                                             implements ISimilarityParser
    {
        protected static final String FETCH_SIMILARITY_URL =
            "http://cwl-projects.cogsci.rpi.edu/cgi-bin/msr/msr.cgi?msr=PMI-G&t1=";

        protected String goalWord = null;
        protected String searchWord = null;

        protected String urlSafeGoal = null;
        protected String urlSafeSearch = null;

        protected double similarity = UNKNOWN;

        public String getURL()
        {
            return FETCH_SIMILARITY_URL + urlSafeGoal
                             + "&t2=" + urlSafeSearch;
        }

        /**
         * Possible responses (tokens separated by white space):
         *
         * ONE OR BOTH WORDS UNKNOWN:
         *    One line, "ERROR"
         *
         * BOTH WORDS KNOWN:
         *    One line with the similarity value
         */
        public boolean process(BufferedReader rdr)
        {
            List<String> errors = getErrors();
            String inputLine;

            try {
                inputLine = rdr.readLine();
            }
            catch (IOException ex) {
                if (errors != null) {
                    errors.add("Reader readLine error trying to fetch from MSR:GetSimilarity for: "
                                + goalWord + ", "
                                + searchWord);
                }

                return false;
            }

            if (inputLine != null) {
                if (inputLine.startsWith("You are not authorized to access this script from IP "))
                {
                    errors.add("Not authorized to use MSR:GetSimilarity for: "
                                    + goalWord + ", "
                                    + searchWord);

                    return false;
                }

                if (! inputLine.equals("ERROR")) {
                    similarity = Double.parseDouble(inputLine);
                }

                return true;
            }

            if (errors != null) {
                errors.add("Unexpected (empty) reply from MSR:GetSimilarity for: "
                                + goalWord + ", "
                                + searchWord);
            }

            return false;
        } // process

        public void reset(String goal, String search, List<String> errors)
        {
            reset(goal, search, errors, retryCount);
        }

        /**
         * To allow an instance of this processor to act as a "singleton",
         * the code that wishes to fetch a word-pair similarity using MSR
         * should reset the parameters used for the fetch.
         *
         * The given words will be used in the URL to MSR.
         */
        public void reset(String goal,
                          String search,
                          List<String> errors,
                          int useRetryCount)
        {
            goalWord = goal;
            searchWord = search;

            similarity = UNKNOWN;

            try {
                urlSafeGoal = URLEncoder.encode(goalWord, "UTF-8");
            }
            catch (UnsupportedEncodingException ex) {
                throw new IllegalStateException("Encoding failed for word: "
                                                    + goalWord);
            }

            try {
                urlSafeSearch =
                    URLEncoder.encode(searchWord, "UTF-8");
            }
            catch (UnsupportedEncodingException ex) {
                throw new IllegalStateException("Encoding failed for word: "
                                                    + searchWord);
            }

            reset(errors, useRetryCount);
        }

        /**
         * Return the goal word last processed (or attempted).
         */
        public String getGoalWord()
        {
            return goalWord;
        }

        /**
         * Return the search word last processed (or attempted).
         */
        public String getSearchWord()
        {
            return searchWord;
        }

        /**
         * Return the similarity last fetched.
         */
        public double getSimilarity()
        {
            return similarity;
        }
    }

    protected static ProcessGoalSimilarity goalSimilarityParser =
        new ProcessGoalSimilarity();

    @Override
    protected ISimilarityParser getSimilarityParser(String goalWord,
                                                    String searchWord,
                                                    List<String> errors)
    {
        goalSimilarityParser.reset(goalWord, searchWord, errors);

        return goalSimilarityParser;
    }

    public ITermSimilarity duplicate()
    {
        return this;
    }
}
