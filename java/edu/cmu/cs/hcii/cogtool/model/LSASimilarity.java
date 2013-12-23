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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.builder.HashCodeBuilder;

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

public class LSASimilarity extends CachedTermSimilarity
{
    public static final int edu_cmu_cs_hcii_cogtool_model_LSASimilarity_version = 1;

    protected static final String spaceVAR = "space";
    protected static final String urlVAR = "url";

    private static ObjectSaver.IDataSaver<LSASimilarity> SAVER =
        new ObjectSaver.ADataSaver<LSASimilarity>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_LSASimilarity_version;
            }

            @Override
            public void saveData(LSASimilarity value, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(value.space, spaceVAR);
                saver.saveObject(value.url, urlVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(LSASimilarity.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<LSASimilarity> LOADER =
        new ObjectLoader.AObjectLoader<LSASimilarity>() {
            @Override
            public LSASimilarity createObject()
            {
                return new LSASimilarity();
            }

            @Override
            public void set(LSASimilarity target,
                            String variable,
                            Object value)
            {
                if (variable != null) {
                    if (variable.equals(spaceVAR)) {
                        target.space = (String) value;
                    }
                    else if (variable.equals(urlVAR)) {
                        target.url = (String) value;
                    }
                }
            }
        };

    private static ObjectLoader.IObjectLoader<LSASimilarity> LOADER_v0 =
        new ObjectLoader.AObjectLoader<LSASimilarity>() {
            @Override
            public LSASimilarity createObject()
            {
                return new LSASimilarity();
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(LSASimilarity.class.getName(),
                                    0,
                                    LOADER_v0);
        ObjectLoader.registerLoader(LSASimilarity.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_LSASimilarity_version,
                                    LOADER);
    }

    /**
     * Default space
     */
    public static final String DEFAULT_SPACE =
        "General_Reading_Up_to_1st_Year_College";

    /**
     * Known "spaces"
     */
    public static final String[] KNOWN_SPACES =
    { DEFAULT_SPACE,
      "General_Reading_Up_to_12th_Grade",
      "General_Reading_Up_to_9th_Grade",
      "General_Reading_Up_to_6th_Grade",
      "General_Reading_Up_to_3rd_Grade",
      "French-Monde-1993",
      "CDUskills_v1",
      "ExpertPilot_v1",
      "CDUskills_v2",
      "CDUskills_v3",
      "CDUskills_v4",
      "CDUskills_v5",
      "ExpertPilot_v2",
      "ExpertPilot_v3",
      "ExpertPilot_v4",
      "ExpertPilot_v5",
      "CDUskills",
      "ExpertPilot"
    };

    /**
     * If url is null, then use the following constant.
     */
    public final static String DEFAULT_LSA_URL =
        "http://autocww2.colorado.edu/cgi-bin/nph-elaborate.cgi?Frequency=50&Cosine=0.5&";

        //"http://aviationknowledge.colorado.edu/cgi-bin/nph-elaborate.cgi?Frequency=50&Cosine=0.5&";

    protected String space = DEFAULT_SPACE;
    protected String url = null;

    @Override
    protected IWordFrequencyParser getWordFreqParser(String word,
                                                     List<String> errors)
    {
        // don't need a word parser for this algorithm
        return null;
    }

    // TODO we need to make the LSA parsing less fragile in the face of
    //      changes to the format returned
    private static Pattern NEW_SERVER_PATTERN =
        Pattern.compile("<TD>\\s*Cosine\\s*</TD>.*<TD>\\s*(-?\\d+\\.\\d+)\\s*\\($|<BR>\\s*</TD>\\)");
    private static Pattern ALTERNATIVE_RESULT_PATTERN =
        Pattern.compile("</TD>\\s*<TD>\\s*(-?\\d+\\.\\d+)\\s*$");

    protected class ProcessGoalSimilarity extends FetchURLUtil.AURLProcessor
                                          implements ISimilarityParser
    {
        protected String goalWord = null;
        protected String searchWord = null;

        protected String urlSafeGoal = null;
        protected String urlSafeSearch = null;

        protected double similarity = UNKNOWN;

        public String getURL()
        {
            String result =
                ((url != null) ? url : DEFAULT_LSA_URL)
                  + "Space=" + space
                  + "&Goal=" + urlSafeGoal
                  + "&Links=" + urlSafeSearch;
            return result;
        }

        /**
         * Possible responses (tokens separated by white space):
         *
         * BOTH WORDS UNKNOWN:
         * or
         * ONE WORD UNKNOWN:
         *    The string "Can't find any terms from text :" in the source
         *
         * BOTH WORDS KNOWN:
         *    Similarity value appears on a line by itself in a table
         *    following the line "</TD> <TD>"
         *
         * ALSO NOW:
         *    for a new instance of the server there's a new HTML format
         *    returned so there's a different possibility, too; sigh
         *
         */
        public boolean process(BufferedReader rdr)
        {
            List<String> errors = getErrors();

            try {
                String inputLine;
                boolean parseNext = false;
                while ((inputLine = rdr.readLine()) != null) {
                    if (inputLine.indexOf("Can't find any terms from text :") != -1)
                    {
                        // Similarity is unknown
                        return true;
                    }

                    Matcher m = NEW_SERVER_PATTERN.matcher(inputLine);
                    if (m.find()) {
                        similarity = Double.parseDouble(m.group(1));
                        return true;
                    }

                    if (parseNext) {
                        similarity = Double.parseDouble(inputLine);
                        return true;
                    }
                    
                    m = ALTERNATIVE_RESULT_PATTERN.matcher(inputLine);
                    if (m.find()) {
                        similarity = Double.parseDouble(m.group(1));
                        return true;
                    }

                    if ("</TD> <TD>".equals(inputLine)) {
                        parseNext = true;
                    }
                }
            }
            catch (IOException ex) {
                ex.printStackTrace();
                if (errors != null) {
                    errors.add("Reader readLine error trying to fetch from LSA:GetSimilarity for: "
                               + goalWord + ", "
                               + searchWord);
                }
            }
            catch (NumberFormatException ex) {
                ex.printStackTrace();
                if (errors != null) {
                    errors.add("Reader readLine error trying to parse the similarity "
                               + ex);
                }
            }

            return false;
        } // process

        public void reset(String goal, String search, List<String> errors)
        {
            reset(goal, search, errors, retryCount);
        }

        /**
         * To allow an instance of this processor to act as a "singleton",
         * the code that wishes to fetch a word-pair similarity using LSA@PARC
         * should reset the parameters used for the fetch.
         *
         * The given words will be used in the URL to LSA@PARC.
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

    protected ProcessGoalSimilarity goalSimilarityParser = null;

    protected LSASimilarity()
    {
        // For create and loading
    }

    protected static LSASimilarity checkAlg = null;

    public static LSASimilarity create()
    {
        return create(null, null);
    }

    public static LSASimilarity create(String useSpace)
    {
        return create(useSpace, null);
    }

    public static LSASimilarity create(String useSpace, String useURL)
    {
        if ((useSpace == null) || "".equals(useSpace)) {
            useSpace = DEFAULT_SPACE;
        }

        if ("".equals(useURL) || DEFAULT_LSA_URL.equals(useURL)) {
            useURL = null;
        }

        if (checkAlg == null) {
            checkAlg = new LSASimilarity();
        }

        checkAlg.space = useSpace;
        checkAlg.url = useURL;

        LSASimilarity registered =
            (LSASimilarity) AlgorithmRegistry.ONLY.register(checkAlg);

        // If null, then the registry used checkAlg and registered it.
        if (registered == null) {
            registered = checkAlg;
            checkAlg = null;
        }

        // registered now contains the algorithm to use
        return registered;
    }

    @Override
    protected ISimilarityParser getSimilarityParser(String goal,
                                                    String search,
                                                    List<String> errors)
    {
        if (goalSimilarityParser == null) {
            goalSimilarityParser = new ProcessGoalSimilarity();
        }

        goalSimilarityParser.reset(goal, search, errors);

        return goalSimilarityParser;
    }

    @Override
    public double determineSimilarity(String goal,
                                      String search,
                                      List<String> errors, 
                                      ITermSimilarity.Continuable cont)
    {
        return getWordSimilarity(goal, search, errors);
    }

    public String getSpace()
    {
        return space;
    }

    public void setSpace(String newSpace)
    {
        if ((newSpace == null) || "".equals(newSpace)) {
            newSpace = DEFAULT_SPACE;
        }

        space = newSpace;
    }

    // Default URL may change from when the object was saved.
    public String getURL()
    {
        return (url == null) ? DEFAULT_LSA_URL : url;
    }

    public void setURL(String newURL)
    {
        if ("".equals(newURL) || DEFAULT_LSA_URL.equals(newURL)) {
            newURL = null;
        }

        url = newURL;
    }

    @Override
    public boolean equals(Object other)
    {
        if (other instanceof LSASimilarity) {
            LSASimilarity otherLSA = (LSASimilarity) other;

            if (space.equals(otherLSA.space)) {
                if (url == null) {
                    return otherLSA.url == null;
                }

                return url.equals(otherLSA.url);
            }
        }

        return false;
    }

    @Override
    public int hashCode()
    {
        // Must have a unique ODD number for each class which uses
        // hashCodeBuilder.
        // this   : 531, 447
        HashCodeBuilder hcb = new HashCodeBuilder(531, 447);

        hcb.append(space.hashCode());

        if (url != null) {
            hcb.append(url.hashCode());
        }

        return hcb.toHashCode();
    }

    public ITermSimilarity duplicate()
    {
        return this;
    }
}
