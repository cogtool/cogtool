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

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.util.FetchURLUtil;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

//The names of classes and interfaces around this are terrible, but we can't
//change them without breaking old .cgt files, since our persistence
//mechanism leaks the implementation detail of our class names into the
//abstraction of our file format.
//Note that there
//is no such thing as an un-cached TermSimilarity.
//Both GoogleSimilarity and CachedGoogleSimilarity are direct subsclasses
//of CachedTermSimilarity, and the one with cached in its name does not
//inherit from the one without. All very confusing.
//TODO once we augment our persistence mechanism in such a way that we
//   can actually rename persistable classes, we should tidy up these names.

/**
 * Corresponds to computeGEntireSimilarity in Leonghwee's code.
 * But in the UI it's called PMI-G (phrase).
 */
public class GoogleSimilarity extends CachedTermSimilarity
                              implements ISitedTermSimilarity
{
    public static final int edu_cmu_cs_hcii_cogtool_model_GoogleSimilarity_version = 0;

    protected static final String contextSiteVAR = "contextSite";

    private static ObjectSaver.IDataSaver<GoogleSimilarity> SAVER =
        new ObjectSaver.ADataSaver<GoogleSimilarity>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_GoogleSimilarity_version;
            }

            @Override
            public void saveData(GoogleSimilarity v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.site, contextSiteVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(GoogleSimilarity.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<GoogleSimilarity> LOADER =
        new ObjectLoader.AObjectLoader<GoogleSimilarity>() {
            @Override
            public GoogleSimilarity createObject()
            {
                return new GoogleSimilarity();
            }

            @Override
            public void set(GoogleSimilarity target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(contextSiteVAR)) {
                        target.site = (String) value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(GoogleSimilarity.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_GoogleSimilarity_version,
                                    LOADER);
    }

    protected String site = null;

    protected static GoogleSimilarity checkAlg = null;

    public static GoogleSimilarity create(String limitToSite)
    {
        if (checkAlg == null) {
            checkAlg = new GoogleSimilarity();
        }

        checkAlg.resetSite(limitToSite);

        GoogleSimilarity registered =
            (GoogleSimilarity) AlgorithmRegistry.ONLY.register(checkAlg);

        // If null, then the registry used checkAlg and registered it.
        if (registered == null) {
            registered = checkAlg;
            checkAlg = null;
        }

        // registered now contains the algorithm to use
        return registered;
    }

    public GoogleSimilarity()
    {
    }

    public void resetSite(String limitToSite)
    {
        if ("".equals(limitToSite)) {
            limitToSite = null;
        }

        site = limitToSite;
    }

    /**
     * Works for terms (i.e., phrases) or words!
     */
    protected static class ProcessTermFrequency
                                             extends FetchURLUtil.AURLProcessor
                                             implements IWordFrequencyParser
    {
        private static final String FETCH_FREQUENCY_URL =
            "http://cogtool.hcii.cs.cmu.edu/cgi-bin/pmi-g-query?auth=none&q=";

        private String site = null;
        private String term = null;

        private StringBuilder urlSafeTerm = new StringBuilder();
        private long frequency = 0;

        public String getURL()
        {
            if (site == null) {
                return FETCH_FREQUENCY_URL + urlSafeTerm.toString();
            }

            return FETCH_FREQUENCY_URL + urlSafeTerm.toString()
                                       + "+site%3A" + site;
        }

        public boolean process(BufferedReader rdr)
        {
            String inputLine = null;
            try {
                inputLine = rdr.readLine();
                if (inputLine != null) {
                    inputLine = inputLine.trim();
                    if (inputLine.equals("")) {
                        frequency = 0;
                    } else {
                        frequency = Long.parseLong(inputLine);
                    }
                    CogTool.logger.fine(String.format(
                          "Fetched frequency for %s, %s: %d",
                          term, site, frequency));
                    return true;
                }
            } catch (IOException ex) {
                // fall through
            } catch (NumberFormatException ex) {
                // fall through
            }
            String err = String.format(
                                       "Unexpected reply from PMI-G server: %s, %s.",
                                       term, inputLine);
            CogTool.logger.fine(err);
            List<String> errors = getErrors();
            if (errors != null) {
                errors.add(err);
            }
            return true;
        }

        public void reset(String forTerm, List<String> errors)
        {
            reset(forTerm, errors, null);
        }

        public void reset(String forTerm, List<String> errors, String site)
        {
            reset(forTerm, errors, site, retryCount);
        }

        /**
         * To allow an instance of this processor to act as a "singleton",
         * the code that wishes to fetch a frequency using Google should
         * reset the parameters used for the fetch.
         *
         * The given word will be used in the URL to Google.
         */
        public void reset(String forTerm,
                          List<String> errors,
                          String site,
                          int useRetryCount)
        {
            term = forTerm.trim();
            frequency = 0;

            urlSafeTerm.delete(0, urlSafeTerm.length());

            String[] words = term.split("\\s+");

            try {
                if ((words != null) && (words.length > 0)) {
                    urlSafeTerm.append("%2B");
                    urlSafeTerm.append(URLEncoder.encode(words[0],
                                                              "UTF-8"));

                    if (words.length > 1) {
                        for (int i = 1; i < words.length; i++) {
                            urlSafeTerm.append("+%2B");
                            urlSafeTerm.append(URLEncoder.encode(words[i],
                                                                      "UTF-8"));
                        }
                    }
                }
                else {
                    urlSafeTerm.append(URLEncoder.encode(("%2B" + term),
                                                              "UTF-8"));
                }
            }
            catch (UnsupportedEncodingException ex) {
                throw new IllegalStateException("Encoding failed for term: "
                                                        + term);
            }

            if ((site != null) && ! site.equals("")) {
                try {
                    this.site = URLEncoder.encode(site, "UTF-8");
                }
                catch (UnsupportedEncodingException ex) {
                    throw new IllegalStateException("Encoding failed for site: "
                                                            + site);
                }
            }
            else {
                this.site = null;
            }

            reset(errors, useRetryCount);
        }

        /**
         * Return the term last processed (or attempted).
         */
        public String getTerm()
        {
            return term;
        }

        // To avoid confusion when dealing with words instead of phrases,
        // a simple alias.
        public String getWord()
        {
            return getTerm();
        }

        /**
         * Return the frequency last fetched.
         */
        public long getTermFrequency()
        {
            return frequency;
        }

        // To avoid confusion when dealing with words instead of phrases,
        // a simple alias.
        public long getWordFrequency()
        {
            return getTermFrequency();
        }
    }

    public static ProcessTermFrequency termFreqParser =
        new ProcessTermFrequency();

    protected double getTermFrequency(String term, List<String> errors)
    {
        if (term == null) {
            return 0.0;
        }

        termFreqParser.reset(term, errors, site);

        if (FetchURLUtil.processURL(termFreqParser)) {
            return termFreqParser.getTermFrequency();
        }

        return 0.0;
    }

    /**
     * No need for a similarity URL processor; term similarity is computed
     * purely using phrase frequencies.
     */
    @Override
    protected Double fetchWordSimilarity(String goalTerm,
                                         String searchTerm,
                                         List<String> errors)
    {
        double numerator =
            getTermFrequency(goalTerm + " " + searchTerm, errors);
        double denominator = getTermFrequency(searchTerm, errors);

        if (denominator != 0.0) {
//            if (numerator > denominator) {
//                errors.add(String.format(
//                   "GoogleSimilarity confusion: %s; %s; %g; %g",
//                   goalTerm, searchTerm, numerator, denominator));
//                return UNKNOWN_SIMILARITY;
//            }
            return new Double(numerator / denominator);
        }

        return UNKNOWN_SIMILARITY;
    }

    /**
     * We re-use the facilities provided by CachedTermSimilarity (i.e.,
     * the cache data structures) where, instead of words, we use phrases
     * (i.e., terms).
     */
    @Override
    public double determineSimilarity(String goalTerm,
                                      String searchTerm,
                                      List<String> errors, 
                                      ITermSimilarity.Continuable cont)
    {
        return getWordSimilarity(goalTerm, searchTerm, errors);
    }

    @Override
    protected IWordFrequencyParser getWordFreqParser(String word,
                                                     List<String> errors)
    {
        termFreqParser.reset(word, errors);

        return termFreqParser;
    }

    @Override
    protected ISimilarityParser getSimilarityParser(String goal,
                                                    String search,
                                                    List<String> errors)
    {
        throw new UnsupportedOperationException("No similarity parser is required for Google for: "
                                                    + goal + ", "
                                                    + search);
    }

    public String getContextSite()
    {
        return site;
    }

    @Override
    public boolean equals(Object other)
    {
        if (other instanceof GoogleSimilarity) {
            GoogleSimilarity cgs = (GoogleSimilarity) other;

            if (site == null) {
                return cgs.site == null;
            }

            return site.equals(cgs.site);
        }

        return false;
    }

    @Override
    public int hashCode()
    {
        // Must have a unique ODD number for each class which uses
        // hashCodeBuilder.
        // this   : 181, 193
        HashCodeBuilder hcb = new HashCodeBuilder(181, 193);

        if (site != null) {
            hcb.append(site.hashCode());
        }

        return hcb.toHashCode();
    }

    public ITermSimilarity duplicate()
    {
        GoogleSimilarity gs = new GoogleSimilarity();
        gs.site = site;
        return gs;
    }
}
