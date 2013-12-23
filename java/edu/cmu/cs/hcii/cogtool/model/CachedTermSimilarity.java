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
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.FetchURLUtil;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
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
 * Implements a standard algorithm that determines a frequency count for each
 * word in term (caching the results), determines a relationship value for
 * pairs of words (caching the results), and, using those frequencies and
 * values, determines a similarity measure.
 */
public abstract class CachedTermSimilarity implements ITermSimilarity
{
    public static String toString(double similarity)
    {
        if (similarity == UNKNOWN) {
            return "Unrelated";
        }

        return Double.toString(similarity);
    }

    public static final Double UNKNOWN_SIMILARITY = new Double(UNKNOWN);

    /**
     * Standard value entry for frequencyTable if zero
     */
    public static final Long ZERO_FREQUENCY = new Long(0);

    // If words are cached, maps word String to frequency Long
    protected Map<String, Long> frequencyTable = new HashMap<String, Long>();

    // Tracks words and phrases not found in a particular corpus;
    // maps word/term to List of "replacement" words
    // TODO: Currently, there is no way to populate this table.
    protected LinkedHashMap<String, List<String>> zeroFreqTerms =
    	new LinkedHashMap<String, List<String>>();

    /**
     * A pair of strings treated as a struct (that is, comparison is
     * overridden so that instances don't require identity match to act
     * as keys in a Map).
     */
    public static class WordPair
    {
        public static final int edu_cmu_cs_hcii_cogtool_model_CachedTermSimilarity$WordPair_version = 0;

        protected static final String goalWordVAR = "goalWord";
        protected static final String searchWordVAR = "searchWord";

        private static ObjectSaver.IDataSaver<WordPair> SAVER =
            new ObjectSaver.ADataSaver<WordPair>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_CachedTermSimilarity$WordPair_version;
                }

                @Override
                public void saveData(WordPair v, ObjectSaver saver)
                    throws java.io.IOException
                {
                    saver.saveObject(v.goalWord, goalWordVAR);
                    saver.saveObject(v.searchWord, searchWordVAR);
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(WordPair.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<WordPair> LOADER =
            new ObjectLoader.AObjectLoader<WordPair>() {
                @Override
                public WordPair createObject()
                {
                    return new WordPair();
                }

                @Override
                public void set(WordPair target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(goalWordVAR)) {
                            target.goalWord = (String) value;
                        }
                        else if (variable.equals(searchWordVAR)) {
                            target.searchWord = (String) value;
                        }
                    }
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(WordPair.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_CachedTermSimilarity$WordPair_version,
                                        LOADER);
        }

        protected WordPair() { } // for loading

        public static final WordPair SEARCH_KEY = new WordPair("", "");

        public String goalWord;
        public String searchWord;

        public WordPair(String goal, String search)
        {
            goalWord = goal;
            searchWord = search;
        }

        protected boolean valueEquals(WordPair other)
        {
            return NullSafe.equals(goalWord, other.goalWord) &&
                   NullSafe.equals(searchWord, other.searchWord);
        }

        @Override
        public boolean equals(Object other)
        {
            return (other != null) &&
                   (other.getClass() == WordPair.class) &&
                   valueEquals((WordPair) other);
        }

        @Override
        public int hashCode()
        {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 47, 5
            return new HashCodeBuilder(47, 5).append(goalWord.hashCode())
                                             .append(searchWord.hashCode())
                                             .toHashCode();
        }
    }

    // Maps WordPair to similarity Double value
    protected Map<WordPair, Double> similarityTable =
    	new HashMap<WordPair, Double>();

    /**
     * An IURLProcessor that fetches a frequency count for a given word.
     */
    protected interface IWordFrequencyParser extends FetchURLUtil.IURLProcessor
    {
        public long getWordFrequency();
    }

    /**
     * Subclasses should implement this to return a URL processor that
     * fetches a frequency count for the given word.
     */
    protected abstract IWordFrequencyParser getWordFreqParser(String word,
                                                              List<String> errors);

    /**
     * At this point, the frequency cache does not know the given word,
     * so get the URL processor for the given word and process the
     * fetched content.
     */
    protected Long fetchWordFrequency(String word, List<String> errors)
    {
        IWordFrequencyParser wordFreqParser =
            getWordFreqParser(word, errors);

        if (FetchURLUtil.processURL(wordFreqParser)) {
            long frequency = wordFreqParser.getWordFrequency();

            // No need to create a new instance for zero.
            if (frequency == 0) {
                return ZERO_FREQUENCY;
            }

            return new Long(frequency);
        }

        // Did not succeed at fetching a value.
        return null;
    }

    /**
     * Look up the word in the cache; if there, return the associated
     * frequency.  If not there, fetch it.
     */
    protected long getWordFrequency(String word, List<String> errors)
    {
        if (word == null) {
            return 0;
        }

        word = word.toLowerCase();

        if (frequencyTable.containsKey(word)) {
            Object frequency = frequencyTable.get(word);

            if (frequency != null) {
                return ((Long) frequency).longValue();
            }

            throw new IllegalStateException("Frequency table contains a null frequency for word: "
                                                + word);
        }

        Long frequency = fetchWordFrequency(word, errors);

        if (frequency != null) {
            frequencyTable.put(word, frequency);

            return frequency.longValue();
        }

        return 0;
    } // getWordFrequency

    private static final Pattern SPLITTER = Pattern.compile("\\s+");
    
    /**
     * Break term into words, fetch each word's frequency, use replacements
     * if necessary (and specified), and return an array of words/replacements
     * that have non-zero frequencies.
     */
    private String[] getWordFrequencies(String term, 
                                        List<String> errors, 
                                        ITermSimilarity.Continuable cont)
    {
        String[] words = SPLITTER.split(term);
        List<String> nonzeroWords = new ArrayList<String>();

        for (String word : words) {
            if (getWordFrequency(word, errors) > 0) {
                nonzeroWords.add(word);
            }
            else if (zeroFreqTerms.containsKey(word)) {
                List<String> replacement = zeroFreqTerms.get(word);

                if (replacement != null) {
                    Iterator<String> others = replacement.iterator();

                    while (others.hasNext()) {
                        getWordFrequency(others.next(), errors);
                        if (! cont.isContinuing()) {
                            return null;
                        }
                    }

                    // TODO: Even if freq returned is zero in loop above?
                    nonzeroWords.addAll(replacement);
                }
                else {
                    // No replacements for previously seen zero freq word;
                    // TODO: return what???? "inform crawlWebsite() to reinsert link into queue"
                }
            }
            else {
                // First time seeing this zero freq word; insert and "return what????"
                zeroFreqTerms.put(word, null);
            }
            if (! cont.isContinuing()) {
                return null;
            }
        }

        if (nonzeroWords.isEmpty()) {
            return null;
        }

        String[] wordFreqs = new String[nonzeroWords.size()];

        return nonzeroWords.toArray(wordFreqs);
    } // getWordFrequencies

    /**
     * An IURLProcessor that fetches a similarity strength for a pair of words.
     */
    protected interface ISimilarityParser extends FetchURLUtil.IURLProcessor
    {
        public double getSimilarity();
    }

    /**
     * Subclasses should implement this to return a URL processor that
     * fetches a similarity strength for a pair of words.
     */
    protected abstract ISimilarityParser getSimilarityParser(String goal,
                                                             String search,
                                                             List<String> errors);

    /**
     * At this point, the word-pair similarity cache does not know,
     * so get the URL processor for the given word pair and process the
     * fetched content.
     */
    protected Double fetchWordSimilarity(String goalWord,
                                         String searchWord,
                                         List<String> errors)
    {
        ISimilarityParser goalSimilarityParser =
            getSimilarityParser(goalWord, searchWord, errors);

        if (FetchURLUtil.processURL(goalSimilarityParser)) {
            return new Double(goalSimilarityParser.getSimilarity());
        }

        return null;
    }

    /**
     * Look up the word pair in the cache; if there, return the associated
     * similarity.  If not there, fetch it.
     */
    protected double getWordSimilarity(String goalWord,
                                       String searchWord,
                                       List<String> errors)
    {
        if ((goalWord == null) || (searchWord == null)) {
            return UNKNOWN;
        }

        WordPair.SEARCH_KEY.goalWord = goalWord.toLowerCase();
        WordPair.SEARCH_KEY.searchWord = searchWord.toLowerCase();

        if (similarityTable.containsKey(WordPair.SEARCH_KEY)) {
            Double cachedSimilarity =
                similarityTable.get(WordPair.SEARCH_KEY);

            if (cachedSimilarity != null) {
                return cachedSimilarity.doubleValue();
            }

            throw new IllegalStateException("Similarity table contains a null similarity for pair: "
                                               + goalWord + ", " + searchWord);
        }

        Double similarity = fetchWordSimilarity(goalWord, searchWord, errors);

        if (similarity != null) {
            WordPair newEntry = new WordPair(goalWord, searchWord);

            similarityTable.put(newEntry, similarity);

            return similarity.doubleValue();
        }

        return UNKNOWN;
    } // getWordSimilarity

    /**
     * Computes the similarity between two strings (both can contain multiple words)
     * using multiple queries
     *
     * The two strings are tokenized into individual words and word-pairs are created
     * between the two strings. Each similarity of each word-pair is computed and then
     * totaled to give the similarity of both entire strings
     *
     * We split on whitespace and leave it to the analysis to split or rewrite words that are
     * hyphenated, apostrophe'd, etc
     */
    public double determineSimilarity(String goalTerm,
                                      String searchTerm,
                                      List<String> errors, 
                                      ITermSimilarity.Continuable cont)
    {
        String[] goalWords = getWordFrequencies(goalTerm, errors, cont);
        if (! cont.isContinuing()) {
            return UNKNOWN;
        }
        String[] searchWords = getWordFrequencies(searchTerm, errors, cont);
        if (! cont.isContinuing()) {
            return UNKNOWN;
        }

        if ((goalWords == null) ||
            (goalWords.length == 0) ||
            (searchWords == null) ||
            (searchWords.length == 0))
        {
            return UNKNOWN;
        }

        double totalPMI = 0.0;
        int pairCount = 0;

        for (String goalWord : goalWords) {
            for (String searchWord : searchWords) {
                double wordSimilarity =
                    getWordSimilarity(goalWord, searchWord, errors);
                if (! cont.isContinuing()) {
                    return UNKNOWN;
                }

                if (wordSimilarity >= 0.0) {
                    totalPMI += wordSimilarity;
                    pairCount++;
                }
            }
        }
        
        if (pairCount > 0) {
            return totalPMI / pairCount;
        }

        return UNKNOWN; // TODO: not quite; better some UNRELATED value
    } // determineSimilarity
}
