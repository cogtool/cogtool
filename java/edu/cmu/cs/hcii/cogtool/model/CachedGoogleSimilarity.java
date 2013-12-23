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

import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
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
 * Corresponds to computeGSimilarity in Leonghwee's code.
 * But in the UI it's called PMI-G (word).
 * It also differs from Leongwhee's code in that we now normalize the results.
 */
public class CachedGoogleSimilarity extends CachedTermSimilarity
                                    implements ISitedTermSimilarity
{
    public static final int edu_cmu_cs_hcii_cogtool_model_CachedGoogleSimilarity_version = 0;

    protected static final String contextSiteVAR = "contextSite";

    private static ObjectSaver.IDataSaver<CachedGoogleSimilarity> SAVER =
        new ObjectSaver.ADataSaver<CachedGoogleSimilarity>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_CachedGoogleSimilarity_version;
            }

            @Override
            public void saveData(CachedGoogleSimilarity v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveObject(v.site, contextSiteVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(CachedGoogleSimilarity.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<CachedGoogleSimilarity> LOADER =
        new ObjectLoader.AObjectLoader<CachedGoogleSimilarity>() {
            @Override
            public CachedGoogleSimilarity createObject()
            {
                return new CachedGoogleSimilarity();
            }

            @Override
            public void set(CachedGoogleSimilarity target,
                            String variable,
                            Object value)
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
        ObjectLoader.registerLoader(CachedGoogleSimilarity.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_CachedGoogleSimilarity_version,
                                    LOADER);
    }

    protected String site = null;

    public static final double PMI_G_SIZE_AUTOMATIC = -1.0;

    protected static CachedGoogleSimilarity checkAlg = null;

    public static CachedGoogleSimilarity create(String limitToSite)
    {
        if (checkAlg == null) {
            checkAlg = new CachedGoogleSimilarity();
        }

        checkAlg.resetSite(limitToSite);

        CachedGoogleSimilarity registered =
            (CachedGoogleSimilarity) AlgorithmRegistry.ONLY.register(checkAlg);

        // If null, then the registry used checkAlg and registered it.
        if (registered == null) {
            registered = checkAlg;
            checkAlg = null;
        }

        // registered now contains the algorithm to use
        return registered;
    }

    protected CachedGoogleSimilarity()
    {
    }

    /**
     * This algorithm also allows one to provide a document set scope
     * by limiting the fetches to a specified site.
     */
    protected void resetSite(String limitToSite)
    {
        if ("".equals(limitToSite)) {
            limitToSite = null;
        }

        site = limitToSite;
    }

    @Override
    protected IWordFrequencyParser getWordFreqParser(String word,
                                                     List<String> errors)
    {
        GoogleSimilarity.termFreqParser.reset(word, errors, site);

        return GoogleSimilarity.termFreqParser;
    } // getWordFreqParser

    protected static final String COMMON_WORD = "the";

    /**
     * No need for a similarity URL processor; word similarity is computed
     * purely using word frequencies.
     */
    @Override
    protected Double fetchWordSimilarity(String goalWord,
                                         String searchWord,
                                         List<String> errors)
    {
        double indexSize = CogToolPref.PMI_G_SIZE.getDouble();
        if (indexSize == CachedGoogleSimilarity.PMI_G_SIZE_AUTOMATIC) {
            indexSize = getWordFrequency(COMMON_WORD, errors);
        }
        double numerator =
            (getWordFrequency(goalWord + " " + searchWord, errors))
               / indexSize;
        double denominator =
            ((getWordFrequency(goalWord, errors)) / indexSize)
               * ((getWordFrequency(searchWord, errors)) / indexSize);

        // TODO we may want to normalize the results differently
        if (denominator != 0.0) {
            double unnormalized = Math.log10(numerator / denominator);
            double normalized = unnormalized / Math.log10(indexSize);
            CogTool.logger.finer(String.format(
                 "Normalizing PMI-G (word) value (%s, %s) from %g to %g",
                 goalWord, searchWord, unnormalized, normalized));
            return new Double(normalized);
        }

        CogTool.logger.finer(String.format(
              "Unknown PMI-G (word) similarity for %s, %s",
              goalWord, searchWord));
        return UNKNOWN_SIMILARITY;
    }

    @Override
    protected ISimilarityParser getSimilarityParser(String goal,
                                                    String search,
                                                    List<String> errors)
    {
        throw new UnsupportedOperationException("No similarity parser is required for CachedGoogle for: "
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
        if (other instanceof CachedGoogleSimilarity) {
            CachedGoogleSimilarity cgs = (CachedGoogleSimilarity) other;

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
        // this   : 139, 207
        HashCodeBuilder hcb = new HashCodeBuilder(139, 207);

        if (site != null) {
            hcb.append(site.hashCode());
        }

        return hcb.toHashCode();
    }

    public ITermSimilarity duplicate()
    {
        CachedGoogleSimilarity cgs = new CachedGoogleSimilarity();
        cgs.site = site;
        return cgs;
    }
}
