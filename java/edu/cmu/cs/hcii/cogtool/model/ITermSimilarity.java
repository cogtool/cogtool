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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.Stoppable;

// The names of classes and interfaces around this are terrible, but we can't
// change them without breaking old .cgt files, since our persistence
// mechanism leaks the implementation detail of our class names into the
// abstraction of our file format.
// Note that the only implementer of this interface is the abstract calls
// CachedTermSimilarity. All concrete classes are subclasses of that, and there
// is no such thing as an un-cached TermSimilarity.
// Both GoogleSimilarity and CachedGoogleSimilarity are direct subsclasses
// of CachedTermSimilarity, and the one with cached in its name does not
// inherit from the one without. All very confusing.
// TODO once we augment our persistence mechanism in such a way that we
//      can actually rename persistable classes, we should tidy up these names.

/**
 * Given two terms, compute/look up the similarity measure measuring
 * how related the two terms are.  Terms may consist of multiple words
 * separated by one or more spaces.
 *
 * Issues: What constitutes a minimum/maximum similarity measure?
 * In other words, should this interface also have methods to return
 * the minimum and maximum similarity values to allow for normalization?
 * If not, then we should state what the normalization domain consists of.
 */
public interface ITermSimilarity
{
    /**
     * A flag value indicating that the algorithm cannot determine the
     * similarity between the two given terms.
     */
    public static final double UNKNOWN = Double.NEGATIVE_INFINITY;

    public static final ITermSimilarity MANUAL = ManualSimilarity.ONLY;
    public static final ITermSimilarity ALL = null;

    public static class ManualSimilarity implements ITermSimilarity
    {
        private static final ManualSimilarity ONLY = new ManualSimilarity();

        public static final int edu_cmu_cs_hcii_cogtool_model_ManualSimilarity_version = 0;

        private static ObjectSaver.IDataSaver<ManualSimilarity> SAVER =
            new ObjectSaver.ADataSaver<ManualSimilarity>() {
                @Override
                public int getVersion()
                {
                    return edu_cmu_cs_hcii_cogtool_model_ManualSimilarity_version;
                }

                @Override
                public void saveData(ManualSimilarity v, ObjectSaver saver)
                {
                    // Nothing to save; it's an ONLY!
                }
            };

        public static void registerSaver()
        {
            ObjectSaver.registerSaver(ManualSimilarity.class.getName(),
                                      SAVER);
        }

        private static ObjectLoader.IObjectLoader<ManualSimilarity> LOADER =
            new ObjectLoader.AObjectLoader<ManualSimilarity>() {
                @Override
                public ManualSimilarity createObject()
                {
                    return ONLY;
                }
            };

        public static void registerLoader()
        {
            ObjectLoader.registerLoader(ManualSimilarity.class.getName(),
                                        edu_cmu_cs_hcii_cogtool_model_ManualSimilarity_version,
                                        LOADER);
        }

        public double determineSimilarity(String goalTerm, String searchTerm,
                                          List<String> errors, Continuable cont)
        {
            throw new IllegalArgumentException("Should never be called on MANUAL");
        }

        public ITermSimilarity duplicate()
        {
            return this;
        }
    }

    /**
     * Determine the similarity of the two given terms.  If any errors
     * are encountered during the processing, they will be added to the
     * end of the given List if it is not <code>null</code>.
     * @param cancelable may be null
     */
    public double determineSimilarity(String goalTerm,
                                      String searchTerm,
                                      List<String> errors, 
                                      Continuable cont);

    public ITermSimilarity duplicate();

    public static class AlgorithmRegistry
    {
        public static final AlgorithmRegistry ONLY = new AlgorithmRegistry();

        // maps ITermSimilarity to itself
        protected Map<ITermSimilarity, ITermSimilarity> algRegistry =
        	new HashMap<ITermSimilarity, ITermSimilarity>();

        /**
         * Returns null if the given algorithm is new and has been registered.
         * Returns the previously registered algorithm that is equivalent otherwise.
         */
        public ITermSimilarity register(ITermSimilarity alg)
        {
            ITermSimilarity registeredAlg = algRegistry.get(alg);

            if (registeredAlg == null) {
                algRegistry.put(alg, alg);

                return null;
            }

            return registeredAlg;
        }

        protected AlgorithmRegistry() { }
    }

    // TODO This whole mishmash of different flavors of progress bars, and
    //      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
    //      the type hierarchy to fiddle this stuff. Instead we should have a
    //      single interface for control of a background operation, subsuming
    //      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
    //      type that takes a bunch of flags indicating what buttons it should
    //      display.
    
    public static class Continuable {
        
        final private Cancelable cancelable;
        final private Stoppable stoppable;
        
        public Continuable(Cancelable c, Stoppable s) {
            cancelable = c;
            stoppable = s;
        }
        
        public boolean isContinuing() {
            return (((cancelable == null) || (! cancelable.isCanceled()))
                    && ((stoppable == null) || (! stoppable.isStopped())));
        }
    }
}
