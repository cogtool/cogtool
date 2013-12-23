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

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictEntry;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary.DictValue;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.ProgressCallback;
import edu.cmu.cs.hcii.cogtool.util.Stoppable;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * Helper class for generating dictionary entries given a design and a goal
 * string
 * @author rmyers
 */
public class DictEntryGenerator
{
    protected static final String GENERATE_DICTIONARY =
        L10N.get("UNDO.PM.GenerateDictionary", "Generate Dictionary");

    protected Design design;

    /**
     * Maps entries to be added to the dictionary to their values
     */
    protected Map<DictEntry, DictValue> newDictEntries =
        new LinkedHashMap<DictEntry, DictValue>();

    /**
     * Maps entries that have been modified in the dictionary to their new
     * values
     */
    protected Map<DictEntry, DictValue> updatedDictEntries =
        new LinkedHashMap<DictEntry, DictValue>();

    /**
     * Maps entries that have been modified in the dictionary to their old
     * values
     */
    protected Map<DictEntry, DictValue> oldDictEntries =
        new LinkedHashMap<DictEntry, DictValue>();

    public DictEntryGenerator(Design d)
    {
        design = d;
    }

    // TODO This whole mishmash of different flavors of progress bars, and
    //      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
    //      the type hierarchy to fiddle this stuff. Instead we should have a
    //      single interface for control of a background operation, subsuming
    //      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
    //      type that takes a bunch of flags indicating what buttons it should
    //      display.
    
   public void generateEntries(String goal,
                                ITermSimilarity alg,
                                Cancelable cancelable,
                                Stoppable stoppable,
                                boolean computeAll,
                                List<String> computeErrors,
                                ProgressCallback progressCallback)
    {
        // TODO figure out how to deal with this for real, long term
        // goal = clean(goal);
        
        ISimilarityDictionary dict =
            (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);
        if (NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY)) {
            dict = new SimilarityDictionary();
            design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, dict);
        }
        int initialSize = dict.size();
        ITermSimilarity.Continuable cont = new ITermSimilarity.Continuable(cancelable, stoppable);
        Iterator<Frame> frames = design.getFrames().iterator();
        while (frames.hasNext() && cont.isContinuing())
        {
            Frame f = frames.next();

            generateOneEntry(f.getSpeakerText(),
                             goal, alg, cont, dict, computeAll, computeErrors, progressCallback);

            Set<SimpleWidgetGroup> swGrps = new HashSet<SimpleWidgetGroup>();
            Iterator<IWidget> widgets = f.getWidgets().iterator();
            while (widgets.hasNext() && cont.isContinuing()) {
                IWidget w = widgets.next();
                generateOneEntry(w.getTitle(),
                                 goal, alg, cont, dict, computeAll, computeErrors, progressCallback);
                if (! cont.isContinuing()) { 
                    break;
                }
                generateOneEntry(w.getTextualCue(),
                                 goal, alg, cont, dict, computeAll, computeErrors, progressCallback);
                SimpleWidgetGroup swg = w.getParentGroup();
                if (swg != null) {
                    swGrps.add(swg);
                }
            }

            Iterator<FrameElementGroup> groups = f.getEltGroups().iterator();
            while (groups.hasNext() && cont.isContinuing()) {
                generateOneEntry(groups.next().getTextualCue(),
                                 goal, alg, cont, dict, computeAll, computeErrors, progressCallback);
            }
            
            Iterator<SimpleWidgetGroup> swgIter = swGrps.iterator();
            while (swgIter.hasNext() && cont.isContinuing()) {
                generateOneEntry(swgIter.next().getTextualCue(),
                                 goal, alg, cont, dict, computeAll, computeErrors, progressCallback);
            }
        }
        if (stoppable.isStopped()) {
            // Clean out the last entry if we were stopped, as it may not be
            // correct. But only if we've added something.
            int n = dict.size();
            if (n > initialSize) {
                dict.removeEntry(n - 1);
            }
        }
    }

    private void generateOneEntry(String search,
                                  String goal,
                                  ITermSimilarity alg,
                                  ITermSimilarity.Continuable cont,
                                  ISimilarityDictionary dict,
                                  boolean computeAll,
                                  List<String> computeErrors,
                                  ProgressCallback progressCallback)
    {
        // TODO figure out how to deal with this for real, long term
        // search = clean(search);
        if (search == null || "".equals(search)) {
            return;
        }
        
        // TODO what if goal is null/empty?

        DictEntry entry = new DictEntry(goal, search, alg);

        // Even if computeAll is true, don't recompute entries that
        // have already been computed during this execution.
        boolean computed = newDictEntries.containsKey(entry) ||
                           updatedDictEntries.containsKey(entry);

        if ((! dict.containsEntry(entry) || computeAll) && ! computed) {
            double similarity;

            if (alg == ITermSimilarity.MANUAL) {
                if (dict.containsEntry(entry)) {
                    // Don't reset a manually entered similarity!
                    return;
                }

                similarity = 0.0;
            }
            else {
                if (progressCallback != null) {
                    progressCallback.updateProgress(0.0,
                                                    goal + " : " + search);
                }
                similarity = alg.determineSimilarity(goal,
                                                     search,
                                                     computeErrors, 
                                                     cont);
            }

            DictValue newValue = new DictValue(similarity);
            if (dict.containsEntry(entry) && cont.isContinuing()) {
                DictValue oldValue = dict.getValue(entry);
                oldDictEntries.put(entry,
                                        new DictValue(oldValue.similarity,
                                                      oldValue.editedDate));
                updatedDictEntries.put(entry, newValue);
            }
            else {
                newDictEntries.put(entry, newValue);
            }
        }
    }
    
    private static final Pattern CLEAN_PAT = Pattern.compile("[^a-zA-Z]+");
    
    // Replaces non-alphabetic characters by spaces, collapses runs of
    // whitespace to a single space, and strips off leading and lying whitespace.
    // Returns null if the argument is null.
    private static String clean(String s) {
        if (s == null || s.equals("")) {
            return s;
        }
        return CLEAN_PAT.matcher(s).replaceAll(" ").trim();
    }

    public void insertEntries(final DictionaryEditorController dec,
                              final boolean computeAll,
                              final ITermSimilarity oldAlg,
                              final ITermSimilarity alg,
                              Project project,
                              IUndoableEditSequence editSequence)
    {
        final ISimilarityDictionary dict =
            (ISimilarityDictionary) design.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

        if ((newDictEntries.size() > 0) ||
            (updatedDictEntries.size() > 0))
        {
            dict.insertEntries(newDictEntries, computeAll);
            dict.insertEntries(updatedDictEntries, computeAll);

            IUndoableEdit edit = new AUndoableEdit(ProjectLID.GenerateDictionary) {
                @Override
                public String getPresentationName()
                {
                    return GENERATE_DICTIONARY;
                }

                @Override
                public void redo()
                {
                    super.redo();

                    dict.insertEntries(newDictEntries,
                                       computeAll);
                    dict.insertEntries(updatedDictEntries,
                                       computeAll);

                    dec.setCurrentAlgorithm(alg);
                }

                @Override
                public void undo()
                {
                    super.undo();

                    dict.removeEntries(newDictEntries);
                    dict.insertEntries(oldDictEntries,
                                       computeAll);

                    dec.setCurrentAlgorithm(oldAlg);
                }
            };
            editSequence.addEdit(edit);
            UndoManager.getUndoManager(dict,
                                                      project).addEdit(edit);
        }
        else {
            dec.interaction.setStatusMessage("No entries to compute.");
        }
    }
}