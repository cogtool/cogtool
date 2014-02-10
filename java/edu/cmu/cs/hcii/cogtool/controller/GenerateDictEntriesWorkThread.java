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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogToolWorkThread;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTExecContext;
import edu.cmu.cs.hcii.cogtool.model.TaskGroup;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.ProjectInteraction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ProgressBar;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

/**
 * Generates a set of semantic dictionary entries.  For each selected task (or,
 * if no tasks are selected, for each task in the design), it searches through
 * either the selected design or all designs to find every widget in that
 * design.  The goal string of the new entry is the task's name, and the
 * display label searched for is the widget's title or name.
 */
public class GenerateDictEntriesWorkThread extends CogToolWorkThread
{
    protected static final String title = L10N.get("PM.Generating",
                                                   "Generating entries...");

    protected ProgressBar progressBar;

    protected Design design;
    protected Project project;
    protected ProjectInteraction.GenerateEntriesData requestData;

    protected ProjectInteraction interaction;

    protected AUndertaking[] tasks;

    protected List<String> computeErrors = new ArrayList<String>();

    /**
     * Maps designs to DictEntryGenerators
     */
    protected Map<Design, DictEntryGenerator> generatorMap =
        new HashMap<Design, DictEntryGenerator>();

    protected IUndoableEditSequence undoMgr;

    protected CompoundUndoableEdit editSequence =
         new CompoundUndoableEdit(DictEntryGenerator.GENERATE_DICTIONARY,
                                  ProjectLID.GenerateDictionary);
    
    // TODO replace this crock with a general purpose mechanism for synchronizing
    //      completion of CogToolWorkThreads, so we don't terminate early. Why
    //      this was never done in the first place is a deep mystery.
    private boolean finished = false;
    public boolean isFinished() {
        synchronized (this) {
            return finished;
        }
    }

    public GenerateDictEntriesWorkThread(ProjectInteraction interactionSpt,
                                         Design d,
                                         AUndertaking[] selectedTasks,
                                         Project p,
                                         IUndoableEditSequence undoManager,
                                         ProjectInteraction.GenerateEntriesData data)
    {
        super();

        interaction = interactionSpt;
        undoMgr = undoManager;

        progressBar =
            interaction.createProgressBar(title,
                                          null,
                                          this,
                                          this,
                                          title,
                                          ProgressBar.INDETERMINATE,
                                          StringUtil.EQUAL); // TODO
        design = d;
        project = p;
        requestData = data;
        tasks = selectedTasks;

        if (d == null && selectedTasks.length == 0) {
            interaction.protestNoSelection();
        }

        setProgressCallback(progressBar, true);
        setDisabler(progressBar.getDisabler());
    }

    protected void generateEntries(Iterator<AUndertaking> tasks,
                                   DictEntryGenerator generator,
                                   ITermSimilarity alg)
    {
        while (tasks.hasNext() && ! isCanceled() && ! isStopped()) {
            AUndertaking t = tasks.next();

            if (t instanceof TaskGroup) {
                SNIFACTExecContext context =
                    (SNIFACTExecContext) t.getAttribute(WidgetAttributes.SNIFACT_CONTEXT_ATTR);

                if (! NullSafe.equals(context, WidgetAttributes.NO_CONTEXT)) {
                    generator.generateEntries(context.getParameters().taskName,
                                              alg,
                                              this,
                                              this,
                                              requestData.computeAll,
                                              computeErrors,
                                              progressCallback);
                }
                else {
                    Iterator<AUndertaking> subTasks =
                        ((TaskGroup) t).getUndertakings().iterator();
                    generateEntries(subTasks, generator, alg);
                }
            }
            else if (! t.isSpawned()) {
                generator.generateEntries(t.getName(),
                                          alg,
                                          this,
                                          this,
                                          requestData.computeAll,
                                          computeErrors,
                                          progressCallback);
            }
        }
    }

    public void doWork()
    {
        if (design == null) {
            // use all selected tasks with all designs
            Iterator<Design> designs = project.getDesigns().iterator();

            while (designs.hasNext()) {
                Design curDesign = designs.next();

                DictEntryGenerator generator = new DictEntryGenerator(curDesign);
                generatorMap.put(curDesign, generator);

                generateEntries(Arrays.asList(tasks).iterator(),
                                generator,
                                requestData.algorithm);
            }
        }
        else {
            DictEntryGenerator generator = new DictEntryGenerator(design);
            generatorMap.put(design, generator);

            if (tasks.length == 0) {
                // use all tasks with selected design
                generateEntries(project.getUndertakings().iterator(),
                                generator,
                                requestData.algorithm);
            }
            else {
                // use selected task and selected design
                generateEntries(Arrays.asList(tasks).iterator(),
                                generator,
                                requestData.algorithm);
            }
        }
        
        synchronized (this) {
            finished = true;
        }

    }

    protected void openDictionaryEditor(Design d)
    {
        ISimilarityDictionary dict =
            (ISimilarityDictionary) d.getAttribute(WidgetAttributes.DICTIONARY_ATTR);

        if (NullSafe.equals(dict, WidgetAttributes.NO_DICTIONARY)) {
            return;
        }

        ITermSimilarity oldAlg = dict.getCurrentAlgorithm();

        dict.setCurrentAlgorithm(requestData.algorithm);

        DictionaryEditorController dec =
            DictionaryEditorController.openController(dict,
                                                                     d,
                                                                     project);

        DictEntryGenerator generator = generatorMap.get(d);

        generator.insertEntries(dec,
                                requestData.computeAll,
                                oldAlg,
                                requestData.algorithm,
                                project,
                                editSequence);
    }
    
    // TODO This whole mishmash of different flavors of progress bars, and
    //      Cancelables/Stoppables/Pausables is a mess. We shouldn't be using
    //      the type hierarchy to fiddle this stuff. Instead we should have a
    //      single interface for control of a background operation, subsuming
    //      all of Cancelable, Stoppable and Pausable, and a single ProgressBar
    //      type that takes a bunch of flags indicating what buttons it should
    //      display.

    @Override
    public void doneCallback()
    {
        // Performed by the main UI thread

        // If an exception was thrown during the import, display error here
        boolean exceptionThrown =
            RcvrExceptionHandler.recoverWorkThread(this, interaction);

        if ((! exceptionThrown) && ! isCanceled()) {
            if (computeErrors.size() > 0) {
                interaction.reportProblems("Encountered:",
                                                computeErrors);
                // TODO: return?
            }

            if (design != null) {
                openDictionaryEditor(design);
            }
            else {
                Iterator<Design> designs = project.getDesigns().iterator();

                while (designs.hasNext()) {
                    openDictionaryEditor(designs.next());
                }
            }

            editSequence.end();
            if (undoMgr != null) {
                undoMgr.addEdit(editSequence);
            }
        }

        super.doneCallback();
    }
}
