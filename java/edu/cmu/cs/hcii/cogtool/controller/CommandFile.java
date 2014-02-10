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

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.ISimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTGroupParameters;
import edu.cmu.cs.hcii.cogtool.model.SimilarityDictionary;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.ProjectContextSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.ProjectInteraction;
import edu.cmu.cs.hcii.cogtool.ui.ProjectUI;
import edu.cmu.cs.hcii.cogtool.uimodel.DictionaryEditorUIModel;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.ObjectPersister;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public class CommandFile
{
    private static class Command {
        private final String name;
        private final String[] arguments;
        private Command(String n, String[] a) {
            name = n;
            arguments = a;
        }
    }
    
    protected static abstract class CommandExecutor {
        protected abstract void execute(String[] args, CommandFile cf) throws Exception;
    }
    
    private static Map<String, CommandExecutor> commandExecutors =
        new HashMap<String, CommandExecutor>();
           
    private static void defineExecutor(String name, CommandExecutor ex) {
        commandExecutors.put(name, ex);
    }
    
    private final List<Command> commands;
    private ProjectController currentProject = null;
    
    public CommandFile(String s) {
        if (!CogTool.quietCommands) {
            System.err.println("Reading and executing commands from " + s);
            System.err.println();
        }
        List<Command> result = new ArrayList<Command>();
        LineNumberReader r = null;
        try {
            r = new LineNumberReader(new FileReader(s));
            String line = null;
            while ((line = r.readLine()) != null) {
                line = line.trim();
                if (line.length() == 0 || line.charAt(0) == '#') {
                    continue;
                }
                String[] pieces = line.split("\\s*\\$\\s*");
                String[] args = new String[pieces.length - 1];
                System.arraycopy(pieces, 1, args, 0, args.length);
                result.add(new Command(pieces[0], args));
            }
        } catch (IOException e) {
            if (r != null) {
                try {
                    r.close();
                } catch (IOException ex) {
                    // Ignore
                }
            }
        } finally {
            CogToolPref.isTracingOverride = null;
        }
        commands = result;
    }
    
    public ProjectController run() {
        for (Command cmd : commands) {
            if (!CogTool.quietCommands) {
                System.err.println(cmd.name);
            }
            CommandExecutor ex = commandExecutors.get(cmd.name);
            if (ex == null) {
                System.err.println("Unknown command " + cmd.name);
                System.exit(4);
            }
            try {
                // Before each operation, ensure the UI thread has a chance
                // to catch up with any pending work.
                WindowUtil.interact(true);
                if (!CogTool.quietCommands) {
                    for (String a : cmd.arguments) {
                        System.err.println(a);
                    }
                    System.err.println();
                }
                ex.execute(cmd.arguments, this);
            } catch (Exception e) {
                e.printStackTrace();
                System.exit(7);
            }
        }
        return currentProject;
    }
    
    static {
        defineExecutor("open", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                Project proj = (Project)ObjectPersister.ONLY.load(new File(args[0]));
                cf.currentProject = ProjectController.openController(proj, false, true);
            }});
        
        defineExecutor("import", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                if (cf.currentProject == null) {
                    cf.currentProject = ProjectController.newProjectController();
                }
                cf.currentProject.importFile = new File(args[0]);
                cf.currentProject.importFileComputes = (args.length > 1 ?
                                                          Boolean.parseBoolean(args[1]) :
                                                          CogToolPref.COMPSCR.getBoolean());
                cf.currentProject.performAction(CogToolLID.ImportXML, 
                                                new ProjectContextSelectionState(cf.currentProject.getModel()));
            }});
        
        defineExecutor("computeAllSkilled", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                ProjectContextSelectionState seln = new ProjectContextSelectionState(cf.currentProject.getProject());
                seln.addSelectedDesigns(cf.currentProject.getProject().getDesigns());
                ((ProjectUI)cf.currentProject.getUI()).selectAllTasks();
                cf.currentProject.recomputeScripts(seln);    
                ((ProjectUI)cf.currentProject.getUI()).deselectAllTasks();
            }});

        defineExecutor("computeNovice", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                Design design = cf.currentProject.getProject().getDesign(args[0]);
                AUndertaking task = cf.currentProject.getProject().getUndertaking(args[1]);
                List<String> targets = new ArrayList<String>();
                for (int i = 6; i < args.length; ++i) {
                    targets.add(args[i]);
                }
                SNIFACTGroupParameters defaults = new SNIFACTGroupParameters(
                    args[1],
                    Integer.parseInt(args[2]),
                    Integer.parseInt(args[3]),
                    args[4],
                    targets,
                    null,
                    Boolean.parseBoolean(args[5]));
                cf.currentProject.computeSnifAct(design, task, null, defaults);
                while (true) {
                    Thread.sleep(1000);
                    if (!SNIFACTCmd.isComputing) {
                        break;
                    }
                }
            }});
        
        defineExecutor("trace", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                if (args[0].equalsIgnoreCase("yes") || args[0].equalsIgnoreCase("true")) {
                    CogToolPref.isTracingOverride = Boolean.TRUE;
                } else if (args[0].equalsIgnoreCase("no") || args[0].equalsIgnoreCase("false")) {
                    CogToolPref.isTracingOverride = Boolean.FALSE;
                } else {
                    throw new IllegalArgumentException("Unknown value for trace on the command line: " + args[0]);
                }
                cf.currentProject.exportFile = args[0];
                cf.currentProject.exportResultsToCSV();
            }});
        
        defineExecutor("exportResults", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                cf.currentProject.exportFile = args[0];
                cf.currentProject.exportResultsToCSV();
            }});
        
        defineExecutor("importDictionary", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                Design design = cf.currentProject.getProject().getDesign(args[0]);
                ISimilarityDictionary dict = new SimilarityDictionary();
                design.setAttribute(WidgetAttributes.DICTIONARY_ATTR, dict);
                DictionaryEditorCmd.importDictionary(design, dict, null, null, null, args[1]);
            }});
        
        defineExecutor("generateDictionary", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                Design design = cf.currentProject.getProject().getDesign(args[0]);
                String algorithm = args[1];
                String site = args[2];
                String space = args[3];
                String url = args[4];
                ITermSimilarity data = DictionaryEditorUIModel.computeAlgorithm(algorithm, url, space, site);
                GenerateDictEntriesWorkThread workThread =
                        new GenerateDictEntriesWorkThread(cf.currentProject.getInteraction(),
                                                          design,
                                                          new AUndertaking[0],
                                                          cf.currentProject.getProject(),
                                                          (IUndoableEditSequence)null,
                                                          new ProjectInteraction.GenerateEntriesData(data, true));
                CogTool.logger.info(String.format(
                        "Generating dictionary for design %s in project %s.",
                        design.getName(), cf.currentProject.getProject().getName()));
                ThreadManager.startNewThread(workThread);
                while (!workThread.isFinished()) {
                    try {
                        Thread.sleep(2000);
                    } catch(InterruptedException ex) {
                    }
                }
            }});

        defineExecutor("saveAs", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                cf.currentProject.saveAsFilename(args[0]);
            }});
        
        defineExecutor("quit", new CommandExecutor() {
            @Override
            protected void execute(String[] args, CommandFile cf) throws Exception {
                cf.currentProject.getUI().performAction(CogToolLID.ExitApplication);
            }});
    }
        

    
}
