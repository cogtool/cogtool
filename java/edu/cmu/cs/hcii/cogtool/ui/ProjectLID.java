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

package edu.cmu.cs.hcii.cogtool.ui;

import java.util.HashMap;
import java.util.Map;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;

/**
 * Specialization of <code>CogToolLID</code> values to actions
 * performed by a <code>ProjectController</code>.
 * Also, actions totally specific to project interaction.
 * <p>
 *    Generic    Selected      ->    Specific
 *    Action      Object              Action
 *    -------    --------            --------
 *      Cut       Design       ->    CutDesign
 *      Cut       AUndertaking  ->    CutTask
 *      Copy      Design       ->    CopyDesign
 *      Copy      AUndertaking  ->    CopyTask
 *      Edit      Design       ->    EditDesign
 *      Edit      AUndertaking  ->    EditTask
 *      Duplicate Design       ->    DuplicateDesign
 *      Duplicate AUndertaking  ->    DuplicateTask
 *      Rename    Design       ->    RenameDesign
 *      Rename    AUndertaking  ->    RenameTask
 *      Delete    Design       ->    DeleteDesign
 *      Delete    AUndertaking  ->    DeleteTask
 * <p>
 * Actions specific to project interaction:
 *      InitiateTaskRename, EditScript,
 *      ExportACTRTrace, ExportActrModelFile, CopyResultsToClipboard,
 *      ExportToHCIPA
 *
 * @author mlh
 */
public class ProjectLID extends CogToolLID
{
    public static final ProjectLID CutDesign =
        new ProjectLID("CutDesign", 1006, CAUSES_DESIGN_SELECTION);

    public static final ProjectLID CutTask =
        new ProjectLID("CutTask", 2006, CAUSES_TASK_SELECTION);

    public static final ProjectLID CopyDesign =
        new ProjectLID("CopyDesign", 1007);

    public static final ProjectLID CopyTask =
        new ProjectLID("CopyTask", 2007);

    public static final ProjectLID EditDesign =
        new ProjectLID("EditDesign", 1013);

    public static final ProjectLID ViewGroupScript =
        new ProjectLID("ViewGroupScript", 1017);

    public static final ProjectLID EditTask =
        new ProjectLID("EditTask", 2013);

    public static final ProjectLID DuplicateDesign =
        new ProjectLID("DuplicateDesign", 1014, CAUSES_DESIGN_SELECTION);

    public static final ProjectLID DuplicateTask =
        new ProjectLID("DuplicateTask", 2014, CAUSES_TASK_SELECTION);

    public static final ProjectLID RenameDesign =
        new ProjectLID("RenameDesign", 1015);

    public static final ProjectLID RenameTask =
        new ProjectLID("RenameTask", 2015);

    public static final ProjectLID InitiateTaskRename =
        new ProjectLID("InitiateTaskRename", 2915);

    public static final ProjectLID DeleteDesign =
        new ProjectLID("DeleteDesign", 1016, CAUSES_DESIGN_SELECTION);

    public static final ProjectLID DeleteTask =
        new ProjectLID("DeleteTask", 2016, CAUSES_TASK_SELECTION);

    public static final ProjectLID EditScript =
        new ProjectLID("EditScript", 2017);

    public static final ProjectLID ExportTraces =
        new ProjectLID("ExportTraces", 2018);

    public static final ProjectLID DisplayTraces =
        new ProjectLID("DisplayTraces", 2019);

    public static final ProjectLID ExportActrModelFile =
        new ProjectLID("ExportActrModelFile", 2020);

    public static final ProjectLID ExportForSanlab =
        new ProjectLID("ExportActrModelFile", 2021);

    public static final ProjectLID NewProjectNewDesign =
        new ProjectLID("NewProjectNewDesign", 2022, CAUSES_DESIGN_SELECTION);

    public static final ProjectLID ImportHumanCSVFile =
        new ProjectLID("ImportHumanCSVFile", 2026);

    public static final ProjectLID SetAlgorithmACTR6 =
        new ProjectLID("SetAlgorithmACTR6", 2030);

    public static final ProjectLID SetAlgorithmACTR5 =
        new ProjectLID("SetAlgorithmACTR5", 2031);

    public static final ProjectLID SetAlgorithmSNIFACT =
        new ProjectLID("SetAlgorithmSNIFACT", 2066);

    public static final ProjectLID EditHumanCSVFile =
        new ProjectLID("EditHumanCSVFile", 2034);

    public static final ProjectLID SetAlgorithmHuman =
        new ProjectLID("SetAlgorithmHuman", 2035);

    public static final ProjectLID SetBackgroundComputationDefault =
        new ProjectLID("SetBackgroundComputationDefault", 2037);

    public static final ProjectLID SetAlgorithmDefault =
        new ProjectLID("SetAlgorithmDefault", 2039);

    public static final ProjectLID SetProjDefaultAlgoACTR =
        new ProjectLID("SetProjDefaultAlgoACTR", 2040);

    public static final ProjectLID SetProjDefaultAlgoSNIFACT =
        new ProjectLID("SetProjDefaultAlgoSNIFACT", 2067);

    public static final ProjectLID SetBackgroundComputationTrue =
        new ProjectLID("SetBackgroundComputationTrue", 2042);

    public static final ProjectLID SetBackgroundComputationFalse =
        new ProjectLID("SetBackgroundComputationFalse", 2043);

    public static final ProjectLID SetProjExecBackground =
        new ProjectLID("SetProjExecBackground", 2044);

    public static final ProjectLID SetProjExecForeground =
        new ProjectLID("SetProjExecForeground", 2045);

   public static final ProjectLID GenerateACTRModelFile =
       new ProjectLID("GenerateACTRModelFile", 2048);

   public static final ProjectLID EditACTRModelFile =
       new ProjectLID("EditACTRModelFile", 2049);

   public static final ProjectLID CopyResultsToClipboard =
       new ProjectLID("CopyResultsToClipboard", 2050);

   public static final ProjectLID PromoteTask =
       new ProjectLID("PromoteTask", 2051, CAUSES_TASK_SELECTION);

   public static final ProjectLID DemoteTask =
       new ProjectLID("DemoteTask", 2052, CAUSES_TASK_SELECTION);

   public static final ProjectLID MoveTaskEarlier =
       new ProjectLID("MoveTaskEarlier", 2053, CAUSES_TASK_SELECTION);

   public static final ProjectLID MoveTaskLater =
       new ProjectLID("MoveTaskLater", 2054, CAUSES_TASK_SELECTION);

   public static final ProjectLID HCIPARenameTask =
       new ProjectLID("HCIPARenameTask", 2055);

   public static final ProjectLID ExportToHCIPA =
       new ProjectLID("ExportToHCIPA", 2056);

   public static final ProjectLID ChangeTaskPosition =
       new ProjectLID("ChangeTaskPosition", 2057, CAUSES_TASK_SELECTION);

   public static final ProjectLID DuplicateTaskFull =
       new ProjectLID("DuplicateTaskFull", 2058, CAUSES_TASK_SELECTION);

   public static final ProjectLID GenerateDictionary =
       new ProjectLID("GenerateDictionary", 2059);

   public static final ProjectLID EditDictionary =
       new ProjectLID("EditDictionary", 2060);

   public static final ProjectLID ExportDictToCSV =
       new ProjectLID("ExportDictToCSV", 2061);

   public static final ProjectLID ImportDict =
       new ProjectLID("ImportDictFromCSV", 2062);

   public static final ProjectLID MoveTaskApplication =
       new ProjectLID("MoveTaskApplication", 2064, CAUSES_CELL_SELECTION);

   public static final ProjectLID DuplicateTaskApplication =
       new ProjectLID("DuplicateTaskApplication", 2065, CAUSES_CELL_SELECTION);

   /**
    * The mapping from generic to specific LIDs for designs
    */
    public static final Map<ListenerIdentifier, ListenerIdentifier> designLIDs =
        new HashMap<ListenerIdentifier, ListenerIdentifier>();
    static {
        designLIDs.put(CogToolLID.Cut, CutDesign);
        designLIDs.put(CogToolLID.Copy, CopyDesign);
        designLIDs.put(CogToolLID.Edit, EditDesign);
        designLIDs.put(CogToolLID.Duplicate, DuplicateDesign);
        designLIDs.put(CogToolLID.Rename, RenameDesign);
        designLIDs.put(CogToolLID.Delete, DeleteDesign);
    }

    /**
     * The mapping from generic to specific LIDs for undertakings
     */
    public static final Map<ListenerIdentifier, ListenerIdentifier> taskLIDs =
        new HashMap<ListenerIdentifier, ListenerIdentifier>();

    static {
        taskLIDs.put(CogToolLID.Cut, CutTask);
        taskLIDs.put(CogToolLID.Copy, CopyTask);
        taskLIDs.put(CogToolLID.Edit, EditTask);
        taskLIDs.put(CogToolLID.Duplicate, DuplicateTask);
        taskLIDs.put(CogToolLID.Rename, InitiateTaskRename);
        taskLIDs.put(CogToolLID.Delete, DeleteTask);
    }

    /**
     * The mapping from generic to specific LIDs for undertakings
     */
    public static final Map<ListenerIdentifier, ListenerIdentifier> scriptLIDs =
        new HashMap<ListenerIdentifier, ListenerIdentifier>();

    static {
        scriptLIDs.put(CogToolLID.Edit, EditScript);
    }

    /**
     * Constructor for new LID values.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @param selectionCanHappen whether selection could happen as a result
     *                           of the operation represented by this LID
     * @author mlh
     */
    protected ProjectLID(String newLabel,
                         int newPersistenceValue,
                         int selectionCanHappen)
    {
        super(newLabel, newPersistenceValue, selectionCanHappen);
    }

    /**
     * Constructor for new LID values.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @author mlh
     */
    protected ProjectLID(String newLabel, int newPersistenceValue)
    {
        this(newLabel, newPersistenceValue, CAUSES_NO_SELECTION);
    }
}