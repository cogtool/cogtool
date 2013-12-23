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

package edu.cmu.cs.hcii.cogtool;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.model.ImportConverter;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;

/**
 * The set of values that represent semantic actions for the CogTool
 * application.  These values are generic, in that each window providing
 * a user interface specific to a model object (such as the Project, Design,
 * or Frame editor windows) may interpret the actions relative to set of
 * objects maintained by the corresponding selection state.
 *
 * @author mlh
 */
public class CogToolLID extends ListenerIdentifier
                        implements Comparable<CogToolLID>
{
    protected static int nextOrdering = 0;

    private final transient String label;
    private final transient int ordering;
    protected final int persistenceValue;
    /**
     * Flag indicating whether the operation represented by this LID
     * should cause property sheet text changes to take effect.
     */
    public final boolean commitsPropertyChanges;

    /**
     * Flag indicating that property changes should not be committed.
     */
    public static final boolean NO_COMMITS = false;

    /**
     * Flag indicating that property changes should be committed.
     */
    public static final boolean COMMITS_CHANGES = true;

    /**
     * Flag indicating whether selection could happen as a result
     * of the operation represented by this LID.
     */
    public final int causesSelection;

    /**
     * Common flag indicating that no kind of selection should occur.
     */
    public static final int CAUSES_NO_SELECTION = 0;

    /**
     * Common flag indicating that all kinds of selection may occur.
     */
    public static final int CAUSES_SELECTION = -1;

    /**
     * Flag indicating that only selection of task(s) and/or task group(s)
     * should occur (remember, use unique powers of 2!)
     */
    public static final int CAUSES_TASK_SELECTION = 1;

    /**
     * Flag indicating that only selection of design(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_DESIGN_SELECTION = 2;

    /**
     * Flag indicating that only selection of frame(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_FRAME_SELECTION = 4;

    /**
     * Flag indicating that only selection of transition(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_TRANSITION_SELECTION = 8;

    /**
     * Flag indicating that only selection of widget(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_WIDGET_SELECTION = 16;

    /**
     * Flag indicating that only selection of script(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_SCRIPT_SELECTION = 32;

    /**
     * Flag indicating that only selection of script step(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_SCRIPTSTEP_SELECTION = 64;

    /**
     * Flag indicating that only selection of cell(s) should occur
     * (remember, use unique powers of 2!)
     */
    public static final int CAUSES_CELL_SELECTION = 128;

    /**
     * Constructor for new LID values.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @param commitsChanges whether any changes to property sheet text
     *                       values should be committed before performing
     *                       the action.
     * @param selectionCanHappen whether selection could happen as a result
     *                           of the operation represented by this LID;
     *                           can be treated as a simple flag or a bitset
     *                           mask of which kind of selection can happen
     *                           (e.g., frame vs. transition in the design
     *                           editor)
     * @author mlh
     */
    protected CogToolLID(String newLabel,
                         int newPersistenceValue,
                         boolean commitsChanges,
                         int selectionCanHappen)
    {
        label = newLabel;
        persistenceValue = newPersistenceValue;
        ordering = nextOrdering++;
        commitsPropertyChanges = commitsChanges;
        causesSelection = selectionCanHappen;
    }

    /**
     * Constructor for new LID values.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @param commitsChanges whether any changes to property sheet text
     *                       values should be committed before performing
     *                       the action.
     * @author mlh
     */
    protected CogToolLID(String newLabel,
                         int newPersistenceValue,
                         boolean commitsChanges)
    {
        this(newLabel, newPersistenceValue,
             commitsChanges, CAUSES_NO_SELECTION);
    }

    /**
     * Constructor for new LID values.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @param selectionCanHappen whether selection could happen as a result
     *                           of the operation represented by this LID;
     *                           can be treated as a simple flag or a bitset
     *                           mask of which kind of selection can happen
     *                           (e.g., frame vs. transition in the design
     *                           editor)
     * @author mlh
     */
    protected CogToolLID(String newLabel,
                         int newPersistenceValue,
                         int selectionCanHappen)
    {
        this(newLabel, newPersistenceValue, NO_COMMITS, selectionCanHappen);
    }

    /**
     * Constructor for new LID values.  Assumes selection will *not* happen.
     *
     * @param newLabel the label for the new value
     * @param newPersistenceValue the enumeration code for the new value
     * @author mlh
     */
    protected CogToolLID(String newLabel, int newPersistenceValue)
    {
        this(newLabel, newPersistenceValue, CAUSES_NO_SELECTION);
    }

    public static final CogToolLID Help =
        new CogToolLID("Help", 0);

    public static final CogToolLID About =
        new CogToolLID("About", 1);

    public static final CogToolLID ExitApplication =
        new CogToolLID("ExitApplication", 2, COMMITS_CHANGES);

    public static final CogToolLID CloseWindow =
        new CogToolLID("CloseWindow", 3, COMMITS_CHANGES);

    public static final CogToolLID Undo =
        new CogToolLID("Undo", 4);

    public static final CogToolLID Redo =
        new CogToolLID("Redo", 5);

    public static final CogToolLID Cut =
        new CogToolLID("Cut", 6, CAUSES_SELECTION);

    public static final CogToolLID Copy =
        new CogToolLID("Copy", 7);

    public static final CogToolLID Paste =
        new CogToolLID("Paste", 8, CAUSES_SELECTION);

    public static final CogToolLID Find =
        new CogToolLID("Find", 9);

    public static final CogToolLID FindAgain =
        new CogToolLID("FindAgain", 10);

    // No delayed work for select all!
    public static final CogToolLID SelectAll =
        new CogToolLID("SelectAll", 11, COMMITS_CHANGES);

    public static final CogToolLID DeselectAll =
        new CogToolLID("DeselectAll", 12, COMMITS_CHANGES);

    public static final CogToolLID Edit =
        new CogToolLID("Edit", 13, COMMITS_CHANGES);

    public static final CogToolLID Duplicate =
        new CogToolLID("Duplicate", 14, COMMITS_CHANGES, CAUSES_SELECTION);

    public static final CogToolLID Rename =
        new CogToolLID("Rename", 15, COMMITS_CHANGES);

    public static final CogToolLID Delete =
        new CogToolLID("Delete", 16, COMMITS_CHANGES, CAUSES_SELECTION);

    public static final CogToolLID Properties =
        new CogToolLID("Properties", 17);

    public static final CogToolLID Preferences =
        new CogToolLID("Preferences", 18);

    public static final CogToolLID NewProject =
        new CogToolLID("NewProject", 100);

    public static final CogToolLID OpenProject =
        new CogToolLID("OpenProject", 101);

    public static final CogToolLID OpenProjectFile =
        new CogToolLID("OpenProjectFile", 117);

    public static final CogToolLID ClearRecent =
        new CogToolLID("ClearRecent", 118);

    public static final CogToolLID SaveProject =
        new CogToolLID("SaveProject", 102, COMMITS_CHANGES);

    public static final CogToolLID SaveProjectAs =
        new CogToolLID("SaveProjectAs", 103, COMMITS_CHANGES);

    public static final CogToolLID ExportToXML =
        new CogToolLID("ExportToXML", 104, COMMITS_CHANGES);

    public static final CogToolLID ImportXML =
        new CogToolLID("ImportXML", 114);

    public static final CogToolLID PrintProject =
        new CogToolLID("PrintProject", 105, COMMITS_CHANGES);

    public static final CogToolLID NewDesign =
        new CogToolLID("NewDesign", 106,
                       COMMITS_CHANGES, CAUSES_DESIGN_SELECTION);

    public static final CogToolLID NewTask =
        new CogToolLID("NewTask", 107, COMMITS_CHANGES, CAUSES_TASK_SELECTION);

    public static final CogToolLID AddDesignDevices =
        new CogToolLID("AddDesignDevices", 108, COMMITS_CHANGES);

    public static final CogToolLID NewTaskGroup =
        new CogToolLID("NewTaskGroup", 109,
                       COMMITS_CHANGES, CAUSES_TASK_SELECTION);

    public static final CogToolLID PrintPreviewProject =
        new CogToolLID("PrintPreviewProject", 110);

    public static final CogToolLID PageSetupProject =
        new CogToolLID("PageSetupProject", 111);

    public static final CogToolLID RecomputeScript =
        new CogToolLID("RecomputeScript", 112);

    public static final CogToolLID CloseProject =
        new CogToolLID("CloseProject", 113, COMMITS_CHANGES);

    public static final CogToolLID CaptureBehavior =
        new CogToolLID("CaptureBehavior", 115);

    public static final CogToolLID ReorderDesigns =
        new CogToolLID("ReorderDesigns", 116, CAUSES_DESIGN_SELECTION);

    public static final CogToolLID NewFrame =
        new CogToolLID("NewFrame", 200,
                       COMMITS_CHANGES, CAUSES_FRAME_SELECTION);

    public static final CogToolLID NewTransition =
        new CogToolLID("NewTransition", 201, CAUSES_TRANSITION_SELECTION);

    public static final CogToolLID NewWidget =
        new CogToolLID("NewWidget", 300,
                       COMMITS_CHANGES, CAUSES_WIDGET_SELECTION);

    // NewWidgetJustWarn is for the menu item which doesn't really add a widget
    public static final CogToolLID NewWidgetJustWarn =
        new CogToolLID("NewWidgetJustWarn", 299);

    // TODO: What is this to be used for??? (see ImportImageDirectory)
    public static final CogToolLID Import =
        new CogToolLID("Import", 301, COMMITS_CHANGES, CAUSES_FRAME_SELECTION);

    public static final CogToolLID NudgeUp =
        new CogToolLID("NudgeUp", 302, COMMITS_CHANGES);

    public static final CogToolLID NudgeDown =
        new CogToolLID("NudgeDown", 303, COMMITS_CHANGES);

    public static final CogToolLID NudgeLeft =
        new CogToolLID("NudgeLeft", 304, COMMITS_CHANGES);

    public static final CogToolLID NudgeRight =
        new CogToolLID("NudgeRight", 305, COMMITS_CHANGES);

    public static final CogToolLID SetBackgroundImage =
        new CogToolLID("SetBackgroundImage", 306, COMMITS_CHANGES);

    public static final CogToolLID PasteBackgroundImage =
        new CogToolLID("PasteBackgroundImage", 329, COMMITS_CHANGES);

    public static final CogToolLID SetZoom =
        new CogToolLID("SetZoom", 326);

    public static final CogToolLID ZoomIn =
        new CogToolLID("ZoomIn", 307);

    public static final CogToolLID ZoomOut =
        new CogToolLID("ZoomOut", 308);

    public static final CogToolLID ZoomToFit =
        new CogToolLID("ZoomToFit", 309);

    public static final CogToolLID ZoomNormal =
        new CogToolLID("ZoomNormal", 310);

    public static final CogToolLID BringToFront =
        new CogToolLID("BringToFront", 311);

    public static final CogToolLID BringForward =
        new CogToolLID("BringForward", 312);

    public static final CogToolLID SendBackward =
        new CogToolLID("SendBackward", 313);

    public static final CogToolLID SendToBack =
        new CogToolLID("SendToBack", 314);

    public static final CogToolLID AlignTop =
        new CogToolLID("AlignTop", 315, COMMITS_CHANGES);

    public static final CogToolLID AlignBottom =
        new CogToolLID("AlignBottom", 316, COMMITS_CHANGES);

    public static final CogToolLID AlignLeft =
        new CogToolLID("AlignLeft", 317, COMMITS_CHANGES);

    public static final CogToolLID AlignRight =
        new CogToolLID("AlignRight", 318, COMMITS_CHANGES);

    public static final CogToolLID AlignCenter =
        new CogToolLID("AlignCenter", 331, COMMITS_CHANGES);

    public static final CogToolLID AlignHorizCenter =
        new CogToolLID("AlignHorizCenter", 332, COMMITS_CHANGES);

    public static final CogToolLID AlignVertCenter =
        new CogToolLID("AlignVertCenter", 333, COMMITS_CHANGES);

    public static final CogToolLID SpaceHorizontally =
        new CogToolLID("SpaceHorizontally", 319, COMMITS_CHANGES);

    public static final CogToolLID SpaceVertically =
        new CogToolLID("SpaceVertically", 320, COMMITS_CHANGES);

    public static final CogToolLID SetWidgetColor =
        new CogToolLID("SetWidgetColor", 321);

    public static final CogToolLID MoveWidgets =
        new CogToolLID("MoveWidgets", 322, COMMITS_CHANGES);

    public static final CogToolLID ResizeWidgets =
        new CogToolLID("ResizeWidgets", 323, COMMITS_CHANGES);

    public static final CogToolLID RemoveBackgroundImage =
        new CogToolLID("RemoveBackgroundImage", 324, COMMITS_CHANGES);

    public static final CogToolLID ImportImageDirectory =
        new CogToolLID("ImportImageDirectory", 325,
                       COMMITS_CHANGES, CAUSES_FRAME_SELECTION);

    public static final CogToolLID CopyPath =
        new CogToolLID("CopyPath", 330, NO_COMMITS);

    public static final CogToolLID RenderAll =
        new CogToolLID("RenderAll", 335, COMMITS_CHANGES);

    public static final CogToolLID UnRender =
        new CogToolLID("UnRender", 336, COMMITS_CHANGES);

    // 331-4 in use above!

    // 326 in use above!

    public static final CogToolLID SetAttribute =
        new CogToolLID("SetAttribute", 327);

    public static final CogToolLID CopyImageAsBackground =
        new CogToolLID("CopyImageAsBackground", 328, COMMITS_CHANGES);

    // 329 in use above!

    public static final CogToolLID ShowMin =
        new CogToolLID("ShowMin", 401);

    public static final CogToolLID ShowMax =
        new CogToolLID("ShowMax", 402);

    public static final CogToolLID ShowMean =
        new CogToolLID("ShowMean", 403);

    public static final CogToolLID ShowSum =
        new CogToolLID("ShowSum", 404);

    public static final CogToolLID ShowStandaloneTime =
        new CogToolLID("ShowStandaloneTime", 405);

    public static final CogToolLID ShowUserTime =
        new CogToolLID("ShowUserTime", 406);

    public static final CogToolLID ShowDrivingTime =
        new CogToolLID("ShowDrivingTime", 407);

    public static final CogToolLID ShowAvgLaneDev =
        new CogToolLID("ShowAvgLaneDev", 408);

    public static final CogToolLID ShowMaxLaneDev =
        new CogToolLID("ShowMaxLaneDev", 409);

    public static final CogToolLID ShowAvgReaction =
        new CogToolLID("ShowAvgReaction", 410);

    public static final CogToolLID ShowMaxReaction =
        new CogToolLID("ShowMaxReaction", 411);

    public static final CogToolLID ChangeProperty =
        new CogToolLID("ChangeProperty", 501);

    public static final CogToolLID MoveFrames =
        new CogToolLID("MoveFrames", 601);

    public static final CogToolLID ExportDesignToHTML =
        new CogToolLID("ExportDesignToHTML", 602, COMMITS_CHANGES);

    public static final CogToolLID ExportScriptToCSV =
        new CogToolLID("ExportScriptToCSV", 600, COMMITS_CHANGES);

    public static final CogToolLID ChangeThinkProperties =
        new CogToolLID("ChangeThinkProperties", 603);

    public static final CogToolLID ChangeWaitProperties =
        new CogToolLID("ChangeWaitDuration", 604);

    public static final CogToolLID ExportResultsToCSV =
        new CogToolLID("ExportResultsToCSV", 605);

    public static final CogToolLID ImportWebCrawl =
        new CogToolLID("ImportWebCrawl", 606);

    public static final CogToolLID SkinNone =
        new CogToolLID("No Skin", 701, COMMITS_CHANGES);

    public static final CogToolLID SkinWireFrame =
        new CogToolLID("Wire Frame", 702, COMMITS_CHANGES);

    public static final CogToolLID SkinMacOSX =
        new CogToolLID("Mac OSX", 703, COMMITS_CHANGES);

    public static final CogToolLID SkinWinXP =
        new CogToolLID("Windows XP", 704, COMMITS_CHANGES);

    public static final CogToolLID SkinPalm =
        new CogToolLID("PalmOS", 705, COMMITS_CHANGES);

    public static final CogToolLID SetStartFrame =
        new CogToolLID("Set Start Frame", 801, CAUSES_SELECTION);

    public static final CogToolLID SetMouseHand =
        new CogToolLID("Set Mouse Hand", 802);

    public static final CogToolLID SetHandLocation =
        new CogToolLID("Set Hand Location", 803);

    public static final CogToolLID RegenerateScript =
        new CogToolLID("Regenerate Script(s)", 804);

    public static final CogToolLID ShowModelVisualization =
        new CogToolLID("ShowModelVisualization", 805);

    public static final CogToolLID SetFrameTemplate =
        new CogToolLID("SetFrameTemplate", 806, CAUSES_SELECTION);

    public static final CogToolLID ClearFrameTemplate =
        new CogToolLID("ClearFrameTemplate", 807);

    public static final CogToolLID Group =
        new CogToolLID("Group", 808);

    public static final CogToolLID Ungroup =
        new CogToolLID("Ungroup", 809);

    public static final CogToolLID.ConverterFilesLID NewDesignFromImport =
        new ConverterFilesLID("NewDesign");

    public static class OpenRecentLID extends CogToolLID
    {
        protected static int persistence = 8000;

        public String path;

        public OpenRecentLID(String newLabel, String file)
        {
            super(newLabel, persistence++);

            path = file;
        }
    }

    public static class ConverterFilesLID extends CogToolLID
    {
        protected static int persistence = 9000;

        public Class<ImportConverter> converterClass;

        public ConverterFilesLID(String newLabel)
        {
            super(newLabel, persistence++);
        }
        
        public Class<ImportConverter> getClassAttribute()
        {
            return converterClass;
        }

        public void setClassAttribute(Class<ImportConverter> convClass)
        {
            converterClass = convClass;
        }
    }

    @Override
    public String toString()
    {
        return label;
    }


    public int compareTo(CogToolLID other)
    {
        return ordering - other.ordering;
    }



    @Override
    public boolean equals(Object other)
    {
        return (other != null) &&
               (ordering == ((CogToolLID) other).ordering);
    }

    protected static final CogToolLID[] PERSISTENCE_ORDERING =
        { Help, About/*, TODO: ... */ };

    protected static final CogToolLID[] ITERATOR_ORDERING =
        { Help, About/*, TODO: ... */ };

    public static final List<CogToolLID> VALUES =
        Collections.unmodifiableList(Arrays.asList(ITERATOR_ORDERING));
}
