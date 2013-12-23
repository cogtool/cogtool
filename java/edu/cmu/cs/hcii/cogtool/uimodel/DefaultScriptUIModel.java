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

package edu.cmu.cs.hcii.cogtool.uimodel;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.TableItem;

import edu.cmu.cs.hcii.cogtool.model.DefaultModelGeneratorState;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Demonstration;
import edu.cmu.cs.hcii.cogtool.model.LookAtScriptStep;
import edu.cmu.cs.hcii.cogtool.model.AScriptStep;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.TextActionSegment;
import edu.cmu.cs.hcii.cogtool.model.ThinkScriptStep;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.view.SWTListMultiColumn;

public class DefaultScriptUIModel extends DefaultUIModel
{
    /**
     * Fetch a localized string that may be used to display the action
     * of the associated script step.  NOTE!  This may be different
     * than the result of invoking <code>getLocalizedString()</code>
     * on the referenced AScriptStep since more information is
     * available from the state (such as the need to "Move" before the action).
     */
    protected static String computeActionString(AScriptStep ss,
                                                IWidget lastMovedToWidget)
    {
        String actionStr = ss.getLocalizedString();

        if (ss instanceof TextActionSegment) {
            return KeyDisplayUtil.convertActionToDisplay(actionStr);
        }

        if ((ss instanceof LookAtScriptStep) ||
            (ss instanceof ThinkScriptStep))
        {
            return actionStr;
        }

        return ss.getLocalizedActionString(actionStr, lastMovedToWidget);
    }

    /**
     * This does not need to be disposed since it's a static.
     */
    protected static final Color autoInsertedScriptStepColor =
        new Color(null, 229, 229, 229);

    /**
     * This does not need to be disposed since it's a static.
     */
    protected static final Color generatedScriptStepColor =
        new Color(null, 255, 255, 204);

    /**
     * Do not dispose since it is a static.
     * Color indicating that the owning step's definition has changed
     * in an "obsoleting" fashion; script needs to be regenerated.
     */
    protected static final Color obsoletingScriptStepColor =
        new Color(null, 153, 204, 255);

    /**
     * This color does not need to be disposed since it's static.
     * This color indicates that the item is "INVALID"
     *
     * TODO 100% red, probably will not be visible to red color blind people
     */
    protected static final Color invalidScriptStepColor =
        new Color(null, 255, 102, 0);

    public static class RenderScriptTable extends SWTListMultiColumn
    {
        protected Demonstration initialDemo;

        // Assuming we only refresh the entire list!
        protected boolean isObsoleting = false;
        protected boolean isInvalid = false;
        protected IWidget lastMovedToWidget = null;

        public RenderScriptTable(int numColumns, Demonstration demo)
        {
            super(numColumns);

            initialDemo = demo;
        }

        @Override
        protected void notifyTableRefresh()
        {
            Frame startFrame = initialDemo.getStartFrame();
            Design design = initialDemo.getTaskApplication().getDesign();

            isInvalid = (! initialDemo.isEditable() &&
                              initialDemo.isInvalid()) ||
                             ! design.containsFrame(startFrame);
            isObsoleting = false;
            lastMovedToWidget =
                initialDemo.getInitialState().getLastMovedToWidget();
        }

        @Override
        protected void renderRowColumn(TableItem item, int column)
        {
            Object itemData = item.getData();

            String columnText = null;

            if (itemData instanceof Frame) {
                if (column == 0) {
                    columnText =
                        isInvalid ? "X"
                                       : (isObsoleting ? "?" : "");
                }
                else if (column == 1) {
                    // even though it's the last line, have to abbreviate the
                    // string here too because the column width will be too
                    // long
                    columnText =
                        SWTStringUtil.insertEllipsis(((Frame) itemData).getName(),
                                                     150,
                                                     StringUtil.NO_FRONT,
                                                     item.getFont());
                }
                else {
                    columnText = "";
                }
            }
            else if (itemData instanceof DefaultModelGeneratorState) {
                DefaultModelGeneratorState stepState =
                    (DefaultModelGeneratorState) itemData;
                AScriptStep ss = stepState.getScriptStep();

                switch (column) {
                    case 0: {
                        AScriptStep owner = ss.getOwner();

                        if (! isInvalid) {
                            isInvalid = owner.isInvalid();
                        }
                        if (! isObsoleting) {
                            isObsoleting = owner.isObsolete();
                        }

                        columnText =
                            isInvalid ? "X"
                                           : (isObsoleting ? "?" : "");
                        break;
                    }
                    case 1: {
                        // make room for the action and widget names
                        columnText =
                            SWTStringUtil.insertEllipsis(ss.getCurrentFrame().getName(),
                                                         150,
                                                         StringUtil.NO_FRONT,
                                                         item.getFont());
                        break;
                    }
                    case 2: {
                        columnText =
                            computeActionString(ss, lastMovedToWidget);

                        break;
                    }
                    case 3: {
                        columnText =
                            SWTStringUtil.insertEllipsis(ss.getLocalizedFocusString(),
                                                         StringUtil.NO_FRONT,
                                                         item.getFont());

                        lastMovedToWidget =
                            stepState.getLastMovedToWidget();
                        break;
                    }
                }
            }

            if (columnText != null) {
                item.setText(column, columnText);
            }
        } // renderRowColumn

        @Override
        protected Color getRowBackground(TableItem item)
        {
            Object itemData = item.getData();

            Color c = null;

            if (itemData instanceof DefaultModelGeneratorState) {
                AScriptStep ss =
                    ((DefaultModelGeneratorState) itemData).getScriptStep();
                AScriptStep owner = ss.getOwner();

                if (isInvalid || owner.isInvalid()) {
                    return invalidScriptStepColor;
                }

                if (isObsoleting || owner.isObsolete()) {
                    return obsoletingScriptStepColor;
                }

                if (! ss.isInsertedByUser()) {
                    if (ss.isDemonstrated()) {
                        return autoInsertedScriptStepColor;
                    }

                    return generatedScriptStepColor;
                }
            }
            else if (itemData instanceof Frame) {
                return isInvalid
                           ? invalidScriptStepColor
                           : (isObsoleting ? obsoletingScriptStepColor
                                                : null);
            }

            return c;
        }
    }

    protected RenderScriptTable rowRenderer;

    public DefaultScriptUIModel(Project proj)
    {
        super(proj);
    }

    /**
     * Implementation of the row renderer.
     */
    public RenderScriptTable getRowRenderer()
    {
        return rowRenderer;
    }
}
