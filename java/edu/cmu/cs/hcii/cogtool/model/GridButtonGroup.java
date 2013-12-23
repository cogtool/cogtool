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
import java.util.Iterator;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ReadOnlyList;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;

/**
 * Support for managing a group of buttons that can be arranged in a grid,
 * i.e. radio buttons or check boxes.
 */
public class GridButtonGroup extends SimpleWidgetGroup
{
    public static final int edu_cmu_cs_hcii_cogtool_model_GridButtonGroup_version = 0;

    protected static final String startXVar = "startX";
    protected static final String startYVar = "startY";

    private static ObjectSaver.IDataSaver<GridButtonGroup> SAVER =
        new ObjectSaver.ADataSaver<GridButtonGroup>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_GridButtonGroup_version;
            }

            @Override
            public void saveData(GridButtonGroup v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveDouble(v.startX, startXVar);
                saver.saveDouble(v.startY, startYVar);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(GridButtonGroup.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<GridButtonGroup> LOADER =
        new ObjectLoader.AObjectLoader<GridButtonGroup>() {
            @Override
            public GridButtonGroup createObject()
            {
                return new GridButtonGroup();
            }

            @Override
            public void set(GridButtonGroup target, String variable, double value)
            {
                if (variable != null) {
                    if (variable.equals(startXVar)) {
                        target.startX = value;
                    }
                    else if (variable.equals(startYVar)) {
                        target.startY = value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(GridButtonGroup.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_GridButtonGroup_version,
                                    LOADER);
    }

    /**
     * These variables save the top left corner of the first widget in the
     * group, so it can tell whether a widget is in the first row or first column.
     */
    protected double startX = 0.0;
    protected double startY = 0.0;

    public static final double PIXEL_EPSILON = 10.0;

    public GridButtonGroup()
    {
        super();
    }

    /**
     * To support twinning
     */
    public GridButtonGroup(double x, double y)
    {
        super();
        setStart(x, y);
    }


    public double getStartX()
    {
        return startX;
    }


    public void setStartX(double x)
    {
        startX = x;
    }


    public double getStartY()
    {
        return startY;
    }


    public void setStartY(double y)
    {
        startY = y;
    }


    public void setStart(double x, double y)
    {
        startX = x;
        startY = y;
    }

    @Override
    public void add(int index, IWidget widget)
    {
        super.add(index, widget);

        if (index == 0) {
            DoublePoint pt = widget.getShape().getOrigin();

            setStart(pt.x, pt.y);
        }
    }

    /**
     * TODO: Temporary hack to let clipboard operation work
     */
    @Override
    public void simpleAddWidget(IWidget widget)
    {
        if (size() == 0) {
            DoublePoint pt = widget.getShape().getOrigin();

            setStart(pt.x, pt.y);
        }

        super.simpleAddWidget(widget);
    }

    /**
     * Return a list of widgets that are in the same column as gb
     */

    public List<GridButton> getColumn(GridButton gb)
    {
        List<GridButton> result = new ArrayList<GridButton>();
        Iterator<IWidget> buttons = iterator();
        double x = gb.getEltBounds().x;

        while (buttons.hasNext()) {
            GridButton w = (GridButton) buttons.next();
            double curX = w.getEltBounds().x;

            if (PrecisionUtilities.withinEpsilon(curX, x, GridButtonGroup.PIXEL_EPSILON)) {
                result.add(w);
            }
        }

        return result;
    }

    /**
     * Given the move direction and the current position of the widget being
     * moved, return a list of all widgets in the group that should be moved
     * as well.
     */

    public ReadOnlyList<GridButton> getMovedButtons(boolean moveIsVertical,
                                                      double x,
                                                      double y)
    {
        List<GridButton> result = new ArrayList<GridButton>();
        Iterator<IWidget> buttons = iterator();

        while (buttons.hasNext()) {
            GridButton w = (GridButton) buttons.next();
            DoubleRectangle widgetBds = w.getEltBounds();

            double curX = widgetBds.x;
            double curY = widgetBds.y;

            if (moveIsVertical) {
                // Only moving widgets below the sash in this column
                if (PrecisionUtilities.withinEpsilon(x, curX, GridButtonGroup.PIXEL_EPSILON) &&
                    (curY >= y))
                {
                    result.add(w);
                }
            }
            else {
                // horizontal; moving any widget to the right of the sash
                // or any in the same column
                if (PrecisionUtilities.withinEpsilon(x, curX, GridButtonGroup.PIXEL_EPSILON) ||
                    (curX > x))
                {
                    result.add(w);
                }
            }
        }

        return new ReadOnlyList<GridButton>(result);
    }

    /**
     * Called when a new group is formed from a copy of a subset of an existing
     * group, and the widgets need to be positioned adjacently based on which
     * column they were in.
     */

    public void recalculateOffsets()
    {
        GridButton first = (GridButton) members.get(0);
        double curX = startX;
        double curY = startY;

        // all widgets in the group are the same size
        DoubleRectangle bounds = first.getEltBounds();
        double width = bounds.width;
        double height = bounds.height;
        double firstColX = curX;

        first.setHorizSpace(0.0);
        first.setVertSpace(0.0);

        for (int i = 1; i < members.size(); i++) {
            GridButton curButton = (GridButton) members.get(i);
            DoubleRectangle bds = curButton.getEltBounds();

            if (PrecisionUtilities.withinEpsilon(curX, bds.x, GridButtonGroup.PIXEL_EPSILON)) {
                curY += height + curButton.getVertSpace();

                if (curX == firstColX) {
                    curButton.setHorizSpace(0.0);
                }
            }
            else {
                // new column; move x over, reset y
                curX += width + curButton.getHorizSpace();
                curY = startY;
                curButton.setVertSpace(0.0);
            }

            curButton.setWidgetOrigin(curX, curY);
        }
    }

    /**
     * Copy the attributes but do not copy the list of widgets; the group
     * twin will be populated later.
     */
    @Override
    public SimpleWidgetGroup twin()
    {
        GridButtonGroup groupTwin =
            new GridButtonGroup(getStartX(), getStartY());

        groupTwin.twinState(this);

        return groupTwin;
    }
}
