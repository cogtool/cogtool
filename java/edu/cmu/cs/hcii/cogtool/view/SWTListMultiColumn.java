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

package edu.cmu.cs.hcii.cogtool.view;

import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

/**
 * SWT List multi column is an extension to SWT List which has support for
 * drawing multiple items on a single row. It's a helper function which
 * handles adding items to an SWT table.
 *
 * To use this you must have a class which extends RowPropertyDisplay.
 * This callback provides methods which allow color and text to be set on a row
 * by row basis.
 *
 * @author alexeiser
 */
public class SWTListMultiColumn extends SWTList
{
    /**
     * The number of columns to use in this table.
     * This is dictated by the number of column titles provided
     */
    protected int numCols = 0;

    /**
     * This constructor assumes no titles, but needs a number of columns
     */
    public SWTListMultiColumn(int numColumns)
    {
        super();

        numCols = numColumns;
    }

    /**
     * This constructor takes a table and a set of strings to be used
     * for the titles of the table.
     */
    public SWTListMultiColumn(Table t, String[] colTitles)
    {
        super(t);

        if ((colTitles != null) && (colTitles.length > 0)) {
            throw new IllegalArgumentException("There must be some titles");
        }

        // Store the number of desired columns
        numCols = colTitles.length;

        setColumnTitles(colTitles);
    }

    public void setColumnTitles(String[] colTitles)
    {
        verifyTable();

        if (colTitles.length != numCols) {
            throw new IllegalStateException("Given column titles are too few or too many");
        }

        // Set the table header to be visible
        table.setHeaderVisible(true);

        // Assign the strings passed in by the caller to the table headers.
        // Also create the columns them selves.
        for (String colTitle : colTitles) {
            TableColumn column = new TableColumn(table, SWT.NONE);

            column.setText(colTitle);
            column.pack();
        }
    }

    /**
     * Helper function to pack all columns. This ensures that the columns can
     * always show all of the text in them.
     */
    protected void packCols()
    {
        for (int i = 0; i < numCols; i++) {
            table.getColumn(i).pack();
        }
    }

    /**
     * Render the specified column in the given row;
     * expected that subclasses will override.
     */
    protected void renderRowColumn(TableItem item, int column)
    {
        // Subclasses should override.
    }

    /**
     * Render the given table row; expected that subclasses will override.
     */
    @Override
    protected void renderRow(TableItem item)
    {
        // For each string in the array of texts, set them to the tablerow.
        for (int i = 0; i < numCols; i++) {
            renderRowColumn(item, i);
        }

        super.renderRow(item);
    }

    /**
     * This call must be made to fill the table.
     *
     * The parent class is used to fill the table, but each call goes to the
     * local setRowContents call.
     *
     * Finally pack columns to ensure each cell is visible.
     *
     * @param frames
     */
    @Override
    public void setListContents(Iterator<?> items)
    {
        super.setListContents(items);

        // Update the col lengths for the new items
        packCols();
    }

    /**
     * Add a new item to that list.
     * The item is automatically selected.
     * @param newListItem expects IMultiColumnData
     */
    @Override
    public void addListItem(Object data, int index)
    {
        super.addListItem(data, index);

        // Update the col lengths for the new items
        packCols();
    }
}
