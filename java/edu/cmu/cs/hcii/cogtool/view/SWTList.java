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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

/**
 * A helper class which is used for managing an SWTTable like a list.
 * Re-architected to enable more control over the row item contents.
 * Now, it is expected that the client will subclass this class and
 * override the <code>renderRow</code> method.
 *
 * @author alexeiser
 */
public class SWTList
{
    /**
     * The underlying table implementation
     */
    protected Table table = null;

    /**
     * A map look up for elements in the table.
     * Holds the data as key, and the table row (TableItem) as value.
     */
    protected Map<Object, TableItem> itemMap = new HashMap<Object, TableItem>();

    /**
     * Simple constructor; requires setting of Table before use!
     */
    public SWTList()
    {
        // Nothing to do; remember to set table before use!
    }

    /**
     * Basic constructor takes a Table T, as its input and creates a new map.
     * takes a SingleColRowPropertyDisplay to handle getting the text needed
     * to fill the table.
     * @param t
     */
    public SWTList(Table t)
    {
        setTable(t);
    }

    /**
     * Reset the associated SWT Table
     */
    public void setTable(Table t)
    {
        if (t == null) {
            throw new IllegalArgumentException("Table cannot be null");
        }

        table = t;
    }

    /**
     * Return the held SWT Table
     */
    public Table getTable()
    {
        return table;
    }

    /**
     * Determine the background color of the given row; if the default color,
     * return <code>null</code>.
     */
    protected Color getRowBackground(TableItem item)
    {
        return null;
    }

    /**
     * Render the given table row; expected that subclasses will override.
     */
    protected void renderRow(TableItem item)
    {
        // Subclasses should override.
        Color c = getRowBackground(item);

        // Set the color; do NOT dispose the color as we don't "own" it.
        if (c != null) {
            item.setBackground(c);
        }
        else {
            // Clear the background color
            item.setBackground(null);
        }
    }

    /**
     * Set the row contents.
     *
     * This is a basic set operation, uses the call back to get the text.
     *
     * Does not set the map.
     *
     * @param item
     * @param data
     */
    protected void setRowContents(TableItem item)
    {
        renderRow(item);
    }

    /**
     * Informs the subclass that the entire table is going to be refreshed
     * and that the items will be "filled in" from the first row to the last.
     */
    protected void notifyTableRefresh()
    {
        // Nothing to do by default.
    }

    protected void verifyTable()
    {
        if (table == null) {
            throw new IllegalStateException("The internal Table has not been set.");
        }
    }

    /**
     * This call will fill the table's rows with the given data.
     *
     * It will reuse table items when possible.
     */
    public void setListContents(Iterator<?> contentObjects)
    {
        verifyTable();

        notifyTableRefresh();

        // Clear the map which stores the
        // look-up of the table cell with the object used to create it
        itemMap.clear();

        TableItem[] tableItems = table.getItems();
        int tableItemIndex = 0;

        while (contentObjects.hasNext()) {
            Object rowData = contentObjects.next();

            TableItem item;

            // If we are setting a row which has already been created,
            // just update the values.
            if (tableItemIndex < tableItems.length) {
                item = tableItems[tableItemIndex++];
            }
            else {
                // These are new rows, create a new row item
                item = new TableItem(table, SWT.NONE);
            }

            // Use the arrayList Item as a key to the table.
            itemMap.put(rowData, item);

            item.setData(rowData);

            doItemAction(item);

            setRowContents(item);
        }

        // Dispose any remaining rows that might be left.
        while (tableItemIndex < tableItems.length) {
            tableItems[tableItemIndex++].dispose();
        }
    }

    protected void doItemAction(TableItem item)
    {
        // do nothing; subclasses override if needed
    }

    /**
     * Add a new item to the list at the specified index
     * The item is automatically selected.
     * @param newListItem Expects a String
     * @param index Specifies the insertion point.
     */
    public void addListItem(Object newListItem, int index)
    {
        verifyTable();

        if (newListItem == null) {
            throw new IllegalArgumentException("Cannot add null to the list");
        }

        TableItem item = new TableItem(table, SWT.NONE, index);

        itemMap.put(newListItem, item);

        item.setData(newListItem);

        doItemAction(item);

        setRowContents(item);
    }

    public void addListItem(Object newListItem)
    {
        addListItem(newListItem, table.getItemCount());
    }

    /**
     * Removes the table row at the specified index.
     *
     * @param removeItemIndex
     */
    public void removeListItem(int removeItemIndex)
    {
        verifyTable();

        TableItem item = table.getItem(removeItemIndex);

        if (item != null) {
            itemMap.remove(item.getData());
            item.dispose();
        }
        else {
            throw new NoSuchElementException("No item with index "
                                                + removeItemIndex
                                                + " exists in the table");
        }
    }

    /**
     * Clear the table of all items.
     * Disposes table rows.
     *
     * Does not clear the map.
     */
    private void clearTable()
    {
        verifyTable();

        TableItem[] items = table.getItems();

        for (TableItem item : items) {
            item.dispose();
        }
    }

    /**
     * Clear the list's contents.
     *
     * Empties and disposes the table rows and the map.
     */
    public void clearListContents()
    {
        clearTable();
        itemMap.clear();
    }

    /**
     * A set Table selected method that uses the tableItem its self.
     *
     * if item is null, a no such element exception is rasied.
     * @param item
     */
    protected void setTableItemSelected(TableItem item)
    {
        verifyTable();

        if (item != null) {
            // Need to put Item in an array
            TableItem[] itemArray = new TableItem[] { item };

            table.setSelection(itemArray); // Clears previous selection
        }
        else {
            throw new NoSuchElementException("Desired Item does not exist "
                                                + "in the table");
        }
    }

    /**
     * Uses an int index, to set the current selected item.
     *
     * If the passed in an index that is not in the list, a
     * NoSuchElementException is thrown
     *
     * @param frameName
     */
    public void setListItemHighlighted(int listItemIndex)
    {
        verifyTable();

        TableItem item = table.getItem(listItemIndex);

        if (item != null) {
            setTableItemSelected(item);
        }
    }

    /**
      * Set a specific item to be selected in the list.
      * @param selectItem
      */
     public void setListItemHighlighted(Object selectItem)
     {
         // Get the table item from the map.
         TableItem item = itemMap.get(selectItem);

         if (item != null) {
             setTableItemSelected(item);
         }
     }

     /**
      * Allow access to deselect items in the table.
      *
      */
     public void deselectAll()
     {
         verifyTable();
         table.deselectAll();
     }

    /**
     * Set a list of items to be selected.
     *
     * If an item is not in the table, an IllegalArgumentException is thrown.
     *
     * @param selectItems
     */
    public void setListItemsHighlighted(Object[] selectItems)
    {
        verifyTable();

        // Create an array of the correct size
        TableItem items[] = new TableItem[selectItems.length];

        // Get the table items needed.
        for(int i = 0; i < selectItems.length; i++) {
            TableItem item = itemMap.get(selectItems[i]);

            // If the item is not in the table, throw exception
            if (item == null) {
                throw new IllegalArgumentException("Attempted to select an object "
                                                        + "that is not in the table.");
            }

            items[i] = item;
        }

        table.setSelection(items);
    }

    public void addListener(int eventType, Listener l)
    {
        verifyTable();
        table.addListener(eventType, l);
    }

    public void removeListener(int eventType, Listener l)
    {
        verifyTable();
        table.removeListener(eventType, l);
    }

    /**
     * Add a new selection listener to the table.
     * @param l
     */
    public void addSelectionListener(SelectionListener l)
    {
        verifyTable();
        table.addSelectionListener(l);
    }

    /**
     * Remove the selection listener from the table.
     * @param l
     */
    public void removeSelectionListener(SelectionListener l)
    {
        verifyTable();
        table.removeSelectionListener(l);
    }

    /**
     * Returns the set of selected table items.
     * Call item.getData() to access the actual object used to
     * make the row. IE the data object.
     * @return
     */
    public TableItem[] getSelectionObject()
    {
        verifyTable();
        return table.getSelection();
    }

    /**
     * On dispose clear the list contents and rows.
     * Do not dispose table in this method. That is the creator of the
     * SWTList's job.
     */
    public void dispose()
    {
        clearListContents();
    }

    /**
     * Returns the ItemMap, which maps the STRING/inputObject to the TableList
     * Note: if using strings, you need the original STRING object otherwise
     * you won't be able to use the map. (instead use iterators)
     * @return
     */
    public Map<Object, TableItem> getItemList()
    {
        return itemMap;
    }

    /**
     * Get the table row item corresponding to the given row data.
     */
    public TableItem getRowItem(Object rowData)
    {
        return itemMap.get(rowData);
    }
}
