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

import java.util.EventObject;
import java.util.Iterator;
import java.util.LinkedHashMap;

import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.ChildWidget;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.uimodel.FrameEltSelnFig;
import edu.cmu.cs.hcii.cogtool.uimodel.GraphicalWidget;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.FilterIterator;

/**
 * Frame editor selection state.
 *
 * Holds the selection. This is done through the use of a map holding
 * the list of selected widgets.  Selection is done through the use of a map
 * between IWidgets and IGraphicalWidgets.
 *
 * @author alexeiser
 *
 */
public class FrameEditorSelectionState extends Alerter 
                                       implements SelectionState
{
    /**
     * Class which handles selection changes.
     * Indicates whether change was "selection" or "deselection".
     * Also indicates if change applies to a single IWidget iff
     * <code>changedWidget</code> is not <code>null</code>.
     *
     * @author alexeiser
     *
     */
    public static class SelectionChange extends EventObject
    {
        /**
         * Is the widget currently selected?
         */
        public boolean selected;

        /**
         * The IGraphicalWidget whose selection is changing
         */
        public FrameEltSelnFig<?> changedElement = null;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param selnState   the FrameEditorSelectionState
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author alex
         */
        public SelectionChange(FrameEditorSelectionState selnState,
                               boolean isSelection)
        {
            super(selnState);

            selected = isSelection;
        }

        /**
         * Initialize the semantic change representing an add or a remove
         * of a single IWidget.
         *
         * @param selnState   the FrameEditorSelectionState
         * @param widget      IGraphicalWidget whose selection state changed
         * @param isSelection a flag indicating whether the change is an add
         *                    or a remove
         * @author alex
         */
        public SelectionChange(FrameEditorSelectionState selnState,
                               FrameEltSelnFig<?> elt,
                               boolean isSelection)
        {
            this(selnState, isSelection);

            changedElement = elt;
        }
    }

    /**
     * Selected Items HashSet, this used to control the list of selected items.
     */
    protected LinkedHashMap<FrameElement, FrameEltSelnFig<?>> selectedElements =
        new LinkedHashMap<FrameElement, FrameEltSelnFig<?>>();

    /**
     * Singleton selection object to use for convenience.
     * Since the IGraphicalWidget is null, this causes all widgets to be
     * unselected.
     */
    protected EventObject offSelectionChangeAlert =
        new SelectionChange(this, false);

    /**
     * Deselect all currently selected widgets. This raises an alert
     * with all currently selected widgets.
     */

    public void deselectAll()
    {
        raiseAlert(offSelectionChangeAlert);

        selectedElements.clear();

        // NOTE: on a deselect all, we need to tell the UI Model that
        // no objects are selected. Previously, we told them we were
        // changing the selection state.. but the selection state was
        // at the time STILL the same.
        // A better solution would be to possibly copy the list
        // of widgets, clear it, then pass them in as an array.
        //
        raiseAlert(offSelectionChangeAlert);
    }

    /**
     * Get the number of selected widgets
     */

    public int getWidgetSelectionCount()
    {
        int count = 0;
        Iterator<IWidget> widgets = getSelectedWidgetsIterator();

        while (widgets.hasNext()) {
            widgets.next();
            count++;
        }

        return count;
    }

    /**
     * Get the number of selected frame elements
     */

    public int getElementSelectionCount()
    {
        return selectedElements.size();
    }

    /**
     * If all of the selected items are widgets with the same
     * parent SimpleWidgetGroup, return that parent group.
     */
    public SimpleWidgetGroup getSelectedWidgetsParent()
    {
        SimpleWidgetGroup parentGroup = null;

        Iterator<FrameElement> selectedElts =
            selectedElements.keySet().iterator();

        while (selectedElts.hasNext()) {
            FrameElement elt = selectedElts.next();

            if (elt instanceof IWidget) {
                SimpleWidgetGroup g = ((IWidget) elt).getParentGroup();

                // Can't share a parent if one is null
                if (g == null) {
                    return null;
                }

                // If no parent recorded yet, keep track
                if (parentGroup == null) {
                    parentGroup = g;
                }
                else if (parentGroup != g) {    // if parents don't match, done
                    return null;
                }
            }
            else {
                // Can't share a parent if not all selected elts are widgets
                return null;
            }
        }

        return parentGroup;
    }

    /**
     * Get the number of selected non-IChildWidget elements
     */

    public int getNonchildSelectionCount()
    {
        int nonchildCount = 0;

        Iterator<FrameElement> allSelectedElements =
            getSelectedElementsIterator();

        while (allSelectedElements.hasNext()) {
            FrameElement w = allSelectedElements.next();

            if (! (w instanceof ChildWidget)) {
                nonchildCount++;
            }
        }

        return nonchildCount;
    }

    /**
     * Returns an iterator of the selected IGraphicalWidgets.
     */
    @SuppressWarnings("unchecked")
    public Iterator<GraphicalWidget<?>> getSelectedWidgetFigures()
    {
        Iterator<FrameEltSelnFig<?>> selectedWidgetFigIterator =
            selectedElements.values().iterator();

        return new FilterIterator(selectedWidgetFigIterator,
                                  GraphicalWidget.class);
    }

    /**
     * Return an iterator of the IWidget objects
     */

    public Iterator<IWidget> getSelectedWidgetsIterator()
    {
        Iterator<FrameElement> selectedWidgetIterator =
            selectedElements.keySet().iterator();

        return new FilterIterator<IWidget>(selectedWidgetIterator,
                                           IWidget.class);
    }

    /**
     * Returns an iterator of the selected IFrameEltSelnFigs.
     */
    public Iterator<FrameEltSelnFig<?>> getSelectedFigures()
    {
        return selectedElements.values().iterator();
    }

    /**
     * Return an iterator of the IFrameElement objects
     */

    public Iterator<FrameElement> getSelectedElementsIterator()
    {
        return selectedElements.keySet().iterator();
    }

    /**
     * Determines if the passed-in IFrameEltSelnFig is part of the selected
     * list. This returns true if the Model of the figure is in the list.
     *
     * @param gw
     * @return
     */
    public boolean isSelectionFigureSelected(FrameEltSelnFig<?> fig)
    {
        // Use the map look-up for determining if element is selected.
        return (selectedElements.get(fig.getModel()) != null);
    }

    /**
     * Determine if the passed in IFrameElement is selected.
     */

    public boolean isElementSelected(FrameElement element)
    {
        return selectedElements.get(element) != null;
    }

    /**
     * Set the passed-in element as selected.
     * Deselects all other elements.
     * Raises an alert with the selection change event.
     */
    public void setSelectedSelnFig(FrameEltSelnFig<?> fig)
    {
        // Remove all elements from the selected list.
        // This will also cause their visual properties to change
        // so they will look unselected
        deselectAll();

        selectSelnFig(fig);
    }

    /**
     * Set the passed-in element to be unselected.
     * Raises an event to indicate the graphical element associated with this
     * frame element is no longer selected.
     * Returns the graphical element deselected.
     */
    public FrameEltSelnFig<?> deselectElement(FrameElement elt)
    {
        FrameEltSelnFig<?> fig = selectedElements.remove(elt);

        if (fig != null) {
            raiseAlert(new SelectionChange(this, fig, false));
        }

        return fig;
    }

    /**
     * Set the passed-in element as unselected.
     * raises an event to indicate this element is no longer selected.
     */
    public void deselectSelnFig(FrameEltSelnFig<?> fig)
    {
        if (fig != null) {
            deselectElement(fig.getModel());
        }
    }

    /**
     * Add the passed frame element (i.e. widget or group) to the list of
     * selected elements; raises an event with this element as being selected.
     */
    public void selectSelnFig(FrameEltSelnFig<?> fig)
    {
        selectedElements.put(fig.getModel(), fig);

        raiseAlert(new SelectionChange(this, fig, true));
    }

    /**
     * Return the array of IWidgets currently selected.
     */

    public IWidget[] getSelectedIWidgets()
    {
        IWidget[] widgets = new IWidget[getWidgetSelectionCount()];
        Iterator<IWidget> widgetIter = getSelectedWidgetsIterator();
        int i = 0;

        while (widgetIter.hasNext()) {
            widgets[i++] = widgetIter.next();
        }

        return widgets;
    }

    /**
     * Return the array of IFrameElements currently selected.
     */

    public FrameElement[] getSelectedIFrameElements()
    {
        FrameElement[] elts = new FrameElement[getElementSelectionCount()];

        selectedElements.keySet().toArray(elts);

        return elts;
    }
}
