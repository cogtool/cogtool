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
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.util.EmptyIterator;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil.SWTWidthComputer;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;

public abstract class TreeItemUpdater<T, C>
{
    protected static IEllipsizer ellipsizer =
        new SWTStringUtil.SWTEllipsizer(SWTWidthComputer.DEFAULT_WIDTH,
                                        StringUtil.NO_FRONT,
                                        FontUtils.SYMBOL_FONT);

    // For use by subclasses that have no children
    protected Iterator<C> NO_CHILDREN = new EmptyIterator<C>();

    protected abstract TreeItem createNextItem(T o);
    protected abstract String getObjectText(T o);

    protected TreeItem[] getUpdateItems()
    {
        // Whenever updateTree() is called on an instance,
        // this method MUST be overridden!
        return null;
    }

    protected Iterator<C> getObjectChildren(T o)
    {
        // Assumes no children here; if there are children for
        // a subclass, this should be overridden
        return NO_CHILDREN;
    }

    protected TreeItemUpdater<C, ?> getChildUpdater(TreeItem row)
    {
        // Assumes no children here; if there are children for
        // a subclass, this should be overridden
        return null;
    }

    protected void populateExtraItems(Iterator<? extends T> objects)
    {
        while (objects.hasNext()) {
            T o = objects.next();
            String text = getObjectText(o);
            TreeItem subrow = createNextItem(o);

            subrow.setData(o);
            if (text != null) {
                subrow.setText(text);
            }
        }
    }

    public void updateTree(Iterator<? extends T> objects)
    {
        updateTreeItems(getUpdateItems(), objects);
    }

    protected void updateTreeItems(TreeItem[] items,
                                   Iterator<? extends T> objects)
    {
        for (int i = 0; i < items.length; i++) {
            if (objects.hasNext()) {
                T o = objects.next();
                String text = getObjectText(o);

                if (! (text.equals(items[i].getText()))) {
                    items[i].setText(text);
                    items[i].setData(o);
                }

                TreeItem[] subrows = items[i].getItems();
                Iterator<C> childObjects = getObjectChildren(o);
                TreeItemUpdater<C, ?> updater = getChildUpdater(items[i]);

                if (updater != null) {
                    updater.updateTreeItems(subrows, childObjects);
                }
            }
            else {
                items[i].dispose();
            }
        }

        populateExtraItems(objects);
    }

    public static class TransitionItemUpdater
                                   extends TreeItemUpdater<Transition, Object>
    {
        protected TreeItem row;

        public void resetRow(TreeItem newRow)
        {
            this.row = newRow;
        }

        @Override
        protected TreeItem createNextItem(Transition o)
        {
            TreeItem subrow = new TreeItem(this.row, SWT.NONE);
            subrow.setFont(FontUtils.SYMBOL_FONT);

            return subrow;
        }

        @Override
        protected String getObjectText(Transition o)
        {
            return o.toString(false, ellipsizer);
        }
    }

    public static final TransitionItemUpdater TRANSITION_ITEM_UPDATER =
        new TransitionItemUpdater();

    public static class WidgetItemUpdater
                        extends TreeItemUpdater<TransitionSource, Transition>
    {
        protected Tree widgetTree = null;
        protected TreeItem parentRow = null;

        public WidgetItemUpdater(Tree tree)
        {
            this.widgetTree = tree;
        }

        public WidgetItemUpdater(TreeItem row)
        {
            this.parentRow = row;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            return this.widgetTree.getItems();
        }

        @Override
        protected TreeItem createNextItem(TransitionSource o)
        {
            TreeItem row = (this.widgetTree == null)
                ? new TreeItem(this.parentRow, SWT.NONE)
                : new TreeItem(this.widgetTree, SWT.NONE);

            Iterator<Transition> transitions = getObjectChildren(o);

            getChildUpdater(row).populateExtraItems(transitions);

            return row;
        }

        @Override
        protected String getObjectText(TransitionSource o)
        {
            return o.getNameLabel();
        }

        @Override
        protected Iterator<Transition> getObjectChildren(TransitionSource o)
        {
            return o.getTransitions().values().iterator();
        }

        @Override
        protected TreeItemUpdater<Transition, ?> getChildUpdater(TreeItem row)
        {
            TRANSITION_ITEM_UPDATER.resetRow(row);
            return TRANSITION_ITEM_UPDATER;
        }
    }

    public static class FrameEltItemUpdater
                            extends TreeItemUpdater<FrameElement, Transition>
    {
        protected Tree eltGroupTree = null;
        protected TreeItem row = null;

        public FrameEltItemUpdater()
        {
            // resetRow will set the row properly
        }

        public FrameEltItemUpdater(Tree tree)
        {
            this.eltGroupTree = tree;
        }

        public void resetRow(TreeItem newRow)
        {
            this.row = newRow;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            if (this.eltGroupTree != null) {
                return this.eltGroupTree.getItems();
            }

            return super.getUpdateItems();
        }

        @Override
        protected TreeItem createNextItem(FrameElement o)
        {
            if (this.eltGroupTree != null) {
                TreeItem row = new TreeItem(this.eltGroupTree, SWT.NONE);

                Iterator<Transition> transitions = getObjectChildren(o);

                TreeItemUpdater<Transition, ?> updater = getChildUpdater(row);

                if (updater != null) {
                    updater.populateExtraItems(transitions);
                }

                return row;
            }

            TreeItem subrow = new TreeItem(this.row, SWT.NONE);
            subrow.setFont(FontUtils.SYMBOL_FONT);

            return subrow;
        }

        @Override
        protected String getObjectText(FrameElement o)
        {
            String text = (o instanceof TransitionSource ?
                               ((TransitionSource)o).getNameLabel() :
                               o.getName());

            if (text == null) {
                if (o instanceof SimpleWidgetGroup) {
                    return "[anonymous group]";
                }

                if (o instanceof FrameElementGroup) {
                    return "[unnamed group]";
                }

                return "[UNNAMED OBJECT; COMPLAIN!]";
            }

            return text;
        }

        @Override
        protected Iterator<Transition> getObjectChildren(FrameElement o)
        {
            if ((this.eltGroupTree != null) && (o instanceof IWidget)) {
                return ((IWidget) o).getTransitions().values().iterator();
            }

            return super.getObjectChildren(o);
        }

        @Override
        protected TreeItemUpdater<Transition, ?> getChildUpdater(TreeItem row)
        {
            if ((this.eltGroupTree != null) &&
                (row.getData() instanceof IWidget))
            {
                TRANSITION_ITEM_UPDATER.resetRow(row);
                return TRANSITION_ITEM_UPDATER;
            }

            return super.getChildUpdater(row);
        }
    }

    public static final FrameEltItemUpdater FRAME_ELT_ITEM_UPDATER =
        new FrameEltItemUpdater();

    public static class EltGroupItemUpdater
                     extends TreeItemUpdater<FrameElementGroup, FrameElement>
    {
        protected Tree eltGroupTree = null;

        public EltGroupItemUpdater(Tree tree)
        {
            this.eltGroupTree = tree;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            return this.eltGroupTree.getItems();
        }

        @Override
        protected TreeItem createNextItem(FrameElementGroup o)
        {
            TreeItem row = new TreeItem(this.eltGroupTree, SWT.NONE);
            Iterator<FrameElement> elements = getObjectChildren(o);
            getChildUpdater(row).populateExtraItems(elements);
            return row;
        }

        @Override
        protected String getObjectText(FrameElementGroup o)
        {
            String text = o.getName();
            if (text == null) {
                text = "[unnamed group]";
            }
            return text;
        }

        @Override
        protected Iterator<FrameElement> getObjectChildren(FrameElementGroup o)
        {
            return o.iterator();
        }

        @Override
        protected TreeItemUpdater<FrameElement, Transition> getChildUpdater(TreeItem row)
        {
            FRAME_ELT_ITEM_UPDATER.resetRow(row);
            return FRAME_ELT_ITEM_UPDATER;
        }
    }
    
    public static class ImplicitItemUpdater extends TreeItemUpdater<IWidget, Transition>
    {
        protected Tree groupTree = null;
        protected TreeItem row = null;

        public ImplicitItemUpdater()
        {
            // resetRow will set the row properly
        }

        public ImplicitItemUpdater(Tree tree)
        {
            this.groupTree = tree;
        }

        public void resetRow(TreeItem newRow)
        {
            this.row = newRow;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            if (this.groupTree != null) {
                return this.groupTree.getItems();
            }
            return super.getUpdateItems();
        }

        @Override
        protected TreeItem createNextItem(IWidget o)
        {
            if (this.groupTree != null) {
                TreeItem row = new TreeItem(this.groupTree, SWT.NONE);
                Iterator<Transition> transitions = getObjectChildren(o);
                TreeItemUpdater<Transition, ?> updater = getChildUpdater(row);
                if (updater != null) {
                    updater.populateExtraItems(transitions);
                }
                return row;
            }
            TreeItem subrow = new TreeItem(this.row, SWT.NONE);
            subrow.setFont(FontUtils.SYMBOL_FONT);
            return subrow;
        }

        @Override
        protected String getObjectText(IWidget o)
        {
            String text = ((TransitionSource)o).getNameLabel();
            if (text == null) {
                if (o instanceof SimpleWidgetGroup) {
                    return "[anonymous group]";
                }

                if (o instanceof FrameElementGroup) {
                    return "[unnamed group]";
                }

                return "[UNNAMED OBJECT; COMPLAIN!]";
            }
            return text;
        }

        @Override
        protected Iterator<Transition> getObjectChildren(IWidget o)
        {
            if ((this.groupTree != null)) {
                return o.getTransitions().values().iterator();
            }
            return super.getObjectChildren(o);
        }

        @Override
        protected TreeItemUpdater<Transition, ?> getChildUpdater(TreeItem row)
        {
            if ((this.groupTree != null) &&
                    (row.getData() instanceof IWidget))
            {
                TRANSITION_ITEM_UPDATER.resetRow(row);
                return TRANSITION_ITEM_UPDATER;
            }
            return super.getChildUpdater(row);
        }
    }

    public static final ImplicitItemUpdater IMPLICIT_ITEM_UPDATER = new ImplicitItemUpdater();

    
    public static class ImplicitGroupItemUpdater
            extends TreeItemUpdater<SimpleWidgetGroup, IWidget>
    {
        protected Tree implicitGroupTree = null;

        public ImplicitGroupItemUpdater(Tree tree)
        {
            this.implicitGroupTree = tree;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            return this.implicitGroupTree.getItems();
        }

        @Override
        protected TreeItem createNextItem(SimpleWidgetGroup o)
        {
            TreeItem row = new TreeItem(this.implicitGroupTree, SWT.NONE);
            Iterator<IWidget> elements = getObjectChildren(o);
            getChildUpdater(row).populateExtraItems(elements);
            return row;
        }

        @Override
        protected String getObjectText(SimpleWidgetGroup o)
        {
            String text = o.getName();
            if (text == null) {
                text = "[unnamed group]";
            }
            return text;
        }

        @Override
        protected Iterator<IWidget> getObjectChildren(SimpleWidgetGroup o)
        {
            return o.iterator();
        }

        @Override
        protected TreeItemUpdater<IWidget, Transition> getChildUpdater(TreeItem row)
        {
            IMPLICIT_ITEM_UPDATER.resetRow(row);
            return IMPLICIT_ITEM_UPDATER;
        }
    }

    public static class FrameItemUpdater
                             extends TreeItemUpdater<Frame, TransitionSource>
    {
        protected Tree frameTree = null;

        public FrameItemUpdater(Tree tree)
        {
            this.frameTree = tree;
        }

        @Override
        protected TreeItem[] getUpdateItems()
        {
            return this.frameTree.getItems();
        }

        @Override
        protected TreeItem createNextItem(Frame o)
        {
            TreeItem row = new TreeItem(this.frameTree, SWT.NONE);

            Iterator<TransitionSource> widgets = getObjectChildren(o);

            getChildUpdater(row).populateExtraItems(widgets);

            return row;
        }

        @Override
        protected String getObjectText(Frame o)
        {
            return SWTStringUtil.insertEllipsis(o.getName(),
                                                StringUtil.NO_FRONT,
                                                SWTStringUtil.DEFAULT_FONT);
        }

        @Override
        protected Iterator<TransitionSource> getObjectChildren(Frame f)
        {
            Set<TransitionSource> children =
                new LinkedHashSet<TransitionSource>();

            children.addAll(f.getWidgets());
            children.addAll(f.getInputDevices());

            return children.iterator();
        }

        @Override
        protected TreeItemUpdater<TransitionSource,
                                  Transition> getChildUpdater(TreeItem row)
        {
            return new WidgetItemUpdater(row);
        }
    }
}