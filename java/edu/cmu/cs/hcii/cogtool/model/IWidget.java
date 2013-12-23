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


/**
 * Widget objects represent parts of a user interface that a user can
 * "manipulate" in order to cause changes in the user interface's state.
 * It is expected that such Widgets may be "controlled" via a mouse or other
 * pointing and selecting device.
 *
 * @author mlh
 */
public interface IWidget extends TransitionSource, FrameElement
{
    public AShape getShape();
    public void setShape(AShape s);

    public WidgetType getWidgetType();
    public void setWidgetType(WidgetType t);

    public boolean sameLocation(IWidget r);
    public boolean sameName(IWidget r);

    /**
     * Set the level of the widget.
     * The level controls the order of visibility.
     * Sometimes hitting the level will occur a lot.
     *
     * You never want 2 objects on the same level. (This is not a hard rule,
     * just a recommendation)
     *
     * @param newLevel
     */
    public void setLevel(int newLevel);

    /**
     * Get the level for this widget. See set Level for more info.
     * @return The level of this widget
     */
    public int getLevel();

    /**
     * Set the widget to the specified X, Y Origin.
     *
     * Negative values can be used.
     *
     * @param x
     * @param y
     */
    public void setWidgetOrigin(double x, double y);

    /**
     * @param newWidth
     * @param newHeight
     */
    public void setWidgetSize(double newWidth, double newHeight);

    /**
     * Get the background image for the widget.
     * @return the background image; null if none.
     */
    public byte[] getImage();

    /**
     * Set the background image for the widget
     * @param img the new background image, or null.
     */
    public void setImage(byte[] img);

    /**
     * Reshapes the graphical widget while preserving its bounds
     * @param newtype the type of the desired shape
     */
    public void setShapeType(ShapeType newtype);

    /**
     * Returns the title of the Widget.
     *
     * @return the Widget's title
     */
    public String getTitle();

    /**
     * Updates the title of the Widget.
     *
     * @param newtitle the new title for the Widget
     */
    public void setTitle(String newtitle);

    /**
     * Indicate whether the widget is rendered using the Design's skin.
     */
    public boolean isRendered();

    /**
     * Set whether the widget is rendered using the Design's skin.
     */
    public void setRendered(boolean render);

    public boolean isIdentical(IWidget r);

    /**
     * Return the grouping this widget belongs to; may be <code>null</code>.
     * Note that WidgetGroups are NOT the same as IFrameElementGroups!
     */
    public SimpleWidgetGroup getParentGroup();

    /**
     * Set the grouping this widget now belongs to.
     * @param newParentGroup the group to contain this IWidget
     */
    public void setParentGroup(SimpleWidgetGroup newParentGroup);

    /**
     * Returns true if is a standard widget, false if a custom widget
     */
    //How is this function working ?? BOGUS
    public boolean isStandard();

    /**
     * Create a "deep" copy of this widget.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Frame).
     *
     * @param duplicator the manager of duplicate Frame instances
     * @return the widget copy
     * @author mlh
     */
    public IWidget duplicate(Frame.IFrameDuplicator duplicator);

    /**
     * Get the additional text for the element.
     */
    public String getAuxiliaryText();

    /**
     * Set the additional text for the element
     */
    public void setAuxiliaryText(String newAuxText);

}
