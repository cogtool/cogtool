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

import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.draw2d.geometry.Translatable;

import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;

/**
* @author alexeiser
 *
 */
public class ScalableGraphicalFigure extends Figure implements CogToolScalableFigure
{
    protected double scale = 1.0;

    /**
     * Returns the area occupied by the children in UNSCALED coordinates
     */
    public Rectangle getChildrenUtilizedAreaUnscaled()
    {
        Rectangle r = null;

        Iterator<?> iter = getChildren().iterator();

        while (iter.hasNext()) {
            IFigure fi = (IFigure) iter.next();

            if (r == null) {
                r = new Rectangle(fi.getBounds());
            }
            else {
                r.union(fi.getBounds());
            }
        }

        return (r == null) ? new Rectangle(0, 0, 0, 0) : r;
    }

    /**
     * Returns the area occupied by the children in SCALED coordinates
     */
    public Rectangle getChildrenUtilizedAreaScaled()
    {
        Rectangle r = getChildrenUtilizedAreaUnscaled();

        int right = PrecisionUtilities.round((r.x + r.width) * scale);
        int bottom = PrecisionUtilities.round((r.y + r.height) * scale);

        r.x = PrecisionUtilities.round(r.x * scale);
        r.y = PrecisionUtilities.round(r.y * scale);
        r.width = right - r.x;
        r.height = bottom - r.y;

        return r;
    }

    /**
    * @see IFigure#getClientArea(Rectangle)
     */
    @Override
    public Rectangle getClientArea(Rectangle r)
    {
        super.getClientArea(r);

        int right = PrecisionUtilities.round((r.x + r.width) / scale);
        int bottom = PrecisionUtilities.round((r.y + r.height) / scale);

        r.x = PrecisionUtilities.round(r.x / scale);
        r.y = PrecisionUtilities.round(r.y / scale);
        r.width = right - r.x;
        r.height = bottom - r.y;

        return r;
    }

    /**
     * @see Figure#getPreferredSize(int, int)
     */
    @Override
    public Dimension getPreferredSize(int wHint, int hHint)
    {
        Dimension d = super.getPreferredSize(wHint, hHint);

        int w = getInsets().getWidth();
        int h = getInsets().getHeight();

        return d.getExpanded(-w, -h).scale(scale).expand(w, h);
    }

    /**
     * Returns the scale level, default is 1.0.
     * @return the scale level
     */
    public double getScale()
    {
        return scale;
    }

    /**
        * @see org.eclipse.draw2d.IFigure#isCoordinateSystem()
     */
    @Override
    public boolean isCoordinateSystem()
    {
        return true;
    }

    /**
     * Sets the zoom level
     * @param newZoom The new zoom level
     */
    public void setScale(double newZoom)
    {
        if (scale == newZoom) {
            return;
        }
        scale = newZoom;
        revalidate();
        repaint();
    }

    /**
     * @see org.eclipse.draw2d.Figure#translateFromParent(Translatable)
     */
    @Override
    public void translateFromParent(Translatable t)
    {
        t.performScale(1.0 / scale);
    }

    /**
     * @see org.eclipse.draw2d.Figure#translateToParent(Translatable)
     */
    @Override
    public void translateToParent(Translatable t)
    {
        t.performScale(scale);
    }

}