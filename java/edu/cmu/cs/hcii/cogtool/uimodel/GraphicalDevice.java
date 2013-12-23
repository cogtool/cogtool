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

import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Image;

import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;

public class GraphicalDevice extends AbstractGraphicalSource<InputDevice>
{
    /**
     * Draw2d delegate that does the hard work of drawing the button for us
     * TODO: Why the overrides? (mlh)
     *
     * @author weianw
     */
    protected class CogButton extends Label
    {
        public CogButton(String text)
        {
            super(text);
        }

        @Override
        public IFigure getParent()
        {
            return GraphicalDevice.this;
        }

        // Make protected painting methods public
        @Override
        public void paintBorder(Graphics g)
        {
            super.paintBorder(g);
        }

        @Override
        public void paintChildren(Graphics g)
        {
            super.paintChildren(g);
        }

        @Override
        public void paintClientArea(Graphics g)
        {
            super.paintClientArea(g);
        }

        @Override
        public void paintFigure(Graphics g)
        {
            super.paintFigure(g);
        }
    }

    protected CogButton button;

    public GraphicalDevice(InputDevice m,
                           int color,
                           boolean supportToolTip,
                           int rolloverCursor,
                           FrameUIModel displayComputer)
    {
        super(supportToolTip, rolloverCursor, displayComputer);

        setSize(new org.eclipse.draw2d.geometry.Dimension(DesignUtil.DEVICE_WIDTH,
                                                          DesignUtil.DEVICE_HEIGHT));
        setPreferredSize(getSize());
        setLocation(new org.eclipse.draw2d.geometry.Point(0, 0));

        setSourceModel(m);

        setColor(color);
    }

    @Override
    protected String getTypeDescription()
    {
        return model.getDeviceType().toString();
    }

    // Perform our own updating when the model changes

    @Override
    public void updateTitle()
    {
        super.updateTitle();
        button = new CogButton(model.getName());
    }

    // Pass all paint calls through to the button
    // and then apply our own decoration

    @Override
    protected void paintBorder(Graphics g)
    {
        super.paintBorder(g);
        button.paintBorder(g);
    }

    @Override
    protected void paintChildren(Graphics g)
    {
        super.paintChildren(g);
        button.paintChildren(g);
    }

    @Override
    protected void paintClientArea(Graphics g)
    {
        super.paintClientArea(g);
        button.paintClientArea(g);
    }

    /**
     * Override to perform drawing tasks common to all graphical sources
     */
    @Override
    protected void paintFigure(Graphics g)
    {
        Dimension size = getSize();

        // Workaround Draw2D's inability to draw 0-size images.
        int width = size.width;
        if (width == 0) {
            width = 1;
        }

        int height = size.height;
        if (height == 0) {
            height = 1;
        }

        Image scaled = null;

        try {
            // Draw foreground sheen (not selected)
            foreground.setAlpha(0,
                                     0,
                                     displayAlpha.determineAlpha(false));

            // TODO: Can this be cached more efficiently?
            scaled = new Image(null, foreground.scaledTo(width, height));

            Point location = getLocation();

            g.drawImage(scaled, location.x, location.y);
        }
        catch (SWTException ex) {
            throw new RcvrImageException("Drawing device foreground image failed",
                                         ex);
        }
        finally {
            if (scaled != null) {
                scaled.dispose();
            }
        }

        super.paintFigure(g);

        button.paintFigure(g);
    }

    // Continually update the size & location of the button delegate.
    // Ensures that the button follows the mouse properly as we drag it

    /**
     * Children override this method to provide custom visble behavior.
     */
    @Override
    public void paint(Graphics g)
    {
        button.setSize(getSize());
        button.setLocation(getLocation());
        super.paint(g);
    }
}
