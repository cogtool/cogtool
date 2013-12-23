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

package edu.cmu.cs.hcii.cogtool.util;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Composite;

import edu.cmu.cs.hcii.cogtool.view.ScalableInteractiveRectangle;

public abstract class SWTTextEditor extends ManagedText
{
    public static final int LEFT_JUSTIFY = SWT.LEFT;
    public static final int CENTER_TEXT = SWT.CENTER;

    public SWTTextEditor(Composite parent, int justification)
    {
        super(parent, SWT.SINGLE | justification, Keypad.FULL_KEYPAD);

        setVisible(false);
    }

    @Override
    public void cancel()
    {
        cleanup();
    }

    public void editText(String initialText, IFigure data, double scale)
    {
        setText(initialText);
        setData(data);

        setVisible(true);
        selectAll();
        setFocus();

        repaintTextEditor(scale);
        redraw();
    }

    public boolean inUse()
    {
        return isVisible() || (keypad != null);
    }

    public void cleanup()
    {
        setVisible(false);
        setText("");
    }

    protected int computeX(Rectangle bounds, double scale, int offset)
    {
        return PrecisionUtilities.round(bounds.x * scale) + offset;
    }

    protected int computeY(Rectangle bounds, double scale, int offset)
    {
        return PrecisionUtilities.round(bounds.y * scale) + offset;
    }

    protected int computeWidth(Rectangle bounds, double scale, int offset)
    {
        // default implementation
        return PrecisionUtilities.round(bounds.width * scale) - (2 * offset);
    }

    protected int computeHeight(Rectangle bounds, double scale, int offset)
    {
        // default implementation
        return PrecisionUtilities.round(bounds.height * scale) - (2 * offset);
    }

    protected Font getFontToUse()
    {
        return ((IFigure) getData()).getFont();
    }

    protected int computeFontHeight(int editorHeight, double scale, int offset)
    {
        int heightAdjustment = OSUtils.MACOSX ? 3
                                              : (int) ((3.0 * scale) + 1.0);
        // jcorn - The above conditional flag may not be necessary since
        //         our switch to SWT 3.2
        // TODO: Look into whether or not this conditional is required
        //int heightAdjustment = (int) ((3.0 * scale) + 1.0);

        return Math.max(editorHeight - (2 * heightAdjustment), 1);
    }

    public void repaintTextEditor(double scale)
    {
        if (getVisible()) {
            IFigure f = (IFigure) getData();
            Rectangle bounds = f.getBounds();

            if (f instanceof ScalableInteractiveRectangle) {
                bounds = PrecisionUtilities.getDraw2DRectangle(bounds.x / scale,
                                                               bounds.y / scale,
                                                               bounds.width / scale,
                                                               bounds.height / scale);
            }

            ensureLayout();

            int offset = OSUtils.MACOSX ? -2 : 1;
            // jcorn - The above conditional flag may not be necessary since
            //         our switch to SWT 3.2
            // TODO: Look into whether or not this conditional is required
            //offset = 1;

            int x = computeX(bounds, scale, offset);
            int y = computeY(bounds, scale, offset);

            setLocation(x, y);

            int width = computeWidth(bounds, scale, offset);
            int height = computeHeight(bounds, scale, offset);

            setSize(width, height);

            // TODO figure out whether getFontToUse() can reasonably
            //      return null; it has done so in some non-reproducible
            //      cases, but I don't yet know whether that is a bug, or if
            //      the bug was that were assuming that it was non-null. Given
            //      this code already appears to say "only set the font if there
            //      is non-zero length font data," it would appear that the
            //      right thing to do is skip the code if there isn't any font
            //      or font data, but with my limited knowledge of what's
            //      going on here I'm a little worried....
            // Was: FontData[] fontData = getFontToUse().getFontData();
            Font font = getFontToUse();
            if (font == null) {
                return;
            }
            FontData[] fontData = font.getFontData();
            if (fontData == null) {
                return;
            }
            // End of replacement described by preceding comment.

            if (fontData.length > 0) {
                String fontName = fontData[0].getName();
                int fontStyle = fontData[0].getStyle();

                FontData newFontData =
                    new FontData(fontName,
                                 computeFontHeight(height, scale, offset),
                                 fontStyle);

                setFont(new Font(WindowUtil.GLOBAL_DISPLAY, newFontData));
            }
        }
    }

    @Override
    protected void checkSubclass()
    {
        // allow subclass; we're not doing anything but adding functionality
    }
}
