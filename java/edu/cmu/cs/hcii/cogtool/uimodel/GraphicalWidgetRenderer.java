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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.draw2d.Graphics;
import org.eclipse.swt.graphics.Font;

/**
 * Renders graphical widgets appropriately for their types
 * @author weianw
 */
public interface GraphicalWidgetRenderer
{
    public static final int NO_LABEL = 0;
    public static final int LABEL_CENTER = 1;
    public static final int LABEL_LEFT = 2;
    public static final int LABEL_RIGHT = 3;

    public static final String SEPARATOR_STRING = "---";

    /**
     * Can use multiple renderers to render a widget
     * @author rmyers
     */
    public abstract static class MultiRenderer implements GraphicalWidgetRenderer
    {
        /**
         * List of renderers for the foreground
         */
        protected List<GraphicalWidgetRenderer> renderers;

        /**
         * Constructor initializes the composite renderer with its child renderers
         * @param gwRenderers the renderers that will be used to paint
         * @param labelIndex the index of the renderer whose style will be used
         *                   to draw the label.
         */
        public MultiRenderer(GraphicalWidgetRenderer[] gwRenderers)
        {
            renderers = Arrays.asList(gwRenderers);
        }

        public void setSize(int width, int height)
        {
            Iterator<GraphicalWidgetRenderer> i = renderers.iterator();
            while (i.hasNext()) {
                i.next().setSize(width, height);
            }
        }
    }

    /**
     * Composites the output from several graphical widget renderers in order to
     * render the actual graphical widget
     * @author weianw
     */
    public static class CompositeRenderer extends MultiRenderer
    {
        protected int labelRenderer;

        /**
         * Constructor initializes the composite renderer with its child renderers
         * @param gwRenderers the renderers that will be used to paint
         * @param labelIndex the index of the renderer whose style will be used
         *                   to draw the label.
         */
        public CompositeRenderer(GraphicalWidgetRenderer[] gwRenderers,
                                 int labelIndex)
        {
            super(gwRenderers);

            labelRenderer = labelIndex;
        }

        /**
         * The FIRST widget in the array is the only one whose text will be
         * drawn. If other widgets have text, they will be ignored.
         */
        public void setText(String text)
        {
            renderers.get(labelRenderer).setText(text);
        }

        // The methods below essentially distribute the incoming calls among the
        // various child renderers to generate the composited output

        public void paintForeground(Graphics g)
        {
            Iterator<GraphicalWidgetRenderer> i = renderers.iterator();
            while (i.hasNext()) {
                g.pushState();
                try {
                    i.next().paintForeground(g);
                }
                finally {
                    g.popState();
                }
            }
        }

        public void paintMidground(Graphics g)
        {
            Iterator<GraphicalWidgetRenderer> i = renderers.iterator();
            while (i.hasNext()) {
                g.pushState();
                try {
                    i.next().paintMidground(g);
                }
                finally {
                    g.popState();
                }
            }
        }

        public void paintBackground(Graphics g)
        {
            Iterator<GraphicalWidgetRenderer> i = renderers.iterator();
            while (i.hasNext()) {
                g.pushState();
                try {
                    i.next().paintBackground(g);
                }
                finally {
                    g.popState();
                }
            }
        }

        // TODO: should this pick a font from the "children" renderers?
        public Font getFont()
        {
            return null;
        }

        public void updateData()
        {
            Iterator<GraphicalWidgetRenderer> i = renderers.iterator();
            while (i.hasNext()) {
                i.next().updateData();
            }
        }
    }

    /**
     * Similar to the CompositeRenderer, but instead of drawing all renderers
     * in the list, it chooses one of them to draw.
     * @author rmyers
     */
    public static class ChoiceMultiRenderer extends MultiRenderer
    {
        protected int curRenderer;

        /**
         * Constructor initializes the composite renderer with its child renderers
         * @param gwRenderers the renderers that will be used to paint
         * @param labelIndex the index of the renderer whose style will be used
         *                   to draw the label.
         */
        public ChoiceMultiRenderer(GraphicalWidgetRenderer[] gwRenderers,
                                   int firstRenderer)
        {
            super(gwRenderers);

            curRenderer = firstRenderer;
        }

        // The methods below pass the call on to the current renderer

        public void paintForeground(Graphics g)
        {
            g.pushState();
            try {
                renderers.get(curRenderer).paintForeground(g);
            }
            finally {
                g.popState();
            }
        }

        public void paintMidground(Graphics g)
        {
            g.pushState();
            try {
                renderers.get(curRenderer).paintMidground(g);
            }
            finally {
                g.popState();
            }
        }

        public void paintBackground(Graphics g)
        {
            g.pushState();
            try {
                renderers.get(curRenderer).paintBackground(g);
            }
            finally {
                g.popState();
            }
        }

        // TODO: should this pick a font from the "children" renderers?
        public Font getFont()
        {
            return null;
        }

        public void updateData()
        {
            // Override in subclasses
        }

        public void setText(String text)
        {
            renderers.get(curRenderer).setText(text);
        }
    }

    /**
     * Renders the graphical widget's foreground onto the given canvas.
     * The foreground generally consists of the title text, and is
     * rendered opaque for readability.
     * Note that drawing should be performed at the location (0, 0).
     *
     * @param g the canvas to render onto
     */
    public void paintForeground(Graphics g);

    /**
     * Renders the graphical widget's midground onto the given canvas.
     * The midground generally consists of shading & borders that make
     * the widget type discernable, and is rendered semi-transparent for
     * aesthetics.
     * Note that drawing should be performed at the location (0, 0).
     *
     * @param g the canvas to render onto
     */
    public void paintMidground(Graphics g);

    /**
     * Renders the graphical widget's background onto the given canvas.
     * Generally empty, it might be a 9-part or image.
     * Note that drawing should be performed at the location (0, 0).
     *
     * @param g the canvas to render onto
     */
    public void paintBackground(Graphics g);

    /**
     * Sets the title for the graphical widget
     *
     * @param text the new title
     */
    public void setText(String text);

    /**
     * Sets the size of the graphical widget
     *
     * @param width the new size
     * @param height the new size
     */
    public void setSize(int width, int height);

    /**
     * Returns the font used for the label
     */
    public Font getFont();

    /**
     * Update any data the renderer needs before paint() is invoked.
     */
    public void updateData();
}
