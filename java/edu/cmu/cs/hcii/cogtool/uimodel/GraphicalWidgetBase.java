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

import java.io.ByteArrayInputStream;
import java.util.Comparator;
import java.util.EventObject;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.draw2d.SWTGraphics;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;

import edu.cmu.cs.hcii.cogtool.model.AShape;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.ShapeOval;
import edu.cmu.cs.hcii.cogtool.model.ShapeRoundedRectangle;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.View;

/**
 * The graphical widget class is the "view" of a Widget.
 * Specifically it is the Draw2D implementation of the widget.
 *
 * Custom optimizations are made for rendering offline and updating the display.
 * @author alexeiser/weian
 *
 */
public class GraphicalWidgetBase<T extends IWidget> extends AbstractGraphicalSource<T>
                                                implements GraphicalWidget<T>
{
    // Factory methods

    /**
     * Factory method that generates a clipper corresponding to a given shape
     * @param shape the shape to clip to
     * @return a clipper appropriate for the shape
     */
    protected static GraphicalWidgetClipper getClipperForShape(AShape shape)
    {
        if (shape instanceof ShapeRoundedRectangle) {
            return new GraphicalWidgetClipper.RoundedClipper((ShapeRoundedRectangle) shape);
        }
        if (shape instanceof ShapeOval) {
            return GraphicalWidgetClipper.OVAL_CLIPPER;
        }

        // By default, do not clip
        return GraphicalWidgetClipper.TRIVIAL_CLIPPER;
    }

    /**
     * Factory method that generates a painter corresponding to a given widget type
     * @param type the widget type, or null to indicate an unrendered widget
     * @return a renderer appropriate to the widget type
     */
    protected GraphicalWidgetRenderer getRendererForType(WidgetType type,
                                                         SkinType skin,
                                                         boolean renderSkin)
    {
        if ((type == WidgetType.Noninteractive) || (type == WidgetType.Text)) {
            return new LabelRenderer(this.attrOverride);
        }

        if ((type == WidgetType.Link) && renderSkin) {
            return new LinkWireRenderer(this, this.attrOverride);
        }

        if (renderSkin && (skin == SkinType.WireFrame)) {
            if (type == WidgetType.Button) {
                return new ItemWireRenderer(ItemWireRenderer.LABEL_CENTER,
                                            this,
                                            this.attrOverride);
            }

            if (type == WidgetType.Check) {
                return new ChoiceWireRenderer(ChoiceWireRenderer.CHECK,
                                              this,
                                              this.attrOverride);
            }

            if (type == WidgetType.Radio) {
                return new ChoiceWireRenderer(ChoiceWireRenderer.RADIO,
                                              this,
                                              this.attrOverride);
            }

            if (type == WidgetType.TextBox) {
                return new TextBoxRenderer(ItemWireRenderer.LABEL_LEFT,
                                           this,
                                           this.attrOverride);
            }

            if (type == WidgetType.Submenu) {
                return new SubmenuWireRenderer(this, this.attrOverride);
            }

            if (type == WidgetType.PullDownList) {
                return new PulldownListWireRenderer(this, this.attrOverride);
            }

            if ((type == WidgetType.PullDownItem) ||
                (type == WidgetType.ListBoxItem) ||
                (type == WidgetType.Menu) ||
                (type == WidgetType.MenuItem) ||
                (type == WidgetType.Graffiti))
            {
               return new ItemWireRenderer(ItemWireRenderer.LABEL_LEFT,
                                           this,
                                           this.attrOverride);
            }
        }

        if ((! renderSkin) || (skin == SkinType.None)) {
            if (type == WidgetType.TextBox) {
                return new LabelRenderer(GraphicalWidgetRenderer.LABEL_LEFT,
                                         this.attrOverride);
            }

            // By default, just paint the widget's title
            if (type == WidgetType.Button) {
                return new LabelRenderer(this.attrOverride);
            }

            if (type.equals(WidgetType.Submenu)) {
                return new SubmenuLabelRenderer(this.attrOverride);
            }
            if (type == WidgetType.PullDownList) {
                LabelRenderer lRenderer =
                    new LabelRenderer(GraphicalWidgetRenderer.LABEL_LEFT,
                                      this.attrOverride)
                    {
                        @Override
                        public void updateData()
                        {
                            GraphicalWidget<?> gw =
                                (GraphicalWidget<?>) getParent();
                            IWidget widget = gw.getModel();

                            setText(RendererSupport.getPullDownSelectedTitle(widget,
                                                                             lAttrOverride));
                        }
                    };

                return lRenderer;
            }
            if ((type == WidgetType.PullDownItem) ||
                (type == WidgetType.ListBoxItem) ||
                (type == WidgetType.MenuItem))
            {
                return new LabelRenderer(GraphicalWidgetRenderer.LABEL_LEFT,
                                         this.attrOverride)
                    {
                        @Override
                        public void updateData()
                        {
                            GraphicalWidget<?> gw =
                                (GraphicalWidget<?>) getParent();
                            IWidget w = gw.getModel();
                            Object sepValue =
                                w.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
                            boolean isSep =
                                NullSafe.equals(WidgetAttributes.IS_SEPARATOR,
                                                sepValue);

                            setText(isSep ? GraphicalWidgetRenderer.SEPARATOR_STRING
                                          : w.getTitle());
                        }
                    };
            }

            return new LabelRenderer(GraphicalWidgetRenderer.LABEL_LEFT,
                                     this.attrOverride);
        }

        // Image renders - either Nine-Part or zoomed image
        if (type == WidgetType.Button) {
            return new NinePartRenderer(this,
                                        type.getKeyName(),
                                        skin.getKeyName(),
                                        NinePartRenderer.LABEL_CENTER,
                                        NinePartRenderer.RENDER_FULL,
                                        this.attrOverride)
            {
                @Override
                public void updateData()
                {
                    IWidget attributed = getModel();

                    boolean isSelected =
                        RendererSupport.isSelected(attributed,
                                                   attrOverride);
                    Object toggleable =
                        attributed.getAttribute(WidgetAttributes.IS_TOGGLEABLE_ATTR);

                    if (isSelected &&
                        NullSafe.equals(toggleable,
                                        WidgetAttributes.IS_TOGGLEABLE) &&
                        (altSet != null))
                    {
                        imageParts = altSet;
                    }
                    else {
                        imageParts = defaultSet;
                    }
                }
            };
        }

        if (type == WidgetType.Check) {
            NinePartRenderer boxRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName(),
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_LEFT_SQUARE,
                                     this.attrOverride);

            ZoomRenderer checkRenderer =
                new ZoomRenderer(this,
                                 type.getKeyName(),
                                 skin.getKeyName(),
                                 ZoomRenderer.NO_LABEL,
                                 ZoomRenderer.RENDER_LEFT_SQUARE,
                                 this.attrOverride);

            GraphicalWidgetRenderer[] rendererArray =
                { boxRenderer, checkRenderer };

            return new CheckboxImageRenderer(rendererArray, 0);
        }

        if (type == WidgetType.Radio) {
            return new ZoomRenderer(this,
                                    type.getKeyName(),
                                    skin.getKeyName(),
                                    ZoomRenderer.LABEL_LEFT,
                                    ZoomRenderer.RENDER_LEFT_SQUARE,
                                    this.attrOverride)
            {
                @Override
                public void updateData()
                {
                    boolean isSelected =
                        RendererSupport.isRadioSelected(getModel(),
                                                        attrOverride);

                    if (isSelected && (altImage != null)) {
                        image = altImage;
                    }
                    else {
                        image = defaultImage;
                    }
                }
            };
        }

        if (type == WidgetType.TextBox) {
            return new NinePartRenderer(this,
                                        type.getKeyName(),
                                        skin.getKeyName(),
                                        NinePartRenderer.LABEL_LEFT,
                                        NinePartRenderer.RENDER_FULL,
                                        this.attrOverride);
        }

        // MacOS pulldownlist only!
        if ((skin == SkinType.MacOSX) && (type == WidgetType.PullDownList)) {
            ZoomRenderer arrowRenderer =
                new ZoomRenderer(this,
                                 type.getKeyName(),
                                 skin.getKeyName(),
                                 ZoomRenderer.LABEL_LEFT,
                                 ZoomRenderer.RENDER_RIGHT_SQUARE,
                                 this.attrOverride);

            NinePartRenderer leftRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName() + "_left",
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_LEFT_FILL,
                                     this.attrOverride);

            NinePartRenderer rightRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName() + "_right",
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_RIGHT_SQUARE,
                                     this.attrOverride)
                {
                    @Override
                    public void updateData()
                    {
                        GraphicalWidget<?> gw =
                            (GraphicalWidget<?>) getParent();
                        IWidget widget = gw.getModel();

                        setText(RendererSupport.getPullDownSelectedTitle(widget,
                                                                         attrOverride));
                    }
                };

            GraphicalWidgetRenderer[] rendererArray =
                { leftRenderer, rightRenderer, arrowRenderer };

            return new GraphicalWidgetRenderer.CompositeRenderer(rendererArray,
                                                                 1);
        }

        // WinXP pulldownlist only!
        if ((skin == SkinType.WinXP) && (type == WidgetType.PullDownList)) {
            ZoomRenderer arrowRenderer =
                new ZoomRenderer(this,
                                 type.getKeyName(),
                                 skin.getKeyName(),
                                 ZoomRenderer.LABEL_LEFT,
                                 ZoomRenderer.RENDER_RIGHT_SQUARE,
                                 this.attrOverride);

            NinePartRenderer leftRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName() + "_left",
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_LEFT_FILL,
                                     this.attrOverride);

            NinePartRenderer rightRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName() + "_right",
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_RIGHT_SQUARE,
                                     this.attrOverride)
                {
                    @Override
                    public void updateData()
                    {
                        GraphicalWidget<?> gw =
                            (GraphicalWidget<?>) getParent();
                        IWidget widget = gw.getModel();

                        setText(RendererSupport.getPullDownSelectedTitle(widget,
                                                                         attrOverride));
                    }
                };

            GraphicalWidgetRenderer[] rendererArray =
                { leftRenderer, rightRenderer, arrowRenderer };

            return new GraphicalWidgetRenderer.CompositeRenderer(rendererArray,
                                                                 1);
        }

        // Pulldown lists for non-macOSX or WinXP skins only
        if (type == WidgetType.Submenu) {
            ZoomRenderer arrowRenderer =
                new ZoomRenderer(this,
                                 type.getKeyName(),
                                 skin.getKeyName(),
                                 ZoomRenderer.LABEL_LEFT,
                                 ZoomRenderer.RENDER_RIGHT_SQUARE,
                                 this.attrOverride);

            NinePartRenderer backgroundRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName(),
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_FULL,
                                     this.attrOverride);

            GraphicalWidgetRenderer[] rendererArray =
                { backgroundRenderer, arrowRenderer };

            return new GraphicalWidgetRenderer.CompositeRenderer(rendererArray,
                                                                 1);
        }

        if (type == WidgetType.PullDownList) {
            ZoomRenderer arrowRenderer =
                new ZoomRenderer(this,
                                 type.getKeyName(),
                                 skin.getKeyName(),
                                 ZoomRenderer.LABEL_LEFT,
                                 ZoomRenderer.RENDER_RIGHT_SQUARE,
                                 this.attrOverride)
                {
                    @Override
                    public void updateData()
                    {
                        GraphicalWidget<?> gw =
                            (GraphicalWidget<?>) getParent();
                        IWidget widget = gw.getModel();

                        setText(RendererSupport.getPullDownSelectedTitle(widget,
                                                                         attrOverride));
                    }
                };

            NinePartRenderer backgroundRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName(),
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_FULL,
                                     this.attrOverride);

            GraphicalWidgetRenderer[] rendererArray =
                { backgroundRenderer, arrowRenderer };

            return new GraphicalWidgetRenderer.CompositeRenderer(rendererArray,
                                                                 1);
        }

        if ((type == WidgetType.MenuItem) ||
            (type == WidgetType.PullDownItem) ||
            (type == WidgetType.ListBoxItem))
        {
            NinePartRenderer itemRenderer =
                new NinePartRenderer(this,
                                     type.getKeyName(),
                                     skin.getKeyName(),
                                     NinePartRenderer.LABEL_LEFT,
                                     NinePartRenderer.RENDER_FULL,
                                     this.attrOverride);

            ZoomRenderer sepRenderer =
                new ZoomRenderer(this,
                                 "separator",
                                 skin.getKeyName(),
                                 ZoomRenderer.NO_LABEL,
                                 ZoomRenderer.RENDER_FULL,
                                 this.attrOverride);

            GraphicalWidgetRenderer[] rendererArray =
                { itemRenderer, sepRenderer };

            return new GraphicalWidgetRenderer.ChoiceMultiRenderer(rendererArray,
                                                                   0)
            {
                @Override
                public void updateData()
                {
                    Object isSep =
                        getModel().getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                    if (NullSafe.equals(isSep, WidgetAttributes.IS_SEPARATOR)) {
                        curRenderer = 1;
                    }
                    else {
                        curRenderer = 0;
                    }
                }
            };
        }

        return new NinePartRenderer(this,
                                    type.getKeyName(),
                                    skin.getKeyName(),
                                    NinePartRenderer.LABEL_LEFT,
                                    NinePartRenderer.RENDER_FULL,
                                    this.attrOverride);
    }

    // Renderers as inner classes

    /**
     * Paints only the title text
     * @author weianw
     */
    protected class LabelRenderer extends Label
                                  implements GraphicalWidgetRenderer
    {
        protected DefaultSEUIModel lAttrOverride;

        public LabelRenderer(DefaultSEUIModel ovr)
        {
            this(LABEL_CENTER, ovr);
        }

        public LabelRenderer(int labelStyle,
                             DefaultSEUIModel ovr)
        {
            this.lAttrOverride = ovr;

            if (labelStyle == NO_LABEL) {
                setVisible(false);
            }
            else if (labelStyle == LABEL_LEFT) {
                setTextPlacement(PositionConstants.LEFT);
                setTextAlignment(PositionConstants.LEFT);
            }
            else if (labelStyle == LABEL_RIGHT) {
                setTextPlacement(PositionConstants.RIGHT);
                setTextAlignment(PositionConstants.RIGHT);
            }
        }


        public void paintForeground(Graphics g)
        {
            if (isVisible()) {
                g.pushState();
                try {
                    super.paintFigure(g);
                }
                finally {
                    g.popState();
                }
            }
        }


        public void paintMidground(Graphics g)
        {
            clipper.fillShape(g, getBounds());
        }


        public void paintBackground(Graphics g)
        {
            // Nothing to do
        }

        protected static final String PADDING = " ";

        @Override
        public void setText(String text)
        {
            super.setText(PADDING + text);
        }

        @Override
        public String getText()
        {
            return super.getText().substring(PADDING.length());
        }

        @Override
        public IFigure getParent()
        {
            return GraphicalWidgetBase.this;
        }


        public void updateData()
        {
            // Do nothing by default
        }
    }

    protected class SubmenuLabelRenderer extends LabelRenderer
    {
        protected static final String SUBMENU_INDICATOR = " >";

        public SubmenuLabelRenderer(DefaultSEUIModel ovr)
        {
            super(LABEL_LEFT, ovr);
        }

        @Override
        public void setText(String text)
        {
            super.setText(text + SUBMENU_INDICATOR);
        }

        @Override
        public String getText()
        {
            String text = super.getText();
            return text.substring(0,
                                  text.length() - SUBMENU_INDICATOR.length());
        }
    }

    protected static class SubmenuWireRenderer extends ItemWireRenderer
    {
        public SubmenuWireRenderer(GraphicalWidgetBase<?> parent,
                                   DefaultSEUIModel ovr)
        {
            super(LABEL_LEFT, parent, true, ovr);
        }

        @Override
        protected void paintRightIcon(Graphics g)
        {
            Rectangle bds = getBounds();

            int x = bds.width - bds.height - 2;
            int y = 4;
            int w = bds.height - 6;
            int h = bds.height - 8;

            if (x < 0) {
                x = 1;
            }
            if (y >= bds.height) {
                y = bds.height;
            }
            if (w <= 0) {
                w = 1;
            }
            if (h <= 0) {
                h = 1;
            }

            int[] triangle = { x, y, x + w, y + (h/2), x, y + h };

            g.drawPolygon(triangle);
        }
    }

    protected static class PulldownListWireRenderer extends ItemWireRenderer
    {
        public PulldownListWireRenderer(GraphicalWidgetBase<?> parent,
                                        DefaultSEUIModel ovr)
        {
            super(LABEL_LEFT, parent, true, ovr);
        }

        @Override
        protected void paintRightIcon(Graphics g)
        {
            Rectangle bds = getBounds();

            int x = bds.width - bds.height - 2;
            int y = 4;
            int w = bds.height - 6;
            int h = bds.height - 8;

            if (x < 0) {
                x = 1;
            }
            if (y >= bds.height) {
                y = bds.height;
            }
            if (w <= 0) {
                w = 1;
            }
            if (h <= 0) {
                h = 1;
            }

            int[] triangle = { x, y, x + w, y, x + (w/2), y + h };

            g.drawPolygon(triangle);
        }

        @Override
        public void updateData()
        {
            GraphicalWidget<?> gw = (GraphicalWidget<?>) getParent();
            IWidget widget = gw.getModel();

            setText(RendererSupport.getPullDownSelectedTitle(widget,
                                                             attrOverride));
        }
    }

    // Fields

    // TODO (?): Currently the regenerateCaches code is inefficient because it
    // creates an image for each widget that is displayed.  The solution to this
    // presumably would be to save only the image data and not the image itself,
    // and only create and dispose of the image when necessary.  However, for
    // some reason that method causes an OutOfMemory error much earlier than
    // SWT runs out of handles when using the original method, so I changed it
    // back.
//    protected static final ImageData DEFAULT_DATA;
//    static {
//        Image img = new Image(null, 1, 1);
//        DEFAULT_DATA = img.getImageData();
//        img.dispose();
//    }

    public static class GraphicalWidgetLevelComparator
                                     implements Comparator<GraphicalWidget<?>>
    {
        /**
         * No state, so only one instance is ever needed.
         */
        public static GraphicalWidgetLevelComparator ONLY =
            new GraphicalWidgetLevelComparator();

        protected GraphicalWidgetLevelComparator() { }

        /**
         * This comparator returns 1 if
         */

        public int compare(GraphicalWidget<?> w1, GraphicalWidget<?> w2)
        {
            return Widget.WidgetLevelComparator.ONLY.compare(w1.getModel(),
                                                      w2.getModel());
        }
    }

    protected boolean selected;

    protected GraphicalWidgetClipper clipper;
    protected GraphicalWidgetRenderer renderer;

    protected boolean fast = true;
    protected Color widgetColor = GraphicsUtil.DEFAULT_COLOR;
    // never null
    protected Color midgroundColor;
    protected Color selectedColor;

    // serializes access to the 2 cache flags
    protected Object flagLock = new Object();
    protected boolean cachedMidgroundDirty;
    protected boolean cachedBackgroundDirty;

//    protected ImageData cachedMidgroundData = DEFAULT_DATA;
    protected Image cachedMidground = new Image(null, 1, 1); // never null
    protected Image cachedBackground;

    protected int desiredRolloverCursor;

    protected DefaultSEUIModel attrOverride;

    protected static int determineProperCursor(IWidget w, int rolloverCursor)
    {
        if ((rolloverCursor == WindowUtil.DRAW_CURSOR) &&
            (w.getWidgetType() == WidgetType.Noninteractive))
        {
            return WindowUtil.SELECT_CURSOR;
        }

        Object isSep = w.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
        if (NullSafe.equals(WidgetAttributes.IS_SEPARATOR, isSep)) {
            return WindowUtil.SELECT_CURSOR;
        }

        return rolloverCursor;
    }

    // Constructor
    public GraphicalWidgetBase(T m,
                           int color,
                           boolean supportToolTip,
                           int rolloverCursor,
                           FrameUIModel displayComputer,
                           DefaultSEUIModel override)
    {
        super(supportToolTip,
              determineProperCursor(m, rolloverCursor),
              displayComputer);

        this.midgroundColor = this.widgetColor;
        this.selectedColor = getOppositeColor(this.midgroundColor);

        // Do this before setSourceModel (tries to reset the cursor!)
        this.desiredRolloverCursor = rolloverCursor;

        this.attrOverride = override;

        setSourceModel(m);
        if (color != GraphicsUtil.defaultWidgetColor) {
            setColor(color);
        }
        setFastMode(false);

        AlertHandler handler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    renderer.updateData();
                    regenerateCaches();
                    repaint();
                }
            };

        model.addHandler(this, IAttributed.AttributeChange.class, handler);
        model.addHandler(this, IAttributed.AuthorityChange.class, handler);

        if (this.attrOverride != null) {
            this.attrOverride.addHandler(this,
                                         DefaultSEUIModel.CurrentOverrideChange.class,
                                         handler);
        }
    }

    @Override
    protected String getTypeDescription()
    {
        return model.getWidgetType().toString();
    }

    // Administrative stuff


    public void addChangeHandler(AlertHandler handler, Class<? extends EventObject> eventClass)
    {
        model.addHandler(this, eventClass, handler);
    }


    public void removeChangeHandler(AlertHandler handler, Class<? extends EventObject> eventClass)
    {
        model.removeHandler(eventClass, handler);
    }


    public void setSelected(boolean isSelected)
    {
        if (this.selected != isSelected) {
            this.selected = isSelected;

//            if (OSUtils.WINDOWS) {
                // XXX: Primarily want to regenerate caches.
                // On Windows, can't just set alpha in
                // the GC for some reason, it doesn't
                // apply to an image with transparency
                // Only way to keep the clip region and
                // the transparency is to reclip when
                // the selection changes.
                // since the transparency can only
                // be set on resize.

            // now that selection changes color instead of alpha, Macs need to
            // regenerate caches also.
            synchronized(this.flagLock) {
                this.cachedMidgroundDirty = true;
            }
            this.renderer.updateData();
            regenerateCaches();
//            }
        }
    }


    public boolean isSelected()
    {
        return this.selected;
    }

    @Override
    protected void updateAttributes()
    {
        // Inherit attributes from model
        updateType();
        updateShape();
        updateImage();
    }

    public Color getMidgroundColor()
    {
        return this.midgroundColor;
    }


    public GraphicalWidgetRenderer getRenderer()
    {
        return this.renderer;
    }

    // These methods update our internal state to correspond to that of our model

    @Override
    synchronized public void updateTitle()
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedMidgroundDirty = true;
        }

        // Update the text in the renderer
        this.renderer.setText(getModel().getTitle());
        this.renderer.updateData();

        if (this instanceof GraphicalChildWidget<?, ?>) {
            GraphicalParentWidget<?, ?> parentFigure =
                ((GraphicalChildWidget<?, ?>) this).getParentFigure();

            parentFigure.getRenderer().updateData();
        }

        regenerateCaches();
    }


    synchronized public void updateType()
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedMidgroundDirty = true;
        }

        IWidget w = getModel();

        try {
            // Change the renderer to one that is appropriate for the type
            this.renderer = getRendererForType(w.getWidgetType(),
                                               w.getFrame().getDesign().getSkin(),
                                               w.isRendered());
        }
        catch (Exception e) {
            System.err.println("Error creating widget renderer: " + e);

            try {
                this.renderer = getRendererForType(w.getWidgetType(),
                                                   SkinType.None,
                                                   w.isRendered());
            }
            catch (Exception ex) {
                System.err.println("Error creating unskinned widget renderer: " + e);

                this.renderer = new LabelRenderer(this.attrOverride);
            }
        }

        Dimension widgetExtent = getSize();
        this.renderer.setSize(widgetExtent.width, widgetExtent.height);
        this.renderer.setText(w.getTitle());
        this.renderer.updateData();

        setCursor(determineProperCursor(w, this.desiredRolloverCursor));
        regenerateCaches();
    }


    synchronized public void updateShape()
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedMidgroundDirty = true;
            this.cachedBackgroundDirty = true;
        }

        // Keep track of some properties that we need later
        Dimension oldSize = getSize();
        GraphicalWidgetClipper oldClipper = this.clipper;

        AShape s = model.getShape();
        DoubleRectangle shapeBds = s.getBounds();
        Rectangle figBds = PrecisionUtilities.getDraw2DRectangle(shapeBds);

        // Inherit properties from our model's shape
        setBounds(figBds);

        if ((figBds.width == 0) || (figBds.height == 0)) {
            if (figBds.width == 0) {
                figBds.width = 1;
            }
            if (figBds.height == 0) {
                figBds.height = 1;
            }

            super.setSize(figBds.width, figBds.height);
        }

        this.renderer.setSize(figBds.width, figBds.height);
        setPreferredSize(figBds.width, figBds.height);

        this.clipper = getClipperForShape(s);

        // Decide whether the caches are really dirty
        if (this.clipper.equals(oldClipper) && getSize().equals(oldSize)) {
            synchronized(this.flagLock) {
                this.cachedMidgroundDirty = false;
                this.cachedBackgroundDirty = false;
            }
        }

        regenerateCaches(figBds.width, figBds.height);
    }


    synchronized public void updateImage()
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedBackgroundDirty = true;
        }

        // Confirm that the image is readable (throws an exception if it is not)
        if (model.getImage() != null) {
            new ImageData(new ByteArrayInputStream(model.getImage()));
        }
        this.renderer.updateData();
        regenerateCaches();
    }


    synchronized public void dynamicHighlight(boolean highlight)
    {
        boolean changed = false;

        if (highlight) {
            if (this.midgroundColor != ColorConstants.gray) {
                changed = true;

                this.midgroundColor = ColorConstants.gray;
            }
        }
        else {
            if (this.midgroundColor != this.widgetColor) {
                changed = true;

                this.midgroundColor = this.widgetColor;
            }
        }

        if (changed) {
            // Mark the caches as dirty so that no one will use them
            synchronized(this.flagLock) {
                this.cachedMidgroundDirty = true;
            }

            regenerateCaches();
            repaint();
        }
    }

    // Setters to manipulate parameters that are not stored in the model

    @Override
    synchronized public void setColor(int color)
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedMidgroundDirty = true;
        }

        boolean changeMidground = (this.midgroundColor == this.widgetColor);

        if (this.widgetColor != GraphicsUtil.DEFAULT_COLOR) {
            this.widgetColor.dispose();
        }
        this.widgetColor = new Color(null, GraphicsUtil.getRGBFromColor(color));

        if (changeMidground) {
            this.midgroundColor = this.widgetColor;
            this.selectedColor = getOppositeColor(this.midgroundColor);
        }

        regenerateCaches();
    }

    protected Color getOppositeColor(Color c)
    {
        int red = findOppositeNumber(c.getRed());
        int green = findOppositeNumber(c.getGreen());
        int blue = findOppositeNumber(c.getBlue());

        return new Color(null, red, green, blue);
    }

    @Override
    synchronized public void setSize(int w, int h)
    {
        // Mark the caches as dirty so that no one will use them
        synchronized(this.flagLock) {
            this.cachedMidgroundDirty = true;
            this.cachedBackgroundDirty = true;
        }

        super.setSize(w, h);
        this.renderer.setSize(w, h);
        this.renderer.updateData();
        regenerateCaches();
    }

    // Drawing stuff

    public Color getSelectionColor()
    {
        return isSelected() ? this.selectedColor : this.midgroundColor;
    }

    public static int findOppositeNumber(int num)
    {
        return 255 - num;
    }

    @Override
    public void paint(Graphics g)
    {
        Rectangle bds = getBounds();

        g.pushState();

        try {
            // Always draw the background skin
            // First pass -- commented out to keep checkins separate.
            int oldAlpha = g.getAlpha();
            g.setAlpha(255);
            g.translate(bds.x, bds.y);
            this.renderer.paintBackground(g);
            g.translate(- bds.x, - bds.y);
            g.setAlpha(oldAlpha);

            // Check if we should perform shortcut rendering
            boolean shortcut;
            synchronized(this.flagLock) {
                shortcut = this.fast || this.cachedMidgroundDirty
                                     || this.cachedBackgroundDirty;
            }

            Color midColor = getSelectionColor();
            int midgroundAlpha =
                displayAlpha.determineAlpha(false);
            if (shortcut) {
                g.setBackgroundColor(midColor);
                g.setAlpha(midgroundAlpha);
                this.clipper.fillShape(g, bds);
            }
            else {
                // Otherwise we render everything properly
                // Lock the caches while we use them
                synchronized(GraphicalWidgetBase.this) {
                    // Render the backgound image if necessary
                    if (this.cachedBackground != null) {
                        g.drawImage(this.cachedBackground, bds.x, bds.y);
                    }

                    // XXX: Workaround for the dysfunctional transparency
                    //      support with Graphics objects on Windows platforms
                    // Most of the work-around for this issue is in
                    // regenerateCaches and in the clipper.
                    // There is also a quick workaround in set selected
                    // This entire "block" can be removed, but
                    // to protect against fixes in SWT causing issues, I
                    // am leaving it here.
                    if (! OSUtils.WINDOWS) {
                        // Render the midground with the appropriate level of transparency
                        g.setAlpha(midgroundAlpha);
                    }
                    g.setBackgroundColor(midColor);

//                    Image img = new Image(null, this.cachedMidgroundData);
                    g.drawImage(this.cachedMidground, bds.x, bds.y);
//                    img.dispose();
                }
            }

            // Always draw the foreground text
            g.setAlpha(255);
            g.translate(bds.x, bds.y);
            this.renderer.paintForeground(g);

            // If this is a remote label, indicate by setting a dotted border
            if (null != model.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR))
            {
                int oldLineStyle = g.getLineStyle();
                g.setLineStyle(Graphics.LINE_DOT);
                g.drawRectangle(0, 0, bds.width - 1, bds.height - 1);
                g.setLineStyle(oldLineStyle);
            }
        }
//        catch (Exception e) {
//            RcvrExceptionHandler.
//        }
        finally {
            // Return the canvas to its original state
            g.popState();
        }
    } // paint

    /**
     * Regenerates the dirty caches in a background thread.
     */
    protected void regenerateCaches()
    {
        Dimension size = getSize();

        regenerateCaches(size.width, size.height);
    }

    protected void regenerateCaches(final int width, final int height)
    {
        // For performance reasons, we defer regeneration until we exit fast mode
        if (! this.fast) {
            if (View.isDrawingOK()) {

            // Recompute the cached data in the background
//            new Thread () {
//                public void run() {
                     boolean needsRepaint = false;

                    // Lock the parameters & caches while we use/manipulate them
//                    synchronized(GraphicalWidget.this) {
                        // Recompute the midground cache if it is dirty
                        if (cachedMidgroundDirty) {
                            needsRepaint = true;

                            if ((width == 0) || (height == 0)) {
                                throw new IllegalArgumentException(
                                        "Width or Height on a widget is 0. This " +
                                        "should have been prevented in setShape " +
                                        "Size");
                            }

                            // Create a temporary canvas on which to draw the midground
                            Image img = new Image(null, width, height);

                            GC gc = new GC(img);
                            SWTGraphics g = new SWTGraphics(gc);
                            g.setAntialias(SWT.OFF);

                            // Use the renderer & clipper to paint the midground nicely
                            g.setBackgroundColor(getSelectionColor());
                            renderer.paintMidground(g);

                            // XXX: Fix for Windows not correctly handling
                            //      Alpha's on images, when setting an alpha
                            //      on the GC.
                            //      This updates the Alpha value for all pixels
                            //      on the image, to use the selected widget
                            //      value.
                            if (OSUtils.WINDOWS) {
                                ImageData id = img.getImageData();

                                id.alpha = displayAlpha.determineAlpha(false);
                                img.dispose();
                                img = new Image(null, id);
                            }

                            // WARNING: this might dispose img, or it might return it
                            // DO NOT DISPOSE img!
                            Image newFG = clipper.clip(img);

                            // Clean the cache
                            cachedMidgroundDirty = false;
                            cachedMidground.dispose();
                            cachedMidground = newFG;
//                            cachedMidgroundData = newFG.getImageData();

                            // Dispose of temporary resources
//                            newFG.dispose();
                            g.dispose();
                            gc.dispose();
                        }

                        // Recompute the background cache if it is dirty
                        if (cachedBackgroundDirty) {
                            needsRepaint = true;
                            Image newBG = null;

                            // Check if background is present
                            byte[] bgImageData = model.getImage();
                            if (bgImageData != null) {
                                ImageData bg =
                                    new ImageData(
                                        new ByteArrayInputStream(bgImageData));

                                newBG =
                                   clipper.clip(new Image(null,
                                                          bg.scaledTo(width,
                                                                      height)));
                            }

                            // Clean the cache
                            cachedBackgroundDirty = false;
                            if (cachedBackground != null) {
                                cachedBackground.dispose();
                            }
                            cachedBackground = newBG;
                        }
//                    }

                    // Make sure the results of our update are shown
                    if (needsRepaint && View.isDrawingOK()) {
//                        WindowUtil.globalDisplay.syncExec(
//                            new Runnable() {
//                                public void run() {
                                     repaint();
//                                 }
//                            }
//                        );
                    }
//                }
//            }.start();
            }
        }
    }


    /**
     * Dispose of all resources
     */
    @Override
    synchronized public void dispose()
    {
        if (this.widgetColor != GraphicsUtil.DEFAULT_COLOR) {
            this.widgetColor.dispose();
        }
        this.cachedMidground.dispose();

        if (this.cachedBackground != null) {
            this.cachedBackground.dispose();
        }

        model.removeAllHandlers(this);
        if (this.attrOverride != null) {
            this.attrOverride.removeAllHandlers(this);
        }

        super.dispose();
    }

    /**
     * Adjusts the speed-quality tradeoff for rendering
     * @param on true to optimize for speed; false for quality
     */

    public void setFastMode(boolean on)
    {
        this.fast = on;

        // Make everything look nice again
        regenerateCaches();
    }
}
