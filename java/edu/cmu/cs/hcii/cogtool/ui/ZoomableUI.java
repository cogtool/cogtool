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

import java.util.Iterator;

import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Canvas;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.uimodel.SelectionFigure;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;
import edu.cmu.cs.hcii.cogtool.util.Zoomable;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.view.InteractionDrawingEditor;

public abstract class ZoomableUI extends DefaultUI implements Zoomable
{
    static public final double ZOOM_FACTOR = 1.15;

    protected InteractionDrawingEditor zoomEditor;

    public ZoomableUI(Project proj,
                      String windowMenuLabel,
                      MenuItemDefinition[] leadItems,
                      UndoManager undoMgr)
    {
        super(proj, windowMenuLabel, leadItems, undoMgr);
    }

    protected void setZoomEditor(InteractionDrawingEditor zoomableEditor)
    {
        zoomEditor = zoomableEditor;
    }

    protected double getZoomingFactor()
    {
        return ZOOM_FACTOR;
    }

    protected Rectangle computeUnion(Iterator<? extends SelectionFigure<?>> figs)
    {
        Rectangle r = null;

        // Go through all given figures and union their bounds.
        while (figs.hasNext()) {
            SelectionFigure<?> fig = figs.next();

            if (r == null) {
                r = new Rectangle(fig.getBounds());
            }
            else {
                r.union(fig.getBounds());
            }
        }

        // If no selected elements, return default
        if (r != null) {
            return r;
        }

        return null;
    }

    /**
     * Support for moving the selection state to the center during zooming
     * Subclasses should override.
     */
    protected Rectangle getSelectedRegion()
    {
        return null;
    }

    protected void setScrollOrigin(int x, int y)
    {
        zoomEditor.getSWTScrollComposite().setOrigin((x < 0) ? 0 : x,
                                                          (y < 0) ? 0 : y);
    }

    protected Point getScrollableSize()
    {
        return zoomEditor.getSWTScrollComposite().getSize();
    }

    /**
     * Return true if an actual centering took place
     */
    protected boolean centerSelectedRegion()
    {
        Rectangle region = getSelectedRegion();

        if (region != null) {
            Point extent = getScrollableSize();

            int centerX = extent.x / 2;
            int centerY = extent.y / 2;

            double scale = getZoom();

            region =
                PrecisionUtilities.getDraw2DRectangle(scale * region.x,
                                                      scale * region.y,
                                                      scale * region.width,
                                                      scale * region.height);

            setScrollOrigin(region.x + (region.width / 2) - centerX,
                            region.y + (region.height / 2) - centerY);

            return true;
        }

        return false;
    }

    /**
     * Set a specific zoom level.
     * @param z the new zoom setting
     */

    public void performZoom(double z)
    {
        performAction(CogToolLID.SetZoom, new Double(z));
        centerSelectedRegion();
    }

    /**
     * Fetch the current zoom value for the window.
     */

    public double getZoom()
    {
        return zoomEditor.getZoomSetting();
    }

    /**
     * Compute the zoom factor that fits the contents to the window.
     */

    public double computeZoomToFit()
    {
        return zoomEditor.computeZoomToFit();
    }

    /**
     * Zoom the window to 100%
     */

    public void zoomToActual()
    {
        setZoom(1.0);
        centerSelectedRegion();
    }

    /**
     * Set the zoom so that the contents fit exactly in the window.
     */

    public void zoomToFit()
    {
        setZoom(zoomEditor.computeZoomToFit());
    }

    /**
     * Zoom in the window by the standard factor
     */

    public void zoomIn()
    {
        setZoom(getZoom() * getZoomingFactor());
        centerSelectedRegion();
    }

    /**
     * Zoom out the window by the standard factor
     */

    public void zoomOut()
    {
        setZoom(getZoom() / getZoomingFactor());
        centerSelectedRegion();
    }

    /**
     * Set the zoom factor for the window to the given value.
     *
     * @param scale the desired zoom value
     */

    public void setZoom(double scale)
    {
        zoomEditor.setZoomSetting(scale);
        saveWindowZoom(getModelObject(), scale);
    }

    protected void restoreZoom()
    {
        setZoom(getWindowZoom(getModelObject()));
    }

    /**
     * External image drag-and-drop support
     */
    protected static Transfer imgTransfer = ClipboardUtil.IMAGE_TRANFERS[0];

    protected abstract class ExternalImageDropTarget extends DropTargetAdapter
    {
        /**
         * To add any CogTool-specific highlighting.
         */
        protected abstract void highlight(DropTargetEvent evt);

        /**
         * To cancel any CogTool-specific highlighting.
         */
        protected abstract void cancelHighlight(DropTargetEvent evt);

        /**
         * To build parameters given the selected object and the "dropped"
         * image data.  Return <code>null</code> to prevent operation.
         */
        protected abstract Object buildParameters(DropTargetEvent evt,
                                                  byte[] imgData);

        @Override
        public void dragEnter(DropTargetEvent evt)
        {
            evt.detail = DND.DROP_NONE;

            for (TransferData dataType : evt.dataTypes) {
                if (imgTransfer.isSupportedType(dataType)) {
                    evt.detail = DND.DROP_COPY;
                }
            }
        }

        @Override
        public void dragLeave(DropTargetEvent evt)
        {
            cancelHighlight(evt);
        }

        @Override
        public void dragOperationChanged(DropTargetEvent evt)
        {
            // Can only copy!
            evt.detail = DND.DROP_COPY;
        }

        @Override
        public void dragOver(DropTargetEvent evt)
        {
            // Add any CogTool-specific highlighting
            highlight(evt);

            // Scroll if necessary
            evt.feedback = DND.FEEDBACK_SCROLL;
            evt.detail = DND.DROP_COPY;
        }

        @Override
        public void dropAccept(DropTargetEvent evt)
        {
            // Nothing to do
        }

        @Override
        public void drop(DropTargetEvent evt)
        {
            byte[] imgData = ClipboardUtil.getImageDataAsBytes(evt.data);
            Object parameters = buildParameters(evt, imgData);

            if ((parameters != null) &&
                performAction(CogToolLID.CopyImageAsBackground,
                              parameters,
                              true))
            {
                evt.detail = DND.DROP_COPY;
            }

            cancelHighlight(evt);
        }
    }

    protected void setUpDropImage(Canvas editorSubstrate,
                                  ExternalImageDropTarget dropTarget)
    {
        DropTarget editorAsTarget =
            new DropTarget(editorSubstrate, DND.DROP_MOVE | DND.DROP_COPY);

        editorAsTarget.setTransfer(ClipboardUtil.IMAGE_TRANFERS);

        editorAsTarget.addDropListener(dropTarget);
    }
}
