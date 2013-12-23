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

package edu.cmu.cs.hcii.cogtool.controller;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.L10N;

/*
 * For CogTool, this class provides default implementations for zooming
 * Controller semantic actions.
 * <p>
 * Semantic actions and parameters (in addition to DefaultController):
 *      SetZoom         Double indicating new zoom factor
 *      ZoomToFit       <no parameters>
 *      ZoomNormal      <no parameters>
 *      ZoomIn          <no parameters>
 *      ZoomOut         <no parameters>
 */
public abstract class ZoomableController extends DefaultController
{
    protected static final String ALIGN_TOP =
        L10N.get("UNDO.FE.AlignTop", "Align Top");

    protected static final String ALIGN_BOTTOM =
        L10N.get("UNDO.FE.AlignBottom", "Align Bottom");

    protected static final String ALIGN_LEFT =
        L10N.get("UNDO.FE.AlignLeft", "Align Left");

    protected static final String ALIGN_RIGHT =
        L10N.get("UNDO.FE.AlignRight", "Align Right");

    protected static final String ALIGN_CENTER =
        L10N.get("UNDO.FE.AlignCenter", "Align Centers");

    protected static final String ALIGN_HORIZ_CENTER =
        L10N.get("UNDO.FE.AlignHorizCenter", "Align Horizontal Center");

    protected static final String ALIGN_VERT_CENTER =
        L10N.get("UNDO.FE.AlignVertCenter", "Align Vertical Center");

    protected static final String SPACE_VERTICALLY =
        L10N.get("UNDO.FE.SpaceVertically", "Space Vertically");

    protected static final String SPACE_HORIZONTALLY =
        L10N.get("UNDO.FE.SpaceHorizontally", "Space Horizontally");

    protected abstract static class AlignmentAction implements IListenerAction
    {
        // Constants that define which alignment action is requested
        protected static final int TOP = 0;
        protected static final int BOTTOM = 1;
        protected static final int LEFT = 2;
        protected static final int RIGHT = 3;
        protected static final int CENTER = 4;
        protected static final int HORIZ_CENTER = 5;
        protected static final int VERT_CENTER = 6;

        protected static final String[] ALIGNMENT_NAMES =
            new String[] {
                ALIGN_TOP,
                ALIGN_BOTTOM,
                ALIGN_LEFT,
                ALIGN_RIGHT,
                ALIGN_CENTER,
                ALIGN_HORIZ_CENTER,
                ALIGN_VERT_CENTER
            };

        protected static final CogToolLID[] LIDS =
            new CogToolLID[] {
                CogToolLID.AlignTop,
                CogToolLID.AlignBottom,
                CogToolLID.AlignLeft,
                CogToolLID.AlignRight,
                CogToolLID.AlignCenter,
                CogToolLID.AlignHorizCenter,
                CogToolLID.AlignVertCenter
            };

        /**
         * Reference rectangle used to specify which rectangle is
         * used to align others in a particular direction
         */
        protected DoubleRectangle reference =
            new DoubleRectangle(0.0, 0.0, 0.0, 0.0);

        protected boolean isFirst = true;

        /**
         * New X location for the moved objects
         */
        protected double newX;

        /**
         * New Y location for the moved objects
         */
        protected double newY;

        /**
         * The alignment action requested
         */
        protected int action;

        public AlignmentAction(int alignAction)
        {
            action = alignAction;
        }

        /**
         * Obtains the display name for this action
         *
         * @return a human-readable string
         */
        protected String getAlignmentName()
        {
            return ALIGNMENT_NAMES[action];
        }

        /**
         * Subclass implementations of performAction must invoke reset() first.
         */
        protected void reset()
        {
            isFirst = true;
        }

        /**
         * Obtains a reference dimension that enables us to decide which widget
         * should become the reference widget. This method must return a value,
         * dependent on the specified parameter, such that the widget whose
         * bounds result in the largest return value becomes the reference
         * widget.
         *
         * @param bounds the bounds to compute the reference dimension from
         */
        protected void computeReference(DoubleRectangle r)
        {
            switch (action) {
                case TOP: {
                    if (isFirst || (reference.y > r.y)) {
                        reference.y = r.y;
                    }
                    break;
                }
                case BOTTOM: {
                    if (isFirst) {
                        reference.y = r.y;
                        reference.height = r.height;
                    }
                    else {
                        double refBottom =
                            reference.y
                                + reference.height;

                        if (refBottom < r.y + r.height) {
                            reference.y = r.y;
                            reference.height = r.height;
                        }
                    }
                    break;
                }
                case LEFT: {
                    if (isFirst || (reference.x > r.x)) {
                        reference.x = r.x;
                    }
                    break;
                }
                case RIGHT: {
                    if (isFirst) {
                        reference.x = r.x;
                        reference.width = r.width;
                    }
                    else {
                        double refRight =
                            reference.x + reference.width;

                        if (refRight < r.x + r.width) {
                            reference.x = r.x;
                            reference.width = r.width;
                        }
                    }
                    break;
                }
                case CENTER: {
                    if (isFirst) {
                        reference.copyFrom(r);
                    }
                    else {
                        reference.unionInto(r.x,
                                                 r.y,
                                                 r.width,
                                                 r.height);
                    }
                    break;
                }
                case HORIZ_CENTER: {
                    if (isFirst) {
                        reference.copyFrom(r);
                    }
                    else {
                        reference.unionInto(r.x,
                                                 r.y,
                                                 r.width,
                                                 r.height);
                    }
                    break;
                }
                case VERT_CENTER: {
                    if (isFirst) {
                        reference.copyFrom(r);
                    }
                    else {
                        reference.unionInto(r.x,
                                                 r.y,
                                                 r.width,
                                                 r.height);
                    }
                    break;
                }
            }

            isFirst = false;
        } // computeReference

        /**
         * Calculates the new origin of a widget bounded by the specified
         * parameter. This method relies on the stored object state (i.e. the
         * reference bound).
         *
         * @param bounds the current bounds of the widget we want to reposition
         */
        protected void computeNewOrigin(DoubleRectangle bounds)
        {
            switch (action) {
                case TOP: {
                    // To align to the top, leave the
                    // x alone, and set the y
                    newX = bounds.x;
                    newY = reference.y;
                    break;
                }
                case BOTTOM: {
                    // To align from the bottom
                    // set the y to the bottom minus the
                    // height of the widget
                    newX = bounds.x;
                    newY = reference.y
                                      + reference.height
                                      - bounds.height;
                    break;
                }
                case LEFT: {
                    // set the X to the reference location
                    newX = reference.x;

                    // leave y untouched
                    newY = bounds.y;
                    break;
                }
                case RIGHT: {
                    // Align on the right by getting the
                    // left edges set to a fixed value,
                    // and sub the widget
                    newX = reference.x
                                      + reference.width
                                      - bounds.width;

                    // leave y untouched
                    newY = bounds.y;
                    break;
                }
                case CENTER: {
                    // set to the center of the union reference
                    newX =
                        reference.x
                            + (reference.width / 2.0)
                            - (bounds.width / 2.0);
                    newY =
                        reference.y
                            + (reference.height / 2.0)
                            - (bounds.height / 2.0);
                    break;
                }
                case HORIZ_CENTER: {
                    // keep y;
                    // set x to the center of the union reference
                    newX =
                        reference.x
                            + (reference.width / 2.0)
                            - (bounds.width / 2.0);

                    newY = bounds.y;
                    break;
                }
                case VERT_CENTER: {
                    // keep x;
                    // set y to the center of the union reference
                    newX = bounds.x;

                    newY =
                        reference.y
                            + (reference.height / 2.0)
                            - (bounds.height / 2.0);
                    break;
                }
            }
        } // computeNewOrigin
    }

    public ZoomableController(Project proj)
    {
        super(proj);
    }

    /**
     * Fetch the implementation of the zoomable view for this window.
     *
     * @return the implementation of the zoomable view for this window
     */
    protected abstract ZoomableUI getZoomableUI();

    /**
     * Registers the set of <code>IListenerAction</code> instances
     * that implement the semantic actions that are possible.
     * <p>
     * For this class, this consists of the actions that all CogTool
     * model editing windows support.
     *
     * @author mlh
     */
    @Override
    protected void assignActions()
    {
        super.assignActions();

        final ZoomableUI ui = getZoomableUI();

        if (ui != null) {
            ui.setAction(CogToolLID.SetZoom,
                         new IListenerAction() {

                             public Class<?> getParameterClass()
                             {
                                 return Double.class;
                             }


                             public boolean performAction(Object prm)
                             {
                                 double zoom = ((Double) prm).doubleValue();
                                 ui.setZoom(zoom);

                                 return true;
                             }
                         });

            ui.setAction(CogToolLID.ZoomToFit,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 ui.zoomToFit();
                                 return true;
                             }
                         });

            ui.setAction(CogToolLID.ZoomNormal,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 ui.zoomToActual();
                                 return true;
                             }
                         });

            ui.setAction(CogToolLID.ZoomIn,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 ui.zoomIn();
                                 return true;
                             }
                         });

            ui.setAction(CogToolLID.ZoomOut,
                         new AListenerAction() {

                             public boolean performAction(Object prms)
                             {
                                 ui.zoomOut();
                                 return true;
                             }
                         });
        }
    }
}
