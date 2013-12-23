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

import java.util.EventObject;

import org.eclipse.draw2d.AbstractConnectionAnchor;
import org.eclipse.draw2d.ChopboxAnchor;
import org.eclipse.draw2d.EllipseAnchor;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.PolygonDecoration;
import org.eclipse.draw2d.geometry.Point;

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.view.InteractionDrawingEditor;
import edu.cmu.cs.hcii.cogtool.view.ResizeThumb;

public class DesignEditorTransition extends BezierPolylineConnection
                                    implements SelectionFigure<Transition>
{
    // When selected, resize handles are displayed at source and target
    // These constants may be used to disambiguate
    public static final int SOURCE = 0;
    public static final int TARGET = 1;

    protected Transition transition;

    protected AlertHandler tooltipBuilder =
        new AlertHandler() {

            public void handleAlert(EventObject evt)
            {
                buildToolTip();
            }
        };

    protected boolean showToolTip;

    // Use source.getWidgetModel() to fetch the source IWidget
    protected GraphicalSource<?> source;
    protected AbstractConnectionAnchor sourceAnchor;
    protected DesignEditorFrame target;

    protected ResizeThumb sourceHandle = null;
    protected ResizeThumb targetHandle = null;

    public DesignEditorTransition(Transition actionTransition,
                                  GraphicalSource<?> sourceFigure,
                                  DesignEditorFrame targetFigure,
                                  boolean supportToolTip)
    {
        super();

        transition = actionTransition;
        source = sourceFigure;
        target = targetFigure;
        showToolTip = supportToolTip;

        // the other choice is ChopBoxAnchor
        sourceAnchor = new EllipseAnchor(sourceFigure);
        setSourceAnchor(sourceAnchor);
        setTargetAnchor(new ChopboxAnchor(targetFigure));

        PolygonDecoration endPtDecoration = new PolygonDecoration();
        endPtDecoration.setTemplate(PolygonDecoration.TRIANGLE_TIP);

        setTargetDecoration(endPtDecoration);

        buildToolTip();

        TransitionSource source = transition.getSource();
        Frame sourceFrame = source.getFrame();
        Frame destFrame = transition.getDestination();

        source.addHandler(DesignEditorTransition.this,
                          Widget.WidgetChange.class,
                          tooltipBuilder);
        sourceFrame.addHandler(DesignEditorTransition.this,
                               NameChangeAlert.class,
                               tooltipBuilder);

        if (sourceFrame != destFrame) {
            destFrame.addHandler(DesignEditorTransition.this,
                                 NameChangeAlert.class,
                                 tooltipBuilder);
        }

        transition.addHandler(this,
                                   Transition.ActionChange.class,
                                   tooltipBuilder);

        AlertHandler handler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    buildToolTip();

                    Transition.SourceChange change =
                        (Transition.SourceChange) alert;

                    TransitionSource oldSource = change.getOldSource();
                    TransitionSource source = transition.getSource();

                    oldSource.removeAllHandlers(DesignEditorTransition.this);
                    source.addHandler(DesignEditorTransition.this,
                                      Widget.WidgetChange.class,
                                      tooltipBuilder);

                    // If the new source is part of the same frame as the old,
                    // do nothing for frame alert handlers.
                    Frame oldFrame = oldSource.getFrame();
                    Frame newFrame = source.getFrame();

                    if (oldFrame != newFrame) {
                        Frame destFrame = transition.getDestination();

                        // If it was not a self-transition, remove handler
                        if (oldFrame != destFrame) {
                            oldFrame.removeAllHandlers(DesignEditorTransition.this);
                        }

                        // If it is not now a self-transition, add handler
                        if (newFrame != destFrame) {
                            newFrame.addHandler(DesignEditorTransition.this,
                                                NameChangeAlert.class,
                                                tooltipBuilder);
                        }
                    }
                }
            };

        transition.addHandler(this,
                                   Transition.SourceChange.class,
                                   handler);

        handler =
            new AlertHandler() {

                public void handleAlert(EventObject alert)
                {
                    buildToolTip();

                    Transition.DestinationChange change =
                        (Transition.DestinationChange) alert;

                    Frame sourceFrame = transition.getSource().getFrame();
                    Frame oldFrame = change.getOldDestination();
                    Frame destFrame = transition.getDestination();

                    // If it was not a self-transition, remove handler
                    if (oldFrame != sourceFrame) {
                        oldFrame.removeAllHandlers(DesignEditorTransition.this);
                    }

                    // If it is not now a self-transition, add handler
                    if (sourceFrame != destFrame) {
                        destFrame.addHandler(DesignEditorTransition.this,
                                             NameChangeAlert.class,
                                             tooltipBuilder);
                    }
                }
            };

        transition.addHandler(this,
                                   Transition.DestinationChange.class,
                                   handler);

        // Get the number of connections.  Used for drawing bezier curves
        connections = transition.getCurveIndex();
    }

    public void addDestinationChangeHandler(AlertHandler handler)
    {
        transition.addHandler(this,
                                   Transition.DestinationChange.class,
                                   handler);
    }

    public void addActionChangeHandler(AlertHandler handler)
    {
        transition.addHandler(this,
                                   Transition.ActionChange.class,
                                   handler);
    }

    public void dispose()
    {
        TransitionSource source = transition.getSource();

        source.removeAllHandlers(this);
        source.getFrame().removeAllHandlers(this);

        transition.getDestination().removeAllHandlers(this);

        transition.removeAllHandlers(this);

        if (sourceHandle != null) {
            sourceHandle.dispose();
            targetHandle.dispose();
        }
    }

    public Transition getTransition()
    {
        return transition;
    }

    public TransitionSource getSourceModel()
    {
        return source.getModel();
    }

    public GraphicalSource<?> getSource()
    {
        return source;
    }

    public void resetSourceAnchor(GraphicalSource<?> newAnchor)
    {
        if (newAnchor != sourceAnchor.getOwner()) {
            sourceAnchor.setOwner(newAnchor);
        }
        anchorMoved(sourceAnchor);
    }

    public DesignEditorFrame getTarget()
    {
        return target;
    }

    public void changeTarget(DesignEditorFrame newTargetFigure)
    {
        setTargetAnchor(new ChopboxAnchor(newTargetFigure));
        connections = transition.getCurveIndex();
    }

    public void repaintSelection(InteractionDrawingEditor editor)
    {
        if (isSelected()) {
            double zoom = editor.getZoomSetting();

            Point pt = getStart();

            sourceHandle.centerAt(PrecisionUtilities.round(pt.x * zoom),
                                       PrecisionUtilities.round(pt.y * zoom));

            pt = getEnd();

            targetHandle.centerAt(PrecisionUtilities.round(pt.x * zoom),
                                       PrecisionUtilities.round(pt.y * zoom));

            sourceHandle.repaint();
            targetHandle.repaint();
        }
    }

    /**
     * Do NOT call this one; rather, call the two-parameter edition
     */

    public void setSelected(boolean selected)
    {
        throw new UnsupportedOperationException("Must invoke the two-parameter version");
    }

    /**
     * This exists because the editor doesn't when most DesignEditorTransition
     * instances are created!
     */
    public void setSelected(InteractionDrawingEditor editor, boolean selected)
    {
        if (selected) {
            sourceHandle = new ResizeThumb(SOURCE);
            targetHandle = new ResizeThumb(TARGET);

            sourceHandle.setData(this);
            targetHandle.setData(this);

            setLineWidth(2);

            editor.addInteractionFigure(sourceHandle);
            editor.addInteractionFigure(targetHandle);
        }
        else {
            editor.removeInteractionFigure(sourceHandle);
            editor.removeInteractionFigure(targetHandle);

            sourceHandle.dispose();
            targetHandle.dispose();

            sourceHandle = null;
            targetHandle = null;

            setLineWidth(1);
        }

        repaintSelection(editor);
    }


    public boolean isSelected()
    {
        return (sourceHandle != null);
    }


    public void dynamicHighlight(boolean highlight)
    {
        // nothing to do
    }


    public Transition getModel()
    {
        return transition;
    }

    @Override
    public void setVisible(boolean visible)
    {
        if (sourceHandle != null) {
            sourceHandle.setVisible(visible);
            targetHandle.setVisible(visible);
        }

        super.setVisible(visible);
    }

    protected void buildToolTip()
    {
        if (showToolTip) {
            String transitionAction =
                transition.getAction().getLocalizedString();
            TransitionSource source = transition.getSource();
            StringBuilder toolTipText = new StringBuilder();
            String sourceName = SWTStringUtil.insertEllipsis(source.getName(),
                                                             100,
                                                             StringUtil.EQUAL,
                                                             FontUtils.SYMBOL_FONT);
            String sourceFrameName =
                SWTStringUtil.insertEllipsis(source.getFrame().getName(),
                                             150,
                                             StringUtil.NO_FRONT,
                                             FontUtils.SYMBOL_FONT);
            String destName =
                SWTStringUtil.insertEllipsis(transition.getDestination().getName(),
                                             StringUtil.NO_FRONT,
                                             FontUtils.SYMBOL_FONT);

            toolTipText.append(KeyDisplayUtil.convertActionToDisplay(transitionAction));
            toolTipText.append("\n    " + L10N.get("DE.Source", "Source") + ": ");
            toolTipText.append(sourceName + " (" + sourceFrameName + ")");
            toolTipText.append("\n    " + L10N.get("DE.Target", "Target") + ": ");
            toolTipText.append(destName);

            Label toolTipLabel = new Label(" " + toolTipText.toString() + " ");

            toolTipLabel.setFont(FontUtils.SYMBOL_FONT);
            setToolTip(toolTipLabel);
        }
    }
}
