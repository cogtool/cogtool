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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.ConnectionAnchor;
import org.eclipse.draw2d.ConnectionAnchorBase;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FigureUtilities;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.PolylineConnection;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Color;

import edu.cmu.cs.hcii.cogtool.model.ResultStep;
import edu.cmu.cs.hcii.cogtool.model.ResultStep.ResultStepDependency;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;


/**
 * Visual representation of a single operator in a cognitive model
 * that can draw itself as a draw2d figure.
 *
 * @author jcorn
 *s
 */
public class PERTChartOperatorBar extends Figure
{
    /**
     * Class that specifies connection points for dependency lines
     *
     * @author jcorn
     *
     */
    public class DependencyAnchor extends ConnectionAnchorBase
    {
        public PERTChartOperatorBar parentBar = null;
        public PERTChartOperatorBar otherBar = null;

        // TODO: add additional variable indicating top/middle/bottom

        public boolean fromRight = true;

        public DependencyAnchor(PERTChartOperatorBar parent,
                                PERTChartOperatorBar other,
                                boolean connectionOnRight)
        {
            parentBar = parent;
            otherBar = other;
            fromRight = connectionOnRight;
        }

        protected void updateConnections()
        {
            fireAnchorMoved();
        }

        /* (non-Javadoc)
         *
         * Location is going to depend on the relative position of the two bars
         *
         * @see org.eclipse.draw2d.ConnectionAnchor#getLocation(org.eclipse.draw2d.geometry.Point)
         */
        public Point getLocation(Point arg0)
        {

            int pointX = parentBar.getX();
            int pointY = parentBar.getY();

            if (parentBar.getWidth() <= 3) {
                pointX += PrecisionUtilities.round(parentBar.getWidth() / 2.0);

            }
            else if (fromRight) {
                pointX += parentBar.getWidth() - 1;
            }

            if (Math.abs(parentBar.getY() - otherBar.getY()) < parentBar.getHeight())
            {
                pointY += PrecisionUtilities.round((parentBar.getHeight() / 2));
            }
            else if (parentBar.getY() + parentBar.getHeight() < otherBar.getY())
            {
                pointY += parentBar.getHeight();
            }

            return new Point(pointX, pointY);
        }

        public IFigure getOwner()
        {
            return parentBar;
        }

        public Point getReferencePoint()
        {
            return parentBar.getLocation();
        }
    }


    /**
     * Inner class to represent a dependency on another PERTChartOperatorBar
     * object, to be visualized as a line between the two.  Note that this
     * is extremely similar to <code>ResultStep.ResultStepDependency</code>
     *
     * @author jcorn
     *
     */
    public static class PERTStepDependency extends PolylineConnection
    {
        // TODO: This should extend PolyLineConnection and, instead of
        //       dependencyType and deltaTime, it should hold a
        //       ResultStepDependency

        /**
         * The PERTChartOperatorBar this depends on
         */
        public PERTChartOperatorBar dependency;

        public PERTChartOperatorBar parent;

        public ResultStepDependency rStepDep;

        // ---- CONSTRUCTORS -------------------------------
        public PERTStepDependency(PERTChartOperatorBar parentBar,
                                  PERTChartOperatorBar dependencyBar,
                                  ResultStepDependency rsDep)
        {
            super();

            rStepDep = rsDep;
            parent = parentBar;
            dependency = dependencyBar;

            setSourceAnchor(parentBar.connectBarToLeft(dependencyBar));
            if (dependencyBar != null) {
                setTargetAnchor(dependencyBar.connectBarToRight(parentBar));
            }
            else {
                System.err.println("Visualization is confused");
            }
            setForegroundColor(ColorConstants.black);
        }

        public int getDependencyType()
        {
            return rStepDep.dependencyType;
        }

        public double getDeltaTime()
        {
            return rStepDep.deltaTime;
        }
    }  // end PERTStepDependency


    protected double startTime;
    protected double duration;
    protected int row;
    protected int endRow = -1;

    protected Color color =
        new Color(null, GraphicsUtil.getRGBFromColor(0x999999));
    protected Color selectedColor =
        new Color(null, GraphicsUtil.getRGBFromColor(0x00CCFF));

    protected String name;
    protected ResultStep step = null;
    protected boolean selected = false;
    protected List<PERTStepDependency> dependencies =
        new ArrayList<PERTStepDependency>();

    protected DependencyAnchor leftAnchor = null;
    protected DependencyAnchor rightAnchor = null;


    // ----------- CONSTRUCTORS ------------------------------------------------

    public PERTChartOperatorBar(ResultStep resultStep,
                                List<String> resourceLabels)
    {
        this(resultStep, resourceLabels, 10);
    }


    public PERTChartOperatorBar(ResultStep resultStep,
                                List<String> resourceLabels,
                                int barHeight)
    {
        //      TODO: no hidden member variables!
        // TODO: save ResultStep object, use to hold on to startTime and duration

        step = resultStep;

        name = resultStep.operation;
        startTime = resultStep.startTime;
        duration = resultStep.duration;
        row = resourceLabels.indexOf(resultStep.resource);

        if (resultStep.targetResource != null) {
            endRow = resourceLabels.indexOf(resultStep.targetResource);
        }

        if (resultStep.colorVal > -1) {
            color.dispose();
            color = new Color(null,
                              GraphicsUtil.getRGBFromColor(resultStep.colorVal));
        }

        // TODO: does this need to be here?
        setBounds(new Rectangle(0, 0, 10, barHeight));

        selected = false;

        dependencies = new ArrayList<PERTStepDependency>();

        setBackgroundColor(color);

        //setCursor(WindowUtil.getCursor(SWT.CURSOR_HAND));
        setToolTip(new Label(resultStep.toString()));
    }


    //  ----------- UI METHODS _------------------------------------------------

    @Override
    public void paint(Graphics g)
    {
        g.pushState();

        try {
            Color drawColor = (selected) ? selectedColor : color;

            Rectangle bds = getBounds();

            g.setBackgroundColor(drawColor);
            g.setForegroundColor(ColorConstants.black);

            g.fillRectangle(bds);

            //g.fillRectangle(x,y,width - 1, height - 1);
            //g.drawRectangle(x,y,width - 1, height - 1);

            if (! selected) {
                g.setForegroundColor(ColorConstants.white);
            }

            // If we can't see at least the first 4 characters of an operator name,
            // don't display it.  This avoids unnecessary (& illegible) visual clutter
            if (name != null)  {
                String minVisibleName = name;
                if (name.length() > 3) {
                    minVisibleName = name.substring(0,3);
                }

                Dimension textDim =
                    FigureUtilities.getTextExtents(minVisibleName,
                                                   g.getFont());

                if (textDim.width < (bds.width - 5)) {
                    if (textDim.height < (bds.height -5)) {
                        g.drawText(name, bds.x + 2, bds.y + 2);
                    }
                }
            }

            g.setForegroundColor(ColorConstants.black);

            if (! isCascade()) {
                g.drawRectangle(new Rectangle(bds.x,
                                              bds.y,
                                              bds.width - 1,
                                              bds.height - 1));
            }
        }
        finally {
            g.popState();
        }
    } // paint


    public void dispose()
    {
        color.dispose();
        selectedColor.dispose();
        getCursor().dispose();
    }

    //  ----------- SETTERS ----------------------------------------------------

    public void setSelected(boolean newSelected)
    {
        selected = newSelected;
    }



    /**
     * Sets the color of the bar.  Note that this creates an SWT object which
     * must then be disposed
     *
     * @param newColor representing a color: e.g. 0x0000FF for blue
     */
    public void setColor(int newColor)
    {
        color.dispose();
        color = new Color(null, GraphicsUtil.getRGBFromColor(newColor));
        setBackgroundColor(color);
    }


    /**
     * Sets the color based on a map associating resources with colors
     *
     * @param colorMap
     */
    public void setColorWithMap(Map<String, Integer> colorMap)
    {
        if (colorMap.containsKey(step.resource)) {
            setColor((colorMap.get(step.resource)).intValue());
        }
    }


    /* (non-Javadoc)
     * Overrides setBounds() to ensure that any associated figures move.
     *
     * @see org.eclipse.draw2d.Figure#setBounds(org.eclipse.draw2d.geometry.Rectangle)
     */
    @Override
    public void setBounds(Rectangle bnds)
    {
        super.setBounds(bnds);
        fireFigureMoved();

        if (leftAnchor != null) {
            leftAnchor.updateConnections();
        }
        if (rightAnchor != null) {
            rightAnchor.updateConnections();
        }
    }



    //  ----------- GETTERS ----------------------------------------------------

    public boolean getSelected()
    {
        return selected;
    }

    public ResultStep getStep()
    {
        return step;
    }

    public int getX()
    {
        return bounds.x;
    }

    public int getY()
    {
        return bounds.y;
    }

    public int getWidth()
    {
        return bounds.width;
    }

    public int getHeight()
    {
        return bounds.height;
    }

    public double getDuration()
    {
        return duration;
    }

    public double getStartTime()
    {
        return startTime;
    }

    public int getRow()
    {
        return row;
    }

    public int getEndRow()
    {
        return endRow;
    }

    public boolean isCascade()
    {
        return (endRow > -1);
    }

    //  ----------- Dependency Methods -----------------------------------------

    public PERTStepDependency addDependency(PERTChartOperatorBar dependentOnBar,
                                            ResultStepDependency rsDep)
    {
        PERTStepDependency dep = new PERTStepDependency(this,
                                                        dependentOnBar,
                                                        rsDep);

        dependencies.add(dep);

        return dep;
    }

    /**
     * Creates and returns a new dependency anchor on the right of this bar
     * for a line drawn to the otherBar.  Note that there should be a dependency
     * anchor on the otherBar as well, which will need to be explicitly created.
     *
     * @param otherBar
     * @return
     */
    public ConnectionAnchor connectBarToRight(PERTChartOperatorBar otherBar)
    {
        rightAnchor = new DependencyAnchor(this, otherBar, true);

        return rightAnchor;
    }

    /**
     * Creates and returns a new dependency anchor on the left of this bar
     * for a line drawn to the otherBar.  Note that there should be a dependency
     * anchor on the otherBar as well, which will need to be explicitly created.
     *
     * @param otherBar
     * @return
     */
    public ConnectionAnchor connectBarToLeft(PERTChartOperatorBar otherBar)
    {
        leftAnchor = new DependencyAnchor(this, otherBar, false);

        return leftAnchor;
    }


    public void addDependency(PERTStepDependency dep)
    {
        dependencies.add(dep);
    }


    public List<PERTStepDependency> getDependencies()
    {
        return dependencies;
    }
}
