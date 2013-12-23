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

import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.FigureListener;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.LightweightSystem;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.TreeSearch;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import edu.cmu.cs.hcii.cogtool.model.ResultStep;
import edu.cmu.cs.hcii.cogtool.ui.PERTChartSelectionState;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartOperatorBar;
import edu.cmu.cs.hcii.cogtool.uimodel.PERTChartOperatorBar.PERTStepDependency;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.ITimeSliceDisplayable;
import edu.cmu.cs.hcii.cogtool.util.PrecisionUtilities;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil.MenuListener;

/**
 * Canvas class that displays a PERT visualization of PERTChartOperatorBar
 * objects, which get their data from model.StandaloneAlgo.ModelStep objects.
 *
 * Subclasses may want to display additional information within the panel.  The
 * margin values define an interior region in which the visualization may be
 * drawn, which allows additional data to be drawn outside of that region.  The
 * PERTInteractivePanel draws a timeline in the upper margin, for example.
 *
 * @author jbc
 *
 */
public class PERTPanel extends Canvas implements ITimeSliceDisplayable
{
    /**
     * Figure for displaying a transparent blue selection halo
     *
     * @author Jason Cornwell
     *
     */
    public class SelectionHalo extends RectangleFigure
    {
        public PERTChartOperatorBar target = null;
        public FigureListener targetListener = null;
        public int offset = 0;
        public Color color = null;

        public SelectionHalo()
        {
            super();

            color = new Color(null,
                                   GraphicsUtil.getRGBFromColor(0x00CCFF));

            setToolTip(new Label(""));
            setVisible(true);
//            setCursor(WindowUtil.getCursor(SWT.CURSOR_HAND));

            targetListener =
                new FigureListener()
                    {
                        public void figureMoved(IFigure source)
                        {
                            setBoundsFromFigure(source);
                        }
                    };
        }

        /**
         * Sets the bounds to be greater than the target figure by 40% of the
         * height of the target figure in each direction.
         *
         * @param figure
         */
        protected void setBoundsFromFigure(IFigure figure)
        {
            Rectangle tBounds = figure.getBounds();

            offset = 5; //(int) (tBounds.height * 0.4);
            this.setBounds(new Rectangle(tBounds.x - offset,
                                         tBounds.y - offset,
                                         tBounds.width + (2 * offset),
                                         tBounds.height + (2 * offset)));
        }

        public PERTChartOperatorBar getTarget()
        {
            return target;
        }

        public void dispose()
        {
            color.dispose();
            //getCursor().dispose();
        }

        /**
         * Sets the target figure for the halo to appear around
         *
         * @param bar
         */
        public void setTarget(PERTChartOperatorBar bar)
        {
            if (target != null) {
                target.removeFigureListener(targetListener);
            }

            target = bar;

            if (target != null) {
                setBoundsFromFigure(target);

                // Get tooltip and save it

                String toolTipText = "Selected Operation";
                try {
                    toolTipText = target.getStep().toString();
                }
                catch (NullPointerException ex) {
                    Label targetToolTip = (Label) target.getToolTip();

                    if (targetToolTip != null) {
                        toolTipText = targetToolTip.getText();
                    }
                    else {
                        toolTipText = "Selected Operation";
                    }
                }

                //((Label) this.getToolTip()).setText(toolTipText);
                setToolTip(new Label(toolTipText));

                target.addFigureListener(targetListener);

            }
            else {
                offset = 0;
                this.setBounds(new Rectangle(0, 0, 0, 0));
            }
        }


        /* (non-Javadoc)
         * Draws the halo
         *
         * @see org.eclipse.draw2d.Figure#paint(org.eclipse.draw2d.Graphics)
         */
        @Override
        public void paint(Graphics g)
        {
            if (target != null) {
                g.pushState();

                try {
                    g.setBackgroundColor(color);
                    g.setAlpha(50);
                    g.fillRoundRectangle(bounds,
                                         2 * offset,
                                         2 * offset);
                }
                finally {
                    g.popState();
                }
            }
        }
    }

    /**
     * List of PERTChartOperatorBar objects to draw
     */
    protected List<PERTChartOperatorBar> bars;

    protected List<PERTStepDependency> connections;

    /**
     * The lightweight system is the interface layer between SWT and draw2d.
     *
     */
    protected LightweightSystem lws;

    protected IFigure contents;

    protected IFigure background;

    protected List<SelectionHalo> selectionBoxes =
        new ArrayList<SelectionHalo>();

    /**
     * The number of resources used by the model, which indicates the number of
     * rows on which PERTChartOperatorBar objects may be drawn
     */
    protected int numRows;

    /**
     * Duration of the model run, in ms.
     */
    protected double totalTime;
    protected double nativeTotalTime;

    protected double displayStartTime = 0;
    protected double displayEndTime = 0;

    /**
     * Margin values define the interior region in which the visualization is
     * drawn
     */
    protected int marginTop;
    protected int marginBottom;
    protected int marginLeft;
    protected int marginRight;

    protected int barHeight;

    // For recovery of the alert handler
    protected PERTChartSelectionState chartSelectionState;

    protected static final Map<String, Integer> colorMap =
        new HashMap<String, Integer>();
    static {
        colorMap.put(ResultStep.FRAME_RESOURCE, new Integer(0x999999));
        colorMap.put(ResultStep.PRODUCTIONS_RESOURCE, new Integer(0xaaaaaa));
        colorMap.put(ResultStep.SYSTEM_RESOURCE, new Integer(0xcccccc));
        colorMap.put(ResultStep.VISION_RESOURCE, new Integer(0xa24ff7));
        colorMap.put(ResultStep.VISION_EXEC_RESOURCE, new Integer(0x450289));
        colorMap.put(ResultStep.VISION_PREP_RESOURCE, new Integer(0xa24ff7));
        colorMap.put(ResultStep.VISION_ENC_RESOURCE, new Integer(0xd7b9f6));
        colorMap.put(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE, new Integer(0xFF3333));
        colorMap.put(ResultStep.MOTOR_RIGHT_INIT_RESOURCE, new Integer(0xFF6666));
        colorMap.put(ResultStep.MOTOR_RIGHT_PREP_RESOURCE, new Integer(0xFF9999));
        colorMap.put(ResultStep.MOTOR_LEFT_EXEC_RESOURCE, new Integer(0xFF3333));
        colorMap.put(ResultStep.MOTOR_LEFT_INIT_RESOURCE, new Integer(0xFF6666));
        colorMap.put(ResultStep.MOTOR_LEFT_PREP_RESOURCE, new Integer(0xFF9999));

        colorMap.put(ResultStep.SPEECH_PREP_RESOURCE,
                     colorMap.get(ResultStep.MOTOR_RIGHT_PREP_RESOURCE));
        colorMap.put(ResultStep.SPEECH_EXEC_RESOURCE,
                     colorMap.get(ResultStep.MOTOR_RIGHT_EXEC_RESOURCE));

        // TODO (hear kludge) remove this when (if ever?)
        // we do things correctly with the aural module
        // of ACT-R.
        colorMap.put(ResultStep.HEAR_RESOURCE,
                     colorMap.get(ResultStep.VISION_ENC_RESOURCE));
    }


    protected static TreeSearch barFilter =
        new TreeSearch () {
            // TODO: we need to be able to select a small neighbor,
            //       but also not lose selection when you click in the halo.
            public boolean accept(IFigure figure)
            {
                return (figure instanceof PERTChartOperatorBar);
            }

            public boolean prune(IFigure figure)
            {
                return false;
            }
        };


    /**
     * Constructor with default margin values of 5 pixels.
     *
     * @param parent containing Composite
     * @param steps List of ResultStep objects to visualize
     * @param resourceLabels List of Strings describing model resources
     * @param barHeight height of each PERTChartOperatorBar object when drawn
     */
    public PERTPanel(Composite parent,
                     List<ResultStep> steps,
                     List<String> resourceLabels,
                     int bHeight)
    {
        this(parent, steps, resourceLabels, bHeight, 5, 5, 5, 5);
    }


    /**
     * PERTPanel Constructor
     *
     * @param parent containing Composite
     * @param steps List of ResultStep objects to visualize
     * @param resourceLabels List of Strings describing model resources
     * @param barHeight height of each PERTChartOperatorBar object when drawn
     * @param marginT upper margin height, in pixels
     * @param marginB lower margin height, in pixels
     * @param marginL left margin width, in pixels
     * @param marginR right margin width, in pixels
     */
    public PERTPanel(Composite parent,
                     List<ResultStep> steps,
                     List<String> resourceLabels,
                     int bHeight,
                     int marginT, int marginB, int marginL, int marginR)
    {
        super(parent, SWT.NONE);

        // create lightweight system
        lws = new LightweightSystem(this);
        contents = new RectangleFigure();
        contents.setBackgroundColor(ColorConstants.white);
        contents.setForegroundColor(ColorConstants.white);
        ((RectangleFigure) contents).setFill(true);

        lws.setContents(contents);

        // set margins
        marginTop = marginT;
        marginBottom = marginB;
        marginLeft = marginL;
        marginRight = marginR;

        barHeight = bHeight;

        numRows = resourceLabels.size();

        // create list of PERTChartOperatorBar from the ModelStep list

        Iterator<ResultStep> stepIterator = steps.iterator();
        bars = new ArrayList<PERTChartOperatorBar>();
        connections = new ArrayList<PERTStepDependency>();
        // TODO: Look into not saving the connections

        nativeTotalTime = 0.0;

        ResultStep step;
        PERTChartOperatorBar bar;

        // create a map so that we can copy the dependency structure from the
        // ResultSteps to the PERTChartOperatorBars
        // TODO: save this state, and use this instead of bars
        Map<ResultStep, PERTChartOperatorBar> barMap =
            new HashMap<ResultStep, PERTChartOperatorBar>();
        List<PERTChartOperatorBar> operators =
            new ArrayList<PERTChartOperatorBar>();

        // iterate through once and create operator bars
        while (stepIterator.hasNext()) {
            step = stepIterator.next();
            bar =
                new PERTChartOperatorBar(step, resourceLabels, barHeight);

            bar.setColorWithMap(colorMap);

            barMap.put(step, bar);

            bars.add(bar);
            if (bar.isCascade()) {
                contents.add(bar);
            }
            else {
                operators.add(bar);
            }

            // compare to find the total time
            nativeTotalTime =
                Math.max(nativeTotalTime, step.startTime + step.duration);
        }

        // iterator through operators, and add them now:
        Iterator<PERTChartOperatorBar> opIter = operators.iterator();
        while (opIter.hasNext()) {
            contents.add(opIter.next());
        }

        // iterate through again setting dependencies on each bar object
        //   based on the dependencies in each ResultStep object

        ResultStep.ResultStepDependency rStepDep;
        Iterator<ResultStep.ResultStepDependency> depStepIter;
        stepIterator = steps.iterator();

        while (stepIterator.hasNext()) {
            step = stepIterator.next();
            bar = barMap.get(step);
            depStepIter = step.getDependencies().iterator();

            while (depStepIter.hasNext()) {
                rStepDep = depStepIter.next();
                PERTChartOperatorBar otherBar = barMap.get(rStepDep.dependency);

                PERTStepDependency dep = bar.addDependency(otherBar, rStepDep);
                contents.add(dep);
                connections.add(dep);
            }
        }

        // set initial start and end times to show entire region
        displayEndTime = nativeTotalTime;
        displayStartTime = 0.0;

        // set initial total time to the native time
        totalTime = nativeTotalTime;

        addListener(SWT.Resize,
                         new Listener() {
                             public void handleEvent(Event e) {
                                 resizeVisualization(e);
                             }
                         });
    }

    /**
     * Returns the PERTChartOperatorBar under the given coordinates.  Note that
     * this
     * @param x
     * @param y
     * @return
     */
    public PERTChartOperatorBar findBarAt(int x, int y)
    {
        return (PERTChartOperatorBar) contents.findFigureAt(x, y,
                                                                 barFilter);
//        PERTChartOperatorBar bar;
//        Iterator<PERTChartOperatorBar> barIt = bars.iterator();
//        while (barIt.hasNext())
//        {
//            bar = barIt.next();
//            if (bar.containsPoint(x, y))
//            {
//                return bar;
//            }
//        }
//        return null;
    }


    /**
     * @return an integer containing the width of the actual drawn
     *         visualization  in pixels. This does not include the margins!
     */
    public int getVisualizationWidth()
    {
        return getBounds().width - (marginLeft + marginRight);
    }

    /**
     * @return an integer containing the height of the actual drawn
     *         visualization in pixels. This does not include the margins!
     */
    public int getVisualizationHeight()
    {
        return getBounds().height - (marginTop + marginBottom);
    }


    public int getTopMargin()
    {
        return marginTop;
    }

    public int getBottomMargin()
    {
        return marginBottom;
    }

    public int getLeftMargin()
    {
        return marginLeft;
    }

    public int getRightMargin()
    {
        return marginRight;
    }


    public void setTotalTime(double tTime) {
        totalTime = Math.max(nativeTotalTime, tTime);
        resizeVisualization(null);
    }

    public double getNativeTotalTime() {
        return nativeTotalTime;
    }

    /**
     * Resets the position and size of the PERTChartOperatorBar objects
     * when the panel is resized.
     *
     * @param e resize Event to respond to.
     */
    public void resizeVisualization(Event e)
    {
        int totalWidth = getVisualizationWidth();
        int totalHeight = getVisualizationHeight();

        int regionHeight =
            PrecisionUtilities.round((totalHeight - 10) / Math.max(numRows,
                                                                   1));

        Iterator<PERTChartOperatorBar> barIterator = bars.iterator();
        PERTChartOperatorBar bar;

        int barW;
        int barH;
        int barX;
        int barY;

        // rescale the width of each bar based on the ratio of it's duration to
        // the total time, and set its new position

        double displayedDuration = displayEndTime - displayStartTime;

        if ((totalTime > 0) && (displayedDuration > 0)) {
            while (barIterator.hasNext()) {
                bar = barIterator.next();

                // Calculate position based on the position of the right of the
                // bar to avoid oscillating widths
                int barRightPos =
                    marginLeft
                        + PrecisionUtilities.round(((bar.getStartTime()
                                                       + bar.getDuration()
                                                       - displayStartTime)
                                                    / displayedDuration)
                                                    * (totalWidth - 10));

//
//                barW = PrecisionUtilities.round((bar.getDuration() * (totalWidth - 10))
//                                                / displayedDuration) + 1;

                barX =
                    marginLeft
                        + PrecisionUtilities.round(((bar.getStartTime()
                                                       - displayStartTime)
                                                    / displayedDuration)
                                                    * (totalWidth - 10));

                barW = barRightPos - barX + 1;

                if (bar.isCascade()) {
                    int y1 = Math.min(bar.getRow(), bar.getEndRow());
                    int y2 = Math.max(bar.getRow(), bar.getEndRow());

                    barY =
                        marginTop
                            + (y1 * regionHeight)
                            + PrecisionUtilities.round(0.5 * barHeight);

                    barH = (y2 - y1) * regionHeight;

                    if (barW < 1) {
                        barW = 1;
                    }
                }
                else {
                    barY = marginTop + (bar.getRow() * regionHeight);
                    barH = barHeight;

                    if (barW < 3) {
                        barX -= 1;
                        barW = 3;
                    }
                }

                bar.setBounds(new Rectangle(barX, barY, barW, barH));
            }

        }

        lws.getUpdateManager().performUpdate();
    }


    /**
     * @return List of PERTChartOperatorBar objects that are being displayed
     */
    public List<PERTChartOperatorBar> getOperatorBarList()
    {
        return bars;
    }


    /**
     * @return total time of the trace being visualized, in ms
     */
    public double getTotalTime()
    {
        return totalTime;
    }

    /**
     * Add a mouse handler for mouse move and click events
     *
     * @param mouseState
     */
    public void addMouseHandler(MenuListener mouseState,
                                MouseMoveListener moveListener)
    {
        if (mouseState != null) {
            addMouseListener(mouseState);
        }

        if (moveListener != null) {
            addMouseMoveListener(moveListener);
        }
    }


    /**
     * Associates this panel with a PERTChartSelectionState so that when a new
     * operator is selected, this panel will redraw itself accordingly.
     *
     * @param selectionState
     */
    public void observeSelectionState(PERTChartSelectionState selectionState)
    {
        chartSelectionState = selectionState;

        AlertHandler handler = new AlertHandler() {
            public void handleAlert(EventObject alert)
            {
                // TODO: Change this to map or set
                List<ResultStep> selectedSteps =
                    ((PERTChartSelectionState.SelectionChange) alert).selectedSteps;

                // clear selection boxes
                Iterator<SelectionHalo> haloIterator = selectionBoxes.iterator();
                SelectionHalo deadHalo = null;

                while (haloIterator.hasNext()) {
                    deadHalo = haloIterator.next();
                    contents.remove(deadHalo);
                    deadHalo.dispose();
                }

                selectionBoxes.clear();

                Iterator<PERTChartOperatorBar> barIterator = bars.iterator();
                PERTChartOperatorBar bar;

                while (barIterator.hasNext()) {
                    bar = barIterator.next();

                    if (selectedSteps.contains(bar.getStep())) {
                        bar.setSelected(true);

                        SelectionHalo halo = new SelectionHalo();
                        halo.setTarget(bar);
                        contents.add(halo);
                        selectionBoxes.add(halo);

                    }
                    else {
                        bar.setSelected(false);
                    }
                }

                //redraw();
                contents.repaint();

            }
        };

        chartSelectionState.addHandler(this,
                                            PERTChartSelectionState.SelectionChange.class,
                                            handler);
    }


    @Override
    public void dispose()
    {
        //iterate through PERTChartOperatorBars and dispose them
        Iterator<PERTChartOperatorBar> pIterator = bars.iterator();

        while (pIterator.hasNext()) {
            pIterator.next().dispose();
        }

        Iterator<SelectionHalo> haloIter = selectionBoxes.iterator();

        while (haloIter.hasNext()) {
             haloIter.next().dispose();
        }

        chartSelectionState.removeAllHandlers(this);

        super.dispose();
    }


    public void figureMoved(IFigure arg0)
    {
        // TODO Auto-generated method stub
    }


    public void displayTimeSlice(double start, double end)
    {
        displayStartTime = start;
        displayEndTime = end;

        resizeVisualization(null);
    }


    public void setResultSteps(List<ResultStep> steps,
                               List<String> resourceLabels)
    {
        // First remove seleciton halos
        Iterator<SelectionHalo> haloIterator = selectionBoxes.iterator();
        SelectionHalo deadHalo = null;

        while (haloIterator.hasNext()) {
            deadHalo = haloIterator.next();
            contents.remove(deadHalo);
            deadHalo.dispose();
        }

        selectionBoxes.clear();


        // Then remove existing operator bars
        Iterator<PERTChartOperatorBar> barIt = bars.iterator();
        PERTChartOperatorBar deadBar = null;
        while (barIt.hasNext()) {
            deadBar = barIt.next();
            Iterator<PERTStepDependency> depIt =
                deadBar.getDependencies().iterator();
            while (depIt.hasNext()) {
                PERTStepDependency deadDependency = depIt.next();

                contents.remove(deadDependency);
            }

            contents.remove(deadBar);
            //deadBar.dispose();
        }

        bars.clear();

        Iterator<ResultStep> stepIterator = steps.iterator();

        ResultStep step;
        PERTChartOperatorBar bar;

        // create a map so that we can copy the dependency structure from the
        // ResultSteps to the PERTChartOperatorBars
        // TODO: save this state, and use this instead of bars
        Map<ResultStep, PERTChartOperatorBar> barMap =
            new HashMap<ResultStep, PERTChartOperatorBar>();
        List<PERTChartOperatorBar> operators =
            new ArrayList<PERTChartOperatorBar>();

        // iterate through once and create operator bars
        while (stepIterator.hasNext()) {
            step = stepIterator.next();
            bar =
                new PERTChartOperatorBar(step, resourceLabels, barHeight);

            bar.setColorWithMap(colorMap);

            barMap.put(step, bar);

            bars.add(bar);
            if (bar.isCascade()) {
                contents.add(bar);
            }
            else {
                operators.add(bar);
            }

            // coompare to find the total time
            totalTime =
                Math.max(totalTime, step.startTime + step.duration);
        }

        // iterator through operators, and add them now:
        Iterator<PERTChartOperatorBar> opIter = operators.iterator();
        while (opIter.hasNext()) {
            contents.add(opIter.next());
        }

        // iterate through again setting dependencies on each bar object
        //   based on the dependencies in each ResultStep object

        ResultStep.ResultStepDependency rStepDep;
        Iterator<ResultStep.ResultStepDependency> depStepIter;
        stepIterator = steps.iterator();

        double ntvTotalTime = 0.0d;

        while (stepIterator.hasNext()) {
            step = stepIterator.next();
            bar = barMap.get(step);
            depStepIter = step.getDependencies().iterator();

            ntvTotalTime =
                Math.max(ntvTotalTime, step.startTime + step.duration);

            while (depStepIter.hasNext()) {
                rStepDep = depStepIter.next();
                PERTChartOperatorBar otherBar = barMap.get(rStepDep.dependency);

                PERTStepDependency dep = bar.addDependency(otherBar, rStepDep);
                contents.add(dep);
                connections.add(dep);
            }
        }

        nativeTotalTime = ntvTotalTime;
        totalTime = nativeTotalTime;

        numRows = resourceLabels.size();

        resizeVisualization(null);


    }
}
