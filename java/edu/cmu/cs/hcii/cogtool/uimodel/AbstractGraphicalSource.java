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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Map.Entry;

import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Label;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;

import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil.SWTWidthComputer;

public abstract class AbstractGraphicalSource<T extends TransitionSource>
                                                 extends Figure
                                                 implements GraphicalSource<T>
{
    private class TransitionIterator implements Iterator<DesignEditorTransition>
    {
        protected Iterator<Entry<Transition, DesignEditorTransition>> inner =
            transitions.entrySet().iterator();
        protected Entry<Transition, DesignEditorTransition> item = null;


        public boolean hasNext()
        {
            return this.inner.hasNext();
        }


        public DesignEditorTransition next()
        {
            if (hasNext()) {
                this.item = this.inner.next();

                return this.item.getValue();
            }

            throw new NoSuchElementException();
        }


        public void remove()
        {
            if (this.item == null) {
                throw new IllegalStateException("Either next() has not been called or remove() has been called twice");
            }

            Transition trans = this.item.getKey();

            trans.removeAllHandlers(AbstractGraphicalSource.this);
            this.inner.remove();
            this.item = null;
        }
    }

    protected Map<Transition, DesignEditorTransition> transitions =
        new HashMap<Transition, DesignEditorTransition>();

    protected boolean showToolTip;

    protected ImageData foreground;

    protected T model;

    /**
     * Used to determine the proper translucency alpha for the color overlay
     * when displaying this graphical source.
     */
    protected FrameUIModel displayAlpha;

    protected AlertHandler changeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                buildToolTip();

                if (alert instanceof Transition.DestinationChange) {
                    Transition.DestinationChange change =
                        (Transition.DestinationChange) alert;

                    Transition transition = (Transition) change.getSource();
                    Frame sourceFrame = transition.getSource().getFrame();
                    Frame oldFrame = change.getOldDestination();
                    Frame destFrame = transition.getDestination();

                    // If it was not a self-transition, remove handler
                    if (oldFrame != sourceFrame) {
                        oldFrame.removeAllHandlers(AbstractGraphicalSource.this);
                    }

                    // If it is not now a self-transition, add handler
                    if (sourceFrame != destFrame) {
                        destFrame.addHandler(AbstractGraphicalSource.this,
                                             NameChangeAlert.class,
                                             changeHandler);
                    }
                }
            }
        };

    protected AlertHandler transitionChangeHandler =
        new AlertHandler() {

            public void handleAlert(EventObject alert)
            {
                buildToolTip();

                TransitionSource.TransitionChange chg =
                    (TransitionSource.TransitionChange) alert;
                Transition transitionChg = (Transition) chg.element;

                if (chg.isAdd) {
                    transitionChg.addHandler(AbstractGraphicalSource.this,
                                             Transition.ActionChange.class,
                                             changeHandler);
                    transitionChg.addHandler(AbstractGraphicalSource.this,
                                             Transition.DestinationChange.class,
                                             changeHandler);
                    transitionChg.getDestination().addHandler(this,
                                                              NameChangeAlert.class,
                                                              changeHandler);
                }
                else {
                    transitionChg.removeHandler(Transition.ActionChange.class,
                                                changeHandler);
                    transitionChg.removeHandler(Transition.DestinationChange.class,
                                                changeHandler);
                    transitionChg.getDestination().removeHandler(NameChangeAlert.class,
                                                                 changeHandler);
                }
            }
        };

    public AbstractGraphicalSource(boolean supportToolTip,
                            int rolloverCursor,
                            FrameUIModel alphaComputer)
    {
        super();

        setOpaque(false);
        this.showToolTip = supportToolTip;
        this.displayAlpha = alphaComputer;

        setCursor(rolloverCursor);
    }

    protected void setCursor(int rolloverCursor)
    {
        setCursor(WindowUtil.getCursor(rolloverCursor));
    }

    // Must be called by subclass constructor
    protected void setSourceModel(T m)
    {
        if (m == null) {
            throw new IllegalArgumentException("Cannot set the model of a graphical source to null");
        }

        TransitionSource oldModel = getModel();

        if (oldModel != m) {
            if (oldModel != null) {
                oldModel.removeAllHandlers(this);
            }

            setModel(m);

            buildToolTip();

            getModel().addHandler(this,
                                  Widget.WidgetChange.class,
                                  this.changeHandler);
            getModel().addHandler(this,
                                  TransitionSource.TransitionChange.class,
                                  this.transitionChangeHandler);

            Iterator<Transition> modelTransitions =
                getModel().getTransitions().values().iterator();

            while (modelTransitions.hasNext()) {
                Transition t = modelTransitions.next();

                t.addHandler(this,
                             Transition.ActionChange.class,
                             this.changeHandler);
                t.addHandler(this,
                             Transition.DestinationChange.class,
                             this.changeHandler);
                t.getDestination().addHandler(this,
                                              NameChangeAlert.class,
                                              this.changeHandler);
            }

            // Inherit attributes from model
            updateAttributes();
        }
    }

    protected void setModel(T m)
    {
        this.model = m;
    }

    protected void updateAttributes()
    {
        updateTitle();
    }


    public T getModel()
    {
        return this.model;
    }

    /**
     * Release resources held by the graphical source
     */

    public void dispose()
    {
        TransitionSource source = getModel();
        Iterator<Transition> modelTransitions =
            source.getTransitions().values().iterator();

        while (modelTransitions.hasNext()) {
            Transition t = modelTransitions.next();
            t.removeAllHandlers(this);
            t.getDestination().removeAllHandlers(this);
        }

        source.removeAllHandlers(this);
    }

    /**
     * Need to specialize the listener so that on Remove, it cleans up
     * after its self in a nice way. IE: remove any alerters it has
     * attached.
     */

    public Iterator<DesignEditorTransition> getTransitionsIterator()
    {
        return new TransitionIterator();
    }


    public DesignEditorTransition getTransition(Transition transition)
    {
        return this.transitions.get(transition);
    }


    public void addTransition(Transition t, DesignEditorTransition f)
    {
        this.transitions.put(t, f);
        t.addHandler(this, Transition.ActionChange.class, this.changeHandler);
        t.addHandler(this,
                     Transition.DestinationChange.class,
                     this.changeHandler);
        t.getDestination().addHandler(this,
                                      NameChangeAlert.class,
                                      this.changeHandler);
    }


    public DesignEditorTransition removeTransition(Transition t)
    {
        t.removeAllHandlers(this);

        return this.transitions.remove(t);
    }

    /**
     * Set the color of the source.
     * @param color the new color in the specified format
     */

    public void setColor(int color)
    {
        this.foreground =
            new ImageData(1, 1, 1,
                          new PaletteData(new RGB[]
                                          {
                                            GraphicsUtil.getRGBFromColor(color)
                                          }));
    }

    /**
     * Tells the graphical source to reset its label
     */

    public void updateTitle()
    {
        // do nothing
    }

    /**
     * Build the ToolTip for this source.
     *
     * @return
     */
    public static String buildToolTipText(TransitionSource source,
                                          String typeDescription)
    {
        // Use a string buffer for performance.
        StringBuilder strBuffer =
            new StringBuilder(source.getNameLabel()
                                      + " [" + typeDescription + "]");

        Iterator<Transition> transIter =
            source.getTransitions().values().iterator();

        IEllipsizer ellipsizer =
            new SWTStringUtil.SWTEllipsizer(SWTWidthComputer.DEFAULT_WIDTH,
                                            StringUtil.NO_FRONT,
                                            FontUtils.SYMBOL_FONT);

        while (transIter.hasNext()) {
            Transition transition = transIter.next();

            strBuffer.append("\n    ");

            String text = transition.toString(false, ellipsizer);
            strBuffer.append(text);
        }

        return strBuffer.toString();
    }

    protected abstract String getTypeDescription();


    public void buildToolTip()
    {
        if (this.showToolTip) {
            String toolTipText = buildToolTipText(getModel(),
                                                  getTypeDescription());

            Label toolTipLabel = new Label(" " + toolTipText + " ");

            toolTipLabel.setFont(FontUtils.SYMBOL_FONT);
            setToolTip(toolTipLabel);
        }
    }
}