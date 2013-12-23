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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.AlertHandler;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * This class is responsible for dealing with all of the
 * Widgets that need to get displayed.
 *
 * @author alexeiser
 */
public class FrameEditorUIModel extends DefaultUIModel
{
    /**
     *  Keep the model of the frame around.
     */
    protected Frame frame;

    /**
     * The design model, needed to hold the backtrace of names
     */
    protected Design design;

    /**
     * The internal Frame UI MOdel.
     * The frame's ui is separate from the frame editor's ui model.
     */
    protected FrameUIModel frameUI;
    protected AlertHandler widgetChangeHandler;

    protected DoubleEntry listenTimeInSecs = null;
    protected Text speakerText = null;

    protected Map<FrameElementGroup, FrameEltGroupHalo> groupFigs =
        new HashMap<FrameElementGroup, FrameEltGroupHalo>();

    /**
     * Create an interactive FrameEditor View. Creates a window.
     */
    public FrameEditorUIModel(Frame modelFrame,
                              Design modelDesign,
                              Project modelProject,
                              double initialZoom,
                              AlertHandler widgetChgHandler)
    {
        super(modelProject);

        // Fail hard on if the model and undo manager are not present.
        if (modelFrame == null) {
            throw new IllegalArgumentException
                ("Cannot create a FrameEditorUIModel with a null Frame");
        }

        // Store model for reference
        frame = modelFrame;
        design = modelDesign;

        frame.addHandler(this,
                              Frame.FrameEltGrpChange.class,
                              new AlertHandler()
                              {

                                  public void handleAlert(EventObject alert)
                                  {
                                      Frame.FrameEltGrpChange chg =
                                          (Frame.FrameEltGrpChange) alert;

                                      if (chg.action == Frame.FrameEltGrpChange.ELEMENT_DELETE)
                                      {
                                          FrameEltGroupHalo halo =
                                              groupFigs.remove(chg.getChangeElement());

                                          if (halo != null) {
                                              halo.dispose();
                                          }
                                      }
                                  }
                              });

        frame.addHandler(this,
                              Frame.SpeakerChange.class,
                              new AlertHandler()
                              {

                                  public void handleAlert(EventObject alert)
                                  {
                                      if (listenTimeInSecs != null) {
                                          setListenTime();
                                      }

                                      if (speakerText != null) {
                                          String text = frame.getSpeakerText();

                                          speakerText.setText(text);
                                          speakerText.setToolTipText(text);
                                      }
                                  }
                              });

        // Create the frame UI model, showing tool tips, using the initial zoom
        frameUI = new FrameUIModel(frame,
                                        true,  // use tool tips
                                        WindowUtil.SELECT_CURSOR,
                                        initialZoom,
                                        true,  // load lazily
                                        null); // no attribute overrides

        widgetChangeHandler = widgetChgHandler;

        frameUI.addWidgetChangeHandlerToAll(widgetChangeHandler);
    } // FrameEditorUIModel (main ctor)

    public Frame getFrame()
    {
        return frame;
    }

    public Design getDesign()
    {
        return design;
    }

    public FrameUIModel getFrameUI()
    {
        return frameUI;
    }

    public static final String NO_LISTEN_TIME_LABEL =
        L10N.get("FEUIM.NoListenTimeLabel", "");

    protected static final Color ENABLED_TEXT_COLOR =
        TextWithEnableFix.enabledColor;

    protected static final Color DISABLED_TEXT_COLOR =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_DARK_GRAY);

    protected void setListenTime()
    {
        double frameListenTime = frame.getListenTimeInSecs();

        if (frameListenTime == Frame.NO_LISTEN_TIME) {
            listenTimeInSecs.setText(NO_LISTEN_TIME_LABEL);
            listenTimeInSecs.setForeground(DISABLED_TEXT_COLOR);
        }
        else {
            listenTimeInSecs.setDoubleValue(frameListenTime);
            listenTimeInSecs.setForeground(ENABLED_TEXT_COLOR);
        }
    }

    public Text getSpeakerText()
    {
        return speakerText;
    }

    public DoubleEntry getListenTimeEntry()
    {
        return listenTimeInSecs;
    }

    public void setSpeaker(DoubleEntry listenTime, Text spkrText)
    {
        listenTimeInSecs = listenTime;
        speakerText = spkrText;

        setListenTime();

        String text = frame.getSpeakerText();

        speakerText.setText(text);
        speakerText.setToolTipText(text);
    }

    public FrameEltGroupHalo getGroupHalo(FrameElementGroup eltGroup)
    {
        FrameEltGroupHalo halo = groupFigs.get(eltGroup);

        if (halo == null) {
            halo = new FrameEltGroupHalo(eltGroup);

            groupFigs.put(eltGroup, halo);

            eltGroup.addHandler(this,
                                FrameElementGroup.GroupChange.class,
                                widgetChangeHandler);
            eltGroup.addHandler(this,
                                IAttributed.AttributeChange.class,
                                widgetChangeHandler);
            eltGroup.addHandler(this,
                                IAttributed.AuthorityChange.class,
                                widgetChangeHandler);
        }

        return halo;
    }

    @Override
    public void dispose()
    {
        frameUI.removeAllHandlers(this);
        frame.removeAllHandlers(this);
        design.removeAllHandlers(this);

        frameUI.dispose();

        Iterator<FrameEltGroupHalo> halos = groupFigs.values().iterator();

        while (halos.hasNext()) {
            FrameEltGroupHalo halo = halos.next();

            halo.dispose();
        }

        groupFigs.clear();

        super.dispose();
    }

    /**
     * Pass-through to FrameUI Model.
     * May want to remove these entirely and have
     * mouse state call directly to frameUIModel
     */
    public GraphicalWidget<?> widgetLocatedAtXY(int x, int y)
    {
        return (GraphicalWidget<?>)
               frameUI.getFigureAtXY(x,
                                          y,
                                          FrameUIModel.ONLY_GRAPHICAL_WIDGETS);
    }
}
