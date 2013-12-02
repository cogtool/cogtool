/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.ui;

import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.KeyEvent;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;

public class SEFrameChooserMouseState extends Draw2DMouseState
{
    protected SEFrameChooserUI ui;

    // For deciding between right-mouse vs. menu-key context menu invocation
    protected boolean isMouseEvent = false;

    public SEFrameChooserMouseState(SEFrameChooserUI frameChooserUI)
    {
        super(frameChooserUI);

        ui = frameChooserUI;
    }

    @Override
    protected boolean dealWithMousePressed(IFigure figure,
                                           int button, int x, int y, int state)
    {
        if (super.dealWithMousePressed(figure, button, x, y, state)) {
            DesignEditorFrame f = ui.structureView.getFrameAtXY(x, y);

            if (f != null) {
                ui.performAction(CogToolLID.SetStartFrame, f.getFrame());
            }

            return true;
        }

        return false;
    }

    @Override
    protected boolean dealWithMouseDoubleClicked(IFigure figure,
                                                 int button,
                                                 int x,
                                                 int y,
                                                 int state)
    {
        if (super.dealWithMouseDoubleClicked(figure, button, x, y, state)) {
            // This should work, but doubleClick is not being called.
            ui.performAction(SEFrameChooserLID.OpenScriptEditor);

            return true;
        }

        return false;
    }

    /**
     * Detects key presses in order to move widgets
     * @param ke contains details of the key press
     */
    @Override
    protected boolean dealWithKeyPressed(KeyEvent ke)
    {
        if (! super.dealWithKeyPressed(ke)) {
            return false;
        }

        // TODO: Why is this not being executed?
        switch (ke.keycode) {
            case '-': {
                if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                    ui.performAction(CogToolLID.ZoomOut);
                }
                break;
            }
            case '=': {
                if ((ke.getState() & CTRL_SHIFT_MASK) == CTRL_SHIFT_MASK) {
                    ui.performAction(CogToolLID.ZoomIn);
                }
                break;
            }
        }

        return true;
    }
}
