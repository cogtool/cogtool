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

package edu.cmu.cs.hcii.cogtool.view;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.geometry.Rectangle;

import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * This class represents a horizontal or vertical sash that is used to move
 * around grid buttons in a group.
 *
 * @author rmyers
 */

public class RadioButtonSash extends ScalableInteractiveRectangle
{
    protected boolean vertical;

    public RadioButtonSash(boolean vert)
    {
        super();

        vertical = vert;

        setOutline(false);
        setOpaque(true);
        setFill(true);
        setBackgroundColor(ColorConstants.blue);
        setCursor(WindowUtil.getCursor(vertical
                                             ? WindowUtil.RESIZE_NS_CURSOR
                                             : WindowUtil.RESIZE_WE_CURSOR));
        setVisible(false);
    }

    @Override
    public void resetBounds(Rectangle r)
    {
        int x;
        int y;
        int width;
        int height;

        if (vertical) {
            x = r.x;
            width = r.width;

            height = (r.height / 10) + 2;
            y = r.y - (height / 2);
        }
        else {
            y = r.y;
            height = r.height;

            width = (r.width / 20) + 2;
            x = r.x - (width / 2);
        }

        super.resetBounds(x, y, width, height);
    }

    @Override
    public void resetBounds(int x, int y, int width, int height)
    {
        if (vertical) {
            height = (height / 10) + 2;
            y -= (height / 2);
        }
        else {
            width = (width / 20) + 2;
            x -= (width / 2);
        }

        super.resetBounds(x, y, width, height);
    }

    public boolean isVertical()
    {
        return vertical;
    }
}
