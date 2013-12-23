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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * @author rmyers
 *
 */
public class Palette extends Composite
{
    protected static final Color SELECTED_BKG =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_BLACK);
    protected static Color UNSELECTED_BKG =
        WindowUtil.GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_INFO_BACKGROUND);

    public static class ButtonListener extends SelectionAdapter
    {
        protected Palette parent;

        protected void selectButton(Button b)
        {
            parent.selectButton(b);
        }

        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            selectButton((Button) evt.getSource());
        }

        public void setPalette(Palette palette)
        {
            parent = palette;
        }
    }

    // Maps assigned button data back to the Button
    protected Map<Object, Button> buttons = new HashMap<Object, Button>();

    public Palette(Composite parent, int style)
    {
        super(parent, style);
        setLayout(new FormLayout());
    }

    public Button getButton(Object data)
    {
        return buttons.get(data);
    }

    // Currently, expects images that are 20x20 pixels
    public Button createPaletteButton(Image img,
                                      Control lastButton,
                                      Composite parent,
                                      Object buttonData,
                                      ButtonListener buttonListener,
                                      String toolTipText)
    {
        buttonListener.setPalette(this);

        // On OSX, we need to use flat buttons. On the PC, toggle is sufficient
        int style = OSUtils.MACOSX ? (SWT.TOGGLE | SWT.FLAT) : SWT.TOGGLE;

        Composite buttonBackground = new Composite(parent, SWT.NONE);
        buttonBackground.setLayout(new FormLayout());

        Button b = new Button(buttonBackground, style);

        if (img != null) {
            b.setImage(img);
        }

        FormData data = new FormData();

        if (lastButton == null) {
            b.setSelection(true);   // default first button to be selected
            setData(b);
            buttonBackground.setBackground(SELECTED_BKG);
            data.top = new FormAttachment(0, 1);
            data.bottom = new FormAttachment(0, 26);
        }
        else {
            Composite lastButtonBkg = lastButton.getParent();
            data.top = new FormAttachment(lastButtonBkg, 0, SWT.BOTTOM);
            data.bottom = new FormAttachment(lastButtonBkg, 25, SWT.BOTTOM);
        }
        data.left = new FormAttachment(0, 1);
        data.right = new FormAttachment(0, 26);
        buttonBackground.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(0, 1);
        data.bottom = new FormAttachment(100, -1);
        data.left = new FormAttachment(0, 1);
        data.right = new FormAttachment(100, -1);

        b.setLayoutData(data);
        b.setData(buttonData);

        b.addSelectionListener(buttonListener);

        b.setToolTipText(toolTipText);

        buttons.put(buttonData, b);

        return b;
    }

    protected void selectButton(Button toBeSelected)
    {
        // It's a toggle button, but pushing it again shouldn't toggle
        // We use toggle so that it can be seen as "depressed".
        Button currentlySelected = (Button) getData();

        if (toBeSelected != currentlySelected) {
            if (currentlySelected != null) {
                Composite bkg = currentlySelected.getParent();

                bkg.setBackground(UNSELECTED_BKG);
                currentlySelected.setSelection(false);
            }
        }

        Composite bkg = toBeSelected.getParent();

        bkg.setBackground(SELECTED_BKG);
        toBeSelected.setSelection(true);
        setData(toBeSelected);
    }

    /**
     * Selects the button in the palette corresponding to the given data
     */
    public void selectPaletteButton(Object buttonData)
    {
        Button b = getButton(buttonData);

        if (b != null) {
            selectButton(b);
        }
    }

    /**
     * Clears the palette, returning the data of the selected button.
     */
    public Object clearButtons()
    {
        Object selectedButtonData = null;

        Iterator<Button> allButtons = buttons.values().iterator();

        while (allButtons.hasNext()) {
            Button b = allButtons.next();

            if (b.getSelection()) {
                selectedButtonData = b.getData();
            }

            b.getParent().dispose();    // buttonBackground
        }

        buttons.clear();

        return selectedButtonData;
    }
}
