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

package edu.cmu.cs.hcii.cogtool.model;

import org.apache.commons.lang.builder.HashCodeBuilder;

import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
 * @author alexeiser
 */
public class KeyAction extends TextAction
{
    public static final int edu_cmu_cs_hcii_cogtool_model_KeyAction_version = 0;

    protected static final String textVAR = "text";
    protected static final String isCommandVAR = "isCommand";
    protected static final String pressTypeVAR = "pressType";
    protected static final String modifiersVAR = "modifiers";

    protected String text;
    protected boolean isCommand;
    protected KeyPressType pressType = KeyPressType.Stroke;
    protected int modifiers = AAction.NONE;

    private int myHashCode;

    private static ObjectSaver.IDataSaver<KeyAction> SAVER =
        new ObjectSaver.ADataSaver<KeyAction>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_KeyAction_version;
            }

            @Override
            public void saveData(KeyAction v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveString(v.text, textVAR);
                saver.saveBoolean(v.isCommand, isCommandVAR);
                saver.saveObject(v.pressType, pressTypeVAR);
                saver.saveInt(v.modifiers, modifiersVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(KeyAction.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<KeyAction> LOADER =
        new ObjectLoader.AObjectLoader<KeyAction>() {
            @Override
            public KeyAction createObject()
            {
                return new KeyAction();
            }

            @Override
            public void set(KeyAction target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(textVAR)) {
                        target.text = (String) value;
                    }
                    else if (variable.equals(pressTypeVAR)) {
                        target.pressType = (KeyPressType) value;
                    }
                }
            }

            @Override
            public void set(KeyAction target, String variable, boolean value)
            {
                if (variable != null) {
                    if (variable.equals(isCommandVAR)) {
                        target.isCommand = value;
                    }
                }
            }

            @Override
            public void set(KeyAction target, String variable, int value)
            {
                if (variable != null) {
                    if (variable.equals(modifiersVAR)) {
                        target.modifiers = value;
                    }
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(KeyAction.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_KeyAction_version,
                                    LOADER);
    }

    // Used during load
    protected KeyAction()
    {
        super(ActionType.KeyPress);
    }

    public KeyAction(String cmd, boolean isCmd, int kbModifiers)
    {
        super(ActionType.KeyPress);

        if (cmd == null) {
            throw new IllegalArgumentException("Key command string cannot be null!");
        }

        text = cmd;
        modifiers = kbModifiers;
        isCommand = isCmd;
    }

    public KeyAction(char key, KeyPressType clk, boolean isCmd, int kbModifiers)
    {
        super(ActionType.KeyPress);

        text = String.valueOf(key);
        pressType = clk;
        modifiers = kbModifiers;
        isCommand = isCmd;
    }

    /**
     * Supports older behavior where the default for isCmd was
     * <code>true</code>.
     *
     * @param key
     * @param clk
     * @param isCmd
     * @param kbModifiers
     */
    public KeyAction(char key, KeyPressType clk, int kbModifiers)
    {
        this(key, clk, true, kbModifiers);
    }

    /**
     * @return string (keys pressed by simulated user)
     */
    @Override
    public String getText()
    {
        return text;
    }

    @Override
    public void setText(String str)
    {
        if ((str == null) || str.equals("")) {
            throw new IllegalArgumentException("Key command string cannot be null or empty!");
        }

        text = str;
    }

    public KeyPressType getPressType()
    {
        return pressType;
    }

    public void setPressType(KeyPressType clk)
    {
        pressType = clk;
    }

    /**
     * @return keyboard modifier state bitfield
     */
    public int getModifiers()
    {
        return modifiers;
    }

    /* (non-Javadoc)
     * @see edu.cmu.cs.hcii.cogtool.model.KeyAction#setState(int)
     */
    public void setModifiers(int newState)
    {
        modifiers = newState;
    }

    /**
     * @return is the text a command
     */
    @Override
    public boolean isCommand()
    {
        return isCommand;
    }

    @Override
    public void setIsCommand(boolean isCmd)
    {
        isCommand = isCmd;
    }

    @Override
    protected void copyState(AAction fromAction)
    {
        super.copyState(fromAction);

        setPressType(((KeyAction) fromAction).getPressType());
    }

    /**
     * Create a copy of this action.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by setting it as the action of an Transition).
     *
     * @return the action copy
     * @author mlh
     */
    @Override
    public AAction duplicate()
    {
        KeyAction action =
            new KeyAction(text, isCommand, modifiers);

        action.copyState(this);

        return action;
    }

    @Override
    public String getLocalizedString()
    {
        String clickStr = pressType.toString();

        String stateStr = "";
        String separator = "";

        if ((modifiers & AAction.FUNCTION) != 0) {
            stateStr = L10N.get("BACT.FUNCTION", "FN");
            separator = "-";
        }

        if ((modifiers & AAction.COMMAND) != 0) {
            stateStr = L10N.get("BACT.COMMAND", "COMMAND");
            separator = "-";
        }

        if ((modifiers & AAction.CTRL) != 0) {
            stateStr += separator + L10N.get("BACT.CTRL", "CTRL");
            separator = "-";
        }

        if ((modifiers & AAction.ALT) != 0) {
            stateStr += separator + L10N.get("BACT.ALT", "ALT");
            separator = "-";
        }

        if ((modifiers & AAction.SHIFT) != 0) {
            stateStr += separator + L10N.get("BACT.SHIFT", "SHIFT");
            separator = "-";
        }

        String result = null;

        if (stateStr.equals("")) {
            result = stateStr;
        }
        else {
            result = stateStr + " ";
        }

        return clickStr + " " + result + "'" + text + "'";
    }

    @Override
    public void accept(AAction.ActionVisitor visitor)
    {
        visitor.visit(this);
    }

    // Actions must implement equals and hashCode
    protected boolean valueEquals(KeyAction other)
    {
        return super.valueEquals(other) &&
               (pressType == other.getPressType()) &&
               (isCommand == other.isCommand()) &&
               ((text == null) ? (other.getText() == null)
                                    : text.equals(other.getText())) &&
               (modifiers == other.getModifiers());
    }

    @Override
    public boolean equals(Object other)
    {
        return (other != null) &&
               (other.getClass() == KeyAction.class) &&
               valueEquals((KeyAction) other);
    }

    @Override
    public int hashCode()
    {
        if (myHashCode == 0 ) {
            // Must have a unique ODD number for each class which uses
            // hashCodeBuilder.
            // this   : 73, 31
            myHashCode =
                new HashCodeBuilder(73, 31).appendSuper(super.hashCode())
                                           .append(getText())
                                           .append(getPressType())
                                           .append(getModifiers())
                                           .toHashCode();
        }

        return myHashCode;
    }

}
