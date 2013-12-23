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
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.Interaction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;

public class DefaultCmd
{
    public static final String SET_ATTRIBUTE =
        L10N.get("UNDO.DC.SetAttribute", "Set Attribute");

    public static final String UNSET_ATTRIBUTE =
        L10N.get("UNDO.DC.UnsetAttribute", "Unset Attribute");

    private DefaultCmd() { } // prevent instantiation

    public static abstract class AttributeAction
    {
        protected ListenerIdentifier lid;
        protected String presentation;
        protected IAttributed target;
        protected String attrName;
        protected Interaction interaction;
        protected DemoStateManager demoStateMgr;

        public AttributeAction(ListenerIdentifier id,
                               String presentationName,
                               IAttributed attributed,
                               String attr,
                               Interaction i,
                               DemoStateManager mgr)
        {
            lid = id;
            presentation = presentationName;
            target = attributed;
            attrName = attr;
            interaction = i;
            demoStateMgr = mgr;
        }

        public abstract void redoAction();

        public abstract void undoAction();

        public IUndoableEdit getUndoRedo()
        {
            if ((demoStateMgr == null) ||
                ! WidgetAttributes.isObsoleting(attrName))
            {
                return new AUndoableEdit(lid)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return presentation;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        redoAction();
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        undoAction();
                    }
                };
            }

            DemoStateManager.IDesignUndoableEdit demoStateEdit =
                new DemoStateManager.ObsoletingEdit(lid,
                                                    demoStateMgr)
                {
                    @Override
                    public String getPresentationName()
                    {
                        return presentation;
                    }

                    @Override
                    public void redo()
                    {
                        super.redo();

                        redoAction();

                        if (target instanceof SimpleWidgetGroup) {
                            stateMgr.noteGroupEdit((SimpleWidgetGroup) target,
                                                        this);
                        }
                        else if (target instanceof IWidget) {
                            stateMgr.noteWidgetEdit((IWidget) target,
                                                         this);
                        }

                        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
                            DemoScriptCmd.regenerateDesignScripts(stateMgr.getProject(),
                                                                  stateMgr.getDesign(),
                                                                  interaction);
                        }
                    }

                    @Override
                    public void undo()
                    {
                        super.undo();

                        undoAction();

                        if (target instanceof SimpleWidgetGroup) {
                            stateMgr.noteGroupEdit((SimpleWidgetGroup) target,
                                                        this);
                        }
                        else if (target instanceof IWidget) {
                            stateMgr.noteWidgetEdit((IWidget) target,
                                                         this);
                        }

                        if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
                            DemoScriptCmd.regenerateDesignScripts(stateMgr.getProject(),
                                                                  stateMgr.getDesign(),
                                                                  interaction);
                        }
                    }
                };

            if (target instanceof SimpleWidgetGroup) {
                demoStateMgr.noteGroupEdit((SimpleWidgetGroup) target,
                                                demoStateEdit);
            }
            else if (target instanceof IWidget) {
                demoStateMgr.noteWidgetEdit((IWidget) target,
                                                 demoStateEdit);
            }

            if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
                DemoScriptCmd.regenerateDesignScripts(demoStateMgr.getProject(),
                                                      demoStateMgr.getDesign(),
                                                      interaction);
            }

            return demoStateEdit;
        }
    }

    public static boolean setAttribute(IAttributed target,
                                       DemoStateManager demoStateMgr,
                                       String attrName,
                                       final Object value,
                                       Interaction interaction,
                                       IUndoableEditSequence editSeq)
    {
        final Object oldValue = target.getAttribute(attrName);

        target.setAttribute(attrName, value);

        if (editSeq != null) {
            AttributeAction action =
                new AttributeAction(CogToolLID.SetAttribute,
                                    SET_ATTRIBUTE,
                                    target,
                                    attrName,
                                    interaction,
                                    demoStateMgr)
            {
                @Override
                public void redoAction()
                {
                    target.setAttribute(attrName, value);
                }

                @Override
                public void undoAction()
                {
                    target.setAttribute(attrName, oldValue);
                }
            };

            editSeq.addEdit(action.getUndoRedo());
        }

        return true;
    } // setAttribute

    public static boolean unsetAttribute(IAttributed target,
                                         DemoStateManager demoStateMgr,
                                         String attrName,
                                         Interaction interaction,
                                         IUndoableEditSequence editSeq)
    {
        final boolean hasAttr = target.getDefiner(attrName) == target;
        final Object oldValue = hasAttr ? target.getAttribute(attrName) : null;

        target.unsetAttribute(attrName);

        if (editSeq != null) {
            AttributeAction action =
                new AttributeAction(CogToolLID.SetAttribute,
                                    SET_ATTRIBUTE,
                                    target,
                                    attrName,
                                    interaction,
                                    demoStateMgr)
            {
                @Override
                public void redoAction()
                {
                    target.unsetAttribute(attrName);
                }

                @Override
                public void undoAction()
                {
                    if (hasAttr) {
                        target.setAttribute(attrName, oldValue);
                    }
                }
            };

            editSeq.addEdit(action.getUndoRedo());
        }

        return true;
    } // unsetAttribute
}
