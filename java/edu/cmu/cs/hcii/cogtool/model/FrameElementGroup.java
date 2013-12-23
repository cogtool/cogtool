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

import java.util.EventObject;
import java.util.Iterator;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class FrameElementGroup extends Association<FrameElement>
{
    public static class GroupChange extends EventObject
    {
        /**
         * The different types of changes
         */
        public static final int AUXILIARY = 1;

        protected int type;

        public GroupChange(FrameElementGroup eltGroup, int changeType)
        {
            super(eltGroup);
            type = changeType;
        }

        public int getChangeType()
        {
            return type;
        }
    }

    public static final int edu_cmu_cs_hcii_cogtool_model_FrameElementGroup_version = 0;

    public static final String auxTextVAR = "auxText";

    /**
     * Additional text for a widget
     */
    protected String auxText = "";

    private static ObjectSaver.IDataSaver<FrameElementGroup> SAVER =
        new ObjectSaver.ADataSaver<FrameElementGroup>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_FrameElementGroup_version;
            }

            @Override
            public void saveData(FrameElementGroup v, ObjectSaver saver)
                throws java.io.IOException
            {
                saver.saveString(v.auxText, auxTextVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(FrameElementGroup.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<FrameElementGroup> LOADER =
        new ObjectLoader.AObjectLoader<FrameElementGroup>() {
            @Override
            public FrameElementGroup createObject()
            {
                return new FrameElementGroup();
            }

            @Override
            public void set(FrameElementGroup target,
                            String variable,
                            Object value)
            {
                if (variable != null) {
                    if (variable.equals(auxTextVAR)) {
                        target.auxText = (String) value;
                    }
                }
            }
    };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(FrameElementGroup.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_FrameElementGroup_version,
                                    LOADER);
    }
    
    public static ObjectLoader.IObjectLoader<FrameElementGroup> getImportLoader()
    {
        return LOADER;
    }

    @Override
    public void add(int index, FrameElement elt)
    {
        // For historical reasons, SimpleWidgetGroup overrides this
        // and does *not* perform the addToAssociation call.
        members.add(index, elt);
        elt.addToEltGroup(this);
    }

    @Override
    public boolean remove(FrameElement elt)
    {
        if (super.remove(elt)) {
            elt.removeFromEltGroup(this);

            return true;
        }

        return false;
    }

    @Override
    public DoubleRectangle getGroupBounds()
    {
        DoubleRectangle r = null;

        Iterator<FrameElement> iter = iterator();

        while (iter.hasNext()) {
            FrameElement elt = iter.next();

            r = unionBounds(r, elt.getEltBounds());

            if (elt instanceof IWidget) {
                if (elt instanceof AParentWidget) {
                    SimpleWidgetGroup children = ((AParentWidget) elt).getChildren();

                    if (children != null) {
                        DoubleRectangle childBds = children.getGroupBounds();

                        r = unionBounds(r, childBds);
                    }
                }
            }
        }

        return r;
    } // getGroupBounds

    /**
     * Copy the state of the fromEltGroup into this new, twinned "copy"
     */
    protected void twinState(FrameElementGroup fromEltGroup)
    {
        setName(fromEltGroup.getName());
        setAuxiliaryText(fromEltGroup.getAuxiliaryText());
        copyAttributes(fromEltGroup);
    }

    /**
     * Creates a copy of the information of the frame element group but
     * does not copy or populate the member IFrameElements.
     */
    public FrameElementGroup twin()
    {
        FrameElementGroup newEltGrp = new FrameElementGroup();

        newEltGrp.twinState(this);

        return newEltGrp;
    }

    /**
     * Get the additional text for the element.
     */

    public String getAuxiliaryText()
    {
        return auxText;
    }

    /**
     * Set the additional text for the element
     */

    public void setAuxiliaryText(String newAuxText)
    {
        if (newAuxText == null) {
            throw new IllegalArgumentException("New aux text cannot be null");
        }

        if (! auxText.equals(newAuxText)) {
            auxText = newAuxText;
            raiseAlert(new FrameElementGroup.GroupChange(this, FrameElementGroup.GroupChange.AUXILIARY));
        }
    }

}
