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

package edu.cmu.cs.hcii.cogtool.model;

import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class HearScriptStep extends AScriptStep
{
    public static final int edu_cmu_cs_hcii_cogtool_model_HearScriptStep_version = 0;

    private static ObjectSaver.IDataSaver<HearScriptStep> SAVER =
        new ObjectSaver.ADataSaver<HearScriptStep>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_HearScriptStep_version;
            }

            @Override
            public void saveData(HearScriptStep v, ObjectSaver saver)
                throws java.io.IOException
            {
                // Nothing to do
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(HearScriptStep.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<HearScriptStep> LOADER =
        new ObjectLoader.AObjectLoader<HearScriptStep>() {
            @Override
            public HearScriptStep createObject()
            {
                return new HearScriptStep();
            }

            @Override
            public void evolve(HearScriptStep target)
            {
                if (target.getOwner() == target) {
                    target.initiallyGenerated = true;
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(HearScriptStep.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_HearScriptStep_version,
                                    LOADER);
    }

    protected final static String HEAR_LBL = L10N.get("HSS.Hear", "Hear");
    protected final static String FOR_LBL = L10N.get("HSS.For", "for");
    protected final static String HEAR_FOR_LBL =
        L10N.get("HSS.HearFor", "Hear for");

    protected HearScriptStep()
    {
        // For loading
    }

    // For initially generated steps
    public HearScriptStep(Frame startFrame)
    {
        super(startFrame, true);
    }

    // For generated step resulting from a transition to a frame with speaker
    public HearScriptStep(AScriptStep ownerStep)
    {
        super(ownerStep, false);

        currentFrame = ownerStep.getDestinationFrame();
    }

    public String getTextToHear()
    {
        return currentFrame.getSpeakerText();
    }

    public double getListenTimeInSecs()
    {
        return currentFrame.getListenTimeInSecs();
    }

    @Override
    public String getLocalizedString()
    {
        String listenText = getTextToHear();
        double listenTimeInSecs = getListenTimeInSecs();

        if (! listenText.equals("")) {
            if (listenTimeInSecs != Frame.NO_LISTEN_TIME) {
                return HEAR_LBL + " \"" + listenText
                                + "\" " + FOR_LBL
                                + " " + Double.toString(listenTimeInSecs)
                                + " s";
            }

            return HEAR_LBL + " \"" + listenText + "\"";
        }

        if (listenTimeInSecs != Frame.NO_LISTEN_TIME) {
            return HEAR_FOR_LBL + " " + Double.toString(listenTimeInSecs)
                                + " s";
        }

        return "Unexpected: nothing to hear?";
    }

    @Override
    public String getLocalizedFocusString()
    {
        return DeviceType.Speaker.getName();
    }

    @Override
    public AScriptStep duplicate(TaskApplication.DemoDuplicateScope duplicateScope,
                                 AScriptStep.GeneratedStepDuplicateScope ownerScope)
    {
        AScriptStep copy =
            (owner != this)
               ? new HearScriptStep(ownerScope.getOwner(owner))
               : new HearScriptStep(duplicateScope.getFrame(getCurrentFrame()));

        copyState(copy);

        return copy;
    }

    @Override
    public void accept(AScriptStep.ScriptStepVisitor visitor)
    {
        visitor.visit(this);
    }
}
