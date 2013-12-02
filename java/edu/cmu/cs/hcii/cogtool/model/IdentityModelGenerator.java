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

import java.util.ArrayList;
import java.util.List;

import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class IdentityModelGenerator implements CognitiveModelGenerator
{
    public static IdentityModelGenerator ONLY = new IdentityModelGenerator();

    public static final int edu_cmu_cs_hcii_cogtool_model_IdentityModelGenerator_version = 0;

    private static ObjectSaver.IDataSaver<IdentityModelGenerator> SAVER =
        new ObjectSaver.ADataSaver<IdentityModelGenerator>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_IdentityModelGenerator_version;
            }

            @Override
            public void saveData(IdentityModelGenerator v, ObjectSaver saver)
            {
                // Nothing to save; it's an ONLY!
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(IdentityModelGenerator.class.getName(),
                                  SAVER);
    }

    private static ObjectLoader.IObjectLoader<IdentityModelGenerator> LOADER =
        new ObjectLoader.AObjectLoader<IdentityModelGenerator>() {
            @Override
            public IdentityModelGenerator createObject()
            {
                return ONLY;
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(IdentityModelGenerator.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_IdentityModelGenerator_version,
                                    LOADER);
    }

    // Singleton!
    protected IdentityModelGenerator() { }

    /**
     * Generates and inserts the initial list of steps based on the given
     * start frame and initial state.  Any warnings are appended to the given
     * list (one String per warning).
     *
     * @param startFrame the start frame for the demonstration
     * @param initialState the initial state for the demonstration
     * @param warnings to hold any generated warnings
     * @param stepStates the list into which to insert the generated states
     * @return the model generator state that results after "generating"
     *         the last script step
     */
    public DefaultModelGeneratorState generateInitialSteps(Frame startFrame,
                                                     DefaultModelGeneratorState state,
                                                     List<String> warnings,
                                                     List<DefaultModelGeneratorState> stepStates)
    {
        return state;
    }

    /**
     * Inserts generated DefaultModelGeneratorState instances into the given list.
     * Each DefaultModelGeneratorState holds onto the AScriptStep that the generator
     * used or generated.  Any warnings are appended to the given list
     * (one String per warning).
     *
     * @param demoStep the next step demonstrated
     * @param currentState the model generator state resulting from
     *                     the previous script step
     * @param warnings to hold any generated warnings
     * @param stepStates the list onto which to append the generated states
     * @return the model generator state that results after "generating"
     *         the last script step (typically the given demonstration step)
     */
    public DefaultModelGeneratorState generateScriptSteps(AScriptStep demoStep,
                                                    DefaultModelGeneratorState state,
                                                    List<String> warnings,
                                                    List<DefaultModelGeneratorState> stepStates)
    {
        DefaultModelGeneratorState newState =
            new DefaultModelGeneratorState(state, demoStep);

        // TODO: This is *not* complete; the state determination stuff
        // currently is limited to the steps generated by SNIFACT.
        if (demoStep instanceof ActionScriptStep) {
            ActionScriptStep actionStep = (ActionScriptStep) demoStep;

            if (actionStep.getAction() instanceof MoveMouseAction) {
                newState.setLastMovedToWidget((IWidget) actionStep.getStepFocus());
            }
        }

        stepStates.add(newState);
        return newState;
    }

    /**
     * Creates a List of DefaultModelGeneratorState instances, each of which holds
     * onto the AScriptStep that the generator used or generated.  Any warnings
     * are appended to the given list (one String per warning).
     *
     * @param demoStep the next step demonstrated
     * @param currentState the model generator state resulting from
     *                     the previous script step
     * @param warnings to hold any generated warnings
     * @return a list of script step states, the last of which is
     *         the state that results after performing the given demo step
     */
    public List<DefaultModelGeneratorState> generateScriptSteps(AScriptStep demoStep,
                                                          DefaultModelGeneratorState currentState,
                                                          List<String> warnings)
    {
        List<DefaultModelGeneratorState> result = new ArrayList<DefaultModelGeneratorState>();
        result.add(new DefaultModelGeneratorState(demoStep));
        return result;
    }
}