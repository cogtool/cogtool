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

import edu.cmu.cs.hcii.cogtool.model.ITermSimilarity.ManualSimilarity;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTGroupParameters;
import edu.cmu.cs.hcii.cogtool.model.SNIFACTPredictionAlgo.SNIFACTParameters;
import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.PersistentDate;

public class CogToolSerialization
{
    public static CogToolSerialization ONLY = new CogToolSerialization();

    protected CogToolSerialization()
    {
        GlobalAttributed.registerSaver();
        GlobalAttributed.registerLoader();
        DoublePoint.registerSaver();
        DoublePoint.registerLoader();
        DoubleSize.registerSaver();
        DoubleSize.registerLoader();
        DoubleRectangle.registerSaver();
        DoubleRectangle.registerLoader();
        Project.registerSaver();
        Project.registerLoader();
        AUndertaking.registerSaver();
        AUndertaking.registerLoader();
        Task.registerSaver();
        Task.registerLoader();
        TaskGroup.registerSaver();
        TaskGroup.registerLoader();
        GroupNature.registerSaver();
        GroupNature.registerLoader();
        Design.registerSaver();
        Design.registerLoader();
        Frame.registerSaver();
        Frame.registerLoader();
        Transition.registerSaver();
        Transition.registerLoader();
        AAction.registerSaver();
        AAction.registerLoader();
        ActionType.registerSaver();
        ActionType.registerLoader();
        ButtonAction.registerSaver();
        ButtonAction.registerLoader();
        MouseButtonState.registerSaver();
        MouseButtonState.registerLoader();
        MousePressType.registerSaver();
        MousePressType.registerLoader();
        KeyAction.registerSaver();
        KeyAction.registerLoader();
        KeyPressType.registerSaver();
        KeyPressType.registerLoader();
        TextActionSegment.registerSaver();
        TextActionSegment.registerLoader();
        VoiceAction.registerSaver();
        VoiceAction.registerLoader();
        ATransitionSource.registerSaver();
        ATransitionSource.registerLoader();
        InputDevice.registerSaver();
        InputDevice.registerLoader();
        DeviceType.registerSaver();
        DeviceType.registerLoader();
        Association.registerSaver();
        Association.registerLoader();
        FrameElementGroup.registerSaver();
        FrameElementGroup.registerLoader();
        SimpleWidgetGroup.registerSaver();
        SimpleWidgetGroup.registerLoader();
        RadioButtonGroup.registerSaver();
        RadioButtonGroup.registerLoader();
        GridButtonGroup.registerSaver();
        GridButtonGroup.registerLoader();
        Widget.registerSaver();
        Widget.registerLoader();
        AParentWidget.registerSaver();
        AParentWidget.registerLoader();
        AMenuWidget.registerSaver();
        AMenuWidget.registerLoader();
        ContextMenu.registerSaver();
        ContextMenu.registerLoader();
        MenuItem.registerSaver();
        MenuItem.registerLoader();
        MenuHeader.registerSaver();
        MenuHeader.registerLoader();
        PullDownHeader.registerSaver();
        PullDownHeader.registerLoader();
        PullDownItem.registerSaver();
        PullDownItem.registerLoader();
        ListItem.registerSaver();
        ListItem.registerLoader();
        RadioButton.registerSaver();
        RadioButton.registerLoader();
        GridButton.registerSaver();
        GridButton.registerLoader();
        CheckBox.registerSaver();
        CheckBox.registerLoader();
        WidgetType.registerSaver();
        WidgetType.registerLoader();
        AShape.registerSaver();
        AShape.registerLoader();
        ShapeOval.registerSaver();
        ShapeOval.registerLoader();
        ShapeRectangle.registerSaver();
        ShapeRectangle.registerLoader();
        ShapeRoundedRectangle.registerSaver();
        ShapeRoundedRectangle.registerLoader();
        TaskApplication.registerSaver();
        TaskApplication.registerLoader();
        APredictionAlgo.registerSaver();
        APredictionAlgo.registerLoader();
        ACTRPredictionAlgo.registerSaver();
        ACTRPredictionAlgo.registerLoader();
        ACTR5PredictionAlgo.registerSaver();
        ACTR5PredictionAlgo.registerLoader();
        ACTR6PredictionAlgo.registerSaver();
        ACTR6PredictionAlgo.registerLoader();
        HumanDataAlgo.registerSaver();
        HumanDataAlgo.registerLoader();
        APredictionResult.registerSaver();
        APredictionResult.registerLoader();
        TimePredictionResult.registerSaver();
        TimePredictionResult.registerLoader();
        TimeDistributionPredictionResult.registerSaver();
        TimeDistributionPredictionResult.registerLoader();
        ResultStep.registerSaver();
        ResultStep.registerLoader();
        Demonstration.registerSaver();
        Demonstration.registerLoader();
        AResult.registerLoader();               // DEPRECATED; only need loader
        Script.registerSaver();
        Script.registerLoader();
        AScriptStep.registerSaver();
        AScriptStep.registerLoader();
        DriveScriptStep.registerSaver();
        DriveScriptStep.registerLoader();
        LookAtScriptStep.registerSaver();
        LookAtScriptStep.registerLoader();
        ActionScriptStep.registerSaver();
        ActionScriptStep.registerLoader();
        TransitionScriptStep.registerSaver();
        TransitionScriptStep.registerLoader();
        ThinkScriptStep.registerSaver();
        ThinkScriptStep.registerLoader();
        DelayScriptStep.registerLoader();
        DelayScriptStep.registerSaver();
        HearScriptStep.registerLoader();
        HearScriptStep.registerSaver();
        TransitionDelayScriptStep.registerLoader();
        TransitionDelayScriptStep.registerSaver();
        GraffitiAction.registerLoader();
        GraffitiAction.registerSaver();
        HomeAction.registerLoader();
        HomeAction.registerSaver();
        HomeKeyboardAction.registerLoader();    // DEPRECATED; only need loader
        HomeMouseAction.registerLoader();       // DEPRECATED; only need loader
        MoveMouseAction.registerLoader();
        MoveMouseAction.registerSaver();
        MouseOverType.registerLoader();
        MouseOverType.registerSaver();
        MouseHoverAction.registerLoader();      // DEPRECATED; only need loader
        TapScriptStep.registerLoader();         // DEPRECATED; only need loader?
        TapScriptStep.registerSaver();  // TODO:mlh need to eliminate need
        TapAction.registerLoader();
        TapAction.registerSaver();
        TapPressType.registerSaver();
        TapPressType.registerLoader();
        SkinType.registerSaver();
        SkinType.registerLoader();
        HandLocation.registerSaver();
        HandLocation.registerLoader();
        KLMCognitiveGenerator.registerSaver();
        KLMCognitiveGenerator.registerLoader();
        DefaultModelGeneratorState.registerSaver();
        DefaultModelGeneratorState.registerLoader();
        TransitionSourceType.registerSaver();   // Not saved anywhere
        TransitionSourceType.registerLoader();  // Not saved anywhere
        URLCrawlEntry.registerSaver();
        URLCrawlEntry.registerLoader();
        URLLabeledLink.registerSaver();
        URLLabeledLink.registerLoader();
        URLPositionedLink.registerSaver();
        URLPositionedLink.registerLoader();
        GLSASimilarity.registerSaver();
        GLSASimilarity.registerLoader();
        MSRSimilarity.registerSaver();
        MSRSimilarity.registerLoader();
        LSASimilarity.registerSaver();
        LSASimilarity.registerLoader();
        GensimLSASimilarity.registerSaver();
        GensimLSASimilarity.registerLoader();
        GoogleSimilarity.registerSaver();
        GoogleSimilarity.registerLoader();
        CachedGoogleSimilarity.registerSaver();
        CachedGoogleSimilarity.registerLoader();
        ManualSimilarity.registerSaver();
        ManualSimilarity.registerLoader();
        SimilarityDictionary.registerSaver();
        SimilarityDictionary.registerLoader();
        PersistentDate.registerSaver();
        PersistentDate.registerLoader();
        SNIFACTPredictionAlgo.registerSaver();
        SNIFACTPredictionAlgo.registerLoader();
        SNIFACTPredictionResult.registerSaver();
        SNIFACTPredictionResult.registerLoader();
        SNIFACTExecContext.registerSaver();
        SNIFACTExecContext.registerLoader();
        SNIFACTParameters.registerSaver();
        SNIFACTParameters.registerLoader();
        SNIFACTGroupParameters.registerSaver();
        SNIFACTGroupParameters.registerLoader();
        IdentityModelGenerator.registerSaver();
        IdentityModelGenerator.registerLoader();

        WidgetAttributes.registerAttributes();
    }
}