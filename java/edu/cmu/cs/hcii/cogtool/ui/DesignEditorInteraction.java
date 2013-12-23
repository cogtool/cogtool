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

package edu.cmu.cs.hcii.cogtool.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.util.IEllipsizer;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.SWTStringUtil;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.view.DesignEditorView;

public class DesignEditorInteraction extends DefaultInteraction
                                     implements ActionInteraction
{
    protected static final String needSelectionMsg =
        L10N.get("DE.NeedSelection",
                 "You must select a frame or transition.");

    protected static final String needSingleFrameSelectionMsg =
        L10N.get("DE.NeedSingleFrameSelection",
                 "You must select a single frame for renaming.");

    protected static final String cannotEditMultipleTransitions =
        L10N.get("DE.CannotEditMultipleTransitions",
                 "We cannot edit multiple transitions at the same time.");

    protected static final String confirmDeleteFramesMsg =
        L10N.get("DE.ConfirmDeleteFrames",
                 "Please confirm the deletion of the following Frame(s):");

    protected static final String confirmDeleteTransitionsMsg =
        L10N.get("DE.ConfirmDeleteTransitions",
                 "Please confirm the deletion of the selected Transition(s).");

    protected static final String confirmTitle =
        L10N.get("DE.ConfirmTitle", "Confirm Deletion");

    protected static final String dErrorTitle =
        L10N.get("DE.ErrorTitle", "Design Editor Error");

    protected static final String frameNotUniqueMsg =
        L10N.get("DE.NotUniqueFrameName",
                 "Frame names must be unique.\nWould you like to try again?");

    protected static final String frameEmptyNameMsg =
        L10N.get("DE.EmptyFrameName",
                 "Frame names must not be empty.\nWould you like to try again?");

    protected static final String inconsistentSourceMsg =
        L10N.get("DE.InconsistentSource",
                 "One cannot change a transition's source to an incompatible widget type.");

    protected static final String askForVoice =
        L10N.get("DE.AskForVoice",
                 "Voice Command:");

    protected static final String askForKeys =
        L10N.get("DE.AskForKeys",
                 "Key Sequence:");

    protected static final String askForGraffiti =
        L10N.get("DE.AskForGraffiti",
                 "Graffiti\u00AE Gestures:");

    protected static final String nonuniqueActionMsg =
        L10N.get("DE.NonuniqueAction",
                 "Cannot have two transitions using this action from this source: ");

    protected static final String unacceptableActionMsg =
        L10N.get("DE.UnacceptableAction",
                 "Unacceptable action for this source: ");

    /**
     * Initialize interaction with the given window that pop-up dialog
     * boxes are modal to.
     *
     * @param v target window for dialog box modality
     * @author mlh
     */
    public DesignEditorInteraction(DesignEditorView v)
    {
        super(v);
    }

    /**
     * Present the opportunity to (re-)specify the action for a transition.
     * If requested, complain that the action last specified isn't unique for
     * the associated transition's source.
     *
     * @param transition from which to get the initial values for the dialog box
     * @param properties to hold the initial values for the dialog box elements
     * @param useWhichParts which "device" option to present at first
     * @param deviceTypes bitset of devices for this dialog box
     * @param limitMode how to limit which "device" options are available;
     *                  use ActionProperties.UNSET to use current action's
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean determineNewAction(Transition transition,
                                      ActionProperties properties,
                                      int useWhichParts,
                                      int deviceTypes,
                                      int limitMode,
                                      String title)
    {
        ((DesignEditorView) view).getDefaultProperties(properties);

        properties.updateProperties(transition,
                                    transition.getAction(),
                                    transition.getSource());
        if (useWhichParts != ActionProperties.UNSET) {
            properties.useWhichParts = useWhichParts;
        }

        return determineNewAction(properties, deviceTypes, limitMode, title);
    }

    /**
     * Complain that no frame has been selected.
     *
     * @author mlh
     */
    public void protestNoSelection()
    {
        reportProblem(dErrorTitle, needSelectionMsg);
    }

    /**
     * Complain that too many frames are selected.
     *
     * @author mlh
     */
    public void protestMultipleFrameSelection()
    {
        reportProblem(dErrorTitle, needSingleFrameSelectionMsg);
    }

    /**
     * Complain that too many transitions are selected.
     */
    public void protestMultipleTransitionSelection()
    {
        reportProblem(dErrorTitle, cannotEditMultipleTransitions);
    }

    /**
     * Ask the user to confirm that the given frames may be deleted.
     *
     * @param frames the frames that will be deleted
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean confirmDeleteFrames(Frame[] frames)
    {
        return (SWT.OK == WindowUtil.presentConfirmItemsDialog(window,
                                                               confirmTitle,
                                                               confirmDeleteFramesMsg,
                                                               frames));
    }

    /**
     * Ask the user to confirm that the given transitions may be deleted.
     *
     * @param frames the transitions that will be deleted
     * @return true if the user indicated that the operation should proceed;
     *         false indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean confirmDeleteTransitions(Transition[] transitions)
    {
        String msg = confirmDeleteTransitionsMsg;

        return (SWT.OK == WindowUtil.presentConfirmDialog(window,
                                                          confirmTitle,
                                                          msg));
    }

    /**
     * Complain that the specified frame is not unique;
     * allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */
    public boolean protestNotUniqueFrameName()
    {
        return reportAndRetry(dErrorTitle, frameNotUniqueMsg);
    }

    /**
     * Complain that the frame name is empty; allow the user to retry.
     *
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel
     * @author mlh
     */
    public boolean protestEmptyFrameName()
    {
        return reportAndRetry(dErrorTitle, frameEmptyNameMsg);
    }

    /**
     * Complain that the reassignment of a transition's source has chosen
     * a source that does not accept the action associated with the transition.
     *
     * @author mlh
     */
    public void protestInconsistentSource()
    {
        reportProblem(dErrorTitle, inconsistentSourceMsg);
    }

    protected boolean protestEmptyText(ActionProperties properties,
                                       int deviceTypes,
                                       int limitMode,
                                       String specificComplaint)
    {
        NewActionChangeDialog newAction =
            new NewActionChangeDialog(window,
                                      deviceTypes,
                                      L10N.get("DE.ChangeActionType",
                                               "Change Action Type"));

        newAction.setProperties(properties, limitMode, specificComplaint);

        Object response = newAction.open();

        return (response != null) &&
               response.equals(WindowUtil.PromptDialog.OK);
    }

    /**
     * Complain that the given voice command is empty; interact with the
     * user to get a non-empty string, which will be stored in the given
     * ActionProperties.
     *
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean protestEmptyVoice(ActionProperties properties,
                                     int deviceTypes,
                                     int limitMode)
    {
        return protestEmptyText(properties, deviceTypes, limitMode,
                                EMPTY_VOICE_MSG);
    }

    /**
     * Complain that the given keyboard string is empty; interact with the
     * user to get a non-empty string, which will be stored in the given
     * ActionProperties.
     *
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean protestEmptyKeys(ActionProperties properties,
                                    int deviceTypes,
                                    int limitMode)
    {
        return protestEmptyText(properties, deviceTypes, limitMode,
                                EMPTY_KEYS_MSG);
    }

    /**
     * Complain that the given graffiti string is empty; interact with the
     * user to get a non-empty string, which will be stored in the given
     * ActionProperties.
     *
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean protestEmptyGraffiti(ActionProperties properties,
                                        int deviceTypes,
                                        int limitMode)
    {
        return protestEmptyText(properties, deviceTypes, limitMode,
                                EMPTY_GRAFFITI_MSG);
    }

    /**
     * Utility to pop a dialog box that prompts for a string and allows the
     * user to cancel
     *
     * @param dialog the box to pop
     * @return the string specified by the user or <code>null</code>
     *         if the user chose to cancel the operation
     */
    protected String promptForStringOrCancel(WindowUtil.PromptDialog dialog)
    {
        // Pop the dialog
        Object result = dialog.open();

        // Check for positive response; if ok, return the user's response
        if ((result != null) && result.equals(WindowUtil.PromptDialog.OK)) {
            return dialog.getPromptResponse();
        }

        // If the user canceled then return an indication of the cancel
        return null;
    }

    /**
     * Complain that the given action is not unique for the specified
     * transition source (a transition source may have only one transition
     * from it for each action); allow the user to retry if specified.
     *
     * @param action the action that is not unique for the given source
     * @param source the transition source that already has a transition
     *               using the given action
     * @param allowRetry whether to allow the user to retry the operation;
     *                   if false, simply report the error
     * @return true if and only if the user wishes to retry; false
     *         indicates a desire to cancel (thus, if allowRetry is false,
     *         false is returned)
     * @author mlh
     */
    public boolean protestNotUniqueAction(AAction action,
                                          TransitionSource source,
                                          boolean allowRetry)
    {
        String actionStr =
            KeyDisplayUtil.convertActionToMenuText(action.getLocalizedString());

        if (allowRetry) {
            return reportAndRetry(dErrorTitle,
                                  nonuniqueActionMsg
                                         + actionStr
                                         + " on "
                                         + source.getName());
        }

        reportProblem(dErrorTitle,
                      nonuniqueActionMsg + actionStr
                                         + " on "
                                         + source.getName());

        return false;
    }

    /**
     * Complain that the given action is not appropriate for the specified
     * transition source.  Transition sources may restrict the actions
     * that a end-user may perform.
     *
     * @param action the action that is not appropriate for the given source
     * @param source the transition source that does not accept the action
     * @author mlh
     */
    public void protestUnacceptableAction(AAction action,
                                          TransitionSource source)
    {
        String actionStr =
            KeyDisplayUtil.convertActionToMenuText(action.getLocalizedString());

        reportProblem(dErrorTitle,
                      unacceptableActionMsg + actionStr
                                            + " on "
                                            + source.getName());
    }

    /**
     * Present the opportunity to (re-)specify the action for a transition.
     *
     * @param props the initial values for the dialog box elements
     * @param deviceTypes bitset of devices for this dialog box
     * @param limitMode how to limit which "device" options are available
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean determineNewAction(ActionProperties props,
                                      int deviceTypes,
                                      int limitMode,
                                      String title)
    {
        return determineNewAction(props, deviceTypes, limitMode,
                                  null, null, null, title);
    }

    /**
     * Present the opportunity to (re-)specify the action for a transition.
     * If requested, complain that the action last specified isn't unique for
     * the associated transition's source.
     *
     * @param props the initial values for the dialog box elements
     * @param deviceTypes bitset of devices for this dialog box
     * @param limitMode how to limit which "device" options are available
     * @param action the action that is nonunique; if not null, complain that a
     *               conflict exists
     * @param source the transition source that already has a transition
     *               for the conflicting action
     * @param actionTransition the existing transition from the given source
     *                         whose action is being modified; if creating
     *                         a new transition, use <code>null</code>
     * @return true if and only if the the user specified a new action;
     *         false if the user indicates a desire to cancel the operation
     * @author mlh
     */
    public boolean determineNewAction(ActionProperties props,
                                      int deviceTypes,
                                      int limitMode,
                                      AAction action,
                                      TransitionSource source,
                                      Transition actionTransition,
                                      String title)
    {
        NewActionChangeDialog newAction =
            new NewActionChangeDialog(window, deviceTypes, title);

        newAction.setProperties(props, limitMode,
                                action, source, actionTransition);

        Object response = newAction.open();

        return (response != null) &&
               response.equals(WindowUtil.PromptDialog.OK);
    }

    public String askForImageDir()
    {
        DirectoryDialog dialog = new DirectoryDialog(window);

        dialog.setText(L10N.get("DE.SelectImages",
                                "Select a directory of images"));
        // XXX: Is there a way to get the following formatted decently? To
        //      see the end of the message you have to make the dialog bigger.
        //      Note that embedded linefeeds do *not* work: the second line
        //      gets truncated to just its top couple of pixels.
        dialog.setMessage(L10N.get("DE.DirectoryOfImages",
                                   "Select a directory with a set of images (.JPG, .PNG or .GIF) to import."));

        return dialog.open();
    }

    public void setTransitionStatusMessage(Transition newTransition)
    {
//        IEllipsizer ellipsizer =
//            new SWTStringUtil.SWTEllipsizer(150,
//                                            StringUtil.NO_FRONT,
//                                            SWTStringUtil.DEFAULT_FONT);
//
//        setStatusMessage(newTransition.toString(true, ellipsizer));
        setStatusMessage("");
    }
}
