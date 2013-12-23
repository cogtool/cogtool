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

import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil.PromptDialog;
import edu.cmu.cs.hcii.cogtool.view.View;

public class SEDemoInteraction extends DefaultInteraction
                               implements ActionInteraction
{
    public static class TimedActionDialog extends PromptDialog
    {
        protected double durationTime;
        protected String labelResponse;
        protected Text labelText;

        public TimedActionDialog(Shell parentWin,
                                 String requestQuestion,
                                 String defaultDurationResponse,
                                 String defaultLabelResponse,
                                 boolean isThink)
        {
            super(parentWin,
                  isThink ? L10N.get("SED.NewThinkProperties",
                                     "Think Properties")
                          : L10N.get("SED.NewWaitProperties",
                                     "Wait Properties"),
                  SWT.PRIMARY_MODAL,
                  L10N.get("SED.ThinkDuration",
                           "Duration (seconds): "),
                  requestQuestion,
                  defaultDurationResponse);

            labelResponse = defaultLabelResponse;
        }

        protected void enableOKButton()
        {
            if (okButton != null) {
                boolean enabled = true;

                if (responseBox != null) {
                    enabled = responseBox.getText().length() > 0;
                }

                if (enabled && (labelText != null)) {
                    enabled = labelText.getText().length() > 0;
                }

                okButton.setEnabled(enabled);
            }
        }

        @Override
        protected void addResponseBox()
        {
            DoubleEntry timeEntry =
                new DoubleEntry(dialog, SWT.BORDER)
                {
                    @Override
                    protected void onModify()
                    {
                        enableOKButton();
                    }
                };

            timeEntry.setAllowNegative(false);
            timeEntry.setUnits("s");

            responseBox = timeEntry;
        }

        /**
         * What to do when the OK button is selected by the user.
         *
         * @author mlh
         */
        @Override
        protected void onOK()
        {
            labelResponse = labelText.getText();
            durationTime =
                ((DoubleEntry) responseBox).getDoubleValue();

            super.onOK();
        }

        public String getDurationString()
        {
            return promptResponse;
        }

        public double getDurationTime()
        {
            return durationTime;
        }

        public String getLabelResponse()
        {
            return labelResponse;
        }

        /**
         * Populate the dialog box.
         */
        @Override
        protected void addMoreFields()
        {
            super.addMoreFields();

            Label lbl = new Label(dialog, SWT.NONE);

            lbl.setText(L10N.get("SED.Label", "Label" + ": "));

            GridData lblLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);

            lbl.setLayoutData(lblLayout);

            labelText =
                new ManagedText(dialog,
                                SWT.SINGLE | SWT.BORDER,
                                Keypad.FULL_KEYPAD)
                {
                    @Override
                    protected void onModify()
                    {
                        enableOKButton();
                    }
                };

            labelText.setText(labelResponse);

            GridData labelLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            labelLayout.grabExcessHorizontalSpace = true;
            labelLayout.horizontalSpan = 3;

            labelText.setLayoutData(labelLayout);
        }
    }

    public static class TimedActionData
    {
        public double duration;
        public String labelString;

        public TimedActionData(double durationInSecs, String label)
        {
            duration = durationInSecs;
            labelString = label;
        }
    }

    protected static final String confirmDeleteStepMsg =
        L10N.get("SED.ConfirmDeleteScriptStep",
                 "Deleting this step will cause all subsequent steps to be deleted");

    protected static final String confirmDestructiveInsertScriptStepMsg =
        L10N.get("SED.ConfirmDestructiveInsertScriptStep",
                 "Inserting this step before the selected "
                      + "step will delete it and all subsequent steps.");

    protected static final String confirmDeletionTitle =
        L10N.get("SED.ConfirmDeletionTitle", "Confirm Deletion");

    protected static final String confirmInsertionTitle =
        L10N.get("SED.ConfirmInsertionTitle", "Confirm Insertion");

    protected static final String emptyThinkDurationMsg =
        L10N.get("SED.ProtestEmptyThinkDuration",
                 "A non-empty think duration is required.");

    protected static final String nonnumericThinkDurationMsg =
        L10N.get("SED.ProtestNonnumericThinkDuration",
                 "A numeric think duration is required.");

    protected static final String nonpositiveThinkDurationMsg =
        L10N.get("SED.ProtestNonpositiveThinkDuration",
                 "A think duration greater then zero is required.");

    protected static final String emptyThinkLabelMsg =
        L10N.get("SED.ProtestEmptyLabel",
                 "A non-empty think label is required.");

    protected static final String notThinkStepMsg =
        L10N.get("SED.ProtestNotThinkStep",
                 "A think step was not selected for modification.");

    protected static final String emptyDelayLabelMsg =
        L10N.get("SED.ProtestEmptyLabel",
                 "A non-empty delay label is required.");

    protected static final String invalidLookAtTargetMsg =
        L10N.get("SED.InvalidLookAtTargetMsg",
                 "A Widget is required as a target for a look-at operation.");

    protected static final String emptyDelayDurationMsg =
        L10N.get("SED.ProtestEmptyDelayDuration",
                 "A non-empty delay duration is required.");

    protected static final String nonnumericDelayDurationMsg =
        L10N.get("SED.ProtestNonnumericDelayDuration",
                 "A numeric delay duration is required.");

    protected static final String nonpositiveDelayDurationMsg =
        L10N.get("SED.ProtestNonpositiveDelayDuration",
                 "A delay duration greater then zero is required.");

    protected static final String notDelayStepMsg =
        L10N.get("SED.ProtestNotDelayStep",
                 "A system delay step was not selected for modification.");

    protected static final String noStepMsg =
        L10N.get("SED.ProtestNoStep",
                 "An inserted step was not selected for modification.");

    protected static final String notEditableStepMsg =
        L10N.get("SED.ProtestNotEditableStep",
                 "An inserted self-transition step was not selected for modification.");

    protected static final String invalidAttemptedAction =
        L10N.get("SED.InvalidAttemptedAction",
                 "The action you attempted is not in the list defined by this widget.");

    protected static final String errorTitle =
        L10N.get("SED.ErrorTitle", "Script Demo Error");

    protected static final String autoRepositionDelayTitle =
        L10N.get("SED.AutomaticRepositionDelay",
                 "Automatic Reposition of Inserted Delay");

    protected static final String autoRepositionDelayDetails =
        L10N.get("SED.AutomaticRepositionDelayDetails",
                 "You have asked for a system delay to be " +
                 "inserted after a Think step. This will " +
                 "cause the underlying cognitive engine to " +
                 "do the complete the Think step before "+
                 "starting to implement the system delay. " +
                 "Since people can think while they are " +
                 "waiting for a long system delay, this is " +
                 "usually an invalid model, but there may " +
                 "be obscure circumstances under which " +
                 "this ordering is indeed correct. \n\n" +

                 "If you select 'yes' to the question " +
                 "below, the system delay will be " +
                 "repositioned to before the Think step, " +
                 "so thinking can be done while waiting. " +
                 "(Recommended)\n" +
                 "If you select 'no', the system delay will "+
                 "remain after the Think step and thinking "+
                 "and waiting will be done in sequence. "+
                 "(Not recommended)\n" +
                 "If you select Cancel, the delay will not "+
                 "be inserted at all.\n\n" +

                 "Do you want CogTool to reposition the "+
                 "Wait for System Delay to be before the " +
                 "Think step, so thinking can be done in " +
                 "parallel with waiting?");

    protected static final String autoRepositionThinkTitle =
        L10N.get("SED.AutomaticRepositionThink",
                 "Automatic Reposition of Inserted Think");

    protected static final String autoRepositionThinkDetails =
        L10N.get("SED.AutomaticRepositionThinkDetails",
                 "You have asked for a Think step to be " +
                 "inserted after a system delay. This will " +
                 "cause the underlying cognitive engine to " +
                 "do the complete the Think step before "+
                 "starting to implement the system delay. " +
                 "Since people can think while they are " +
                 "waiting for a long system delay, this is " +
                 "usually an invalid model, but there may " +
                 "be obscure circumstances under which " +
                 "this ordering is indeed correct. \n\n" +

                 "If you select 'yes' to the question " +
                 "below, the think step will be " +
                 "repositioned to after the system delay, " +
                 "so thinking can be done while waiting. " +
                 "(Recommended)\n" +
                 "If you select 'no', the Think step will "+
                 "remain before the system delay and thinking "+
                 "and waiting will be done in sequence. "+
                 "(Not recommended)\n" +
                 "If you select Cancel, the Think step will not "+
                 "be inserted at all.\n\n" +

                 "Do you want CogTool to reposition the "+
                 "Think step to be after the " +
                 "Wait for System Delay, so thinking can be done in " +
                 "parallel with waiting?");

    protected static final String noAvailableActions =
        L10N.get("SED.NoAvailableActions", "No available actions.");

    protected static final String availableActionsAre =
        L10N.get("SED.AvailableActionsAre",
                 "The list of available actions are:");

    protected static final String useContextMenu =
        L10N.get("SED.UseContextMenu",
                 "To access these actions, use the context menu.");

    protected static final String noHandednessMsg =
        L10N.get("SE.NoHandedness",
                 "A specific hand should be selected to manipulate the mouse.");

    protected static final String noHandLocationMsg =
        L10N.get("SE.SelectMouseHand",
                 "A specific location must be selected for the mouse hand.");

    protected static final String noStartFrameMsg =
        L10N.get("SE.SelectFrameBeforeComputing",
                 "A start frame must be selected in order to compute");

    protected static final String computeTitle =
        L10N.get("SDC.Compute", "Compute");

    protected static final String recomputeTitle =
        L10N.get("PC.Recompute", "Recompute");

    protected static final String invalidDemonstrationMsg =
        L10N.get("SDC.InvalidDemonstration",
                 "The design has changed and the demonstration is invalid.");

    protected static final String scriptRegenerationRequired =
        L10N.get("SDC.NeedsRegeneration",
                 "The design has changed and the script needs to be regenerated.");

    public SEDemoInteraction(View v)
    {
        super(v);
    }

    public boolean confirmDeleteScriptStep()
    {
        String msg = confirmDeleteStepMsg;

        return (SWT.OK == WindowUtil.presentConfirmDialog(window,
                                                          confirmDeletionTitle,
                                                          msg));
    }

    public boolean confirmDestructiveInsert()
    {
        String msg = confirmDestructiveInsertScriptStepMsg;

        return (SWT.OK == WindowUtil.presentConfirmDialog(window,
                                                          confirmInsertionTitle,
                                                          msg));
    }

    public SEDemoInteraction.TimedActionData requestTimedActionData(double defaultThinkTime,
                                                  String defaultThinkLabel,
                                                  boolean isThink)
    {
        TimedActionDialog askDuration =
            new TimedActionDialog(window,
                                  "",
                                  Double.toString(defaultThinkTime),
                                  defaultThinkLabel,
                                  isThink);

        SEDemoInteraction.TimedActionData thinkResult = null;

        boolean notDone = true;

        while (notDone) {
            Object response = askDuration.open();

            if ((response != null) &&
                response.equals(WindowUtil.PromptDialog.OK))
            {
                String thinkLabel = askDuration.getLabelResponse();

                if ("".equals(thinkLabel)) {
                    askDuration = new TimedActionDialog(window,
                                                        isThink ? emptyThinkLabelMsg
                                                                : emptyDelayLabelMsg,
                                                        Double.toString(defaultThinkTime),
                                                        defaultThinkLabel,
                                                        isThink);

                    continue;
                }

                String userTimeResponse = askDuration.getDurationString();

                if ("".equals(userTimeResponse)) {
                    askDuration =
                        new TimedActionDialog(window,
                                              isThink ? emptyThinkDurationMsg
                                                      : emptyDelayDurationMsg,
                                              Double.toString(defaultThinkTime),
                                              defaultThinkLabel,
                                              isThink);
                }
                else {
                    double duration;
                    try {
                        duration = askDuration.getDurationTime();
                    }
                    catch (NumberFormatException e) {
                        askDuration =
                            new TimedActionDialog(window,
                                                  isThink ? nonnumericThinkDurationMsg
                                                          : nonnumericDelayDurationMsg,
                                                  userTimeResponse,
                                                  defaultThinkLabel,
                                                  isThink);

                        continue;
                    }

                    if (duration <= 0.0) {
                        askDuration =
                            new TimedActionDialog(window,
                                                  isThink ? nonpositiveThinkDurationMsg
                                                          : nonpositiveDelayDurationMsg,
                                                  userTimeResponse,
                                                  defaultThinkLabel,
                                                  isThink);
                    }
                    else {
                        thinkResult = new SEDemoInteraction.TimedActionData(duration, thinkLabel);
                        notDone = false;
                    }
                }
            }
            else {
                notDone = false;
            }
        }

        return thinkResult;
    } // requestNewThinkDuration

    /**
     * Inform the user that a think duration must be selected.
     */
    public void protestNotThinkStep()
    {
        reportProblem(errorTitle, notThinkStepMsg);
    }

    public Double requestNewDelayDuration(double defaultDelayTime)
    {
        WindowUtil.PromptDialog askDuration =
            new WindowUtil.PromptDialog
                                (window,
                                 L10N.get("SED.SystemWaitTitle",
                                          "Wait Duration"),
                                 SWT.PRIMARY_MODAL,
                                 L10N.get("SED.SystemWaitLabel",
                                          "Wait Duration (seconds): "),
                                 L10N.get("SED.WaitForSystemDesc",
                                          "How long will the user wait for the system?"),
                                 Double.toString(defaultDelayTime));

        Double responseTime = null;

        boolean notDone = true;

        while (notDone) {
            Object response = askDuration.open();

            if ((response != null) &&
                response.equals(WindowUtil.PromptDialog.OK))
            {
                String userTimeResponse = askDuration.getPromptResponse();

                if ("".equals(userTimeResponse)) {
                    askDuration =
                        new WindowUtil.PromptDialog
                                (window,
                                 L10N.get("SED.SystemWaitTitle",
                                          "Wait Duration"),
                                 SWT.PRIMARY_MODAL,
                                 L10N.get("SED.SystemWaitLabel",
                                          "Wait Duration (seconds): "),
                                 emptyDelayDurationMsg,
                                 Double.toString(defaultDelayTime));
                }
                else {
                    double duration = 0.0;

                    try {
                        duration = Double.parseDouble(userTimeResponse);
                    }
                    catch (NumberFormatException e) {
                        askDuration =
                            new WindowUtil.PromptDialog
                                    (window,
                                     L10N.get("SED.SystemWaitTitle",
                                              "Wait Duration"),
                                     SWT.PRIMARY_MODAL,
                                     L10N.get("SED.SystemWaitLabel",
                                              "Wait Duration (seconds): "),
                                     nonnumericDelayDurationMsg,
                                     userTimeResponse);

                        continue;
                    }

                    if (duration <= 0.0) {
                        askDuration =
                            new WindowUtil.PromptDialog
                                    (window,
                                     L10N.get("SED.SystemWaitTitle",
                                              "Wait Duration"),
                                     SWT.PRIMARY_MODAL,
                                     L10N.get("SED.SystemWaitLabel",
                                              "Wait Duration (seconds): "),
                                     nonpositiveDelayDurationMsg,
                                     userTimeResponse);
                    }
                    else {
                        responseTime = new Double(duration);
                        notDone = false;
                    }
                }
            }
            else {
                notDone = false;
            }
        }

        return responseTime;
    } // requestNewDelayDuration

    /**
     * Inform the user that a system delay duration must be selected.
     */
    public void protestNotDelayStep()
    {
        reportProblem(errorTitle, notDelayStepMsg);
    }
    /**
     * Inform the user that a valid step must be selected
     */
    public void protestNoStep()
    {
        reportProblem(errorTitle, noStepMsg);
    }

    /**
     * Inform the user that an editable step must be selected
     */
    public void protestNotEditable()
    {
        reportProblem(errorTitle, notEditableStepMsg);
    }

    public boolean protestInvalidLookAtTarget()
    {
        return reportAndRetry(errorTitle, invalidLookAtTargetMsg);
    }

    public String requestFreeFormGraffiti()
    {
        WindowUtil.PromptDialog askName =
            new WindowUtil.PromptDialog
                                (window,
                                 L10N.get("SED.GraffitiTextTitle",
                                          "Graffiti\u00AE Text"),
                                 SWT.PRIMARY_MODAL,
                                 L10N.get("SED.GraffitiTextLabel",
                                          "Graffiti\u00AE Text:"),
                                 L10N.get("SED.GraffitiTextDesc",
                                          "What text will the user enter via Graffiti\u00AE?"),
                                 ""); // default string is empty // TODO:

        Object response = askName.open();

        return
            (response != null) && response.equals(WindowUtil.PromptDialog.OK)
                    ? askName.getPromptResponse()
                    : null;
    }

    public String requestFreeFormText()
    {
        WindowUtil.PromptDialog askName =
            new WindowUtil.PromptDialog
                                (window,
                                 L10N.get("SED.KeyboardTextTitle",
                                          "Keyboard Text"),
                                 SWT.PRIMARY_MODAL,
                                 L10N.get("SED.KeyboardTextLabel",
                                          "Keyboard Text:"),
                                 L10N.get("SED.KeyboardTextDesc",
                                          "What text will the user type?"),
                                 ""); // default string is empty // TODO:

        Object response = askName.open();

        return
            (response != null) && response.equals(WindowUtil.PromptDialog.OK)
                    ? askName.getPromptResponse()
                    : null;
    }

    public void informUserOfAvailableActions(List<AAction> actions)
    {
        String msg = invalidAttemptedAction;

        if (actions.size() == 0 ) {
            msg += "\n" + noAvailableActions;
        }
        else {
            msg += "\n\n" + availableActionsAre;

            Iterator<AAction> iter = actions.iterator();

            while (iter.hasNext()) {
                String actionStr = iter.next().getLocalizedString();

                msg += "\n" + KeyDisplayUtil.convertActionToMenuText(actionStr);
            }

            msg += "\n\n" + useContextMenu;
        }

        reportProblem(errorTitle, msg);
    }

    public Boolean confirmNewDelayLocation()
    {
        int result =
            WindowUtil.presentYesNoCancelDialog(window,
                                                autoRepositionDelayTitle,
                                                autoRepositionDelayDetails);

        switch (result) {
            case SWT.YES:
                return Boolean.TRUE;

            case SWT.NO:
                return Boolean.FALSE;

            case SWT.CANCEL:
            default:
                return null;
        }
    }

    public Boolean confirmNewThinkLocation()
    {
        int result =
            WindowUtil.presentYesNoCancelDialog(window,
                                                autoRepositionThinkTitle,
                                                autoRepositionThinkDetails);

        switch (result) {
            case SWT.YES:
                return Boolean.TRUE;

            case SWT.NO:
                return Boolean.FALSE;

            case SWT.CANCEL:
            default:
                return null;
        }
    }

    /**
     * Complain that the attempt to recompute was prevented by an
     * invalid demonstration.
     *
     * @author mlh
     */
    public void reportInvalidDemonstration()
    {
        reportProblem(computeTitle, invalidDemonstrationMsg);
    }

    /**
     * Complain that the attempt to recompute was prevented by the script
     * requiring regeneration.  Ask whether to regenerate then compute
     * or to cancel.  Returns <code>true</code> if the user indicated
     * to regenerate then compute.
     *
     * @author mlh
     */
    public boolean reportRegenerationRequired()
    {
        return SWT.OK == WindowUtil.presentMessageDialog(window,
                                                         recomputeTitle,
                                                         scriptRegenerationRequired,
                                                         SWT.OK | SWT.CANCEL,
                                                         SWT.ICON_WARNING,
                                                         SWT.PRIMARY_MODAL);
    }

    public void protestNoHandedness()
    {
        reportProblem(errorTitle, noHandednessMsg);
    }

    public void protestNoHandLocation()
    {
        reportProblem(errorTitle, noHandLocationMsg);
    }

    public void protestNoStartFrame()
    {
        reportProblem(errorTitle, noStartFrameMsg);
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

    protected static final String EDIT_FRAME_TITLE =
        L10N.get("DE.EditFrameTitle", "Cannot Edit Hear Properties");

    protected static final String HEAR_STEPS_MSG =
        L10N.get("DE.HearStepsMsg",
                 "\"Hear\" steps cannot be edited in this window.");

    protected static final String EDIT_FRAME_INSTRUCTIONS =
        L10N.get("DE.EditFrameInstructions",
                 "Click Edit Frame to edit the frame's speaker output or Cancel to return to the Script Edit window.");

    /**
     * Returns true if user asks to edit the frame; false to acknowledge message
     */
    public boolean askEditFrame()
    {
        WindowUtil.CustomDialog dialog =
            new WindowUtil.CustomDialog(window,
                                        EDIT_FRAME_TITLE,
                                        SWT.PRIMARY_MODAL,
                                        SWT.DIALOG_TRIM)
            {
                @Override
                protected void addMoreFields()
                {
                    super.addMoreFields();

                    Label hearStepsMsg =
                        new Label(dialog, SWT.WRAP);
                    hearStepsMsg.setText(HEAR_STEPS_MSG);

                    GridData data = new GridData();
                    data.horizontalSpan = 4;
                    data.widthHint = 410;
                    hearStepsMsg.setLayoutData(data);

                    Label editFrameInstructions =
                        new Label(dialog, SWT.WRAP);
                    editFrameInstructions.setText(EDIT_FRAME_INSTRUCTIONS);
                    editFrameInstructions.setLayoutData(data);
                }

                @Override
                protected void addMoreButtons()
                {
                    super.addMoreButtons();

                    okButton.setText(L10N.get("DE.EditFrame",
                                                   "Edit Frame"));
                    // On Macintosh the width of the button is set explcitly
                    // for "OK" according to Apple's HIGs; but this is too
                    // narro for "Edit Frame" so unset it again.
                    ((GridData) okButton.getLayoutData()).widthHint = SWT.DEFAULT;
                }
            };

        return dialog.open() == WindowUtil.CustomDialog.OK;
    }
}
