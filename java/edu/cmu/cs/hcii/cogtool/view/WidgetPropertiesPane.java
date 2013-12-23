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

import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.AParentWidget;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.GridButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.RadioButton;
import edu.cmu.cs.hcii.cogtool.model.RadioButtonGroup;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Widget;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.model.WidgetType;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.FrameEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.util.ComboWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.DisplayLabel;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.NullSafe;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;

public class WidgetPropertiesPane extends Composite
{
    /**
     * Handler for changes in Widget Text.
     * Handles mouse events, key events etc for a SWT widget.
     * Used to determine focus lost, and selection for generating an LID event
     *
     * NOTE: requires an LID to function.
     * @author alexeiser
     *
     */
    // TODO realy an "SWT widget"? Isn't this about CogTool widgets?
    protected static class WidgetChangeText extends View.PropertiesChangeText
    {
        public WidgetChangeText(Composite parent,
                                int style,
                                CogToolLID setLid,
                                FrameEditorView v)
        {
            super(parent, style, setLid, v);
        }

        /**
         * Handle the event:
         * If no widgets are selected, suppresses the event.
         *
         * IE: raise an LID event.
         * @param e
         */
        @Override
        public boolean confirm(int focusRule)
        {
            if (((FrameEditorView) view).selection.getWidgetSelectionCount() > 0)
            {
                return super.confirm(focusRule);
            }
            // otherwise, do nothing!

            return true;
        }
    }

    /**
     * When the associated SWT widget is manipulated, the appropriate
     * attribute is set to the desired value (provided by the subclasses).
     * TODO: subclass ComboEntryListener for any Combo that might be read/write
     */
    protected abstract class WidgetAttributeListener extends SelectionAdapter
    {
        protected String attrName;

        public WidgetAttributeListener(String name)
        {
            attrName = name;
        }

        public abstract Object getValue();

        /**
         * By default, the target (the object whose attribute is being set)
         * is the selected widget itself; subclasses can override this.
         */
        public IAttributed getTarget()
        {
            return view.selection.getSelectedIWidgets()[0];
        }

        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            Control source = (Control) evt.getSource();

            if (source.isEnabled()) {
                if (view.selection.getWidgetSelectionCount() == 1) {
                    IAttributed target = getTarget();

                    if (target == null) {
                        return;
                    }

                    Object value = getValue();

                    if ((target instanceof RadioButtonGroup) &&
                        (attrName.equals(WidgetAttributes.SELECTION_ATTR)))
                    {
                        // for radio buttons, the value is the button to be
                        // selected; we need to check if this button is
                        // selected already
                        RadioButton button = (RadioButton) getValue();
                        RadioButtonGroup rbg = (RadioButtonGroup) target;
                        if (button == rbg.getSelection()) {
                            return;
                        }
                    }

                    UI.SetAttributeParameters saprms =
                        new UI.SetAttributeParameters(target,
                                                   attrName,
                                                   value);

                    view.performAction(CogToolLID.SetAttribute, saprms, true);
                }
            }

            // It is important that we not grab the keyboard focus until *after*
            // we've done performAction, above. In some circumstances on the Mac
            // the act of grabbing the focus reverts the selection state of the
            // widget back to what it was before the event to which we're now
            // responding. Need to be careful since performAction might dispose
            // of the widget.
            if (! source.isDisposed()) {
                source.forceFocus();
            }
        }
    }

    /**
     * The purpose of this class is to receive getValue() and getTarget()
     * requests from subclasses of {@link WidgetAttributeListener} and do the
     * appropriate work.  Since the initially selected attribute applies to
     * radio buttons, pull-down lists, and list box items, which have different
     * targets for the setAttribute call, instances of this class tell the
     * listener what to do.
     * @author rmyers
     */
    protected class SelectionAttributeHelper
    {
        /**
         * In the case of list box items and pull-down lists, the list of
         * relevant widgets may include some that are separators.  However,
         * separators cannot be selected, so this array maps the index of the
         * selected item in the SWT pull-down list to the corresponding
         * actual widget (i.e. non-separator) in the group.  This map is built
         * in <code>showAttributeWidgets</code>.
         */
        protected IWidget[] indexMap;

        public Object getValue()
        {
            int index = initiallySelected.getSelectionIndex();

            if (index == 0) {
                return WidgetAttributes.NONE_SELECTED;
            }

            return indexMap[index - 1];
        }

        public IAttributed getTarget(IAttributed origTarget)
        {
            return origTarget;
        }

        public void setIndexMap(IWidget[] map)
        {
            indexMap = map;
        }
    }

    protected final SelectionAttributeHelper RADIO_HELPER =
        new SelectionAttributeHelper()
        {
            @Override
            public IAttributed getTarget(IAttributed origTarget)
            {
                if (origTarget instanceof IWidget) {
                    return ((IWidget) origTarget).getParentGroup();
                }

                return null;
            }
        };

    /**
     * This listener is built specifically for the "initially selected" combo
     * box for radio buttons, list box items, and pull-down lists.  Radio
     * buttons and list box items belong to a parent group, and in those cases
     * the group stores the selection attribute.  For pull-down lists, it is
     * the header itself that has that attribute set.  This is the reason for
     * the SelectionAttributeHelper (which is one of the three instances above).
     * One instance of this listener is saved so that setAttributeHelper can
     * be called on it when needed.
     * @author rmyers
     */
    protected class SelectionAttributeListener extends WidgetAttributeListener
    {
        protected SelectionAttributeHelper helper;

        public SelectionAttributeListener()
        {
            super(WidgetAttributes.SELECTION_ATTR);
        }

        @Override
        public Object getValue()
        {
            if (helper != null) {
                return helper.getValue();
            }

            return null;
        }

        @Override
        public IAttributed getTarget()
        {
            if (helper != null) {
                return helper.getTarget(super.getTarget());
            }

            return null;
        }

        public void setAttributeHelper(SelectionAttributeHelper h,
                                       IWidget[] map)
        {
            helper = h;
            helper.setIndexMap(map);
        }
    }

    protected final SelectionAttributeHelper LISTBOX_HELPER = RADIO_HELPER;

    protected final SelectionAttributeHelper PULLDOWN_HELPER =
        new SelectionAttributeHelper();

    protected static final String DISPLAYED_LABEL =
        L10N.get("FE.WidgetLabelCaption", "Displayed Label") + ":";
    protected static final String AUX_TEXT_LABEL =
        L10N.get("FE.WidgetAuxTextCaption", "Auxiliary Text") + ":";
    protected static final String TEXT_CONTENTS =
        L10N.get("FE.TextLabelCaption", "Contents") + ":";
    protected static final String IMAGE_PATH =
        L10N.get("FE.ImagePathCaption", "Image file name") + ":";

    protected static final String N_A = L10N.get("FE.NA", "n/a");

    public static final String SELECT_NONE =
        L10N.get("FE.SelectNone", "None Selected");

    protected static final String CLICK_SUBMENU_ACTION =
        L10N.get("FE.ClickSubmenuAction", "Click");

    protected static final String TAB_SUBMENU_ACTION =
        L10N.get("FE.TapSubmenuAction", "Tap");

    protected static final String RIGHTCLICK_CONTEXT_ACTION =
        L10N.get("FE.RightClick", "Right Click");

    protected static final String CTRLLEFTCLICK_CONTEXT_ACTION =
        L10N.get("FE.CtrlLeftClick", "Ctrl Left Click");

    protected static final String TAPHOLD_CONTEXT_ACTION =
        L10N.get("FE.TapAndHold", "Tap-and-Hold");

    protected SelectionAttributeListener selectionAttrListener =
        new SelectionAttributeListener();

    // Property pane elements
    protected Label widgetName;
    protected ManagedText widgetNameText;
    protected Label widgetTitle;
    protected ManagedText widgetTitleText;
    protected Label widgetAux;
    protected ManagedText widgetAuxText;
    protected Label widgetType;
    protected Label widgetTypeLabel;
    protected Label widgetMode;
    protected Label widgetModeValue;
    protected Button widgetRender;
    protected Label remoteLabel;
    protected Link remoteLabelFind;
    protected ManagedText remoteLabelText;
    protected Label remoteLabelOwner;
    protected Link remoteLabelOwnerName;
    protected Label remoteLabelType;
    protected Combo remoteLabelTypeCombo;

    protected static final WidgetType[] widgetTypeChoices = new WidgetType[4];
    static {
        widgetTypeChoices[0] = WidgetType.Text;
        widgetTypeChoices[1] = WidgetType.Link;
        widgetTypeChoices[2] = WidgetType.Check;
        widgetTypeChoices[3] = WidgetType.Noninteractive;
    }

    protected FormData noRemoteLabelAlign;
    protected FormData hasRemoteLabelAlign;
    protected FormData isRemoteLabelAlign;

    protected Label imagePath;
    protected Text imagePathText;

    // for buttons
    protected Button isToggleable;
    protected Button isButtonSelected;

    // for checkboxes
    protected Button isInitiallySelected;

    // for text areas
    //protected Button isMultiLine;

    // for list boxes, radio buttons, and pull-down lists
    protected Combo initiallySelected;

    // for list boxes
    protected Label selectLabel;
    protected Label visibleLabel;
    protected Combo firstVisible;
    protected Label numVisibleLabel;
    protected Spinner numVisible;

    // for menu items, pull-down items, list box items
    protected Button isSeparator;

    // for menus and context menus
    protected Label submenuActionLabel;
    protected Combo submenuAction;
    protected Integer[] submenuActions;
    protected Label submenuDelayLabel;
    protected Combo submenuDelay;

    // for context menus
    protected Label contextMenuActionLabel;
    protected Combo contextMenuAction;
    protected Integer[] contextMenuActions;

    protected FrameEditorView view;

    public WidgetPropertiesPane(Composite parent,
                                int style,
                                FrameEditorView frameView)
    {
        super(parent, style);

        view = frameView;

        setLayout(new FormLayout());

        createWidgets();
        layOutWidgets();
    }

    protected void createWidgets()
    {
        // set up widgets, set disabled for launch
        widgetName = new DisplayLabel(this, SWT.NONE);
        widgetName.setText(L10N.get("FE.WidgetNameCaption", "Widget Name")
                                     + ":");

        widgetNameText =
            new WidgetChangeText(this,
                                 SWT.SINGLE | SWT.BORDER,
                                 FrameEditorLID.ChangeNameProperty,
                                 view);

        widgetTitle = new DisplayLabel(this, SWT.NONE);
        widgetTitle.setText(DISPLAYED_LABEL);

        widgetTitleText =
            new WidgetChangeText(this,
                                 SWT.SINGLE | SWT.BORDER,
                                 FrameEditorLID.ChangeTitleProperty,
                                 view);

        widgetAux = new DisplayLabel(this, SWT.NONE);
        widgetAux.setText(AUX_TEXT_LABEL);

        widgetAuxText =
            new WidgetChangeText(this,
                                 SWT.SINGLE | SWT.BORDER,
                                 FrameEditorLID.ChangeAuxTextProperty,
                                 view);

        widgetType = new DisplayLabel(this, SWT.NONE);
        widgetType.setText(L10N.get("FE.WidgetTypeCaption", "Type") + ":");
        widgetTypeLabel = new Label(this, SWT.NONE);

        widgetMode = new DisplayLabel(this, SWT.NONE);
        widgetMode.setText(L10N.get("FE.WidgetModeCaption", "Mode") + ":");
        widgetModeValue = new Label(this, SWT.NONE);

        widgetRender = new Button(this, SWT.CHECK);
        widgetRender.setText(L10N.get("FE.WidgetRenderedCaption",
                                           "Render Widget Skin"));
        // Add listener to the rendered widget property
        widgetRender.addSelectionListener(
                view.createWidgetChgHandler(FrameEditorLID.SetRenderSkin));

        remoteLabel = new DisplayLabel(this, SWT.NONE);
        remoteLabel.setText(L10N.get("FE.RemoteLabelCaption",
                                          "Remote Label") + ":");
        remoteLabelFind = new Link(this, SWT.NONE);
        remoteLabelFind.setText(L10N.get("FE.RemoteLabelFind",
                                              "<a>Find</a>"));
        remoteLabelFind.setFont(FontUtils.getAdjustedFont(remoteLabelFind.getFont(),
                                                               8,
                                                               SWT.BOLD));

        remoteLabelText =
            new WidgetChangeText(this,
                                 SWT.SINGLE | SWT.BORDER,
                                 FrameEditorLID.SetRemoteLabelText,
                                 view);

        remoteLabelOwner = new DisplayLabel(this, SWT.NONE);
        remoteLabelOwner.setText(L10N.get("FE.RemoteLabelOwnerCaption",
                                               "Remote Label Owner") + ":");

        remoteLabelOwnerName = new Link(this, SWT.NONE);

        remoteLabelType = new DisplayLabel(this, SWT.NONE);
        remoteLabelType.setText(L10N.get("FE.RemoteLabelTypeCaption",
                                              "Remote Label Type") + ":");

        remoteLabelTypeCombo =
            new View.PerformActionCombo(this, SWT.DROP_DOWN | SWT.READ_ONLY)
            {
                @Override
                protected boolean doChangeAction()
                {
                    int selectedType = remoteLabelTypeCombo.getSelectionIndex();

                    FrameEditorUI.SetRemoteLabelTypeParms setTypeParms =
                        new FrameEditorUI.SetRemoteLabelTypeParms(widgetTypeChoices[selectedType],
                                                    view.selection.getSelectedIWidgets()[0]);

                    return view.performAction(FrameEditorLID.SetRemoteLabelType,
                                              setTypeParms,
                                              true);
                }
            };

        for (WidgetType widgetTypeChoice : widgetTypeChoices) {
            remoteLabelTypeCombo.add(widgetTypeChoice.toString());
        }

        imagePath = new DisplayLabel(this, SWT.NONE);
        imagePath.setText(IMAGE_PATH);

        imagePathText =
            new TextWithEnableFix(this, SWT.SINGLE | SWT.READ_ONLY);

        isInitiallySelected = new Button(this, SWT.CHECK);
        isInitiallySelected.setText(L10N.get("FE.InitiallySelected",
                                                  "Initially Selected"));

        WidgetAttributeListener listener =
            new WidgetAttributeListener(WidgetAttributes.IS_SELECTED_ATTR) {
                @Override
                public Object getValue()
                {
                    return new Boolean(isInitiallySelected.getSelection());
                }
            };
        isInitiallySelected.addSelectionListener(listener);

        isButtonSelected = new Button(this, SWT.CHECK);
        isButtonSelected.setText(L10N.get("FE.InitiallySelected",
                                                  "Initially Selected"));
        isButtonSelected.setEnabled(false);

        listener =
            new WidgetAttributeListener(WidgetAttributes.IS_SELECTED_ATTR) {
                @Override
                public Object getValue()
                {
                    return new Boolean(isButtonSelected.getSelection());
                }
            };
        isButtonSelected.addSelectionListener(listener);

        isToggleable = new Button(this, SWT.CHECK);
        isToggleable.setText(L10N.get("FE.Togglable", "Can be toggled"));

        listener =
            new WidgetAttributeListener(WidgetAttributes.IS_TOGGLEABLE_ATTR) {
                @Override
                public Object getValue()
                {
                    boolean sel = isToggleable.getSelection();
                    isButtonSelected.setEnabled(sel);
                    return new Boolean(sel);
                }
            };
        isToggleable.addSelectionListener(listener);

        /*this.isMultiLine = new Button(parent, SWT.CHECK);
        this.isMultiLine.setText(L10N.get("FE.MultiLine", "Multiple Lined"));

        listener = new WidgetAttributeListener(WidgetType.IS_MULTILINE_ATTR) {
            @Override
            public Object getValue() {
                return new Boolean(isMultiLine.getSelection());
            }
        };
        this.isMultiLine.addSelectionListener(listener);*/

        selectLabel = new DisplayLabel(this, SWT.NONE);
        selectLabel.setText(L10N.get("FE.ComboSelectCaption",
                                          "Initially Selected Item") + ":");

        initiallySelected =
            new ComboWithEnableFix(this, SWT.DROP_DOWN | SWT.READ_ONLY);
        initiallySelected.setVisibleItemCount(8);
        initiallySelected.addSelectionListener(selectionAttrListener);

        isSeparator = new Button(this, SWT.CHECK);
        isSeparator.setText(L10N.get("FE.Separator", "Separator"));

        listener =
            new WidgetAttributeListener(WidgetAttributes.IS_SEPARATOR_ATTR) {
                @Override
                public Object getValue()
                {
                    return (isSeparator.getSelection())
                                ? WidgetAttributes.IS_SEPARATOR
                                : WidgetAttributes.NON_SEPARATOR;
                }
        };
        isSeparator.addSelectionListener(listener);
        isSeparator.moveAbove(widgetTypeLabel);

        submenuActionLabel = new DisplayLabel(this, SWT.NONE);
        submenuActionLabel.setText(L10N.get("FE.SubmenuActionLabel",
                                                 "Submenu transition action")
                                                     + ":");

        submenuAction =
            new ComboWithEnableFix(this, SWT.DROP_DOWN | SWT.READ_ONLY);
        submenuActions =         // Maximize the possible set of values.
            new Integer[] { WidgetAttributes.HOVER_SUBMENU_ACTION, null, null };

        submenuAction.add(L10N.get("FE.HoverSubmenuAction", "Hover"));

        listener =
            new WidgetAttributeListener(WidgetAttributes.SUBMENU_ACTION_ATTR) {
                @Override
                public Object getValue()
                {
                    Object a = submenuActions[submenuAction.getSelectionIndex()];

                    submenuDelay.setEnabled(a == WidgetAttributes.HOVER_SUBMENU_ACTION);

                    return a;
                }
            };
        submenuAction.addSelectionListener(listener);

        submenuDelayLabel = new DisplayLabel(this, SWT.NONE);
        submenuDelayLabel.setText(L10N.get("FE.SubmenuDelay",
                                                "Submenu transition delay")
                                                    + ":");

        submenuDelay = // TODO: remove READ_ONLY when editing allowed (check exception in setSubmenuDelayAttr)
            new ComboWithEnableFix(this, SWT.DROP_DOWN | SWT.READ_ONLY);
        submenuDelay.add("0.0 s ("
                              + L10N.get("FE.TypicalForMac", "typical for a Mac")
                              + ")");
        submenuDelay.add("0.5 s ("
                              + L10N.get("FE.TypicalForPC", "typical for a PC")
                              + ")");
        listener =
            new WidgetAttributeListener(WidgetAttributes.SUBMENU_DELAY_ATTR) {
                @Override
                public Object getValue()
                {
                    switch (submenuDelay.getSelectionIndex()) {
                        case 0: {
                            return WidgetAttributes.NO_SUBMENU_DELAY;
                        }
                        case 1: {
                            return WidgetAttributes.PC_SUBMENU_DELAY;
                        }
                    }

                    return null; // should be the current value!
                }

                @Override
                public void widgetDefaultSelected(SelectionEvent evt)
                {
                    view.setSubmenuDelayAttr(view.selection.getSelectedIWidgets()[0],
                                             submenuDelay.getText());
                }
            };
        submenuDelay.addSelectionListener(listener);
        submenuDelay.addKeyListener(new KeyListener()
            {

                public void keyPressed(KeyEvent evt)
                {
                    /*
                     * We can allow certain types of characters here:
                     * Control characters (arrow keys, etc): Character.CONTROL
                     * Numerics: Character.DECIMAL_DIGIT_NUMBER
                     * Decimal Point: keycodes 46 and 16777262
                     *
                     * Disallow anything else
                     */
                    int characterType = Character.getType(evt.character);
                    if ((characterType == Character.CONTROL) ||
                        (characterType == Character.DECIMAL_DIGIT_NUMBER) ||
                        (evt.keyCode == 46) || (evt.keyCode == 16777262))
                    {
                        evt.doit = true;
                    }
                    else {
                        evt.doit = false;
                    }
                }

                // listen for the enter key on the number pad for OS X

                public void keyReleased(KeyEvent evt)
                {
                    if (OSUtils.MACOSX) {
                        if (evt.keyCode == SWT.KEYPAD_CR) {
                            view.setSubmenuDelayAttr(view.selection.getSelectedIWidgets()[0],
                                                     submenuDelay.getText());
                        }
                    }
                }
            });

        contextMenuActionLabel = new DisplayLabel(this, SWT.NONE);
        contextMenuActionLabel.setText(L10N.get("FE.ContextMenuAction",
                                                     "Context Menu Action")
                                                         + ":");

        // Maximize the possible set of values.
        contextMenuActions = new Integer[] { null, null, null };
        contextMenuAction =
            new ComboWithEnableFix(this, SWT.DROP_DOWN | SWT.READ_ONLY);

//TODO: this.contextMenuAction.add(L10N.get("FE.MenuKeyPress", "Menu Key Press"));

        listener =
            new WidgetAttributeListener(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR) {
                @Override
                public Object getValue()
                {
                    return contextMenuActions[contextMenuAction.getSelectionIndex()];
                }
            };
        contextMenuAction.addSelectionListener(listener);

        // Set initial selection
        //widgetShapeCombo.select(0);
        widgetTypeLabel.setText(N_A);
        widgetModeValue.setText(N_A);
    }

    protected void layOutWidgets()
    {
        // Set up the layouts

        // lay out the area using a form layout.
        // name and type are on the same line
        // label and shape are on the same line
        // since display label is longer then widget name, align text boxes to it.
        // have buttons appear in a column on the bottom.

        FormData data = new FormData();
        data.left = new FormAttachment(0, 5);
        data.top = new FormAttachment(0, 5);
        widgetName.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetName, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -10);
        widgetNameText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetNameText.getOuter(), 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        widgetTitle.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetTitle, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -10);
        widgetTitleText.setLayoutData(data);

        ManagedText lastText = widgetTitleText;

        if (CogToolPref.RESEARCH.getBoolean()) {
            data = new FormData();
            data.top = new FormAttachment(widgetTitleText.getOuter(), 5);
            data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
            widgetAux.setLayoutData(data);

            data = new FormData();
            data.top = new FormAttachment(widgetAux, 5);
            data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
            data.right = new FormAttachment(100, -10);
            widgetAuxText.setLayoutData(data);

            lastText = widgetAuxText;
        } else {
            widgetAux.setVisible(false);
            widgetAuxText.setVisible(false);
        }

        data = new FormData();
        data.top = new FormAttachment(widgetTypeLabel, 0, SWT.CENTER);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        widgetType.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(lastText.getOuter(), 5);
        data.left = new FormAttachment(widgetType, 3);
        data.right = new FormAttachment(100, -5);
        widgetTypeLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetModeValue, 0, SWT.CENTER);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        widgetMode.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetTypeLabel, 5);
        data.left = new FormAttachment(widgetMode, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, 0);
        widgetModeValue.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetModeValue, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        widgetRender.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetRender, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        remoteLabel.setLayoutData(data);

        data = new FormData();
        data.bottom = new FormAttachment(remoteLabel, -3, SWT.BOTTOM);
        data.left = new FormAttachment(remoteLabel, 5, SWT.RIGHT);
        data.right = new FormAttachment(100, -10);
        remoteLabelFind.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabel, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -10);
        remoteLabelText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(widgetRender, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        remoteLabelOwner.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabelOwner, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -10);
        remoteLabelOwnerName.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabelOwnerName, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        remoteLabelType.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(remoteLabelType, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -10);
        remoteLabelTypeCombo.setLayoutData(data);

        noRemoteLabelAlign = new FormData();
        noRemoteLabelAlign.top =
            new FormAttachment(widgetRender, 5, SWT.BOTTOM);
        noRemoteLabelAlign.left =
            new FormAttachment(widgetName, 0, SWT.LEFT);
        imagePath.setLayoutData(noRemoteLabelAlign);

        hasRemoteLabelAlign = new FormData();
        hasRemoteLabelAlign.top =
            new FormAttachment(remoteLabelText.getOuter(), 5, SWT.BOTTOM);
        hasRemoteLabelAlign.left =
            new FormAttachment(widgetName, 0, SWT.LEFT);

        isRemoteLabelAlign = new FormData();
        isRemoteLabelAlign.top =
            new FormAttachment(remoteLabelTypeCombo, 5, SWT.BOTTOM);
        isRemoteLabelAlign.left =
            new FormAttachment(widgetName, 0, SWT.LEFT);

        data = new FormData();
        data.top = new FormAttachment(imagePath, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(100, -5);
        imagePathText.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePathText, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(imagePathText, 0, SWT.RIGHT);
        isInitiallySelected.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePathText, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(imagePathText, 0, SWT.RIGHT);
        isToggleable.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(isToggleable, 5);
        data.left = new FormAttachment(widgetName, 10, SWT.LEFT);
        data.right = new FormAttachment(isToggleable, 0, SWT.RIGHT);
        isButtonSelected.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePathText, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(imagePathText, 0, SWT.RIGHT);
        selectLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(selectLabel, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right = new FormAttachment(selectLabel, 0, SWT.RIGHT);
        initiallySelected.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(lastText.getOuter(), 5);
        data.left = new FormAttachment(100, -85);
        data.right = new FormAttachment(100, 0);
        isSeparator.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(imagePathText, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        submenuActionLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(submenuActionLabel, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        submenuAction.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(submenuAction, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        submenuDelayLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(submenuDelayLabel, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        submenuDelay.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(submenuDelay, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        contextMenuActionLabel.setLayoutData(data);

        data = new FormData();
        data.top = new FormAttachment(contextMenuActionLabel, 5);
        data.left = new FormAttachment(widgetName, 0, SWT.LEFT);
        data.right =
            new FormAttachment(widgetNameText.getOuter(), 0, SWT.RIGHT);
        contextMenuAction.setLayoutData(data);
    } // layOutWidgets

    public void setSelectionListener(SelectionListener listener)
    {
        remoteLabelOwnerName.addSelectionListener(listener);
        remoteLabelFind.addSelectionListener(listener);
    }

    public String getWidgetName()
    {
        return widgetNameText.getText();
    }

    public void setWidgetName(String name)
    {
        if (name == null) {
            widgetNameText.setText(N_A);
        }
        else {
            widgetNameText.setText(name);
        }
    }

    public void requestRename()
    {
        // Check that the name text field is enabled
        if (widgetNameText.isEnabled()) {
            // if so, set the focus and selection
            widgetNameText.setFocus();
            widgetNameText.setSelection(0,
                                             widgetNameText.getCharCount());
        }
    }

    public String getWidgetTitle()
    {
        return widgetTitleText.getText();
    }

    public void setWidgetTitle(String title)
    {
        if (title == null) {
            widgetTitleText.setText(N_A);
        }
        else {
            widgetTitleText.setText(title);
        }
    }

    public String getWidgetAuxText()
    {
        return widgetAuxText.getText();
    }

    public void setWidgetAuxText(String text)
    {
        if (text == null) {
            widgetAuxText.setText(N_A);
        }
        else {
            widgetAuxText.setText(text);
        }
    }

    public void requestRetitle()
    {
        // Check that the name text field is enabled
        if (widgetTitleText.isEnabled()) {
            // if so, set the focus and selection
            widgetTitleText.setFocus();
            widgetTitleText.setSelection(0,
                                              widgetTitleText.getCharCount());
        }
    }

    public void setWidgetType(WidgetType wdgtType)
    {
        if (wdgtType != null) {
            widgetTypeLabel.setText(wdgtType.toString());
        }
        else {
            widgetTypeLabel.setText(N_A);
        }
    }

    public void setWidgetRenderable(boolean value)
    {
        widgetRender.setVisible(value);
    }

    public void setWidgetMode(Boolean isStandard)
    {
        if (isStandard != null) {
            widgetModeValue.setText(isStandard.booleanValue()
                                              ? FrameEditorView.STANDARD_LABEL
                                              : FrameEditorView.CUSTOM_LABEL);
        }
        else {
            widgetModeValue.setText(N_A);
        }
    }

    public boolean getWidgetRendered()
    {
        return widgetRender.getSelection();
    }

    public void setWidgetRendered(boolean rendered) //aaa
    {
        widgetRender.setSelection(rendered);
    }

    public String getRemoteLabelText()
    {
        return remoteLabelText.getText();
    }

    public void setRemoteLabelText(String title)
    {
        remoteLabelText.setText(title);
    }

    public boolean isInitiallySelected()
    {
        return isInitiallySelected.getVisible() &&
               isInitiallySelected.getSelection();
    }

    public void enableSingleSelect(boolean enable,
                                   boolean frameBackgroundAvailable)
    {
        widgetName.setEnabled(enable);
        widgetNameText.setEnabled(enable);
        widgetTitle.setEnabled(enable);
        widgetTitleText.setEnabled(enable);
        widgetAux.setEnabled(enable);
        widgetAuxText.setEnabled(enable);
        widgetType.setEnabled(enable);
        widgetTypeLabel.setEnabled(enable);
        widgetMode.setEnabled(enable);
        widgetModeValue.setEnabled(enable);
        widgetRender.setEnabled(enable);
        remoteLabel.setEnabled(enable);
        remoteLabelFind.setEnabled(enable);
        remoteLabelText.setEnabled(enable);
        remoteLabelOwner.setEnabled(enable);
        remoteLabelOwnerName.setEnabled(enable);
        remoteLabelType.setEnabled(enable);
        remoteLabelTypeCombo.setEnabled(enable);

        if (! enable) {
            setWidgetName(null);
            setWidgetTitle(null);
            setWidgetAuxText(null);
            setWidgetType(null);
            setWidgetMode(null);
        }
    }

    public void enableMultiSelect(boolean frameBackgroundAvailable)
    {
        widgetName.setEnabled(false);
        widgetNameText.setEnabled(false);
        widgetTitle.setEnabled(false);
        widgetTitleText.setEnabled(false);
        widgetAux.setEnabled(false);
        widgetAuxText.setEnabled(false);
        widgetType.setEnabled(false);
        widgetTypeLabel.setEnabled(false);
        widgetMode.setEnabled(false);
        widgetModeValue.setEnabled(false);
        widgetRender.setEnabled(false);
        remoteLabel.setEnabled(false);
        remoteLabelFind.setEnabled(false);
        remoteLabelText.setEnabled(false);
        remoteLabelOwner.setEnabled(false);
        remoteLabelOwnerName.setEnabled(false);
        remoteLabelType.setEnabled(false);
        remoteLabelTypeCombo.setEnabled(false);

        setWidgetName(null);
        setWidgetTitle(null);
        setWidgetAuxText(null);
        setWidgetType(null);
        setWidgetMode(null);
    }

    public void hideAttributeWidgets()
    {
        imagePath.setVisible(false);
        imagePathText.setVisible(false);
        isInitiallySelected.setVisible(false);
        isToggleable.setVisible(false);
        isButtonSelected.setVisible(false);
        selectLabel.setVisible(false);
        initiallySelected.setVisible(false);
        isSeparator.setVisible(false);
        submenuActionLabel.setVisible(false);
        submenuAction.setVisible(false);
        submenuDelayLabel.setVisible(false);
        submenuDelay.setVisible(false);
        contextMenuActionLabel.setVisible(false);
        contextMenuAction.setVisible(false);
        remoteLabel.setVisible(false);
        remoteLabelFind.setVisible(false);
        remoteLabelText.setVisible(false);
        remoteLabelOwner.setVisible(false);
        remoteLabelOwnerName.setVisible(false);
        remoteLabelType.setVisible(false);
        remoteLabelTypeCombo.setVisible(false);
    }

    /**
     * Utility to select item from Combo based on current value
     * and given value array.
     */
    protected <T> void selectCurrentValue(Combo items,
                                          T[] values,
                                          T currentValue)
    {
        int itemCount = items.getItemCount();

        for (int i = 0; i < itemCount; i++) {
            if (NullSafe.equals(currentValue, values[i])) {
                items.select(i);
                items.setData(currentValue);
                return;
            }
        }
    }

    public void showAttributeWidgets(IWidget widget)
    {
        WidgetType type = widget.getWidgetType();

        widgetTitle.setText(DISPLAYED_LABEL);

        Object pathObj = widget.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);
        if (! NullSafe.equals(WidgetAttributes.NO_IMAGE, pathObj)) {
            String imgPath = (String) pathObj;

            imagePath.setVisible(true);
            imagePathText.setVisible(true);
            imagePathText.setText(imgPath);
            imagePathText.setSelection(imgPath.length());
        }

        if ((type == WidgetType.MenuItem) ||
            (type == WidgetType.PullDownItem) ||
            (type == WidgetType.ListBoxItem))
        {
            isSeparator.setVisible(true);
            Object value =
                widget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);
            boolean isSep =
                NullSafe.equals(WidgetAttributes.IS_SEPARATOR, value);

            isSeparator.setSelection(isSep);

            widgetTitleText.setEnabled(! isSep);
            widgetAuxText.setEnabled(! isSep);
        }

        // Remote label support
        // First check if this is a remote label
        FrameElement remoteLabelOwner =
                (FrameElement)
                widget.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

        if (remoteLabelOwner != null) {
            String ownerWidgetName = remoteLabelOwner.getName();

            if ((ownerWidgetName == null) || ownerWidgetName.equals("")) {
                if (remoteLabelOwner instanceof RadioButtonGroup) {
                    ownerWidgetName = "[ anonymous radio button group ]";
                }
                else if (remoteLabelOwner instanceof GridButtonGroup) {
                    ownerWidgetName = "[ anonymous checkbox group ]";
                }
                else if (remoteLabelOwner instanceof SimpleWidgetGroup) {
                    ownerWidgetName = "[ anonymous widget group ]";
                }
                else if (remoteLabelOwner instanceof SimpleWidgetGroup) {
                    ownerWidgetName = "[ unnamed element group ]";
                }
                else {
                    ownerWidgetName = "[ unnamed widget ]";
                }
            }

            this.remoteLabelOwner.setVisible(true);
            remoteLabelOwnerName.setVisible(true);
            remoteLabelOwnerName.setText("<a>" + ownerWidgetName + "</a>");
            remoteLabelType.setVisible(true);
            remoteLabelTypeCombo.setVisible(true);
            selectCurrentValue(remoteLabelTypeCombo,
                               widgetTypeChoices,
                               widget.getWidgetType());
            imagePath.setLayoutData(isRemoteLabelAlign);
        }
        else {
            // Otherwise, check if this widget has a remote label
            remoteLabelOwner = widget.getRemoteLabelOwner();

            // Check first if it can't have one; if so,
            // reset layout to eliminate space for the remote label
            if (remoteLabelOwner == null) {
                imagePath.setLayoutData(noRemoteLabelAlign);
            }
            else {
                IWidget remoteLabelWidget =
                        (IWidget)
                        remoteLabelOwner.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);

                if (remoteLabelWidget != null) {
                    remoteLabelText.setText(remoteLabelWidget.getTitle());
                    remoteLabelFind.setVisible(true);
                }
                else {
                    // Display an empty remote label stuff to allow one to be set
                    remoteLabelText.setText("");
                    remoteLabelFind.setVisible(false);
                }

                remoteLabel.setVisible(true);
                remoteLabelText.setVisible(true);
                imagePath.setLayoutData(hasRemoteLabelAlign);
            }
        }

        if (! widget.isStandard()) {
            layout();
            return;
        }

        if ((type == WidgetType.Menu) || (type == WidgetType.ContextMenu)) {
            submenuActionLabel.setVisible(true);
            submenuAction.setVisible(true);
            submenuDelayLabel.setVisible(true);
            submenuDelay.setVisible(true);

            Integer submenuAction =
                (Integer)
                    widget.getAttribute(WidgetAttributes.SUBMENU_ACTION_ATTR);

            selectCurrentValue(this.submenuAction,
                               submenuActions,
                               submenuAction);

            Double delay =
                (Double)
                    widget.getAttribute(WidgetAttributes.SUBMENU_DELAY_ATTR);

            if (NullSafe.equals(WidgetAttributes.NO_SUBMENU_DELAY, delay)) {
                submenuDelay.select(0);
            }
            else if (NullSafe.equals(WidgetAttributes.PC_SUBMENU_DELAY, delay))
            {
                submenuDelay.select(1);
            }
            else {
                submenuDelay.setText(delay.toString() + " s");
            }

            if (type == WidgetType.ContextMenu) {
                contextMenuActionLabel.setVisible(true);
                contextMenuAction.setVisible(true);

                Integer contextAction =
                    (Integer)
                        widget.getAttribute(WidgetAttributes.CONTEXT_MENU_ACTION_ATTR);

                selectCurrentValue(contextMenuAction,
                                   contextMenuActions,
                                   contextAction);
            }
        }

        if (type == WidgetType.Check) {
            isInitiallySelected.setVisible(true);

            Boolean selected =
                (Boolean) widget.getAttribute(WidgetAttributes.IS_SELECTED_ATTR);

            isInitiallySelected.setSelection(selected.booleanValue());
        }
        else if (type == WidgetType.Button) {   // TODO: "clicked-on" for Link?
            isToggleable.setVisible(true);

            Boolean selected =
                (Boolean) widget.getAttribute(WidgetAttributes.IS_TOGGLEABLE_ATTR);

            isToggleable.setSelection(selected.booleanValue());

            isButtonSelected.setEnabled(selected.booleanValue());
            isButtonSelected.setVisible(true);
            selected =
                (Boolean) widget.getAttribute(WidgetAttributes.IS_SELECTED_ATTR);
            isButtonSelected.setSelection(selected.booleanValue());
        }
        else if (type == WidgetType.TextBox) {
//            this.isMultiLine.setVisible(true);
//
//            Boolean multi =
//                (Boolean) widget.getAttribute(WidgetType.IS_MULTILINE_ATTR);
//
//            this.isMultiLine.setSelection(multi.booleanValue());

            widgetTitle.setText(TEXT_CONTENTS);
        }
        else if (type == WidgetType.Radio) {
            selectLabel.setVisible(true);
            initiallySelected.setVisible(true);
            initiallySelected.removeAll();
            initiallySelected.add(SELECT_NONE);

            RadioButtonGroup group =
                (RadioButtonGroup) widget.getParentGroup();

            if (group != null) {
                Iterator<IWidget> widgets = group.iterator();
                IWidget[] map = new IWidget[group.size()];
                int i = 0;

                while (widgets.hasNext()) {
                    IWidget curWidget = widgets.next();
                    map[i++] = curWidget;
                    String name = curWidget.getNameLabel();
                    initiallySelected.add(name);
                }

                // This works because null isn't in the list so indexOf
                // returns -1 if SELECT_NONE is chosen
                int index = group.indexOf(group.getSelection()) + 1;

                initiallySelected.select(index);

                selectionAttrListener.setAttributeHelper(RADIO_HELPER, map);
            }
        }
        else if (type == WidgetType.PullDownList) {
            selectLabel.setVisible(true);
            initiallySelected.setVisible(true);
            initiallySelected.removeAll();
            initiallySelected.add(SELECT_NONE);
            initiallySelected.select(0);

            SimpleWidgetGroup group = ((AParentWidget) widget).getChildren();
            Iterator<IWidget> widgets = group.iterator();
            IWidget[] map = new IWidget[group.size()];
            int i = 0;

            Widget selected =
                (Widget) widget.getAttribute(WidgetAttributes.SELECTION_ATTR);

            while (widgets.hasNext()) {
                IWidget curWidget = widgets.next();

                Object isSep =
                    curWidget.getAttribute(WidgetAttributes.IS_SEPARATOR_ATTR);

                if (NullSafe.equals(WidgetAttributes.NON_SEPARATOR, isSep)) {
                    String name = curWidget.getNameLabel();
                    initiallySelected.add(name);
                    if (curWidget == selected) {
                        initiallySelected.select(i + 1);
                    }
                    map[i++] = curWidget;
                }
            }

            selectionAttrListener.setAttributeHelper(PULLDOWN_HELPER, map);
        }
        // TODO implement list box support
//        else if (type == WidgetType.ListBoxItem) {
//            this.selectLabel.setVisible(true);
//            this.initiallySelected.setVisible(true);
//            this.initiallySelected.removeAll();
//            this.initiallySelected.add(SELECT_NONE);
//            this.visibleLabel.setVisible(true);
//            this.firstVisible.setVisible(true);
//            this.firstVisible.removeAll();
//            this.numVisibleLabel.setVisible(true);
//            this.numVisible.setVisible(true);
//
//            SimpleWidgetGroup group = widget.getParentGroup();
//
//            Integer num =
//                (Integer) group.getAttribute(WidgetType.NUM_VISIBLE_ATTR);
//            this.numVisible.setSelection(num.intValue());
//
//            Iterator<IWidget> widgets = group.getAllWidgets();
//            while (widgets.hasNext()) {
//                IWidget curWidget = widgets.next();
//                String name = curWidget.getDisplayLabel();
//                this.initiallySelected.add(name);
//                this.firstVisible.add(name);
//            }
//
//            IWidget init =
//                (IWidget) group.getAttribute(WidgetType.SELECTION_ATTR);
//            int ind = group.indexOf(init) + 1;
//            this.initiallySelected.select(ind);
//            init = (IWidget) group.getAttribute(WidgetType.FIRST_VISIBLE_ATTR);
//            ind = group.indexOf(init);
//            this.firstVisible.select(ind);
//        }

        layout();
    } // showAttributeWidgets

    /**
     * Utility to set the label for the given index's item, if it exists.
     * Otherwise, adds the new item.
     */
    protected <T> void setLabel(int atIndex, String label, T value,
                                Combo items, T[] values)
    {
        int maxValidIndex = items.getItemCount() - 1;

        if (atIndex <= maxValidIndex) {
            items.setItem(atIndex, label);
        }
        else {
            items.add(label);
        }

        values[atIndex] = value;
    }

    /**
     * Determine standard widget parameter choices based on available
     * device types.
     */
    public void setParameterChoices(int deviceTypes)
    {
        int atIndex = 1;    // Hover is always the first element!

        if (DeviceType.Mouse.isMember(deviceTypes)) {
            setLabel(atIndex++,
                     CLICK_SUBMENU_ACTION,
                     WidgetAttributes.CLICK_SUBMENU_ACTION,
                     submenuAction,
                     submenuActions);
        }
        if (DeviceType.Touchscreen.isMember(deviceTypes)) {
            setLabel(atIndex++,
                     TAB_SUBMENU_ACTION,
                     WidgetAttributes.TAP_SUBMENU_ACTION,
                     submenuAction,
                     submenuActions);
        }

        Integer currentValue = (Integer) submenuAction.getData();

        if (currentValue != null) {
            selectCurrentValue(submenuAction,
                               submenuActions,
                               currentValue);
        }

        atIndex = 0;

        if (DeviceType.Mouse.isMember(deviceTypes)) {
            setLabel(atIndex++,
                     RIGHTCLICK_CONTEXT_ACTION,
                     WidgetAttributes.RIGHT_CLICK,
                     contextMenuAction,
                     contextMenuActions);

            if (DeviceType.Keyboard.isMember(deviceTypes)) {
                setLabel(atIndex++,
                         CTRLLEFTCLICK_CONTEXT_ACTION,
                         WidgetAttributes.CTRL_LEFT_CLICK,
                         contextMenuAction,
                         contextMenuActions);
            }
        }

        if (DeviceType.Touchscreen.isMember(deviceTypes)) {
            setLabel(atIndex++,
                     TAPHOLD_CONTEXT_ACTION,
                     WidgetAttributes.TAP_HOLD,
                     contextMenuAction,
                     contextMenuActions);
        }

        currentValue = (Integer) contextMenuAction.getData();

        if (currentValue != null) {
            selectCurrentValue(contextMenuAction,
                               contextMenuActions,
                               currentValue);
        }
    }
}
