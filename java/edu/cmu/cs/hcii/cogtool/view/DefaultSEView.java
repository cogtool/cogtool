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

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.uimodel.DefaultSEUIModel;
import edu.cmu.cs.hcii.cogtool.uimodel.DesignEditorFrame;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.FontUtils;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil;
import edu.cmu.cs.hcii.cogtool.util.SWTContextMenuUtil;
import edu.cmu.cs.hcii.cogtool.util.TextWithEnableFix;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.SimpleMenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory.IWindowMenuData;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory.MenuType;

/**
 * Common code for SEDemoView and ScriptViewerView
 */
public class DefaultSEView extends ScriptView
{
    /**
     * A simple class whose purpose is to perform the extra layout required
     * by ScriptViewerView's editor.  Because of constructor binding time issues,
     * we cannot access "final" variables in the override to addAndLayOutFields
     * as that is called by StandardDrawingEditor's constructor.  So, we
     * use this class to hold the same values and to perform the layout using
     * the necessary instance variables from InteractionDrawingEditor.
     */
    protected static class FrameNameDevicesLayout
    {
        protected DefaultSEUIModel uiModel;
        protected SWTContextMenuUtil.MenuListener deviceL;
        protected SWTContextMenuUtil.MenuListener frameL;
        protected boolean usesSpeaker;

        public void reset(DefaultSEUIModel uim,
                          SWTContextMenuUtil.MenuListener deviceLr,
                          SWTContextMenuUtil.MenuListener frameLr,
                          int deviceTypes)
        {
            uiModel = uim;
            deviceL = deviceLr;
            frameL = frameLr;
            usesSpeaker =
                DeviceType.contains(deviceTypes,
                                    DeviceType.SPEAKER_PERSISTENCE);
        }

        public void addSpeakerDevice(DefaultSEUIModel uim,
                                     int deviceTypes,
                                     Composite bodyComposite,
                                     Composite scrollComposite,
                                     FormData scrollFormData)
        {
            uiModel = uim;
            usesSpeaker =
                DeviceType.contains(deviceTypes,
                                    DeviceType.SPEAKER_PERSISTENCE);
            addSpeaker(bodyComposite, scrollComposite, scrollFormData);
        }

        protected void addSpeaker(Composite bodyComposite,
                                  Composite scrollComposite,
                                  FormData scrollFormData)
        {
            if (usesSpeaker && (uiModel.getSpeakerText() == null)) {
                Image speakerIconImage =
                    DesignEditorFrame.getSpeakerIconImage();

                Label speakerImg = new Label(bodyComposite, SWT.NONE);

                speakerImg.setImage(speakerIconImage);
                speakerImg.setToolTipText(L10N.get("FE.SpeakerText",
                                                   "Speaker text"));

                FormData formData = new FormData();
                formData.top = scrollFormData.top;
                formData.left =
                    new FormAttachment(scrollComposite, 0, SWT.LEFT);
                speakerImg.setLayoutData(formData);

                scrollFormData.top =
                    new FormAttachment(speakerImg, 0, SWT.BOTTOM);

                Text speakerText =
                    new TextWithEnableFix(bodyComposite,
                                          SWT.BORDER | SWT.READ_ONLY);
                DoubleEntry listenTime =
                    new DoubleEntry(bodyComposite, SWT.BORDER | SWT.READ_ONLY);

                speakerText.setBackground(SPEAKER_BGK_COLOR);
                listenTime.setUnits("s");
                listenTime.setDecimalPlaces(2);
                listenTime.setToolTipText(DesignEditorFrame.LISTEN_TIME_TOOLTIP);
                listenTime.setBackground(SPEAKER_BGK_COLOR);

                formData = new FormData();
                formData.top = new FormAttachment(speakerImg, 0, SWT.CENTER);
                formData.left = new FormAttachment(speakerImg, 0, SWT.RIGHT);
                formData.right =
                    new FormAttachment(listenTime.getOuter(), 0, SWT.LEFT);
                speakerText.setLayoutData(formData);

                formData = new FormData();
                formData.top = new FormAttachment(speakerImg, 0, SWT.CENTER);
                formData.width = 100;
                formData.right =
                    new FormAttachment(scrollComposite, 0, SWT.RIGHT);
                listenTime.setLayoutData(formData);

                uiModel.setSpeaker(listenTime, speakerText);
            }
        }

        public void addInputDevice(DefaultSEUIModel uim,
                                   SWTContextMenuUtil.MenuListener deviceLr,
                                   InputDevice inputDevice,
                                   Composite bodyComposite,
                                   Composite scrollComposite,
                                   FormData scrollFormData)
        {
            uiModel = uim;
            deviceL = deviceLr;

            Composite devicesFooter = uiModel.getDevicesFooter();

            if (devicesFooter == null) {
                devicesFooter = layoutHelper.addDevicesFooter(bodyComposite,
                                                              scrollComposite,
                                                              scrollFormData);
            }

            addDeviceToFooter(inputDevice, devicesFooter);
            uiModel.updateDeviceDisplay();
        }

        protected void addDeviceToFooter(InputDevice inputDevice,
                                         Composite devicesFooter)
        {
            Label deviceLabel = new Label(devicesFooter, SWT.CENTER);

            deviceLabel.setBackground(INPUT_DEVICE_BKG_COLOR);
            deviceLabel.setFont(FontUtils.DEFAULT_FONT);

            deviceLabel.addListener(SWT.MenuDetect, deviceL);
            deviceLabel.addMouseListener(deviceL);

            uiModel.addDeviceLabel(inputDevice, deviceLabel);
        }

        public Composite addDevicesFooter(Composite bodyComposite,
                                          Composite scrollComposite,
                                          FormData scrollFormData)
        {
            Composite devicesFooter =
                new Composite(bodyComposite, SWT.BORDER);

            devicesFooter.setBackground(HEADER_FOOTER_BKG_COLOR);

            FormData formData = new FormData();
            formData.top =
                new FormAttachment(scrollComposite, 0, SWT.BOTTOM);
            formData.bottom = scrollFormData.bottom;
            formData.left =
                new FormAttachment(scrollComposite, 0, SWT.LEFT);
            formData.right =
                new FormAttachment(scrollComposite, 0, SWT.RIGHT);
            devicesFooter.setLayoutData(formData);

            uiModel.setDevicesFooter(devicesFooter);

            scrollFormData.bottom =
                new FormAttachment(devicesFooter,
                                   - DesignUtil.DEVICE_HEIGHT - 5,
                                   SWT.BOTTOM);

            return devicesFooter;
        }

        public void addFrameNameDevices(Composite bodyComposite,
                                        Composite scrollComposite,
                                        FormData scrollFormData)
        {
            Label frameNameHdr =
                new Label(bodyComposite,
                          SWT.BORDER | SWT.CENTER | SWT.SHADOW_IN);
            frameNameHdr.setFont(FontUtils.DEFAULT_FONT);
            frameNameHdr.setBackground(HEADER_FOOTER_BKG_COLOR);

            FormData formData = new FormData();
            formData.top = scrollFormData.top;
            formData.left =
                new FormAttachment(scrollComposite, 0, SWT.LEFT);
            formData.right =
                new FormAttachment(scrollComposite, 0, SWT.RIGHT);
            formData.height = 22;
            frameNameHdr.setLayoutData(formData);

            scrollFormData.top =
                new FormAttachment(frameNameHdr, 0, SWT.BOTTOM);

            uiModel.setFrameHeader(frameNameHdr);

            frameNameHdr.addListener(SWT.MenuDetect, frameL);
            frameNameHdr.addMouseListener(frameL);

            addSpeaker(bodyComposite, scrollComposite, scrollFormData);

            Collection<InputDevice> devices =
                uiModel.getCurrentFrame().getFrame().getInputDevices();

            if (devices.size() > 0) {
                Composite devicesFooter = addDevicesFooter(bodyComposite,
                                                           scrollComposite,
                                                           scrollFormData);

                Iterator<InputDevice> inputDevices = devices.iterator();

                while (inputDevices.hasNext()) {
                    addDeviceToFooter(inputDevices.next(),
                                      devicesFooter);
                }

                uiModel.updateDeviceDisplay();
            }
        }
    }

    protected static FrameNameDevicesLayout layoutHelper =
        new FrameNameDevicesLayout();

    /**
     * SWT Label for the step table
     */
    protected Label scriptStepListLabel;

    /**
     * The SIDE area as specified in interactionEditor
     * Also used to set the limits of how big you can make the properties or
     * drawing area by dragging the sash.
     */
    final static protected int HISTORY_LIST_WIDTH = 200;

    /**
     * The minimum size of the drawing area.
     * This is needed since their are properties in the drawing area that
     * should not be hidden.
     */
    final static protected int MIN_DRAWING_AREA_WIDTH = 375;

    // The size of the bottom SWT area for DEMO VIEW
    final static protected int PROPERTIES_PANE_HEIGHT = 45;

    /**
     * Return the list of needed menus
     * This is to handle MACOS which is only supposed to show a limited set of
     * specific menus.
     */
    @Override
    protected MenuType[] neededMenus()
    {
        return new MenuFactory.MenuType[]
                        { MenuFactory.MenuType.FileMenu,
                          MenuFactory.MenuType.EditMenu,
                          MenuFactory.MenuType.ScriptModifyMenu,
                          //TODO: script menu?
                          MenuFactory.MenuType.WindowMenu,
                          MenuFactory.MenuType.HelpMenu };
    }

    protected static MenuItemDefinition editFrameItem =
        new SimpleMenuItemDefinition(L10N.get("SV.EditFrame", "Edit Frame"),
                                     DesignEditorLID.EditFrame,
                                     MenuUtil.ENABLED);

    protected static Menu editFrameMenu = null;

    protected DefaultSEUIModel uiModel;
    protected SWTContextMenuUtil.MenuListener deviceL;

    public DefaultSEView(ListenerIdentifierMap listenerIDMap,
                         ILIDTransmuter transformer,
                         IWindowMenuData<Project> menuData,
                         DefaultSEUIModel uiM,
                         SWTContextMenuUtil.MenuListener deviceLr,
                         Rectangle loc,
                         int defaultWidth,
                         int defaultHeight)
    {
        super(listenerIDMap, transformer, menuData, loc,
              defaultWidth, defaultHeight);

        uiModel = uiM;
        deviceL = deviceLr;
    }

    public void addInputDevice(InputDevice inputDevice)
    {
        Composite bodyComposite = editor.getSWTBodyComposite();

        layoutHelper.addInputDevice(uiModel, deviceL, inputDevice,
                                    bodyComposite,
                                    editor.getSWTScrollComposite(),
                                    editor.getScrollFormData());

        bodyComposite.layout();
    }

    public void addSpeakerDevice(int deviceTypes)
    {
        Composite bodyComposite = editor.getSWTBodyComposite();

        layoutHelper.addSpeakerDevice(uiModel,
                                      deviceTypes,
                                      bodyComposite,
                                      editor.getSWTScrollComposite(),
                                      editor.getScrollFormData());

        bodyComposite.layout();
    }

    protected static final int STANDARD_MENU = 0;
    protected static final int SCRIPT_STEP_MENU = 1;

    /**
     * If the user clicked on open area, then show the zoom contexual menu
     */
    protected static final MenuItemDefinition[] STANDARD_ITEMS =
        new MenuItemDefinition[]
        {
            MenuFactory.ZOOM_IN,
            MenuFactory.ZOOM_OUT,
            MenuFactory.ZOOM_NORMAL,
            MenuFactory.ZOOM_FIT
        };

    public void showFrameMenu()
    {
        if (editFrameMenu == null) {
            MenuItemDefinition[] frameMenuItems = new MenuItemDefinition[1];
            frameMenuItems[0] = editFrameItem;

            editFrameMenu =
                contextMenus.createContextMenu(frameMenuItems);
        }

        editFrameMenu.setVisible(true);
    }

    /**
     * Show the standard script menu
     */
    public void showStandardMenu()
    {
        contextMenus.setContextSelection(View.SELECTION);
        contextMenus.getMenu(STANDARD_MENU).setVisible(true);
    }
}
