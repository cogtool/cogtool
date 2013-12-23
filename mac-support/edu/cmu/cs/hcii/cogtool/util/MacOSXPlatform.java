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

package edu.cmu.cs.hcii.cogtool.util;

import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.internal.Callback;
import org.eclipse.swt.internal.carbon.HICommand;
import org.eclipse.swt.internal.carbon.OS;
import org.eclipse.swt.widgets.Display;

// Note that this file will only compile against the Macintosh
// SWT library, and needs to be segregate so no attempt is made to compile
// it against the Windows SWT. For that reason it is kept in a separate
// source hierarcy, mac-support, that is only included in the Eclipse
// build path on Macintosh.
public class MacOSXPlatform extends PlatformAdapter
{

    protected static final int kHICommandPreferences =
        ('p' << 24) + ('r' << 16) + ('e' << 8) + 'f';
    protected static final int kHICommandAbout =
        ('a' << 24) + ('b' << 16) + ('o' << 8) + 'u';
    protected static final int kHICommandServices =
        ('s' << 24) + ('e' << 16) + ('r' << 8) + 'v';
    // TODO think about localization of the following
    protected static final String fAboutActionName = "About CogTool";

    /**
     * See Apple Technical Q&A 1079 (http://developer.apple.com/qa/qa2001/qa1079.html)
     */
    @Override
    public void initPlatformMenu(Display display,
                                 final PlatformMenuActions actions)
    {
        // Callback target
        Object target = new Object() {
            int commandProc(int nextHandler, int theEvent, int userData) {
                if (OS.GetEventKind(theEvent) == OS.kEventProcessCommand) {
                    HICommand command = new HICommand();
                    OS.GetEventParameter(theEvent, OS.kEventParamDirectObject,
                            OS.typeHICommand, null, HICommand.sizeof, null, command);
                    switch (command.commandID) {
                    case kHICommandPreferences:
                        actions.doPreferences();
                        return OS.noErr;
                    case kHICommandAbout:
                        actions.doAbout();
                        return OS.noErr;
                    case OS.kHICommandQuit:
                        actions.doExitApplication();
                        return OS.noErr;
                    default:
                        break;
                    }
                }
                return OS.eventNotHandledErr;
            }
        };

        final Callback commandCallback = new Callback(target, "commandProc", 3); //$NON-NLS-1$
        int commandProc = commandCallback.getAddress();
        if (commandProc == 0) {
            commandCallback.dispose();
            return; // give up
        }

        // Install event handler for commands
        int[] mask = new int[] { OS.kEventClassCommand, OS.kEventProcessCommand };
        OS.InstallEventHandler(OS.GetApplicationEventTarget(), commandProc,
                mask.length / 2, mask, 0, null);

        // create About Eclipse menu command
        int[] outMenu = new int[1];
        short[] outIndex = new short[1];
        if (OS.GetIndMenuItemWithCommandID(0, kHICommandPreferences, 1, outMenu, outIndex) == OS.noErr
                && outMenu[0] != 0) {
            int menu = outMenu[0];

            int l = fAboutActionName.length();
            char buffer[] = new char[l];
            fAboutActionName.getChars(0, l, buffer, 0);
            int str = OS.CFStringCreateWithCharacters(OS.kCFAllocatorDefault, buffer, l);
            OS.InsertMenuItemTextWithCFString(menu, str, (short) 0, 0, kHICommandAbout);
            OS.CFRelease(str);

            // add separator between About & Preferences
            OS.InsertMenuItemTextWithCFString(menu, 0, (short) 1, OS.kMenuItemAttrSeparator, 0);

            // enable pref menu
            OS.EnableMenuCommand(menu, kHICommandPreferences);

            // disable services menu
            OS.DisableMenuCommand(menu, kHICommandServices);
        }

        // schedule disposal of callback object
        display.disposeExec(new Runnable() {
            public void run() {
                commandCallback.dispose();
            }
        });
    }

    @Override
    public void initTransferData(TransferData td) {
        td.data = new byte[][] { new byte[4] };
    }

}
