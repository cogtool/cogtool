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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

/**
 * Support for creating application and pop-up windows and interacting
 * with the constructed user interface.
 * <p>
 * It is not necessary (and therefore not allowed) to create an instance
 * of <code>WindowUtil</code>.
 * <p>
 * In order to support multiple (non-pop-up) windows for an application, the
 * <code>Display</code> object's user data is used to reference-count
 * the number of windows; interaction terminates when the count reaches zero.
 */
public class WindowUtil
{
    /*
     * Constants for cursors
     */
    public static final int SELECT_CURSOR = SWT.CURSOR_ARROW;
    public static final int DRAW_CURSOR = SWT.CURSOR_CROSS;
    public static final int TEXT_CURSOR = SWT.CURSOR_IBEAM;
    public static final int LINK_CURSOR = SWT.CURSOR_HAND;
    public static final int HELP_CURSOR = SWT.CURSOR_HELP;
    public static final int NOT_ALLOWED_CURSOR = SWT.CURSOR_NO;
    public static final int BUSY_CURSOR = SWT.CURSOR_WAIT;
    public static final int RESIZE_NS_CURSOR = SWT.CURSOR_SIZENS;
    public static final int RESIZE_WE_CURSOR = SWT.CURSOR_SIZEWE;
    public static final int RESIZE_NW_SE_CURSOR = SWT.CURSOR_SIZENWSE;
    public static final int RESIZE_NE_SW_CURSOR = SWT.CURSOR_SIZENESW;
    public static final int RESIZE_ALL_CURSOR = SWT.CURSOR_SIZEALL;

    public static final int MAC_MENU_HEIGHT = 24;

    protected static final String DEFERRED_DISPOSALS_KEY =
        "edu.cmu.hcii.cogtool.deferredDisposals";

    public static Map<Integer, Cursor> cursorRegistry =
        new HashMap<Integer, Cursor>();

    public static Cursor getCursor(int cursorIndex)
    {
        Integer cursorKey = new Integer(cursorIndex);

        Cursor cachedCursor = cursorRegistry.get(cursorKey);

        if (cachedCursor == null) {
            cachedCursor = new Cursor(GLOBAL_DISPLAY, cursorIndex);

            cursorRegistry.put(cursorKey, cachedCursor);
        }

        return cachedCursor;
    }

    public static Cursor OPEN_HAND_CURSOR = null;
    public static Cursor CLOSED_HAND_CURSOR = null;

    /**
     * Returns a hand cursor.
     *
     * IMPORTANT: this cursor must be disposed when its creator is disposed,
     *            or we will have a memory link.
     *
     * @return a hew hand cursor
     */
    public static Cursor getOpenHandCursor()
    {
        if (OPEN_HAND_CURSOR == null) {
            ImageData curData =
                GraphicsUtil.getImageDataFromResource("edu/cmu/cs/hcii/cogtool/resources/open_hand.gif");

            if (curData == null) {
                return null;
            }

            OPEN_HAND_CURSOR = new Cursor(GLOBAL_DISPLAY, curData, 8, 8);
        }

        return OPEN_HAND_CURSOR;
    }

    public static Cursor getClosedHandCursor()
    {
        if (CLOSED_HAND_CURSOR == null) {
            ImageData curData =
                GraphicsUtil.getImageDataFromResource("edu/cmu/cs/hcii/cogtool/resources/closed_hand.gif");

            if (curData == null) {
                return null;
            }

            CLOSED_HAND_CURSOR = new Cursor(GLOBAL_DISPLAY, curData, 8, 8);
        }

        return CLOSED_HAND_CURSOR;
    }

    /**
     * The global SWT Display object
     */
    public static final Display GLOBAL_DISPLAY;
    static {
        Display.setAppName("CogTool");
        System.setProperty("dock:name", "CogTool");
        GLOBAL_DISPLAY = Display.getDefault();
        GLOBAL_DISPLAY.disposeExec(new Runnable() {
            public void run() {
                processDeferredDisposals();
                GLOBAL_DISPLAY.setData(DEFERRED_DISPOSALS_KEY, null);
            }
        });
    }

    public static final Cursor BUSY_CURSOR_INSTANCE = getCursor(BUSY_CURSOR);

    //  Prevent instantiation
    private WindowUtil() { }

    /**
     * Gets the Text control with the keyboard focus, or null if no Text has
     * the focus.
     *
     * @return a Text or null
     */
    public static Text getFocusedText()
    {
        Control focused = GLOBAL_DISPLAY.getFocusControl();

        if (focused instanceof Text) {
            return (Text) focused;
        }

        return null;
    }

    /**
     * Gets the Combo control with the keyboard focus, or null if no Combo has
     * the focus.
     *
     * @return a Combo or null
     */
    public static Combo getFocusedCombo()
    {
        Control focused = GLOBAL_DISPLAY.getFocusControl();

        if (focused instanceof Combo) {
            return (Combo) focused;
        }

        return null;
    }

    // Record that a new application window has been created.
    private static void incrementReferenceCount(Display display)
    {
        Integer refs = (Integer) display.getData();

        display.setData(new Integer((refs == null) ? 1
                                                   : refs.intValue() + 1));
    }

    // Record that an application window has been closed.
    // Returns true iff the decrement results in zero; at that point,
    // the user data is reset to <code>null</code>.
    // On the Mac, the application must be explicitly closed with a call to
    // Display.setData(null).  This behavior can be overridden by setting the
    // system property "edu.cmu.cs.hcii.cogtool.util.QuitAfterLastWindowClosed"
    // to a value of "true".
    private static void decrementReferenceCount(Display display)
    {
        Integer refs = (Integer) display.getData();

        if (refs != null) {
            if (refs.intValue() == 1) {
                if ((! OSUtils.MACOSX) ||
                    Boolean.getBoolean("edu.cmu.cs.hcii.cogtool.util.QuitAfterLastWindowClosed"))
                {
                    display.setData(null);
                }
                else {
                    display.setData(new Integer(0));
                }
            }
            else {
                display.setData(new Integer(refs.intValue() - 1));
            }
        }
    }

    // Attached to each application window to decrement the display's
    // reference count when the window is closed (on the SWT.Dispose event).
    private static Listener WindowClosed =
        new Listener()
        {
            public void handleEvent(Event evt)
            {
                decrementReferenceCount(evt.display);
            }
        };

    /**
     * This listener forces termination of the main interaction loop
     * by ignoring the reference count of application windows and resetting
     * the user data to null.
     *
     * @see #interact(Display display)
     */
    public static Listener ExitApplication =
        new Listener()
        {
            public void handleEvent(Event evt)
            {
                if (evt.doit) {
                    evt.display.setData(null);
                }
            }
        };

    /**
     * This listener closes the specified application window upon request.
     * <p>
     * This listener should use a window created by
     * <code>createNormalWindow</code> and attached
     * (using <code>addListener</code>) to the user interface
     * component that represents a request to close that window
     * (e.g., a <code>MenuItem</code>).
     * <p>
     * <b>Implementation note</b>: The <code>close</code> method causes a
     * <code>ShellEvent</code> to the appropriate listener on the
     * <code>Shell</code> object.  If you wish to "prevent" closing,
     * you can provide such a listener that sets <code>doit</code> false.
     * <p>
     * <b>Implementation note</b>: If the <code>close</code> succeeds,
     * or the associated application window is closed in any other way,
     * a <code>SWT.Dispose</code> event is sent to registered listeners,
     * one of which will be <code>WindowClosed</code>.
     *
     * @see #createNormalWindow
     * @see org.eclipse.swt.events.ShellEvent
     */
    public static class CloseListener implements Listener
    {
        Shell window;

        /**
         * Constructor for the event listener that closes the given
         * application window upon request.
         *
         * @param win   the application window
         * @author      mlh
         */
        public CloseListener(Shell win)
        {
            window = win;
        }

        /**
         * Handle the request to close an application window.
         *
         * @param evt   the event representing the request
         * @author      mlh
         */
        public void handleEvent(Event evt)
        {
            // Calls dispose; causes ShellEvent to be sent to:
            //     DefaultUI.closeListener
            //     AView.shell (in ctor)
            if (evt.doit) {
                window.close();
            }
        }
    }

    /**
     * Create a normal application window.
     * <p>
     * The window is created with application "trim" (i.e., close/resize
     * buttons, etc.).  The created window participates in the reference
     * counting that controls when the main interaction loop exits.
     *
     * @param display     the device that represents the display screen
     * @param windowTitle the string to be used as the window's title
     * @param bounds      the desired position with respect to the display,
     *                    and size of the window
     * @param setPosition if true, the position information in <code>bounds</code>
     *                    is used; otherwise a platform dependent default
     *                    position is used; note that the size information in
     *                    <code>bounds</code> is always used.
     * @param layout      the SWT object expressing layout rules for the
     *                    window's contents
     * @return            the SWT object representing the application window
     */
    public static Shell createNormalWindow(Display display,
                                           String windowTitle,
                                           Rectangle bounds,
                                           boolean setPosition,
                                           Layout layout)
    {
        Shell window = new Shell(display, SWT.SHELL_TRIM);

        window.setLayout(layout);
        window.setText(windowTitle);

        if (setPosition) {
            window.setBounds(bounds);
        }
        else {
            window.setSize(bounds.width, bounds.height);

            // Try to ensure that the window is in the client area of the display;
            // in particular, it shouldn't be under the Windows taskbar. Note that if
            // the window's bigger than the client area there's no way we can
            // succeed, and we'll just leave it in the upper left corner.
            Rectangle ca = display.getClientArea();
            Rectangle wb = window.getBounds();

            // There appears to be an SWT bug where, when there is more than one
            // monitor, it includes the Mac Menus in the client area, which is Not
            // Good for window positioning. This works around that problem.
            int minY = (OSUtils.MACOSX ? MAC_MENU_HEIGHT : 0);

            // if it's too low, move it up, but no higher than y = 0 (suitably
            // adjusted for the bug alluded to above).
            int newY = wb.y;
            int diff = ca.height - (wb.y + wb.height);
            if (diff < 0) {
                newY += diff;
            }
            if (newY < minY) {
                newY = minY;
            }

            // it's too far to the right, move it left, but not past x = 0
            int newX = wb.x;
            diff = ca.width - (wb.x + wb.width);
            if (diff < 0) {
                newX += diff;
            }
            if (newX < 0) {
                newX = 0;
            }

            window.setLocation(newX, newY);
        }

        incrementReferenceCount(display);

        window.addListener(SWT.Dispose, WindowClosed);

        return window;
    }

    /**
     * Display the given application window on its associated device.
     * <p>
     * Packing the contents of a window may adjust the window's size
     * to be different from that requested.
     *
     * @param shell        the window to display
     * @param packContents whether or not to "minimize" area displayed
     * @author             mlh
     * @see org.eclipse.swt.widgets.Control#pack
     */
    public static void display(Shell shell, boolean packContents)
    {
        if (packContents) {
            shell.pack();
        }

        shell.open();
    }

    /**
     * Display the given application window on its associated device.
     * <p>
     * Packing the contents of a window may adjust the window's size
     * to be different from that requested.
     *
     * @param shell        the window to display
     * @param packContents whether or not to "minimize" area displayed
     * @param changed      allows the caller to request that layout caches
     *                     be flushed
     * @author             mlh
     * @see org.eclipse.swt.widgets.Control#pack
     */
    public static void display(Shell shell,
                               boolean packContents,
                               boolean changed)
    {
        if (packContents) {
            shell.pack(changed);
        }

        shell.open();
    }

    /**
     * The main interaction loop.
     * <p>
     * Continue to process events while an open window exists.
     * <p>
     * <b>Implementation note</b>: On Macintosh systems, the convention
     * is to keep the application running even if there are no open windows.
     *
     * @see decrementReferenceCount(Display)
     * @author          mlh
     */
    public static void interact(boolean returnWhenDone)
    {
        try {
            while (GLOBAL_DISPLAY.getData() != null) {
                processDeferredDisposals();

                if (! GLOBAL_DISPLAY.readAndDispatch()) {
                    if (returnWhenDone) {
                        return;
                    }
                    GLOBAL_DISPLAY.sleep();
                }
            }
        }
        catch (SWTException e) {
            // In the case where the display is already disposed.
            // This happens on the Mac if you use the QUIT function in the SWT
            // menu or the red 'x' to close the window.
            // Does not happen if you exit using the file -> exit menu item,
            // or on Windows.

            // Do not display error.

            // Hides some error reports that are important... print out trace.
            if (! e.getMessage().equals("Device is disposed")) {
                System.err.println("SWTException caught in main CogTool loop.");
                e.printStackTrace();
                System.err.println();
                // then let the Exception catching code in main() try to
                // report it to the user
                throw e;
            }
        }
    }
    
    public static void interact() {
        interact(false);
    }

    /**
     * Arrange for the disposal of a widget to be deferred until the
     * next iteration of the main event loop. If there is no open window
     * go ahead and dispose it right away.
     */
	@SuppressWarnings("unchecked")
    public static void deferDisposal(Widget w)
    {
        if (GLOBAL_DISPLAY.getData() == null) {
            w.dispose();
            return;
        }
        List<Widget> deferrals =
            (List<Widget>) GLOBAL_DISPLAY.getData(DEFERRED_DISPOSALS_KEY);
        if (deferrals == null) {
            deferrals = new ArrayList<Widget>(1);
            GLOBAL_DISPLAY.setData(DEFERRED_DISPOSALS_KEY, deferrals);
        }
        deferrals.add(w);
    }

	@SuppressWarnings("unchecked")
    protected static void processDeferredDisposals()
    {
        List<Widget> deferred =
            (List<Widget>) GLOBAL_DISPLAY.getData(DEFERRED_DISPOSALS_KEY);
        if (deferred != null) {
            for (Iterator<Widget> it = deferred.iterator(); it.hasNext(); ) {
                it.next().dispose();
                it.remove();
            }
        }
    }

    /**
     * Nested interaction for pop-up (modal) windows.
     * <p>
     * This allows one to fetch values from the user and maintain
     * the place in the execution thread that wishes to use those values.
     *
     * @param shell   the pop-up window
     * @author        mlh
     */
    public static void interact(Shell shell)
    {
        Display display = shell.getDisplay();

        while (! shell.isDisposed()) {
            if (! display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    /**
     * Both displays and provides nested interaction for a pop-up (modal)
     * window.
     *
     * @param shell   the pop-up window
     * @author        mlh
     */
    public static void displayAndInteract(Shell shell)
    {
        displayAndInteract(shell, true);
    }

    /**
     * Both displays and provides nested interaction for a pop-up (modal)
     * window.
     *
     * @param shell        the pop-up window
     * @param packContents whether or not to "minimize" area displayed
     * @author             mlh
     */
    public static void displayAndInteract(Shell shell, boolean packContents)
    {
        display(shell, packContents);

        interact(shell);
    }

    /**
     * Creates a nested interaction pop-up (modal) window.
     *
     * For the "flag" parameters, use the following values:
     * <code>
     * buttons: one of the following combinations
     *          SWT.OK
     *          SWT.OK | SWT.CANCEL
     *          SWT.YES | SWT.NO
     *          SWT.YES | SWT.NO | SWT.CANCEL
     *          SWT.RETRY | SWT.CANCEL
     *          SWT.ABORT | SWT.RETRY | SWT.IGNORE
     *
     * icon: one of
     *       SWT.ICON_ERROR, SWT.ICON_INFORMATION, SWT.ICON_QUESTION,
     *       SWT.ICON_WARNING, SWT.ICON_WORKING, SWT.NONE
     *
     * mode: one of (in increasing order of restriction)
     *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
     * </code>
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @param buttons   which buttons to display in the pop-up
     * @param icon      the icon to display, indicating the message's nature
     * @param mode      whether the pop-up is modal relative to
     *                  the parent window, the application as a whole,
     *                  or the entire user's system/computer
     * @return          the created MessageBox
     * @author          mlh
     * @see org.eclipse.swt.widgets.MessageBox
     * @see org.eclipse.swt.SWT
     */
    public static MessageBox createMessageDialog(Shell parent,
                                                 String title,
                                                 String message,
                                                 int buttons,
                                                 int icon,
                                                 int mode)
    {
        MessageBox msgBox = new MessageBox(parent, buttons | icon | mode);

        msgBox.setText(title);
        msgBox.setMessage(message);

        return msgBox;
    }

    /**
     * Both displays and provides nested interaction for a pop-up (modal)
     * window built using SWT's <code>MessageBox</code>.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @param buttons   which buttons to display in the pop-up
     * @param icon      the icon to display, indicating the message's nature
     * @param mode      whether the pop-up is modal relative to
     *                  the parent window, the application as a whole,
     *                  or the entire user's system/computer
     * @return          which button was pushed to dismiss the pop-up
     *                  (see the <code>SWT</code> class for code constants)
     * @author          mlh
     * @see createMessageDialog
     */
    public static int presentMessageDialog(Shell parent,
                                           String title,
                                           String message,
                                           int buttons,
                                           int icon,
                                           int mode)
    {
        if (message.length() > MAX_BUFFER) {
            ScrollableDialog d =
                new ScrollableDialog(parent,
                                     title,
                                     buttons | icon | mode,
                                     message);

            Object response = d.open();

            return ((Integer) response).intValue();
        }

        MessageBox msgBox = createMessageDialog(parent, title, message,
                                                buttons, icon, mode);

        if (parent != null) {
            // TODO: it would be nice to center this on the parent window!
//            Point parentLoc = parent.getLocation();
//            Point parentSize = parent.getSize();
//            Point size = msgBox.getSize();
//            int x = parentLoc.x + parentSize.x / 2 - size.x / 2;
//            int y = parentLoc.y + parentSize.y / 2 - size.y / 2;
//            msgBox.setLocation(x, y);
        }
        // else don't bother trying to center it, since no parent

        return msgBox.open();   // no need to dispose; not a Widget!
    }

    /**
     * Simple modal error message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return SWT.OK
     * @author          mlh
     */
    public static MessageBox createErrorDialog(Shell parent,
                                               String title,
                                               String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.OK,
                                   SWT.ICON_ERROR,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal error message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.OK</code>, which must be the button pushed
     *                  to dismiss the pop-up
     * @author          mlh
     */
    public static int presentErrorDialog(Shell parent,
                                         String title,
                                         String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.OK,
                                    SWT.ICON_ERROR,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal warning message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return SWT.OK
     * @author          mlh
     */
    public static MessageBox createWarningDialog(Shell parent,
                                                 String title,
                                                 String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.OK,
                                   SWT.ICON_WARNING,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal warning message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.OK</code>, which must be the button pushed
     *                  to dismiss the pop-up
     * @author          mlh
     */
    public static int presentWarningDialog(Shell parent,
                                           String title,
                                           String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.OK,
                                    SWT.ICON_WARNING,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal informational message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return SWT.OK
     * @author          mlh
     */
    public static MessageBox createInformationDialog(Shell parent,
                                                     String title,
                                                     String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.OK,
                                   SWT.ICON_INFORMATION,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal informational message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.OK</code>, which must be the button pushed
     *                  to dismiss the pop-up
     * @author          mlh
     */
    public static int presentInformationDialog(Shell parent,
                                               String title,
                                               String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.OK,
                                    SWT.ICON_INFORMATION,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal error message with options to retry or cancel.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return one of SWT.RETRY or SWT.CANCEL
     * @author          mlh
     */
    public static MessageBox createErrorAbortDialog(Shell parent,
                                                    String title,
                                                    String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.RETRY | SWT.CANCEL,
                                   SWT.ICON_ERROR,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal error message with options to retry or cancel.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.RETRY</code> or <code>SWT.CANCEL</code>
     * @author          mlh
     */
    public static int presentErrorAbortDialog(Shell parent,
                                              String title,
                                              String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.RETRY | SWT.CANCEL,
                                    SWT.ICON_ERROR,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal confirm message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created Message; use open() to interact, which
     *                  will return one of SWT.OK or SWT.CANCEL
     * @author          mlh
     */
    public static MessageBox createConfirmDialog(Shell parent,
                                                 String title,
                                                 String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.OK | SWT.CANCEL,
                                   SWT.ICON_QUESTION,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal confirm message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.OK</code> or <code>SWT.CANCEL</code>
     * @author          mlh
     */
    public static int presentConfirmDialog(Shell parent,
                                           String title,
                                           String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.OK | SWT.CANCEL,
                                    SWT.ICON_QUESTION,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * The maximum number of items <code>presentConfirmItemsDialog</code> will show
     * without truncating and adding "and <i>N</i> more." An exception is that if
     * there are one more items than this constant, all will be shown, since it
     * would be silly to simply replace the last with "and 1 more."
     */
    public static final int CONFIRM_ITEMS_LIMIT = 12;

    /**
     * This displays a modal confirm dialog, that also lists an array of items.
     * A prototypical use of this would be "Really delete the following?" and a
     * list of the things to be deleted. The items being listed must implement
     * the interface INamedObject.
     *
     * @param parent     the window the alert dialog adheres to
     * @param title      the title for the alert dialog window
     * @param message    the message to display
     * @param items      an array of items, whose names are to be be enumerated
     * @return           <code>SWT.OK</code> or <code>SWT.CANCEL</code>
     */
    public static int presentConfirmItemsDialog(Shell parent,
                                                String title,
                                                String message,
                                                NamedObject[] items)
    {
        String[] itemNames = new String[items.length];

        for (int i = 0; i < items.length; i++) {
            itemNames[i] = items[i].getName();
        }

        return presentConfirmItemsDialog(parent, title, message, itemNames);
    }

    /**
     * This displays a modal confirm dialog, that also lists an array of items,
     * but just passing in the <code>String</code> names of the items rather
     * than the items themselves.
     *
     * @param parent     the window the alert dialog adheres to
     * @param title      the title for the alert dialog window
     * @param message    the message to display
     * @param items      an array of <code>String</code> names of the items
     * @return           <code>SWT.OK</code> or <code>SWT.CANCEL</code>
     */
    public static int presentConfirmItemsDialog(Shell parent,
                                                String title,
                                                String message,
                                                String[] itemNames)
    {
        String indent = "\n    ";
        StringBuilder formattedMessage = new StringBuilder(message);

        int limit = itemNames.length;

        for (int i = 0; i < limit; ++i) {
            formattedMessage.append(indent);
            formattedMessage.append(itemNames[i]);
        }

        return presentMessageDialog(parent,
                                    title,
                                    formattedMessage.toString(),
                                    SWT.OK | SWT.CANCEL,
                                    SWT.ICON_QUESTION,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal yes-no question message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return one of SWT.YES or SWT.NO
     * @author          mlh
     */
    public static MessageBox createYesNoQuestionDialog(Shell parent,
                                                       String title,
                                                       String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.YES | SWT.NO,
                                   SWT.ICON_QUESTION,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal yes-no question message.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.YES</code> or <code>SWT.NO</code>
     * @author          mlh
     */
    public static int presentYesNoQuestionDialog(Shell parent,
                                                 String title,
                                                 String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.YES | SWT.NO,
                                    SWT.ICON_QUESTION,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal yes-no question with option to cancel.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          the created MessageBox; use open() to interact, which
     *                  will return one of SWT.YES, SWT.NO, or SWT.CANCEL
     * @author          mlh
     */
    public static MessageBox createYesNoCancelDialog(Shell parent,
                                                     String title,
                                                     String message)
    {
        return createMessageDialog(parent,
                                   title,
                                   message,
                                   SWT.YES | SWT.NO | SWT.CANCEL,
                                   SWT.ICON_QUESTION,
                                   SWT.PRIMARY_MODAL);
    }

    /**
     * Simple modal yes-no question with option to cancel.
     *
     * @param parent    the window the pop-up "adheres" to
     * @param title     the title for the pop-up window
     * @param message   the message to display to the user
     * @return          <code>SWT.YES</code> or <code>SWT.NO</code>
     *                  or <code>SWT.CANCEL</code>
     * @author          mlh
     */
    public static int presentYesNoCancelDialog(Shell parent,
                                               String title,
                                               String message)
    {
        return presentMessageDialog(parent,
                                    title,
                                    message,
                                    SWT.YES | SWT.NO | SWT.CANCEL,
                                    SWT.ICON_QUESTION,
                                    SWT.PRIMARY_MODAL);
    }

    /**
     * A class to support the construction of more interesting pop-up
     * dialog boxes.
     * <p>
     * A subclass need only override <code>buildDialog</code> to build
     * the contents of the dialog's window.  Other methods may be provided
     * by subclasses as needed, such as to provide access to the values
     * collected from the user.
     *
     * @author           mlh
     */
    public static abstract class SimpleDialog
    {
        protected String dialogTitle;
        protected int popupMode;
        protected int style;
        protected Font buttonFont;
        protected Font textFont;
        protected Object userResponse = null; // how user dismisses the dialog
        protected Shell dialog;
        protected Shell parent;

        /**
         * Initialize the dialog for later pop-up.
         * <p>
         * mode: one of (in increasing order of restriction)
         * <code>
         *       SWT.MODELESS, SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL,
         *       SWT.SYSTEM_MODAL
         * </code>
         * <p>
         * <b>Implementation note</b>: The values pertinent for the
         * <code>styleFlags</code> parameter are those defined in SWT's
         * documentation the Shell constructors' style parameter that are not
         * used for the <code>mode</code> parameter.  Thus, we separate two
         * sets of things, mode and style, which are combined into one by SWT;
         * for the purposes of this class, values of style should be the
         * set theoretic difference of SWT's style values and the modality
         * values.)
         *
         * @param parentWin the window the pop-up "adheres" to
         * @param title     the string to be used as the dialog window's title
         * @param mode      whether the pop-up is modal relative to
         *                  the parent window, the application as a whole,
         *                  or the entire user's system/computer
         * @param styleFlags flags passed through to SWT's <code>Dialog</code>
         * @author          mlh
         * @see org.eclipse.swt.widgets.Dialog
         */
        public SimpleDialog(Shell parentWin,
                            String title,
                            int mode,
                            int styleFlags)
        {
            parent = parentWin;
            dialogTitle = title;
            popupMode = mode;
            style = styleFlags;
            userResponse = null;
            dialog = null;

            if (OSUtils.MACOSX) {
                buttonFont = new Font(WindowUtil.GLOBAL_DISPLAY,
                                           "Lucida Grande", 13, SWT.NORMAL);
                textFont = buttonFont;
            }
        }

        /**
         * Initialize the dialog for later pop-up.
         * <p>
         * mode: one of (in increasing order of restriction)
         * <code>
         *       SWT.MODELESS, SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL,
         *       SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin    the window the pop-up "adheres" to
         * @param title     the string to be used as the dialog window's title
         * @param mode      whether the pop-up is modal relative to
         *                  the parent window, the application as a whole,
         *                  or the entire user's system/computer
         * @author          mlh
         * @see org.eclipse.swt.widgets.Dialog
         */
        public SimpleDialog(Shell parentWin, String title, int mode)
        {
            this(parentWin, title, mode, SWT.DIALOG_TRIM);
        }

        public SimpleDialog(String title, int mode, int styleFlags)
        {
            this(null, title, mode, styleFlags);
        }

        public SimpleDialog(String title, int mode)
        {
            this(null, title, mode, SWT.DIALOG_TRIM);
        }

        /**
         * Construct the contents of the dialog window.
         * <p>
         * Subclasses should override this method; it is responsible for
         * creating contents by adding to <code>this.dialog</code>.
         * <p>
         * Listeners based on contents are responsible for setting
         * <code>this.userResponse</code>
         * and for closing or disposing the <code>this.dialog</code> shell!
         *
         * @author          mlh
         */
        protected abstract void buildDialog();

        protected static final int MIN_X = 0;
        protected static final int MIN_Y = (OSUtils.MACOSX ? 24 : 0);

        /**
         * Create the pop-up dialog window, populate it (using
         * <code>buildDialog</code>), and interact with the user.
         * <p>
         * As noted in <code>buildDialog</code>, the subclass' listeners are
         * responsible for closing or disposing the dialog window and
         * for setting which user's response was used to dismiss the window.
         *
         * @return          the user's response that dismissed the window
         * @author          mlh
         */
        public Object open()
        {
            if (parent != null) {
                dialog =
                    new Shell(parent, style | popupMode);
            }
            else {
                dialog = new Shell(WindowUtil.GLOBAL_DISPLAY,
                                        style | popupMode);
            }

            dialog.addDisposeListener(new DisposeListener() {

                public void widgetDisposed(DisposeEvent arg0)
                {
                    if (buttonFont != null) {
                        buttonFont.dispose();
                    }
                }
            });

            dialog.setText(dialogTitle);

            buildDialog();

            display(dialog, true);

            if (parent != null) {
                Point parentLoc = parent.getLocation();
                Point parentSize = parent.getSize();
                Point size = dialog.getSize();
                int x = parentLoc.x + parentSize.x / 2 - size.x / 2;
                if (x < MIN_X) {
                    x = MIN_X;
                }
                int y = parentLoc.y + parentSize.y / 2 - size.y / 2;
                if (y < MIN_Y) {
                    y = MIN_Y;
                }
                dialog.setLocation(x, y);
            }
            // else don't bother trying to center it, since no parent

            interact(dialog);

            return userResponse;
        }
    }

    /**
     * A simple class representing a dialog pop-up that prompts for a single
     * <code>String</code> value
     * <p>
     * The dialog has two buttons: "OK" and "Cancel" (localized)
     * <p>
     * The return value of <code>open</code> on an instance of
     * <code>PromptDialog</code> will indicate which button was selected;
     * see the constants <code>OK</code> and <code>CANCEL</code>.
     *
     * @author           mlh
     */
    public static class PromptDialog extends CustomDialog
    {
        protected static int NO_WIDTH_HINT = -1;

        protected String responseLabel;
        protected String request;
        protected String promptResponse;

        // Needed for listeners
        protected Text responseBox;

        // By default, OK is disabled unless the prompt response is not empty
        protected boolean disableOK = true;

        protected int widthHint = NO_WIDTH_HINT;

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         * <p>
         * <p>
         * <b>Implementation note</b>: The values pertinent for the
         * <code>styleFlags</code> parameter are those defined in SWT's
         * documentation the Shell constructors' style parameter that are not
         * used for the <code>mode</code> parameter.  Thus, we separate two
         * sets of things, mode and style, which are combined into one by SWT;
         * for the purposes of this class, values of style should be the
         * set theoretic difference of SWT's style values and the modality
         * values.)
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param widHint         the width hint for the response box, or
         *                        NO_WIDTH_HINT
         * @param responseLbl     label of the text input field
         * @param requestQuestion question or instructions
         * @param defaultResponse initial value for the text input field
         * @param styleFlags      flags passed to SWT's <code>Dialog</code>
         * @author                mlh
         * @see org.eclipse.swt.widgets.Dialog
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            int widHint,
                            String responseLbl,
                            String requestQuestion,
                            String defaultResponse,
                            int styleFlags)
        {
            super(parentWin, title, mode, styleFlags);

            responseLabel = responseLbl;
            request = requestQuestion;
            promptResponse = defaultResponse;
            widthHint = widHint;
        }

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         * <p>
         * <p>
         * <b>Implementation note</b>: The values pertinent for the
         * <code>styleFlags</code> parameter are those defined in SWT's
         * documentation the Shell constructors' style parameter that are not
         * used for the <code>mode</code> parameter.  Thus, we separate two
         * sets of things, mode and style, which are combined into one by SWT;
         * for the purposes of this class, values of style should be the
         * set theoretic difference of SWT's style values and the modality
         * values.)
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param responseLbl     label of the text input field
         * @param requestQuestion question or instructions
         * @param defaultResponse initial value for the text input field
         * @param styleFlags      flags passed to SWT's <code>Dialog</code>
         * @author                mlh
         * @see org.eclipse.swt.widgets.Dialog
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            String responseLbl,
                            String requestQuestion,
                            String defaultResponse,
                            int styleFlags)
        {
            this(parentWin, title, mode, NO_WIDTH_HINT,
                 responseLbl, requestQuestion, defaultResponse,
                 SWT.DIALOG_TRIM);
        }

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param widHint         the width hint for the response box, or
         *                        NO_WIDTH_HINT
         * @param responseLbl     label of the text input field
         * @param requestQuestion question or instructions
         * @param defaultResponse initial value for the text input field
         * @author                mlh
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            int widHint,
                            String responseLbl,
                            String requestQuestion,
                            String defaultResponse)
        {
            this(parentWin, title, mode, widHint,
                 responseLbl, requestQuestion, defaultResponse,
                 SWT.DIALOG_TRIM);
        }

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param responseLbl     label of the text input field
         * @param requestQuestion question or instructions
         * @param defaultResponse initial value for the text input field
         * @author                mlh
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            String responseLbl,
                            String requestQuestion,
                            String defaultResponse)
        {
            this(parentWin, title, mode,
                 responseLbl, requestQuestion, defaultResponse,
                 SWT.DIALOG_TRIM);
        }

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * Assumes an empty default value ("").
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param responseLbl     label of the text input field
         * @param requestQuestion question or instructions
         * @author                mlh
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            String responseLbl,
                            String requestQuestion)
        {
            this(parentWin, title, mode,
                 responseLbl, requestQuestion, "", SWT.DIALOG_TRIM);
        }

        /**
         * Initialize the string prompting dialog for later pop-up.
         * <p>
         * Assumes an empty default value ("") and an empty question.
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param responseLbl     label of the text input field
         * @author                mlh
         */
        public PromptDialog(Shell parentWin,
                            String title,
                            int mode,
                            String responseLbl)
        {
            this(parentWin, title, mode, responseLbl, "", "", SWT.DIALOG_TRIM);
        }

        /**
         * What to do when the OK button is selected by the user.
         *
         * @author mlh
         */
        @Override
        protected void onOK()
        {
            promptResponse = responseBox.getText();
            super.onOK();
        }

        /**
         * Fetch the <code>String</code> value of the prompt specified by
         * the user.
         *
         * @return     the <code>String</code> value of the prompt
         *             specified by the user.
         * @author     mlh
         */
        public String getPromptResponse()
        {
            return promptResponse;
        }

        protected void addResponseBox()
        {
            responseBox =
                new ManagedText(dialog,
                                SWT.SINGLE | SWT.BORDER,
                                Keypad.FULL_KEYPAD)
                {
                    @Override
                    protected void onModify()
                    {
                        if ((okButton != null) && disableOK) {
                            okButton.setEnabled(getText().length() > 0);
                        }
                    }
                };
        }

        /**
         * Allows subclasses to add more dialog "fields" between the question
         * and response "field" and the OK/CANCEL buttons.
         * <p>
         * Subclasses should know that the dialog uses a
         * <code>GridLayout</code> instance of four columns, and thus should
         * attach <code>GridData</code> instances to each "field", using
         * <code>setLayoutData</code>.
         *
         * @author mlh
         */
        @Override
        protected void addMoreFields()
        {
//<<<<<<< .mine
//            // Four columns
//            GridLayout layout = new GridLayout(4, false);
//
//            if (OSUtils.MACOSX) {
//                layout.marginLeft = 19;
//                layout.marginRight = 13;
//                layout.marginTop = 7;
//                layout.marginBottom = 12;
//
//                layout.horizontalSpacing = 0;
//                layout.verticalSpacing = 4;
//            }
//
//            this.dialog.setLayout(layout);
//
//=======
//>>>>>>> .r832
            // If given a request question, center, filling width
            if ((request != null) && (request != "")) {
                Label requestLabel = new Label(dialog, SWT.NONE);

                if (textFont != null) {
                    requestLabel.setFont(textFont);
                }

                requestLabel.setText(request);

                GridData reqLayout = new GridData();

                reqLayout.grabExcessHorizontalSpace = true;
                reqLayout.horizontalSpan = 4;

                requestLabel.setLayoutData(reqLayout);
            }

            // If given a label, left-most column, right-justified
            if ((responseLabel != null) && (responseLabel != "")) {
                Label lbl = new Label(dialog, SWT.NONE);

                if (textFont != null) {
                    lbl.setFont(textFont);
                }

                lbl.setText(responseLabel);

                GridData lblLayout =
                    new GridData(GridData.HORIZONTAL_ALIGN_END);

                lbl.setLayoutData(lblLayout);
            }

            // Text box to collect response, three columns, align left
            addResponseBox();

            if ((promptResponse != null) && (promptResponse != "")) {
                if (textFont != null) {
                    responseBox.setFont(textFont);
                }

                responseBox.setText(promptResponse);
                responseBox.selectAll();
            }

            GridData responseLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            responseLayout.grabExcessHorizontalSpace = true;
            if (widthHint > 0) {
                responseLayout.widthHint = widthHint;
            }
            responseLayout.horizontalSpan =
                ((responseLabel != null) && ! responseLabel.equals(""))
                        ? 3
                        : 4;    // use entire width if no label!

            responseBox.setLayoutData(responseLayout);
            responseBox.setFocus();

            // If needed, subclasses should extend this method.
        }

        /**
         * Allows subclasses to add more dialog "buttons" after the
         * OK/CANCEL buttons.
         * <p>
         * Subclasses should know that the dialog uses a
         * <code>GridLayout</code> instance of four columns, and thus should
         * attach <code>GridData</code> instances to each "button", using
         * <code>setLayoutData</code>.
         *
         * @author mlh
         */
        @Override
        protected void addMoreButtons()
        {
            if (disableOK) {
                okButton.setEnabled(promptResponse.length() > 0);
            }
        }

    } // PromptDialog

    /**
     * A simple class representing a dialog pop-up.
     * <p>
     * The dialog has two buttons: "OK" and "Cancel" (localized)
     * <p>
     * The return value of <code>open</code> on an instance of
     * <code>PromptDialog</code> will indicate which button was selected;
     * see the constants <code>OK</code> and <code>CANCEL</code>.
     *
     * @author           mlh
     */
    public static class CustomDialog extends SimpleDialog
    {
        /**
         * The return value of <code>open</code> when the user selects "OK".
         */
        public static final String OK = "OK";

        /**
         * The return value of <code>open</code> when the user selects
         * "Cancel".
         */
        public static final String CANCEL = "Cancel";

        // Available for alignment
        protected Button okButton;
        protected Button cancelButton;
        protected Label rightOfButtons;

        protected Listener okListener;

        /**
         * <p>
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         * <p>
         * <p>
         * <b>Implementation note</b>: The values pertinent for the
         * <code>styleFlags</code> parameter are those defined in SWT's
         * documentation the Shell constructors' style parameter that are not
         * used for the <code>mode</code> parameter.  Thus, we separate two
         * sets of things, mode and style, which are combined into one by SWT;
         * for the purposes of this class, values of style should be the
         * set theoretic difference of SWT's style values and the modality
         * values.)
         *
         * @param parentWin       the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @param styleFlags      flags passed to SWT's <code>Dialog</code>
         * @param actionOnOK      what to do when the OK button is picked
         * @author                alexeiser
         * @see org.eclipse.swt.widgets.Dialog
         */
        public CustomDialog(Shell parentWin,
                            String title,
                            int mode,
                            int styleFlags)
        {
            super(parentWin, title, mode, SWT.DIALOG_TRIM | styleFlags);

            okListener = new Listener() {
                                  public void handleEvent(Event evt)
                                  {
                                      onOK();
                                  }
                              };
        }

        /**
         * <code>
         * mode: one of (in increasing order of restriction)
         *       SWT.PRIMARY_MODAL, SWT.APPLICATION_MODAL, SWT.SYSTEM_MODAL
         * </code>
         *
         * @param parentWin          the window the pop-up "adheres" to
         * @param title           the dialog window's title
         * @param mode            whether the pop-up is modal relative to the
         *                        parent window, the application as a whole,
         *                        or the entire user's system/computer
         * @author                alexeiser
         */
        public CustomDialog(Shell parentWin, String title, int mode)
        {
            this(parentWin, title, mode, SWT.NONE);
        }

        /**
         *
         */
        public Button getOK()
        {
            return okButton;
        }

        /**
         * What to do when the OK button is selected by the user.
         *
         * @author mlh
         */
        protected void onOK()
        {
            userResponse = OK;   // success!
            dialog.close();
        }

        /**
         * Construct the contents of the dialog window.
         * <p>
         * First, the question/instructions, if specified.<br>
         * Next, the label (if specified) and a text input field,
         * on the same line.<br>
         * Finally, an "OK" and a "Cancel" button.
         *
         * @author          mlh
         */
        @Override
        protected void buildDialog()
        {
            // Four columns
            GridLayout layout = new GridLayout(4, false);

            if (OSUtils.MACOSX) {
                layout.marginLeft = 19;
                layout.marginRight = 13;
                layout.marginTop = 7;
                layout.marginBottom = 12;
            }

            dialog.setLayout(layout);

            addMoreFields();

            // Buttons (OK and Cancel), centered in the 2nd and 3rd columns
            Label emptyCell = new Label(dialog, SWT.NONE);
            GridData emptyCellLayout = new GridData();
            emptyCellLayout.grabExcessHorizontalSpace = true;
            emptyCell.setLayoutData(emptyCellLayout);

            okButton = new Button(dialog, SWT.PUSH);
            cancelButton = new Button(dialog, SWT.PUSH);

            if (OSUtils.MACOSX) {
                rightOfButtons =
                    new Label(dialog, SWT.NONE); // 2nd column
                emptyCellLayout = new GridData();
                emptyCellLayout.grabExcessHorizontalSpace = true;
                rightOfButtons.setLayoutData(emptyCellLayout);

                // See Apple HIGs:
                // http://developer.apple.com/documentation/UserExperience/Conceptual/OSXHIGuidelines/XHIGControls/chapter_18_section_2.html#//apple_ref/doc/uid/TP30000359-TPXREF186
//                this.cancelButton.setSize(68, 20);
//                this.okButton.setSize(68, 20);
            }

            if (buttonFont != null) {
                okButton.setFont(buttonFont);
                cancelButton.setFont(buttonFont);
            }

            okButton.setText(L10N.get("B.OK", "OK"));
            cancelButton.setText(L10N.get("B.CANCEL", "Cancel"));

            dialog.setDefaultButton(okButton);

            GridData okLayout = new GridData(GridData.HORIZONTAL_ALIGN_END);
            GridData cancelLayout =
                new GridData(GridData.HORIZONTAL_ALIGN_END);

            okButton.setLayoutData(okLayout);
            cancelButton.setLayoutData(cancelLayout);

            if (OSUtils.MACOSX) {
                // See Apple HIGs:
                // http://developer.apple.com/documentation/UserExperience/Conceptual/OSXHIGuidelines/XHIGControls/chapter_18_section_2.html#//apple_ref/doc/uid/TP30000359-TPXREF186
                okLayout.widthHint = 82;
                cancelLayout.widthHint = 82;
            }
            else {
                rightOfButtons =
                    new Label(dialog, SWT.NONE); // 4th column
                emptyCellLayout = new GridData();
                emptyCellLayout.grabExcessHorizontalSpace = true;
                rightOfButtons.setLayoutData(emptyCellLayout);
            }

            // Dismiss the dialog box when OK button selected, with success
            okButton.addListener(SWT.Selection, okListener);

            // Dismiss the dialog box when Cancel button selected, with failure
            cancelButton.addListener(SWT.Selection,
                                          new Listener() {
                                              public void handleEvent(Event e)
                                              {
                                                  userResponse = CANCEL;
                                                  dialog.close();
                                              }
                                          });

            addMoreButtons();
        } // buildDialog

        /**
         * Allows subclasses to add more dialog "fields" between the question
         * and response "field" and the OK/CANCEL buttons.
         * <p>
         * Subclasses should know that the dialog uses a
         * <code>GridLayout</code> instance of four columns, and thus should
         * attach <code>GridData</code> instances to each "field", using
         * <code>setLayoutData</code>.
         *
         * @author mlh
         */
        protected void addMoreFields()
        {
            // If needed, subclasses should override this method.
        }

        /**
         * Allows subclasses to add more dialog "buttons" after the
         * OK/CANCEL buttons.
         * <p>
         * Subclasses should know that the dialog uses a
         * <code>GridLayout</code> instance of four columns, and thus should
         * attach <code>GridData</code> instances to each "button", using
         * <code>setLayoutData</code>.
         *
         * @author mlh
         */
        protected void addMoreButtons()
        {
            // If needed, subclasses should override this method.
        }

    } // CustomDialog

    /**
     * Utility a child thread may use to schedule a Runnable on the
     * main UI thread asynchronously.
     *
     * @param r runnable to be scheduled asynchronously
     */
    public static void scheduleAsynchronously(Runnable r)
    {
        // Note that the f.run() throws an uncaught Exception, it will be
        // wrapped in an SWTException and caught not in main, but in
        // WindowUtil.interact().
        if (! WindowUtil.GLOBAL_DISPLAY.isDisposed()) {
            WindowUtil.GLOBAL_DISPLAY.asyncExec(r);
        }
    }

    public static abstract class CustomToolTip
    {
        protected Shell toolTipWindow = null;
        protected Label toolTipLabel = null;
        protected Point evtLocation = new Point(0, 0);
        protected StringBuilder toolTipText = new StringBuilder();
        protected Control tipOwner;

        protected Runnable visibleTimer =
            new Runnable() {
                public void run()
                {
                    // It's possible to close the window before the
                    // timer goes off.
                    if ((toolTipWindow != null) && ! toolTipWindow.isDisposed())
                    {
                        toolTipWindow.setVisible(false);
                    }
                }
            };

        public CustomToolTip(Control owningWidget)
        {
            Listener tipListener =
                new Listener() {
                    public void handleEvent(Event evt)
                    {
                        switch (evt.type) {
                            case SWT.Dispose: {
                                if (toolTipWindow != null) {
                                    toolTipWindow.dispose();
                                    toolTipWindow = null;
                                    toolTipLabel = null;
                                }
                                break;
                            }
                            case SWT.KeyDown:
                            case SWT.MouseMove: {
                                if (toolTipWindow != null) {
                                    toolTipWindow.setVisible(false);
                                }
                                break;
                            }
                            case SWT.MouseHover: {
                                setToolTip(evt.x, evt.y);
                                break;
                            }
                        }
                    }
                };

            tipOwner = owningWidget;
            tipOwner.addListener(SWT.Dispose, tipListener);
            tipOwner.addListener(SWT.KeyDown, tipListener);
            tipOwner.addListener(SWT.MouseMove, tipListener);
            tipOwner.addListener(SWT.MouseHover, tipListener);
        }

        protected abstract void setTipContents(int x, int y);

        protected void setToolTip(int x, int y)
        {
            evtLocation.x = x;
            evtLocation.y = y;

            if (toolTipWindow == null) {
                createToolTip();
            }

            setTipContents(x, y);

            Point size =
                toolTipWindow.computeSize(SWT.DEFAULT, SWT.DEFAULT);
            Point pt = tipOwner.toDisplay(x, y);

            toolTipWindow.setBounds(pt.x + 2, pt.y + 2, size.x, size.y);
//TODO: when fixed            this.toolTipWindow.setVisible(true);
            WindowUtil.GLOBAL_DISPLAY.timerExec(5000, visibleTimer);
        }

        protected abstract boolean handleClick(Event evt);

        protected void createToolTip()
        {
            toolTipWindow =
                new Shell(tipOwner.getShell(), SWT.ON_TOP | SWT.TOOL);
            toolTipWindow.setVisible(false);
            toolTipWindow.setLayout(new FillLayout());

            toolTipLabel = new Label(toolTipWindow, SWT.NONE);

            Color labelFG =
                GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_INFO_FOREGROUND);
            Color labelBG =
                GLOBAL_DISPLAY.getSystemColor(SWT.COLOR_INFO_BACKGROUND);
            toolTipLabel.setForeground(labelFG);
            toolTipLabel.setBackground(labelBG);

            Listener labelListener =
                new Listener() {
                    public void handleEvent(Event evt)
                    {
                        switch (evt.type) {
                            case SWT.MouseDown: {
                                if (! handleClick(evt)) {
                                    break;
                                }

                                // Fall through!
                            }
                            case SWT.MouseExit: {
                                toolTipWindow.setVisible(false);
                                break;
                            }
                        }
                    }
                };

            toolTipLabel.addListener(SWT.MouseExit, labelListener);
            toolTipLabel.addListener(SWT.MouseDown, labelListener);
        }
    }

    public static final int MAX_BUFFER = 1000;

    public static class ScrollableDialog extends SimpleDialog
    {
        protected static final int OK_INDEX = 0;
        protected static final int CANCEL_INDEX = 1;
        protected static final int YES_INDEX = 2;
        protected static final int NO_INDEX = 3;
        protected static final int RETRY_INDEX = 4;
        protected static final int ABORT_INDEX = 5;
        protected static final int IGNORE_INDEX = 6;

        protected static final Integer OK = new Integer(SWT.OK);
        protected static final Integer CANCEL = new Integer(SWT.CANCEL);
        protected static final Integer YES = new Integer(SWT.YES);
        protected static final Integer NO = new Integer(SWT.NO);
        protected static final Integer RETRY = new Integer(SWT.RETRY);
        protected static final Integer ABORT = new Integer(SWT.ABORT);
        protected static final Integer IGNORE = new Integer(SWT.IGNORE);

        // Values are in the same order as the index constants above
        protected static final Integer[] VALUES =
            { OK, CANCEL, YES, NO, RETRY, ABORT, IGNORE };

        protected static final String[] LABELS =
        {
            L10N.get("B.OK", "OK"),
            L10N.get("B.Cancel", "Cancel"),
            L10N.get("B.Yes", "Yes"),
            L10N.get("B.No", "No"),
            L10N.get("B.Retry", "Retry"),
            L10N.get("B.Abort", "Abort"),
            L10N.get("B.Ignore", "Ignore")
        };

        protected final SelectionListener OK_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = OK;
                    dialog.close();
                }
            };
        protected final SelectionListener CANCEL_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = CANCEL;
                    dialog.close();
                }
            };
        protected final SelectionListener YES_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = YES;
                    dialog.close();
                }
            };
        protected final SelectionListener NO_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = NO;
                    dialog.close();
                }
            };
        protected final SelectionListener RETRY_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = RETRY;
                    dialog.close();
                }
            };
        protected final SelectionListener ABORT_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = ABORT;
                    dialog.close();
                }
            };
        protected final SelectionListener IGNORE_LISTENER =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent e)
                {
                    userResponse = IGNORE;
                    dialog.close();
                }
            };

        protected final SelectionListener[] LISTENERS =
        { OK_LISTENER, CANCEL_LISTENER, YES_LISTENER, NO_LISTENER,
          RETRY_LISTENER, ABORT_LISTENER, IGNORE_LISTENER };

        protected String message;

        // saved to determine which buttons should be added
        // TODO: add icons?
        protected int mode;

        public ScrollableDialog(Shell parentWin,
                                String title,
                                int flags,
                                String msg)
        {
            super(parentWin, title, flags);

            message = msg;
            mode = flags;
        }

        protected void addEmptyCell()
        {
            Label emptyCell = new Label(dialog, SWT.NONE);

            GridData emptyCellLayout = new GridData();

            emptyCellLayout.grabExcessHorizontalSpace = true;

            emptyCell.setLayoutData(emptyCellLayout);
        }

        protected Button buildButton(int index, boolean isDefault)
        {
            Button newButton = new Button(dialog, SWT.PUSH);

            newButton.setText(LABELS[index]);

            newButton.addSelectionListener(LISTENERS[index]);

            GridData buttonLayout = new GridData(GridData.HORIZONTAL_ALIGN_FILL);

            buttonLayout.widthHint = 100;

            newButton.setLayoutData(buttonLayout);

            if (isDefault && (dialog.getDefaultButton() == null)) {
                newButton.setFocus();
                dialog.setDefaultButton(newButton);
            }

            return newButton;
        }

        @Override
        public void buildDialog()
        {
            Layout layout = new GridLayout(5, false);
            dialog.setLayout(layout);

            GridData textLayout = new GridData();

            textLayout.horizontalAlignment = GridData.FILL;
            textLayout.grabExcessHorizontalSpace = true;
            textLayout.widthHint = 600;
            textLayout.heightHint = 500;
            textLayout.horizontalSpan = 5;

            Text t = new Text(dialog,
                              SWT.MULTI | SWT.READ_ONLY | SWT.WRAP |
                              SWT.V_SCROLL);
            t.setText(message);
            t.setLayoutData(textLayout);

            int numButtons = 0;

            boolean[] setButtons = new boolean[VALUES.length];

            for (int i = 0; i < VALUES.length; i++) {
                if ((mode & VALUES[i].intValue()) != 0) {
                    setButtons[i] = true;
                    numButtons++;
                }
            }

            if ((numButtons == 1) || (numButtons > 3)) {
                addEmptyCell();
                addEmptyCell();
                addEmptyCell();
                addEmptyCell();
                buildButton(OK_INDEX, true);
            }
            else if (numButtons == 2) {
                addEmptyCell();
                addEmptyCell();
                addEmptyCell();
                if (setButtons[YES_INDEX]) {
                    if (OSUtils.MACOSX) {
                        buildButton(NO_INDEX, false);
                        buildButton(YES_INDEX, true);
                    }
                    else {
                        buildButton(YES_INDEX, true);
                        buildButton(NO_INDEX, false);
                    }
                }
                else if (setButtons[RETRY_INDEX]) {
                    if (OSUtils.MACOSX) {
                        buildButton(CANCEL_INDEX, false);
                        buildButton(RETRY_INDEX, true);
                    }
                    else {
                        buildButton(RETRY_INDEX, true);
                        buildButton(CANCEL_INDEX, false);
                    }
                }
                else {
                    if (OSUtils.MACOSX) {
                        buildButton(CANCEL_INDEX, false);
                        buildButton(OK_INDEX, true);
                    }
                    else {
                        buildButton(OK_INDEX, true);
                        buildButton(CANCEL_INDEX, false);
                    }
                }
            }
            else if (numButtons == 3) {
                if (OSUtils.MACOSX) {
                    addEmptyCell();
                    if (setButtons[ABORT_INDEX]) {
                        buildButton(RETRY_INDEX, false);
                        addEmptyCell();
                        buildButton(IGNORE_INDEX, false);
                        buildButton(ABORT_INDEX, true);
                    }
                    else {
                        buildButton(NO_INDEX, false);
                        addEmptyCell();
                        buildButton(CANCEL_INDEX, false);
                        buildButton(YES_INDEX, true);
                    }
                }
                else {
                    addEmptyCell();
                    addEmptyCell();
                    if (setButtons[ABORT_INDEX]) {
                        buildButton(ABORT_INDEX, true);
                        buildButton(RETRY_INDEX, false);
                        buildButton(IGNORE_INDEX, false);
                    }
                    else {
                        buildButton(YES_INDEX, true);
                        buildButton(NO_INDEX, false);
                        buildButton(CANCEL_INDEX, false);
                    }
                }
            }
        } // buildDialog
    } // ScrollableDialog
} // WindowUtil
