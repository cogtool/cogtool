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

import java.io.File;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.model.ImportConverter;
import edu.cmu.cs.hcii.cogtool.util.DoubleEntry;
import edu.cmu.cs.hcii.cogtool.util.Keypad;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifier.ILIDTransmuter;
import edu.cmu.cs.hcii.cogtool.util.ListenerIdentifierMap;
import edu.cmu.cs.hcii.cogtool.util.ManagedCombo;
import edu.cmu.cs.hcii.cogtool.util.ManagedText;
import edu.cmu.cs.hcii.cogtool.util.MenuUtil.MenuItemDefinition;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.StatusDisplayable;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

/**
 * Abstract class for sharing implementation that depends on having
 * an SWT Shell instance representing the View's window.
 *
 * @author mlh
 */
public abstract class View implements StatusDisplayable
{
    /**
     * Drawing block flag -- set true when you know useless drawing is about
     * to be triggered indirectly.
     */
    public static final ThreadLocal<Boolean> drawingFlags =
        new ThreadLocal<Boolean>()
        {
            @Override
            protected Boolean initialValue()
            {
                return Boolean.TRUE;
            }
        };

    protected static Image[] images;

    /**
     * The SWT instance representing the View's window.
     */
    protected Shell shell;

    /**
     * The window's contextual menu manager
     */
    protected ContextMenuManager contextMenus = null;

    protected ListenerIdentifierMap lIDMap;
    protected ILIDTransmuter transmuter;
    protected boolean performingAction = false;

    protected MenuFactory.IWindowMenuData<?> windowMenuData;

    // Generic selection adapter for events
    // used by FrameEditor and DemoView and others to have an easy way of
    // adding a listener to an SWT widget which has a WidgetSelectEvent
    // Great for buttons
    protected class SWTWidgetChangeHandler extends SelectionAdapter
    {
        CogToolLID lid;

        public SWTWidgetChangeHandler(CogToolLID setLid)
        {
            if (setLid == null) {
                throw new IllegalArgumentException(
                        "Cannot create a widgetChangeHandler with a null lid");
            }

            lid = setLid;
        }

        @Override
        public void widgetSelected(SelectionEvent evt)
        {
            Control source = (Control) evt.getSource();

            if (source.isEnabled()) {
                performAction(lid);
            }
        }
    }

    /**
     * Property sheet text support
     */
    public static class PropertiesChangeText extends View.PerformActionText
    {
        /**
         * The LID to post when this event "happens"
         */
        protected ListenerIdentifier lid;

        protected View view;

        public PropertiesChangeText(Composite parent,
                                    int style,
                                    ListenerIdentifier setLid,
                                    View v)
        {
            super(parent, style);

            if (setLid == null) {
                throw new IllegalArgumentException(
                        "Cannot create a WidgetChangeText with a null lid");
            }

            lid = setLid;
            view = v;
        }

        @Override
        public boolean confirm(int focusRule)
        {
            if (! view.isPerformingAction()) {
                return super.confirm(focusRule);
            }
            // otherwise, do nothing!

            return true;
        }

        @Override
        protected boolean doChangeAction()
        {
            return view.performAction(lid);
        }

        @Override
        protected void onFocus()
        {
            super.onFocus();

            view.getTransmuter().setLIDEnabledState();
        }
    }

    /**
     * Subclass this when assigning SWT SelectionListener
     * (for widgetDefaultSelected; i.e., when Enter is pressed) and
     * FocusListener (for focusLost) to Text instances that should
     * perform a Controller action as a result.
     */
    public static class PerformActionDouble extends DoubleEntry
    {
        /**
         * Prevent recursive calls (annoyingly enough)
         *
         * Don't handle events for changes to properties
         * when another event is active
         */
        protected boolean notActive = true;

        public PerformActionDouble(Composite parent, int style)
        {
            super(parent, style);
        }

        @Override
        public boolean confirm(int focusRule)
        {
            if (isEnabled()) {
                return performChangeAction();
            }

            return true;
        }

        public boolean performChangeAction()
        {
            boolean performed = true;

            if (notActive) {
                try {
                    notActive = false;
                    performed = doChangeAction();
                }
                finally {
                    notActive = true;
                }
            }

            return performed;
        }

        protected boolean doChangeAction()
        {
            // OVERRIDE this method
            return true;
        }
    }

    /**
     * Subclass this when assigning SWT SelectionListener
     * (for widgetDefaultSelected; i.e., when Enter is pressed) and
     * FocusListener (for focusLost) to Text instances that should
     * perform a Controller action as a result.
     */
    public static class PerformActionText extends ManagedText
    {
        /**
         * Prevent recursive calls (annoyingly enough)
         *
         * Don't handle events for changes to properties
         * when another event is active
         */
        protected boolean notActive = true;

        public PerformActionText(Composite parent, int style)
        {
            // TODO: Technically, View is generic enough that we should
            //       *not* be citing CogToolPrefs here but instead
            //       requiring the caller to specify the value as a parameter.
            super(parent, style, Keypad.FULL_KEYPAD);
        }

        @Override
        public boolean confirm(int focusRule)
        {
            if (isEnabled()) {
                return performChangeAction();
            }

            return true;
        }

        public boolean performChangeAction()
        {
            boolean performed = true;

            if (notActive) {
                try {
                    notActive = false;
                    performed = doChangeAction();
                }
                finally {
                    notActive = true;
                }
            }

            return performed;
        }

        protected boolean doChangeAction()
        {
            // OVERRIDE this method
            return true;
        }
    }

    /**
     * Subclass this when assigning SWT SelectionListener
     * (for widgetDefaultSelected; i.e., when Enter is pressed) and
     * FocusListener (for focusLost) to Combo instances that should
     * perform a Controller action as a result.
     */
    public static class PerformActionCombo extends ManagedCombo
    {
        /**
         * Prevent recursive calls (annoyingly enough)
         *
         * Don't handle events for changes to properties
         * when another event is active
         */
        protected boolean notActive = true;

        public PerformActionCombo(Composite parent, int style)
        {
            super(parent, style);
        }

        @Override
        public boolean confirm(int focusRule)
        {
            if (isEnabled()) {
                return performChangeAction();
            }

            return true;
        }

        public boolean performChangeAction()
        {
            boolean performed = true;

            if (notActive) {
                try {
                    notActive = false;
                    performed = doChangeAction();
                }
                finally {
                    notActive = true;
                }
            }

            return performed;
        }

        protected boolean doChangeAction()
        {
            // OVERRIDE this method
            return true;
        }
    }

    protected Listener selectionListener =
        new Listener() {

            public void handleEvent(Event evt)
            {
                //performAction((ListenerIdentifier) evt.widget.getData());
                ListenerIdentifier id= (ListenerIdentifier) evt.widget.getData();

                if(id instanceof CogToolLID.ConverterFilesLID)
                {
                    MenuItem w = (MenuItem)evt.widget;
                    String wText = w.getText().substring(w.getText().indexOf("from")+5);
                    //System.out.println("widget " + wText);
                    File file = null;
                    String directory = CogToolPref.CONVERTER_DIRECTORY.getString();
                    if(directory != null)
                    {
                        file = new File(directory);
                        URL[] urls = null;
                        try {
                            /**
                             * TODO: fix this deprecated method
                             */
                            URL url= file.toURL();
                            //System.out.println("file   "+ file + " url   " + url);
                            urls = new URL[]{url};
                        }
                        catch (MalformedURLException e1) {
                            // TODO Auto-generated catch block
                            e1.printStackTrace();
                        }

                        //TODO: comment more here
                        URLClassLoader classLoader = new URLClassLoader(urls);

                        String[] children = file.list();
                        for (String resource : children) {
                            try {
                                Class<ImportConverter> translatorClass = (Class<ImportConverter>) classLoader.loadClass(resource.substring(0, resource.indexOf('.')));
                                try{
                                    Object converter = translatorClass.newInstance();
                                    Class[] parameters = new Class[0];
                                    Method method = translatorClass.getMethod("name", parameters);
                                    String name = (String)method.invoke(converter);
                                    if(name.equals(wText)){
                                        CogToolLID.ConverterFilesLID.NewDesignFromImport.setClassAttribute(translatorClass);
                                    }
                                }
                                catch( Exception ex){
                                    ex.printStackTrace();
                                }
                            }
                            catch (ClassNotFoundException e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
                performAction(id);
            }

        };

    /**
     * Common constant for indicating that context menu is for currently
     * selected objects.
     */
    public static final boolean SELECTION = false;

    /**
     * Common constant for indicating that context menu is for transiently
     * selected object (menu-click on unselected entity).
     */
    public static final boolean CONTEXT = true;

    /**
     * Initializes the shared implementation for this View instance.
     * <p>
     * It stores the SWT window instance and constructs the main menu bar
     * for the window.  The construction of the main menu bar proceeds
     * according to the UI conventions of the executing platform.
     * <p>
     * The <code>ListenerIdentifierMap</code> instance is provided by
     * the invoking <code>UIModel</code> and the <code>Shell</code>
     * is provided by the subclass.
     *
     * @param windowShell    the SWT instance representing the window
     * @param listenerIDMap  used to map <code>ListenerIdentifier</code> values
     *                       to application-specific, semantic code snippets
     * @param lIDtransformer used to convert <code>ListenerIdentifier</code>
     *                       instances to more concrete values
     * @author mlh
     * @see MenuFactory.buildMenu
     */
    public View(Shell windowShell,
                 ListenerIdentifierMap listenerIDMap,
                 ILIDTransmuter lIDtransformer,
                 MenuFactory.IWindowMenuData<?> menuData)
    {
        shell = windowShell;
        lIDMap = listenerIDMap;
        transmuter = lIDtransformer;

        windowMenuData = menuData;

        // Set an icon for the window control box and Windows taskbar
        if (OSUtils.WINDOWS) {
            // Load the image data statically and keep it around
            if (images == null) {
                ImageLoader loader = new ImageLoader();
                ImageData[] imageData =
                    loader.load(ClassLoader.getSystemResourceAsStream
                            ("edu/cmu/cs/hcii/cogtool/resources/CogTool_Icon.png"));

                images = new Image[imageData.length];
                for (int i = 0; i < imageData.length; i++) {
                    images[i] = new Image(WindowUtil.GLOBAL_DISPLAY, imageData[i]);
                }
            }
            shell.setImages(images);
        }

        menuData.setView(this);

        buildMenus();

        // Add a listener to the close window event to "attempt" to capture changes
        // as the window closes.. unfortunately may cause a crash on quit,
        // (mlh: haven't seen this!) or close window when the change conflicts.
        // and on quit if it doesn't crash, it doesn't ask the user to save the change
        shell.addShellListener(new ShellAdapter() {
            /**
             * Change focus when shell closed to trigger appropriate events.
             */
            @Override
            public void shellClosed(ShellEvent evt)
            {
                if (! evt.widget.isDisposed()) {
                    ((Shell) evt.widget).setFocus();
                }

                super.shellClosed(evt);
            }

        });
    }

    public ListenerIdentifierMap getLIDMap()
    {
        return lIDMap;
    }

    protected void buildMenus()
    {
        MenuItemDefinition[][] defs = getContextMenuDefinitions();

        if (defs != null) {
            if (contextMenus != null) {
                contextMenus.dispose();
            }

            contextMenus = new ContextMenuManager(shell,
                                                       lIDMap,
                                                       transmuter,
                                                       defs);
        }

        MenuFactory.buildMenu(neededMenus(),
                              shell,
                              selectionListener,
                              lIDMap,
                              windowMenuData);
    }

    /**
     * Show a dynamic menu for contextual click with the list of menu items.
     */

    public void showDynamicMenu(List<MenuItemDefinition> dynamicMenuItems)
    {
        contextMenus.createDynamicMenu(dynamicMenuItems).setVisible(true);
    }


    public void rebuildMenus()
    {
        shell.getMenuBar().dispose();
        buildMenus();
    }

    /**
     * Wrapper method for invoking performAction; uses the cached transmuter
     * and (!) assumes NOT caused by a context menu.  ContextMenuManager
     * is currently the only place that invokes performAction as "context".
     *
     * @param id the general semantic LID to lookup and perform action for
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     */
    public boolean performAction(ListenerIdentifier id)
    {
        return transmuter.performAction(id);
    }

    /**
     * Wrapper method for invoking performAction; uses the cached transmuter
     * and (!) assumes NOT caused by a context menu.  ContextMenuManager
     * is currently the only place that invokes performAction as "context".
     *
     * @param id the general semantic LID to lookup and perform action for
     * @return <code>true</code> if it is ok to continue with action processing
     *         and <code>false</code> if processing should stop.
     */
    public boolean performAction(ListenerIdentifier id,
                                 Object actionParms,
                                 boolean doCleanup)
    {
        return transmuter.performAction(id, actionParms, doCleanup);
    }

    /**
     * This method provides the top-level pull-downs that this View
     * requires in the main menu bar.
     * <p>
     * Different platforms (specifically, Windows vs. Macintosh)
     * have different conventions for presenting the choices for the
     * main menu bar when the application has multiple, different windows.
     * Windows tends to present only those needed by the specific window
     * while Macintosh presents the union of all choices and simply disables
     * the choices that are unavailable in the currently active window.
     *
     * @returns the sequence of top-level pull-downs needed by this View
     * @author mlh
     */
    protected abstract MenuFactory.MenuType[] neededMenus();

    /**
     * Fetch the Shell being used to present the view's window.
     *
     * @return the SWT Shell instance for the view's window
     * @author mlh
     */

    public Shell getShell()
    {
        return shell;
    }


    public ILIDTransmuter getTransmuter()
    {
        return transmuter;
    }

    /**
     * Set the window's title to the given string.
     *
     * @param newTitle the new title for the associated window
     * @author mlh
     */

    public void setWindowTitle(String newTitle)
    {
        Shell window = getShell();

        if (window != null) {
            window.setText(newTitle);
        }
        else {
            throw new RcvrUIException("No window available for setting window title.");
        }
    }

    /**
     * Set whether the associated window (and view) is visible.
     *
     * @param visible true iff the associated window/view should be visible
     * @author mlh
     */

    public void setVisible(boolean visible)
    {
        Shell window = getShell();

        if (window != null) {
            if (visible) {
                // Suppress drawing until everything is done.
//                setDrawingOK(false);

                WindowUtil.display(window, false);

                // Recompute layout of the shell contents.
                // Should not be called too often, but since SetVisible should
                // only be called on window creation it should be done here.
                window.layout(true, true);

//                setDrawingOK(true);
            }
            else {
                window.setVisible(false);
            }
        }
        else {
            throw new RcvrUIException("No window available for setting window visibility.");
        }
    }

    /**
     * Bring the associated window to the front and make active by
     * making the window have the focus and "un-minimize" it.
     *
     * @author mlh
     */

    public void takeFocus()
    {
        Shell window = getShell();

        if (window != null) {
            window.setActive();
            window.setMinimized(false);
        }
        else {
            throw new RcvrUIException("No window available for taking the focus.");
        }
    }

    /**
     * Request that the associated window be closed; it is possible for some
     * interaction with the user to occur which might "cancel" the operation.
     *
     * @return true if and only if the close was successful.
     */

    public boolean requestClose()
    {
        Shell window = getShell();

        if (window != null) {
            // Ask the shell to close (this will prompt user for save).
            window.close();

            // If the shell didn't close, the exit command was cancelled.
            return window.isDisposed();
        }

        throw new RcvrUIException("No window available to close.");
    }

    /**
     * Recover any system resources being used to support this window/view.
     *
     * @author mlh
     */

    public void dispose()
    {
        Shell window = getShell();

        if (window != null) {
            if (contextMenus != null) {
                contextMenus.dispose();
                contextMenus = null;
            }

            if (! window.isDisposed()) {
                window.setVisible(false);
                window.close();
                window.dispose();
            }
        }
        else {
            throw new RcvrUIException("No window available for recovering system resources.");
        }
    }

    /**
     * Indicate whether or not the associated window has been closed.
     *
     * @return true if the associated window has been closed; false otherwise
     */

    public boolean isDisposed()
    {
        Shell window = getShell();

        if (window != null) {
            return window.isDisposed();
        }
        else {
            throw new RcvrUIException("No window available for testing if closed.");
        }
    }

    /**
     * Returns the set of context menus used by the view
     * @return the context menus
     */

    public MenuItemDefinition[][] getContextMenuDefinitions()
    {
        // By default, we have no context menus
        return null;
    }

    /**
     * Accessor for the drawing block flag.
     */
    public static boolean isDrawingOK()
    {
        return true;
//        return drawingFlags.get().booleanValue();
    }

    /**
     * Mutator for the drawing block flag -- set true when wasteful drawing is
     * about to be triggered indirectly.
     */
    public static void setDrawingOK(boolean newVal)
    {
        drawingFlags.set(newVal ? Boolean.TRUE : Boolean.FALSE);
    }

    /**
     * Support for creating the view's window.
     * <p>
     * This is called by createShell(Rectangle) in most of our subclasses,
     * but needs to be a static method in each of them, hence we can't use
     * overriding, and instead have this as a helper to be called explicitly.
     *
     * @param bounds if null, use a default position, and otherwise use this as
     *               position and size of the window
     * @param width  if bounds is null, use this as the default width; otherwise
     *               the value of <code>width</code> is ignored
     * @param height if bounds is null, use this as the default height; otherwise
     *               the value of <code>height</code> is ignored
     * @param layout the Layout used by the window
     * @return       the new window
     */
    protected static Shell createShell(Rectangle bounds,
                                       int width,
                                       int height,
                                       Layout layout)
    {
        return WindowUtil.createNormalWindow(WindowUtil.GLOBAL_DISPLAY,
                                             "",
                                             ((bounds != null)
                                                     ? bounds
                                                     : new Rectangle(0,
                                                                     0,
                                                                     width,
                                                                     height)),
                                             (bounds != null),
                                             layout);
    }

    /**
     * Some reactions should not happen if the application is in the
     * middle of performing another action.  Use this to detect.
     */

    public boolean isPerformingAction()
    {
        return performingAction;
    }

    /**
     * Indicate whether or not the application is in the midst of performing
     * another action.
     */

    public void setPerformingAction(boolean performing)
    {
        performingAction = performing;
    }


    public void setStatusMessage(String message)
    {
        // By default, no place to set status message
    }


    public void setStatusMessage(String message, int duration)
    {
        // By default, no place to set status message
    }
}
