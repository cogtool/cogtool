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

package edu.cmu.cs.hcii.cogtool;

//import java.io.FileOutputStream;
//import java.io.PrintWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import edu.cmu.cs.hcii.cogtool.model.AUndertaking;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.FrameElement;
import edu.cmu.cs.hcii.cogtool.model.FrameElementGroup;
import edu.cmu.cs.hcii.cogtool.model.Association;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.IWidget;
import edu.cmu.cs.hcii.cogtool.model.SimpleWidgetGroup;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectPersist;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

public class CogToolClipboard
{
    /*
     * Various "purposes" for serialization.
     */
    public static final Object FilePersistence = Project.FILE_PERSISTENCE;

    public static final String CopyTasks = AUndertaking.COPY_TASK_ONLY;
    public static final String CopyDesigns = Design.COPY_ENTIRE_DESIGN;
    public static final String CopyFrames = Frame.COPY_FRAME_ONLY;
    public static final String CopyWidgets =
        TransitionSource.COPY_TRANSITION_SOURCE_ONLY;

    protected static class CogToolTransfer extends ByteArrayTransfer
    {
        /*
         * Use long form name and Transfer.isSupportedType(TransferData) for
         * platform isolation. Apple suggests using reverse-dns convention, but
         * SWT (as of 3.1) uses deprecated "Scrap Manager" APIs that still use
         * 4-char OSTypes. If we want to support x-application data transfer, we
         * should switch to an OSType compatible name, e.g. "CgTl".
         */
//        private static final String COGTOOL_NAME =
//            "edu.cmu.cs.hcii.cogtool.CogTool";
        private static final String COGTOOL_NAME = "CgTl";
        private static final int COGTOOL_ID = registerType(COGTOOL_NAME);

        private static CogToolTransfer ONLY = new CogToolTransfer();

        private CogToolTransfer() { }

        public static CogToolTransfer getInstance()
        {
            return ONLY;
        }

        @Override
        public void javaToNative(Object object, TransferData transferData)
        {
            if ((object == null) || ! (object instanceof String)) {
                return;
            }

            if (isSupportedType(transferData)) {
                String xml = (String) object;

                // write data to a byte array and then ask super to convert
                ByteArrayOutputStream out =
                    new ByteArrayOutputStream(xml.length() * 2);
                try {
                    PrintStream printer = new PrintStream(out, false, "UTF-8");

                    printer.print(xml);
                }
                catch (UnsupportedEncodingException e) {
                    // ignored
                }

                super.javaToNative(out.toByteArray(), transferData);
            }
            else {
                System.err.println("Not isSupportedType()!" + object);
            }
        }

        @Override
        public Object nativeToJava(TransferData transferData)
        {
            if (isSupportedType(transferData)) {
                byte[] buffer = (byte[]) super.nativeToJava(transferData);

                if (buffer != null) {
                    ByteArrayInputStream in =
                        new ByteArrayInputStream(buffer);
                    try {
                        InputStreamReader reader =
                            new InputStreamReader(in, "UTF-8");
                        StringBuilder s = new StringBuilder();
                        int c;

                        while ((c = reader.read()) != -1) {
                            s.append((char) c);
                        }

                        return s.toString();
                    }
                    catch (UnsupportedEncodingException e) {
                        // ignored
                    }
                    catch (IOException e) {
                        throw new ClipboardUtil.ClipboardException("Fetch problem", e);
                    }
                }

                System.err.println("Nothing to paste!");
            }

            return null;
        }

        @Override
        protected int[] getTypeIds()
        {
            return new int[] { COGTOOL_ID };
        }

        @Override
        protected String[] getTypeNames()
        {
            return new String[] { COGTOOL_NAME };
        }
    }

    protected static Transfer[] cogtoolClipboardTypes =
        new Transfer[] { CogToolTransfer.getInstance() };

    /**
     * No actual object version is negative; we can use -1 to
     * indicate a version saved only for the clipboard.
     */
    protected static final int CLIPBOARD_VERSION = -1;

    /**
     * Object to represent which Project the objects were 'copied' from.
     */
    public static class ProjectScope
    {
        protected Project projectScope;

        protected static final String scopeVAR = "scope";

        protected static ObjectSaver.IDataSaver<ProjectScope> SAVER =
            new ObjectSaver.ADataSaver<ProjectScope>() {
                @Override
                public int getVersion()
                {
                    return CLIPBOARD_VERSION;
                }

                @Override
                public void saveData(ProjectScope v, ObjectSaver saver)
                    throws IOException
                {
                    saver.saveString(v.projectScope.getUUID(),
                                     scopeVAR);
                }
            };

        public ProjectScope(Project scope)
        {
            projectScope = scope;
        }

        public Project getProject()
        {
            return projectScope;
        }

        public void clearProject()
        {
            projectScope = null;
        }
    }

    public static final boolean SAVE_TO_CLIPBOARD = true;
    public static final boolean SAVE_STRING_ONLY = false;

    public static class ClipboardClassSaver extends ObjectSaver
    {
        protected boolean saveToClipboard;

        public ClipboardClassSaver(Object saverPurpose, boolean toClipboard)
            throws IOException
        {
            this(saverPurpose, null, toClipboard);
        }

        public ClipboardClassSaver(Object saverPurpose,
                                   ISaverRegistry registry,
                                   boolean toClipboard)
            throws IOException
        {
            super(new StringWriter(), saverPurpose, registry);

            saveToClipboard = toClipboard;
        }

        @Override
        public void finish()
            throws IOException
        {
            super.finish();

            if (saveToClipboard) {
                ClipboardUtil.copyDataToClipboard(sink.toString(),
                                                  cogtoolClipboardTypes);
            }
        }

        public String getSavedString()
        {
            return sink.toString();
        }
    }

    /**
     * When 'copying' an Design to the clipboard, save the Tasks associated
     * with the TaskApplication instances by their full path names.
     * When 'pasting' these ITaskApplications, if the project is the same,
     * the names will be used to look up instances; if not found or the project
     * is not the same, the leaf name will be used to create a new Task.
     */
    protected static class DesignClipboardSaverRegistry
                                    extends ObjectSaver.OverrideSaverRegistry
    {
        public DesignClipboardSaverRegistry()
        {
            super(ObjectSaver.DEFAULT_REGISTRY);

            registerSaver(ProjectScope.class.getName(), ProjectScope.SAVER);
        }
    }

    /**
     * Since there is no state, only one of these is required.
     */
    protected static ObjectSaver.ISaverRegistry designClipboardSaverRegistry =
        new DesignClipboardSaverRegistry();

    /**
     * When 'pasting' an Design, if an Project scope is given (i.e., not
     * </code>null</code>), then look up Task instances instead of creating
     * new ones.  Create a new one if, for some reason, the look-up fails
     * (e.g., the task was deleted between the 'copy' and the 'paste').
     */
    protected static class ClipboardLoaderRegistry
                                    extends ObjectLoader.OverrideLoaderRegistry
    {
        protected ProjectScope taskScope;

        protected void checkTaskScope(String uuid)
        {
            Project taskScopeProject = taskScope.getProject();

            if ((taskScopeProject == null) ||
                ! taskScopeProject.getUUID().equals(uuid))
            {
                taskScope.clearProject();
            }
        }

        private ObjectLoader.IObjectLoader<ProjectScope> ProjectScope_Loader =
            new ObjectLoader.AObjectLoader<ProjectScope>() {
                @Override
                public ProjectScope createObject()
                {
                    return taskScope;
                }

                @Override
                public void set(ProjectScope target, String variable, Object value)
                {
                    if (variable != null) {
                        if (variable.equals(ProjectScope.scopeVAR)) {
                            checkTaskScope((String) value);
                        }
                    }
                }
            };

        public ClipboardLoaderRegistry(Project scope)
        {
            super(ObjectLoader.DEFAULT_REGISTRY);

            taskScope = new ProjectScope(scope);

            registerLoader(ProjectScope.class.getName(),
                           CLIPBOARD_VERSION,
                           ProjectScope_Loader);
        }
    }

    protected static class SelectedDesignSaver extends ClipboardClassSaver
    {
        public SelectedDesignSaver(boolean toClipboard)
            throws IOException
        {
            super(CogToolClipboard.CopyDesigns,
                  designClipboardSaverRegistry,
                  toClipboard);
        }
    }

    protected static class SelectedFrameSaver extends ClipboardClassSaver
    {
        protected Set<Frame> selectedFrames = new HashSet<Frame>();
        protected Set<Transition> safeTransitions = new HashSet<Transition>();

        public SelectedFrameSaver(Object saverPurpose,
                                  Frame[] selection,
                                  boolean toClipboard)
            throws java.io.IOException
        {
            super(saverPurpose, toClipboard);

            if (selection != null) {
                for (Frame element : selection) {
                    selectedFrames.add(element);
                }
            }
        }

        // ATransitionSource ignores the return value.
        @Override
        public Object filterObject(Object value)
            throws java.io.IOException
        {
            if (value instanceof Transition) {
                if (! safeTransitions.contains(value)) {
                    Transition t = (Transition) value;

                    if (selectedFrames.contains(t.getDestination()) &&
                        selectedFrames.contains(t.getSource().getFrame()))
                    {
                        safeTransitions.add(t);
                    }
                }

                return null;
            }

            return super.filterObject(value);
        }

        @Override
        public void finish()
            throws java.io.IOException
        {
            Iterator<Transition> transitions = safeTransitions.iterator();

            while (transitions.hasNext()) {
                Transition t = transitions.next();

                saveObject(t);
            }

            super.finish();
        }
    }

    protected static class SelectedElementSaver extends ClipboardClassSaver
    {
        protected Set<FrameElement> selectedElements =
            new HashSet<FrameElement>();
        protected Map<SimpleWidgetGroup, SimpleWidgetGroup> twinnedGroups =
        	new HashMap<SimpleWidgetGroup, SimpleWidgetGroup>();

        public SelectedElementSaver(Object saverPurpose,
                                    FrameElement[] selection,
                                    boolean toClipboard)
            throws java.io.IOException
        {
            super(saverPurpose, toClipboard);

            if (selection != null) {
                selectedElements.addAll(Arrays.asList(selection));
            }
        }

        /**
         * Indicates whether the given remote label owner is itself selected.
         */
        protected boolean isRemoteLabelOwnerSelected(FrameElement elt)
        {
            // If a parent widget group, then a member must be selected
            if (elt instanceof SimpleWidgetGroup) {
                Iterator<?> members = ((SimpleWidgetGroup) elt).iterator();

                while (members.hasNext()) {
                    if (selectedElements.contains(members.next())) {
                        return true;
                    }
                }
            }
            else {
                // Otherwise, the element is either an IWidget or
                // an FrameElementGroup, which must be selected
                return selectedElements.contains(elt);
            }

            return false;
        }

        /**
         * If a remote label widget whose owner element is not part of
         * the selection, then temporarily remove the REMOTE_LABEL_OWNER_ATTR
         * attribute while saving.
         */
        @Override
        public void saveObject(Object value)
            throws java.io.IOException
        {
            IWidget asRemoteLabel = null;   // null indicates not a remote label
            FrameElement remoteLabelOwner = null;

            if (value instanceof IWidget) {
                asRemoteLabel = (IWidget) value; // while we check for attr

                remoteLabelOwner =
                    (FrameElement)
                        asRemoteLabel.getAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);

                // If not a remote label or the owner is selected, unset
                if ((remoteLabelOwner == null) ||
                    isRemoteLabelOwnerSelected(remoteLabelOwner))
                {
                    asRemoteLabel = null;
                }
                else {
                    // Otherwise, temporarily unset
                    asRemoteLabel.unsetAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR);
                }
            }

            // Save the object
            super.saveObject(value);

            // Re-establish the owner attribute if necessary
            if (asRemoteLabel != null) {
                asRemoteLabel.setAttribute(WidgetAttributes.REMOTE_LABEL_OWNER_ATTR,
                                           remoteLabelOwner);
            }
        }

        protected boolean isMemberOfSelectedGroup(FrameElement elt)
        {
            FrameElement rootElt = elt.getRootElement();
            Iterator<FrameElementGroup> eltGroups =
                rootElt.getEltGroups().iterator();

            // If a selected item is a member of an FrameElementGroup
            // that is also selected, skip it
            while (eltGroups.hasNext()) {
                if (selectedElements.contains(eltGroups.next())) {
                    return true;
                }
            }

            return false;
        }

        // During copying to the clipboard, anonymous parent groups that are
        // not themselves copied as part of selected element groups must be
        // twinned for:
        //     MenuHeader, GridButton, and IListBoxItem
        @Override
        public Object filterObject(Object value)
            throws java.io.IOException
        {
            if (value instanceof SimpleWidgetGroup) {
                SimpleWidgetGroup valueGroup = (SimpleWidgetGroup) value;

                if (! isMemberOfSelectedGroup(valueGroup)) {
                    SimpleWidgetGroup replacementGroup =
                        twinnedGroups.get(valueGroup);

                    if (replacementGroup == null) {
                        replacementGroup = valueGroup.twin();

                        twinnedGroups.put(valueGroup, replacementGroup);

                        Iterator<IWidget> groupWidgets =
                            valueGroup.iterator();

                        while (groupWidgets.hasNext()) {
                            IWidget widget = groupWidgets.next();

                            if (selectedElements.contains(widget)) {
                                replacementGroup.simpleAddWidget(widget);
                            }
                        }
                    }

                    return replacementGroup;
                }
            }
            else if (value instanceof Set<?>) {
                Set<Association<?>> replacement = null;
                Iterator<?> elements = ((Set<?>) value).iterator();

                while (elements.hasNext()) {
                    Object element = elements.next();

                    if (element instanceof FrameElementGroup) {
                        FrameElementGroup group = (FrameElementGroup) element;

                        if (replacement == null) {
                            replacement = new HashSet<Association<?>>();
                        }

                        if (selectedElements.contains(group)) {
                            replacement.add(group);
                        }
                    }
                }

                if (replacement != null) {
                    return replacement;
                }
            }

            return super.filterObject(value);
        }
    }

    // If toClipboard, finish() will save results to system clipboard
    public static ClipboardClassSaver startClipboardSave(Object purpose,
                                                         boolean toClipboard)
        throws IOException
    {
        return new ClipboardClassSaver(purpose, toClipboard);
    }

    public static ClipboardClassSaver startClipboardDesignSave(Project scope,
                                                               boolean toClipboard)
        throws IOException
    {
        ClipboardClassSaver saver = new SelectedDesignSaver(toClipboard);

        saver.saveObject(new ProjectScope(scope));

        return saver;
    }

    public static ClipboardClassSaver startClipboardSave(Object purpose,
                                                         Frame[] selection,
                                                         boolean toClipboard)
        throws IOException
    {
        return new SelectedFrameSaver(purpose, selection, toClipboard);
    }

    public static ClipboardClassSaver startClipboardSave(Object purpose,
                                                         FrameElement[] seln,
                                                         boolean toClipboard)
        throws IOException
    {
        return new SelectedElementSaver(purpose, seln, toClipboard);
    }

    /**
     * Checks the system clipboard for CogTool model objects.
     * @return true if the clipboard contains CogTool model objects, else false
     */
    public static boolean hasCogToolObjects()
    {
        return ClipboardUtil.checkTypes(CogToolTransfer.getInstance());
    }

    public static boolean hasNonTextPasteContent()
    {
        return hasCogToolObjects() || ClipboardUtil.hasImageData();
    }

    public static List<Object> loadCogToolObjects(String str, Project scope)
        throws ParserConfigurationException, SAXException, IOException
    {
        if ((str == null) || ! str.startsWith("<" + ObjectPersist.PERSIST_ELT))
        {
            return null;        // ? empty collection
        }

        ObjectLoader l = new ObjectLoader(new ClipboardLoaderRegistry(scope));

        return l.load(new InputSource(new StringReader(str)), null);
    }

    public static List<Object> loadCogToolObjects(String str)
        throws ParserConfigurationException, SAXException, IOException
    {
        return loadCogToolObjects(str, null);
    }

    public static List<Object> fetchCogToolObjects(Project scope)
        throws ParserConfigurationException, SAXException, IOException
    {
        String str =
           (String)
               ClipboardUtil.fetchClipboardData(CogToolTransfer.getInstance());

        return loadCogToolObjects(str, scope);
    }

    public static List<Object> fetchCogToolObjects()
        throws ParserConfigurationException, SAXException, IOException
    {
        return fetchCogToolObjects(null);
    }
}
