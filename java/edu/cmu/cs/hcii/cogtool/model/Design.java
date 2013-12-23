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

import java.io.IOException;
import java.util.Collection;
import java.util.EventObject;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import edu.cmu.cs.hcii.cogtool.util.GlobalAttributed;
import edu.cmu.cs.hcii.cogtool.util.NameChangeAlert;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;

/**
70   * A CogTool Design consists of Frames.  All designs have a display name.
71   *
72   * @author mlh
73   */
public class Design extends GlobalAttributed implements NamedObject
{
    /**
     * Constants for supporting cut/copy clipboard modes;
     * these will ultimately be used for the purpose in the ObjectSaver.
     * NOTE: By convention, null is used to signify normal file persistence.
     */
    public static final String COPY_ENTIRE_DESIGN = "EntireDesign";

    public static final int edu_cmu_cs_hcii_cogtool_model_Design_version = 0;

    public static final String nameVAR = "name";
    public static final String deviceTypesVAR = "deviceTypes";
    public static final String framesVAR = "frames";
    public static final String skinVAR = "skin";

    protected String name;
    protected Set<DeviceType> deviceTypes;
    protected Set<Frame> frames = new LinkedHashSet<Frame>();
    protected SkinType skin = SkinType.WireFrame;

    private static ObjectSaver.IDataSaver<Design> SAVER =
        new ObjectSaver.ADataSaver<Design>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_Design_version;
            }

            @Override
            public void saveData(Design v, ObjectSaver saver)
                throws IOException
            {
                saver.saveString(v.name, nameVAR);
                saver.saveObject(v.deviceTypes, deviceTypesVAR);
                saver.saveObject(v.skin, skinVAR);
                saver.saveObject(v.frames, framesVAR);
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(Design.class.getName(), SAVER);
    }

    public static ObjectLoader.IObjectLoader<Design> getImportLoader()
    {
        return LOADER;
    }

    private static ObjectLoader.IObjectLoader<Design> LOADER =
        new ObjectLoader.AObjectLoader<Design>() {
            @Override
            public Design createObject()
            {
                return new Design();
            }

            @Override
        	@SuppressWarnings("unchecked")
            public void set(Design target, String variable, Object value)
            {
                if (variable != null) {
                    if (variable.equals(nameVAR)) {
                        target.name = (String) value;
                    }
                    else if (variable.equals(deviceTypesVAR)) {
                        target.deviceTypes = (Set<DeviceType>) value;
                    }
                    else if (variable.equals(skinVAR)) {
                        SkinType designSkin = (SkinType) value;

                        // Replace default skin if other than None was saved
                        if (designSkin != SkinType.None) {
                            target.skin = designSkin;

                            // Non-beta versions that used a skin used to save
                            // the skin after the frames.  Starting in beta-20,
                            // the skin type is saved before the frames, and
                            // thus the set of frames would be empty here.  In
                            // that case, this code has no effect, which
                            // is desired for beta-20 and beyond since each
                            // widget's rendering is set independently!
                            Iterator<Frame> designFrames = target.getFrames().iterator();

                            // We had removed the renderSkin flag from widgets
                            // between beta-18 and beta-20 (beta-19 is defunct)
                            // so we should set all widgets in the design to
                            // be rendered in this case; see previous note!
                            while (designFrames.hasNext()) {
                                Frame f = designFrames.next();

                                Iterator<IWidget> frameWidgets =
                                    f.getWidgets().iterator();

                                while (frameWidgets.hasNext()) {
                                    IWidget w = frameWidgets.next();

                                    w.setRendered(true);
                                }
                            }
                        }
                    }
                }
            }

            @Override
            public Collection<?> createCollection(Design target,
                                                  String variable,
                                                  int size)
            {
                if (variable != null) {
                    if (variable.equals(deviceTypesVAR)) {
                        target.deviceTypes = new HashSet<DeviceType>();
                        return target.deviceTypes;
                    }
                    else if (variable.equals(framesVAR)) {
                        return target.frames;
                    }
                }

                return null;
            }

            @Override
            public ObjectLoader.IAggregateLoader getLoader(String variable)
            {
                if (variable.equals(framesVAR)) {
                    return new ObjectLoader.AAggregateLoader() {
                        @Override
                        public <T> void addToCollection(ObjectLoader l,
                                                        Collection<? super T> c,
                                                        T v)
                        {
                            super.addToCollection(l, c, v);

                            // Collection is 0, Design is 1
                            Object parent = l.getPendingObject(1);

                            ((Frame) v).setDesign((Design) parent);
                        }
                    };
                }

                return super.getLoader(variable);
            }

            protected void evolveTransitionSources(Iterator<? extends TransitionSource> sources)
            {
                while (sources.hasNext()) {
                    TransitionSource source = sources.next();
                    Collection<Transition> transitions =
                        source.getTransitions().values();
                    int[] indexes = new int[transitions.size()];
                    Iterator<Transition> fixTransitions =
                        transitions.iterator();

                    while (fixTransitions.hasNext()) {
                        Transition t = fixTransitions.next();
                        Iterator<Transition> testTrans =
                            transitions.iterator();

                        if (t.getCurveIndex() == 0) {
                            int newCurveIndex =
                                ATransitionSource.computeCurveIndex(t,
                                                                    testTrans,
                                                                    indexes);
                            t.setCurveIndex(newCurveIndex);
                        }
                    }
                }
            }

            @Override
            public void evolve(Design target)
            {
                Iterator<Frame> frames = target.getFrames().iterator();

                while (frames.hasNext()) {
                    Frame frame = frames.next();

                    evolveTransitionSources(frame.getInputDevices().iterator());
                    evolveTransitionSources(frame.getWidgets().iterator());
                }
            }
        };

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(Design.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_Design_version,
                                    LOADER);
    }

    /**
     * Checks if 2 objects are identical
     */
    static public boolean isIdentical(Design l, Design r)
    {
        return
            l.name.equals(r.name) &&
            l.frames.equals(r.frames) &&
            l.skin.equals(r.skin);
    }

    /**
     * Initialize the design with the given name, an empty frame set, and no skin.
     *
     * @param nm    the name of the design (for display), must not be null or empty
     * @param devTypes the devices this design expects
     * @throws IllegalArgumentException if nm is null or empty
     * @author mlh
     */
    public Design(String nm, Set<DeviceType> devTypes)
    {
        if (nm == null || nm.equals("")) {
            throw new IllegalArgumentException("Design name cannot be null or empty!");
        }

        name = nm;
        deviceTypes = devTypes;
    }

    /**
     * Zero-argument constructor for use during loading.
     */
    protected Design() { }

    /**
     * Fetch the name of the design, used for display.
     *
     * @return    the design's name
     * @author mlh
     */
    public String getName()
    {
        return name;
    }

    /**
     * Change the name of the design, used for display.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>NameChangeAlert</code> instance.
     *
     * @param newName  the new design name, must not be null or empty
     * @throws IllegalArgumentException if newName is null or empty
     * @author mlh
     */
    public void setName(String newName)
    {
        if (newName == null || newName.equals("")) {
            throw new IllegalArgumentException("Design name cannot be null or empty!");
        }

        name = newName;

        raiseAlert(new NameChangeAlert(this));
    }

    /**
     * Fetch the entire set of <code>DeviceType</code>s.
     * <p>
     * Each element in the returned <code>Set</code> is an instance
     * of <code>DeviceType</code>.
     *
     * @return    the design's device types
     * @author mlh
     */
    public Set<DeviceType> getDeviceTypes()
    {
        return deviceTypes;
    }

    /**
     * Compute the default widget type based on the set of device types
     * supporting the design.
     *
     * @return the default widget type for widgets created in this
     *         design's frames
     * @author mlh
     */
    public WidgetType getDefaultWidgetType()
    {
        if (deviceTypes.contains(DeviceType.Mouse) ||
            deviceTypes.contains(DeviceType.Touchscreen))
        {
            return WidgetType.Button;
        }

        return WidgetType.Noninteractive;
    }

    /**
     * Fetch the entire set of frames.
     * <p>
     * Each element in the returned <code>Set</code> is an instance
     * of <code>Frame</code>.
     *
     * @return    the design's frames
     * @author mlh
     */
    public Set<Frame> getFrames()
    {
        return frames;
    }

    /**
     * Fetch the frame of the design of the specified name.
     * <p>
     * A design's frames must have mutually distinct names.
     * <p>
     * If the frame is not found, <code>null</code> is returned.
     *
     * @param frameName  the name of the frame to find
     * @return            the frame of the given name held by the design,
     *                    or <code>null</code> if not found
     * @author mlh
     */
    public Frame getFrame(String frameName)
    {
        Iterator<Frame> frameIterator = frames.iterator();

        while (frameIterator.hasNext()) {
            Frame testFrame = frameIterator.next();

            if ((testFrame.getName()).equals(frameName)) {
                return testFrame;
            }
        }

        return null;
    }

    /**
     * Does this design contain the specified frame
     */
    public boolean containsFrame(Frame frame)
    {
        return frames.contains(frame);
    }

    /**
     * Add the given frame to the end of the design's list of frames.
     * <p>
     * Each implementation must check for frame name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>FrameChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if given frame
     * has the same name as one already held by the design.
     *
     * @param newFrame  the frame to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addFrame(Frame newFrame)
    {
        // Must check for Frame name uniqueness
        if (getFrame(newFrame.getName()) == null) {
            frames.add(newFrame);
            newFrame.setDesign(this);

            raiseAlert(new Design.FrameChange(this, newFrame, true));
        }
        else {
            throw new IllegalArgumentException("Cannot add frame of the same name to a design.");
        }
    }

    /**
     * Add the given frames to the end of the design's list of frames.
     * <p>
     * Each implementation must check for frame name uniqueness.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>FrameChange</code> instance.
     * <p>
     * Throws <code>IllegalArgumentException</code> if any given frame
     * has the same name as one already held by the design.
     *
     * @param newFrames  the frames to add
     * @exception IllegalArgumentException
     * @author mlh
     */
    public void addFrames(Frame[] newFrames)
    {
        int i;

        // Check for name uniqueness
        for (i = 0; i < newFrames.length; i++) {
            if (getFrame(newFrames[i].getName()) != null) {
                throw new IllegalArgumentException("Cannot add frame of the same name to a design.");
            }
        }

        for (i = 0; i < newFrames.length; i++) {
            frames.add(newFrames[i]);
            newFrames[i].setDesign(this);
        }

        raiseAlert(new Design.FrameSetChange(this, newFrames, true));
    }

    /**
     * Find the frame of the given name and, if found, remove from
     * the design's list of frames.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>FrameChange</code> instance.
     *
     * @param frameName  the name of the frame to remove
     * @return           true iff the frame was successfully removed
     * @author mlh
     */
    public boolean removeFrame(String frameName)
    {
        Frame frameToRemove = getFrame(frameName);

        if (frameToRemove != null) {
            return removeFrame(frameToRemove);
        }

        return false;
    }

    /**
     * Remove the given frame from the design's list of frames,
     * if it contains that frame.
     * <p>
     * When done, registered alert handlers are notified with
     * a <code>FrameChange</code> instance.
     *
     * @param frameToRemove  the frame to remove
     * @return               true iff the frame was successfully removed
     * @author mlh
     */
    public boolean removeFrame(Frame frameToRemove)
    {
        if (frames.remove(frameToRemove)) {
            frameToRemove.removeIncidentTransitions();

            raiseAlert(new Design.FrameChange(this, frameToRemove, false));

            return true;
        }

        return false;
    }

    public boolean removeFrames(Frame[] framesToRemove)
    {
        boolean okRemoval = true;

        for (Frame element : framesToRemove) {
            if (frames.remove(element)) {
                element.removeIncidentTransitions();

                raiseAlert(new Design.FrameChange(this, element, false));
            }
            else {
                okRemoval = false;
                break;
            }
        }

        return okRemoval;
    }

    /**
     * Support for managing the duplication of all Frames in a Design.
     * Frames are added immediately after creation.
     */
    protected static class DesignFrameDuplicator
                                             implements Frame.IFrameDuplicator
    {
        protected Design scopeDesign;

        public DesignFrameDuplicator(Design design)
        {
            scopeDesign = design;
        }

        public Frame getOrDuplicate(Frame frameToCopy)
        {
            String frameName = frameToCopy.getName();
            Frame frame = scopeDesign.getFrame(frameName);

            if (frame == null) {
                frame = frameToCopy.duplicate(frameName, this);
            }

            return frame;
        }

        public void recordDuplicateFrame(Frame originalFrame,
                                         Frame frameDuplicate)
        {
            scopeDesign.addFrame(frameDuplicate);
        }
    }

    /**
     * Event to indicate that new input device types have been added
     */
    public static class DeviceTypeChange extends EventObject
    {
        public DeviceTypeChange(Design design)
        {
            super(design);
        }
    }

    /**
     * Semantic change for <code>addFrame</code> and
     * <code>removeFrame</code>.
     * <p>
     * Frames are added in no particular order.  Thus, only one kind
     * of add is allowed.
     *
     * @author mlh
     * @see ListChange
     */
    public static class FrameChange extends ListChange
    {
        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param design    the design that was modified
         * @param frameChg  the frame added or removed
         * @param add       a flag indicating whether the change is an add
         *                  or a remove
         * @author mlh
         */
        public FrameChange(Design design, Frame frameChg, boolean add)
        {
            super(design, frameChg, AT_END, add);
        }
    }

    /**
     * Semantic change for <code>addFrames</code>.
     *
     * @author mlh
     * @see EventObject
     */
    public static class FrameSetChange extends EventObject
    {
        public Frame frames[];
        public boolean isAdd;

        /**
         * Initialize the semantic change representing an add or a remove.
         *
         * @param design    the design that was modified
         * @param frames    the frames added or removed
         * @param add       a flag indicating whether the change is an add
         *                  or a remove
         * @author mlh
         */
        public FrameSetChange(Design design, Frame[] frameChg, boolean add)
        {
            super(design);

            frames = frameChg;
            isAdd = add;
        }
    }

    public static class WidgetAppearanceChange extends EventObject
    {
        public WidgetAppearanceChange(Design design)
        {
            super(design);
        }
    }

    /**
     * Create a "deep" copy of this design.
     * <p>
     * It is the responsibility of the caller to "place" the copy
     * (usually by adding it to an Project).
     *
     * @param newName  the name of the resulting copy
     * @return the design copy
     * @author mlh
     */
    public Design duplicate(String newName)
    {
        Design designCopy = new Design(newName,
                                        new HashSet<DeviceType>(deviceTypes));

        DesignFrameDuplicator duplicator =
            new DesignFrameDuplicator(designCopy);

        // Add deep copies of the contained frames.
        for (Frame childFrameToCopy : frames) {
            duplicator.getOrDuplicate(childFrameToCopy);
        }

        designCopy.copyAttributes(this);

        designCopy.setSkin(skin);

        return designCopy;
    }

    public SkinType getSkin()
    {
        return skin;
    }

    public void setSkin(SkinType newSkin)
    {
        if (newSkin != null) {
            if (skin != newSkin) {
                skin = newSkin;
                raiseAlert(new Design.WidgetAppearanceChange(this));
            }
        }
        else {
            throw new IllegalArgumentException("Skin cannot be null!");
        }
    }

    protected Design.DeviceTypeChange deviceTypesAdded = new Design.DeviceTypeChange(this);

    /**
     * Add given device types to the design's set, returning the original set
     */
    public Set<DeviceType> addDeviceTypes(Set<DeviceType> addDeviceTypes)
    {
        Set<DeviceType> existingTypes =
            new HashSet<DeviceType>(deviceTypes);

        deviceTypes.addAll(addDeviceTypes);

        if (! existingTypes.equals(deviceTypes)) {
            raiseAlert(deviceTypesAdded);
        }

        return existingTypes;
    }
}
