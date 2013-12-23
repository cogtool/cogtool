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

package edu.cmu.cs.hcii.cogtool.controller;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import edu.cmu.cs.hcii.cogtool.CogToolClipboard;
import edu.cmu.cs.hcii.cogtool.CogToolLID;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.CogToolWorkThread;
import edu.cmu.cs.hcii.cogtool.FrameTemplateSupport;
import edu.cmu.cs.hcii.cogtool.model.AAction;
import edu.cmu.cs.hcii.cogtool.model.Design;
import edu.cmu.cs.hcii.cogtool.model.DesignUtil;
import edu.cmu.cs.hcii.cogtool.model.DeviceType;
import edu.cmu.cs.hcii.cogtool.model.DoublePoint;
import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;
import edu.cmu.cs.hcii.cogtool.model.ExportCogToolXML;
import edu.cmu.cs.hcii.cogtool.model.Frame;
import edu.cmu.cs.hcii.cogtool.model.InputDevice;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.model.Project.ITaskDesign;
import edu.cmu.cs.hcii.cogtool.model.SkinType;
import edu.cmu.cs.hcii.cogtool.model.TaskApplication;
import edu.cmu.cs.hcii.cogtool.model.Transition;
import edu.cmu.cs.hcii.cogtool.model.TransitionSource;
import edu.cmu.cs.hcii.cogtool.model.WidgetAttributes;
import edu.cmu.cs.hcii.cogtool.ui.ActionProperties;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorInteraction;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorLID;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.DesignEditorUI;
import edu.cmu.cs.hcii.cogtool.ui.FrameSelectionState;
import edu.cmu.cs.hcii.cogtool.ui.Interaction.ProgressBar;
import edu.cmu.cs.hcii.cogtool.ui.ProjectLID;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.ui.SelectionState;
import edu.cmu.cs.hcii.cogtool.ui.UI;
import edu.cmu.cs.hcii.cogtool.ui.ZoomableUI;
import edu.cmu.cs.hcii.cogtool.util.AListenerAction;
import edu.cmu.cs.hcii.cogtool.util.AUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.ClipboardUtil;
import edu.cmu.cs.hcii.cogtool.util.CompoundUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.FileUtil;
import edu.cmu.cs.hcii.cogtool.util.GraphicsUtil;
import edu.cmu.cs.hcii.cogtool.util.IListenerAction;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEdit;
import edu.cmu.cs.hcii.cogtool.util.IUndoableEditSequence;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.NamedObjectUtil;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.RcvrClipboardException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIOSaveException;
import edu.cmu.cs.hcii.cogtool.util.RcvrIllegalStateException;
import edu.cmu.cs.hcii.cogtool.util.RcvrImageException;
import edu.cmu.cs.hcii.cogtool.util.RcvrOutOfMemoryException;
import edu.cmu.cs.hcii.cogtool.util.RcvrUIException;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.ThreadManager;
import edu.cmu.cs.hcii.cogtool.util.UndoManager;

/**
 * The Controller for semantic actions specific to the Project editor window.
 * <p>
 * Semantic actions and parameters:
 *      Undo                <no parameters>
 *      Redo                <no parameters>
 *      SelectAll           <no parameters>       (selects all frames)
 *      DeselectAll         ISelectionState       (objects to be deselected)
 *
 *      Paste               <no parameters>
 *      CopyFrame           IFrameSelectionState  (what to copy)
 *      CutFrame            IFrameSelectionState  (what to cut)
 *
 *      ExportDesignToHTML  <no parameters>
 *      SkinNone/SkinWireFrame/SkinMacOSX/SkinWinXP/SkinPalm  <no parameters>
 *
 *      AddDesignDevices    <no parameters>
 *      NewFrame            <no parameters>
 *      DuplicateFrame      IFrameSelectionState  (what to duplicate)
 *      InitiateFrameRename IFrameSelectionState  (what to rename)
 *      RenameFrame         FrameRenameParameters (what to rename, new name)
 *      EditFrame           IFrameSelectionState  (what to edit)
 *      DeleteFrame         IFrameSelectionState  (what to delete)
 *      MoveFrames          MoveParameters        (what to move, by how much)
 *      NudgeLeft/Right/Up/Down IFrameSelectionState (what to move)
 *      SetBackgroundImage      IFrameSelectionState (which frames to set bkg)
 *      RemoveBackgroundImage   IFrameSelectionState (which frames to set bkg)
 *      SetWidgetColor          IFrameSelectionState (frames to set color)
 *      ImportImageDirectory    <no parameters>
 *
 *      NewTransition       NewTransitionParameters (src, frame, new frame loc)
 *      EditTransition      DesignEditorSelectionState (what to edit)
 *      DeleteTransition    DesignEditorSelectionState (what to delete)
 *      ChangeTarget        ChangeTargetParameters  (transition, new target)
 *      ChangeSource        ChangeSourceParameters  (transition, new source)
 *      ChangeWidgetAction  ChangeActionParameters  (transitions, action prms)
 *      ChangeDeviceAction  ChangeActionParameters  (transitions, action prms)
 */
public class DesignEditorController extends ZoomableController
{
	protected static final String DEFAULT_FRAME_PREFIX =
		L10N.get("DE.FrameNamePrefix", "Frame");

	public static final int INITIAL_FRAME_SUFFIX = 1;
	public static final String INITIAL_FRAME_NAME =
		DEFAULT_FRAME_PREFIX + " " + INITIAL_FRAME_SUFFIX;
	protected static final String moveFrames =
		L10N.get("UNDO.DE.MoveFrames", "Move Frames");
	protected static final String moveFrame =
		L10N.get("UNDO.DE.MoveFrame", "Move Frame");
	protected static final String renameFrame =
		L10N.get("UNDO.DE.RenameFrame", "Rename Frame");
	protected static final String renameDesign =
		L10N.get("UNDO.DE.RenameFrame", "Rename Design");
	protected static final String deleteFrames =
		L10N.get("UNDO.DE.DeleteFrames", "Delete Frames");
	protected static final String deleteFrame =
		L10N.get("UNDO.DE.DeleteFrame", "Delete Frame");
	protected static final String deleteTransitions =
		L10N.get("UNDO.DE.DeleteTransitions", "Delete Transitions");
	protected static final String deleteTransition =
		L10N.get("UNDO.DE.DeleteTransition", "Delete Transition");

	protected static final String selectWidgetColor =
		L10N.get("DESIGN.WIDGET.selectColor", "Select Widget Color");

	protected static final String changeTransitionSource =
		L10N.get("UNDO.DE.ChangeSource", "Change Transition Source");

	protected static final String changeAction =
		L10N.get("UNDO.DE.ChangeAction", "Change Action");

	protected static final String duplicateFrames =
		L10N.get("UNDO.DE.DuplicateFrames", "Duplicate Frames");

	protected static final String duplicateFrame =
		L10N.get("UNDO.DE.DuplicateFrame", "Duplicate Frame");

	protected static final String nothingPasted =
		L10N.get("DE.NothingPasted", "Nothing pasted");
	protected static final String pasteComplete =
		L10N.get("DE.PasteComplete", "object(s) pasted");
	protected static final String removeBackgroundImage =
		L10N.get("UNDO.FE.RemoveBackgroundImage",
		"Remove Frame Background Image");
	protected static final String setBackgroundImage =
		L10N.get("UNDO.FE.SetBackgroundImage", "Set Frame Background Image");

	protected static final String setWidgetColor =
		L10N.get("UNDO.FE.SetWidgetColor", "Set Widget Color");

	protected static final String importBackgroundImages =
		L10N.get("UNDO.DE.ImportBackgroundImages", "Import Background Images");

	protected static final String changeSkin =
		L10N.get("UNDO.DE.ChangeSkin", "Change Skin");

	protected static final String changeDelay =
		L10N.get("UNDO.DE.ChangeDelay", "Change Transition Delay");

	protected static final Pattern IMAGE_FILE_NAME =
		Pattern.compile("(.+)\\.(jpe?g|gif|png)", Pattern.CASE_INSENSITIVE);

	protected static final String FRAME_COPIED =
		L10N.get("DE.FrameCopied", "Frame copied to the clipboard");

	protected static final String FRAMES_COPIED =
		L10N.get("DE.FramesCopied", "Frames copied to the clipboard");

	protected Design design;

	protected DesignEditorUI ui;
	protected DemoStateManager demoStateMgr;
	protected DesignEditorInteraction interaction;

	protected int nextNewFrameSuffix = INITIAL_FRAME_SUFFIX;

	/**
	 * The local properties variable. Used to prevent instantiating extra
	 * action properties on the fly. Set the value to UNSET, since the
	 * which tab to use is always "fetched" before it is used.
	 */
	protected ActionProperties properties =
		new ActionProperties(ActionProperties.UNSET);

	protected class FrameAlignmentAction extends AlignmentAction
	{
		public FrameAlignmentAction(int alignAction)
		{
			super(alignAction);
		}

		/**
		 * Set the expected parameter class.
		 * The parameter is a map from each frame that is selected to the
		 * bounds of its figure (including the name label and device box).
		 */

		public Class<?> getParameterClass()
		{
			return Map.class;
		}


		@SuppressWarnings("unchecked")
		public boolean performAction(Object actionParms)
		{
			Map<Frame, DoubleRectangle> frameMap =
				(Map<Frame, DoubleRectangle>) actionParms;

			CompoundUndoableEdit editSequence =
				new CompoundUndoableEdit(getAlignmentName(), LIDS[action]);

			// Reset for new computation
			reset();

			// Get the list of widgets to align
			Iterator<DoubleRectangle> values = frameMap.values().iterator();

			// Determine the reference widget
			while (values.hasNext()) {
				DoubleRectangle r = values.next();

				// Get reference dimension we are aligning on for comparison
				computeReference(r);
			}

			Iterator<Map.Entry<Frame, DoubleRectangle>> entries =
				frameMap.entrySet().iterator();

			while (entries.hasNext()) {
				Map.Entry<Frame, DoubleRectangle> entry = entries.next();

				final Frame f = entry.getKey();
				// Get the bounds
				DoubleRectangle bounds = entry.getValue();

				final double oldX = bounds.x;
				final double oldY = bounds.y;

				// Compute where the bounds should be based on the reference
				computeNewOrigin(bounds);

				final double new_X = newX;
				final double new_Y = newY;

				// Move the widget to the new location
				f.setFrameOrigin(new_X, new_Y);

				IUndoableEdit edit =
					new AUndoableEdit(LIDS[action])
				{
					@Override
					public String getPresentationName()
					{
						return getAlignmentName();
					}

					@Override
					public void redo()
					{
						super.redo();

						// Move
						f.setFrameOrigin(new_X, new_Y);
					}

					@Override
					public void undo()
					{
						super.undo();

						// move to old location
						f.setFrameOrigin(oldX, oldY);
					}
				};

				editSequence.addEdit(edit);
			}

			editSequence.end();

			// Only add this edit if it is significant
			if (editSequence.isSignificant()) {
				undoMgr.addEdit(editSequence);
			}

			interaction.setStatusMessage(getAlignmentName());

			return true;
		}
	}

	public DesignEditorController(Design designToEdit, Project designProject)
	{
		super(designProject);

		design = designToEdit;

		undoMgr = UndoManager.getUndoManager(design,
				project);

		demoStateMgr =
			DemoStateManager.getStateManager(project, design);

		ui = new DesignEditorUI(design, project, undoMgr);

		interaction = ui.getInteraction();

		assignActions();

		ui.setVisible(true);
	}

	@Override
	protected Object getModelObject()
	{
		return getModel();
	}

	/**
	 * Accessor.
	 * @return the Design model being managed by this controller
	 */
	public Design getModel()
	{
		return design;
	}

	@Override
	public UI getUI()
	{
		return ui;
	}

	@Override
	protected ZoomableUI getZoomableUI()
	{
		return ui;
	}

	@Override
	public void assignActions()
	{
		super.assignActions();

		ui.setAction(DesignEditorLID.ExportDesignToHTML,
				createExportDesignToHTMLAction());

		ui.setAction(DesignEditorLID.ExportToXML,
				createExportDesignToXMLAction());

		ui.setAction(DesignEditorLID.Undo,
				new UndoController.UndoAction(undoMgr,
						interaction));

		ui.setAction(DesignEditorLID.Redo,
				new UndoController.RedoAction(undoMgr,
						interaction));

		ui.setAction(DesignEditorLID.Paste, createPasteAction());
		ui.setAction(DesignEditorLID.CopyFrame, createCopyFrameAction());
		ui.setAction(DesignEditorLID.CutFrame, createCutFrameAction());
		ui.setAction(DesignEditorLID.ClearFrameTemplate,
				new AListenerAction() {

			public boolean performAction(Object prms)
			{
				FrameTemplateSupport.clearFrameTemplate(design);
				return true;
			}
		});

		ui.setAction(DesignEditorLID.DeselectAll,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return SelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				SelectionState selection =
					(SelectionState) prms;

				selection.deselectAll();

				return true;
			}
		});

		ui.setAction(DesignEditorLID.MoveFrames,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.MoveParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.MoveParameters movePrms =
					(DesignEditorUI.MoveParameters) prms;

				if (movePrms != null) {
					return moveFrames(movePrms.dx,
							movePrms.dy,
							movePrms.selection);
				}
				else {
					throw new RcvrUIException("Cannot move frames without parameters.");
				}
			}
		});

		ui.setAction(DesignEditorLID.AddDesignDevices,
				createAddDevicesAction());
		ui.setAction(DesignEditorLID.NewFrame, createNewFrameAction());

		// Align selected frames
		ui.setAction(DesignEditorLID.AlignTop,
				new FrameAlignmentAction(AlignmentAction.TOP));

		ui.setAction(DesignEditorLID.AlignBottom,
				new FrameAlignmentAction(AlignmentAction.BOTTOM));

		ui.setAction(DesignEditorLID.AlignLeft,
				new FrameAlignmentAction(AlignmentAction.LEFT));

		ui.setAction(DesignEditorLID.AlignRight,
				new FrameAlignmentAction(AlignmentAction.RIGHT));

		ui.setAction(DesignEditorLID.AlignCenter,
				new FrameAlignmentAction(AlignmentAction.CENTER));

		ui.setAction(DesignEditorLID.AlignHorizCenter,
				new FrameAlignmentAction(AlignmentAction.HORIZ_CENTER));

		ui.setAction(DesignEditorLID.AlignVertCenter,
				new FrameAlignmentAction(AlignmentAction.VERT_CENTER));

		// Space selected frames equally
		ui.setAction(DesignEditorLID.SpaceVertically,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return Map.class;
			}


			@SuppressWarnings("unchecked")
			public boolean performAction(Object prms)
			{
				Map<Frame, DoubleRectangle> frameMap =
					(Map<Frame, DoubleRectangle>) prms;

				// Equally space the widgets in the
				// vertical axis.
				final boolean VERTICAL = true;

				return spaceFramesEqually(frameMap,
						VERTICAL);
			}
		});

		ui.setAction(DesignEditorLID.SpaceHorizontally,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return Map.class;
			}


			@SuppressWarnings("unchecked")
			public boolean performAction(Object prms)
			{
				Map<Frame, DoubleRectangle> frameMap =
					(Map<Frame, DoubleRectangle>) prms;

				// Equally space the widgets in the
				// horizontal axis.
				final boolean HORIZONTAL = false;

				return spaceFramesEqually(frameMap,
						HORIZONTAL);
			}
		});

		ui.setAction(DesignEditorLID.InitiateFrameRename,
				createInitiateFrameRenameAction());

		ui.setAction(DesignEditorLID.RenameFrame,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.FrameRenameParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.FrameRenameParameters evt =
					(DesignEditorUI.FrameRenameParameters) prms;

				return renameFrame(evt.frame,
						evt.newName);
			}
		});

		ui.setAction(ProjectLID.RenameDesign,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.DesignRenameParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.DesignRenameParameters evt =
					(DesignEditorUI.DesignRenameParameters) prms;

				return renameDesign(evt.design,
						evt.newText);
			}
		});

		ui.setAction(DesignEditorLID.NewTransition,
				createNewTransitionAction());

		ui.setAction(DesignEditorLID.EditFrame, createEditFrameAction());

		ui.setAction(DesignEditorLID.EditTransition,
				createEditTransitionAction());

		ui.setAction(DesignEditorLID.DeleteFrame,
				createDeleteFrameAction());

		ui.setAction(DesignEditorLID.DeleteTransition,
				createDeleteTransitionAction());

		// Nudge selected frame(s)
		ui.setAction(DesignEditorLID.NudgeLeft,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				double dx = -1.0 / ui.getZoom();

				return moveFrames(dx, 0.0, selection);
			}
		});

		ui.setAction(DesignEditorLID.NudgeRight,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				double dx = 1.0 / ui.getZoom();

				return moveFrames(dx, 0.0, selection);
			}
		});

		ui.setAction(DesignEditorLID.NudgeUp,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				double dy = -1.0 / ui.getZoom();

				return moveFrames(0.0, dy, selection);
			}
		});

		ui.setAction(DesignEditorLID.NudgeDown,
				new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				double dy = 1.0 / ui.getZoom();

				return moveFrames(0.0, dy, selection);
			}
		});

		ui.setAction(DesignEditorLID.SelectAll,
				new AListenerAction() {

			public boolean performAction(Object prms)
			{
				ui.selectAllFrames();

				return true;
			}
		});

		ui.setAction(DesignEditorLID.ChangeTarget,
				createChangeTargetAction());

		ui.setAction(DesignEditorLID.ChangeSource,
				createChangeSourceAction());

		// The following 6 are also functionality in FrameEditorController

		ui.setAction(DesignEditorLID.SetBackgroundImage,
				createSetBackgroundImageAction());

		ui.setAction(DesignEditorLID.RemoveBackgroundImage,
				createRemoveBackgroundImageAction());

		ui.setAction(DesignEditorLID.CopyImageAsBackground,
				createCopyImageAsBkgAction());

		ui.setAction(DesignEditorLID.PasteBackgroundImage,
				createPasteBackgroundImageAction());

		ui.setAction(DesignEditorLID.SetWidgetColor,
				createSetWidgetColorAction());

		// Skins!
		ui.setAction(DesignEditorLID.SkinWireFrame,
				createSetSkinAction(SkinType.WireFrame,
						DesignEditorLID.SkinWireFrame));
		ui.setAction(DesignEditorLID.SkinMacOSX,
				createSetSkinAction(SkinType.MacOSX,
						DesignEditorLID.SkinMacOSX));
		ui.setAction(DesignEditorLID.SkinWinXP,
				createSetSkinAction(SkinType.WinXP,
						DesignEditorLID.SkinWinXP));
		ui.setAction(DesignEditorLID.SkinPalm,
				createSetSkinAction(SkinType.Palm,
						DesignEditorLID.SkinPalm));

        ui.setAction(CogToolLID.RenderAll,
                     createRenderAllAction(true, CogToolLID.RenderAll));
        ui.setAction(CogToolLID.UnRender,
                     createRenderAllAction(false, CogToolLID.UnRender));

		ui.setAction(DesignEditorLID.ImportImageDirectory,
				createImportImageDirectory());

		IListenerAction action = createChangeActionAction();

		ui.setAction(DesignEditorLID.ChangeWidgetAction, action);
		ui.setAction(DesignEditorLID.ChangeDeviceAction, action);

		ui.setAction(DesignEditorLID.DuplicateFrame,
				createDuplicateFrameAction());

		ui.setAction(DesignEditorLID.ChangeDelay,
				createChangeDelayAction());
	}


	/**
	 * Update the design's name.
	 * The name must be unique.
	 *
	 * @param design
	 * @param newName
	 * @return
	 */
	protected boolean renameDesign(final Design design, String tryName)
	{
		final String oldName = design.getName();

		boolean notDone = true;

		do {
			if (tryName.length() == 0) {
				tryName =
					interaction.protestNameCannotBeEmpty("Design");

				// If canceled, indicate so; otherwise, test again
				if (tryName == null) {
					return false;
				}
			}
			else {
				Design designForName = project.getDesign(tryName);

				// If the widget for the given name is the same,
				// then no change is necessary!
				if (designForName == design) {
					notDone = false;
				}
				else if (designForName != null) {
					// A non-null design for the tryName indicates a collision
					tryName =
						interaction.protestNameCollision("Design");

					// If canceled, indicate so; otherwise, test again
					if (tryName == null) {
						return false;
					}
				}
				else {
					// Not canceled, not empty, and not a collision
					notDone = false;

					final String newName = tryName;

					design.setName(newName);

					IUndoableEdit edit =
						new AUndoableEdit(ProjectLID.RenameDesign)
					{
						@Override
						public String getPresentationName()
						{
							return renameDesign;
						}

						@Override
						public void redo()
						{
							super.redo();
							design.setName(newName);
							makeDesignNameUnique(design);
						}

						@Override
						public void undo()
						{
							super.undo();
							design.setName(oldName);
							makeDesignNameUnique(design);
						}
					};

					undoMgr.addEdit(edit);
				}
			}
		} while (notDone);

		return true;
	}

	/**
	 * Used to determine the horizontal relationship of 2 frames,
	 * based on a map from the frame to its bounds.
	 * This is used in the calculation of alignment, and spacing.
	 * Frame1 to the left of Frame2 returns a < 0 number
	 */
	protected static Comparator<Map.Entry<Frame, DoubleRectangle>> frameHorizontalComparator =
		new Comparator<Map.Entry<Frame, DoubleRectangle>>() {

		public int compare(Map.Entry<Frame, DoubleRectangle> l,
				Map.Entry<Frame, DoubleRectangle> r)
		{
			// Compare the left & right for ordering
			DoubleRectangle left = l.getValue();
			DoubleRectangle right = r.getValue();

			double diff = left.x - right.x;

			return (diff < 0) ? -1 : ((diff > 0) ? 1 : 0);
		}
	};

	/**
	 * Used to determine the Vertical relationship of 2 frames,
	 * based on a map from the frame to its bounds.
	 * This is used in the calculation of alignment, and spacing.
	 * Frame1 above Frame2 returns a < 0 number
	 */
	protected static Comparator<Map.Entry<Frame, DoubleRectangle>> frameVerticalComparator =
		new Comparator<Map.Entry<Frame, DoubleRectangle>>() {

		public int compare(Map.Entry<Frame, DoubleRectangle> u,
				Map.Entry<Frame, DoubleRectangle> l)
		{
			// Compare up and down for ordering.
			DoubleRectangle upper = u.getValue();
			DoubleRectangle lower = l.getValue();

			double diff = upper.y - lower.y;

			return (diff < 0) ? -1 : ((diff > 0) ? 1 : 0);
		}
	};

	/**
	 * Spaces the selected frames equally along a certain axis
	 * @param vertical true for vertical axis; false for horizontal
	 */
	protected boolean spaceFramesEqually(Map<Frame, DoubleRectangle> frameMap,
			final boolean vertical)
	{
		final String undoRedoLabel = vertical ? SPACE_VERTICALLY
				: SPACE_HORIZONTALLY;

		CogToolLID lid = vertical ? DesignEditorLID.SpaceVertically
				: DesignEditorLID.SpaceHorizontally;

		CompoundUndoableEdit editSequence =
			new CompoundUndoableEdit(undoRedoLabel, lid);

		// Order the widgets according to location
		// (either from the left or from the top)
		Comparator<Map.Entry<Frame, DoubleRectangle>> c =
			vertical ? frameVerticalComparator
					: frameHorizontalComparator;
		@SuppressWarnings("unchecked")
		Map.Entry<Frame, DoubleRectangle>[] entries =
			new Map.Entry[frameMap.size()];

		frameMap.entrySet().toArray(entries);
		Arrays.sort(entries, c);

		// Calculate the spacing between widgets
		double sum = 0;
		double min = Double.MAX_VALUE;
		double max = Double.MIN_VALUE;

		// Go through each frame that is selected
		// Determine the size, min & max of the region.
		// this can then be used to do spacing.
		for (Entry<Frame, DoubleRectangle> entrie : entries) {
			DoubleRectangle bounds = entrie.getValue();

			double size = vertical ? bounds.height : bounds.width;
			double position = vertical ? bounds.y : bounds.x;

			sum += size;
			min = Math.min(min, position);
			max = Math.max(max, size + position);
		}

		// Get the spacing to use between each item.
		double spacing = ((max - min) - sum) / (entries.length - 1);

		// Adjust the spacings to the correct values
		for (Entry<Frame, DoubleRectangle> entrie : entries) {
			// go through each frame and set the frame's origin
			final Frame f = entrie.getKey();
			final DoubleRectangle bounds = entrie.getValue();

                // TODO (dfm) this is probably the place to walk over the design pre-flighting
                //            problems we want to warn the user about, and give the opportunity
                //            to skip the export if desired

			// Determine the old point...
			final double p_old = vertical ? bounds.y : bounds.x;
			final double p_new = min;

			// Set the new location
			f.setFrameOrigin(vertical ? bounds.x : p_new,
					vertical ? p_new : bounds.y);

			// Advance the pointer to the next location
			min += spacing + (vertical ? bounds.height : bounds.width);

			IUndoableEdit edit =
				new AUndoableEdit(lid)
			{
				@Override
				public String getPresentationName()
				{
					return undoRedoLabel;
				}

				@Override
				public void redo()
				{
					super.redo();

					f.setFrameOrigin(vertical ? bounds.x : p_new,
							vertical ? p_new : bounds.y);
				}

				@Override
				public void undo()
				{
					super.undo();

					f.setFrameOrigin(vertical ? bounds.x : p_old,
							vertical ? p_old : bounds.y);
				}
			};

			editSequence.addEdit(edit);
		}

		editSequence.end();

		// Only add this edit if it is significant
		if (editSequence.isSignificant()) {
			undoMgr.addEdit(editSequence);
		}

		return true;
	}

	// TODO this should be merged with ProjectController.createWebPages in
	//      some way, probably by moving to DefaultController (sigh)
	protected IListenerAction createExportDesignToHTMLAction()
	{
		return new AListenerAction() {

			public boolean performAction(Object actionParms)
			{
				return ExportToHTMLWorkThread.exportDesignToHTML(design,
				                                                 interaction);
			}
		};
	}

	protected IListenerAction createExportDesignToXMLAction()
	{
		return new AListenerAction() {

			public boolean performAction(Object actionParms)
			{
				String defaultFilename = design.getName() + ".xml";
				File exportFile = interaction.selectXMLFile(false,
						defaultFilename);

				if (exportFile == null) {
					return false;
				}

				OutputStream oStream = null;

				try {
					try {
						oStream = new FileOutputStream(exportFile);
						Writer xmlWriter =
							new BufferedWriter(new OutputStreamWriter(oStream,
									"UTF-8"));
						Map<ITaskDesign, TaskApplication> designTAs =
							project.taskApplicationsForDesign(design);

						ExportCogToolXML.exportXML(design,
								designTAs,
								xmlWriter,
								"UTF-8");

						xmlWriter.flush();
					}
					finally {
						if (oStream != null) {
							oStream.close();
						}
					}
				}
				catch (IllegalArgumentException e) {
					throw new RcvrIllegalStateException("Invalid argument exporting to XML", e);
				}
				catch (IllegalStateException e) {
					throw new RcvrIllegalStateException("Exporting to XML", e);
				}
				catch (IOException e) {
					throw new RcvrIOSaveException("Exporting to XML", e);
				}
				catch (Exception e) {
					throw new RecoverableException("Exporting to XML", e);
				}

				interaction.setStatusMessage(L10N.get("PC.DesignExportedToXML",
				"Design exported to XML:")
				+ " "
				+ exportFile.getName());

				return true;
			}
		};
	}

	protected void makeFrameNameUnique(Frame frame,
			Collection<Frame> pendingAdds)
	{
		frame.setName(NamedObjectUtil.makeNameUnique(frame.getName(),
				design.getFrames(),
				pendingAdds));
	}

	protected void makeFrameNameUnique(Frame frame)
	{
		makeFrameNameUnique(frame, null);
	}

	protected void makeDesignNameUnique(Design design)
	{
		design.setName(NamedObjectUtil.makeNameUnique(design.getName(),
				project.getDesigns()));
	}

	/**
	 * Create a ListenerAction to handle removing frame background image on
	 * multiple frames
	 */
	protected IListenerAction createRemoveBackgroundImageAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				// Get selected frames from parameters
				FrameSelectionState seln = (FrameSelectionState) prms;
				Frame[] frames = seln.getSelectedFrames();

				setBackgroundImageOnFrames(frames,
						null,
						WidgetAttributes.NO_IMAGE,
						DesignEditorLID.RemoveBackgroundImage);

				return true;
			}
		};
	}

	/**
	 * Create a ListenerAction to handle setting frame background image on
	 * multiple frames
	 */
	protected IListenerAction createSetBackgroundImageAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				// Get selected frames from parameters
				FrameSelectionState seln = (FrameSelectionState) prms;
				Frame[] frames = seln.getSelectedFrames();

				// this code is from FrameEditorController

				// load image
				// Pull up interaction to select a file
				String imageURL = interaction.selectImageFile();

				// Check if the dialog was cancelled
				if (imageURL != null) {
					byte[] imageData;
					try {
						// Set the background to new image
						imageData = GraphicsUtil.loadImageFromFile(imageURL);

						setBackgroundImageOnFrames(frames,
								imageData,
								imageURL,
								DesignEditorLID.SetBackgroundImage);
						return true;
					}
					catch (IOException e) {
						interaction.protestUnreadableFile();
						imageData = null;
					}
				}

				return false;
			}
		};
	}

	protected IListenerAction createPasteBackgroundImageAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.PasteBackgroundImageParms.class;
			}


			public boolean performAction(Object prms)
			{
				// Get selected frames from parameters
				DesignEditorUI.PasteBackgroundImageParms parms =
					(DesignEditorUI.PasteBackgroundImageParms) prms;
				Frame[] frames = parms.selection.getSelectedFrames();

				if ((frames == null) || (frames.length == 0)) {
					interaction.protestNoSelection();
					return false;
				}

				setBackgroundImageOnFrames(frames,
						parms.imageData,
						WidgetAttributes.NO_IMAGE,
						DesignEditorLID.PasteBackgroundImage);
				return true;
			}
		};
	}

	/**
	 * Create a ListenerAction to handle setting widget color on multiple frames
	 */
	protected IListenerAction createSetWidgetColorAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				// Get selected frames from parameters
				FrameSelectionState seln = (FrameSelectionState) prms;
				Frame[] frames = seln.getSelectedFrames();

				// TODO: Which color to use as old widget color?
				int oldColor = (frames.length > 0)
				? frames[0].getWidgetColor()
						: 128;

				// Pull up interaction to select a file
				Integer newColorChoice =
					interaction.selectColor(oldColor, selectWidgetColor);

				// Check if the dialog was canceled
				if (newColorChoice != null) {
					setWidgetColorForFrames(frames, newColorChoice.intValue());

					return true;
				}

				return false;
			}
		};
	}

	protected IListenerAction createPasteAction()
	{
		return new AListenerAction() {

			public boolean performAction(Object prms)
			{
				try {
					Collection<Object> objects =
						CogToolClipboard.fetchCogToolObjects();

					if ((objects != null) && (objects.size() > 0)) {
						CompoundUndoableEdit editSequence =
							new CompoundUndoableEdit(L10N.get("UNDO.Paste",
									"Paste"),
									DesignEditorLID.Paste);

						Set<DeviceType> devTypes = design.getDeviceTypes();

						int numPasted = 0;
						Iterator<Object> objIt = objects.iterator();

						while (objIt.hasNext()) {
							Object o = objIt.next();

							if (o instanceof Frame) {
								Frame frame = (Frame) o;
								makeFrameNameUnique(frame);

								// Find an unoccupied starting position
								// by cascading.
								DoublePoint origin = frame.getFrameOrigin();

								DesignUtil.findDistinctOrigin(design, origin,
										16.0, 16.0);

								// Union devices
								Iterator<InputDevice> frameDevices =
									frame.getInputDevices().iterator();

								while (frameDevices.hasNext()) {
									InputDevice inputDevice =
										frameDevices.next();
									DeviceType devType =
										inputDevice.getDeviceType();

									if (! devTypes.contains(devType)) {
										DesignCmd.addDevice(design, devType);
									}
								}

								Iterator<DeviceType> designDevTypes =
									devTypes.iterator();

								while (designDevTypes.hasNext()) {
									DeviceType devType = designDevTypes.next();

									if (frame.getInputDevice(devType) == null) {
										frame.addInputDevice(devType);
									}
								}

								addFrame(frame, editSequence);
								numPasted++;
							}
							else if (o instanceof Transition) {
								Transition t = (Transition) o;
								DeviceType device =
									t.getAction().getDefaultDeviceType();

								if (! devTypes.contains(device)) {
									DesignCmd.addDevice(design, device);
								}

								IUndoableEdit edit =
									DesignEditorCmd.addTransition(demoStateMgr,
											t);

								editSequence.addEdit(edit);
								numPasted++;
							}
						}

						editSequence.end();
						undoMgr.addEdit(editSequence);
						interaction.setStatusMessage(numPasted + " "
								+ pasteComplete);
					}
					else {
						interaction.setStatusMessage(nothingPasted);
					}
				}
				catch (IOException e) {
					throw new RcvrClipboardException(e);
				}
				catch (ParserConfigurationException e) {
					throw new RcvrClipboardException(e);
				}
				catch (SAXException e) {
					throw new RcvrClipboardException(e);
				}
				catch (ClipboardUtil.ClipboardException e) {
					throw new RcvrClipboardException(e);
				}

				return true;
			}
		};
	} // createPasteAction

	protected IListenerAction createCopyImageAsBkgAction()
	{
		return new IListenerAction()
		{

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.CopyBackgroundImageParms.class;
			}


			public boolean performAction(Object actionParms)
			{
				DesignEditorUI.CopyBackgroundImageParms prms =
					(DesignEditorUI.CopyBackgroundImageParms) actionParms;

				if (prms.selectedFrame == null) {
					interaction.protestNoSelection();
					return false;
				}

				setBackgroundImageOnFrames(new Frame[] { prms.selectedFrame },
						prms.imageData,
						WidgetAttributes.NO_IMAGE,
						CogToolLID.CopyImageAsBackground);

				return true;
			}
		};
	}

	protected IListenerAction createCopyFrameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState seln = (FrameSelectionState) prms;

				Frame[] frames = seln.getSelectedFrames();

				if ((frames != null) && (frames.length > 0)) {
					copyFrames(frames);

					if (frames.length == 1) {
						interaction.setStatusMessage(FRAME_COPIED);
					}
					else {
						interaction.setStatusMessage(FRAMES_COPIED);
					}

					return true;
				}

				interaction.protestNoSelection();

				return false;
			}
		};
	}

	protected void copyFrames(Frame[] frames)
	{
		try {
			// Passing the set of frames as part of purpose allows us
			// to copy only those transitions that link those frames.
			ObjectSaver s =
				CogToolClipboard.startClipboardSave(CogToolClipboard.CopyFrames,
						frames,
						CogToolClipboard.SAVE_TO_CLIPBOARD);

			for (Frame frame : frames) {
				s.saveObject(frame);
			}

			s.finish();
		}
		catch (IOException e) {
			throw new RcvrClipboardException(e);
		}
		catch (IllegalStateException e) {
			throw new RcvrClipboardException(e);
		}
		catch (OutOfMemoryError error) {
			throw new RcvrOutOfMemoryException("Copying Frames", error);
		}
	}

	protected IListenerAction createCutFrameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState seln = (FrameSelectionState) prms;

				Frame[] frames = seln.getSelectedFrames();

				if ((frames != null) && (frames.length > 0)) {
					copyFrames(frames);
					deleteFrames(frames);

					return true;
				}

				interaction.protestNoSelection();

				return false;
			}
		};
	}

	protected IListenerAction createAddDevicesAction()
	{
		return new AListenerAction() {

			public boolean performAction(Object prms)
			{
				return DesignCmd.addDevices(project, design, interaction);
			}
		};
	}

	protected IListenerAction createNewFrameAction()
	{
		return new AListenerAction() {

			public boolean performAction(Object prms)
			{
				// Find an unoccupied starting position by cascading.
				DoublePoint origin = new DoublePoint(10.0, 10.0);

				DesignUtil.findDistinctOrigin(design, origin, 16.0, 16.0);

				Frame frame = createNewFrame(origin.x, origin.y);

				ui.initiateFrameRename(frame);
				return true;
			}
		};
	}

	protected Frame createNewFrame(double x, double y)
	{
		return createNewFrame(x, y, null);
	}

	protected void addFrame(Frame frame, IUndoableEditSequence editSequence)
	{
		DesignEditorCmd.addFrame(project,
				design,
				demoStateMgr,
				frame,
				editSequence);
	} // addFrame

	protected Frame createFrame(String frameName, double x, double y)
	{
		Frame frame = new Frame(frameName, design.getDeviceTypes());

		frame.setFrameOrigin(x, y);

		return frame;
	}

	protected Frame createNewFrame(double x, double y,
			IUndoableEditSequence editSequence)
	{
		String frameNamePrefix = DEFAULT_FRAME_PREFIX + " ";
		String frameName = frameNamePrefix + nextNewFrameSuffix;

		while (design.getFrame(frameName) != null) {
			frameName = frameNamePrefix + ++nextNewFrameSuffix;
		}

		Frame frame = createFrame(frameName, x, y);

		addFrame(frame, (editSequence != null) ? editSequence : undoMgr);

		return frame;
	}

	protected boolean moveFrames(double dx, double dy,
			FrameSelectionState selection)
	{
		Frame[] frames = selection.getSelectedFrames();

		if ((frames != null) && (frames.length > 0)) {
			String editLabel;
			if (frames.length > 1) {
				editLabel = moveFrames;
			}
			else {
				editLabel = moveFrame;
			}
			CompoundUndoableEdit editSequence =
				new CompoundUndoableEdit(editLabel, DesignEditorLID.MoveFrames);

			for (Frame frame : frames) {
				moveFrame(frame, dx, dy, editSequence);
			}

			editSequence.end();
			undoMgr.addEdit(editSequence);
		}

		return true;
	}

	protected void moveFrame(final Frame frame,
			final double dx,
			final double dy,
			IUndoableEditSequence editSequence)
	{
		// Create a new point, so we can modify it without side-effects
		final DoublePoint oldP = new DoublePoint(frame.getFrameOrigin());
		final DoublePoint p = new DoublePoint(oldP);

		// Compute the new location.
		p.x += dx;
		p.y += dy;

		// Ensure that it is not less then 0,0
		if (p.x < 0) {
			p.x = 0;
		}
		if (p.y < 0) {
			p.y = 0;
		}

		frame.setFrameOrigin(p);

		editSequence.addEdit(new AUndoableEdit(DesignEditorLID.MoveFrames)
		{
			@Override
			public String getPresentationName()
			{
				return moveFrame;
			}

			@Override
			public void redo()
			{
				super.redo();

				frame.setFrameOrigin(p);
			}

			@Override
			public void undo()
			{
				super.undo();

				frame.setFrameOrigin(oldP);
			}
		});
	}

	protected IListenerAction createEditFrameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				Frame[] frames = selection.getSelectedFrames();

				if ((frames == null) || (frames.length == 0)) {
					interaction.protestNoSelection();

					return false;
				}

				for (Frame frame : frames) {
					try {
						FrameEditorController.openController(frame,
								design,
								project);
					}
					catch (GraphicsUtil.ImageException ex) {
						interaction.protestInvalidImageFile();
					}
				}

				return true;
			}
		};
	} // createEditDesignAction

	protected IListenerAction createEditTransitionAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.EditTransitionParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.EditTransitionParameters parms =
					(DesignEditorUI.EditTransitionParameters) prms;

				Transition[] transitions =
					parms.selection.getSelectedTransitions();

				// Probably only one transition selected
				if ((transitions != null) && (transitions.length > 0)) {
					if (transitions.length == 1) {
						TransitionSource source = transitions[0].getSource();
						AAction action = transitions[0].getAction();
						int limitMode =
							ActionProperties.determineChangeActionMode(source);
						int deviceTypes =
							DeviceType.buildDeviceSet(design.getDeviceTypes());

						if (! interaction.determineNewAction(transitions[0],
								properties,
								parms.useWhichParts,
								deviceTypes,
								limitMode,
								L10N.get("DE.ChangeActionType",
										"Change Action Type")))
						{
							return false;
						}

						action =
							EditActionCmd.buildActionFromProperties(properties,
									deviceTypes,
									limitMode,
									interaction);

						if (action == null) {
							return false;
						}

						action =
							EditActionCmd.ensureActionIsUnique(source,
									action,
									properties,
									deviceTypes,
									limitMode,
									transitions[0],
									interaction);

						if (action == null) {
							return false;
						}

						return changeTransitionsAction(transitions,
								action,
								properties.delayInSecs,
								properties.delayLabel);
					}
					else {
						interaction.protestMultipleTransitionSelection();
					}
				}
				else {
					interaction.protestNoSelection();
				}

				return false;
			}
		};
	} // createEditTransitionAction

	protected IListenerAction createInitiateFrameRenameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				int selectedFrameCount = selection.getSelectedFrameCount();

				if (selectedFrameCount == 1) {
					ui.initiateFrameRename(selection);

					return true;
				}

				if (selectedFrameCount > 1) {
					interaction.protestMultipleFrameSelection();
					return false;
				}

				interaction.protestNoSelection();

				return false;
			}
		};
	}

	protected boolean isFrameNameUnique(String newFrameName,
			String oldFrameName)
	{
		if (newFrameName.equals(oldFrameName)) {
			return true;
		}

		return design.getFrame(newFrameName) == null;
	}

	protected boolean renameFrame(final Frame renamedFrame,
			String tryName)
	{
		final String oldName = renamedFrame.getName();

		boolean notDone = true;

		do {
			if (tryName.length() == 0) {
				tryName =
					interaction.protestNameCannotBeEmpty(DEFAULT_FRAME_PREFIX);

				// If canceled, indicate so; otherwise, test again
				if (tryName == null) {
					return false;
				}
			}
			else {
				Frame frameForName = design.getFrame(tryName);

				// If the widget for the given name is the same,
				// then no change is necessary!
				if (frameForName == renamedFrame) {
					notDone = false;
				}
				else if (frameForName != null) {
					// A non-null frame for the tryName indicates a collision
					tryName =
						interaction.protestNameCollision(DEFAULT_FRAME_PREFIX);

					// If canceled, indicate so; otherwise, test again
					if (tryName == null) {
						return false;
					}
				}
				else {
					// Not canceled, not empty, and not a collision
					notDone = false;

					final String newName = tryName;

					renamedFrame.setName(newName);

					IUndoableEdit edit =
						new AUndoableEdit(DesignEditorLID.RenameFrame)
					{
						@Override
						public String getPresentationName()
						{
							return renameFrame;
						}

						@Override
						public void redo()
						{
							super.redo();
							Frame testFrame = design.getFrame(newName);
							renamedFrame.setName(newName);

							if (testFrame != null) {
								makeFrameNameUnique(renamedFrame);
							}
						}

						@Override
						public void undo()
						{
							super.undo();
							Frame testFrame = design.getFrame(oldName);
							renamedFrame.setName(oldName);

							if (testFrame != null) {
								makeFrameNameUnique(renamedFrame);
							}
						}
					};

					undoMgr.addEdit(edit);
				}
			}
		} while (notDone);

		return true;
	} // renameFrame

	protected IListenerAction createNewTransitionAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.NewTransitionParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.NewTransitionParameters newPrms =
					(DesignEditorUI.NewTransitionParameters) prms;

				if (newPrms != null) {
					CompoundUndoableEdit editSequence = null;

					if (newPrms.target == null) {
						editSequence =
							new CompoundUndoableEdit(DesignEditorCmd.NEW_TRANSITION,
									DesignEditorLID.NewTransition);

						newPrms.target =
							createNewFrame(newPrms.x, newPrms.y, editSequence);
					}

					IUndoableEdit edit =
						createNewTransition(newPrms.source, newPrms.target);

					// null is returned if the operation is canceled.
					if (edit != null) {
						if (editSequence != null) {
							editSequence.addEdit(edit);
							editSequence.end();

							edit = editSequence;
						}

						undoMgr.addEdit(edit);

						if (editSequence != null) {
							ui.initiateFrameRename(newPrms.target);
						}

						return true;
					}
					else if (editSequence != null) {
						editSequence.end();
						editSequence.undo();
					}

					return false;
				}
				else {
					throw new RcvrUIException("Cannot create transition without parameters.");
				}
			}
		};
	}

	/**
	 * Pass-through using controller's resources
	 * @param source
	 * @param target
	 * @return
	 */
	protected AAction createDefaultAction(TransitionSource source)
	{
		ui.getDefaultProperties(source, properties);

		// Determine desired transition type from the UI (palette setting)
		return EditActionCmd.createDefaultAction(design,
				source,
				ui.getCurrentActionType(),
				properties);
	}

	// Return null to indicate cancel
	protected IUndoableEdit createNewTransition(final TransitionSource source,
			Frame target)
	{
		int deviceTypes =
			DeviceType.buildDeviceSet(design.getDeviceTypes());

		// Helps control which options are available for editing the action
		int limitMode = ActionProperties.determineChangeActionMode(source);

		AAction action = createDefaultAction(source);

		if (action == null) {
			if (! interaction.determineNewAction(properties,
					deviceTypes,
					limitMode,
					L10N.get("DE.SetActionType",
							"Set Action Type")))
			{
				return null;    // Indicate that the user canceled
			}

			action = EditActionCmd.buildActionFromProperties(properties,
					deviceTypes,
					limitMode,
					interaction);

			if (action == null) {
				return null;
			}
		}

		// action is not null at this point; no transition yet for this action!
		action = EditActionCmd.ensureActionIsUnique(source,
				action,
				properties,
				deviceTypes,
				limitMode,
				null,
				interaction);

		if (action == null) {
			return null;
		}

		Transition newTransition = new Transition(source, target, action);

		newTransition.setDelayInfo(properties.delayInSecs,
				properties.delayLabel);

		IUndoableEdit edit =
			DesignEditorCmd.addTransition(demoStateMgr, newTransition);

		interaction.setTransitionStatusMessage(newTransition);

		return edit;
	} // createNewTransition

	protected IListenerAction createDeleteFrameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return FrameSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				FrameSelectionState selection =
					(FrameSelectionState) prms;

				Frame[] frames = selection.getSelectedFrames();

				if ((frames != null) && (frames.length > 0)) {
					if (interaction.confirmDeleteFrames(frames)) {
						deleteFrames(frames);
						return true;
					}
				}
				else {
					interaction.protestNoSelection();
				}

				return false;
			}
		};
	} // createDeleteFrameAction

	// Currently, used by import images as frames
	protected void deleteFrame(Frame frame,
			CogToolLID lid,
			IUndoableEditSequence editSequence)
	{
		DesignEditorCmd.deleteFrame(project,
				design,
				demoStateMgr,
				frame,
				lid,
				editSequence);
	} // deleteFrame

	protected void deleteFrames(final Frame[] frames)
	{
		if ((frames != null) && (frames.length > 0)) {
			final Transition[][] incidentTransitions =
				new Transition[frames.length][];

			for (int i = 0; i < frames.length; i++) {
				// Remove Transition instances that target this Frame
				incidentTransitions[i] = frames[i].removeIncidentTransitions();
			}

			for (Frame frame : frames) {
				design.removeFrame(frame);
			}

			DemoStateManager.IDesignUndoableEdit edit =
				new DemoStateManager.InvalidatingEdit(DesignEditorLID.DeleteFrame,
						demoStateMgr)
			{
				protected boolean recoverMgrs = true;

				@Override
				public String getPresentationName()
				{
					return (frames.length > 1) ? deleteFrames : deleteFrame;
				}

				@Override
				public void redo()
				{
					super.redo();

					recoverMgrs = true;

					for (Frame frame : frames) {
						frame.removeIncidentTransitions();
					}

					for (Frame frame : frames) {
						design.removeFrame(frame);
					}

					stateMgr.noteFramesEdit(frames, this);
				}

				@Override
				public void undo()
				{
					super.undo();

					recoverMgrs = false;

					for (int i = frames.length - 1; i >= 0; i--) {
						design.addFrame(frames[i]);
					}

					for (int i = frames.length - 1; i >= 0; i--) {
						DesignEditorCmd.addIncidentTransitions(incidentTransitions[i]);
					}

					stateMgr.noteFramesEdit(frames, this);
				}

				@Override
				public void die()
				{
					super.die();

					if (recoverMgrs) {
						for (Frame frame : frames) {
							recoverManagers(frame);
						}
					}
				}
			};

			demoStateMgr.noteFramesEdit(frames, edit);
			undoMgr.addEdit(edit);
		}
	} // deleteFrames

	protected IListenerAction createDeleteTransitionAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorSelectionState.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorSelectionState selection =
					(DesignEditorSelectionState) prms;

				Transition[] transitions = selection.getSelectedTransitions();

				if ((transitions != null) && (transitions.length > 0)) {
					if (interaction.confirmDeleteTransitions(transitions)) {
						deleteTransitions(transitions);

						return true;
					}
				}
				else {
					interaction.protestNoSelection();
				}

				return false;
			}
		};
	} // createDeleteTransitionAction

	protected IListenerAction createChangeTargetAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.ChangeTargetParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.ChangeTargetParameters chgPrms =
					(DesignEditorUI.ChangeTargetParameters) prms;

				DesignEditorCmd.changeTransitionTarget(demoStateMgr,
						chgPrms.transition,
						chgPrms.newDestination,
						undoMgr);

				return true;
			}
		};
	}

	protected IListenerAction createChangeSourceAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.ChangeSourceParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.ChangeSourceParameters chgPrms =
					(DesignEditorUI.ChangeSourceParameters) prms;

				return changeTransitionSource(chgPrms.transition,
						chgPrms.newSource);
			}
		};
	}

	protected void deleteTransitions(final Transition[] transitions)
	{
		// For redo, must save the corresponding source for each Transition
		if ((transitions != null) && (transitions.length > 0)) {
			final TransitionSource[] sources =
				new TransitionSource[transitions.length];

			for (int i = 0; i < transitions.length; i++) {
				Transition t = transitions[i];
				sources[i] = t.getSource();

				sources[i].removeTransition(t);
			}

			DemoStateManager.IDesignUndoableEdit edit =
				new DemoStateManager.InvalidatingEdit(DesignEditorLID.DeleteTransition,
						demoStateMgr)
			{
				@Override
				public String getPresentationName()
				{
					return (transitions.length > 1) ? deleteTransitions
							: deleteTransition;
				}

				@Override
				public void redo()
				{
					super.redo();

					for (int i = 0; i < transitions.length; i++) {
						Transition t = transitions[i];

						sources[i].removeTransition(t);
					}

					stateMgr.noteTransitionsEdit(transitions, this);
				}

				@Override
				public void undo()
				{
					super.undo();

					for (int i = 0; i < transitions.length; i++) {
						Transition t = transitions[i];

						sources[i].addTransition(t);
					}

					stateMgr.noteTransitionsEdit(transitions, this);
				}
			};

			demoStateMgr.noteTransitionsEdit(transitions, edit);
			undoMgr.addEdit(edit);
		}
	} // deleteTransitions

	protected boolean changeTransitionSource(final Transition transition,
			final TransitionSource newSource)
	{
		final TransitionSource oldSource = transition.getSource();

		if (newSource != oldSource) {
			AAction action = transition.getAction();

			// Check that the new source is consistent the transition's action
			if (! newSource.canAccept(action)) {
				interaction.protestInconsistentSource();
				return false;
			}

			while (newSource.getTransition(action) != null) {
				//TODO: allow interaction to change?  Must handle in undo then!
				if (! interaction.protestNotUniqueAction(action,
						newSource,
						false))
				{
					return false;
				}
			}

			oldSource.removeTransition(transition);
			transition.setSource(newSource);
			newSource.addTransition(transition);

			final boolean invalidating =
				(newSource.getFrame() != oldSource.getFrame());

			DemoStateManager.IDesignUndoableEdit edit =
				new DemoStateManager.DesignUndoableEdit(DesignEditorLID.ChangeSource,
						demoStateMgr)
			{
				@Override
				public String getPresentationName()
				{
					return changeTransitionSource;
				}

				@Override
				public Boolean getEditNature()
				{
					return invalidating ? DemoStateManager.INVALIDATING
							: DemoStateManager.OBSOLETING;
				}

				@Override
				public void redo()
				{
					super.redo();

					oldSource.removeTransition(transition);
					transition.setSource(newSource);
					newSource.addTransition(transition);

					stateMgr.noteTransitionEdit(transition, this);

					if (! invalidating && CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
						DemoScriptCmd.regenerateDesignScripts(project,
								design,
								interaction);
					}
				}

				@Override
				public void undo()
				{
					super.undo();

					newSource.removeTransition(transition);
					transition.setSource(oldSource);
					oldSource.addTransition(transition);

					stateMgr.noteTransitionEdit(transition, this);

					if (! invalidating && CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
						DemoScriptCmd.regenerateDesignScripts(project,
								design,
								interaction);
					}
				}
			};

			demoStateMgr.noteTransitionEdit(transition, edit);
			undoMgr.addEdit(edit);

			if (! invalidating && CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
				DemoScriptCmd.regenerateDesignScripts(project,
						design,
						interaction);
			}
		}

		return true;
	}

	protected IListenerAction createChangeActionAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.ChangeActionParameters.class;
			}


			public boolean performAction(Object prms)
			{
				DesignEditorUI.ChangeActionParameters chgPrms =
					(DesignEditorUI.ChangeActionParameters) prms;

				if (chgPrms != null) {
					Transition[] transitions =
						chgPrms.selection.getSelectedTransitions();

					if ((transitions != null) && (transitions.length > 0)) {
						properties.copyValues(chgPrms.properties);

						// TODO: Assume one for the moment; ui enforces it!
						TransitionSource source = transitions[0].getSource();
						int deviceTypes =
							DeviceType.buildDeviceSet(design.getDeviceTypes());

						int limitMode =
							ActionProperties.determineChangeActionMode(source);
						AAction newAction =
							EditActionCmd.buildActionFromProperties(properties,
									deviceTypes,
									limitMode,
									interaction);

						if (newAction == null) {
							return false;
						}

						newAction =
							EditActionCmd.ensureActionIsUnique(source,
									newAction,
									properties,
									deviceTypes,
									limitMode,
									transitions[0],
									interaction);

						if (newAction == null) {
							return false;
						}

						return changeTransitionsAction(transitions,
								newAction,
								NO_DELAY_CHANGE,
								null);
						// TODO: If the given action properties are unusable
						// (e.g., not unique from the source or empty string)
						// and we should therefore return false so that an
						// action that commits property changes can be
						// canceled, assign the ensureActionIsUnique result
						// to a new variable uniqueAction and return the AND of
						// the changeTransitionsAction call (first) with
						// (newAction == uniqueAction).
					}
				}

				return false;
			}
		};
	}

	/**
	 * Negative delays are not allowed, thus use -1.0 to indicate no chg
	 */
	protected static final double NO_DELAY_CHANGE = -1.0;

	protected boolean changeTransitionsAction(Transition[] transitions,
			AAction newAction,
			double delayInSecs,
			String delayLabel)
	{
		if ((transitions != null) && (transitions.length > 0)) {
			CompoundUndoableEdit editSequence =
				new CompoundUndoableEdit(changeAction,
						DesignEditorLID.ChangeWidgetAction);

			for (Transition transition : transitions) {
				changeTransitionAction(transition,
						newAction,
						delayInSecs,
						delayLabel,
						editSequence);
			}

			editSequence.end();

			// Only add the compound edit if it contains something
			if (editSequence.isSignificant()) {
				undoMgr.addEdit(editSequence);
			}
		}

		return true;
	}

	protected void changeTransitionAction(final Transition transition,
			final AAction newAction,
			final double delayInSecs,
			final String delayLabel,
			IUndoableEditSequence editSequence)
	{
		final AAction oldAction = transition.getAction();
		final double oldDelayInSecs = transition.getDelayInSecs();
		final String oldDelayLabel = transition.getDelayLabel();

		// Must check that the transition's source can accept the new action
		if ((! oldAction.equals(newAction)) ||
				((delayInSecs != NO_DELAY_CHANGE) &&
						((delayInSecs != oldDelayInSecs) ||
								! oldDelayLabel.equals(delayLabel))))
		{
			transition.setAction(newAction);

			if (delayInSecs != NO_DELAY_CHANGE) {
				transition.setDelayInfo(delayInSecs, delayLabel);
			}

			DemoStateManager.IDesignUndoableEdit edit =
				new DemoStateManager.ObsoletingEdit(DesignEditorLID.ChangeWidgetAction,
						demoStateMgr)
			{
				@Override
				public String getPresentationName()
				{
					return changeAction;
				}

				@Override
				public void redo()
				{
					super.redo();

					transition.setAction(newAction);

					if (delayInSecs != NO_DELAY_CHANGE) {
						transition.setDelayInfo(delayInSecs, delayLabel);
					}

					stateMgr.noteTransitionEdit(transition, this);

					if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
						DemoScriptCmd.regenerateDesignScripts(project,
								design,
								interaction);
					}
				}

				@Override
				public void undo()
				{
					super.undo();

					transition.setAction(oldAction);

					if (delayInSecs != NO_DELAY_CHANGE) {
						transition.setDelayInfo(oldDelayInSecs,
								oldDelayLabel);
					}

					stateMgr.noteTransitionEdit(transition, this);

					if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
						DemoScriptCmd.regenerateDesignScripts(project,
								design,
								interaction);
					}
				}
			};

			demoStateMgr.noteTransitionEdit(transition, edit);
			editSequence.addEdit(edit);

			if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
				DemoScriptCmd.regenerateDesignScripts(project,
						design,
						interaction);
			}
		}
	}

	protected class FrameSubsetDuplicator implements Frame.IFrameDuplicator
	{
		protected Set<Frame> framesToCopy = new HashSet<Frame>();
		protected Map<Frame, Frame> copiedFrames =
			new HashMap<Frame, Frame>(); // old frame -> frame copy

		public FrameSubsetDuplicator(Frame[] frames)
		{
			for (Frame frame : frames) {
				framesToCopy.add(frame);
			}
		}

		/**
		 * Only duplicate frames that are in the selection must be copied;
		 * otherwise, return the given frame itself.
		 */

		public Frame getOrDuplicate(Frame frameToCopy)
		{
			Frame frame = copiedFrames.get(frameToCopy);

			if (frame == null) {
				if (framesToCopy.contains(frameToCopy)) {
					frame = frameToCopy.duplicate(frameToCopy.getName(), this);

					// Warning: it is important that each frame be added
					// to the design *before* we make the next frame name
					// unique, or we can end up with non-unique names.
					makeFrameNameUnique(frame, copiedFrames.values());
				}
				else {
					frame = frameToCopy;
				}
			}

			return frame;
		}


		public void recordDuplicateFrame(Frame originalFrame,
				Frame frameDuplicate)
		{
			copiedFrames.put(originalFrame, frameDuplicate);
		}
	}

	protected IListenerAction createDuplicateFrameAction()
	{
		return new IListenerAction() {

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.DuplicateParameters.class;
			}


			public boolean performAction(Object prms)
			{
				// From DesignEditorUI
				DesignEditorUI.DuplicateParameters parameters = (DesignEditorUI.DuplicateParameters) prms;

				Frame[] framesToDuplicate =
					parameters.selection.getSelectedFrames();

				if ((framesToDuplicate != null) &&
						(framesToDuplicate.length > 0))
				{
					FrameSubsetDuplicator duplicator =
						new FrameSubsetDuplicator(framesToDuplicate);

					Frame[] duplicatedFrames =
						new Frame[framesToDuplicate.length];

					for (int i = 0; i < framesToDuplicate.length; i++) {
						Frame duplicatedFrame =
							duplicator.getOrDuplicate(framesToDuplicate[i]);

						duplicatedFrames[i] = duplicatedFrame;

						duplicatedFrame.moveFrameOrigin(parameters.dx,
								parameters.dy);
					}

					design.addFrames(duplicatedFrames);
					undoMgr.addEdit(createDuplicateFramesEdit(duplicatedFrames));

					return true;
				}

				interaction.protestNoSelection();

				return false;
			}
		};
	}

	protected IUndoableEdit createDuplicateFramesEdit(final Frame[] frames)
	{
		// Based on the code in deleteFrames
		return new DemoStateManager.InvalidatingEdit(DesignEditorLID.DuplicateFrame,
				demoStateMgr)
		{
			protected Transition[][] incidentTransitions = null;
			protected boolean recoverMgrs = false;

			@Override
			public String getPresentationName()
			{
				return (frames.length > 1) ? duplicateFrames
						: duplicateFrame;
			}

			@Override
			public void redo()
			{
				super.redo();

				recoverMgrs = false;

				for (int i = frames.length - 1; i >= 0; i--) {
					design.addFrame(frames[i]);
				}

				for (int i = frames.length - 1; i >= 0; i--) {
					DesignEditorCmd.addIncidentTransitions(incidentTransitions[i]);
				}

				stateMgr.noteFramesEdit(frames, this);
			}

			@Override
			public void undo()
			{
				super.undo();

				recoverMgrs = true;

				if (incidentTransitions == null) {
					incidentTransitions =
						new Transition[frames.length][];

					for (int i = 0; i < frames.length; i++) {
						// Remove Transition instances for each Frame
						incidentTransitions[i] =
							frames[i].removeIncidentTransitions();
					}

					for (Frame frame : frames) {
						design.removeFrame(frame);
					}
				}
				else {
					for (Frame frame : frames) {
						frame.removeIncidentTransitions();
					}

					for (Frame frame : frames) {
						design.removeFrame(frame);
					}
				}

				stateMgr.noteFramesEdit(frames, this);
			}

			@Override
			public void die()
			{
				super.die();

				if (recoverMgrs) {
					for (Frame frame : frames) {
						recoverManagers(frame);
					}
				}
			}
		};
	} // createDuplicateFramesEdit

	/**
	 * Internal class for storing image data as a byte array
	 * along with its associated bounding rectangle
	 *
	 * @author jcorn
	 */
	protected static class ImageData
	{
		public DoubleRectangle bounds;
		public byte[] data;
		public String imageURL;

		public ImageData(DoubleRectangle bds, byte[] imgData, String url)
		{
			bounds = bds;
			data = imgData;
			imageURL = url;
		}
	}

	/**
	 * Returns a Map mapping IFrames to their current background image
	 * and bounds in an ImageData object
	 *
	 * @param frames an array of IFrames
	 * @return a Hashtable mapping IFrames to their current background images
	 */
	protected Map<Frame, ImageData> getBackgroundImageData(Frame[] frames)
	{
		Map<Frame, ImageData> previousImagesData =
			new HashMap<Frame, ImageData>();

		for (Frame f : frames) {
			String imgPath =
				(String) f.getAttribute(WidgetAttributes.IMAGE_PATH_ATTR);

			// Save previous data for undo operation
			previousImagesData.put(f,
					new ImageData(f.getBackgroundBounds(),
							f.getBackgroundImage(),
							imgPath));
		}

		return previousImagesData;
	}

	/**
	 * Sets the background image for the array of frames
	 *
	 * @param frames an array of IFrames
	 * @param imageData the new image, or null if none
	 * @param imageURL the file path of the new image, or
	 * <code>WidgetAttributes.NO_IMAGE</code>
	 */
	protected void setBackgroundImageOnFrames(final Frame[] frames,
			final byte[] imageData,
			final String imageURL,
			CogToolLID editLID)
	{
		try {
			// compute the size of the new image.
			final DoubleRectangle imageSize =
				GraphicsUtil.getImageBounds(imageData);

			// save previous data for undo/redo
			final Map<Frame, ImageData> previousImageData =
				getBackgroundImageData(frames);

			// do operation on all frames
			for (Frame frame : frames) {
				frame.setBackgroundImage(imageData, imageSize);
				frame.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
						imageURL);
			}

			// Add the undo edit
			IUndoableEdit edit = new AUndoableEdit(editLID)
			{
				@Override
				public String getPresentationName()
				{
					return (imageData == null) ? removeBackgroundImage
							: setBackgroundImage;
				}

				@Override
				public void redo()
				{
					super.redo();

					// Set background image to new image data for all
					// selected frames
					try {
						for (Frame frame : frames) {
							frame.setBackgroundImage(imageData, imageSize);
							frame.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
									imageURL);
						}
					}
					catch (GraphicsUtil.ImageException ex) {
						throw new RcvrImageException("Redo set background image failed",
								ex);
					}
				}

				@Override
				public void undo()
				{
					super.undo();

					try {
						// iterate through frames
						Iterator<Entry<Frame, ImageData>> imageEntryIterator =
							previousImageData.entrySet().iterator();

						while (imageEntryIterator.hasNext()) {
							Entry<Frame, ImageData> imageEntry =
								imageEntryIterator.next();
							Frame f = imageEntry.getKey();
							ImageData id = imageEntry.getValue();

							f.setBackgroundImage(id.data, id.bounds);
							f.setAttribute(WidgetAttributes.IMAGE_PATH_ATTR,
									id.imageURL);
						}
					}
					catch (GraphicsUtil.ImageException ex) {
						throw new RcvrImageException("Undo set background image failed",
								ex);
					}
				}
			};

			undoMgr.addEdit(edit);
			for (Frame frame : frames) {
				UndoManager frameMgr =
					UndoManager.getUndoManager(frame,
							project);
				frameMgr.addEdit(edit);
			}
		}
		catch (GraphicsUtil.ImageException e) {
			// setBackgroundImage and GraphicsUtil.getImageBounds may
			// throw ImageException, translating any other exception.
			interaction.protestInvalidImageFile();
		}
	}

	/**
	 * Returns a Map mapping IFrames to their current widget color
	 *
	 * @param frames an array of IFrames
	 * @return Map mapping IFrames to their current widget color
	 */
	protected Map<Frame, Integer> getWidgetColorData(Frame[] frames)
	{
		Map<Frame, Integer> widgetColorData = new HashMap<Frame, Integer>();

		for (Frame frame : frames) {
			widgetColorData.put(frame,
					new Integer(frame.getWidgetColor()));
		}

		return widgetColorData;
	}

	/**
	 * Sets the widget color on all widgets contained within the given frames
	 *
	 * @param frames an array of IFrames
	 * @param color integer containing new widget color
	 */
	protected void setWidgetColorForFrames(final Frame[] frames,
			final int color)
	{
		// save previous data for undo/redo
		final Map<Frame, Integer> previousWidgetColorData =
			getWidgetColorData(frames);

		// do operation on all frames
		for (Frame frame : frames) {
			frame.setWidgetColor(color);
		}

		// Add the undo edit
		IUndoableEdit edit = new AUndoableEdit(DesignEditorLID.SetWidgetColor)
		{
			@Override
			public String getPresentationName()
			{
				return setWidgetColor;
			}

			@Override
			public void redo()
			{
				super.redo();

				// Set background image to new image data for all
				// selected frames
				for (Frame frame : frames) {
					frame.setWidgetColor(color);
				}
			}

			@Override
			public void undo()
			{
				super.undo();

				// Iterate through frames, resetting widget colors
				// to old values
				Iterator<Entry<Frame, Integer>> colorEntryIterator =
					previousWidgetColorData.entrySet().iterator();

				while (colorEntryIterator.hasNext()) {
					Entry<Frame, Integer> colorEntry =
						colorEntryIterator.next();
					Frame f = colorEntry.getKey();
					int oldColor = colorEntry.getValue().intValue();

					f.setWidgetColor(oldColor);
				}
			}
		};

		undoMgr.addEdit(edit);

		for (Frame frame : frames) {
			UndoManager frameMgr =
				UndoManager.getUndoManager(frame,
						project);
			frameMgr.addEdit(edit);
		}
	}

	/**
	 * Support for sorting image file names alphabetically
	 */
	protected static final Comparator<File> fileNameAlphabetizer =
		new Comparator<File>() {

		public int compare(File f1, File f2)
		{
			String name0 = f1.getName();
			String name1 = f2.getName();

			return name0.compareTo(name1);
		}
	};

	/**
	 * A threaded object which is responsible for generating
	 * loading of images and creating frames with the images as background.
	 */
	protected class ImportImageDirThread extends CogToolWorkThread
	{
		protected final String title = L10N.get("WDX.ImportingImageDirectory",
		"Importing Images ...");
		protected ProgressBar progressBar =
			interaction.createProgressBar(title,
					this,
					title,
					ProgressBar.SMOOTH);

		protected File[] imageList;
		protected Frame[] createdFrames;
		protected double frameX;
		protected double frameY;
		// If doWork() fails to complete by throwing an Exception, we don't want
		// to try to install the frames, so we use finished to say whether or not
		// doWork() completed normally.
		protected boolean finished = false;
		protected Frame frameToDelete;

		protected CompoundUndoableEdit editSequence =
			new CompoundUndoableEdit(importBackgroundImages,
					DesignEditorLID.ImportImageDirectory);

		// Used for computing progress percentage
		protected double completedCount = 0.0;

		public ImportImageDirThread(File[] images,
				double originX,
				double originY,
				Frame frameBeingDeleted)
		{
			super();

			if (images == null) {
				throw new RcvrImageException("Required images array was null");
			}

			imageList = images;
			frameX = originX;
			frameY = originY;

			// Sort the images by name
			Arrays.sort(imageList, fileNameAlphabetizer);

			createdFrames = new Frame[imageList.length];

			frameToDelete = frameBeingDeleted;

			setProgressCallback(progressBar, true);
			setDisabler(progressBar.getDisabler());
		}


		public void doWork()
		{
			// Performed by child thread
			byte[] imageData;
			DoubleRectangle imageSize;
			int i = 0;

			Set<DeviceType> designDeviceTypes = design.getDeviceTypes();
			int minFrameWidth =
				DesignUtil.getFrameMinWidth();
			int minFrameHeight =
				DesignUtil.getFrameMinHeight();
			double frameScale =
				DesignUtil.getFrameScaleFactor();
			DesignUtil.IFrameSituator frameSituator =
				new DesignUtil.ByRowFrameSituator(frameX,
						frameY,
						16.0,
						16.0,
						minFrameWidth,
						minFrameHeight,
						CogToolPref.FRAMES_PER_ROW.getInt(),
						frameScale);

			while ((! isCanceled()) && (i < imageList.length)) {
				try {
					File imgFile = imageList[i];

					// Set the background to new image
					imageData =
						GraphicsUtil.loadImageFromFile(imgFile.getAbsolutePath());
					imageSize = GraphicsUtil.getImageBounds(imageData);

					// Strip extension off the filename to get the frame name
					String fileName = imgFile.getName();
					Matcher imageFileNameMatcher =
						IMAGE_FILE_NAME.matcher(fileName);

					if (! imageFileNameMatcher.matches()) {
						throw new RcvrImageException(
								"Formerly matching image file name no longer matches: "
								+ fileName);
					}

					// Create new frame & add it to design
					Frame frame = new Frame(imageFileNameMatcher.group(1),
							designDeviceTypes);

					frame.setBackgroundImage(imageData, imageSize);

					frameSituator.situateNextFrame(frame);

					createdFrames[i] = frame;

					// Update current progress
					completedCount += 1.0;
					setProgress(completedCount / imageList.length,
							fileName);

					// Prepare for next frame
					i++;
				}
				catch (GraphicsUtil.ImageException e) {
					addWorkException(new RcvrImageException(e));
				}
				catch (IOException e) {
					addWorkException(new RcvrIOException(e));
				}
			}

			finished = true;
		}

		@Override
		public void doneCallback()
		{
			// Performed by the main UI thread

			// If canceled or failed to complete normally, don't
			// make the changes
			if ((! isCanceled()) &&
					finished &&
					(createdFrames.length > 0))
			{
				if (frameToDelete != null) {
					deleteFrame(frameToDelete,
							DesignEditorLID.ImportImageDirectory,
							editSequence);
				}

				double maxY = 0.0;
				Frame lastFrame = null;

				Iterator<Frame> existingFrames = design.getFrames().iterator();

				while (existingFrames.hasNext()) {
					Frame f = existingFrames.next();
					double curY = ui.getFrameDisplayBounds(f).y;

					if (curY > maxY) {
						maxY = curY;
						lastFrame = f;
					}
				}

				if (lastFrame != null) {
					maxY += ui.getFrameDisplayBounds(lastFrame).height + 16.0;
				}

				for (Frame createdFrame : createdFrames) {
					makeFrameNameUnique(createdFrame);
					addFrame(createdFrame, editSequence);

					if (maxY > 0.0) {
						createdFrame.moveFrameOrigin(0.0, maxY);
					}
				}

				editSequence.end();

				undoMgr.addEdit(editSequence);
			}

			RcvrExceptionHandler.recoverWorkThread(this, interaction);

			progressBar.dispose();
		}
	}

	protected IListenerAction createImportImageDirectory()
	{
		return new AListenerAction()
		{

			public boolean performAction(Object actionParms)
			{
				// Ask for a directory of images
				String filePath = interaction.askForImageDir();
				if (filePath != null) {
					File dir = new File(filePath);
					if (dir.isDirectory()) {
						final File[] files =
							dir.listFiles(new FileFilter() {

								public boolean accept(File pathname)
								{
									String name = pathname.getName();
									Matcher imageFileNameMatcher =
										IMAGE_FILE_NAME.matcher(name);

									if (imageFileNameMatcher.matches()) {
										// Test for Mac resource forks
										if (name.startsWith("._")) {
											String realName = name.substring(2);
											String fullPath =
												pathname.getParent()
												+ FileUtil.FILE_SEP
												+ realName;

											return ! new File(fullPath).exists();
										}

										return true;
									}

									return false;
								}
							});

						// Find an unoccupied starting position by cascading.
						DoublePoint origin = new DoublePoint(10.0, 10.0);

						// Check if only the default frame is present;
						// if so, and it hasn't been significantly modified or
						// used, indicate that it should be deleted when
						// the images are imported.
						Frame frameToDelete =
							design.getFrame(DesignEditorController.INITIAL_FRAME_NAME);

						if ((frameToDelete == null) ||
								(design.getFrames().size() > 1) ||
								(frameToDelete.getWidgets().size() > 0) ||
								(frameToDelete.getBackgroundImage() != null))
						{
							frameToDelete = null;
						}

						ImportImageDirThread workThread =
							new ImportImageDirThread(files, origin.x, origin.y,
									frameToDelete);
						ThreadManager.startNewThread(workThread);
					}
				}

				return true;
			}
		};
	}

	protected IListenerAction createSetSkinAction(final SkinType newSkin,
			final CogToolLID lid)
	{
		return new AListenerAction() {

			public boolean performAction(Object prms)
			{
				final SkinType oldSkin = design.getSkin();

				design.setSkin(newSkin);

				undoMgr.addEdit(new AUndoableEdit(lid)
				{
					@Override
					public String getPresentationName()
					{
						return changeSkin;
					}

					@Override
					public void redo()
					{
						super.redo();

						design.setSkin(newSkin);
					}

					@Override
					public void undo()
					{
						super.undo();

						design.setSkin(oldSkin);
					}
				});

				return true;
			}
		};
	}

	protected IListenerAction createRenderAllAction(final boolean render,
	                                                final CogToolLID lid)
	{
	    return new AListenerAction() {
	        public boolean performAction(Object prms)
	        {
	            FrameEditorController.renderUnRenderAll(design,
	                                                    render,
	                                                    lid,
	                                                    DesignEditorController.this.undoMgr);
	            return true;
	        }
	    };
	}

	protected IListenerAction createChangeDelayAction()
	{
		return new IListenerAction()
		{

			public Class<?> getParameterClass()
			{
				return DesignEditorUI.ChangeDelayParameters.class;
			}


			public boolean performAction(Object actionParms)
			{
				DesignEditorUI.ChangeDelayParameters prm = (DesignEditorUI.ChangeDelayParameters) actionParms;
				int numTransitions = prm.selection.getSelectedTransitionCount();

				if (numTransitions == 0) {
					interaction.protestNoSelection();
					return false;
				}

				if (numTransitions > 1) {
					interaction.protestMultipleTransitionSelection();
					return false;
				}

				final Transition transition =
					prm.selection.getSelectedTransitions()[0];
				final double oldDelayInSecs = transition.getDelayInSecs();
				final String oldDelayLabel = transition.getDelayLabel();

				if ((oldDelayInSecs != prm.delayInSecs) ||
						! oldDelayLabel.equals(prm.delayLabel))
				{
					final double newDelayInSecs = prm.delayInSecs;
					final String newDelayLabel = prm.delayLabel;

					transition.setDelayInfo(newDelayInSecs, newDelayLabel);

					DemoStateManager.IDesignUndoableEdit edit =
						new DemoStateManager.ObsoletingEdit(DesignEditorLID.ChangeDelay,
								demoStateMgr)
					{
						@Override
						public String getPresentationName()
						{
							return changeDelay;
						}

						@Override
						public void redo()
						{
							super.redo();

							transition.setDelayInfo(newDelayInSecs,
									newDelayLabel);

							stateMgr.noteTransitionEdit(transition,
									this);

							if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
								DemoScriptCmd.regenerateDesignScripts(project,
										design,
										interaction);
							}
						}

						@Override
						public void undo()
						{
							super.undo();

							transition.setDelayInfo(oldDelayInSecs,
									oldDelayLabel);

							stateMgr.noteTransitionEdit(transition,
									this);

							if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
								DemoScriptCmd.regenerateDesignScripts(project,
										design,
										interaction);
							}
						}
					};

					demoStateMgr.noteTransitionEdit(transition, edit);
					undoMgr.addEdit(edit);

					if (CogToolPref.REGENERATE_AUTOMATICALLY.getBoolean()) {
						DemoScriptCmd.regenerateDesignScripts(project,
								design,
								interaction);
					}

				}

				return true;
			}
		};
	}

    /**
     * Creates a new DesignEditorController instance for editing an existing
     * Design instance.
     *
     * @param design the Design instance to edit.
     * @param project the Project containing the design
     * @return the Controller instance for editing the given Design instance
     * @author mlh
     */
    public static DesignEditorController openController(Design design,
                                                 Project project)
    {
        DesignEditorController controller =
            (DesignEditorController)
                ControllerRegistry.ONLY.findOpenController(design);

        // If already open, just bring it to front
        if (controller != null) {
            controller.takeFocus();
        }
        else {
            controller = new DesignEditorController(design, project);

            ControllerRegistry.ONLY.addOpenController(controller);
        }

        return controller;
    }

}
