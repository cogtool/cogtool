/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2013 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * jopt-simpler
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.cmu.cs.hcii.cogtool.CogTool;
import edu.cmu.cs.hcii.cogtool.CogToolPref;
import edu.cmu.cs.hcii.cogtool.util.Cancelable;
import edu.cmu.cs.hcii.cogtool.util.FileUtil;
import edu.cmu.cs.hcii.cogtool.util.IAttributed;
import edu.cmu.cs.hcii.cogtool.util.KeyDisplayUtil;
import edu.cmu.cs.hcii.cogtool.util.KeyboardUtil;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.LispUtil;
import edu.cmu.cs.hcii.cogtool.util.NamedObject;
import edu.cmu.cs.hcii.cogtool.util.ObjectLoader;
import edu.cmu.cs.hcii.cogtool.util.ObjectSaver;
import edu.cmu.cs.hcii.cogtool.util.ProcessTraceCallback;
import edu.cmu.cs.hcii.cogtool.util.StringUtil;
import edu.cmu.cs.hcii.cogtool.util.Subprocess;

// TODO the model file we write, while capable of being run standalone, still
// contains lots of detritus from our old structure; it should be tidied up
// to look like decent ACT-R code someone might write by hand, or at least as
// close to that as we can conveniently manage.

public class ACTRPredictionAlgo extends APredictionAlgo
{
    // The following should be incremented every time changes are made that
    // make the trace sufficiently different that visualization will fail.
    public static final int TRACE_VERSION = 2;

    public static final String USES_OBSOLETE_WAITS =
        "ACTRPredictionAlgo.usesObsoleteWaits";
    public static final String TRACE_VERSION_ATTR =
        "ACTRPredictionAlgo.traceVersion";
    
    private static final double ARTICULATION_TIME_PER_CHAR = 0.050; // seconds
    private static final double BACKGROUND_EFFORT = 0.005; // seconds
    // whether to emit a "keep alive" production--it's not clear whether
    // or not we're going to want this for real, but even if not it's is
    // convenient to be able to turn it on easily for debugging
    private static final boolean EMIT_KEEP_ALIVE = false;
    private static final String BACK_BUTTON_SEPARATOR = "[\n\r]+";
    
    private static final String CTE_LOG_FILE = "cogtool-explorer.log";

    // We preserve the old value here as it appears it may be needed
    // for reading really old .cgt files. Bit of a mystery, really.
    public static final int OLD_TIMEOUT_VALUE = 600;

    private static final String ACTR_BOILERPLATE =
        "edu/cmu/cs/hcii/cogtool/resources/cogtool-actr.lisp";
    private static final String CT_EXPLORER_SUPPORT =
        "edu/cmu/cs/hcii/cogtool/resources/ct-explorer-support.lisp";
    private static final String CT_EXPLORER_MODEL =
        "edu/cmu/cs/hcii/cogtool/resources/ct-explorer-model.lisp";
    private static final String EMMA =
            "edu/cmu/cs/hcii/cogtool/resources/emma.lisp";
    
    private static final double TIME_EQUALITY_TOLERANCE = 0.0001;
    
    public static boolean emitVirtualFrames = false;
    
    private static boolean equalTimes(Double t1, Double t2) {
        return Math.abs(t1 - t2) <= TIME_EQUALITY_TOLERANCE;
    }
    
    private static double getActrTimeoutInSeconds() {
        return CogToolPref.ACTR_TIMEOUT.getInt() / 1000.0;
    }
    
    public static final int edu_cmu_cs_hcii_cogtool_model_ACTRPredictionAlgo_version = 0;

    private static ObjectSaver.IDataSaver<ACTRPredictionAlgo> SAVER =
        new ObjectSaver.ADataSaver<ACTRPredictionAlgo>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_ACTRPredictionAlgo_version;
            }

            @Override
            public void saveData(ACTRPredictionAlgo v, ObjectSaver saver)
                throws java.io.IOException
            { }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(ACTRPredictionAlgo.class.getName(), SAVER);
    }

    private static ObjectLoader.IObjectLoader<ACTRPredictionAlgo> LOADER =
        new ObjectLoader.AObjectLoader<ACTRPredictionAlgo>();

    // This object will always be ignored when used during loading
    // of scripts and results (see TaskApplication version 0 loading)
    private static final ACTRPredictionAlgo StandaloneAlgo_ONLY =
        new ACTRPredictionAlgo();

    private static ObjectLoader.IObjectLoader<ACTRPredictionAlgo> StandaloneAlgo_LOADER =
        new ObjectLoader.AObjectLoader<ACTRPredictionAlgo>() {
            @Override
            public ACTRPredictionAlgo createObject()
            {
                return StandaloneAlgo_ONLY;
            }
        };

    // The last known version for StandaloneAlgo
    private static final int edu_cmu_cs_hcii_cogtool_model_StandaloneAlgo_version = 0;


    public static void registerLoader()
    {
        ObjectLoader.registerLoader(ACTRPredictionAlgo.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_ACTRPredictionAlgo_version,
                                    LOADER);

        ObjectLoader.registerLoader("edu.cmu.cs.hcii.cogtool.model.StandaloneAlgo",
                                    edu_cmu_cs_hcii_cogtool_model_StandaloneAlgo_version,
                                    StandaloneAlgo_LOADER);
    }

    protected ACTRPredictionAlgo() { }

    static {
        IAttributed.AttributeRegistry.ONLY.defineAttribute(USES_OBSOLETE_WAITS,
                                                           Boolean.class,
                                                           Boolean.FALSE);

        IAttributed.AttributeRegistry.ONLY.defineAttribute(TRACE_VERSION_ATTR,
                                                           Integer.class,
                                                           new Integer(0));
    }

    // Use of this static method should guarantee that the attribute has
    // been defined, since the class must be loaded to call this.
    public static boolean usesObsoleteWaits(APredictionResult r)
    {
        return ((Boolean) r.getAttribute(USES_OBSOLETE_WAITS)).booleanValue();
    }

    // Use of this static method should guarantee that the attribute has
    // been defined, since the class must be loaded to call this.
    public static int getTraceVersion(APredictionResult r) {
        return ((Integer) r.getAttribute(TRACE_VERSION_ATTR)).intValue();
    }

    protected class ACTRAnalysisOutput implements IAnalysisOutput
    {
        protected Script script;
        protected List<String> traceLines;
        protected List<String> errorLines;
        protected boolean usesObsoleteWaits;

        public ACTRAnalysisOutput(Script s,
                                  List<String> traces,
                                  List<String> errors,
                                  boolean usesObsWaits)
        {
            script = s;
            traceLines = traces;
            errorLines = errors;
            usesObsoleteWaits = usesObsWaits;
        }

        public APredictionResult completeWork()
        {
            int lineCount = traceLines.size();

            TimePredictionResult result = null;

            if (lineCount > 0) {
                try {
                    // Parse trace and return result with steps
                    String lastLine = traceLines.get(lineCount - 1);
                    double taskTime = Double.parseDouble(lastLine);

                    TraceParser<ResultStep> parser = getTraceParser();

                    List<ResultStep> resultSteps =
                    	parser.parseTrace(traceLines);

//                  resultSteps = TraceReducer.simplifyTrace(resultSteps);

                    if (! equalTimes(taskTime, (double)OLD_TIMEOUT_VALUE)) {
                        result = new TimePredictionResult("ACT-R Time: " + taskTime,
                                                          script,
                                                          ACTRPredictionAlgo.this,
                                                          traceLines,
                                                          errorLines,
                                                          resultSteps,
                                                          taskTime);
                    }
                    else {
                        result = new TimePredictionResult("ACT-R Model Timeout",
                                                          script,
                                                          ACTRPredictionAlgo.this,
                                                          traceLines,
                                                          errorLines,
                                                          resultSteps);
                    }
                }
                catch (NumberFormatException e) {
                    result = new TimePredictionResult("ACT-R Parse Error",
                                                      script,
                                                      ACTRPredictionAlgo.this,
                                                      traceLines,
                                                      errorLines);
                }
            }
            else {
                result = new TimePredictionResult("ACT-R Error",
                                                  script,
                                                  ACTRPredictionAlgo.this,
                                                  traceLines,
                                                  errorLines);
            }

            result.setAttribute(TRACE_VERSION_ATTR, new Integer(TRACE_VERSION));
            if (usesObsoleteWaits) {
                result.setAttribute(USES_OBSOLETE_WAITS, Boolean.TRUE);
            }
            return result;
        }
    }

    protected class ACTRAnalysisInput extends AAnalysisInput
    {
        protected Script script;
        protected String lispMem;
        protected File file;
        protected String cmd;
        protected boolean usesObsoleteWaits;

        public ACTRAnalysisInput(Script s,
                                 String imageFile,
                                 String inputFile,
                                 String cmdStr,
                                 boolean usesObsWaits)
        {
            script = s;
            lispMem = imageFile;
            file = new File(inputFile);
            cmd = cmdStr;
            usesObsoleteWaits = usesObsWaits;
        }


        public IAnalysisOutput compute(ProcessTraceCallback progressCallback,
                                       Cancelable cancelable)
        {
            // Ignore callback until we can figure it out.
            List<String> traceLines = new ArrayList<String>();
            List<String> errorLines = new ArrayList<String>();
            List<File> files = new ArrayList<File>(1);
            files.add(file);

            if (!usesObsoleteWaits) {
                // Execute clisp, loading stored memory image and temp files
                try {
                    Subprocess.execLisp(lispMem,
                                        files,
                                        cmd,
                                        traceLines,
                                        errorLines,
                                        progressCallback,
                                        cancelable); // ignore return value
                }
                catch (Subprocess.ExecuteException ex) {
                    throw new ComputationException("Executing LISP failed", ex);
                }
            }

            return new ACTRAnalysisOutput(script,
                                          traceLines,
                                          errorLines,
                                          usesObsoleteWaits);
        }
    }

    protected static class ObsoleteWaitException extends RuntimeException
    {
        public ObsoleteWaitException()
        {
            super("Contains obsolete wait steps");
        }
    }

    @Override
    public IAnalysisInput prepareComputation(Script script)
    {
        Demonstration demonstration = script.getDemonstration();
        boolean usesObsoleteWaits = false;

        String path = script.getAssociatedPath();
        File actrFile = null;
        if (path != null) {
            actrFile = new File(path);
        }
        else {
            try {
                // Create a temp file to hold the ACT-R model
                actrFile = File.createTempFile("cogtool-actr-model-", ".lisp");
                actrFile.deleteOnExit();
                TaskApplication ta = demonstration.getTaskApplication();
                outputModel(ta.getDesign(),
                            ta.getTask(),
                            demonstration.getStartFrame(),
                            script,
                            actrFile,
                            null);
            }
            catch (ObsoleteWaitException e) {
                usesObsoleteWaits = true;
            }
            catch (IOException e) {
                throw new ComputationException("IOException creating ACT-R model", e);
            }
        }

        return new ACTRAnalysisInput(script,
                                     "actr6.mem",
                                     actrFile.getAbsolutePath(),
                                     "*cogtool-result*",
                                     usesObsoleteWaits);
    }

    /**
     * Write a Lisp representation of this design to a PrintWriter.
     * @param d the design to output
     * @param startFrame the frame that should be initially active
     * @param out the output PrintWriter
     */
    protected static void outputDesign(Design d, PrintWriter out)
    {
        out.println("(terpri)");
        if (CogToolPref.isTraceEmitted()) {
            out.println("(princ \"Cogtool " +
                        LispUtil.clean(CogTool.getVersion()) +
                        "\")");
        } else {
            out.println("(setq *suppress-trace* t)");
            out.println("(defparameter *overridden-global-parameters* " 
                        + "'(:v nil :trace-detail low :use-tree t :ncnar nil :dcnn nil))");
        }
        if (CogToolPref.RESEARCH.getBoolean()) {
            out.println("(setq *cogtool-debug* " + 
                        CogToolPref.ACTR_DEBUG_LEVEL.getInt() +
                        ")");
        }
        boolean useACTRDefaults = 
                (!CogToolPref.RESEARCH.getBoolean() || !CogToolPref.ACTR_ALTERNATIVE_PARAMETERS.getBoolean());
        out.format(Locale.US,
                   ("(setq *overridden-global-parameters* `(" +
                    ":visual-attention-latency %.3f :motor-initiation-time %.3f :peck-fitts-coeff %.3f :dat %.3f " +
                    ",@*overridden-global-parameters*))\n"),
                    (useACTRDefaults ? CogToolPref.VISUAL_ATTENTION.getIntDefault() : CogToolPref.VISUAL_ATTENTION.getInt()) / 1000.0,
                    (useACTRDefaults ? CogToolPref.MOTOR_INITIATION.getIntDefault() : CogToolPref.MOTOR_INITIATION.getInt()) / 1000.0,
                    (useACTRDefaults ? CogToolPref.PECK_FITTS_COEFF.getIntDefault() : CogToolPref.PECK_FITTS_COEFF.getInt()) / 1000.0,
                    (useACTRDefaults ? CogToolPref.ACTR_DAT.getIntDefault() : CogToolPref.ACTR_DAT.getInt()) / 1000.0);
        out.println();
        
        out.println(";; ==== Design ====\n");

        out.println("(defvar *frame-definitions* nil)\n");

        // Create the frames
        Iterator<Frame> framesIt = d.getFrames().iterator();
        int i = 0;

        while (framesIt.hasNext()) {
            Frame frame = framesIt.next();
            String fn = "cogtool-frame-fn-" + (++i);
            out.println("(defun " + fn + " ()");
            out.println("\n  (let ((frames (frames *cogtool-design*)) frame widget groups)");
            outputFrame(frame, out);
            out.println("\n\n    (setf (gethash "
                               + LispUtil.safeString(frame.getName())
                               + " frames) frame)))\n");
            out.println("(push #'" + fn + " *frame-definitions*)\n");
        }

        out.println("(setq *frame-definitions* (nreverse *frame-definitions*))\n");

        out.println("(defun define-design ()");
        out.println("  (setq *cogtool-design* (make-instance 'cogtool-device))");
        out.println("  (loop for f in *frame-definitions* do (funcall f)))\n");
        
        out.println("(when *cogtool-random-seed*");
        out.println("  (setf (getf *default-global-parameters* :seed) *cogtool-random-seed*))\n\n");
    }
    
    private static Set<String> backButtonLabels = null;
    
    public void outputModel(Design design,
                            AUndertaking task,
                            Frame startFrame,
                            Script script,
                            File file,
                            Map<String, Object> variablesToDefine)
        throws IOException
    {
        backButtonLabels = new HashSet<String>(
                Arrays.asList(CogToolPref.CTE_DEFAULT_BACK_LABEL.getString().split(BACK_BUTTON_SEPARATOR)));
        PrintWriter w = null;
        try {
            w = new PrintWriter(
                   new BufferedWriter(
                      new OutputStreamWriter(
                          new FileOutputStream(file),
                          "US-ASCII")));
            if (CogToolPref.SYSWVO.getBoolean()) {
                w.println("(defparameter *system-wait-blocks-only-vision* t)\n");
            }
            if (variablesToDefine != null) {
                // Note that the Objects that are values in the map should
                // be clean for printing into a form Lisp will read; integers
                // and doubles are fine, strings and symbols may need to be
                // LispUtil.clean'd+quoted or LispUtil.safeString'd before being
                // inserted into the Map.
                for (String k : variablesToDefine.keySet()) {
                    w.print("(defparameter " + k + " ");
                    Object val = variablesToDefine.get(k);
                    if (val == null) {
                        w.print("nil");
                    }
                    else if (val instanceof Collection<?>) {
                        w.print(LispUtil.makeList((Collection<?>) val));
                    }
                    else {
                        w.print(val);
                    }
                    w.println(")");
                }
                w.println();
            }
            w.println(";; For Design: " + design.getName());
            if (task != null) {
                w.println(";; For Task: " + task.getName() + "\n");
            }
            if (CogToolPref.USE_EMMA.getBoolean()) {
                w.println(";; EMMA\n");
                FileUtil.copyTextResourceToWriter(EMMA, w);
            }
            FileUtil.copyTextResourceToWriter(ACTR_BOILERPLATE, w);
            outputDesign(design, w);
            if (script != null) {
                outputScript(script, startFrame, w);
            }
            else if (design.getDeviceTypes().contains(DeviceType.Touchscreen)) {
                w.println("\n(setq *use-finger-default-value* t)");
            }
            w.println("\n(load-pending-cogtool-files)");
            w.println("\n(setq *cogtool-result* (cogtool-run-model))");
        }
        finally {
            if (w != null) {
                w.close();
            }
        }
    }
    
    // While in normal CogTool we take special care of clicks and taps
    // in exactly the same location, this logic is not available to CT-E,
    // which causes trouble, particularly with taps. Therefore as a temporary
    // kludge until we figure out a better way to deal with it, when emitting
    // a design for CT-E ensure that no widgets are at identical positions.
    private static List<DoublePoint>frameElementPositions = null;
    
    public void outputModel(Design design,
                            AUndertaking task,  // may be null if unknown or irrelevant
                            File file,
                            String scoresPath,
                            SNIFACTPredictionAlgo.SNIFACTParameters parms)
        throws IOException
    {
        Map<String, Object> lispVars = new HashMap<String, Object>();

        lispVars.put("*task-description*",
                     LispUtil.safeString(parms.taskName));
        lispVars.put("*number-of-runs*", new Integer(parms.numRuns));
        lispVars.put("*SNIF-ACT-k-value*", new Integer(parms.kValue));
        lispVars.put("*CogTool-Explorer-start-frame*",
                     LispUtil.safeString(parms.startFrame));

        List<String> tFrames = new ArrayList<String>();
        for (String s : parms.targetFrames)
        {
            tFrames.add(LispUtil.safeString(s));
        }
        lispVars.put("*CogTool-Explorer-target-frames*", tFrames);

        List<String> files = new ArrayList<String>();

        File f = File.createTempFile("cogtool-explorer-support-", ".lisp");
        f.deleteOnExit();
        FileUtil.copyResourceToFile(CT_EXPLORER_SUPPORT, f);
        String supportPath = f.getAbsolutePath();

        f = File.createTempFile("cogtool-explorer-model-", ".lisp");
        f.deleteOnExit();
        FileUtil.copyResourceToFile(CT_EXPLORER_MODEL, f);
        String modelPath = f.getAbsolutePath();

        files.add(LispUtil.safeString(supportPath));
        files.add(LispUtil.safeString(scoresPath));
        files.add(LispUtil.safeString(modelPath));
        lispVars.put("*cogtool-files-to-load*", files);
        
        File logFile = new File(CogToolPref.LOG_DIRECTORY.getString(), CTE_LOG_FILE);
        lispVars.put("*log-file*", LispUtil.safeString(logFile.getCanonicalPath()));
        lispVars.put("*CT-E_timeout*", Double.toString(getActrTimeoutInSeconds()));
        if (! CogToolPref.CTE_SUPPRESS_NOISE.getBoolean()) {
            lispVars.put("*cogtool-random-seed*", "nil");
        }
        int backButtonSemantics = CogToolPref.CTE_BACK_BUTTON_SEMANTICS.getInt();
        if (backButtonSemantics !=  SNIFACTPredictionAlgo.IMPLICIT_BACK &&
                backButtonSemantics != SNIFACTPredictionAlgo.NO_BACK) {
            lispVars.put("*use-back-button*", "t");
            lispVars.put("*use-back-button-history-in-half-flatten-layout*", "t");
        } else {
            lispVars.put("*use-back-button*", "nil");
            if (backButtonSemantics == SNIFACTPredictionAlgo.IMPLICIT_BACK) {
                lispVars.put("*allow-magic-go-backs*", "t");
            } else {
                lispVars.put("*allow-magic-go-backs*", "nil");
            }
        }
        String logDir = CogToolPref.LOG_DIRECTORY.getString();
        if (!logDir.endsWith("/")) {
            logDir += "/";
        }
        lispVars.put("*log-file-directory*", LispUtil.safeString(logDir));

        frameElementPositions = new ArrayList<DoublePoint>();
        outputModel(design, task, null, null, file, lispVars);
        frameElementPositions = null;
    }
    
    private static Map<SimpleWidgetGroup, FrameElement> simpleWidgetGroupOwners =
            new HashMap<SimpleWidgetGroup, FrameElement>();
    
    /**
     * Write a Lisp representation of this frame to a PrintWriter.
     * Uses the Lisp free variables frame, widget, frame-trans and
     * widget-trans, which should be bound in the current lexical scope.
     * @param out the output PrintWriter
     */
    private static void outputFrame(Frame f, PrintWriter out)
    {
        virtualFrames.clear(); // redundant, but extra safety
        Set<FrameElement> grps = new HashSet<FrameElement>();
        simpleWidgetGroupOwners.clear();
        out.println("\n  ;; ==== New Frame ====\n");

        // Create the frame
        out.print("    (setq frame (make-instance 'cogtool-frame :name "
                                + LispUtil.safeString(f.getName()));
        String st = f.getSpeakerText();
        double duration = f.getListenTimeInSecs();
        if (duration != Frame.NO_LISTEN_TIME) {
            st = articulateTextForDuration(st, duration);
        }
        if (st != null && st.length() > 0) {
            out.print(" :speaker-text " + LispUtil.safeString(st));
        }
        out.print("))");

        for (IWidget widget : f.getWidgets()) {
            if (widget.getWidgetType() == WidgetType.Noninteractive &&
                    StringUtil.isNullOrEmpty(widget.getTitle()) &&
                    StringUtil.isNullOrEmpty(widget.getAuxiliaryText()) &&
                    CogToolPref.CTE_SUPPRESS_NONINTERACTIVE.getBoolean()) {
                continue;
            }
            if (emitVirtualFrames) {
                if (widget instanceof MenuHeader || 
                        widget instanceof ContextMenu ||
                        widget instanceof PullDownHeader ||
                        (widget instanceof MenuItem && ((MenuItem)widget).isSubmenu())) { 
                    addVirtualFrame(f.getName(), (AParentWidget)widget); 
                } else if (widget instanceof MenuItem || widget instanceof PullDownItem) {
                    VirtualFrame vf = virtualFrames.get(((ChildWidget)widget).getParent());
                    if (vf != null) {
                        vf.items.add((ChildWidget)widget);
                    }
                    continue;
                }   
            }
            outputWidget(widget, grps, out);
            out.print("    (push widget (widgets frame))");
        }  
        
        out.println();
        out.println();
        out.print("    (setq groups '())");
        Set<FrameElement> emittedGrps = new HashSet<FrameElement>();
        while (! grps.isEmpty()) {
            Set<FrameElement> newGrps = new HashSet<FrameElement>();
            for (FrameElement g : grps) {
                if (! emittedGrps.contains(g)) {
                    outputFrameElement(g, null, null, f, newGrps, out);
                    out.print("    (push widget groups)");
                    emittedGrps.add(g);
                }
            }
            grps = newGrps;
        }
        
        out.println();
        out.println();
        out.print("    (resolve-widget-refs frame groups)");
        simpleWidgetGroupOwners.clear();
        
        for (VirtualFrame vf : virtualFrames.values()) {
            vf.outputSelf(out);
        }
        virtualFrames.clear();

    }
    
    private static class VirtualFrame {

        private final String name;
        private List<ChildWidget> items = new ArrayList<ChildWidget>();

        private VirtualFrame(String nm) {
            name = nm;
        }

        private void outputSelf(PrintWriter out) {
            out.print(String.format("\n\n    (let ((subframe (make-instance 'cogtool-frame :name %s)))",
                                    LispUtil.safeString(name)));
            for (ChildWidget it : items) {
                outputWidget(it, null, out);
                out.print("    (push widget (widgets subframe))");
            }
            out.print("\n      (setf (gethash "
                    + LispUtil.safeString(name)
                    + " frames) subframe)) ; end subframe");
        }
    }
    
    private static Map<AParentWidget, VirtualFrame> virtualFrames =
            new HashMap<AParentWidget, VirtualFrame>();
    
    private static void addVirtualFrame(String parentName, AParentWidget header) {
        virtualFrames.put(header,
                          new VirtualFrame(String.format("Virtual Subframe %d of %s",
                                                         (virtualFrames.size() + 1),
                                                         parentName)));
    }

    private static String articulateTextForDuration(String text, double duration)
    {
        int chars =
            Math.round((float) (duration / ARTICULATION_TIME_PER_CHAR));
        if (chars <= 0) {
            chars = 1;
        }
        if (text != null && chars <= text.length()) {
            return text.substring(0, chars);
        }
        StringBuilder result = new StringBuilder(chars);
        if (text != null) {
            result.append(text);
        }
        while (result.length() < chars) {
            result.append('X');
        }
        return result.toString();
    }


    // cogtool-device.lisp expects a set of string, widget type names.
    // Formerly, these were generated from the display names for the various
    // widget types, but that would break when we were localized, so these
    // fixed names are provided here instead.
    // XXXX: The case sensitive comparison of strings for types in
    //       cogtool-device.lisp probably isn't the right thing to be doing.
    //       Rather, we should probably be doing eq comparison on symbols,
    //       or something like that. This needs to be thought through further
    //       at some point.
    protected static final Map<WidgetType, String> widgetTypeNames =
    	new HashMap<WidgetType, String>(12);
    static {
        widgetTypeNames.put(WidgetType.Button, "cogtool-button");
        widgetTypeNames.put(WidgetType.Link, "cogtool-link");
        widgetTypeNames.put(WidgetType.ContextMenu, "cogtool-context-menu");
        widgetTypeNames.put(WidgetType.Menu, "cogtool-menu");
        widgetTypeNames.put(WidgetType.Submenu, "cogtool-submenu");
        widgetTypeNames.put(WidgetType.MenuItem, "cogtool-menu-item");
        widgetTypeNames.put(WidgetType.TextBox, "cogtool-text-box");
        widgetTypeNames.put(WidgetType.Text, "cogtool-text");
        widgetTypeNames.put(WidgetType.PullDownList, "cogtool-pull-down-list");
        widgetTypeNames.put(WidgetType.PullDownItem, "cogtool-pull-down-item");
        widgetTypeNames.put(WidgetType.ListBoxItem, "cogtool-list-box-item");
        widgetTypeNames.put(WidgetType.Radio, "cogtool-radio-button");
        widgetTypeNames.put(WidgetType.Check, "cogtool-checkbox");
        widgetTypeNames.put(WidgetType.Graffiti, "cogtool-graffiti");
        widgetTypeNames.put(WidgetType.Noninteractive,
                            "cogtool-non-interactive");
    }

    /**
     * Write a Lisp representation of this widget to a PrintWriter.
     * Uses the Lisp free variables widget and
     * widget-trans, which should be bound in the current lexical scope.
     * @param out the output PrintWriter
     */
    private static void outputWidget(IWidget widget,
                                       Set<FrameElement> allGrps,
                                       PrintWriter out)
    {
        outputFrameElement(widget,
                           widget.getTitle(),
                           widget.getWidgetType(),
                           widget.getFrame(),
                           allGrps,
                           out);
        
        // Note that we never actually use the transition stuff at all, and
        // don't need it for CogTool per se. However Leonghwee's SNIF-ACT stuff
        // does need this, so we should be sure to keep this alive.
        // Create transitions
        if (widget.getTransitions().size() > 0) {
            out.print("    (let ((widget-trans (transitions widget)))");
            for (Transition t : widget.getTransitions().values()) {
                AAction a = t.getAction();
                if (! (a instanceof ButtonAction || a instanceof TapAction)) {
                    // TODO For now we're just doing buttons, since that's all
                    //      Leonghwee needs; more needs to be added soon
                    continue;
                }

                out.print("\n      (setf (gethash ");
                outputAction(t.getAction(), out);
                out.print(" widget-trans)\n          ");
                outputTransition(t, out);
                out.print(")");
            }
            out.println(")");
        } else if (emitVirtualFrames && 
                     (widget instanceof MenuHeader || 
                         widget instanceof ContextMenu ||
                         widget instanceof PullDownHeader ||
                         (widget instanceof MenuItem && 
                             ((MenuItem)widget).isSubmenu()))) {
            out.print("    (let ((widget-trans (transitions widget)))");
            out.print("\n      (setf (gethash '((click left)) widget-trans)\n          ");
            outputTransition(virtualFrames.get(widget).name, out);
            out.println("))");
        }
    }
    
    /**
     * Write a Lisp representation of this widget to a PrintWriter.
     * Uses the Lisp free variables widget and
     * widget-trans, which should be bound in the current lexical scope.
     * @param out the output PrintWriter
     */
    private static void outputFrameElement(FrameElement fe,
                                           String title,
                                           WidgetType typ,
                                           Frame frame,
                                           Set<FrameElement> allGrps,
                                           PrintWriter out)
    {
        out.println();
        out.println();
        out.print("    (setf widget (make-instance 'cogtool-widget :name ");
        out.print(safeGlobalName(fe, frame));
        if (title != null && title.length() > 0) {
            out.print(" :title ");
            out.print(LispUtil.safeString(title));
        }
        out.println();

        String aux = fe.getTextualCue();
        if (aux != null && aux.length() > 0) {
            out.print("                                :textual-cue ");
            out.println(LispUtil.safeString(aux));
        }

        DoubleRectangle bounds = new DoubleRectangle(fe.getEltBounds());
        if (frameElementPositions != null) {
            while (isExisitingPosition(bounds)) {
                bounds.x += 10;
            }
            frameElementPositions.add(new DoublePoint(bounds.x, bounds.y));
        }

        String wTypeName = null;
        if (typ == null) {
            wTypeName = "cogtool-group";
        } else {
            wTypeName = widgetTypeNames.get(typ);
            if (wTypeName == null) {
                throw new ComputationException("Unknown widget type: "
                                           + typ.toString());
            }
        }

        out.print("                                :x ");
        out.print(bounds.getX());
        out.print(" :y ");
        out.print(bounds.getY());
        out.print(" :width ");
        out.print(bounds.getWidth());
        out.print(" :height ");
        out.println(bounds.getHeight());

        if (title != null && backButtonLabels.contains(title)) {
            out.println("                                :is-back-button t");
        }
        
        out.print("                                :wtype '");
        out.print(wTypeName);

        FrameElement remoteLabelOwner = fe.getRemoteLabelOwner();
        if (remoteLabelOwner != null) {
            IWidget remoteLabel =
                (IWidget) fe.getAttribute(WidgetAttributes.REMOTE_LABEL_ATTR);
            if (remoteLabel != null) {
                out.println();
                out.print("                                :has-remote-label ");
                out.print(safeGlobalName(remoteLabel));
            }
        }

        Set<FrameElementGroup> grps = fe.getEltGroups();
        if (grps != null && ! grps.isEmpty()) {
            allGrps.addAll(grps);
            out.println();
            out.print("                                :member-of-groups ");
            String sep = "'(";
            for (FrameElementGroup g : grps) {
                out.print(sep);
                sep = " ";
                out.print(safeGlobalName(g, frame));
            }
            out.print(")");
        }
        
        if (fe instanceof AParentWidget) {
            SimpleWidgetGroup swg = ((AParentWidget)fe).getChildren();
            if (swg != null) {
                simpleWidgetGroupOwners.put(swg, fe);
            }
        }
        
        if (fe instanceof SimpleWidgetGroup) {
            FrameElement owner = simpleWidgetGroupOwners.get(fe);
            if (owner != null) {
                out.println();
                out.print("                                :member-of-groups '(");
                out.print(safeGlobalName(((AParentWidget)owner).getParentGroup(), frame));
                out.print(")");
            }
        }
        
        if (fe instanceof IWidget) {
            IWidget widget = (IWidget)fe;
            SimpleWidgetGroup pgrp = widget.getParentGroup();
            if (pgrp != null && allGrps != null) {
                allGrps.add(pgrp);
                out.println();
                out.print("                                :member-of-groups '(");
                out.print(safeGlobalName(pgrp, frame));
                out.print(")");
            } 
        }

        out.println("))");
    }
    
    private static boolean isExisitingPosition(DoubleRectangle bnds) {
        for (DoublePoint p : frameElementPositions) {
            if (Math.abs(p.x - bnds.x) < 2.1 && Math.abs(p.y - bnds.y) < 2.1) {
                return true;
            }
        }
        return false;
    }
    
    public static void outputTransition(String destName, PrintWriter out) {
        out.print("(make-instance 'cogtool-transition :target \"");
        out.print(LispUtil.clean(destName));
        out.print("\")");
    }

    /**
     * Write a lisp representation of this transition to a PrintWriter.
     * @param out the output PrintWriter
     */
    public static void outputTransition(Transition t, PrintWriter out)
    {
        outputTransition(t.getDestination().getName(), out);
    }

    /**
     * Write a lisp representation of the trigger for this action to a
     * PrintWriter.
     * @param out the output PrintWriter
     */
    public static void outputAction(AAction a, final PrintWriter out)
    {
        if (! (a instanceof ButtonAction)) {
        	// TODO this works for now when all we care about is clicks and taps
        	//      on buttons, but we'll need to do something more sophisticated
        	//      when we support inferring transitions on the Lisp side
        	//      for all widgets x
        	out.print("'((click left))");
        	return;
        }

        ButtonAction ba = (ButtonAction) a;
        MouseButtonState btn = ba.getButton();
        MousePressType bpt = ba.getPressType();

        // Clicks are represented by lists of symbol-tuples
        String button = null;

        if (btn == MouseButtonState.Left) {
            button = "left";
        }
        else if (btn == MouseButtonState.Middle) {
            button = "middle";
        }
        else if (btn == MouseButtonState.Right) {
            button = "right";
        }
        else if (bpt != MousePressType.Hover || btn != null) {
            throw new IllegalStateException("Unrecognized MouseButtonState: "
                                                + btn);
        }

        // TODO: respond to mouse down/up in Lisp
        if (bpt == MousePressType.Click) {
            out.print("'((click " + button + "))");
        }
        else if (bpt == MousePressType.Double) {
            out.print("'((click " + button + ") (click" + button + "))");
        }
        else if (bpt == MousePressType.Triple) {
            out.print("'((click " + button +
                      ") (click" + button +
                      ") (click" + button +
                      "))");
        }
        else if (bpt == MousePressType.Down) {
            out.print("'((down-click " + button + "))");
        }
        else if (bpt == MousePressType.Up) {
            out.print("'((up-click " + button + "))");
        }
        else if (bpt == MousePressType.Hover) {
            out.print("'((hover))");
        }
        else {
            throw new IllegalStateException("Unrecognized MousePressType: "
                                                + bpt);
        }
    }

    /**
     * Encodes a character into the textual form that Lisp will read as what
     * ACT-R expects as the name of a key. This is some sort of atom. For a
     * digit it is the fixnum, and otherwise a symbol. For individual letters
     * it is a symbol whose name is just that letter, and otherwise a word
     * (such as "return") describing the key. ACT-R actually uses upper-case
     * letters to describe keys, even though they are unshifted; we currently
     * map both upper and lower case letters to the same key.
     *
     * @param val the character to encode as a key
     * @return the textual representation of the corresponding Lisp object that
     *         ACT-R uses to describe the key
     */
    protected static String encodeKey(char val, boolean strict)
    {
        // Unfortunately, Character.isDigit says 'true' for too many values!
        if (('0' <= val) && (val <= '9')) {
            return Character.toString(val);
        }

        // Unfortunately, Character.isLetter says 'true' for too many values!
        if (('a' <= val) && (val <= 'z')) {
            return Character.toString(Character.toUpperCase(val));
        }

        if (('A' <= val) && (val <= 'Z')) {
            return Character.toString(val);
        }

        switch (val) {
            // TODO: below several newly supported characters are mapped to
            //       other characters for testing; when I've put the correct
            //       support into ACT-R, these should be reverted to the
            //       real things
            case ' ': return "space";

            case ',': return "comma";
            case '.': return "period";
            case '/': return "slash";

            case ';': return "semicolon";
            case '\'': return "quote";

            case '`': return "backquote";
            case '-': return "hyphen";
            case '=': return "hyphen";  // "equals";

            case '\t':
            case KeyboardUtil.TAB_CHAR:
                return "tab";
            case '[':  return "P"; // "left-bracket";
            case ']':  return "P"; // "right-bracket";
            case '\\': return "backslash";

            case '\r':
            case '\n':
            case KeyboardUtil.CR_CHAR:
                return "return";

            case KeyboardUtil.CAPSLOCK_CHAR:
                return "Z"; // "caps-lock";

            // TODO: perhaps backspace and delete should be different "keys"?
            case '\b':
            case '\177':
            case KeyboardUtil.BS_CHAR:
            case KeyboardUtil.DEL_CHAR:
                return "delete";

            case KeyboardUtil.MENU_CHAR: // return "menu-key";
                return ".";

            case KeyboardUtil.ESC_CHAR:   return "Q"; // "escape";
            case KeyboardUtil.SHIFT_CHAR: // return "left-shift";
            case KeyboardUtil.LEFT_SHIFT: // return "left-shift";
            case KeyboardUtil.CTRL_CHAR:  // return "left-ctrl";
            case KeyboardUtil.LEFT_CTRL: // return "left-ctrl";
            case KeyboardUtil.ALT_CHAR:   // return "left-alt";
            case KeyboardUtil.LEFT_ALT: // return "left-alt";
            case KeyboardUtil.COMMAND_CHAR:  // return "left-cmd";
            case KeyboardUtil.LEFT_COMMAND: // return "left-cmd";
            case KeyboardUtil.FUNCTION_CHAR: // return "left-fn";
            case KeyboardUtil.LEFT_FUNCTION: // return "left-fn";
                return "Z";

            case KeyboardUtil.RIGHT_SHIFT: // return "right-shift";
            case KeyboardUtil.RIGHT_CTRL: // return "right-ctrl";
            case KeyboardUtil.RIGHT_ALT: // return "right-alt";
            case KeyboardUtil.RIGHT_COMMAND: // return "right-cmd";
            case KeyboardUtil.RIGHT_FUNCTION: // return "right-fn";
                return "/";

            case KeyboardUtil.UP_ARROW_CHAR: // return "up-arrow";
            case KeyboardUtil.DOWN_ARROW_CHAR: // return "down-arrow";
            case KeyboardUtil.LEFT_ARROW_CHAR: // return "left-arrow";
            case KeyboardUtil.RIGHT_ARROW_CHAR: // return "right-arrow";
                return "slash";

            default:
                if (strict) {
                    throw new ComputationException("Unsupported character: " +
                                                    val + " (" + ((int) val) + ")");
                } else {
                    return "X";
                }
        }
    }

    protected static final String[] ACTR_FINGERS = new String[] {
        // finger constants are one-based, so they're just like
        // piano fingering
        null, "thumb", "index", "middle", "ring", "pinkie"
    };

    protected static final double STD_LOOKAT_DURATION = 0.15;

    protected static abstract class ScriptStepWriter
                                      extends AScriptStep.ScriptStepVisitor
    {
        protected ThinkScriptStep prevMental = null;

        protected int state = 1;

        protected Script script;
        protected PrintWriter out;
        protected int nextStep = 1;

        protected int stepCount;
        protected int nameCounter = 1;

        // A move-mouse followed immediately by a tap should always be
        // treated as a move-and-tap!
        protected boolean actAsMoveAndTap = false;

        protected IWidget lastLookedAtWidget = null;
        protected IWidget lastMovedToWidget = null;
        protected StringBuilder properties = new StringBuilder();

        public ScriptStepWriter(Script s, PrintWriter w)
        {
            script = s;
            out = w;

            stepCount = script.getStepStates().size();
        }

        abstract public void startScript(Frame startFrame);

        public void outputScriptStepState(DefaultModelGeneratorState stepState)
        {
            AScriptStep step = stepState.getScriptStep();
            TransitionSource lookAtTarget = step.getStepFocus();

            // We track the last looked-at widget internally since
            // the model generation may not generate look-at's
            // that ACT-R requires.
            if ((lookAtTarget instanceof IWidget) &&
                ! ((IWidget) lookAtTarget).sameLocation(lastLookedAtWidget) &&
                ! (step instanceof LookAtScriptStep))
            {
                // If there is a pending mental step, decrease its duration and
                // generate its associated production.
//TODO:mlh trying to make it look like it did in the past
//                decrAndFlushMentalStep(STD_LOOKAT_DURATION);
                flushMentalStep();

                // Now insert the LookAt, but only if it's not typing
                if (! (step instanceof TextActionSegment)) {
                    generateLookAt((IWidget) lookAtTarget);
                }
            }
            else {
                // If a mental is pending, generate its associated production.
                flushMentalStep();
            }

            // Generate the production(s) for this script step
            step.accept(this);

            lastMovedToWidget = stepState.getLastMovedToWidget();
            resetForNextStep();
        }

        protected void generateLookAt(IWidget targetWidget,
                                      Frame destinationFrame)
        {
            // Overriding methods are expected to invoke super.generateLookAt
            // as their last statement.
            lastLookedAtWidget = targetWidget;
        }

        protected void generateLookAt(IWidget targetWidget)
        {
            // A null destinationFrame means no transition!
            generateLookAt(targetWidget, null);
        }

        protected void registerMentalStep(ThinkScriptStep mentalStep)
        {
            // Although two mentals should never occur in sequence,
            // handle anyway.
            if (prevMental != null) {
                flushMentalStep();
            }

            prevMental = mentalStep;
        }

        protected void decrAndFlushMentalStep(double decrBy)
        {
            if (prevMental != null) {
                generateMental(prevMental,
                               prevMental.getThinkDuration() - decrBy);
            }
        }

        protected abstract void generateMental(ThinkScriptStep step,
                                               double thinkDuration);

        public void flushMentalStep()
        {
            if (prevMental != null) {
                generateMental(prevMental,
                               prevMental.getThinkDuration());
                prevMental = null;
            }
        }

        protected void resetForNextStep()
        {
            // If subclass overrides, should invoke super.resetForNextStep last
            nextStep++;
        }

        protected boolean cursorAtFocus(TransitionSource focus)
        {
            return (focus instanceof IWidget) &&
                   ((IWidget) focus).sameLocation(lastMovedToWidget);
        }
    }

    public void outputScript(Script script, Frame startFrame, PrintWriter out)
    {
        out.println(";; ==== Script ==== \n");

        ACTR6ScriptStepWriter ssWriter =
            (ACTR6ScriptStepWriter) startScriptOutput(script, startFrame, out);

        Iterator<DefaultModelGeneratorState> stepStateIt =
        	script.getStepStates().iterator();

        while (stepStateIt.hasNext()) {
            DefaultModelGeneratorState stepState = stepStateIt.next();

            ssWriter.outputScriptStepState(stepState);
        }

        ssWriter.flushMentalStep();
        ssWriter.endScript();
        endScriptOutput(script, out);
    }
    
    private static WeakHashMap<Frame, ArrayList<NamedObject>> implicitGroups = 
            new WeakHashMap<Frame, ArrayList<NamedObject>>();
    
    public static int putImplicitGroup(NamedObject thing, Frame frame) {
        ArrayList<NamedObject> grps = implicitGroups.get(frame);
        if (grps == null) {
            grps = new ArrayList<NamedObject>();
            implicitGroups.put(frame, grps);
        }
        int result = grps.indexOf(thing);
        if (result < 0) {
            result = grps.size();
            grps.add(thing);
        }
        return result + 1;
    }
    
    public static NamedObject getImplicitGroup(int i, Frame frame) {
        --i;
        ArrayList<NamedObject> grps = implicitGroups.get(frame);
        if (grps == null || i >= grps.size()) {
            return null;
        }
        return grps.get(i);
    }

    // For seriously localizing this it shouldn't just be an infix, since
    // that might not be an appropriate scheme for non-English languages.
    // However, this really isn't intended for presentation to the user,
    // and even localizing this much is probably silly....
    private static final String GLOBAL_NAME_INFIX =
        L10N.get("lisp.infix", " in ");

    protected static String safeGlobalName(NamedObject thing, Frame frame)
    {
        String name = (thing != null ? thing.getName() : null);
        if (name == null) {
            name = String.format("GROUP [i%d]", putImplicitGroup(thing, frame));
        }
        return LispUtil.safeString(name + GLOBAL_NAME_INFIX + frame.getName());
    }

    /**
     * Returns a name for a widget that is unique across the whole design,
     * suitably escaped for inclusion in Lisp code.
     */
    protected static String safeGlobalName(TransitionSource widget)
    {
        return safeGlobalName(widget, widget.getFrame());
    }

    protected String getActRVersion()
    {
        return "6";
    }

    protected TraceParser<ResultStep> getTraceParser()
    {
        return new ACTRTraceParser();
    }

    protected ScriptStepWriter startScriptOutput(Script script,
                                                 Frame startFrame,
                                                 PrintWriter out)
    {
        Demonstration demonstration = script.getDemonstration();
        DefaultModelGeneratorState initialState = demonstration.getInitialState();
        boolean mouseHand = demonstration.getMouseHand();
        HandLocation handStartLoc =
            initialState.getHandLocation(mouseHand);

        out.println("(define-cogtool-model (:start-with-mouse "
                    + ((handStartLoc == HandLocation.OnMouse) ? "t" : "nil")
                    + " :timeout "
                    + getActrTimeoutInSeconds()
                    + ")\n");
        ScriptStepWriter result =
            new ACTR6ScriptStepWriter(script, out, mouseHand);
        result.startScript(startFrame);
        return result;
    }

    protected void endScriptOutput(Script script, PrintWriter out)
    {
        out.println(")");
    }

    protected static class ACTR6ScriptStepWriter extends ScriptStepWriter
    {
        protected boolean mouseHand;

        public ACTR6ScriptStepWriter(Script s,
                                     PrintWriter outWriter,
                                     boolean mHand)
        {
            super(s, outWriter);
            mouseHand = mHand;
        }

        protected static final Pattern NEEDS_ESCAPE =
            Pattern.compile("[\\\\$]");

        protected static String escapeForRegex(String s)
        {
            return NEEDS_ESCAPE.matcher(s).replaceAll("\\\\$0");
        }

        protected static final Pattern TEMPLATE_ITEM =
            Pattern.compile("%(\\d*)([a-zA-Z%])");

        protected boolean handToHomePending = false;
        protected StringBuffer productionStr = new StringBuffer();

        // ACT-R productions are written using a simple template language,
        // rather like a much reduced printf in C. The template is emitted
        // verbatim, except items preceded by % are substituted. The %items
        // may have an optional, non-negative numeric argument between the %
        // and the letter naming the kind of substitution; if not supplied this
        // argument defaults to zero. For example, %a and %0a are exactly
        // equivalent. Substitutions are
        //   %c is the number of the production, used to ensure unique names;
        //      the numeric argument is ignored;
        //   %s is the current state; if a non-zero numeric argument is
        //      supplied the value substituted is the current state plus that
        //      argument.
        //   %S is like %s, but also sets the stored state to that new value;
        //      note that %0S (or equivalently %S) has no additional meaning
        //      beyond %s, and is therefore disallowed since any attempt to use
        //      it probably reflects some misunderstanding!
        //   %f is the current frame name
        //   %a substitutes additional values, supplied as additional arguments;
        //      these are numbered starting with 0:
        //      the first argument is %0a or just %a; the next %1a, and so on
        //   %p substitutes a Lisp s-expression that sets the cogtool-plist;
        //      the numeric argument is ignored
        protected void emitProduction(AScriptStep step,
                                      boolean checkTransition,
                                      String template,
                                      String... substitutions)
        {
            Frame destination = null;
            TransitionDelay delay = null;
            if (checkTransition) {
                destination = step.getDestinationFrame();

                if (step instanceof ActionScriptStep) {
                    ActionScriptStep actionStep =
                        (ActionScriptStep) step;
                    double delayInSecs = actionStep.getDelayInSecs();

                    if (delayInSecs > 0.0) {
                        delay = actionStep;
                    }
                }
                else if (step.isInsertedByUser()) {
                    AScriptStep owner = step.getOwner();

                    if (owner instanceof TransitionScriptStep) {
                        delay = ((TransitionScriptStep) owner).getTransition();
                    }
                    else if (owner instanceof ActionScriptStep) {
                        delay = (ActionScriptStep) owner;
                    }
                }
            }
            emitProduction(step.getCurrentFrame(),
                           destination,
                           delay,
                           template,
                           substitutions);
        }

        protected void emitProduction(Frame currentFrame,
                                      Frame destinationFrame,
                                      TransitionDelay delay,
                                      String template,
                                      String... substitutions)
        {
            handToHomePending = false;
            actAsMoveAndTap = false;

            if (currentFrame != null) {
                addProperty(":current-frame",
                            LispUtil.safeString(currentFrame.getName()));
            }

            if ((destinationFrame != null)) {
                boolean emitDestinationFrame = false;

                // Check to see if this step triggered a transition;
                // if the next step's destination is not null, and it's
                // a different frame, then it is a (non-self) transition.
                if (destinationFrame != currentFrame) {
                    emitDestinationFrame = true;
                }

                if (delay != null) {
                    double duration = delay.getDelayInSecs();
                    if (duration > 0.0) {
                        addProperty(":delay-duration", Double.toString(duration));
                        addProperty(":delay-label",
                                    LispUtil.safeString(delay.getDelayLabel()));
                        // need to "follow" the transition even if a
                        // self-transition if there's a system wait attached to it
                        emitDestinationFrame = true;
                    }
                }

                if (emitDestinationFrame) {
                    addProperty(":destination-frame",
                                LispUtil.safeString(destinationFrame.getName()));
                }
            }

            Matcher m = TEMPLATE_ITEM.matcher(template);

            while (m.find()) {
                String rep = null;

                String arg = m.group(1);
                int n = 0;
                if (arg.length() > 0) {
                    n = Integer.parseInt(arg);
                }

                switch (m.group(2).charAt(0)) {
                    // Don't use %n for anything. Because the templates are so
                    // full of \n's, using %n makes it too hard to read.

                    case 'c':
                        rep = Integer.toString(nameCounter);
                        break;

                    case 's':
                        rep = Integer.toString(state + n);
                        break;

                    case 'S':
                        if (n == 0) {
                            throw new IllegalArgumentException(
                                "%0S (or %S) is unlikely to be what you " +
                                "really want; use %s instead");
                        }
                        state += n;
                        rep = Integer.toString(state);
                        break;

                    case 'f':
                        if (currentFrame == null) {
                            throw new IllegalStateException("No current frame");
                        }
                        rep = escapeForRegex(LispUtil.safeString(currentFrame.getName()));
                        break;

                    case 'a':
                        if (n >= substitutions.length) {
                            throw new IllegalArgumentException(
                                "Insufficient substitutions provided: (" +
                                n + ", " + substitutions.length + ")");
                        }
                        rep = escapeForRegex(substitutions[n]);
                        break;

                    case 'p':
                        rep = escapeForRegex("(set-cogtool-plist "
                                                + properties.toString()
                                                + ")");
                        break;

                    case '%':
                        rep = "%";
                        break;

                }

                m.appendReplacement(productionStr, rep);
            }

            m.appendTail(productionStr);
            productionStr.append("\n");
            out.println(productionStr.toString());
            productionStr.delete(0, productionStr.length());

            ++nameCounter;
            properties.delete(0, properties.length());
        }

        protected void addProperty(String key, String value)
        {
            if (properties.length() > 0) {
                properties.append(" ");
            }
            properties.append(key);
            properties.append(" ");
            properties.append(value);
        }

        protected void handleCharacters(AScriptStep step, String text)
        {
            // Encode multiple-character key commands
            BitSet boundaries = new BitSet(text.length());
            List<String> words = new ArrayList<String>();
            findWordBoundaries(text, boundaries, words);

            boolean eFree = true;
            if (handToHomePending && text.length() > 0) {
                char ch = text.charAt(0);
                if (KeyboardUtil.needsLeftHand(ch) == (mouseHand == HandLocation.RIGHT_HAND)) {
                    eFree = false;
                }
            }

            Boolean previousLeft = null;
            for (int i = 0; i < text.length(); i++) {
                if (boundaries.get(i)) {
                    String word = LispUtil.makeSymbol(words.remove(0));
                    emitProduction(step,
                                   false,
                                   "(p get-spelling-%a-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "  ?manual>\n" +
                                   "    preparation free\n" +
                                   (eFree ? "    execution free\n" : "") +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  =goal>\n" +
                                   "    state %1S)\n\n",
                                   word);
                    eFree = true;
                }

                char ch = text.charAt(i);
                String key = encodeKey(ch, true);
                boolean usesLeft = KeyboardUtil.needsLeftHand(ch);
                boolean sameHand = (previousLeft == Boolean.valueOf(usesLeft));
                previousLeft = Boolean.valueOf(usesLeft);

                String disp = Character.toString(ch);
                disp = KeyDisplayUtil.convertActionToMenuText(disp);
                addProperty(":key", LispUtil.safeString(disp));

                addProperty(":hand", (usesLeft ? "'LEFT" : "'RIGHT"));
                emitProduction(step,
                               ((i + 1) >= text.length()),
                               "(p press-key-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %s\n" +
                               "  ?frame>\n" +
                               "    name %f\n" +
                               "  ?manual>\n" +
                               "    preparation free\n" +
                               "%0a" +
                               "==>\n" +
                               "  !eval! %p\n" +
                               "  +manual>\n" +
                               "    isa press-key\n" +
                               "    key %1a\n" +
                               "  =goal>\n" +
                               "    state %1S)",
                               (sameHand ? "    execution free\n" : ""),
                               key);
            }
        }

        protected static final Pattern WORD =
            Pattern.compile("[^ -@\\[-`\\{-~\\s]{2,}");

        // TODO this will not work if the text contains non-ASCII
        //      punctuation characters; we don't support that so it's
        //      probably not a big deal, but should be born in mind if
        //      we ever start paying attention to non-English -- even in
        //      a Latin script we'll run into funny punctuation, such as
        //      guillemonts and the like.
        protected void findWordBoundaries(String text,
                                          BitSet boundaries,
                                          List<String> words)
        {
            // Only words of length greater than one are considered "words".
            // Boundaries is used to describe where the words start.
            Matcher m = WORD.matcher(text);
            while (m.find()) {
                boundaries.set(m.start());
                words.add(m.group(0));
            }
        }

        @Override
        public void startScript(Frame startFrame)
        {
            if (EMIT_KEEP_ALIVE) {
                emitProduction(null,
                               null,
                               null,
                               "(p wait-background-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "==>\n" +
                               "  !eval! %p)\n\n" +
                               "(spp wait-background-%c :u -100.0 :at %0a)",
                               Double.toString(BACKGROUND_EFFORT));
            }
            emitProduction(null,
                           null,
                           null,
                           "(p *suppress-re-encoding-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    suppress-re-encoding t\n" +
                                   "  ?visual>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    status visible\n" +
                                   "==>\n" +
                                   "  +visual>\n" +
                                   "    isa clear\n" +
                                   "  =goal>\n" +
                                   "    suppress-re-encoding nil)\n" +
                    "(spp *suppress-re-encoding-%c :at 0.0)");
            emitProduction(null,
                           startFrame,
                           null,
                           "(p set-start-frame-%c\n" +
                           "  =goal>\n" +
                           "    isa klm\n" +
                           "    state %s\n" +
                           "==>\n" +
                           "  !eval! %p\n" +
                           "  =goal>\n" +
                           "    state %1S)\n\n" +
                           "(spp set-start-frame-%c :at 0.0)");
        }

        public void endScript()
        {
            emitProduction(null,
                           null,
                           null,
                           "(p stop-%c\n" +
                           "  =goal>\n" +
                           "    isa klm\n" +
                           "    state %s\n" +
                           "==>\n" +
                           "  !eval! %p\n" +
                           "  +goal>\n" +
                           "    isa stop)\n\n" +
                           "(spp stop-%c :at 0.0)");
        }

        protected void startVisit(AScriptStep step, String stepKind)
        {
            out.print(";; ScriptStep " + stepKind);
            if (step != null) {
                String s = step.getLocalizedString();
                out.print(" (" + LispUtil.stripNonASCII(s) + ")");
                if (step.isInsertedByUser()) {
                    out.print(" [demonstrated]");
                }
            }
            out.println("\n");
        }


        @Override
        public void visit(TextActionSegment step)
        {
            handleCharacters(step, step.getText());
        }


        @Override
        public void visit(HearScriptStep step)
        {
            double duration = step.getListenTimeInSecs();
            String speakerText = step.getTextToHear();
            String traceText = speakerText;
            Frame frame = step.getCurrentFrame();

            if (duration == Frame.NO_LISTEN_TIME) {
                if (speakerText != null) {
                    duration = articulationTime(speakerText);
                }
            }
            else {
                speakerText = articulateTextForDuration(speakerText, duration);
                // TODO The following may not really make sense in all languages,
                //      as it assumes an infix preposition, and postfix units
                if (traceText != null && traceText.length() > 0) {
                    traceText += (" " +
                                  L10N.get("lisp.duratonString", "for") +
                                  " ");
                } else {
                    traceText = "";
                }
                traceText += (duration + "s");
            }

            // TODO this interacts badly if the user is speaking, as
            //      ACT-R hears what it is saying itself, and becomes confused
            if (duration != Frame.NO_LISTEN_TIME) {
                addProperty(":spoken-text", LispUtil.safeString(speakerText));
                addProperty(":trace-text", LispUtil.safeString(traceText));
                emitProduction(frame,
                               null,
                               null,
                               "(p queue-sound-to-hear-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?aural>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  =goal>\n" +
                                   "    state %1S)\n\n" +
                                   "(spp queue-sound-to-hear-%c :at 0.0)");
                emitProduction(frame,
                               null,
                               null,
                               "(p listen-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  =aural-location>\n" +
                                   "    isa audio-event\n" +
                                   "    kind word\n" +
                                   "    location loudspeaker\n" +
                                   "  ?aural>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +aural>\n" +
                                   "    isa sound\n" +
                                   "    event =aural-location\n" +
                                   "  =goal>\n" +
                                   "    state %1S)");
                emitProduction(frame,
                               null,
                               null,
                               "(p wait-for-listening-to-finish-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?aural>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  =goal>\n" +
                                   "    state %1S)\n\n" +
                                   "(spp wait-for-listening-to-finish-%c :at 0.0)");
            }
        }


        @Override
        public void visit(ActionScriptStep step)
        {
            visitAction(step, step.getAction(), step.getStepFocus(), null);
        }


        @Override
        public void visit(TransitionScriptStep step)
        {
            Transition t = step.getTransition();
            visitAction(step, t.getAction(), t.getSource(), t);
        }

        protected double articulationTime(String text)
        {
            // This is the same crude algorithm that ACT-R uses
            double result = (ARTICULATION_TIME_PER_CHAR * text.length());
            return (Math.round(1000 * result) / 1000.0); // round to 3 places
        }

        protected void visitAction(AScriptStep step,
                                   AAction action,
                                   TransitionSource source,
                                   TransitionDelay delay)
        {
            ActionType type = action.getType();

            if (type.equals(ActionType.ButtonPress)) {
                // If a multiple click, emit multiple click-mouse's,
                // but only transition on the last
                MousePressType pt = ((ButtonAction) action).getPressType();
                int times = 1;
                if (pt.equals(MousePressType.Double)) {
                    times = 2;
                }
                else if (pt.equals(MousePressType.Triple)) {
                    times = 3;
                }
                String target = safeGlobalName(source);

                startVisit(step, "BUTTON-PRESS");

                if (pt.equals(MousePressType.Down)) {
                    addProperty(":target", target);
                    emitProduction(step,
                                   true,
                                   "(p down-click-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?manual>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +manual>\n" +
                                   "    isa finger-down\n" +
                                   "    hand right\n" +
                                   "    finger index\n" +
                                   "  =goal>\n" +
                                   "    state %1S)");
                }
                else if (pt.equals(MousePressType.Up)) {
                    addProperty(":target", target);
                    emitProduction(step,
                                   true,
                                   "(p up-click-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?manual>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +manual>\n" +
                                   "    isa finger-up\n" +
                                   "    hand right\n" +
                                   "    finger index\n" +
                                   "  =goal>\n" +
                                   "    state %1S)");
                }
                else {
                    for (int i = 1; i <= times; ++i) {
                        addProperty(":target", target);
                        emitProduction(step,
                                       false,
                                       "(p prepare-click-%c\n" +
                                           "  =goal>\n" +
                                           "    isa klm\n" +
                                           "    state %s\n" +
                                           "  ?manual>\n" +
                                           "    preparation free\n" +
                                           "  ?frame>\n" +
                                           "    name %f\n" +
                                           "==>\n" +
                                           "  !eval! %p\n" +
                                           "  +manual>\n" +
                                           "    isa prepare\n" +
                                           "    style punch\n" +
                                           "    hand right\n" +
                                           "    finger index\n" +
                                           "  =goal>\n" +
                                           "    state %1S)");

                        addProperty(":target", target);
                        emitProduction(step,
                                       (i == times),
                                       "(p click-mouse-%c\n" +
                                           "  =goal>\n" +
                                           "    isa klm\n" +
                                           "    state %s\n" +
                                           "  ?manual>\n" +
                                           "    execution free\n" +
                                           "  ?frame>\n" +
                                           "    name %f\n" +
                                           "==>\n" +
                                           "  !eval! %p\n" +
                                           "  +manual>\n" +
                                           "    isa click-mouse\n" +
                                           "  =goal>\n" +
                                           "    state %1S)");
                    }
                }
            }
            else if (type.equals(ActionType.MouseOver) ||
                     type.equals(ActionType.MoveMouse))
            {
                // Peek at the next step; to fix a bug, if we see a Tap,
                // treat the tap as a move-and-tap instead!
                boolean tapIsNext = false;

                startVisit(step,
                           (type.equals(ActionType.MouseOver) ? "HOVER"
                                                              : "MOVE-MOUSE"));

                if (nextStep < stepCount) {
                    DefaultModelGeneratorState peekStepState =
                        script.getStepState(nextStep);
                    AScriptStep peekStep = peekStepState.getScriptStep();

                    if (peekStep instanceof ActionScriptStep) {
                        ActionScriptStep peekActionStep =
                            (ActionScriptStep) peekStep;

                        tapIsNext =
                            (peekActionStep.getAction().getType() == ActionType.Tap);
                    }
                }

                if (tapIsNext) {
                    // Since tap is next, treat the upcoming tap as
                    // move-and-tap and ignore this move!
                    actAsMoveAndTap = true;
                }
                else {
                    String target = safeGlobalName(source);
// TODO: possible that we're looking for a transition on a move-mouse
//       even though the transition occurs later on a hover!
                    emitProduction(step,
                                   false,
                                   "(p find-location-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  ?frame>\n" +
                                       "    name %f\n" +
                                       "    status visible\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +visual-location>\n" +
                                       "    isa visual-location\n" +
                                       "    value %a\n" +
                                       "  =goal>\n" +
                                       "    state %s)\n\n" +
                                       "(spp find-location-%c :u -1.0)",
                                   target);
                    emitProduction(step,
                                   true,
                                   "(p move-mouse-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  =visual-location>\n" +
                                       "    isa visual-location\n" +
                                       "    value %a\n" +
                                       "  ?manual>\n" +
                                       "    preparation free\n" +
                                       "    execution free\n" +
                                       "    state free\n" +
                                       "  ?frame>\n" +
                                       "    name %f\n" +
                                       "    status visible\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +manual>\n" +
                                       "    isa move-cursor\n" +
                                       "    loc =visual-location\n" +
                                       "  =visual-location>\n" +
                                       "  =goal>\n" +
                                       "    state %1S)",
                                   target);
                }
            }
            else if (type.equals(ActionType.KeyPress)) {
                startVisit(step, "KEY-PRESS");

                handleCharacters(step, ((KeyAction) action).getText());
            }
            else if (type.equals(ActionType.GraffitiStroke)) {
                String cmd = ((GraffitiAction) action).getText();

                startVisit(step, "GRAFFITI");

                // Encode multiple-character graffiti commands
                for (int i = 0; i < cmd.length(); i++) {
                    String key = encodeKey(cmd.charAt(i), false);

                    addProperty(":key", ("'" + key));
                    emitProduction(step,
                                   ((i + 1) >= cmd.length()),
                                   "(p graffiti-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  ?manual>\n" +
                                       "    state free\n" +
                                       "  ?frame>\n" +
                                       "    name %f\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +manual>\n" +
                                       "    isa graffiti-gesture\n" +
                                       "    key %a\n" +
                                       "  =goal>\n" +
                                       "    state %1S)",
                                   key);
                }
            }
            else if (type.equals(ActionType.Voice)) {
                String target = safeGlobalName(source);
                String utterance = ((VoiceAction) action).getText();

                startVisit(step, "SAY");

                addProperty(":target", target);
                emitProduction(step,
                               true,
                               "(p say-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?vocal>\n" +
                                   "    state free\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +vocal>\n" +
                                   "    isa speak\n" +
                                   "    string %a\n" +
                                   "  =goal>\n" +
                                   "    state %1S)",
                               LispUtil.safeString(utterance));
                emitProduction(step,
                               false,
                               "(p wait-for-speech-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?vocal>\n" +
                                   "    state free\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   // clear the aural-location buffer so we do
                                   // sit around waiting to hear ourselves next
                                   // time we try to listen to something!
                                   "  -aural-location>\n" +
                                   "  =goal>\n" +
                                   "    state %1S)\n\n" +
                                   "(spp wait-for-speech-%c :at 0.0)");
            }
            else if (type.equals(ActionType.Home)) {
                HomeAction homeAction = (HomeAction) action;

                if (homeAction.getHomeTarget() == HandLocation.OnKeyboard) {
                    startVisit(step, "HOME-KEYBOARD");

                    addProperty(":target", "'keyboard");
                    emitProduction(step,
                                   false,
                                   "(p move-hand-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  ?manual>\n" +
                                       "    state free\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +manual>\n" +
                                       "    isa hand-to-home\n" +
                                       "  =goal>\n" +
                                       "    state %1S)");
                    handToHomePending = true;
                }
                else {
                   startVisit(step, "HOME-MOUSE");

                    addProperty(":target", "'mouse");
                    emitProduction(step,
                                   false,
                                   "(p move-hand-%c\n" +
                                        "  =goal>\n" +
                                        "    isa klm\n" +
                                        "    state %s\n" +
                                        "  ?manual>\n" +
                                        "    state free\n" +
                                        "==>\n" +
                                        "  !eval! %p\n" +
                                        "  +manual>\n" +
                                        "    isa hand-to-mouse\n" +
                                        "  =goal>\n" +
                                        "    state %1S)");
                }
            }
            else if (type.equals(ActionType.Tap)) {
                TapPressType pt = ((TapAction) action).getTapPressType();
                int times = 1;
                if (pt.equals(TapPressType.DoubleTap)) {
                    times = 2;
                }
                else if (pt.equals(TapPressType.TripleTap)) {
                    times = 3;
                }
                boolean moveRequired =
                    actAsMoveAndTap || ! cursorAtFocus(source);
                if (times > 1 || pt.equals(TapPressType.Tap)) {
                    tap(step,
                        moveRequired ? safeGlobalName(source) : null,
                                     times,
                                     moveRequired);
                } else {
                    startVisit(step, "BUTTON-PRESS");
                    addProperty(":target", safeGlobalName(source));
                    addProperty(":use-finger", "t");
                    if (pt.equals(TapPressType.Down)) {
                        emitProduction(step,
                                       true,
                                       "(p down-tap-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  ?manual>\n" +
                                       "    state free\n" +
                                       "  ?frame>\n" +
                                       "    name %f\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +manual>\n" +
                                       "    isa finger-down\n" +
                                       "    hand right\n" +
                                       "    finger index\n" +
                                       "  =goal>\n" +
                                       "    state %1S)");

                    } else if (pt.equals(TapPressType.Up)) {
                        emitProduction(step,
                                       true,
                                       "(p up-tap-%c\n" +
                                       "  =goal>\n" +
                                       "    isa klm\n" +
                                       "    state %s\n" +
                                       "  ?manual>\n" +
                                       "    state free\n" +
                                       "  ?frame>\n" +
                                       "    name %f\n" +
                                       "==>\n" +
                                       "  !eval! %p\n" +
                                       "  +manual>\n" +
                                       "    isa finger-up\n" +
                                       "    hand right\n" +
                                       "    finger index\n" +
                                       "  =goal>\n" +
                                       "    state %1S)");

                    }
                }
            }
            else {
                throw new ComputationException("An unexpected Action type ("
                                                + type
                                                + ") was associated with an "
                                                + "ActionScriptStep. "
                                                + "Check the FSM.");
            }
        }

        // NOTE - the following is only here because older versions of
        // CogTool could create a TapScriptStep instead of an ActionScriptStep
        // with a tap or a TransitionScriptStep with a TapAction
        @Override
        public void visit(TapScriptStep step)
        {
            TapPressType tpt =
                ((TapAction) step.getAction()).getTapPressType();
            int tapCount = 1;
            if (tpt == TapPressType.DoubleTap) {
                tapCount = 2;
            }
            else if (tpt != TapPressType.Tap) {
                throw new IllegalStateException(
                    "Unknown TapPressType in old file: " + tpt);
            }

            startVisit(step, "TAP");

            if (step.isMoveRequired()) {
                TransitionSource src = step.getStepFocus();

                tap(step, safeGlobalName(src), tapCount, true);
            }
            else {
                tap(step, null, tapCount, false);
            }
        }

        // Expects target to be properly quoted.
        protected void tap(AScriptStep step,
                           String target,
                           int count,
                           boolean moveRequired)
        {
            if (moveRequired) {
                emitProduction(step,
                               false,
                               "(p find-location-%c\n" +
                                    "  =goal>\n" +
                                    "    isa klm\n" +
                                    "    state %s\n" +
                                    "  ?frame>\n" +
                                    "    name %f\n" +
                                    "    status visible\n" +
                                   "==>\n" +
                                    "  !eval! %p\n" +
                                    "  +visual-location>\n" +
                                    "    isa visual-location\n" +
                                    "    value %a\n" +
                                    "  =goal>\n" +
                                    "    state %s)\n\n" +
                                    "(spp find-location-%c :u -1.0)",
                                target);

                addProperty(":use-finger", "t");
                emitProduction(step,
                               false,
                               "(p move-finger-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  =visual-location>\n" +
                                   "    isa visual-location\n" +
                                   "    value %a\n" +
                                   "  ?manual>\n" +
                                   "    preparation free\n" +
                                   "    execution free\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "    status visible\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +manual>\n" +
                                   "    isa move-cursor\n" +
                                   "    loc =visual-location\n" +
                                   "  =visual-location>\n" +
                                   "  =goal>\n" +
                                   "    state %1S)",
                               target);
                addProperty(":force-transition", "t");
                emitProduction(step,
                               (count == 1),
                               "(p *suppress-re-encoding-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?visual>\n" +
                                   "    state free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "    status visible\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +visual>\n" +
                                   "    isa clear\n" +
                                   "  =goal>\n" +
                                   "    state %1S)" +
                                   "(spp *suppress-re-encoding-%c :at 0.0)");

                --count;
            }

            while (count > 0) {
                if (target != null) {
                    addProperty(":target", target);
                }
                addProperty(":use-finger", "t");
                emitProduction(step,
                               false,
                               "(p prepare-tap-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?manual>\n" +
                                   "    preparation free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +manual>\n" +
                                   "    isa prepare\n" +
                                   "    style punch\n" +
                                   "    hand right\n" +
                                   "    finger index\n" +
                                   "  =goal>\n" +
                                   "    state %1S)");

                if (target != null) {
                    addProperty(":target", target);
                }
                addProperty(":use-finger", "t");
                emitProduction(step,
                               (count == 1),
                               "(p tap-%c\n" +
                                   "  =goal>\n" +
                                   "    isa klm\n" +
                                   "    state %s\n" +
                                   "  ?manual>\n" +
                                   "    execution free\n" +
                                   "  ?frame>\n" +
                                   "    name %f\n" +
                                   "==>\n" +
                                   "  !eval! %p\n" +
                                   "  +manual>\n" +
                                   "    isa click-mouse\n" +
                                   "  =goal>\n" +
                                   "    state %1S)");

                --count;
            }
        }


        @Override
        public void visit(DelayScriptStep step)
        {
            addProperty(":old-system-wait",
                        Double.toString(step.getDelayDuration()));
            addProperty(":old-system-wait-label",
                        LispUtil.safeString(step.getLabel()));
            emitProduction(step,
                           false,
                           "(p wait-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %s\n" +
                               "  ?frame>\n" +
                               "    name %f\n" +
                              "==>\n" +
                               "  !eval! %p\n" +
                               "  =goal>\n" +
                               "    state %1s)\n\n" +
                               "(spp find-%c :at 0.0)");
        }


        @Override
        public void visit(TransitionDelayScriptStep step)
        {
            // do nothing on the ScriptStep; this is handled by the transition
            // information on the ScriptStep firing the transition
        }


        @Override
        public void visit(DriveScriptStep step)
        {
            throw new UnsupportedOperationException("Driving is not currently supported.");
        }

        @Override
        protected void generateLookAt(IWidget targetWidget,
                                      Frame destinationFrame)
        {
            Frame currentFrame = targetWidget.getFrame();
            String target = safeGlobalName(targetWidget);

            startVisit(null, "LOOK-AT");

            addProperty(":target", target);
            emitProduction(currentFrame,
                           null,
                           null,
                           "(p find-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %s\n" +
                               "  ?frame>\n" +
                               "    name %f\n" +
                               "    status visible\n" +
                               "==>\n" +
                               "  !eval! %p\n" +
                               "  +visual-location>\n" +
                               "    isa visual-location\n" +
                               "    value %a\n" +
                               "  =goal>\n" +
                               "    state %1s)\n\n" +
                               "(spp find-%c :u -1.0)",
                           target);

             addProperty(":target", target);
             emitProduction(currentFrame,
                           destinationFrame,
                           null, // TODO is this right?
                           "(p look-at-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %s\n" +
                               "  =visual-location>\n" +
                               "    isa visual-location\n" +
                               "    value %a\n" +
                               "  ?visual>\n" +
                               "    state free\n" +
                               "  ?frame>\n" +
                               "    name %f\n" +
                               "    status visible\n" +
                               "==>\n" +
                               "  !eval! %p\n" +
                               "  +visual>\n" +
                               "    isa move-attention\n" +
                               "    screen-pos =visual-location\n" +
                               "  =visual-location>\n" +
                               "  =goal>\n" +
                               "    state %2s\n" +
                               "    suppress-re-encoding t)",
                           target);

            addProperty(":target", target);
            emitProduction(currentFrame,
                           destinationFrame,
                           null, // TODO is this right?
                           "(p look-at-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %1s\n" +
                               "  =visual-location>\n" +
                               "    isa visual-location\n" +
                               "    value %a\n" +
                               "  ?visual>\n" +
                               "    state free\n" +
                               "  ?frame>\n" +
                               "    name %f\n" +
                               "    status visible\n" +
                               "==>\n" +
                               "  !eval! %p\n" +
                               "  +visual>\n" +
                               "    isa move-attention\n" +
                               "    screen-pos =visual-location\n" +
                               "  =visual-location>\n" +
                               "  =goal>\n" +
                               "    state %2S" +
                               "    suppress-re-encoding t)",
                           target);

            super.generateLookAt(targetWidget, destinationFrame);
        }


        @Override
        public void visit(LookAtScriptStep step)
        {
            generateLookAt(step.getLookAtTarget(), step.getDestinationFrame());
        }

        @Override
        protected void generateMental(ThinkScriptStep step,
                                      double thinkDuration)
        {
            String duration = Double.toString(thinkDuration);
            String label = step.getLabel();
            if (label == null || label.length() == 0) {
                label = "THINK";
            }

            startVisit(null, LispUtil.safeString(label));

            addProperty(":duration", duration);

            emitProduction(prevMental,
                           false,
                           "(p %0a-%c\n" +
                               "  =goal>\n" +
                               "    isa klm\n" +
                               "    state %s\n" +
                               "  ?manual>\n" +
                               "    state free\n" +
                               "==>\n" +
                               "  !eval! %p\n" +
                               "  =goal>\n" +
                               "    state %1S)\n\n" +
                               "(spp %0a-%c :at %1a)",
                           LispUtil.makeSymbol(label),
                           duration);
        }


        @Override
        public void visit(ThinkScriptStep step)
        {
            // May need to decrement the mental's duration if a look-at
            // is needed immediately following!
            registerMentalStep(step);
        }
    }
}
