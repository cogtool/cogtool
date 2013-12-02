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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import edu.cmu.cs.hcii.cogtool.CogToolPref;

// The heavy lifting is delegated to instances of the static inner class
// StepParser. Each such instances corresponds to a particular kind of step,
// or possible set of steps that occur together, and are bounded by a starting
// end ending trace line, possibly with intermediate trace lines.
// A StepParser always acts on lines of a single module, and has regular
// expressions matching the appropriate part of the trace line after the
// module name. Methods in the TraceParser construct appropriate ResultSteps;
// the surrounding code that calls these methods takes care of shared
// operations such as setting the time and duration of the ResultStep.
//
// The trace lines are matched in order against the possible StepParsers.
// Only "normal" trace lines, consisting of a time, module name, and other
// information are matched, other lines in the trace being skipped.
// Once a StepParser has matched the beginning trace line it stores information
// in a StepParser.State object that can be consulted when further lines
// denoting the end and/or intermediate trace lines are matched.
//
// The various StepParsers are all declared in a static initializer at the
// end of this file. Adding further trace information typically consists
// of declaring a new StepParser there.
//
// In a few cases there is more global information stored in the
// surrounding ACTRTraceParser, in addition to the State object corresponding
// to a single ResultStep.
public class ACTRTraceParser implements TraceParser<ResultStep>
{

    // ACT-R in its infinite wisdom does not emit a trace line for the start of
    // motor operation initialization, only a line for its end. So we need to
    // work backwards. Fortunately init time in ACT-R is constant. If we change
    // it in ACT-R with spp. though, we'll need to update the value here, too.
    protected static final int INIT_TIME = 50; // msec

    protected static final String DEFAULT_HEARD_TEXT = "<text heard>";

    protected static final Pattern TRACE_LINE_PAT =
        Pattern.compile("\\s+(\\d+\\.\\d\\d\\d)\\s+(\\w+)\\s+(.*?)\\s*");

    protected static final Map<String, List<StepParser>> stepParsers =
    	new HashMap<String, List<StepParser>>();
    protected static final Map<String, List<StepParser.State>> pendingSteps =
    	new HashMap<String, List<StepParser.State>>();

    protected static boolean buildingStepParsers = true;

    protected final List<ResultStep> resultSteps = new ArrayList<ResultStep>();
    protected int line = -1;
    protected double time = 0.0;
    protected ResultStep lastProductionResultStep = null;

    /**
     * Parses an ACT-R 6 trace and returns a list of ResultStep objects
     * that represent that trace.
     *
     * @param traceLines
     * @return List of ResultStep objects
     */
    public List<ResultStep> parseTrace(List<String> traceLines)
    {
        if (buildingStepParsers) {
            throw new IllegalStateException(
                "Data required for parsing ACT-R trace not properly initialized");
        }

        for (List<StepParser.State> lst : pendingSteps.values())
        {
        	lst.clear();
        }

        for (String s: traceLines)
        {
            ++line;
            Matcher m = TRACE_LINE_PAT.matcher(s);
            if (! m.matches()) {
                if (s.startsWith("#|Warning: Move-cursor action aborted because cursor is at requested target")) {
                    abandonPendingMoveCuror();
                }
                continue;
            }

            try {
                time = (Double.parseDouble(m.group(1)) * 1000);
            } catch (NumberFormatException e) {
                throw new IllegalStateException(
                   "Text matching pattern specifying double not parseable as double " +
                   m.group(1));
            }
            if (time < 0.0) {
                continue;
            }

            String module = m.group(2);
            String detail = m.group(3);

            processTraceLine(module, detail);
        }

        List<StepParser.State> finishThis = pendingSteps.get("COGTOOL");
        if (! finishThis.isEmpty()) {
            StepParser.State s = finishThis.get(0);
            s.intermediate = false;
            processTraceLine("COGTOOL", "Finished");
        }

        abandonPendingEyeMovementPreps(null);

        for (String k : pendingSteps.keySet())
        {
            List<StepParser.State> lst = pendingSteps.get(k);
            if (! lst.isEmpty()) {
                // TODO figure out how to deal with this more gracefully;
                //      for now just write something to stderr, and ignore it
//                throw new IllegalStateException(
//                    "Not all ResultSteps were completed: " + lst.get(0));
                System.err.println("Not all ResultSteps were completed: " +
                		           k + " -> " + lst.get(0));
            }
        }

        return resultSteps;
    }

    protected static void abandonPendingEyeMovementPreps(StepParser.State leaveThisOne) {
 		 List<StepParser.State> lst = pendingSteps.get("VISION");
		 for (Iterator<StepParser.State> it = lst.iterator(); it.hasNext(); ) {
			 StepParser.State other = it.next();
			 if (other != leaveThisOne && other.data == null) {
				 it.remove();
			 }
		 }
    }
    
    // This gets fired when there is an attempt to move the cursor to exactly
    // where it is already. In KLM CogTool this should never happen, but in CT-E
    // if you click the back button two or more times in succession it does.
    protected static void abandonPendingMoveCuror() {
        List<StepParser.State> lst = pendingSteps.get("MOTOR");
        for (Iterator<StepParser.State> it = lst.iterator(); it.hasNext(); ) {
            Object d = it.next().data;
            if (d instanceof String && 
                    ((String)d).startsWith("Move Cursor")) {
                it.remove();
                return;
            }
        }
   }

    protected void processTraceLine(String module, String detail)
    {
        // First see if this trace line ends a pending step
        List<StepParser.State> pending = pendingSteps.get(module);
        if (pending != null && ! pending.isEmpty()) {
            for (Iterator<StepParser.State> it = pending.iterator(); it.hasNext(); ) {
                StepParser.State state = it.next();
                Matcher m = state.pattern().matcher(detail);
                if (m.lookingAt()) {
                    // grab the following value before execute has a chance to change it
                    boolean intermed = state.intermediate;
                    ResultStep result = state.execute(this, m);
                    if (result != null) {
                        if (result.startTime < 0.0); {
                            result.startTime = state.startTime;
                        }
                        if (result.duration < 1.0) {
                            result.duration = (time - state.startTime);
                        }
                        if (result.traceStart < 0) {
                            result.traceStart = state.startIndex;
                        }
                        // TODO investigate how the consumer of this information
                        //      is fiddling it -- I suspect it's doing a subtract
                        //      one that it really shouldn't be, and that's why
                        //      we're having to add one here!
                        result.traceEnd = (line + 1);
                        resultSteps.add(result);
                        if (intermed) {
                            state.startTime = time;
                            state.startIndex = line;
                        }
                        else {
                            it.remove();
                        }
                        return;
                    }
                }
            }
        }

        // If not does it start a new one?
        List<StepParser> parsers = stepParsers.get(module);
        if (parsers != null) {
        	for (StepParser p : parsers) {
                Matcher m = p.startPat.matcher(detail);
                if (m.lookingAt()) {
                    StepParser.State state = p.new State(time, line);
                    state.lastProduction = lastProductionResultStep;
                    p.startAction(this, m, state);
                    if (p.intermediatePat != null) {
                        state.intermediate = true;
                    }

                    pendingSteps.get(module).add(state);
                    return;
                }
            }
        }

    }

    protected static abstract class StepParser
    {
        final String module;
        final Pattern startPat;
        final Pattern intermediatePat;
        final Pattern endPat;

        class State
        {
            double startTime;
            int startIndex;
            Object data = null;
            boolean intermediate = false;
            ResultStep.ResultStepDependency dependency = null;
            ResultStep lastProduction = null;

            State(double t, int i)
            {
                startTime = t;
                startIndex = i;
            }

            @Override
            public String toString()
            {
                return ("State:"
                        + module + ":"
                        + startPat.pattern() + ":"
                        + startIndex + ":"
                        + data);
            }

            Pattern pattern()
            {
                return (intermediate ? intermediatePat : endPat);
            }

            ResultStep execute(ACTRTraceParser parser, Matcher match)
            {
                // grab the dependency before executing the action, since
                // the action may modify it
                ResultStep.ResultStepDependency d = dependency;
                ResultStep result = null;
                if (intermediate) {
                    intermediate = false;
                    result = intermediateAction(parser, match, this);
                    if (result == null) {
                        intermediate = true;
                    }
                } else {
                    result = endAction(parser, match, this);
                }
                if (result != null && d != null) {
                    result.dependencies.add(d);
                }
                return result;
            }

            protected void addProductionDependency(ResultStep rs)
            {
                if (lastProduction != null) {
                    rs.dependencies.add(new ResultStep.ResultStepDependency(lastProduction));
                }
            }
        }

        StepParser(String mod,
                   String start,
                   String imed,
                   String end)
        {
            module = mod;
            startPat = Pattern.compile(start);
            intermediatePat = (imed != null ? Pattern.compile(imed) : null);
            endPat = Pattern.compile(end);
        }

        StepParser(String mod, String start, String end)
        {
            this(mod, start, null, end);
        }

        void startAction(ACTRTraceParser parser,
                                  Matcher match,
                                  State state)
        {
            // Will override if any extra state needs to be preserved
        }

        ResultStep intermediateAction(ACTRTraceParser parser,
                                      Matcher match,
                                      State state) {
            // Will normally override whenever intermediate pattern is supplied
            return null;
        }

        abstract ResultStep endAction(ACTRTraceParser parser,
                                      Matcher match,
                                      State state);
    }

    protected static void declareStep(StepParser p)
    {
        if (! buildingStepParsers) {
            throw new IllegalStateException("Can no longer add StepParsers");
        }
        List<StepParser> lst = stepParsers.get(p.module);
        if (lst == null) {
            lst = new ArrayList<StepParser>();
            stepParsers.put(p.module, lst);
        }
        lst.add(p);
    }

    protected static void finishBuildingStepParsers() {
        buildingStepParsers = false;
        for (String mod : stepParsers.keySet()) {
        	pendingSteps.put(mod, new ArrayList<StepParser.State>());
        }
    }

    protected LinkedList<ResultStep> pendingMotorOperations = new LinkedList<ResultStep>();

    // TODO How we want motor operations displayed has evolved repeatedly,
    //      and has involved a variety of complications requiring knowledge
    //      of one step to be passed to another, but in changing ways as our
    //      understanding of what we want has evolved. The result is we now
    //      have a Byzantine structure with some undoubtedly vestigial items
    //      in it. It would be worth have a complete re-think of how we want to
    //      do this in light of what we now know is the end result we want.
    static class MotorStepParser extends StepParser
    {
        final String label;
        final boolean appendCapture;
        final boolean leftHand;

        MotorStepParser(String start,
                        String lab,
                        boolean appendCap,
                        boolean left)
        {
            super("MOTOR", start, "PREPARATION-COMPLETE");
            label = lab;
            appendCapture = appendCap;
            leftHand = left;
        }

        MotorStepParser(String start, String lab, boolean appendCap)
        {
            this(start, lab, appendCap, false);
        }

        MotorStepParser(String start, String lab)
        {
            this(start, lab, false);
        }

        @Override
        void startAction(ACTRTraceParser p, Matcher m, State s)
        {
            if (appendCapture) {
                s.data = (label + m.group(1));
            }
        }

        @Override
        ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
        {
            ResultStep result =
                new ResultStep((leftHand ? ResultStep.MOTOR_LEFT_PREP_RESOURCE
                                         : ResultStep.MOTOR_RIGHT_PREP_RESOURCE),
                               (appendCapture ? (String) s.data : label));
            s.addProductionDependency(result);
            p.pendingMotorOperations.addLast(result);
            return result;
        }
    }

    static {
        // create the StepParsers

        // order matters here; the earlier ones are tried before the later ones

        declareStep(new StepParser("COGTOOL",
                                   "START-SYSTEM-WAIT [0-9\\.]+ \"(.+)\"",
                                   "SYSTEM-WAIT-DONE .*")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                ResultStep result = new ResultStep(ResultStep.SYSTEM_RESOURCE,
                                                   (String) s.data);
                return result;
            }
        });

        declareStep(new StepParser("PROCEDURAL",
                                   "PRODUCTION-SELECTED ([^*].*)",
                                   "PRODUCTION-FIRED (.+)")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                String lab = (String) s.data;
                if (lab != null && lab.equals(m.group(1))) {
                    ResultStep result =
                        new ResultStep(ResultStep.PRODUCTIONS_RESOURCE, lab);
                    p.lastProductionResultStep = result;
                    return result;
                } else {
                    return null;
                }
            }
        });

        declareStep(new StepParser("COGTOOL",
                                   "TRANSITION-TO \"(.*)\"",
                                   "TRANSITION-TO \"(.*)\"",
                                   "Finished")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
            }

            protected ResultStep makeResultStep(State s)
            {
                return new ResultStep(ResultStep.FRAME_RESOURCE, (String) s.data);
            }

            @Override
            ResultStep intermediateAction(ACTRTraceParser p, Matcher m, State s)
            {
                ResultStep result = makeResultStep(s);
                s.intermediate = true;
                s.data = m.group(1);
                return result;
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                return makeResultStep(s);
            }
        });

        declareStep(new StepParser("MOTOR",
                                   "INITIATION-COMPLETE",
                                   "FINISH-MOVEMENT")
        {
            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                ResultStep prep;
                try {
                    prep = p.pendingMotorOperations.removeFirst();
                } catch (NoSuchElementException e) {
//                    RuntimeException re =
//                        new IllegalStateException("Unmatched motor state");
//                    re.initCause(e);
//                    throw re;
                    // TODO revisit what to do in this case; for now just ignore
                    System.err.println("Unmatched motor state");
                    return null;
                }

                // what we visualize as "exec" time is really what ACT-R
                // considers the sum prep + init + exec
                s.startTime -= (prep.duration + INIT_TIME);

                String resource = ResultStep.MOTOR_RIGHT_EXEC_RESOURCE;
                if (prep.resource == ResultStep.MOTOR_LEFT_PREP_RESOURCE) {
                    resource = ResultStep.MOTOR_LEFT_EXEC_RESOURCE;
                }

                ResultStep result = new ResultStep(resource, prep.operation);

                result.dependencies.addAll(prep.dependencies);
                result.traceStart = prep.traceStart;

                p.resultSteps.remove(prep);

                return result;
            }
        });

        // Several regular expressions below use the positively Luciferian
        // construct
        //    \"((?:\\\\\"|[^\"])*)\"
        // This means
        //   - match a quote (which needs to be escaped to get it into a Java String)
        //   - start a capturing group
        //   - then start a non-capturing group, just precedence stuff
        //   - then match a back slash; that's four backslashes, to get two
        //     into the Java String, since we need two since back slashes as an
        //     escape in Regex land, too
        //   - followed by yet another back slash quoting another quote
        //   - an 'or'
        //   - then a character class matching anything but another double quote
        //     (which, of course, has to be escaped with back slash for the String)
        //   - close the non-capturing group, and have it matching zero or more times
        //   - close the capturing group, to grab the whole matching thing
        //   - then the close double quote, appropriately escaped for the string
        // In summary, it matches a Lisp string, which might have embedded
        // double quotes escaped by back slashes, and captures the contents of
        // that string without the surrounding quotes. Note that it does leave
        // the escaping back slash with each embedded double quote in the captured
        // group, however.

        declareStep(new MotorStepParser("MOVE-CURSOR .* \"((?:\\\\\"|[^\"])*)\" .* NIL",
                                        "Move Cursor to ",
                                        true));
        declareStep(new MotorStepParser("MOVE-CURSOR .* \"((?:\\\\\"|[^\"])*)\" .* T",
                                        "Move Finger to ",
                                        true));

        declareStep(new MotorStepParser("CLICK-MOUSE NIL", "Click Mouse"));
        declareStep(new MotorStepParser("CLICK-MOUSE T", "Tap"));
        declareStep(new MotorStepParser("HAND-TO-HOME", "Hand to Home"));
        declareStep(new MotorStepParser("HAND-TO-MOUSE", "Hand to Mouse"));
        declareStep(new MotorStepParser("PRESS-MOUSE-BUTTON", "Press Mouse Button"));
        declareStep(new MotorStepParser("RELEASE-MOUSE-BUTTON", "Release Mouse Button"));
        declareStep(new MotorStepParser("FINGER-UP .*", "Finger Up"));
        declareStep(new MotorStepParser("FINGER-DOWN .*", "Finger Down"));
        declareStep(new MotorStepParser("GRASP-KNOB", "Grasp Knob"));
        declareStep(new MotorStepParser("RELEASE-KNOB", "Release Knob"));
        declareStep(new MotorStepParser("TURN-KNOB-90-DEG", "Turn Knob 90 Degrees"));

        declareStep(new MotorStepParser("START-PRESS-KEY RIGHT (.*)",
                                        "Press Key ",
                                        true,
                                        false));
        declareStep(new MotorStepParser("START-PRESS-KEY LEFT (.*)",
                                        "Press Key ",
                                        true,
                                        true));

        declareStep(new MotorStepParser("GRAFFITI-GESTURE KEY (.*)",
                                        "Graffiti Gesture ",
                                        true,
                                        false));

        // TODO the following two are very Boeing application specific
        // and probably don't really belong here long term.
        declareStep(new MotorStepParser("REACH-FROM-LAP-TO-MCP-HDG-KNOB", "Reach from Lap to MCP HDG Knob"));
        declareStep(new MotorStepParser("REACH-FROM-MCP-HDG-KNOB-TO-LAP", "Reach from MCP HDG Knob to Lap"));

        declareStep(new StepParser("SPEECH",
                                   "SPEAK TEXT (.*)",
                                   "FINISH-MOVEMENT")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                String t = (String) s.data;
                ResultStep result =
                    new ResultStep(ResultStep.SPEECH_EXEC_RESOURCE, t);
                s.addProductionDependency(result);
                return result;
            }
        });

        declareStep(new StepParser("VISION",
                                   "Move-attention (.*) NIL \"((?:\\\\\"|[^\"])*)\".*",
                                   "Encoding-[cC]omplete (.*)")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = new String[] { m.group(1), m.group(2) };
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                String[] sa = (String[]) s.data;
                String matchThis = m.group(1);
                if (!CogToolPref.USE_EMMA.getBoolean()) {
                    if (matchThis.endsWith(" NIL")) {
                        matchThis = matchThis.substring(0, matchThis.length() - 4);
                    } else {
                        return null;
                    }
                }
                if (!matchThis.equals(sa[0])) {
                    return null;
                }
                ResultStep result =
                    new ResultStep(ResultStep.VISION_ENC_RESOURCE, sa[1]);
                s.addProductionDependency(result);
                return result;
            }
        });

        declareStep(new StepParser("VISION",
                                   "PREPARE-EYE-MOVEMENT",
                                   "Preparation-complete (.*)",
                                   "Complete-eye-movement (.*) #\\((\\d+) (\\d+)\\)")
        {
        	 @Override
            void startAction(ACTRTraceParser parser,
                              Matcher match,
                              State state)
        	 {
        		 // If there's a pending vision prep, abandon it
        		 abandonPendingEyeMovementPreps(state);
        	 }

            @Override
            ResultStep intermediateAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
                ResultStep result =
                    new ResultStep(ResultStep.VISION_PREP_RESOURCE,
                                   "Eye Movement Preparation");
                s.dependency =
                    new ResultStep.ResultStepDependency(result);
                return result;
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                if (! m.group(1).equals(s.data)) {
                    return null;
                }
                ResultStep result =
                    new ResultStep(ResultStep.VISION_EXEC_RESOURCE,
                                   ("Eye Movement to ("
                                           + m.group(2)
                                           + ", "
                                           + m.group(3)
                                           +")"));
                result.dependencies.add(s.dependency);
                return result;
            }
        });

        declareStep(new StepParser("AUDIO",
                                   "LOUDSPEAKER \"(.*)\"",
                                   "AUDIO-ENCODING-COMPLETE .*")
        {
            @Override
            void startAction(ACTRTraceParser p, Matcher m, State s)
            {
                s.data = m.group(1);
            }

            @Override
            ResultStep endAction(ACTRTraceParser p, Matcher m, State s)
            {
                ResultStep result =
                    new ResultStep(ResultStep.HEAR_RESOURCE, (String) s.data);
                s.addProductionDependency(result);
                return result;
            }
        });


        finishBuildingStepParsers();
    }

}
