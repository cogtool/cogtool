/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.1, Copyright (c) 2005-2009 Carnegie Mellon University
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
 * Apache Jakarta Commons-Collections 3.1
 * Apache Jakarta Commons-Lang 2.1
 *
 * This product contains software developed by the Apache Software Foundation
 * (http://www.apache.org/)
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
 *******************************************************************************/
package edu.cmu.cs.hcii.cogtool;

import java.util.ArrayList;
import java.util.EventObject;
import java.util.List;
import java.util.Set;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import edu.cmu.cs.hcii.cogtool.model.CachedGoogleSimilarity;
import edu.cmu.cs.hcii.cogtool.ui.PreferencesDialog;
import edu.cmu.cs.hcii.cogtool.util.Alerter;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.view.MenuFactory;

public enum CogToolPref {
    
    RESEARCH("CogTool.research", Kind.BOOLEAN, false),
    RECENT_PREFIX("CogTool.recent.", Kind.STRING, null),
    HCIPA("CogTool.HCIPA", Kind.BOOLEAN, false),
    KLM_RESULT_RANGE("CogTool.klmResultRange", Kind.BOOLEAN, false),
    SYSWVO("CogTool.SystemWaitVisionOnly", Kind.BOOLEAN, false),
    COMPSCR("CogTool.enableComputeScripts", Kind.BOOLEAN, true),
    MIN_FRAME_WIDTH("CogTool.minFrameWidth", Kind.INT, PreferencesDialog.DEFAULT_MIN_FRAME_WIDTH),
    FRAMES_PER_ROW("CogTool.framesPerRow", Kind.INT, 3),
    ACTR_TIMEOUT("CogTool.actrTimeout", Kind.INT, 600000),
    CONVERTER_DIRECTORY("CogTool.converterDirectory", Kind.STRING, null),
    USE_KEYPAD("CogTool.useKeypad", Kind.BOOLEAN, false),
    IS_LOGGING("CogTool.isLogging", Kind.BOOLEAN, false),
    LOG_DIRECTORY("CogTool.logDirectory", Kind.STRING, System.getProperty("user.home")), // No UI yet for modifying this
    IS_TRACING("CogTool.isTracing", Kind.BOOLEAN, true),
    ACTR_DEBUG_LEVEL("CogTool.actrDebugLevel", Kind.INT, 0),
    PMI_G_SIZE("CogTool.pmiGSize", Kind.DOUBLE, CachedGoogleSimilarity.PMI_G_SIZE_AUTOMATIC),
    CTE_SUPPRESS_NOISE("CogTool.CTESuppressNoise", Kind.BOOLEAN, false),
    CTE_DEFAULT_BACK_LABEL("CogTool.CTEDefaultBackLabel", Kind.STRING, L10N.get("PREFS.defaultBackLabel", "Back")),
    GENERATE_THINKS_ON_IMPORT("CogTool.KLMGenerateOnImport", Kind.BOOLEAN, true),
    ACTR_ALTERNATIVE_PARAMETERS("CogTool.AlternativeParameters", Kind.BOOLEAN, false),
    VISUAL_ATTENTION("CogTool.VisualAttention", Kind.INT, 85),
    MOTOR_INITIATION("CogTool.MotorInitiation", Kind.INT, 50),
    PECK_FITTS_COEFF("CogTool.PeckFittsCoeff", Kind.INT, 75),
    ACTR_DAT("CogTool.ACTRDAT", Kind.INT, 50),
    USE_EMMA("CogTool.UseEMMA", Kind.BOOLEAN, true),
    REGENERATE_AUTOMATICALLY("CogTool.RegenerateAutomatically", Kind.BOOLEAN, true), // No UI yet for modifying this
    NESTED_GROUPS_SHOWN_AT_TOP_LEVEL("CogTool.NestedGroupsShownAtTopLevel", Kind.BOOLEAN, false), // No UI yet for modifying this
    DISPLAY_DIGITS("CogTool.DisplayDigits", Kind.INT, 1);
    
     public static class PreferencesChange extends EventObject {
        private final Set<CogToolPref> prefsChanged;
        public PreferencesChange(Set<CogToolPref> pc) {
            super(CogToolPref.class);
            prefsChanged = pc;
        }
        public Set<CogToolPref> getPrefs() {
            return prefsChanged;
        }
    }
    
    private enum Kind { STRING, BOOLEAN, INT, DOUBLE };
    
    public static final int NUM_RECENT_FILES = 10;
    public static final Alerter ALERTER = new Alerter();
    
    private static Preferences prefs =
            Preferences.userNodeForPackage(CogTool.class);
    static {
        for (CogToolPref p : CogToolPref.values()) {
            switch (p.kind) {
                case STRING:
                    p.value = prefs.get(p.key, (String)p.defaultValue);
                    break;
                case BOOLEAN:
                    p.value = prefs.getBoolean(p.key, (Boolean)p.defaultValue);
                    break;
                case INT:
                    p.value = prefs.getInt(p.key, (Integer)p.defaultValue);
                    break;
                case DOUBLE:
                    p.value = prefs.getDouble(p.key, (Double)p.defaultValue);
            }
        }
    };
    
    private static List<String> recentFiles = new ArrayList<String>();
    static {
        for (int i = 0; i < NUM_RECENT_FILES; i++) {
            recentFiles.add(prefs.get((RECENT_PREFIX.key) + i, MenuFactory.UNSET_FILE));
        }
    }

    private final String key;
    private final Kind kind;
    private Object value = null;
    private final Object defaultValue;
    
    private CogToolPref(String ky, Kind kd, Object dv) {
        key = ky;
        kind = kd;
        defaultValue = dv;
    }
    
    private void testKind(Kind k) {
        if (kind != k) {
            throw new IllegalArgumentException(String.format(
               "%s does not name a %s-valued preference", this, k));
        }
    }
    
    public String getString() {
        testKind(Kind.STRING);
        return (String)value;
    }
    
    public boolean getBoolean() {
        testKind(Kind.BOOLEAN);
        return (Boolean)value;
    }
    
    public int getInt() {
        testKind(Kind.INT);
        return (Integer)value;
    }
    
    public double getDouble() {
        testKind(Kind.DOUBLE);
        return (Double)value;
    }
    
    public boolean setString(String val) {
        testKind(Kind.STRING);
        if (val == value) {
            return false;
        }
        value = val;
        prefs.put(key, val);
        return true;
    }
    
    public boolean setBoolean(boolean val) {
        testKind(Kind.BOOLEAN);
        if (val == (Boolean)value) {
            return false;
        }
        value = val;
        prefs.putBoolean(key, val);
        return true;
    }
    
    public boolean setInt(int val) {
        testKind(Kind.INT);
        if (val == (Integer)value) {
            return false;
        }
        value = val;
        prefs.putInt(key, val);
        return true;
    }
    
    public boolean setDouble(double val) {
        testKind(Kind.DOUBLE);
        if (val == (Double)value) {
            return false;
        }
        value = val;
        prefs.putDouble(key, val);
        return true;
    }
    
    public String getStringDefault() {
        testKind(Kind.STRING);
        return (String)defaultValue;
    }
    
    public boolean getBooleanDefault() {
        testKind(Kind.BOOLEAN);
        return (Boolean)defaultValue;
    }
    
    public int getIntDefault() {
        testKind(Kind.INT);
        return (Integer)defaultValue;
    }
    
    public double getDoubleDefault() {
        testKind(Kind.DOUBLE);
        return (Double)defaultValue;
    }
        
    // Force the preferences stuff to be written to whatever mechanism is used
    // to persist it. Otherwise it is not guaranteed to be written until the
    // JVM terminates, though it may be written sooner if the JVM so chooses.
    // And, of course, if the JVM crashes or otherwise terminates abnormally
    // all bets are off, unless an explicit flush() has been performed
    // successfully.
    public static boolean flush() {
        try {
            prefs.flush();
            return true;
        } catch (BackingStoreException e) {
            // mostly ignore it -- the preferences changes will apply only
            // to the current invocation of CogTool and not be saved for the
            // future; however, scribble some info to stderr to help a
            // determined debugger to learn something
            System.err.println("Couldn't flush CogTool Preferences node");
            e.printStackTrace();
            return false;
        }
    }
    
    public static List<String> getRecent()
    {
        return new ArrayList<String>(recentFiles); // defensive copy
    }

    public static boolean setRecent(String newFile)
    {
        if ((newFile == null) || MenuFactory.UNSET_FILE.equals(newFile)) {
            return false;
        }
        if (recentFiles.size() > 0) {
            String def = recentFiles.get(0);

            if (newFile.equals(def)) {
                return false;
            }
        }
        int index = recentFiles.indexOf(newFile);
        if (index != -1) {
            recentFiles.remove(index);
        }
        else if (recentFiles.size() == NUM_RECENT_FILES) {
            recentFiles.remove(NUM_RECENT_FILES - 1);
        }
        recentFiles.add(0, newFile);
        for (int i = 0; i < recentFiles.size(); i++) {
            prefs.put((RECENT_PREFIX.key + i), recentFiles.get(i));
        }
        flush();
        return true;
    }

    public static void clearRecent()
    {
        for (int i = 0; i < NUM_RECENT_FILES; i++) {
            prefs.put((RECENT_PREFIX.key + i), MenuFactory.UNSET_FILE);
        }
        recentFiles.clear();
        flush();
    }

    public static boolean hasRecent()
    {
        return (recentFiles.size() > 0) &&
                (! (recentFiles.get(0).equals(MenuFactory.UNSET_FILE)));
    }
     
}

 
