/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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

package edu.cmu.cs.hcii.cogtool;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import edu.cmu.cs.hcii.cogtool.controller.ControllerNexus;
import edu.cmu.cs.hcii.cogtool.controller.ControllerRegistry;
import edu.cmu.cs.hcii.cogtool.controller.ProjectController;
import edu.cmu.cs.hcii.cogtool.controller.RootController;
import edu.cmu.cs.hcii.cogtool.model.CogToolSerialization;
import edu.cmu.cs.hcii.cogtool.model.Project;
import edu.cmu.cs.hcii.cogtool.ui.ProjectInteraction;
import edu.cmu.cs.hcii.cogtool.ui.RcvrExceptionHandler;
import edu.cmu.cs.hcii.cogtool.util.DelayedWorkManager;
import edu.cmu.cs.hcii.cogtool.util.DelayedWorkPhase;
import edu.cmu.cs.hcii.cogtool.util.ErrorDialog;
import edu.cmu.cs.hcii.cogtool.util.L10N;
import edu.cmu.cs.hcii.cogtool.util.OSUtils;
import edu.cmu.cs.hcii.cogtool.util.ObjectPersister;
import edu.cmu.cs.hcii.cogtool.util.RecoverableException;
import edu.cmu.cs.hcii.cogtool.util.Subprocess;
import edu.cmu.cs.hcii.cogtool.util.WindowUtil;

public class CogTool
{
    public static final int ERROR_STATUS = 1;

    // Don't allow instantiation.
    private CogTool() { }
    
    private static final String LOG_FILE_PATTERN = "/cogtool%u.%g.log";
    private static final int LOG_FILE_MAX_SIZE = 1000000;
    private static final int LOG_FILES_MAX_COUNT = 4;
    private static final Level DEFAULT_LOG_LEVEL = Level.INFO;
    
    // Grab the root logger for us to have convenient access to for displaying
    // our state.
    public static final Logger logger = Logger.getLogger("");
    static {
        logger.setLevel(DEFAULT_LOG_LEVEL);
    }
    
    // TODO replace a lot of our System.err.printlns and System.out.printlns
    //      by logging calls, and consider adding logging calls in other
    //      useful places.
    
    public static boolean projectManagesOthers = false;

    // persistence 
    // TODO this variable is never accessed: should we ensure that
    //      CogToolSerialization is loaded in some other way?
    public static final CogToolSerialization serialization =
        CogToolSerialization.ONLY;

    /**
     * Controller registration
     */
    public static ControllerNexus<Project> controllerNexus =
        new ControllerNexus<Project>();

    /**
     * Delayed selection work should be added to this phase.
     */
    public static final DelayedWorkPhase selectionPhase =
        new DelayedWorkPhase();

    /**
     * Delayed repaint work should be added to this phase.
     */
    public static final DelayedWorkPhase repaintPhase =
        new DelayedWorkPhase();

    /**
     * When delayed work is to be performed, invoke
     * "delayedWorkMgr.doDelayedWork(notCanceled)", where notCanceled is
     * <code>false</code> if the user canceled the operation,
     * <code>true</code> otherwise.
     */
    public static final DelayedWorkManager delayedWorkMgr =
        new DelayedWorkManager();

    private static final String VERSION =
        System.getProperty("cogtool.version");
    private static String revision =
        System.getProperty("cogtool.revision");
    private static final String BUILD_TIME =
        System.getProperty("cogtool.build");

    private static ReportInteraction interaction =
        new ReportInteraction()
        {
            public void reportException(String title, String msg, Exception e)
            {
                ErrorDialog dlog = new ErrorDialog(title, msg, e);
                dlog.open();
            }
        };

    private static final int ERROR_HEIGHT = 450;

    // Set to non-null only on Macintosh; we can use this to get at
    // Macintosh specific functionality. See the explanation on
    // RootView.java.
    private static RootController rootCtl = null;

    // The exportCVSKludge doesn't work on Windows
    public static File exportCSVKludgeDir = null;

    private static String[] COMMANDS = { 
        "import-xml", "export-xml", "import-html", "export-csv" }; 
    
    private static OptionSet parseOptions(String args[]) {
        OptionParser parser = new OptionParser();
        // The psn is supplied on MacOS when a GUI application is double-cliced;
        // we just ignore it, but need to recognize it so we can ignore it.
        parser.accepts("psn", "process serial number (ignored)").withRequiredArg();
        for (String c : COMMANDS) {
            parser.accepts(c);
        }
        OptionSet result = null;
        try {
            result = parser.parse(args);
        } catch (OptionException ex) {
            System.err.println(L10N.get(
               "CT.CLIError",
               "Could not interpret the command line arguments to CogTool: ")
               + args);
            System.exit(1);
        }
        return result;
    }
    
    private static String getCommand(OptionSet opts) {
        String result = null;
        for (String c : COMMANDS) {
            if (opts.has(c)) {
                if (result != null) {
                    System.err.println(
                        String.format(
                          L10N.get(
                              "CT.CLIDuplicate",
                              "Too many commands in command line arguments to CogTool: %s, %s"),
                          result,
                          c));                        
                    System.exit(4);
                } else {
                    result = c;
                }
            }
        }
        return result;
    }
    
    private static void testArgs(List<String> args, int count, String cmd) {
        if (args.size() != count) {
            System.err.println(String.format(
                L10N.get("CT.CLIWrongArgCount",
                         "Wrong number of arguments for %s command; expected %d but got %d."),
                cmd,
                count,
                args.size()));
            System.exit(16);
        }
    }                            

    public static void main(String args[])
    {
        // Do a clean exit if running on a machine with an old JRE.
        if (! OSUtils.supportCogTool()) {
            System.out.println("JRE 1.5 or later is required to run CogTool");
            // TODO: add simple window with message?
            System.exit(1024);
        }
        
        if (revision == null) {
            // we're running under the development environment; get the revision
            // dynamically, and also make our subprocesses echo what's going
            // on to stdout
            revision = getRevisionAtRuntime();
            Subprocess.setDebug(true);
        }

        // Insert the two phases into the delayed work manager, selection
        // first and repaint second, since selection can cause repaint requests
        delayedWorkMgr.addDelayedWork(selectionPhase);
        delayedWorkMgr.addDelayedWork(repaintPhase);

        try {
            if (OSUtils.MACOSX) {
                // we need to create the RootController, but will never
                // actually need to interact with it programmatically
                rootCtl = new RootController();
            }

            // TODO temporary kludge until we update the preference dialog to supply
            //      a UI for turning this on and off
            CogToolPref.IS_LOGGING.setBoolean(true);
            
            enableLogging(CogToolPref.IS_LOGGING.getBoolean());
            
            OptionSet opts = parseOptions(args);
            String command = getCommand(opts);
            List<String> arguments = opts.nonOptionArguments();
            List<String> filesToLoad = new ArrayList<String>();
            if (command == null) {
                filesToLoad.addAll(arguments);
            } else if (command.equals("import-xml")) {
                testArgs(arguments, 2, "import-xml");
                filesToLoad.add(arguments.get(1));                
            } else if (command.equals("export-xml")) {
                testArgs(arguments, 2, "export-xml");
                filesToLoad.add(arguments.get(0));
            } else if (command.equals("import-html")) {
                testArgs(arguments, 9, "import-html");
            } else if (command.equals("export-csv")) {
                testArgs(arguments, 1, "iexport-csv");
                filesToLoad.add(arguments.get(0));
            } else {
                // No need to localize this, as it's an illegal state that
                // can't be caused by user input
                System.err.println("Unknown command line command: " + command);
                System.exit(8);
            }
            
            List<RecoverableException> loadExceptions =
            	new ArrayList<RecoverableException>();
            ProjectController openedProject = null;
            File file = null;
            if (! filesToLoad.isEmpty()) {
                ObjectPersister persister = ObjectPersister.ONLY;
                for (String fn : filesToLoad) {
                    try {
                        file = new File(fn);
                        Project proj = (Project) persister.load(file);

                        openedProject =
                            ProjectController.openController(proj, false, true);
                    }
                    catch (Exception e) {
                        RecoverableException re =
                            new RecoverableException("Error occurred while loading: "
                                                     + fn,
                                                     e);
                        RcvrExceptionHandler.recover(re, interaction);
                    }
                }
            }

            if (openedProject != null) {
                if (System.getProperty("edu.cmu.cs.hcii.cogtool.ExportCSVKludge") != null) {
                    exportCSVKludgeDir =
                        file.getAbsoluteFile().getParentFile();
                    openedProject.exportCSVKludge();
                    System.exit(0);
                }
            }
            
            if (command == null) {
                if (openedProject == null) {
                    // no project was opened successfully, or none were specified,
                    // so create a fresh project and use its Interaction to
                    // report load errors
                    ProjectController c =
                        ProjectController.newProjectController();
                    c.setLocation(25.0, 25.0);
                    String response = c.getInteraction().createNewOrOpenExisting();
                    if (response == ProjectInteraction.CREATE) {
                        // Populate the empty project to avoid a "blank screen".
                        c.populateProject();
                    }
                    else if (response == null) {
                        c.requestClose();
                    } else {
                        if (response == ProjectInteraction.OPEN) {
                            c.openExistingProject(c.getLocation());
                        } else {
                            c.performAction(CogToolLID.OpenProjectFile,
                                            response,
                                            false);
                        }
                        if (ControllerRegistry.ONLY.openControllerCount() > 1) {
                            c.requestClose();
                        }
                    }
                }
            } else if (command.equals("export-csv")) {
                if (openedProject == null) {
                    System.err.println(String.format(
                            L10N.get("CT.NoFileForCSV",
                                     "Couldn't open %s for CSV export."),
                            arguments.get(0)));
                    System.exit(32);
                }
                exportCSVKludgeDir =
                    file.getAbsoluteFile().getParentFile();
                openedProject.exportCSVKludge();
                System.exit(0);
            }

            // Note that the following catches and does not rethrow any
            // SWTExceptions. This means reportTopLevelException never gets
            // a chance to report these to the user.
            WindowUtil.interact();
        }
        catch (RecoverableException e) {
            RcvrExceptionHandler.recover(e, interaction);
        }
        catch (Exception e) {
            System.err.println("Catching exception...");
            reportTopLevelAnomaly(e);
        }
        catch (Error e) {
            // Before trying to throw up a dialog it is important that we dump
            // the trace to stderr, the same way as would happen if this went
            // uncaught. At least that way if we are in so hosed a state that
            // we can't even popup the dialog we've made the patient no worse
            // than it would have been had we not caught this. We don't even
            // wait for reportTopLevelAnomoly before printing it in case just
            // that extra level of function call will make a difference in
            // whether or not we succeed.
            System.err.println("Catching error...");
            e.printStackTrace();
            reportTopLevelAnomaly(e);
        }
        catch (Throwable e) {
            // This shouldn't be possible -- the only unchecked Throwables
            // are Errors and RuntimeExceptions, both of which should have
            // been caught above. But just in case someone does something
            // deeply bizarre and has something we call in this try able to
            // throw a checked non-Exception Throwable, let's be extra paranoid.
            System.err.println("Catching throwable...");
            reportTopLevelAnomaly(new Exception(
                     L10N.get("CT.UncaughtThrowable",
                              ("Uncaught non-Exception, non-Error Throwable " +
                               "propagated all the way to top-level.")),
                     e));
        }

        // All the windows successfully closed -- quit the application.
        // See the comment in WindowUtil.interact() for an explanation of why
        // the globalDisplay might already be disposed by the time we get
        // here.
        if (! WindowUtil.GLOBAL_DISPLAY.isDisposed()) {
            WindowUtil.GLOBAL_DISPLAY.close();
        }
        // Just calling the preceding is *not* sufficient to ensure we quit,
        // in the rare case where we've thrown a non-recoverable Exception
        // but left a background thread alive
        System.exit(-1);
    }

    /**
     * Attend to an Exception or Error caught at top level.
     * Attempts to interact with the user, presenting a stack trace and
     * related information.  If this also throws an Exception, we just
     * give up in disgust, spitting out what we know to the console.
     *
     * @param e the Throwable that was caught at top level
     */
    public static void reportTopLevelAnomaly(Throwable e)
    {
        try {
            final Shell window = new Shell(WindowUtil.GLOBAL_DISPLAY,
                                           SWT.TITLE | SWT.BORDER | SWT.RESIZE);
            window.setText(L10N.get("UnexpectedError", "Unexpected Error"));
            Layout layout = new GridLayout(1, false);
            window.setLayout(layout);

            Label announcement = new Label(window, SWT.LEFT);
            announcement.setText(L10N.get("ErrorHappened",
                                          "An unexpected error has occurred. Please contact CogTool support with the following information:"));
            GridData announcementLayout = new GridData();
            announcementLayout.grabExcessHorizontalSpace = true;
            announcement.setLayoutData(announcementLayout);

            Text trace =
                new Text(window,
                         SWT.LEFT | SWT.READ_ONLY | SWT.MULTI | SWT.V_SCROLL);
            StringWriter stringBuffer = new StringWriter();
            String version = getVersion();
            stringBuffer.write(version);
            stringBuffer.write('\n');
            String runtimeDesc = OSUtils.runtimeDescription();
            stringBuffer.write(runtimeDesc);
            stringBuffer.write('\n');
            stringBuffer.write(getMemoryUsage());
            stringBuffer.write('\n');

            e.printStackTrace(new PrintWriter(stringBuffer));

            // echo version and trace information to the console
            System.err.println(version);
            System.err.println(runtimeDesc);
            if (! (e instanceof Error)) {
                // If it's an Error, we've already done this
                e.printStackTrace();
            }

            trace.setText(stringBuffer.toString());
            int lineHeight = trace.getLineHeight();
            int lineCount = trace.getLineCount();
            int height = (OSUtils.MACOSX) ? ERROR_HEIGHT
                                          : Math.min(lineHeight * lineCount,
                                                     ERROR_HEIGHT);
            GridData traceLayout = new GridData();
            traceLayout.grabExcessHorizontalSpace = true;
            traceLayout.heightHint = height;
            trace.setLayoutData(traceLayout);

            Button okButton = new Button(window, SWT.PUSH);
            okButton.setText(L10N.get("B.Exit", "Exit CogTool"));
            okButton.setFocus(); // ignore return value from setFocus()
            window.setDefaultButton(okButton);

            GridData okLayout = new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
            okButton.setLayoutData(okLayout);

            // Dismiss the dialog box when OK button selected, with success
            okButton.addListener(SWT.Selection,
                                 new Listener()
                                 {

                                     public void handleEvent(Event evt)
                                     {
                                         window.close();
                                     }
                                 });

            // Exits when window has been disposed.
            WindowUtil.displayAndInteract(window);
        }
        catch (Throwable anotherExceptionOrError) {
            // Again, no need to localize
            System.err.println("Another Exception or Error was thrown while " +
                               "attending to one caught at top level. " +
                               "This is a bug.");
            System.err.println("\n*** The new Exception or Error:");
            anotherExceptionOrError.printStackTrace();
            System.err.println("\n*** The original Exception or Error:");
            e.printStackTrace();
            System.exit(ERROR_STATUS);
        }
    }

    /**
     * Returns a description of the version of CogTool currently running.
     * The result begins with the word "Version: ", and typically contains the
     * version number, Subversion revision number, and build date.
     * @return  a String describing the version of the running application
     */
    public static String getVersion()
    {
        StringBuilder result = new StringBuilder();

        // Build number
        if (VERSION != null) {
            result.append(VERSION);
        }

        // revision number
        if (revision != null) {
            if (result.length() > 0) {
                result.append(" ");
            }
            result.append("(");
            result.append(revision);
            result.append(")");
        }

        // build time
        if (BUILD_TIME != null) {
            if (result.length() > 0) {
                result.append(" ");
            }
            result.append(BUILD_TIME);
        }

        if (result.length() == 0) {
            // The following need not be localizable since this shouldn't
            // normally occur.
            result.append("[none specified]");
        }

        result.insert(0, L10N.get("CT.Version", "Version: "));
        return result.toString();
    } // end getVersion()

    public static boolean isBuilt() {
        return VERSION != null || BUILD_TIME != null;
    }

    protected static String getRevisionAtRuntime()
    {
        if (OSUtils.WINDOWS) {
            // don't know how to do this in windows yet
            return null;
        }
        List<String> command = new ArrayList<String>();
        command.add("bash");
        command.add("-l");
        command.add("-c");
        command.add("svn info");

        List<String> result = new ArrayList<String>();
        try {
            Subprocess.exec(command, result, null, null, null);

            for (String s : result) {
                if (s.startsWith("Revision")) {
                    return s;
                }
            }
        }
        catch (RuntimeException e) {
            // Don't let it stop anything, but do leave some spoor on stderr
            // so we can at least try to figure out what went wrong.
            e.printStackTrace();
        }
        return null;
    }

    public static String getMemoryUsage()
    {
        Runtime rt = Runtime.getRuntime();
        long max = rt.maxMemory();
        long free = rt.freeMemory();
        long total = rt.totalMemory();
        double used = (100.0 * (total - free)) / total;

        NumberFormat fmt = NumberFormat.getInstance();
        fmt.setMinimumIntegerDigits(1);
        fmt.setMinimumFractionDigits(2);
        fmt.setMaximumFractionDigits(2);
        fmt.setGroupingUsed(true);

        StringBuilder result =
            new StringBuilder(L10N.get("CT.Memory", "Memory usage: "));
        // TODO this is not properly locale sensitive, as it assumes US
        //      conventions for percent format and lists
        result.append(fmt.format(used));
        result.append(L10N.get("CT.PercentOf", "% of "));
        result.append(fmt.format(total / 1000000.0));
        result.append(";  ");
        result.append(fmt.format(max / 1000000.0));

        return result.toString();
    }

    public static String getConfigurationProperties()
    {
        StringBuilder result =
            new StringBuilder(L10N.get("CT.Properies", "Props: "));
        result.append(System.getProperty("user.region"));
        result.append(", ");
        result.append(System.getProperty("user.language"));
        result.append(", ");
        result.append(System.getProperty("file.encoding"));
        result.append(", ");
        result.append(System.getProperty("file.encoding.pkg"));
        return result.toString();
    }

    public static void enableLogging(boolean value) {
        if (value) {
            logger.setLevel(Level.ALL);
            for (Handler h : logger.getHandlers()) {
                if (h instanceof FileHandler) {
                    return;
                }
            }
            FileHandler fh = null;
            try {
                fh = new FileHandler(
                     (CogToolPref.LOG_DIRECTORY.getString() + LOG_FILE_PATTERN), 
                     LOG_FILE_MAX_SIZE,
                     LOG_FILES_MAX_COUNT,
                     true);
            } catch (IOException ex) {
                logger.warning("Couldn't create log file: " + ex);
                return;
            }
            fh.setFormatter(new Formatter() {
                @Override
                public String format(LogRecord r) {
                    long ms = r.getMillis();
                    return String.format("%tF %tT.%tL\t%s\n",
                                         ms, ms, ms, 
                                         r.getMessage());
                }
            });
            logger.addHandler(fh);
        } else {
            logger.setLevel(DEFAULT_LOG_LEVEL);
            for (Handler h : logger.getHandlers()) {
                if (! (h instanceof ConsoleHandler)) {
                    logger.removeHandler(h);
                }
            }
        }
    }
    
}