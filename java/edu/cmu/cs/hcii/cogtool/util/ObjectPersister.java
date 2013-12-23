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

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Implementation of persistence using the XML serialization support
 * of ObjectSaver/ObjectLoader.
 *
 * @author mlh
 */
public class ObjectPersister 
{
    /**
     * Name of the checkpoint file containing the XML serialization.
     */
    private static final String PERSIST_FILE = "PERSIST";
    
    public static final ObjectPersister ONLY = new ObjectPersister();
    
    private ObjectPersister() { }

    /**
     * The information about objects that may be persisted;
     * includes the object itself, the directory containing the checkpoint
     * file, and the last file into which the object was saved, if any.
     */
    private static class PersistInfo
    {
        public Object obj;
        public File checkpointDir;
        public File originalFile;

        public PersistInfo(Object o, File chkptFile)
        {
            this(o, chkptFile, null);
        }

        public PersistInfo(Object o, File chkptFile, File origFile)
        {
            obj = o;
            checkpointDir = chkptFile;
            originalFile = origFile;
        }
    }

    /**
     * Location of the system temporary directory.
     */
    private static final File tmpDir =
        new File(System.getProperty("java.io.tmpdir"));

    /**
     * Prefix for CogTool temporary directories and files.
     */
    private static String tmpFilePrefix = "CGT";

    /**
     * Suffix for CogTool temporary directories containing checkpoint files.
     */
    private static String ckpFileSuffix = ".ckp";

    /**
     * Map from canonical file names to the persistence data for an object.
     */
    private Map<String, PersistInfo> fileInfos =
        new HashMap<String, PersistInfo>();

    /**
     * Map from persisted object to the persistence data for the object.
     */
    private Map<Object, PersistInfo> objInfos =
        new HashMap<Object, PersistInfo>();

    /**
     * Find the persistence information associated with the given object.
     *
     * @param obj object for which the persistence information is desired
     * @return the persistence information for the given object;
     *         <code>null</code> if no information registered for the object
     * @author mlh
     */
    private PersistInfo getInfoByObject(Object obj)
    {
        return objInfos.get(obj);
    }

    /**
     * Find the persistence information associated with the given
     * saved file name (in canonical form).
     *
     * @param canonicalFileName the name for which the persistence
     *                          information is desired
     * @return the persistence information for the given name;
     *         <code>null</code> if no information registered for the name
     * @author mlh
     */
    private PersistInfo getInfoByName(String canonicalFileName)
    {
        return fileInfos.get(canonicalFileName);
    }

    /**
     * Ensures that a desired amount of disk space is available for dstFile.
     * This method helps implement a fail-early, recover-early policy for IO.
     *
     * @param dstFile the location to check for space
     * @param size the amount of space desired
     * @param msg error message for IOException when space is not available
     * @throws FileNotFoundException if dstFile does not exist
     * @throws IOException if size is not available for dstFile
     */
    private static void checkDiskSpace(File dstFile, long size, String msg)
        throws FileNotFoundException, IOException
    {
        RandomAccessFile rndFile = new RandomAccessFile(dstFile, "rw");

        try {
            rndFile.setLength(size);
        }
        catch (IOException e) {
            // if not enough space, report problem to user
            IOException newE = new IOException(msg);
            newE.initCause(e);

            throw newE;
        }
        finally {
            rndFile.close();
        }
    }

    /**
     * Recursively computes the size on disk of a file or directory.
     *
     * @param file the file or directory to check
     * @return sum of RandomAccessFile.length() for all files at this location
     * @throws IOException if any error occurs while reading file sizes
     */
    private static long diskSize(File file) throws IOException
    {
        if (file.isFile()) {
            // Base case
            long size;

            RandomAccessFile f = null;

            try {
                f = new RandomAccessFile(file, "r");
                size = f.length();
            }
            finally {
                if (f != null) {
                    f.close();
                }
            }

            return size;
        }

        // Recursive case
        File[] files = file.listFiles();
        long sum = 0;

        for (File file2 : files) {
            sum += diskSize(file2);
        }

        return sum;
    }

    /**
     * Creates a directory with a unique name to hold checkpoint files
     * in the system's temporary directory.
     *
     * @returns the abstract representation of the new directory
     * @throws java.io.IOException if the directory cannot be created
     * @author mlh
     */
    private static File createTempDirectory()
        throws IOException
    {
        // This actually creates a file on the file system!
        File uniqueFileName =
            File.createTempFile(tmpFilePrefix, ckpFileSuffix, tmpDir);

        // Create internal representation only!
        File tmpDirName = uniqueFileName.getCanonicalFile();

        // Delete the stupid file since we want a directory.
        if (! uniqueFileName.delete()) {
            throw new IOException("Could not delete temp filename");
        }

        // Go the make the directory!
        if (! tmpDirName.mkdir()) {
            throw new IOException("Could not register temp directory");
        }

        return tmpDirName;
    }

    /**
     * Delete all files referenced by the given abstract name;
     * that is, delete the file if it is a single file or delete
     * all of the directory's contents and the directory itself
     * if it is a directory.
     * <p>
     * If any <code>delete</code> call fails, it uses <code>delteOnExit</code>.
     *
     * @param fileOrDir the abstract name of a file or directory
     * @author mlh
     */
    private static void deleteAll(File fileOrDir)
    {
        if (fileOrDir.exists()) {
            // Base case
            if (fileOrDir.isFile()) {
                if (! fileOrDir.delete()) {
                    fileOrDir.deleteOnExit();
                }
            }
            else if (fileOrDir.isDirectory()) {
                File[] files = fileOrDir.listFiles();

                for (File file : files) {
                    deleteAll(file);
                }

                if (! fileOrDir.delete()) {
                    fileOrDir.deleteOnExit();
                }
            }
        }
    } // deleteAll

    /**
     * Checks whether an Object has a permanent location registered with this
     * ObjectPersister.  If this returns true, save(project, null) has a well-defined
     * destination.
     *
     * @param obj the Object to check for a permanent location
     * @return true if obj has a known permanent location
     */

    public boolean isPermanent(Object obj)
    {
        PersistInfo info = getInfoByObject(obj);

        // Check for a null originalFile
        return info.originalFile != null;
    }

    /**
     * Create a temporary persistent representation for a new Object.  This
     * representation will be used for saving checkpoints and eventually to
     * save the Object to a more permanent storage format.
     *
     * @param obj the Object to register for persistence
     */

    public void registerForPersistence(Object obj) throws IOException
    {
        // Create checkpoint directory
        File chkptFile = createTempDirectory();

        // Keep track of this file for future lookup.
        PersistInfo info = new PersistInfo(obj, chkptFile);

        objInfos.put(obj, info);
    } // registerForPersistence

    /**
     * Load an Object from a file.  This method assumes that all error checking
     * and prompting/correction for the validity of the file has already
     * been completed.
     *
     * @param src the File location of the saved Object
     * @return a new instantiated Object
     * @throws java.io.IOException if any file operation fails or the SAX XML
     *         parser fails
     */

    public Object load(File src) throws IOException
    {
        // See if an object is already loaded for the given file
        String canonicalFileName = src.getCanonicalPath();

        PersistInfo info = getInfoByName(canonicalFileName);

        if (info != null) {
            return info.obj;
        }

        // Attempt to create a file of 3x size in the temp directory
        // to hold the expanded XML serialization.
        File sizeCheckFile =
            File.createTempFile(tmpFilePrefix, ".size", tmpDir);

        try {
            checkDiskSpace(sizeCheckFile,
                           3 * src.length(),
                           "Not enough temp space on disk to open file: "
                                                                        + src);
        }
        finally {
            sizeCheckFile.delete();
        }

        // If we're here, no exception was thrown; create checkpoint directory
        File chkptFile = createTempDirectory();

        // Open file as ZipFile and decompress
        ZipFile zip = null;
        try {
            zip = new ZipFile(src);

            // Unzip the file to the checkpoint directory
            ZipUtil.unzip(zip, chkptFile);
        }
        catch (ZipException ex) {
            IOException newE =
                new IOException("load encountered zip compression error");
            newE.initCause(ex);
            throw newE;
        }
        finally {
            if (zip != null) {
                zip.close();
            }
        }

        // Load object from the expanded XML serialization
        ObjectLoader l = new ObjectLoader();
        Object obj = null;

        Reader reader = null;

        try {
            reader =
                new InputStreamReader(new FileInputStream(new File(chkptFile,
                                                                   PERSIST_FILE)),
                                      "UTF-8");

            Collection<?> objSet = l.load(new InputSource(reader), null);

            // There should be only one top-level object
            Iterator<?> objs = objSet.iterator();

            if (objs.hasNext()) {
                obj = objs.next();
            }

            // Register this file for future lookup, both by object
            // and by file name
            info = new PersistInfo(obj, chkptFile, src);

            objInfos.put(obj, info);
            fileInfos.put(canonicalFileName, info);
        }
        catch (ParserConfigurationException e) {
            IOException newE = new IOException("load encountered parser error");
            newE.initCause(e);

            throw newE;
        }
        catch (SAXException e) {
            IOException newE = new IOException("load encountered SAX error");
            newE.initCause(e);

            throw newE;
        }
        finally {
            if (reader != null) {
                reader.close();
            }
        }

        return obj;
    } // load

    /**
     * Return whether the given file is already loaded.
     *
     * @param src the File location of a saved object
     * @return the loaded object if loaded, <code>null</code> otherwise
     */

    public Object isLoaded(File src) throws IOException
    {
        String canonicalFileName = src.getCanonicalPath();

        PersistInfo info = getInfoByName(canonicalFileName);

        if (info != null) {
            return info.obj;
        }

        return null;
    }

    /**
     * Helper class for recoverFiles, accepts only file names that start with
     * tmpFilePrefix and end with ckpFileSuffix.
     */
    private static class PrefixFilter implements FileFilter
    {
        public static final PrefixFilter ONLY = new PrefixFilter();

        private PrefixFilter() {}


        public boolean accept(File f)
        {
            String fName = f.getName();

            return (f.isDirectory() && fName.startsWith(tmpFilePrefix)
                                    && fName.endsWith(ckpFileSuffix));
        }
    }

    /**
     * Look for checkpoint files that were not cleaned up properly by a
     * previous application run.
     *
     * @return an array of recovered Objects
     */

    public Object[] recoverFiles() throws IOException
    {
        // Look for possible checkpoint directories
        File[] chkptDirs = tmpDir.listFiles(PrefixFilter.ONLY);

        // Try to revive each
        ObjectLoader l = new ObjectLoader();

        Object[] recovered = new Object[chkptDirs.length];

        for (int i = 0; i < recovered.length; i++) {
            FileReader reader = null;

            try {
                reader = new FileReader(new File(chkptDirs[i], PERSIST_FILE));

                recovered[i] = l.load(new InputSource(reader), null);

                PersistInfo info = new PersistInfo(recovered[i], chkptDirs[i]);
                objInfos.put(recovered[i], info);

                // TODO ... keep track of original file name somehow!
            }
            catch (ParserConfigurationException e) {
                IOException newE =
                    new IOException("load encountered parser error");
                newE.initCause(e);

                throw newE;
            }
            catch (SAXException e) {
                IOException newE =
                    new IOException("load encountered SAX error");
                newE.initCause(e);

                throw newE;
            }
            finally {
                if (reader != null) {
                    reader.close();
                }
            }
        }

        return recovered;
    }

    /**
     * Flushes all in-memory changes for the given Object to disk as a
     * checkpoint.  The Object must have been created via this class (load or
     * recoverFiles) or must be registered via the registerForPersistence
     * method before it can be checkpointed.
     *
     * @param obj the Object to checkpoint
     * @throws IllegalArgumentException if the given object had not been
     *         previously registered via a call to load, recoverFiles, or
     *         registerForPersistence
     * @throws java.io.IOException if checkpointing or renaming fails
     */

    public void checkpoint(Object obj) throws IOException
    {
        // The given object must have been previously registered
        PersistInfo info = getInfoByObject(obj);

        if (info == null) {
            throw new IllegalArgumentException("Cannot find persistence info for given object");
        }

        // Create a file to hold the XML serialization in the checkpoint dir
        File chkpt = new File(info.checkpointDir, PERSIST_FILE);
        File oldChkpt = null;

        // If a checkpoint already exists, rename it to PERSIST.old
        if (chkpt.exists()) {
            oldChkpt = new File(info.checkpointDir, PERSIST_FILE + ".old");

            if (oldChkpt.exists()) {
                oldChkpt.delete();      // try to recover from bad situation
            }

            // Use oldChkpt File object below because this rename doesn't
            // actually change the name for existingChkpt (Java, grrrr!)
            if (! chkpt.renameTo(oldChkpt)) {
                throw new IOException("Cannot rename old checkpoint file");
            }
        }

        // Create a sink into the file and serialize into XML
        Writer writer = null;

        try {
            writer =
                new OutputStreamWriter(new FileOutputStream(chkpt), "UTF-8");

            ObjectSaver s = new ObjectSaver(writer);

            s.saveObject(obj);
            s.finish();         // ensures a flush!
        }
        finally {
            if (writer != null) {
                writer.close();
            }
        }

        // If there was an existing checkpoint, delete the .old file
        if (oldChkpt != null) {
            if (! oldChkpt.delete()) {
                throw new IOException("Cannot delete old checkpoint file");
            }
        }
    } // checkpoint

    /**
     * Persist an Object to a file on the filesystem.  The Object must have
     * been created via this class (load or recoverFiles) or must be registered
     * via the registerForPersistence method before it can be saved.  This
     * method assumes that all necessary error checking and
     * prompting/correction for the destination has already been completed.
     *
     * @param obj the Object that will be saved to disk
     * @param dst the destination File location, or null to reuse the
     *            load location of this Object
     * @throws IllegalArgumentException if the given object had not been
     *         previously registered via a call to load, recoverFiles, or
     *         registerForPersistence
     * @throws IOException if there isn't enough disk space to save obj to dst
     * @throws IllegalStateException if (dst == null) && ! isPermanent(obj)
     */

    public void save(Object obj, File dst) throws IOException
    {
        // The given object must have been previously registered
        PersistInfo info = getInfoByObject(obj);

        if (info == null) {
            throw new IllegalArgumentException("Cannot find persistence info for given object");
        }

        // Use original file if destination not specified
        if (dst == null) {
            if (info.originalFile == null) {
                throw new IllegalStateException("Unspecified save location!");
            }

            dst = info.originalFile;
        }

        // First, checkpoint
        checkpoint(obj);

        // Create a temp zip file in the same directory as dst
        File tmp =
            File.createTempFile(tmpFilePrefix, ".cgt", dst.getParentFile());

        // Try to ensure the save will succeed
        try {
            checkDiskSpace(tmp,
                           diskSize(info.checkpointDir),
                           "Insufficient space to save project to file: "
                                                                        + dst);
        }
        finally {
            tmp.delete();
        }

        // If we're here, no exception was thrown; compress the checkpoint
        // file(s) into a temporary file in the destination file's directory
        File[] files = info.checkpointDir.listFiles();
        ZipUtil.zip(Arrays.asList(files), tmp);

        // If all went well, delete dst and move tmp to dst
        if (dst.exists()) {
            if (! dst.delete()) {
                throw new IOException("File cannot be deleted: "
                                                      + dst.getAbsolutePath());
            }
        }

        tmp.renameTo(dst);

        // If all went well, reset original file to destination if necessary
        String originalCanonicalPath =
            (info.originalFile != null) ? info.originalFile.getCanonicalPath()
                                        : "";

        String dstCanonicalPath = dst.getCanonicalPath();

        // Reset the file name registration if the name has changed
        if (! dstCanonicalPath.equals(originalCanonicalPath)) {
            fileInfos.remove(originalCanonicalPath); // works even if ""!
            info.originalFile = new File(dstCanonicalPath);
            fileInfos.put(dstCanonicalPath, info);
        }
    } // save

    /**
     * Releases any persistence-related resources attached to this Object.
     *
     * @param obj the obj whose persistence resources will be closed
     * @throws IllegalArgumentException if the given object had not been
     *         previously registered via a call to load, recoverFiles, or
     *         registerForPersistence
     * @throws java.io.IOException if any file manipulation files
     */

    public void close(Object obj) throws IOException
    {
        // The given object must have been previously registered
        PersistInfo info = getInfoByObject(obj);

        if (info != null) {
            // Delete the checkpoint directory
            deleteAll(info.checkpointDir);

            // Remove from registries
            objInfos.remove(obj);

            if (info.originalFile != null) {
                fileInfos.remove(info.originalFile.getCanonicalPath());
            }
        }
    } // close

    /**
     * Determines whether the given file is associated with a currently loaded
     * object that is not the given object.
     *
     * @param dst the destination file to be used for a "save-as"
     * @param obj the object to be saved; it is ok if the file specified is
     *            associated with the given object
     * @return <code>true</code> if and only if the file is registered as
     *         associated with an object that is not the given object
     * @throws IOException
     */

    public boolean isRegistered(File dst, Object obj) throws IOException
    {
        // See if an object is already loaded for the given file
        String canonicalFileName = dst.getCanonicalPath();

        PersistInfo info = getInfoByName(canonicalFileName);

        if (info != null) {
            return info.obj != obj;
        }

        return false;
    }
}
