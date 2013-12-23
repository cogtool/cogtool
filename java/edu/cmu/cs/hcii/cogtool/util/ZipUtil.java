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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

/**
 * Utility class for zipping and unzipping files.
 * @author centgraf
 */
public class ZipUtil
{
  // Prevent instantiation
  private ZipUtil() { }

  /**
   * Zips a set of files into a single zip archive file.
   * @param srcFiles a list containing Files to compress
   * @param zip the destination file location for the archive
   */
  public static void zip(List<File> srcFiles, File dst) throws IOException
  {
    // Create a ZipOutputStream
    FileOutputStream fos = null;
    ZipOutputStream zip = null;

    try {
      fos = new FileOutputStream(dst);
      zip = new ZipOutputStream(fos);

      zip.setLevel(9);
      zip.setMethod(ZipOutputStream.DEFLATED);

      // Recursively add file entries
      for (File src : srcFiles) {
        if (src.isDirectory()) {
          zipDirectory(src, "", zip);
        }
        else {
          zipOneFile(src, "", zip);
        }
      }
    }
    finally {
      // Close the output stream
      if (fos != null) {
        if (zip != null) {
          zip.close();
        }

        fos.close();
      }
    }
  }

  private static void zipDirectory(File dir, String base, ZipOutputStream zout)
      throws IOException
  {
    // list all
    File[] files = dir.listFiles();
    base += dir.getName() + "/";

    if (files.length > 0) {
      // if there are files in this directory, add them (this dir is implicit)
      for (File file : files) {
        if (file.isDirectory()) {
          // recur on directories
          zipDirectory(file, base, zout);
        }
        else {
          // add files to the stream
          zipOneFile(file, base, zout);
        }
      }
    }
    else {
      // we have to add this dir explicitly as an entry
      // Build proper entry name
      String name = base + dir.getName() + '/';

      // Create a ZipEntry
      ZipEntry entry = new ZipEntry(name);
      entry.setTime(dir.lastModified());

      // Put the entry into the zip (actually, this just writes the header)
      zout.putNextEntry(entry);

      // Close off the entry (actually, this just writes more header info)
      zout.closeEntry();
    }
  }

  private static void zipOneFile(File file, String base, ZipOutputStream zout)
      throws IOException
  {
    // Build proper entry name
    String name = base + file.getName();

    // Create a ZipEntry
    ZipEntry entry = new ZipEntry(name);
    entry.setTime(file.lastModified());

    // Put the entry into the zip (actually, this just writes the header)
    zout.putNextEntry(entry);

    // Input stream for reading the file
    FileInputStream in = new FileInputStream(file);

    try {
      // Copy the file contents to the zip, 100k at a time
      long size = file.length();
      int chunkSize = 100*1024;
      byte[] buff = new byte[chunkSize];
      int len = chunkSize;

      for (; size > 0; size -= len) {
        if (size < chunkSize) {
          len = (int) size;
        }
        else {
          len = chunkSize;
        }

        int actualBytes = 0;
        int off = 0;
//        System.out.print("" + '\n' + name + ' ');

        do {
          actualBytes = in.read(buff, off, len);

          if (actualBytes == -1) {
            zout.write(buff, off, len);
//           System.out.print("!" + len + ':' + actualBytes + ':' + off + ' ');
            throw new RuntimeException("Bad math in zip!");
          }
          else {
            zout.write(buff, off, actualBytes);
//            System.out.print("" + len + ':' + actualBytes + ':' + off + ' ');
          }

          len -= actualBytes;
          size -= actualBytes;
          off += actualBytes;
        } while ((len > 0));
      }
    }
    finally {
      // Close off the input stream
      in.close();
    }

    // Close off the entry (actually, this just writes more header info)
    zout.closeEntry();
  }

  /**
   * Unzips a zipfile to a destination directory.
   * @param zip the file to unzip
   * @param fileDir the destination directory for zipfile contents
   * @throws FileNotFoundException
   * @throws IOException
   */
  public static void unzip(ZipFile zip, File fileDir)
      throws FileNotFoundException, IOException
  {
    // Read out all entries from ZipFile via input streams
    for (Enumeration<? extends ZipEntry> en = zip.entries(); en.hasMoreElements();) {
      ZipEntry ze = en.nextElement();

      // Get info from file entry
      long size = ze.getSize();

      // Create File in fileDir for unpacked entry
      String name = ze.getName();
//      System.out.println("Unzipping: " + name);
      File zeFile = new File(fileDir, name);

      // Check for a trailing slash to see if this is a directory entry
      if (name.charAt(name.length()-1) == '/') {
        // If this entry is a directory, make it
        zeFile.mkdirs();
      }
      else {
        // if this entry is a file, make its parent directories, then it
        zeFile.getParentFile().mkdirs();
        zeFile.createNewFile();

        // Create plus OutputStream to the new file
        FileOutputStream fout = null;
        OutputStream out = null;

        // Get ZipInputStream for reading data
        InputStream zin = null;

        try {
          fout = new FileOutputStream(zeFile);
          out = new BufferedOutputStream(fout);
          zin = zip.getInputStream(ze);

          // Set modification time
          zeFile.setLastModified(ze.getTime());

          // Copy data from zin to out, 100k at a time
          int chunkSize = 100*1024;
          byte[] buff = new byte[chunkSize];
          int len = chunkSize;

          for (; size > 0; size -= len) {
            if (size < chunkSize) {
              len = (int) size;
            }
            else {
              len = chunkSize;
            }

            int actualBytes = 0;
            int off = 0;
//            System.out.print("" + '\n' + name + ' ');

            do {
              actualBytes = zin.read(buff, off, len);

              if (actualBytes == -1) {
                out.write(buff, off, len);
//                System.out.print("!" + len + ':' + actualBytes + ':' + off + ' ');
                throw new RuntimeException("Bad math in unzip!");
              }
              else {
                out.write(buff, off, actualBytes);
//                System.out.print("" + len + ':' + actualBytes + ':' + off + ' ');
              }

              len -= actualBytes;
              size -= actualBytes;
              off += actualBytes;
            } while ((len > 0));
          }
        }
        finally {
          // Close the streams
          if (fout != null) {
            if (out != null) {
              if (zin != null) {
                zin.close();
              }

              out.close();
            }

            fout.close();
          }
        }
      }
    }
  }

  /**
   * Command-line hook for manual testing.
   * @param args 0: "zip" or "unzip" 1: first filename arg 2: second filename arg
   */
  public static void main(String[] args)
  {
    try {
      if (args[0].equals("zip")) {
        List<File> list = new ArrayList<File>(1);
        list.add(new File(args[1]));
        zip(list, new File(args[2]));
      }
      else if (args[0].equals("unzip")) {
        unzip(new ZipFile(new File(args[1])), new File(args[2]));
      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }
}
