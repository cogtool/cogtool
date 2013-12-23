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

import java.io.ByteArrayOutputStream;

import com.philschatz.swt.dnd.WindowsImageTransfer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;

public class ClipboardUtil
{
    //  Prevent instantiation
    private ClipboardUtil() { }

    public static class ClipboardException extends RuntimeException
    {
        public ClipboardException(String s, Exception e)
        {
            super(s, e);
        }

        public ClipboardException(String s)
        {
            super(s);
        }
    }

    // OSTypes taken from http://developer.apple.com/documentation/Carbon/Conceptual/understanding_utis/index.html#//apple_ref/doc/uid/TP40001319
    // These should be maintained in order of preference
    public static final String[] MAC_IMAGE_TYPES =
        new String[] { "PNGf", "JPEG", "TIFF", "BMP ", "BMPf", "GIFF" };
    public static int[] MAC_IMAGE_IDS;
    static {
        // Compute IDs for each named MAC_IMAGE_TYPE
        // SWT 3.1 implementation simply packs 8-bit chars into an int OSType
        MAC_IMAGE_IDS = new int[MAC_IMAGE_TYPES.length];
        for (int i = 0; i < MAC_IMAGE_IDS.length; i++) {
            MAC_IMAGE_IDS[i] = Transfer.registerType(MAC_IMAGE_TYPES[i]);
        }
    }
    // Yes, we really just want the unprocessed raw bytes
    public static final ByteArrayTransfer MAC_IMAGE_TRANSFER =
        new ByteArrayTransfer() {
            @Override
            protected int[] getTypeIds()
            {
                return MAC_IMAGE_IDS;
            }

            @Override
            protected String[] getTypeNames()
            {
                return MAC_IMAGE_TYPES;
            }
        };

    public static final Transfer[] IMAGE_TRANFERS =
        new Transfer[] { OSUtils.MACOSX ? MAC_IMAGE_TRANSFER
                                        : WindowsImageTransfer.getInstance() };

    /**
     * The system clipboard and support
     */
    protected static Clipboard systemClipboard;
    static {
        systemClipboard = new Clipboard(WindowUtil.GLOBAL_DISPLAY);
    }

    public static Transfer[] textClipboardType =
        new Transfer[] { TextTransfer.getInstance() };

    /**
     * Checks clipboard for types supported by a particular Transfer.
     * @param transfer a Transfer class we wish to check for support
     * @return true iff support a supported type is available
     */
    public static boolean checkTypes(Transfer transfer)
    {
        TransferData[] types = systemClipboard.getAvailableTypes();

        // If transfer supports one of the types, return true;
        for (TransferData type : types) {
            if (transfer.isSupportedType(type)) {
                return true;
            }
        }

        // clipboard doesn't have a supported type
        return false;
    }

    /**
     * Checks clipboard for a set of named types.
     * @param goodTypes an array of type names
     * @return true iff a supported type is available
     */
    public static boolean checkTypes(String[] goodTypes)
    {
        String[] types = systemClipboard.getAvailableTypeNames();

        // If clipboard supports one of the types, return true;
        for (String type : types) {
            for (String goodType : goodTypes) {
                if (type.equals(goodType)) {
                    return true;
                }
            }
        }

        // clipboard doesn't have a supported type
        return false;
    }

    /**
     * Checks the system clipboard for text.
     */
    public static boolean hasTextData()
    {
        return checkTypes(TextTransfer.getInstance());
    }

    /**
     * Checks the system clipboard for Windows image data.
     * @return true if the clipboard contains Windows image data, else false
     */
    public static boolean hasImageData()
    {
        // First, check for the Windows clipboard image type.
        if (checkTypes(WindowsImageTransfer.getInstance())) {
            return true;
        }

        // Finally, check for Mac image format data

        return checkTypes(MAC_IMAGE_TYPES);
    }

    protected static String[] clipboardData = new String[1];

    public static void copyDataToClipboard(String data, Transfer[] types)
    {
        clipboardData[0] = data;

        systemClipboard.setContents(clipboardData, types);
    }

    /**
     * Returns <code>null</code> if no data of desired type is available
     */
    public static Object fetchClipboardData(Transfer desiredType)
    {
        return systemClipboard.getContents(desiredType);
    }

    public static void copyTextData(String data)
    {
        copyDataToClipboard(data, textClipboardType);
    }

    public static String fetchTextData()
    {
        return (String) fetchClipboardData(TextTransfer.getInstance());
    }

    protected static byte[] fetchWindowsImageData(ImageData imgData)
    {
        // Set up an ImageLoader to convert the data to BMP
        ImageLoader loader = new ImageLoader();
        loader.data = new ImageData[] { imgData };

        // Save the ImageData to a byte stream
        ByteArrayOutputStream byteStream =
            new ByteArrayOutputStream(imgData.data.length);
        loader.save(byteStream, SWT.IMAGE_BMP_RLE);

        return byteStream.toByteArray();
    }

    public static byte[] getImageDataAsBytes(Object imgData)
    {
        if (OSUtils.WINDOWS) {
            if (imgData instanceof ImageData) {
                return fetchWindowsImageData((ImageData) imgData);
            }

            throw new ClipboardException("Expected ImageData image data type");
        }

        if (OSUtils.MACOSX) {
            if (imgData instanceof byte[]) {
                return (byte[]) imgData;
            }

            throw new ClipboardException("Expected byte[] image data type");
        }

        throw new ClipboardException("Unexpected operating system");
    }

    public static byte[] fetchImageData()
    {
        // This is more complicated on Windows
        if (OSUtils.WINDOWS) {
            // First, get an ImageData
            ImageData imgData =
              (ImageData)
                systemClipboard.getContents(WindowsImageTransfer.getInstance());

            return fetchWindowsImageData(imgData);
        }

        if (OSUtils.MACOSX) {
            return getImageDataAsBytes(systemClipboard.getContents(MAC_IMAGE_TRANSFER));
        }

        throw new ClipboardException("Can't fetch an image from the clipboard -- nothing to fetch!");
    }
}
