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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import edu.cmu.cs.hcii.cogtool.model.DoubleRectangle;

public class GraphicsUtil
{
    /**
     * Java has no ImageException, and we need a non-recoverable exception
     * to throw when image-based operations fail in util so that controller
     * classes do not need to know about SWTExceptions
     *
     * @author jcorn
     */
    public static class ImageException extends RuntimeException
    {
        public ImageException(String message, Throwable ex)
        {
            super(message, ex);
        }

        public ImageException(String message)
        {
            super(message);
        }
    }

    /**
     * Default color for widgets
     */
    public static final int defaultWidgetColor =
        getColorFromRGB(new RGB(255, 128, 0));
    public static final org.eclipse.swt.graphics.Color DEFAULT_COLOR =
        new org.eclipse.swt.graphics.Color(null, 255, 128, 0);

    /**
     * Alpha values to use when drawing graphical widgets
     */
    public static final int WIDGET_NORMAL_ALPHA = 128;
    public static final int WIDGET_SELECTED_ALPHA = 175;

    /**
     * Converts a platform-independent color value to an SWT RGB object.
     * Note that any alpha information encoded in the color value will be lost.
     *
     * The platform-independent color value is encoded in the following format:
     * bits 0-7: blue; bits 7-15: green; bits 16-23: red; bits 24-31: alpha
     *
     * @param color the platform-independent color value to convert
     */
    public static RGB getRGBFromColor(int color)
    {
        return new RGB((color >> 16) & 255,
                       (color >> 8 ) & 255,
                       (color      ) & 255);
    }

    /**
     * Converts an SWT RGB object into a platform-independent color value.
     * Note that the returned color value will be opaque.
     *
     * The platform-independent color value is encoded in the following format:
     * bits 0-7: blue; bits 7-15: green; bits 16-23: red; bits 24-31: alpha
     *
     * @param color the platform-independent color value to convert
     */
    public static int getColorFromRGB(RGB rgb)
    {
        return (rgb.red   << 16) |
               (rgb.green << 8 ) |
               (rgb.blue       ) ;
    }

    /**
     * Sets the alpha value encoded in a platform-independent color value.
     *
     * The platform-independent color value is encoded in the following format:
     * bits 0-7: blue; bits 7-15: green; bits 16-23: red; bits 24-31: alpha
     *
     * @param color the base platform-independent color value
     * @param alpha the alpha value to set (between 0 & 255)
     */
    public static int getColorWithAlpha(int color, int alpha)
    {
        return (color      ) |
               (alpha << 24) ;
    }

    /**
     * Returns an <code>Image</code> based on a string describing a resource.
     * If no resource is found at the specified location, returns null.
     *
     * @param resource
     * @return Image or null
     */
    public static Image getImageFromResource(String resource)
    {
        InputStream strm = ClassLoader.getSystemResourceAsStream(resource);

        if (strm != null) {
            return new Image(WindowUtil.GLOBAL_DISPLAY, new ImageData(strm));
        }

        return null;
    }

    /**
     * Returns <code>ImageData</code> based on a string describing a resource.
     * If no resource is found at the specified location, returns null.
     *
     * @param resource
     * @return ImageData or null
     */
    public static ImageData getImageDataFromResource(String resource)
    {
        InputStream strm = ClassLoader.getSystemResourceAsStream(resource);

        if (strm != null) {
            return new ImageData(strm);
        }

        return null;
    }

    /**
     * Loads an image from a stream into a byte array.
     * @param stream the image data as an InputStream
     * @return an array of bytes containing the image data
     */
    public static byte[] loadImage(InputStream stream)
    {
        ByteArrayOutputStream cache = new ByteArrayOutputStream();

        // XXX: This is absolutely horrid for efficiency.
        try {
            while (stream.available() > 0) {
                cache.write(stream.read());
            }
        }
        catch (IOException e) {
            throw new ImageException(
                "Could not load image from stream: " + e.getMessage(), e);
        }

        return cache.toByteArray();
    }

    /**
     * Loads an image from a file on disk
     * @param imageURL the URL of the file
     * @return the image data
     * @throws IOException if there was a problem loading the file
     */
    public static byte[] loadImageFromFile(String imageURL) throws IOException
    {
        if (imageURL == null) {
            return null;
        }

        RandomAccessFile imageFile = null;
        byte[] imageData = null;

        // Wrap in a try & finally to match the "code structure" used
        // else where. the error must be caught else where.
        try {
            imageFile = new RandomAccessFile(new File(imageURL), "r");

            imageData = new byte[(int) imageFile.length()];

            imageFile.readFully(imageData);
        }
        finally {
            if (imageFile != null) {
                imageFile.close();
            }
        }

        return imageData;
    }


    /**
     * Crops an image to the specified dimensions
     * @param image the image to crop
     * @param x the x-coordinate of the new origin
     * @param y the y-coordinate of the new origin
     * @param width the cropped width
     * @param height the cropped height
     * @return the cropped image
     */
    public static byte[] cropImage(byte[] image,
                                   double x,
                                   double y,
                                   double width,
                                   double height)
    {
        Image img =
            new Image(null, new ImageData(new ByteArrayInputStream(image)));
        Image out = new Image(null,
                              PrecisionUtilities.round(width),
                              PrecisionUtilities.round(height));

        GC gc = new GC(out);
        gc.drawImage(img,
                     -PrecisionUtilities.round(x),
                     -PrecisionUtilities.round(y));

        ImageLoader saver = new ImageLoader();

        saver.data = new ImageData[] { out.getImageData() };

        gc.dispose();
        out.dispose();
        img.dispose();

        ByteArrayOutputStream ret = new ByteArrayOutputStream();

        saver.save(ret, SWT.IMAGE_JPEG);

        byte[] croppedImgData = ret.toByteArray();

        try {
            ret.close();
        }
        catch (IOException e) {
            // ignore
        }

        return croppedImgData;
    }

    /**
     * A utility to load an image and then dispose it from a byte array.
     * This utility returns the size of the image.
     * This is not a good function to call often. its SLOW.
     *
     * If the image is null, then returns a new rectangle (0,0,0,0);
     * TODO: should this return null
     * @param image
     * @return
     */
    public static DoubleRectangle getImageBounds(byte[] image)
    {
        if (image == null) {
            return new DoubleRectangle(0, 0, 0, 0);
        }

        try {
            Image img = new Image(null, new ByteArrayInputStream(image));

            Rectangle rect = img.getBounds();

            img.dispose();

            return new DoubleRectangle(rect.x, rect.y, rect.width, rect.height);
        }
        catch (SWTException ex) {
            throw new ImageException("Failed to get image bounds.", ex);
        }
    }

    public static void drawOverlay(GC gc,
                                   ImageData overlayData,
                                   Rectangle area)
    {
       // GC gc = new GC(surface);

        Image overlay =
            new Image(null,
                      overlayData.scaledTo(area.width, area.height));

        try {
            gc.drawImage(overlay, area.x, area.y);
        }
        finally {
            overlay.dispose();
        }
     //   gc.dispose();
    }

    /**
     * newType is SWT.IMAGE_*
     */
    public static byte[] convertImageType(ImageData imgData, int newType)
    {
        ImageLoader loader = new ImageLoader();

        loader.data = new ImageData[] { imgData };

        ByteArrayOutputStream out = new ByteArrayOutputStream(32768);
        loader.save(out, newType);

        return out.toByteArray();
    }
}
