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

package edu.cmu.cs.hcii.cogtool.uimodel;

import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.swt.graphics.Image;

public class NinePartRenderer extends ImageRenderer
{
    protected static String NINE_PART_LOCATION =
        "edu/cmu/cs/hcii/cogtool/resources/widgets/9part/";

    // Depending on the state of the widget, imageParts is either defaultSet
    // or altSet
    protected Image[] imageParts = null;
    protected Image[] defaultSet = null;
    protected Image[] altSet = null;

    protected String skinName;

    protected NinePartRenderer(GraphicalWidget<?> parent,
                               String widgetName,
                               String skin,
                               int labelStyle,
                               int area,
                               DefaultSEUIModel override)
    {
        super(parent, widgetName, skin, labelStyle, area, override);

        skinName = skin;

        defaultSet = getNinePartImages(skinName, widgetName, "");

        try {
            altSet = getNinePartImages(skinName, widgetName, "s");
        }
        catch (Exception e) {
            // "alt" images don't necessarily exist for all widgets
            altSet = null;
        }

        imageParts = defaultSet;
    }

    /**
     * Loads the nine images needed by a nine-part image
     * artist. Assumes that the images have names of the form
     * &lt;skin&gt;/&lt;name&gt;/&lt;name&gt;&lt;number&gt;.jpg.
     */
    protected Image[] getNinePartImages(String skin, String name, String suffix)
    {
        Image[] iAry = new Image[9];

        for (int i = 0; i < iAry.length; i++) {
            iAry[i] = getImage(NINE_PART_LOCATION + skin + "/" + name + "/"
                                + i + suffix + "." + FILETYPE);
        }

        return iAry;
    }// getNinePartImages


    public void paintBackground(Graphics g)
    {
        if ((imageParts == null) ||
            (imageParts.length == 0) ||
            (imageParts[0] == null))
        {
            return;
        }

        g.pushState();

        try {
            // This is the clip to the figure. We must clip to this the whole time!
            Rectangle mainClip = getBounds();

            g.setClip(mainClip);

            // This will represent temporary clips while drawing the 9part.
            // All subClips must be a union of mainClip and something else.
            Rectangle subClip = mainClip;

            // This is the top and left of the 9-part tiled image.
            // widgetBounds.x and .y should both always be 0 since we want to fill
            // the space we have been given.
            int topOfImg = widgetBounds.x;
            int leftOfImg = widgetBounds.y;

            // These variables represent the distances between the actual edges
            // of the 9-part tiling and the edge of the center area of the 9-part.
            // we will assume we have all images so no error checking is needed.
            int leftSize = imageParts[0].getBounds().width;
            int rightSize = imageParts[2].getBounds().width;
            int topSize = imageParts[0].getBounds().height;
            int bottomSize = imageParts[6].getBounds().height;

            // These variables represent the absolute coordinates
            // of the edges of the center area.
            int rightOfCenter =
                leftOfImg + widgetBounds.width - rightSize;
            int leftOfCenter = leftOfImg + leftSize;
            int topOfCenter = topOfImg + topSize;
            int bottomOfCenter =
                topOfImg + widgetBounds.height - bottomSize;

            // These are  variables for when I am drawing a row of images.
            int drawX;
            int drawY;

            // When I am drawing a row of images, these variables store the
            // dimension of the image I'm drawing.
            int imgWidth;
            int imgHeight;


            // Setting the subclip for drawing the center area.
            subClip = mainClip;
            subClip =
                subClip.getIntersection(
                               new Rectangle(leftOfCenter,
                                             topOfCenter,
                                             rightOfCenter - leftOfCenter + 2,
                                             bottomOfCenter - topOfCenter + 2));
            g.setClip(subClip);

            if (imageParts[4] != null) {
                imgWidth = imageParts[4].getBounds().width;
                imgHeight = imageParts[4].getBounds().height;
                for (drawX = leftOfCenter;
                     drawX < rightOfCenter;
                     drawX += imgWidth)
                {
                    for (drawY = topOfCenter;
                         drawY < bottomOfCenter;
                         drawY += imgHeight)
                    {
                        g.drawImage(imageParts[4], drawX, drawY);
                    }
                }
            }

            // Setting the subclip for drawing horizontal rows.
            subClip = mainClip;
            subClip =
                subClip.getIntersection(
                                 new Rectangle(leftOfCenter,
                                               topOfImg,
                                               rightOfCenter - leftOfCenter + 2,
                                               widgetBounds.height));
            g.setClip(subClip);

            // Drawing the top row.
            if (imageParts[1] != null) {
                imgWidth = imageParts[1].getBounds().width;

                for (drawX = leftOfCenter;
                     drawX < rightOfCenter;
                     drawX += imgWidth)
                {
                    g.drawImage(imageParts[1], drawX, topOfImg);
                }
            }

            // Drawing the bottom row.
            if (imageParts[7] != null) {
                imgWidth = imageParts[7].getBounds().width;

                for (drawX = leftOfCenter;
                     drawX < rightOfCenter;
                     drawX += imgWidth)
                {
                    g.drawImage(imageParts[7], drawX, bottomOfCenter);
                }
            }

            // Setting the subclip for drawing vertical rows.
            subClip = mainClip;
            subClip =
                subClip.getIntersection(
                               new Rectangle(leftOfImg,
                                             topOfCenter,
                                             widgetBounds.width,
                                             bottomOfCenter - topOfCenter + 2));
            g.setClip(subClip);

            // Drawing the left side row.
            if (imageParts[3] != null) {
                imgHeight = imageParts[3].getBounds().height;

                for (drawY = topOfCenter;
                     drawY < bottomOfCenter;
                     drawY += imgHeight)
                {
                    g.drawImage(imageParts[3], leftOfImg, drawY);
                }
            }

            // Drawing the right side row.
            if (imageParts[5] != null) {
                imgHeight = imageParts[5].getBounds().height;

                for (drawY = topOfCenter;
                     drawY < bottomOfCenter;
                     drawY += imgHeight)
                {
                    g.drawImage(imageParts[5], rightOfCenter, drawY);
                }
            }

            // Reset the clip to the main clip, the entire size of the frame.
            g.setClip(mainClip);

            // Drawing the top left corner.
            if (imageParts[0] != null) {
                g.drawImage(imageParts[0], leftOfImg, topOfImg);
            }

            // Drawing the top right corner.
            if (imageParts[2] != null) {
                g.drawImage(imageParts[2], rightOfCenter, topOfImg);
            }

            // Drawing the bottom left corner.
            if (imageParts[6] != null) {
                g.drawImage(imageParts[6], leftOfImg, bottomOfCenter);
            }

            // Drawing the bottom right corner.
            if (imageParts[8] != null) {
                g.drawImage(imageParts[8], rightOfCenter, bottomOfCenter);
            }
        }
        finally {
            g.popState();
        }
    } // paintBackground



    public void paintForeground(Graphics g)
    {
        if (label != null) {
            label.paint(g);
        }
    }
}
