/*
 * Created on Jul 25, 2004
 */
package com.philschatz.swt.dnd;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Strips off the BITMAPFILEHEADER from a ImageLoader.save() call
 *
 * Used when converting from ImageData to CF_DIB (pasting on clipboard)
 *
 * &#64;author Philip Schatz ( www.philschatz.com )
 */
class RemoveWinBMPHeaderFilterOutputStream extends OutputStream {
        private final OutputStream out;
        private int counter = 0;

        public RemoveWinBMPHeaderFilterOutputStream(OutputStream out) {
                this.out = out;
        }
        public void write(int b) throws IOException {
                //ignore the bmp file header
                if (this.counter < PrependWinBMPHeaderFilterInputStream.BITMAPFILEHEADER_SIZEOF) {
                        this.counter++;
                } else {
                        this.out.write(b);
                }
        }
}