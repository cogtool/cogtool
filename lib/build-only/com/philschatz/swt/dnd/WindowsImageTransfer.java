package com.philschatz.swt.dnd;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.internal.ole.win32.COM;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Singleton class that reads Images (CF_DIB) from the Windows Clipboard as
 * ImageData objects.
 *
 * If you found this class useful (or made some improvements)   drop me a line.
 *
 *
 * &#64;author Philip Schatz ( www.philschatz.com )
 */
public class WindowsImageTransfer extends ByteArrayTransfer {

      private static final WindowsImageTransfer INSTANCE = new   WindowsImageTransfer();
      private static final String CF_DIB = "CF_DIB";
      private static final int CF_DIBID = COM.CF_DIB;

      private WindowsImageTransfer() {
              //Singleton
      }

      public static WindowsImageTransfer getInstance() {
              return WindowsImageTransfer.INSTANCE;
      }

      protected final int[] getTypeIds() {
              return new int[]{WindowsImageTransfer.CF_DIBID};
      }

      public final String[] getTypeNames() {
              return new String[]{WindowsImageTransfer.CF_DIB};
      }

      protected Object nativeToJava(TransferData transferData) {
              final Object o = super.nativeToJava(transferData);
              final byte[] bytes = (byte[]) o;

              try {
                      final InputStream bis = new PrependWinBMPHeaderFilterInputStream(
                                      new UncompressDibFilterInputStream(new ByteArrayInputStream(
                                                      bytes)));
                      final ImageData[] data = new ImageLoader().load(bis);
                      if (data.length < 1) {
                              return null;
                      }
                      return data[0];
              } catch (IOException e) {
                      return null;
              }
      }
      protected void javaToNative(Object object, TransferData transferData) {
              final ImageData imgData = (ImageData) object;
              final ImageLoader loader = new ImageLoader();
              final ByteArrayOutputStream bos = new ByteArrayOutputStream();
              final byte[] bytes;

              loader.data = new ImageData[]{imgData};
              loader.save(new RemoveWinBMPHeaderFilterOutputStream(bos), SWT.IMAGE_BMP);
              bytes = bos.toByteArray();
              super.javaToNative(bytes, transferData);
      }
      
      public static void main(String[] args) throws Exception {
          Display display = new Display();
          Shell shell = new Shell(display);
          Clipboard c = new Clipboard(display);

          System.out.println("types on the clipboard: "
                          + Arrays.asList(c.getAvailableTypeNames()));
          Object o = c.getContents(WindowsImageTransfer.getInstance());
          ImageData d = (ImageData) o;
          if (d == null) {
                  System.out.println("no image found on clipboard. try hitting the printscreen key, or using MS Paint to put an image on the clipboard.");
                  return;
          }

          //Change what's on the clipboard to show we can also put images on the
          // clipboard.
          c.setContents(new Object[]{"howdy"}, new Transfer[]{TextTransfer
                          .getInstance()});
          System.out.println("types on the clipboard: "

                          + Arrays.asList(c.getAvailableTypeNames()));

          //now put the ImageData back onto the clipboard.
          c.setContents(new Object[]{d}, new Transfer[]{WindowsImageTransfer
                          .getInstance()});
          System.out.println("types on the clipboard: "
                          + Arrays.asList(c.getAvailableTypeNames()));

          //now read the CF_DIB (ImageData) back off the clipboard.
          // and display it by using it as the image for the button.
          ImageData imgData = (ImageData) c.getContents(WindowsImageTransfer
                          .getInstance());

          Image img = new Image(display, imgData);
          Button button = new Button(shell, SWT.NONE);
          button.setImage(img);
          shell.pack();
          button.pack();
          shell.open();

          while (!shell.isDisposed()) {
                  if (!display.readAndDispatch()) {
                          display.sleep();
                  }
          }
          display.dispose();
          button.dispose();
  }
}