package edu.cmu.cs.hcii.cogtool.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

public class Keypad extends WindowUtil.SimpleDialog
{
    public static final int FULL_KEYPAD = 1;
    public static final int NUMPAD_ONLY = 2;

    public static final int FULL_WIDTH = 935;
    public static final int NUMPAD_WIDTH = 150;

    public static final String SHIFT_KEY =
        L10N.get("KEYPAD.SHIFT", "SHIFT");
    public static final String CAPS_KEY =
        L10N.get("KEYPAD.CAPS_LOCK", "CAPS");
    public static final String SPACE_KEY =
        L10N.get("KEYPAD.SPACE", "SPACE");
    public static final String BS_KEY =
        L10N.get("KEYPAD.BACKSPACE", "BACKSPACE");
    public static final String ENTER_KEY =
        L10N.get("KEYPAD.ENTER", "Enter");
    public static final String CANCEL_KEY =
        L10N.get("KEYPAD.CANCEL", "Cancel");

    protected static Color disabledColor = null;
    protected static Color defaultBGColor = null;

    protected Color getDisabledColor()
    {
        if (disabledColor == null) {
            disabledColor =
                dialog.getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        return disabledColor;
    }

    protected Color getDefaultBGColor()
    {
        if (defaultBGColor == null) {
            defaultBGColor =
                dialog.getDisplay().getSystemColor(SWT.COLOR_WHITE);
        }

        return defaultBGColor;
    }

    protected class ButtonHitDetection implements SelectionListener
    {
        public void widgetSelected(SelectionEvent event)
        {
            if (shift) {
                shift = ! shift;
            }

            String buttonLabel = ((Button) event.getSource()).getText();

            if (buttonLabel.equals(SHIFT_KEY)) {
                shift = ! shift;
//                redisplayKeys();
            }
            else if (buttonLabel.equals(CAPS_KEY)) {
                capsLock = ! capsLock;
//                redisplayKeys();
            }
            else if (buttonLabel.equals(SPACE_KEY)) {
                enteredText.append(" ");
            }
            else if (buttonLabel.equals(BS_KEY)) {
                String text = enteredText.getText();
                int currentLen = text.length();

                if (enteredText.getSelectionCount() == 0) {
                    enteredText.setText(text.substring(0, currentLen - 1));
                    enteredText.setSelection(currentLen - 1);
                }
                else {
                    Point sel = enteredText.getSelection();

                    String tail = (currentLen == sel.y)
                                    ? ""
                                    : text.substring(sel.y, currentLen - 1);

                    enteredText.setText(text.substring(0, sel.x).concat(tail));
                    enteredText.setSelection(sel.x);
                }
            }
            else if (buttonLabel.equals("&&")) {
                enteredText.append("&");
            }
            else {
                enteredText.append(buttonLabel);
            }

            if (keypadType == FULL_KEYPAD) {
                redisplayKeys();
            }
        }


        public void widgetDefaultSelected(SelectionEvent evt)
        {
            widgetSelected(evt);
        }
    }

    protected SelectionListener hitDetection = new ButtonHitDetection();

    protected static final String keysRow1[] =
        { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=" };
    protected static final String keysRow2[] =
        { "q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "[", "]", "\\" };
    protected static final String keysRow3[] =
        { "a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "'" };
    protected static final String keysRow4[] =
        { "z", "x", "c", "v", "b", "n", "m", ",", ".", "/" };
    protected static final String keysRow1_shift[] =
        { "!", "@", "#", "$", "%", "^", "&&", "*", "(", ")", "_", "+" };
    protected static final String keysRow2_shift[] =
        { "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", "{", "}", "|" };
    protected static final String keysRow3_shift[] =
        { "A", "S", "D", "F", "G", "H", "J", "K", "L", ":", "\"" };
    protected static final String keysRow4_shift[] =
        { "Z", "X", "C", "V", "B", "N", "M", "<", ">", "?" };
    protected static final String numberKeys[] =
        { "1", "2", "3", "4", "5", "6", "7", "8", "9", "-", "0", ".", BS_KEY };

    protected Button letterButtons[] =
        new Button[keysRow1.length + keysRow2.length
                                   + keysRow3.length
                                   + keysRow4.length];

    protected boolean shift = false;
    protected boolean capsLock = false;
    protected int keypadType;
    protected Text enteredText;

    public Keypad(String title, int mode, int type)
    {
        super(title, mode);

        keypadType = type;
    }

    protected Button makeButton(Composite parent,
                                String buttonLabel,
                                GridData gridKeysOption)
    {
        return makeButton(parent,
                          buttonLabel,
                          gridKeysOption,
                          hitDetection);
    }

    protected Button makeButton(Composite parent,
                                String buttonLabel,
                                GridData gridKeysOption,
                                SelectionListener listener)
    {
        Button keypadButton = new Button(parent, SWT.PUSH);

        keypadButton.setText(buttonLabel);
        keypadButton.setFont(FontUtils.DEFAULT_FONT);
        keypadButton.setLayoutData(gridKeysOption);
        keypadButton.addSelectionListener(listener);

        return keypadButton;
    }

    /**
     * Creates and lays out the buttons for the keypad's letter keys.
     *
     * @author Paul Rubritz (ptr@andrew.cmu.edu)
     */
    protected void makeLetters()
    {
        Composite letters = new Composite(dialog, SWT.NONE);
        GridLayout grid = new GridLayout();

        grid.numColumns = 13;
        letters.setLayout(grid);

        GridData gridKeysOption;

        int row = 1;
        int rowLength = keysRow1.length;
        int buttonCount = 0;

        for (int i = 0; i < rowLength; i++) {
            String buttonLabel = "";

            gridKeysOption = new GridData();

            gridKeysOption.widthHint = 50;
            gridKeysOption.heightHint = 50;

            switch (row) {
                case 1: {
                    buttonLabel = keysRow1[i];
                    break;
                }
                case 2: {
                    buttonLabel = keysRow2[i];
                    break;
                }
                case 3: {
                    buttonLabel = keysRow3[i];
                    break;
                }
                case 4: {
                    buttonLabel = keysRow4[i];
                    break;
                }
            }

            if (i == rowLength - 1) {
                row++;

                switch (row) {
                    case 2: {
                        rowLength = keysRow2.length;
                        i = -1;
                        gridKeysOption.horizontalSpan = 2;
                        break;
                    }
                    case 3: {
                        rowLength = keysRow3.length;
                        i = -1;
                        break;
                    }
                    case 4: {
                        rowLength = keysRow4.length;
                        i = -1;
                        gridKeysOption.horizontalSpan = 3;
                        break;
                    }
                    case 5: {
                        gridKeysOption.horizontalSpan = 4;
                        break;
                    }
                    default: {
                        break;
                    }
                }
            }

            letterButtons[buttonCount++] =
                makeButton(letters, buttonLabel, gridKeysOption);
        }

        /* shift button */
        gridKeysOption = new GridData();
        gridKeysOption.horizontalSpan = 2;
        gridKeysOption.widthHint = 105;
        gridKeysOption.heightHint = 50;

        makeButton(letters, SHIFT_KEY, gridKeysOption);

        /* caps lock button */
        gridKeysOption = new GridData();
        gridKeysOption.horizontalSpan = 2;
        gridKeysOption.widthHint = 105;
        gridKeysOption.heightHint = 50;

        makeButton(letters, CAPS_KEY, gridKeysOption);

        /* space bar button */
        gridKeysOption = new GridData();
        gridKeysOption.horizontalSpan = 6;
        gridKeysOption.widthHint = 325;
        gridKeysOption.heightHint = 50;

        makeButton(letters, SPACE_KEY, gridKeysOption);

        /* backspace button */
        gridKeysOption = new GridData();
        gridKeysOption.horizontalSpan = 3;
        gridKeysOption.widthHint = 160;
        gridKeysOption.heightHint = 50;

        makeButton(letters, BS_KEY, gridKeysOption);
    }

    /**
     * Creates and lays out the buttons for the keypad's number pad.
     *
     * @author Paul Rubritz (ptr@andrew.cmu.edu)
     */
    protected void makeNumbers()
    {
        GridLayout grid = new GridLayout();
        grid.numColumns = 3;

        GridData gridKeysOption = new GridData();

        gridKeysOption.horizontalIndent =
            (keypadType == FULL_KEYPAD) ? 60 : 0;
        gridKeysOption.verticalAlignment = SWT.TOP;

        Composite numbers = new Composite(dialog, SWT.NONE);

        numbers.setLayoutData(gridKeysOption);
        numbers.setLayout(grid);

        Button keypadButton;

        for (int i = 0; i < numberKeys.length; i++) {
            gridKeysOption = new GridData();

            if (i < numberKeys.length - 1) {
                gridKeysOption.widthHint = 50;
            }
            else if (keypadType == NUMPAD_ONLY) {
                gridKeysOption.widthHint = 160;
                gridKeysOption.horizontalSpan = 3;
            }
            else {
                break;
            }

            gridKeysOption.heightHint = 50;

            keypadButton = makeButton(numbers, numberKeys[i], gridKeysOption);

            // TODO: This must be dependent upon the invocation context
            //       (see IntegerEntry)
            if ((keypadType == NUMPAD_ONLY) &&
                numberKeys[i].equals("-"))
            {
                keypadButton.setBackground(getDisabledColor());
                keypadButton.setEnabled(false);
            }
        }
    }

    /**
     * Creates and lays out the elements for the keypad's enter/cancel box.
     *
     * @author Paul Rubritz (ptr@andrew.cmu.edu)
     */
    protected void makeEntry()
    {
        GridLayout grid = new GridLayout();
        grid.numColumns = 3;

        GridData gridKeysOption = new GridData();

        gridKeysOption.horizontalSpan = 2;

        Composite entry = new Composite(dialog, SWT.NONE);

        entry.setLayoutData(gridKeysOption);
        entry.setLayout(grid);

        int width =
            (keypadType == FULL_KEYPAD) ? FULL_WIDTH : NUMPAD_WIDTH;

        /* text input box */
        enteredText = new Text(entry, SWT.NONE);
        enteredText.setFont(FontUtils.DEFAULT_FONT);

        gridKeysOption = new GridData();
        gridKeysOption.horizontalSpan = 3;
        gridKeysOption.widthHint = width;
        gridKeysOption.heightHint = 100;

        enteredText.setLayoutData(gridKeysOption);
        enteredText.setBackground(getDefaultBGColor());
        enteredText.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent evt)
            {
                if (evt.keyCode == SWT.ESC) {
                    userResponse = null;
                    dialog.close();
                }
            }
        });

        /* enter button */
        gridKeysOption = new GridData();
        gridKeysOption.verticalIndent = 10;
        gridKeysOption.horizontalSpan = 2;
        gridKeysOption.widthHint = width / 2;
        gridKeysOption.heightHint = 75;
        gridKeysOption.horizontalAlignment = SWT.RIGHT;

        SelectionListener listener =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent event)
                {
                    userResponse = enteredText.getText();
                    dialog.close();
                }
            };

        makeButton(entry, ENTER_KEY, gridKeysOption, listener);

        /* cancel button */
        gridKeysOption = new GridData();
        gridKeysOption.verticalIndent = 10;
        gridKeysOption.horizontalSpan = 1;
        gridKeysOption.widthHint = width / 2;
        gridKeysOption.heightHint = 75;
        gridKeysOption.horizontalAlignment = SWT.RIGHT;

        listener =
            new SelectionAdapter()
            {
                @Override
                public void widgetSelected(SelectionEvent event)
                {
                    userResponse = null;
                    dialog.close();
                }
            };

        makeButton(entry, CANCEL_KEY, gridKeysOption, listener);
    }

    /**
     * Displays appropriate changes to keys when shift or caps lock is pressed.
     *
     * @author Paul Rubritz (ptr@andrew.cmu.edu)
     */
    public void redisplayKeys()
    {
        int btnIndex = 0;
        int row = 1;
        int rowLength = keysRow1.length;

        for (int i = 0; i < rowLength; i++) {
            Button keypadButton = letterButtons[btnIndex++];

            if ((! shift && ! capsLock) ||
                (capsLock && shift))
            {
                switch (row) {
                    case 1: {
                        keypadButton.setText(keysRow1[i]);
                        break;
                    }
                    case 2: {
                        keypadButton.setText(keysRow2[i]);
                        break;
                    }
                    case 3: {
                        keypadButton.setText(keysRow3[i]);
                        break;
                    }
                    case 4: {
                        keypadButton.setText(keysRow4[i]);
                        break;
                    }
                }
            }
            else {
                switch (row) {
                    case 1: {
                        keypadButton.setText(keysRow1_shift[i]);
                        break;
                    }
                    case 2: {
                        keypadButton.setText(keysRow2_shift[i]);
                        break;
                    }
                    case 3: {
                        keypadButton.setText(keysRow3_shift[i]);
                        break;
                    }
                    case 4: {
                        keypadButton.setText(keysRow4_shift[i]);
                        break;
                    }
                }
            }

            if (i == rowLength - 1) {
                row++;

                switch (row) {
                    case 2: {
                        rowLength = keysRow2.length;
                        i = -1;
                        break;
                    }
                    case 3: {
                        rowLength = keysRow3.length;
                        i = -1;
                        break;
                    }
                    case 4: {
                        rowLength = keysRow4.length;
                        i = -1;
                        break;
                    }
                    default: {
                        break;
                    }
                }
            }
        }
    }

    @Override
    protected void buildDialog()
    {
        GridLayout grid = new GridLayout();

        switch (keypadType) {
            case FULL_KEYPAD: {
                grid.numColumns = 2;

                makeLetters();
                makeNumbers();
                makeEntry();

                break;
            }
            case NUMPAD_ONLY: {
                grid.numColumns = 1;

                makeNumbers();
                makeEntry();

                break;
            }
        }

        dialog.setLayout(grid);
    }
}
