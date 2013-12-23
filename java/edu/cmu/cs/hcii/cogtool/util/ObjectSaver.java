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

import java.io.Writer;
import java.lang.reflect.Array;
import java.rmi.UnexpectedException;
import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.ecf.core.util.Base64;

/**
 * Class to support the serialization of normal Java objects into
 * an XML stream, suitable for saving persistently.
 * <p>
 * Every object must register a saver (see, for example,
 * <code>CogToolSerialization</code>).
 * <p>
 * This architecture handles objects that are array types or that subclass
 * <code>Collection</code> and <code>Map</code>.  However, it does *not*
 * handle <code>Map</code> key values that themselves are aggregates.
 * <p>
 * All "primitive" values (int, double, char, boolean, their "object"
 * counterparts, and String) are stored directly as base cases.  Enumeration
 * values are determined using the <code>isEnum</code> method of the
 * class' saver object and the representation stored is based on the
 * enum "code" (we currently assume that the enumeration class subclasses
 * <code>Enumerated</code>.
 * <p>
 * The serialization of an object class will store the information held
 * by the class' superclass if the superclass has also registered a saver.
 * Once a superclass is encountered that has not registered saver, no further
 * attempts are made, even if some ancestor superclass has registered a saver.
 * <p>
 * Serializations can be for several purposes; an object class may
 * choose to save itself differently for different purposes.
 * <p>
 * Multiple objects can be saved in a single serialization.  The
 * <code>finish</code> method should be invoked when all objects of interest
 * have been saved.  If the <code>Writer</code> involves any system resources
 * that should be recovered, the appropriate method should also then be called
 * (such as the <code>close()</code> method for <code>FileWriter</code>).
 *
 * @author mlh
 */
public class ObjectSaver implements ObjectPersist
{
    /**
     * A class' saver informs us about the version of the serialization
     * that will be produced, whether the value will represent an enumeration,
     * and how to serialize the "persistent" data of the value.
     *
     * @author mlh
     */
    public interface IDataSaver<T>
    {
        /**
         * Return the format version for this serialization of the object.
         *
         * @return the format version for this serialization of the object
         * @author mlh
         */
        public int getVersion();

        /**
         * Return whether the value represents an enumeration element.
         *
         * @return true if and only if the value represents an enumeration
         * @author mlh
         */
        public boolean isEnum();   // if so, saveData() is not interesting

        /**
         * Serialize the data of the given object value using the given saver.
         *
         * @param v the object to serialize
         * @param saver the "destination" of the serialization
         * @throws java.io.IOException if the saver's sink generates one
         *         during calls to <code>write</code>
         * @author mlh
         */
        public void saveData(T v, ObjectSaver saver)
            throws java.io.IOException;
    }

    /**
     * This represents a default implementation of a saver that assumes
     * the value is not an enumeration and otherwise throws exceptions
     * to indicate unsupported operations.
     *
     * @author mlh
     */
    public static class ADataSaver<T> implements IDataSaver<T>
    {

        public boolean isEnum()
        {
            return false;
        }


        public void saveData(T v, ObjectSaver saver)
            throws java.io.IOException
        {
            throw new UnsupportedOperationException("saveData");
        }


        public int getVersion()
        {
            throw new UnsupportedOperationException("getVersion");
        }
    }

    public interface ISaverRegistry
    {
        /**
         * Every object must register a saver that specifies how to serialize
         * its value.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @param saver the saver object that controls how to serialize a value
         *              of the given type
         * @author mlh
         */
        public <E> void registerSaver(String className, IDataSaver<E> saver);

        /**
         * Every object must register a saver that specifies how to serialize
         * its value.  This method returns the saver associated with the given
         * class name.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @return the saver object that controls how to serialize a value of
         *         the given type, or <code>null</code> if none was registered
         * @author mlh
         */
        public IDataSaver<?> getSaver(String className);
    }

    /**
     * Default implementation, especially for the default registry.
     */
    public static class DefaultSaverRegistry implements ISaverRegistry
    {
        // registry of IDataSavers mapping
        // (class-name + '#' + version) to IDataSaver
        protected Map<String, IDataSaver<?>> saverRegistry =
            new HashMap<String, IDataSaver<?>>();


        public <E> void registerSaver(String className, IDataSaver<E> saver)
        {
            saverRegistry.put(className, saver);
        }


        public IDataSaver<?> getSaver(String className)
        {
            return saverRegistry.get(className);
        }
    }

    /**
     * Allows the caller to provide different behavior for a subset of
     * objects, maintaining "normal" behavior for other objects.
     */
    public static class OverrideSaverRegistry extends DefaultSaverRegistry
    {
        protected ISaverRegistry overriddenRegistry;

        public OverrideSaverRegistry(ISaverRegistry overridden)
        {
            overriddenRegistry = overridden;
        }

        @Override
        public IDataSaver<?> getSaver(String className)
        {
            IDataSaver<?> saver = super.getSaver(className);

            if (saver != null) {
                return saver;
            }

            return overriddenRegistry.getSaver(className);
        }
    }

    /**
     * Default registry for savers.
     */
    public static final ISaverRegistry DEFAULT_REGISTRY =
        new DefaultSaverRegistry();

    // Registry to use for the saving process
    protected ISaverRegistry saverRegistry;

    // maps Object to id
    protected Map<Object, Integer> savedObjects =
        new IdentityHashMap<Object, Integer>();

    // The sink receives the XML serialization.
    protected Writer sink;

    // The indentation is based on the nesting level of the object "hierarchy"
    // TODO: Make this into a StringBuilder
    protected String indent = "";

    // For generating unique id's (see savedObjects above)
    protected int idGen = 1;

    // Serializations can be for several purposes; an object class may
    // choose to save itself differently for different purposes.
    protected Object purpose;

    /**
     * The default purpose for serialization.
     */
    public static final Object DEFAULT_PURPOSE = null;

    /**
     * A constructor using the default "purpose".  The serialization will be
     * "saved" to the given sink.  When all objects of interest have been saved,
     * the <code>finish()</code> method should be invoked.  If the
     * <code>Writer</code> involves any system resources that should be
     * recovered, the appropriate method should also then be called
     * (such as the <code>close()</code> method for <code>FileWriter</code>).
     *
     * @param objectSink the sink that will accept the serialization as
     *                   it is generated
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public ObjectSaver(Writer objectSink)
        throws java.io.IOException
    {
        this(objectSink, null);
    }

    /**
     * A constructor for a specific "purpose".  The serialization will be
     * "saved" to the given sink.  When all objects of interest have been
     * saved, the <code>finish()</code> method should be invoked.  If the
     * <code>Writer</code> involves any system resources that should be
     * recovered, the appropriate method should also then be invoked by the
     * caller (such as <code>close()</code> for a <code>FileWriter</code>).
     *
     * @param objectSink the sink that will accept the serialization as
     *                   it is generated
     * @param savePurpose the purpose for the serialization; this may be used
     *                    by object savers to alter exactly what is serialized
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public ObjectSaver(Writer objectSink, Object savePurpose)
        throws java.io.IOException
    {
        this(objectSink, savePurpose, null);
    }

    /**
     * A constructor for a specific "purpose".  The serialization will be
     * "saved" to the given sink.  When all objects of interest have been
     * saved, the <code>finish()</code> method should be invoked.  If the
     * <code>Writer</code> involves any system resources that should be
     * recovered, the appropriate method should also then be invoked by the
     * caller (such as <code>close()</code> for a <code>FileWriter</code>).
     *
     * @param objectSink the sink that will accept the serialization as
     *                   it is generated
     * @param savePurpose the purpose for the serialization; this may be used
     *                    by object savers to alter exactly what is serialized
     * @param registry the saver registry to use for fetching the IDataSaver
     *                 instances for saving each object
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public ObjectSaver(Writer objectSink,
                       Object savePurpose,
                       ISaverRegistry registry)
        throws java.io.IOException
    {
        saverRegistry = (registry != null) ? registry : DEFAULT_REGISTRY;
        sink = objectSink;
        purpose = savePurpose;

        // Start the outermost XML element
        sink.write("<" + PERSIST_ELT
                            + addAttribute(VERSION_ATTR,
                                           Integer.toString(FORMAT_VERSION))
                            + addAttribute("cogtool_version",
                                           System.getProperty("cogtool.version"))
                            + addAttribute("cogtool_revision",
                                           System.getProperty("cogtool.revision"))
                            + addAttribute("cogtool_buildtime",
                                           System.getProperty("cogtool.build"))
                            + addAttribute("java_version",
                                           System.getProperty("java.version"))
                            + addAttribute("os_version",
                                           System.getProperty("os.version"))
                            + addAttribute("os_name",
                                           System.getProperty("os.name"))
                            + ">\n");
    }

    /**
     * Return the registry associated with this saver.
     */
    public ISaverRegistry getRegistry()
    {
        return saverRegistry;
    }

    /**
     * Terminates the serialization and flushes the output sink.
     *
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code> or <code>flush</code>
     * @author mlh
     */
    public void finish()
        throws java.io.IOException
    {
        sink.write("</" + PERSIST_ELT + ">\n");
        sink.flush();
    }

    /**
     * Return the "purpose" for this serialization; an object class may
     * choose to save itself differently for different purposes.
     *
     * @return the purpose given on construction
     * @author mlh
     */
    public Object getPurpose()
    {
        return purpose;
    }

    /**
     * Every object must register a saver that specifies how to serialize
     * its value.
     *
     * @param className the name of the class; this should almost always be
     *                  the value of <code>C.class.getName()</code>
     * @param saver the saver object that controls how to serialize a value
     *              of the given type
     * @author mlh
     */
    public static <T> void registerSaver(String className, IDataSaver<T> saver)
    {
        DEFAULT_REGISTRY.registerSaver(className, saver);
    }

    /**
     * This method quotes the given string so that it results in a valid
     * XML attribute value (e.g., double quotes are represented by the
     * appropriate XML entity).
     *
     * @param str the string to be quoted
     * @return the quoted string
     * @author mlh
     */
    protected String quote(String str)
    {
        // TODO: needs something like PHP's htmlentities in Java
        return (str != null) ? str.replaceAll("\"", "&quot;") : "";
    }

    /**
     * This method returns the proper form of an XML element attribute
     * with the given name and value.
     *
     * @param attr the attribute name
     * @param value the attribute value
     * @return the substring representing the proper form of an XML attribute
     * @author mlh
     */
    protected String addAttribute(String attr, String value)
    {
        return " " + attr + "=\"" + quote(value) + "\"";
    }

    /**
     * This method produces an XML element attribute for the "variable" name
     * if the name is specified (that is, not <code>null</code>).
     *
     * @param variable the name of the variable attribute;
     *                 may be <code>null</code>
     * @return the string representing the "variable" XML attribute if the
     *         given string is not <code>null</code>;
     *         the empty string otherwise
     * @author mlh
     */
    protected String addVariableAttribute(String variable)
    {
        return (variable != null) ? addAttribute(VAR_ATTR, variable) : "";
    }

    /**
     * Method to save a primitive <code>int</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveInt(int value)
        throws java.io.IOException
    {
        saveInt(value, null);
    }

    /**
     * Method to save a primitive <code>long</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveLong(long value)
        throws java.io.IOException
    {
        saveLong(value, null);
    }

    /**
     * Method to save a primitive <code>double</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveDouble(double value)
        throws java.io.IOException
    {
        saveDouble(value, null);
    }

    /**
     * Method to save a primitive <code>boolean</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveBoolean(boolean value)
        throws java.io.IOException
    {
        saveBoolean(value, null);
    }

    /**
     * Method to save a <code>String</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveString(String value)
        throws java.io.IOException
    {
        saveString(value, null);
    }

    /**
     * Method to save a primitive <code>char</code> value that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveChar(char value)
        throws java.io.IOException
    {
        saveChar(value, null);
    }

    /**
     * Method to save an object that is either
     * a top-level value or an element in an array, <code>Map</code>, or
     * <code>Collection</code>, but not an instance variable of another object.
     * To be successful, a saver for the value's class must have been
     * previously registered.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the value to serialize
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @throws UnexpectedException if no saver has been registered for the
     *         value's class
     * @author mlh
     */
    public void saveObject(Object value)
        throws java.io.IOException
    {
        saveObject(value, null);
    }

    /**
     * Method to save a primitive <code>int</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveInt(int value, String variable)
        throws java.io.IOException
    {
        sink.write(indent
                            + "<" + INT_ELT
                            + addVariableAttribute(variable)
                            + addAttribute(VALUE_ATTR, Integer.toString(value))
                            + "/>\n");
    }

    /**
     * Method to save a primitive <code>long</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveLong(long value, String variable)
        throws java.io.IOException
    {
        sink.write(indent
                            + "<" + LONG_ELT
                            + addVariableAttribute(variable)
                            + addAttribute(VALUE_ATTR, Long.toString(value))
                            + "/>\n");
    }

    /**
     * Method to save a primitive <code>double</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveDouble(double value, String variable)
        throws java.io.IOException
    {
        sink.write(indent
                            + "<" + DOUBLE_ELT
                            + addVariableAttribute(variable)
                            + addAttribute(VALUE_ATTR, Double.toString(value))
                            + "/>\n");
    }

    /**
     * Method to save a primitive <code>boolean</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveBoolean(boolean value, String variable)
        throws java.io.IOException
    {
        sink.write(indent
                            + "<" + BOOL_ELT
                            + addVariableAttribute(variable)
                            + addAttribute(VALUE_ATTR,
                                           (value ? BOOL_TRUE : BOOL_FALSE))
                            + "/>\n");
    }

    private static String LEGAL_LOW_CHARACTERS = "\t\n\r";
    
    /**
     * Method to save a <code>String</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveString(String value, String variable)
        throws java.io.IOException
    {
        if (value == null) {
            sink.write(indent
                                + "<" + NULL_ELT
                                + addVariableAttribute(variable)
                                + "/>\n");
        }
        else {
            // TODO perhaps this shouldn't be here in perpetuity; it's been
            //      added to try to track down an infrequently recurring
            //      problem where one user sometimes gets a Unit Separator
            //      character in a display label, which then makes the SAX
            //      parser go south when trying to read the resulting file.
            for (char c : value.toCharArray()) {
                if (c < ' ' && LEGAL_LOW_CHARACTERS.indexOf(c) < 0) {
                    throw new IllegalStateException(String.format(
                      "Unexpected character (%d decimal) encountered when writing file", 
                      (int)c));
                }
            }
            // SAX parser incorrectly assumes a ']' at the end of our string value
            // introduces end-of-CDATA; thus, always append '@' to prevent the bug.
            sink.write(indent
                                + "<" + STR_ELT
                                + addVariableAttribute(variable)
                                + addAttribute(SIZE_ATTR,
                                               Integer.toString(value.length()))
                                + "><![CDATA["
                                + value + "@"
                                + "]]></" + STR_ELT + ">\n");
        }
    }

    /**
     * Method to save a primitive <code>char</code> value that is an
     * instance variable of an object.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the primitive value to serialize
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public void saveChar(char value, String variable)
        throws java.io.IOException
    {
        // SAX parser incorrectly assumes a character value of ']'
        // introduces end-of-CDATA; thus, always append '@' to prevent the bug.
        sink.write(indent
                            + "<" + CHAR_ELT
                            + addVariableAttribute(variable)
                            + "><![CDATA["
                            + Character.toString(value) + "@"
                            + "]]></" + CHAR_ELT + ">\n");
    }

    /**
     * Method to serialize and save a previously saved object as a reference.
     * Each object is assigned a unique id and registered in
     * <code>savedObjects</code>.  If <code>saveObject</code> sees a previously
     * saved object, a reference is generated instead of saving it out again.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param id the generated unique identifier for the object
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected void saveReference(Integer id, String variable)
        throws java.io.IOException
    {
        sink.write(indent + "<" + REF_ELT
                                    + addVariableAttribute(variable)
                                    + addAttribute(IDREF_ATTR, id.toString())
                                    + "/>\n");
    }

    /**
     * Method to serialize and save an enumeration value.  The serialization
     * of an enumeration value consists solely of its "persistence" enum code.
     * It is the responsibility of the loader that deserializes this value
     * to return the unique (singleton) enumeration value based on that code.
     * To support evolution of enumeration values, the enumeration class'
     * name and serialization version is part of the value's serialization.
     * <p>
     * NOTE: We currently assume that the enumeration class subclasses
     * <code>Enumerated</code>.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     * <p>
     * Note that enumeration values do not get assigned unique id's because
     * they are effectively unique (like singleton) values.
     *
     * @param className the name of the enumeration class
     * @param saver the saver registered for the given class
     * @param value the enumeration value to serialize; as noted, we expect the
     *              class of this value to subclass <code>Enumerated</code>
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected void saveEnum(String className,
                            IDataSaver<?> saver,
                            Object value,
                            String variable)
        throws java.io.IOException
    {
        String version = Integer.toString(saver.getVersion());

        String enumCode = ((Enumerated) value).persistenceValue();

        sink.write(indent + "<" + ENUM_ELT
                                    + addVariableAttribute(variable)
                                    + addAttribute(VALUE_ATTR, enumCode)
                                    + addAttribute(CLASS_ATTR, className)
                                    + addAttribute(VERSION_ATTR, version)
                                    + "/>\n");
    }

    /**
     * This method saves the given object that must be an instance of an array
     * class.  Each member of the array is recursively serialized/saved.
     * To support evolution of array types, the array elements' base class
     * name is part of the array value's serialization.  Since array objects
     * can be shared, a unique id should be generated for this value so that
     * it will be serialized only once.
     * <p>
     * Arrays of type <code>byte[]</code> are stored specially using
     * byte64 encoding.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param valueClass the name of the base class for the array's elements
     * @param value the array object itself
     * @param id the generated unique identifier for the array object
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, <code>Map</code>,
     *                 or <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected void saveArray(Class<?> valueClass,
                             Object value,
                             Integer id,
                             String variable)
        throws java.io.IOException
    {
        Class<?> eltType = valueClass.getComponentType();

        // Check if the array is byte[]
        if (eltType == Byte.TYPE) {
            String encoded = Base64.encode((byte[]) value);
            sink.write(indent + "<" + BYTES_ELT
                                        + addVariableAttribute(variable)
                                        + addAttribute(ID_ATTR, id.toString())
                                        + addAttribute(SIZE_ATTR,
                                                       Integer.toString(encoded.length()))
                                        + ">");
            sink.write(encoded);
            sink.write("</" + BYTES_ELT + ">\n");
        }
        else {
            int count = Array.getLength(value);

            sink.write(indent + "<" + ARRAY_ELT
                                        + addVariableAttribute(variable)
                                        + addAttribute(ID_ATTR, id.toString())
                                        + addAttribute(CLASS_ATTR,
                                                       eltType.getName())
                                        + addAttribute(SIZE_ATTR,
                                                       Integer.toString(count))
                                        + ">\n");

            String oldIndent = indent;
            indent += " ";

            // Serialize element values recursively as efficiently as possible.
            if (eltType.isPrimitive()) {
                if (eltType == Integer.TYPE) {
                    for (int i = 0; i < count; i++) {
                        saveInt(Array.getInt(value, i));
                    }
                }
                else if (eltType == Long.TYPE) {
                    for (int i = 0; i < count; i++) {
                        saveLong(Array.getLong(value, i));
                    }
                }
                else if (eltType == Double.TYPE) {
                    for (int i = 0; i < count; i++) {
                        saveDouble(Array.getDouble(value, i));
                    }
                }
                else if (eltType == Boolean.TYPE) {
                    for (int i = 0; i < count; i++) {
                        saveBoolean(Array.getBoolean(value, i));
                    }
                }
                else if (eltType == Character.TYPE) {
                    for (int i = 0; i < count; i++) {
                        saveChar(Array.getChar(value, i));
                    }
                }
            }
            else {
                // Element values are String or other objects;
                // recursively serialize.
                for (int i = 0; i < count; i++) {
                    saveObject(Array.get(value, i));
                }
            }

            indent = oldIndent;
            sink.write(indent + "</" + ARRAY_ELT + ">\n");
        }
    } // saveArray

    /**
     * This method saves the given object that must be an instance of
     * <code>Map</code>.  Each key and value component of the <code>Map</code>
     * is recursively serialized/saved.  Since <code>Map</code> objects
     * can be shared, a unique id should be generated for this value so that
     * it will be serialized only once.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param mapping the <code>Map</code> object itself
     * @param id the generated unique identifier for the array object
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, the value
     *                 component of a <code>Map</code> entry,
     *                 or an entry in a <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected <K, V> void saveMap(Map<K, V> mapping,
                                  Integer id,
                                  String variable)
        throws java.io.IOException
    {
        int count = mapping.size();

        sink.write(indent + "<" + MAP_ELT
                                    + addVariableAttribute(variable)
                                    + addAttribute(ID_ATTR, id.toString())
                                    + addAttribute(SIZE_ATTR,
                                                   Integer.toString(count))
                                    + ">\n");

        String oldIndent = indent;
        indent += " ";

        // Enumerate each key-value pair and recursively save the key and
        // value objects.  In this case, if the class of either is
        // (effectively) String or primitive, saveObject will take care of it.
        Iterator<Map.Entry<K, V>> pairs = mapping.entrySet().iterator();

        while (pairs.hasNext()) {
            Map.Entry<K, V> entry = pairs.next();
            sink.write(indent + "<" + KEY_ELT + ">\n");
            String currentIndent = indent;
            indent += " ";
            saveObject(entry.getKey());
            indent = currentIndent;
            sink.write(indent + "</" + KEY_ELT + ">\n");
            saveObject(entry.getValue());
        }

        indent = oldIndent;
        sink.write(indent + "</" + MAP_ELT + ">\n");
    } // saveMap

    /**
     * This method saves the given object that must be an instance of
     * <code>Collection</code>.  Each element of the <code>Collection</code>
     * is recursively serialized/saved.  Since <code>Collection</code> objects
     * can be shared, a unique id should be generated for this value so that
     * it will be serialized only once.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param elts the <code>Collection</code> object itself
     * @param id the generated unique identifier for the array object
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, the value
     *                 component of a <code>Map</code> entry,
     *                 or an entry in a <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected void saveCollection(Collection<?> elts,
                                  Integer id,
                                  String variable)
        throws java.io.IOException
    {
        int count = elts.size();

        sink.write(indent + "<" + COLLECTION_ELT
                                    + addVariableAttribute(variable)
                                    + addAttribute(ID_ATTR, id.toString())
                                    + addAttribute(SIZE_ATTR,
                                                   Integer.toString(count))
                                    + ">\n");

        String oldIndent = indent;
        indent += " ";

        // Enumerate each element of the Collection and save recursively;
        // if the class of either is (effectively) String or primitive,
        // saveObject will take care of it.
        Iterator<?> eltIt = elts.iterator();

        while (eltIt.hasNext()) {
            saveObject(eltIt.next());
        }

        indent = oldIndent;
        sink.write(indent + "</" + COLLECTION_ELT + ">\n");
    }

    /**
     * This method saves the given object.  If the object is <code>null</null>,
     * then the appropriate XML element is generated.  Otherwise, a unique
     * id is generated for the object; if one had already been assigned, then
     * the appropriate XML element for an object reference is generated.
     * If the object represents a primitive or String value, then the
     * corresponding XML is generated.  If the object if a member of an
     * enumerated type, then save that way.
     * <p>
     * The XML format will be as described in ObjectPersist.java.
     *
     * @param value the object to be serialized/saved
     * @param variable the name of the instance variable for this value in the
     *                 containing object; may be <code>null</code> to indicate
     *                 that value is an element in an array, the value
     *                 component of a <code>Map</code> entry,
     *                 or an entry in a <code>Collection</code>
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @throws UnexpectedException if no saver has been registered for the
     *         value's class
     * @author mlh
     */
    @SuppressWarnings("unchecked")
    public <T> void saveObject(T value, String variable)
        throws java.io.IOException
    {
        // If null, generate the null XML element
        if (value == null) {
            sink.write(indent
                                + "<" + NULL_ELT
                                + addVariableAttribute(variable)
                                + "/>\n");
        }
        else {
            Integer id = savedObjects.get(value);

            // If previously saved, generate a reference element
            if (id != null) {
                saveReference(id, variable);
            }
            else {
                Class<?> valueClass = value.getClass();

                // If a primitive or String value, generate the corresponding
                // element.
                if (valueClass == Integer.class) {
                    saveInt(((Integer) value).intValue(), variable);
                }
                else if (valueClass == Long.class) {
                    saveLong(((Long) value).longValue(), variable);
                }
                else if (valueClass == Double.class) {
                    saveDouble(((Double) value).doubleValue(), variable);
                }
                else if (valueClass == Boolean.class) {
                    saveBoolean(((Boolean) value).booleanValue(), variable);
                }
                else if (valueClass == String.class) {
                    saveString((String) value, variable);
                }
                else if (valueClass == Character.class) {
                    saveChar(((Character) value).charValue(), variable);
                }
                else {
                    String className = valueClass.getName();

                    IDataSaver<T> saver =
                        (IDataSaver<T>) saverRegistry.getSaver(className);

                    // Check whether the value represents an enumerated value
                    if ((saver != null) && saver.isEnum()) {
                        saveEnum(className, saver, value, variable);
                    }
                    else {
                        // Assign a unique id to this value and register
                        id = new Integer(idGen++);

                        savedObjects.put(value, id);

                        // If an aggregate (array, Map, or Collection),
                        // save appropriately
                        if (valueClass.isArray()) {
                            saveArray(valueClass, value, id, variable);
                        }
                        else if (Map.class.isAssignableFrom(valueClass)) {
                            saveMap((Map<?, ?>) value, id, variable);
                        }
                        else if (Collection.class.isAssignableFrom(valueClass))
                        {
                            saveCollection((Collection<?>) value, id, variable);
                        }
                        else {
                            // If no saver has been registered, complain
                            if (saver == null) {
                                throw new IllegalStateException("no saver found for: " + className);
                            }

                            // Save object header, with current serialization
                            // format version.
                            String version =
                                Integer.toString(saver.getVersion());

                            sink.write(indent
                                               + "<" + OBJ_ELT
                                               + addVariableAttribute(variable)
                                               + addAttribute(ID_ATTR,
                                                              id.toString())
                                               + addAttribute(CLASS_ATTR,
                                                              className)
                                               + addAttribute(VERSION_ATTR,
                                                              version)
                                               + ">\n");

                            // Save any super class data that has registered
                            // savers.
                            String oldIndent = indent;
                            indent += " ";
                            saveAsSuper(value, valueClass);

                            // Save the data for this object corresponding
                            // only to this class.
                            saver.saveData(value, this);
                            indent = oldIndent;

                            // Close the XML element
                            sink.write(indent
                                               + "</" + OBJ_ELT + ">\n");
                        }
                    }
                }
            }
        }
    } // saveObject

    /**
     * Allow subclass to decide whether or not to record the given object
     * to be serialized when all other objects have been saved.
     * <p>
     * Subclass should then override finish() to serialize the recorded
     * objects.
     * <p>
     * This is useful for clipboard operations where not all of the child
     * objects of a parent object should be serialized, and certainly not
     * at the same point as the serialization of the parent object.
     *
     * @param value the object to be serialized/saved
     * @return the filtered version of the given object value, if desired
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    public Object filterObject(Object value)
        throws java.io.IOException
    {
        // If desired, subclass should override.
        return value;
    }

    /**
     * This method saves the data associated with the object's class'
     * superclasses, as long as a saver has been registered for them.  Once
     * a superclass is encountered that has not registered saver, no further
     * attempts are made, even if some ancestor superclass has registered a
     * saver.
     * <p>
     * This method is recursive so that the data associated with more remote
     * ancestor classes are saved before that associated with closer
     * superclasses.
     *
     * @param obj the object being saved
     * @param subclass the class whose superclass for which we are trying to
     *                 save data
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    @SuppressWarnings("unchecked")
    protected <T> void saveAsSuper(T obj, Class<?> subclass)
        throws java.io.IOException
    {
        // There is no superclass of Object!
        if (subclass != Object.class) {
            Class<?> superclass = subclass.getSuperclass();

            String superclassName = superclass.getName();

            // Try to find a saver for the superclass
            IDataSaver<T> saver =
                (IDataSaver<T>) saverRegistry.getSaver(superclassName);

            // Stop saving here if superclass doesn't require "persistence".
            if (saver != null) {
                // Otherwise, recursively try up the "tree".
                saveAsSuper(obj, superclass);

                // Regardless, save the data for this object that is associated
                // with this superclass.
                saveSuper(obj, superclassName, saver);
            }
        }
    }

    /**
     * This method saves the data of the given object value that is associated
     * with the specified superclass (<code>className</code>) of the value's
     * object class.  The given saver is the one registered for that
     * superclass.
     *
     * @param value the object being saved
     * @param className the name of the superclass whose data is being saved
     * @param saver the saver object that controls how to serialize a value
     *              of the given type
     * @throws java.io.IOException if the sink generates one during a call
     *         to <code>write</code>
     * @author mlh
     */
    protected <T> void saveSuper(T value,
                                 String className,
                                 IDataSaver<T> saver)
        throws java.io.IOException
    {
        sink.write(indent
                           + "<" + SUPER_ELT
                           + addAttribute(CLASS_ATTR, className)
                           + addAttribute(VERSION_ATTR,
                                          Integer.toString(saver.getVersion()))
                           + ">\n");

        String oldIndent = indent;
        indent += " ";
        saver.saveData(value, this);
        indent = oldIndent;

        sink.write(indent + "</" + SUPER_ELT + ">\n");
    }
}
