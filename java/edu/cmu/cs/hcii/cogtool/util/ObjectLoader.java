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

import java.lang.reflect.Array;
import java.rmi.UnexpectedException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.eclipse.ecf.core.util.Base64;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Class to support the deserialization of a previously saved set
 * of Java objects.
 * <p>
 * Every object must register a loader (see, for example,
 * <code>CogToolSerialization</code>).
 * <p>
 * Loaders come in three "flavors":
 * (1) A standard object loader assigns values to the instance variables
 * of the object being loaded and creates the aggregate objects (array,
 * <code>Map</code>, or <code>Collection</code> into which nested objects
 * are placed.  (2) An aggregate loader simply creates nested aggregate
 * objects.  (3) An enumeration loader returns the unique (singleton)
 * enumerated value for a given persisted enumeration code.
 * <p>
 * Creating array objects is actually performed by reflection;
 * standard and aggregate loaders simply indicate the proper element type
 * for the array (which may have changed since the objects were originally
 * serialized).
 * <p>
 * Standard and aggregate loaders also provide for determining the appropriate
 * loader(s) for nested aggregate objects.
 * <p>
 * Because several objects may be stored in a serialization (see
 * <code>ObjectSaver</code>), the <code>load</code> method returns an ordered
 * <code>Collection</code> of the saved objects.
 * <p>
 * The SAX parser handler methods are not really exported!
 * (In other words, omit <code>startDocument</code>, <code>endDocument</code>,
 * <code>startElement</code>, <code>endElement</code>, <code>characters</code>,
 * <code>error</code>, <code>fatalError</code>, and <code>warning</code>.
 *
 * @author mlh
 */
public class ObjectLoader extends DefaultHandler implements ObjectPersist
{
    /**
     * The interface for an enumeration loader.
     * An enumeration loader returns the unique (singleton)
     * enumerated value for a given persisted enumeration code.
     *
     * @author mlh
     */
    public interface IEnumLoader
    {
        /**
         * Return the unique enumerated value for the given enumeration code.
         * <p>
         * Instances of this interface are only fetched from the loader
         * registry and never pushed on the loader stack (activeLoaders).
         *
         * @param persistentValue the serialized enumeration code
         * @return the unique enumerated value for the given enumeration code
         * @author mlh
         */
        public Object createEnum(String persistentValue);
        public Object createEnum(ObjectLoader l, String persistentValue);
    }

    /**
     * A common super-interface for <code>IAggregateLoader</code>
     * and <code>IObjectLoader</code>; this allows us to say that loader stack
     * has related type elements.
     *
     * @author mlh
     */
    public interface ILoader<T>
    {
        /**
         * Evolution support; override this to achieve changes that
         * require the entire object to be complete.
         *
         * @param target the object to evolve; guaranteed to be the same
         *               object created by createObject()
         */
        public void evolve(T target);
        public void evolve(ObjectLoader l, T target);
    }

    /**
     * The interface for an aggregate loader.
     * An aggregate loader creates nested aggregate objects.
     * It also provides the appropriate loader for nested aggregate objects
     * (for a <code>Map</code>, this loader would be for a nested value
     * component, since we assume that key components are not aggregate).
     * Finally, methods are provided to add objects to aggregates.
     * <p>
     * Aggregate loaders not registered (what type would you use to fetch it?).
     * Instead, they are "generated" via calls to <code>getLoader</code> on
     * either an <code>IObjectLoader</code> or <code>IAggregateLoader</code>
     * instance.  Aggregate loaders are pushed onto the loader stack
     * (<code>activeLoaders</code>) while an aggregate object is being
     * reconstituted.
     *
     * @author mlh
     */
    public interface IAggregateLoader extends ILoader<Object>
    {
        /**
         * Ensure a <code>Map</code> exists as a nested object in the
         * parent aggregate (that is, the parent array, <code>Map</code>,
         * or <code>Collection</code>).
         * <p>
         * The given size may be used to optimize the created
         * <code>Map</code>.
         *
         * @param size the number of key-value entries in the serialized
         *             <code>Map</code>
         * @return the nested <code>Map</code> value
         * @author mlh
         */
        public Map<?, ?> createMap(int size);
        public Map<?, ?> createMap(ObjectLoader l, int size);

        /**
         * Ensure a <code>Collection</code> exists as a nested object in the
         * parent aggregate (that is, the parent array, <code>Map</code>,
         * or <code>Collection</code>).
         * <p>
         * The given size may be used to optimize the created
         * <code>Collection</code>.
         *
         * @param size the number of entries in the serialized
         *             <code>Collection</code>
         * @return the nested <code>Collection</code> value
         * @author mlh
         */
        public Collection<?> createCollection(int size);
        public Collection<?> createCollection(ObjectLoader l, int size);

        /**
         * Determine the actual class to use as the element type
         * for the array to be created using reflection.  The given
         * class name is that of the element type of the array when
         * it was serialized.
         * <p>
         * The array being created is a nested object in a parent
         * aggregate (that is, the parent array, <code>Map</code>,
         * or <code>Collection</code>).
         *
         * @param className the class name of the element type of the
         *                  array when it was serialized
         * @return the class to use as the element type of the array being
         *         created
         * @author mlh
         */
        public Class<?> getArrayEltType(String className);
        public Class<?> getArrayEltType(ObjectLoader l, String className);

        /**
         * Return the aggregate loader for aggregate objects that are nested
         * within the aggregate object associated with this loader.  If this
         * aggregate is a <code>Map</code>, the returned loader would be for
         * the value component, since we assume that key components are not
         * aggregate.
         *
         * @return the aggregate loader for nested aggregate objects
         * @author mlh
         */
        public IAggregateLoader getLoader();
        public IAggregateLoader getLoader(ObjectLoader l);

        /**
         * Add key-value entry to given Map.
         *
         * @param l   the object loader controlling the deserialization
         * @param m   the map to add entry mapping to
         * @param key the key of the entry
         * @param v   the value of the entry
         * @author mlh
         */
        public <K, V> void putInMap(Map<K, V> m, K key, V v);
        public <K, V> void putInMap(ObjectLoader l, Map<K, V> m, K key, V v);

        /**
         * Add value to given Collection.
         *
         * @param l   the object loader controlling the deserialization
         * @param c   the collection to add value to
         * @param v   the value to add
         * @author mlh
         */
        public <T> void addToCollection(Collection<? super T> c, T v);
        public <T> void addToCollection(ObjectLoader l,
                                        Collection<? super T> c,
                                        T v);

        /**
         * Add Object value to given Array.
         *
         * @param l   the object loader controlling the deserialization
         * @param a   the array to add value to
         * @param i   index at which to add value
         * @param v   the value to add
         * @author mlh
         */
        public void addToArray(Object a, int i, Object v);
        public void addToArray(ObjectLoader l, Object a, int i, Object v);
    }

    /**
     * The interface for an object loader, which assigns values to the instance
     * variables of the object being loaded and creates the aggregate objects
     * (array, <code>Map</code>, or <code>Collection</code> into which nested
     * objects are placed.
     * It also provides the appropriate loader for nested aggregate objects.
     * <p>
     * For nested aggregate objects, the loader must override the appropriate
     * <code>createMap</code>, <code>createCollection</code>, and
     * <code>getArrayEltType</code>.  If the object can receive the aggregate
     * (instead of always creating the aggregate itself), then an appropriate
     * clause in the <code>set</code> method that takes an <code>Object</code>
     * value must also exist.  Remember that the first occurrence of any
     * object (including arrays, maps, and collections) is serialized in place
     * whereas subsequent references to the same object generate a
     * "reference" XML element and are "set".
     * <p>
     * Object loaders are fetched from the loader registry and pushed onto
     * the loader stack (<code>activeLoaders</code>) while an object of the
     * corresponding type is being reconstituted.
     *
     * @author mlh
     */
    public interface IObjectLoader<T> extends ILoader<T>
    {
        /**
         * Return the actual (empty) object instance of the class associated
         * with this loader.
         *
         * @return an empty object instance of the associated class
         * @author mlh
         */
        public T createObject();
        public T createObject(ObjectLoader l);

        /**
         * Assign the given <code>int</code> to the specified instance variable
         * of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>int</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, int value);
        public void set(T target, String variable, int value);

        /**
         * Assign the given <code>long</code> to the specified instance variable
         * of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>long</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, long value);
        public void set(T target, String variable, long value);

        /**
         * Assign the given <code>double</code> to the specified instance
         * variable of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>double</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, double value);
        public void set(T target, String variable, double value);

        /**
         * Assign the given <code>boolean</code> to the specified instance
         * variable of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>boolean</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, boolean value);
        public void set(T target, String variable, boolean value);

        /**
         * Assign the given <code>char</code> to the specified instance
         * variable of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>char</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, char value);
        public void set(T target, String variable, char value);

        /**
         * Assign the given <code>Object</code> to the specified instance
         * variable of the given target object which is of the associated class
         * (created by <code>createObject</code>).
         * <p>
         * NOTE: This is how a <code>String</code> value will be set.
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param value the <code>Object</code> value to assign
         * @author mlh
         */
        public void set(ObjectLoader l,
                        T target, String variable, Object value);
        public void set(T target, String variable, Object value);

        /**
         * Create the <code>Map</code> and assign it to the specified instance
         * variable for the given target object associated with the loader's
         * class (that is, created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param size the number of key-value entries in the serialized
         *             <code>Map</code>
         * @author mlh
         */
        public Map<?, ?> createMap(ObjectLoader l,
                                   T target, String variable, int size);
        public Map<?, ?> createMap(T target, String variable, int size);

        /**
         * Create the <code>Collection</code> and assign it to the specified
         * instance variable for the given target object associated with the
         * loader's class (that is, created by <code>createObject</code>).
         *
         * @param target the object to assign into
         * @param variable the instance variable to receive the value
         * @param size the number of key-value entries in the serialized
         *             <code>Collection</code>
         * @author mlh
         */
        public Collection<?> createCollection(ObjectLoader l,
                                              T target,
                                              String variable,
                                              int size);
        public Collection<?> createCollection(T target,
                                              String variable,
                                              int size);

        /**
         * Determine the actual class to use as the element type
         * for the array for the specified instance variable
         * to be created using reflection.  The given
         * class name is that of the element type of the array when
         * it was serialized.
         *
         * @param variable the instance variable to receive the value
         * @param className the class name of the element type of the
         *                  array when it was serialized
         * @return the class to use as the element type of the array being
         *         created
         * @author mlh
         */
        public Class<?> getArrayEltType(ObjectLoader l,
                                        String variable,
                                        String className);
        public Class<?> getArrayEltType(String variable, String className);

        /**
         * Return the aggregate loader for aggregate objects that are assigned
         * to the instance variable of objects associated with this loader.
         *
         * @param variable the instance variable to receive the value
         * @return the aggregate loader for nested aggregate objects
         * @author mlh
         */
        public IAggregateLoader getLoader(ObjectLoader l, String variable);
        public IAggregateLoader getLoader(String variable);
    }

    public static class ALoader<T> implements ILoader<T>
    {
        public void evolve(ObjectLoader l, T target)
        {
            evolve(target);
        }

        public void evolve(T target)
        {
            // Nothing to do, in general
        }
    }

    /**
     * This class represents a default implementation of an nested
     * aggregate loader.
     *
     * @author mlh
     */
    public static class AAggregateLoader extends ALoader<Object>
                                         implements IAggregateLoader
    {
        public static final AAggregateLoader ONLY = new AAggregateLoader();

        public Map<?, ?> createMap(ObjectLoader l, int size)
        {
            // Override this only if the ObjectLoader is really needed
            return createMap(size);
        }
        public Map<?, ?> createMap(int size)
        {
            throw new UnsupportedOperationException("createMap");
        }

        public Collection<?> createCollection(ObjectLoader l, int size)
        {
            // Override this only if the ObjectLoader is really needed
            return createCollection(size);
        }
        public Collection<?> createCollection(int size)
        {
            throw new UnsupportedOperationException("createCollection");
        }

        public Class<?> getArrayEltType(ObjectLoader l, String className)
        {
            // Override this only if the ObjectLoader is really needed
            return getArrayEltType(className);
        }
        public Class<?> getArrayEltType(String className)
        {
            throw new UnsupportedOperationException("getArrayEltType");
        }

        public IAggregateLoader getLoader(ObjectLoader l)
        {
            // Override this only if the ObjectLoader is really needed
            return getLoader();
        }
        public IAggregateLoader getLoader()
        {
            return ONLY;
        }

        public <K, V> void putInMap(ObjectLoader l, Map<K, V> m, K key, V v)
        {
            // Override this only if the ObjectLoader is really needed
            putInMap(m, key, v);
        }
        public <K, V> void putInMap(Map<K, V> m, K key, V v)
        {
            m.put(key, v);
        }

        public <T> void addToCollection(ObjectLoader l,
                                        Collection<? super T> c,
                                        T v)
        {
            // Override this only if the ObjectLoader is really needed
            addToCollection(c, v);
        }
        public <T> void addToCollection(Collection<? super T> c, T v)
        {
            c.add(v);
        }

        public void addToArray(ObjectLoader l, Object a, int i, Object v)
        {
            // Override this only if the ObjectLoader is really needed
            addToArray(a, i, v);
        }
        public void addToArray(Object a, int i, Object v)
        {
            Array.set(a, i, v);
        }
    }

    /**
     * This represents a default implementation for an enumeration loader.
     */
    public static class AEnumLoader implements IEnumLoader
    {
        public Object createEnum(ObjectLoader l, String persistentValue)
        {
            // Override this only if the ObjectLoader is really needed
            return createEnum(persistentValue);
        }

        public Object createEnum(String persistentValue)
        {
            throw new UnsupportedOperationException("createEnum");
        }
    }
    /**
     * This represents a default implementation for an object loader
     * that assumes that none of the instance variables are aggregates.
     * <p>
     * If building a loader for an abstract superclass, one need not
     * overide <code>createObject</code>.
     *
     * @author mlh
     */
    public static class AObjectLoader<T> extends ALoader<T>
                                         implements IObjectLoader<T>
    {
        public T createObject(ObjectLoader l)
        {
            // Override this only if the ObjectLoader is really needed
            return createObject();
        }
        public T createObject()
        {
            throw new UnsupportedOperationException("createObject");
        }

        public void set(ObjectLoader l,
                        T target, String variable, int value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, int value)
        {
            throw new UnsupportedOperationException("set int");
        }

        public void set(ObjectLoader l,
                        T target, String variable, long value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, long value)
        {
            throw new UnsupportedOperationException("set long");
        }

        public void set(ObjectLoader l,
                        T target, String variable, double value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, double value)
        {
            throw new UnsupportedOperationException("set double");
        }

        public void set(ObjectLoader l,
                        T target, String variable, boolean value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, boolean value)
        {
            throw new UnsupportedOperationException("set boolean");
        }

        public void set(ObjectLoader l,
                        T target, String variable, char value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, char value)
        {
            throw new UnsupportedOperationException("set char");
        }

        public void set(ObjectLoader l,
                        T target, String variable, Object value)
        {
            // Override this only if the ObjectLoader is really needed
            set(target, variable, value);
        }
        public void set(T target, String variable, Object value)
        {
            throw new UnsupportedOperationException("set object");
        }

        public Map<?, ?> createMap(ObjectLoader l,
                                   T target, String variable, int size)
        {
            // Override this only if the ObjectLoader is really needed
            return createMap(target, variable, size);
        }
        public Map<?, ?> createMap(T target, String variable, int size)
        {
            throw new UnsupportedOperationException("createMap");
        }

        public Collection<?> createCollection(ObjectLoader l,
                                              T target,
                                              String variable,
                                              int size)
        {
            // Override this only if the ObjectLoader is really needed
            return createCollection(target, variable, size);
        }
        public Collection<?> createCollection(T target,
                                              String variable,
                                              int size)
        {
            throw new UnsupportedOperationException("createCollection");
        }

        public Class<?> getArrayEltType(ObjectLoader l,
                                     String variable,
                                     String className)
        {
            // Override this only if the ObjectLoader is really needed
            return getArrayEltType(variable, className);
        }
        public Class<?> getArrayEltType(String variable, String className)
        {
            throw new UnsupportedOperationException("getArrayEltType");
        }

        public IAggregateLoader getLoader(ObjectLoader l, String variable)
        {
            // Override this only if the ObjectLoader is really needed
            return getLoader(variable);
        }
        public IAggregateLoader getLoader(String variable)
        {
            return AAggregateLoader.ONLY;
        }
    }

    public interface ILoaderRegistry
    {
        /**
         * Every object must register a loader that specifies how to reconstruct
         * its value from a serialization of the given format version.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @param version the version of the format that was used to serialize
         *                the object
         * @param loader the loader object that controls how to restore a value
         *               of the given type serialized to the given format version
         * @author mlh
         */
        public <E> void registerLoader(String className,
                                       int version,
                                       IObjectLoader<E> loader);

        /**
         * Every object must register a loader that specifies how to reconstruct
         * its value from a serialization.  This method returns the loader
         * associated with the given class name for the given format version.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @param version the version of the format that was used to serialize
         *                the object
         * @return the loader object that controls how to restore a value
         *         of the given type serialized to the given format version,
         *         or <code>null</code> if none was registered
         * @author mlh
         */
        public <E> IObjectLoader<E> getLoader(String className, int version);

        /**
         * Every enumerated type must register a loader that specifies how to
         * reconstruct its value from a serialization of the given format version.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @param version the version of the format that was used to serialize
         *                the object
         * @param loader the loader object that controls how to restore a value
         *               of the given type serialized to the given format version
         * @author mlh
         */
        public void registerEnumLoader(String className,
                                       int version,
                                       IEnumLoader loader);

        /**
         * Every enumerated type must register a loader that specifies how to
         * reconstruct its value from a serialization.  This method returns the
         * loader associated with the given class name for the given format
         * version.
         *
         * @param className the name of the class; this should almost always be
         *                  the value of <code>C.class.getName()</code>
         * @param version the version of the format that was used to serialize
         *                the object
         * @return the loader object that controls how to restore a value
         *         of the given type serialized to the given format version,
         *         or <code>null</code> if none was registered
         * @author mlh
         */
        public IEnumLoader getEnumLoader(String className, int version);
    }

    /**
     * Default implementation, especially for the default registry.
     */
    public static class DefaultLoaderRegistry implements ILoaderRegistry
    {
        // registry of IObjectLoaders mapping
        // (class-name + '#' + version) to ILoader or IEnumLoader
        protected Map<String, Object> loaderRegistry =
            new HashMap<String, Object>();

        protected String buildKey(String className, int version)
        {
            return className + "#" + Integer.toString(version);
        }

        public <E> void registerLoader(String className,
                                       int version,
                                       IObjectLoader<E> loader)
        {
            loaderRegistry.put(buildKey(className, version), loader);
        }

    	@SuppressWarnings("unchecked")
        public <E> IObjectLoader<E> getLoader(String className, int version)
        {
            return (IObjectLoader<E>) loaderRegistry.get(buildKey(className,
                                                                       version));
        }

        public void registerEnumLoader(String className,
                                       int version,
                                       IEnumLoader loader)
        {
            loaderRegistry.put(buildKey(className, version), loader);
        }

        public IEnumLoader getEnumLoader(String className, int version)
        {
            return (IEnumLoader) loaderRegistry.get(buildKey(className,
                                                                  version));
        }
    }

    /**
     * Allows the caller to provide different behavior for a subset of
     * objects, maintaining "normal" behavior for other objects.
     */
    public static class OverrideLoaderRegistry extends DefaultLoaderRegistry
    {
        protected ILoaderRegistry overriddenRegistry;

        public OverrideLoaderRegistry(ILoaderRegistry overridden)
        {
            overriddenRegistry = overridden;
        }

        @Override
        public <E> IObjectLoader<E> getLoader(String className, int version)
        {
            IObjectLoader<E> loader = super.getLoader(className, version);

            if (loader != null) {
                return loader;
            }

            return overriddenRegistry.getLoader(className, version);
        }

        @Override
        public IEnumLoader getEnumLoader(String className, int version)
        {
            IEnumLoader loader = super.getEnumLoader(className, version);

            return (loader != null)
                       ? loader
                       : overriddenRegistry.getEnumLoader(className,
                                                               version);
        }
    }

    /**
     * Default registry for savers.
     */
    public static final ILoaderRegistry DEFAULT_REGISTRY =
        new DefaultLoaderRegistry();

    // Registry to use for the loading process
    protected ILoaderRegistry loaderRegistry;

    // Stack of active loaders; although all elements are of type ILoader,
    // the actual type of loader at the top of the stack is determined by
    //  the current objectState:
    //    if (objectState == IN_OBJECT), then (loader instanceof IObjectLoader)
    //    otherwise, (loader instanceof IAggregateLoader)
    protected Stack<ILoader<Object>> activeLoaders =
        new Stack<ILoader<Object>>();

    // Stack of pending Objects; the top element is the
    // current object being reconstituted
    protected Stack<Object> pendingObjects = new Stack<Object>();

    // maps Integer(id) to Object
    protected Map<Integer, Object> loadedObjects =
        new HashMap<Integer, Object>();

    // pending array indexes
    protected static final int ARRAY_STACK_DEPTH = 100;
    protected int[] arrayIndexes = new int[ARRAY_STACK_DEPTH];
    protected int arrayStackTop = -1;

    static SAXParserFactory parserFactory = SAXParserFactory.newInstance();

    // Assignment states for String and char
    protected static final int NORMAL = 0;
    protected static final int IN_STRING = 1;
    protected static final int IN_CHAR = 2;
    protected static final int IN_BYTES = 3;
    protected static final int ASSIGNED = -1;

    protected int assignmentState = NORMAL;

    // Save the id attribute for delayed construction objects (i.e., BYTE array)
    protected int idref;

    // Nesting states for object construction
    protected static final char NONE = ' ';
    protected static final char IN_OBJECT = 'o';
    protected static final char IN_ARRAY = 'a';
    protected static final char IN_COLLECTION = 'c';
    protected static final char IN_MAP = 'm';
    protected static final char IN_KEY = 'k';

    // Current state
    protected char objectState = NONE;

    // Acting as a stack of nested object states
    protected StringBuilder stateStack = new StringBuilder();

    // The key value of a mapping entry (assigned when IN_KEY)
    protected Object entryKey;

    // The name of the instance variable for later invocation of "set"
    // for STRING, BYTES, and CHAR
    protected String pendingVariable;

    // The number of (total) characters in the STRING or BYTES.
    // For STRING, this will be one more than the actual STRING value.
    protected int stringLength;

    // The "characters" method gets called multiple times for long
    // STRING or BYTES values (because reading is buffered).  The value
    // is accumulated using this StringBuilder.
    protected StringBuilder accumulator = new StringBuilder();

    /**
     * Constructor -- the created object may be used for multiple loads.
     *
     * @author mlh
     */
    public ObjectLoader()
    {
        this(null);
    }

    /**
     * Constructor -- the created object may be used for multiple loads.
     *
     * @param registry registry that holds ILoader and IEnumLoader instances
     *                 to support loading saved objects
     * @author mlh
     */
    public ObjectLoader(ILoaderRegistry registry)
    {
        loaderRegistry = (registry != null) ? registry : DEFAULT_REGISTRY;
    }

    /**
     * Every object must register a loader that specifies how to reconstruct
     * its value from a serialization of the given format version.
     *
     * @param className the name of the class; this should almost always be
     *                  the value of <code>C.class.getName()</code>
     * @param version the version of the format that was used to serialize
     *                the object
     * @param loader the loader object that controls how to restore a value
     *               of the given type serialized to the given format version
     * @author mlh
     */
    public static <T> void registerLoader(String className,
                                          int version,
                                          IObjectLoader<T> loader)
    {
        DEFAULT_REGISTRY.registerLoader(className, version, loader);
    }

    /**
     * Every enumerated type must register a loader that specifies how to
     * reconstruct its value from a serialization of the given format version.
     *
     * @param className the name of the class; this should almost always be
     *                  the value of <code>C.class.getName()</code>
     * @param version the version of the format that was used to serialize
     *                the object
     * @param loader the loader object that controls how to restore a value
     *               of the given type serialized to the given format version
     * @author mlh
     */
    public static void registerEnumLoader(String className,
                                          int version,
                                          IEnumLoader loader)
    {
        DEFAULT_REGISTRY.registerEnumLoader(className, version, loader);
    }

    /**
     * The object being assigned into an aggregate (Array, Collection, or Map)
     * may need to know which objects it is nested within (e.g., for setting
     * "parentage").  This method may be used to fetch those nested objects.
     * <p>
     * Indexes are zero-based; increasing indexes yield deeper nested
     * pending objects. In particular, to fetch the aggregate object itself,
     * use an index of zero.  An index of one, therefore, yields the object
     * that contains the aggregate.
     *
     * @param depthIndex "nested-ness" of the pending object desired
     * @return the object at the specified depth, or <code>null</code>
     *         if the index is greater than or equal to the number
     *         of pending objects
     */
    public Object getPendingObject(int depthIndex)
    {
        if (depthIndex < 0) {
            throw new IllegalArgumentException("Index cannot be negative.");
        }

        int lastValidStackIndex = pendingObjects.size() - 1;

        if (depthIndex > lastValidStackIndex) {
            return null;
        }

        return pendingObjects.get(lastValidStackIndex - depthIndex);
    }

    public <T> T getPendingObject(Class<T> ofType)
    {
        return getPendingObject(ofType, 0);
    }

    public <T> T getPendingObject(Class<T> ofType, int depthIndexOfType)
    {
        if (depthIndexOfType < 0) {
            throw new IllegalArgumentException("Index cannot be negative.");
        }

        for (int i = pendingObjects.size() - 1; i >= 0; i--) {
            Object pendingObject = pendingObjects.get(i);

            if (ofType.isInstance(pendingObject)) {
                if (depthIndexOfType == 0) {
                    return ofType.cast(pendingObject);
                }

                depthIndexOfType--;
            }
        }

        return null;
    }

    /**
     * Based on the serialization contained by the given input source
     * and the initial aggregate loader (that is, for top-level objects
     * that are themselves aggregates), reconstitute the top-level objects
     * and return them in an ordered collection.
     * <p>
     * The caller is responsible for recovering any resources associated
     * with the input source (such as <code>close()</code>).
     *
     * @param src the previously saved serialization of a sequence of objects
     * @param initialLoader the loader to use for creating top-level aggregate
     *                      objects; it may be <code>null</code> if no
     *                      top-level object is an aggregate
     * @throws ParserConfigurationException based on the SAX parsing
     * @throws SAXException based on the SAX parsing
     * @throws java.io.IOException if the source generates one
     *         during read calls generated by the SAX parser
     * @throws java.io.IOException
     */
	@SuppressWarnings("unchecked")
    public List<Object> load(InputSource src, IAggregateLoader initialLoader)
        throws ParserConfigurationException, SAXException, java.io.IOException
    {
        activeLoaders.push((initialLoader != null)
                                    ? initialLoader
                                    : AAggregateLoader.ONLY);

        // A List will keep the reconstituted objects in order
        pendingObjects.push(new ArrayList<Object>());
        pushObjectState(IN_COLLECTION);

        SAXParser p = parserFactory.newSAXParser();

        // The List is populated during the parse
        p.parse(src, this);

        return (List<Object>) pendingObjects.peek();
    }

    /**
     * There is a corresponding object state for each nested object
     * construction.  Also, IN_KEY is pushed when the key component
     * of a mapping entry is encountered.
     * <p>
     * The stack of nested object states is kept as a <code>StringBuilder</code>
     * so we don't have to create actual objects for the states.
     *
     * @param newObjectState the object state that should become current
     * @author mlh
     */
    protected void pushObjectState(char newObjectState)
    {
        stateStack.append(objectState);
        objectState = newObjectState;
    }

    /**
     * Pop the most recent pending nested object state from the stack
     * and make it the current state.
     *
     * @author mlh
     */
    protected void popObjectState()
    {
        int lastIndex = stateStack.length() - 1;
        objectState = stateStack.charAt(lastIndex);
        stateStack.deleteCharAt(lastIndex);
    }

    /**
     * When reconstructing a nested object, the current object's
     * reconstruction must be "paused".  The top of the stack of pending
     * objects is the current object being reconstructed.
     * <p>
     * Push the new object to be reconstructed, push the new state
     * reflecting the nature of the object (e.g., array, map, or normal),
     * push the loader that supports the reconstruction of the object,
     * and register the object in the <code>loadedObjects</code> registry
     * (so that nested references to this object are recognized).
     *
     * @param attrs the attributes used to fetch the "id" value
     *              for registering the object
     * @param value the new "current" object to be reconstructed
     * @param loader the loader object that controls how to restore the value
     * @param newState the object state reflecting the nature of the new object
     * @author mlh
     */
    protected void pushObject(Attributes attrs,
                              Object value,
                              ILoader<Object> loader,
                              char newState)
    {
        loadedObjects.put(new Integer(getIntAttribute(attrs, ID_ATTR)),
                               value);

        pendingObjects.push(value);
        activeLoaders.push(loader);
        pushObjectState(newState);
    }

    /**
     * Array elements are stored in index order; we need to keep track of the
     * current index for assigning nested elements into the proper index.
     * If we encounter an array of arrays, we need to "pause" the index
     * assignment of the outer array and start assigning from zero for
     * the nested array.
     *
     * @author mlh
     */
    protected void pushArrayIndex()
    {
        if (++arrayStackTop >= ARRAY_STACK_DEPTH) {
            arrayIndexes[arrayStackTop] = 0;
        }
        else {
            throw new IndexOutOfBoundsException("array nesting too deep");
        }
    }

    /**
     * When we are done with a nested array, we can pop the stack of
     * current index values so that the outer array (if one exists)
     * can resume assigning nested elements into the proper index.
     *
     * @author mlh
     */
    protected void popArrayIndex()
    {
        arrayStackTop--;
    }

    /**
     * Fetch the current index value for the current array for assigning
     * the next nested array element, then increment the current index
     * value for the next assignment (if any).
     *
     * @return the current index value for the current array being
     *         reconstructed
     * @author mlh
     */
    protected int nextArrayIndex()
    {
        return arrayIndexes[arrayStackTop]++;
    }

    // Debugging support
    protected void printAttrs(Attributes attrs)
    {
        int count = attrs.getLength();

        for (int i = 0; i < count; i++) {
            System.out.println("    " + attrs.getQName(i)
                               + ": " + attrs.getValue(i));
        }
    }

    /**
     * A shortcut for fetching an attribute that should be an <code>int</code>.
     * Note that a null access exception will be thrown if the requested
     * attribute is not in the given set.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be an <code>int</code>
     * @return the value of the requested attribute
     * @author mlh
     */
    protected int getIntAttribute(Attributes attrs, String key)
    {
        return Integer.parseInt(attrs.getValue(key));
    }

    /**
     * A shortcut for fetching an attribute that should be an <code>long</code>.
     * Note that a null access exception will be thrown if the requested
     * attribute is not in the given set.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be an <code>long</code>
     * @return the value of the requested attribute
     * @author mlh
     */
    protected long getLongAttribute(Attributes attrs, String key)
    {
        return Long.parseLong(attrs.getValue(key));
    }

    /**
     * A shortcut for fetching an attribute that should be a
     * <code>double</code>.
     * Note that a null access exception will be thrown if the requested
     * attribute is not in the given set.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be a
     *            <code>double</code>
     * @return the value of the requested attribute
     * @author mlh
     */
    protected double getDoubleAttribute(Attributes attrs, String key)
    {
        return Double.parseDouble(attrs.getValue(key));
    }

    /**
     * A shortcut for fetching an attribute that should be a
     * <code>boolean</code>.   Note that a null access exception will be
     * thrown if the requested attribute is not in the given set.
     * Note also that it expects the attribute value to be either
     * <code>BOOL_TRUE</code> or <code>BOOL_FALSE</code>.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be a
     *            <code>boolean</code>
     * @return the value of the requested attribute
     * @author mlh
     */
    protected boolean getBooleanAttribute(Attributes attrs, String key)
    {
        return attrs.getValue(key).equalsIgnoreCase(BOOL_TRUE);
    }

    /**
     * A shortcut for fetching an attribute that should be a
     * <code>char</code>.  (This is probably never used.)
     * Note that a null access exception will be thrown if the requested
     * attribute is not in the given set.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be a
     *            <code>char</code>
     * @return the value of the requested attribute
     * @author mlh
     */
    protected char getCharAttribute(Attributes attrs, String key)
    {
        return attrs.getValue(key).charAt(0);
    }

    /**
     * A shortcut for fetching an attribute that should be a
     * <code>String</code>.  Callers are responsible for checking for a
     * <code>null</code> return, if desired.
     *
     * @param attrs the set of attributes for the current XML element
     * @param key the attribute name whose value should be a
     *            <code>String</code>
     * @return the value of the requested attribute if it exists;
     *         <code>null</code> otherwise
     * @author mlh
     */
    protected String getStringAttribute(Attributes attrs, String key)
    {
        return attrs.getValue(key);
    }

    /**
     * A shortcut for fetching the value of the attribute representing an
     * instance variable name.
     *
     * @param attrs the set of attributes for the current XML element
     * @return the value of the instance variable name attribute
     * @throws UnexpectedException if the attribute is not in the given set
     * @author mlh
     */
    protected String getVariable(Attributes attrs)
    {
        String variable = getStringAttribute(attrs, VAR_ATTR);

        // If we called this, we fully expect a value to exist.
        if (variable == null) {
            throw new IllegalStateException("variable attribute not found");
        }

        return variable;
    }

    /**
     * See the three-parameter addObject.
     * This version gets the current loader from the top of the
     * activeLoaders stack.
     */
    protected void addObject(Attributes attrs, Object value)
    {
        addObject(activeLoaders.peek(), attrs, value);
    }

    /**
     * Add the given object (the next to be reconstituted) to the current
     * object being reconstituted.  The given object, if necessary, will
     * become the "new" current object being reconstituted (unless, for
     * example, the new object is an enumeration value).
     * <p>
     * The current object state determines the nature of the current
     * object being reconstituted.  If the current object state is
     * <code>IN_KEY</code>, we simply remember the key value for later
     * reconstruction of the key-value pair for the current mapping object.
     * This occurs when the value part is "added" in the <code>IN_MAP</code>
     * state.
     * <p>
     * The given attributes are used to determine to which instance variable
     * the new, given object belongs if the current object is a
     * non-aggregate object.
     *
     * @param loader the loader to control how to add the given value
     * @param attrs the attributes used to fetch the instance variable name
     * @param value the object just created to be added to the current object
     * @throws UnexpectedException if the current object state is unknown
     * @author mlh
     */
	@SuppressWarnings("unchecked")
    protected void addObject(ILoader<Object> loader,
                             Attributes attrs,
                             Object value)
    {
        switch (objectState) {
            case IN_OBJECT: {
                IObjectLoader<Object> objectLoader =
                    (IObjectLoader<Object>) loader;

                // Assigning to an instance variable in a normal object
                objectLoader.set(this,
                                 pendingObjects.peek(),
                                 getVariable(attrs),
                                 value);

                break;
            }
            case IN_ARRAY: {
                IAggregateLoader aggLoader = (IAggregateLoader) loader;

                // Assigning to the next index in the current array object
                aggLoader.addToArray(this,
                                     pendingObjects.peek(),
                                     nextArrayIndex(),
                                     value);
                break;
            }
            case IN_COLLECTION: {
                IAggregateLoader aggLoader = (IAggregateLoader) loader;

                // Adding to the current collection object
                aggLoader.addToCollection(this,
                                          (Collection<Object>)
                                              pendingObjects.peek(),
                                          value);
                break;
            }
            case IN_KEY: {
                // Remember the key for the current key-value mapping entry
                entryKey = value;
                break;
            }
            case IN_MAP: {
                IAggregateLoader aggLoader = (IAggregateLoader) loader;

                // Use the remembered key and the given value to build
                // the next key-value entry in the current mapping object
                aggLoader.putInMap(this,
                                   (Map<Object, Object>)
                                       pendingObjects.peek(),
                                   entryKey,
                                   value);
                break;
            }
            default: {
                throw new IllegalStateException("Unknown state");
            }
        }
    } // addObject

    @Override
    public void startDocument()
        throws SAXException
    {
//        System.out.println("startDocument");
    }

    @Override
	@SuppressWarnings("unchecked")
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes attrs)
        throws SAXException
    {
        // use qName
//        System.out.println("startElement-- qName: " + qName);
//        printAttrs(attrs);

        if (qName.equalsIgnoreCase(KEY_ELT)) {
            // Only need to change the object state to "remember" the key
            // of the next key-value mapping entry.  We could (I suppose)
            // ensure that we are currently in the IN_MAP state.
            pushObjectState(IN_KEY);
        }
        else if (qName.equalsIgnoreCase(ARRAY_ELT)) {
            Object value;
            IAggregateLoader aggregateLoader;

            // Regardless of the current state, we will need the number
            // of array elements and the base element class.
            int eltCount = getIntAttribute(attrs, SIZE_ATTR);
            String eltClass = getStringAttribute(attrs, CLASS_ATTR);

            // If we are currently reconstituting a normal object, we
            // need to assign the created array to the appropriate instance
            // variable.  We know that the loader is an IObjectLoader.
            if (objectState == IN_OBJECT) {
                IObjectLoader<Object> loader =
                    (IObjectLoader<Object>) activeLoaders.peek();

                String variable = getVariable(attrs);

                // Create the array using the element type returned by
                // the loader for this variable.
                value = Array.newInstance(loader.getArrayEltType(this,
                                                                 variable,
                                                                 eltClass),
                                          eltCount);

                // Currently, we insist that an array is actually created.
                if (value == null) {
                    throw new IllegalStateException("array create was not successful");
                }

                // Assign the array to the appropriate instance variable
                loader.set(this, pendingObjects.peek(), variable, value);

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are nested within this array.
                aggregateLoader = loader.getLoader(this, variable);
            }
            else {
                // The current object being reconstituted is a mapping,
                // collection, or itself an array.  Thus, we know the loader
                // is an IAggregateLoader.
                IAggregateLoader loader =
                    (IAggregateLoader) activeLoaders.peek();

                // Create the array using the element type returned by
                // the loader.
                value =
                    Array.newInstance(loader.getArrayEltType(this, eltClass),
                                      eltCount);

                // Currently, we insist that an array is actually created.
                if (value == null) {
                    throw new IllegalStateException("array create was not successful");
                }

                // Add the object to the current object as appropriate
                // (mapping, collection, or array).
                // This could be used instead of the "set" call in the
                // IN_OBJECT case above, but we already had the right loader.
                addObject(loader, attrs, value);

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are nested within this array.
                aggregateLoader = loader.getLoader(this);
            }

            // "Pause" any outer array index tracking and create an new
            // index counter for this array starting at zero.
            pushArrayIndex();

            // Push state, object, and loader, and register object
            pushObject(attrs, value, aggregateLoader, IN_ARRAY);
        }
        else if (qName.equalsIgnoreCase(COLLECTION_ELT)) {
            Collection<?> value;
            IAggregateLoader aggregateLoader;

            // Although most collection construction doesn't care, we
            // pass the element count in case some optimization is possible.
            int eltCount = getIntAttribute(attrs, SIZE_ATTR);

            // If we are currently reconstituting a normal object, we
            // need to create and assign the created collection to the
            // appropriate instance variable.  We know that the loader
            // is an IObjectLoader.
            if (objectState == IN_OBJECT) {
                IObjectLoader<Object> loader =
                    (IObjectLoader<Object>) activeLoaders.peek();

                String variable = getVariable(attrs);

                // Create and assign the collection based on the specified
                // instance variable using the loader.
                value = loader.createCollection(this,
                                                pendingObjects.peek(),
                                                variable,
                                                eltCount);

                // Currently, we insist that a collection is actually created.
                if (value == null) {
                    throw new IllegalStateException("collection create was not successful");
                }

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are nested within this collection.
                aggregateLoader = loader.getLoader(this, variable);
            }
            else {
                // The current object being reconstituted is a mapping,
                // array, or itself a collection.  Thus, we know the loader
                // is an IAggregateLoader.
                IAggregateLoader loader =
                    (IAggregateLoader) activeLoaders.peek();

                // Create the nested collection.
                value = loader.createCollection(this, eltCount);

                // Currently, we insist that a collection is actually created.
                if (value == null) {
                    throw new IllegalStateException("collection create was not successful");
                }

                // Add the object to the current object as appropriate
                // (mapping, collection, or array).  Note that, since we needed
                // to create the collection in the IN_OBJECT case above,
                // we *cannot* use this call in that state.
                addObject(loader, attrs, value);

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are nested within this collection.
                aggregateLoader = loader.getLoader(this);
            }

            // Push state, object, and loader, and register object
            pushObject(attrs, value, aggregateLoader, IN_COLLECTION);
        }
        else if (qName.equalsIgnoreCase(MAP_ELT)) {
            Map<?, ?> value;
            IAggregateLoader aggregateLoader;

            // Although most mapping construction doesn't care, we
            // pass the element count in case some optimization is possible.
            int eltCount = getIntAttribute(attrs, SIZE_ATTR);

            // If we are currently reconstituting a normal object, we
            // need to create and assign the created mapping to the
            // appropriate instance variable.  We know that the loader
            // is an IObjectLoader.
            if (objectState == IN_OBJECT) {
                IObjectLoader<Object> loader =
                    (IObjectLoader<Object>) activeLoaders.peek();

                String variable = getVariable(attrs);

                // Create and assign the mapping based on the specified
                // instance variable using the loader.
                value = loader.createMap(this,
                                         pendingObjects.peek(),
                                         variable,
                                         eltCount);

                // Currently, we insist that a mapping is actually created.
                if (value == null) {
                    throw new IllegalStateException("map create was not successful for "
                                                    + variable);
                }

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are value components within
                // this mapping.
                aggregateLoader = loader.getLoader(this, variable);
            }
            else {
                // The current object being reconstituted is a collection,
                // array, or itself a mapping.  Thus, we know the loader
                // is an IAggregateLoader.
                IAggregateLoader loader =
                    (IAggregateLoader) activeLoaders.peek();

                // Create the nested mapping.
                value = loader.createMap(this, eltCount);

                // Currently, we insist that a mapping is actually created.
                if (value == null) {
                    throw new IllegalStateException("map create was not successful");
                }

                // Add the object to the current object as appropriate
                // (mapping, collection, or array).  Note that, since we needed
                // to create the mapping in the IN_OBJECT case above,
                // we *cannot* use this call in that state.
                addObject(loader, attrs, value);

                // Fetch the aggregate loader for controlling reconstruction
                // of aggregate objects that are value components within
                // this mapping.
                aggregateLoader = loader.getLoader(this);
            }

            // Push state, object, and loader, and register object
            pushObject(attrs, value, aggregateLoader, IN_MAP);
        }
        else if (qName.equalsIgnoreCase(OBJ_ELT)) {
            // Trying to reconstitute a non-aggregate, non-enumeration object;
            // fetch the registered loader.
            String className = getStringAttribute(attrs, CLASS_ATTR);
            int savedVersion = getIntAttribute(attrs, VERSION_ATTR);
            IObjectLoader<Object> loader =
                loaderRegistry.getLoader(className, savedVersion);

            // The loader is necessary.
            if (loader == null) {
                throw new IllegalStateException("Object class loader not "
                                                   + "found for class:"
                                                   + className
                                                   + " version "
                                                   + savedVersion);
            }

            // Ask the loader to create a new, empty object of the right type.
            Object newObj = loader.createObject(this);

            // Currently, we insist that the object is actually created.
            if (newObj == null) {
                throw new IllegalStateException("Object creation was not "
                                                   + "successful for class:"
                                                   + className
                                                   + " version "
                                                   + savedVersion);
            }

            // Add the object to the current object as appropriate (object,
            // mapping, collection, or array).
            addObject(attrs, newObj);

            // Push state, object, and loader, and register object
            pushObject(attrs, newObj, loader, IN_OBJECT);
        }
        else if (qName.equalsIgnoreCase(SUPER_ELT)) {
            // At this point, we're trying to reconstitute the data
            // associated with a superclass of the current object's type;
            // fetch the registered loader.
            String className = getStringAttribute(attrs, CLASS_ATTR);
            int savedVersion = getIntAttribute(attrs, VERSION_ATTR);
            IObjectLoader<Object> loader =
                loaderRegistry.getLoader(className, savedVersion);

            // The loader is necessary.
            if (loader == null) {
                throw new IllegalStateException("Super class loader not found "
                                                   + "for class:"
                                                   + className
                                                   + " version "
                                                   + savedVersion);
            }

            // The object doesn't change, nor the state (we don't need to know
            // that we're in a superclass part), but the loader that controls
            // the reconstitution does change -- push that.
            activeLoaders.push(loader);
        }
        else if (qName.equalsIgnoreCase(REF_ELT)) {
            // The object this reference represents has been saved earlier
            // in the serialization (it may also be in the process of being
            // reconstituted); it should have registered.
            int id = getIntAttribute(attrs, IDREF_ATTR);
            Object refObject = loadedObjects.get(new Integer(id));

            // If not found, complain!
            if (refObject == null) {
                throw new IllegalStateException("ref object with id="
                                                        + id + " not found");
            }

            // Add the object to the current object as appropriate (object,
            // mapping, collection, or array).
            addObject(attrs, refObject);
        }
        else if (qName.equalsIgnoreCase(NULL_ELT)) {
            // Add null to the current object as appropriate (object,
            // mapping, collection, or array).
            addObject(attrs, null);
        }
        else if (qName.equalsIgnoreCase(INT_ELT)) {
            // The int value is kept directly as an attribute
            int value = getIntAttribute(attrs, VALUE_ATTR);

            // Based on the current object's nature, assign the value.
            switch (objectState) {
                case IN_OBJECT: {
                    IObjectLoader<Object> loader =
                        (IObjectLoader<Object>) activeLoaders.peek();

                    // Assign to the appropriate instance variable
                    loader.set(this,
                               pendingObjects.peek(),
                               getVariable(attrs),
                               value);

                    break;
                }
                case IN_ARRAY: {
                    // Assign to the next index in the current array
                    Array.setInt(pendingObjects.peek(),
                                 nextArrayIndex(),
                                 value);
                    break;
                }
                case IN_COLLECTION: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Add the value as an object to the current collection
                    loader.addToCollection(this,
                                           (Collection<Object>)
                                               pendingObjects.peek(),
                                           new Integer(value));
                    break;
                }
                case IN_KEY: {
                    // Remember the value (as an object) as the key
                    // for the next key-value pair in the current mapping
                    entryKey = new Integer(value);
                    break;
                }
                case IN_MAP: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Assign the remembered key and this value (as an object)
                    // as the next key-value pair in the current mapping
                    loader.putInMap(this,
                                    (Map<Object, Object>)
                                        pendingObjects.peek(),
                                    entryKey,
                                    new Integer(value));
                    break;
                }
                default: {
                    throw new IllegalStateException("Unknown state");
                }
            }
        }
        else if (qName.equalsIgnoreCase(LONG_ELT)) {
            // The long value is kept directly as an attribute
            long value = getLongAttribute(attrs, VALUE_ATTR);

            // Based on the current object's nature, assign the value.
            switch (objectState) {
                case IN_OBJECT: {
                    IObjectLoader<Object> loader =
                        (IObjectLoader<Object>) activeLoaders.peek();

                    // Assign to the appropriate instance variable
                    loader.set(this,
                               pendingObjects.peek(),
                               getVariable(attrs),
                               value);

                    break;
                }
                case IN_ARRAY: {
                    // Assign to the next index in the current array
                    Array.setLong(pendingObjects.peek(),
                                  nextArrayIndex(),
                                  value);
                    break;
                }
                case IN_COLLECTION: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Add the value as an object to the current collection
                    loader.addToCollection(this,
                                           (Collection<Object>)
                                               pendingObjects.peek(),
                                           new Long(value));
                    break;
                }
                case IN_KEY: {
                    // Remember the value (as an object) as the key
                    // for the next key-value pair in the current mapping
                    entryKey = new Long(value);
                    break;
                }
                case IN_MAP: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Assign the remembered key and this value (as an object)
                    // as the next key-value pair in the current mapping
                    loader.putInMap(this,
                                    (Map<Object, Object>)
                                        pendingObjects.peek(),
                                    entryKey,
                                    new Long(value));
                    break;
                }
                default: {
                    throw new IllegalStateException("Unknown state");
                }
            }
        }
        else if (qName.equalsIgnoreCase(DOUBLE_ELT)) {
            // The double value is kept directly as an attribute
            double value = getDoubleAttribute(attrs, VALUE_ATTR);

            // Based on the current object's nature, assign the value.
            switch (objectState) {
                case IN_OBJECT: {
                    IObjectLoader<Object> loader =
                        (IObjectLoader<Object>) activeLoaders.peek();

                    // Assign to the appropriate instance variable
                    loader.set(this,
                               pendingObjects.peek(),
                               getVariable(attrs),
                               value);

                    break;
                }
                case IN_ARRAY: {
                    // Assign to the next index in the current array
                    Array.setDouble(pendingObjects.peek(),
                                    nextArrayIndex(),
                                    value);
                    break;
                }
                case IN_COLLECTION: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Add the value as an object to the current collection
                    loader.addToCollection(this,
                                           (Collection<Object>)
                                               pendingObjects.peek(),
                                           new Double(value));
                    break;
                }
                case IN_KEY: {
                    // Remember the value (as an object) as the key
                    // for the next key-value pair in the current mapping
                    entryKey = new Double(value);
                    break;
                }
                case IN_MAP: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Assign the remembered key and this value (as an object)
                    // as the next key-value pair in the current mapping
                    loader.putInMap(this,
                                    (Map<Object, Object>)
                                        pendingObjects.peek(),
                                    entryKey,
                                    new Double(value));
                    break;
                }
                default: {
                    throw new IllegalStateException("Unknown state");
                }
            }
        }
        else if (qName.equalsIgnoreCase(BOOL_ELT)) {
            // The boolean value is kept directly as an attribute
            boolean value = getBooleanAttribute(attrs, VALUE_ATTR);

            // Based on the current object's nature, assign the value.
            switch (objectState) {
                case IN_OBJECT: {
                    IObjectLoader<Object> loader =
                        (IObjectLoader<Object>) activeLoaders.peek();

                    // Assign to the appropriate instance variable
                    loader.set(this,
                               pendingObjects.peek(),
                               getVariable(attrs),
                               value);

                    break;
                }
                case IN_ARRAY: {
                    // Assign to the next index in the current array
                    Array.setBoolean(pendingObjects.peek(),
                                     nextArrayIndex(),
                                     value);
                    break;
                }
                case IN_COLLECTION: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Add the value as an object to the current collection
                    loader.addToCollection(this,
                                           (Collection<Object>)
                                               pendingObjects.peek(),
                                           new Boolean(value));
                    break;
                }
                case IN_KEY: {
                    // Remember the value (as an object) as the key
                    // for the next key-value pair in the current mapping
                    entryKey = new Boolean(value);
                    break;
                }
                case IN_MAP: {
                    IAggregateLoader loader =
                        (IAggregateLoader) activeLoaders.peek();

                    // Assign the remembered key and this value (as an object)
                    // as the next key-value pair in the current mapping
                    loader.putInMap(this,
                                    (Map<Object, Object>)
                                        pendingObjects.peek(),
                                    entryKey,
                                    new Boolean(value));
                    break;
                }
                default: {
                    throw new IllegalStateException("Unknown state");
                }
            }
        }
        else if (qName.equalsIgnoreCase(STR_ELT)) {
            // String values are not kept as an attribute value but rather as
            // text in a CDATA section between the XML start and end elements.
            // Thus, the value must be assigned once it has been accumulated.

            // If the current object is non-aggregate, then remember the
            // instance variable for this string.
            if (objectState == IN_OBJECT) {
                pendingVariable = getVariable(attrs);
            }

            // The SAX parser incorrectly assumes a ']' at the end of our
            // string value introduces end-of-CDATA; thus, ClassSaver
            // always appends '@' to prevent the bug; add one to the length!
            // Clear the accumulator for collecting string fragments that
            // arise because the parser apparently buffers its reads.
            stringLength = getIntAttribute(attrs, SIZE_ATTR) + 1;
            assignmentState = IN_STRING;
            accumulator.delete(0, accumulator.length());
        }
        else if (qName.equalsIgnoreCase(CHAR_ELT)) {
            // A character value is not kept as an attribute value since
            // it may not be a legal attribute value. Instead, it is placed
            // in a CDATA section between the XML start and end elements.
            // Thus, the value must be assigned once it has been accumulated.

            // If the current object is non-aggregate, then remember the
            // instance variable for this character value.
            if (objectState == IN_OBJECT) {
                pendingVariable = getVariable(attrs);
            }

            // The SAX parser incorrectly assumes a ']' as the
            // character value introduces end-of-CDATA; thus, ClassSaver
            // always appends '@' to prevent the bug!  We may need to
            // use the accumulator if buffering splits the two characters;
            // clear the accumulator!
            assignmentState = IN_CHAR;
            accumulator.delete(0, accumulator.length());
        }
        else if (qName.equalsIgnoreCase(BYTES_ELT)) {
            // An array of byte[] is treated differently from all other arrays.
            // Its value is stored as a base64 encoded string between the XML
            // start and end elements.
            // Thus, the value must be assigned once it has been accumulated.

            // If the current object is non-aggregate, then remember the
            // instance variable for this byte[] value.
            if (objectState == IN_OBJECT) {
                pendingVariable = getVariable(attrs);
            }

            // Get the number of base64 characters and clear the accumulator
            assignmentState = IN_BYTES;
            idref = getIntAttribute(attrs, ID_ATTR);

            stringLength = getIntAttribute(attrs, SIZE_ATTR);
            accumulator.delete(0, accumulator.length());
        }
        else if (qName.equalsIgnoreCase(ENUM_ELT)) {
            // At this point, we're trying to fetch the unique (singleton)
            // value for an enumerated type.  An IEnumLoader knows how to
            // translate the persisted enumeration code into the appropriate
            // value.
            IEnumLoader loader =
                loaderRegistry.getEnumLoader(getStringAttribute(attrs,
                                                                     CLASS_ATTR),
                                                  getIntAttribute(attrs,
                                                                  VERSION_ATTR));

            // The loader is necessary.
            if (loader == null) {
                throw new IllegalStateException("enum class loader not found");
            }

            // Ask the loader to fetch the appropriate enumeration value
            // for the persisted enumeration code.
            Object newEnum =
                loader.createEnum(this, getStringAttribute(attrs, VALUE_ATTR));

            // Currently, we insist that a non-null enumeration value
            // be fetched
            if (newEnum == null) {
                throw new IllegalStateException("enum create was not successful");
            }

            // Add the enumeration object to the current object as appropriate
            // (object, mapping, collection, or array).
            addObject(attrs, newEnum);
        }
    } // startElement

    // We use this method to reset state.
    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
        throws SAXException
    {
        // use qName
//        System.out.println("endElement-- qName: " + qName);

        if (qName.equalsIgnoreCase(ARRAY_ELT) ||
            qName.equalsIgnoreCase(COLLECTION_ELT) ||
            qName.equalsIgnoreCase(MAP_ELT) ||
            qName.equalsIgnoreCase(OBJ_ELT))
        {
            // If we were constructing an object, we can pop the stacks:
            //  - for loaders associated with objects being constructed,
            //  - for objects being constructed
            //  - for the natures of the objects being constructed
            //  - and, if the current object is an array, the current
            //    index for placing array elements.
            ILoader<Object> loader = activeLoaders.pop();
            Object obj = pendingObjects.pop();
            popObjectState();

            if (qName.equalsIgnoreCase(ARRAY_ELT)) {
                popArrayIndex();
            }

            // Make any type evolution changes to the object
            loader.evolve(this, obj);
        }
        else if (qName.equalsIgnoreCase(SUPER_ELT)) {
            // If we were dealing with reconstructing the data for one of the
            // current object's superclasses, only a loader was pushed.
            ILoader<Object> loader = activeLoaders.pop();
            Object obj = pendingObjects.peek();

            // Make any type evolution changes to the object
            loader.evolve(this, obj);
        }
        else if (qName.equalsIgnoreCase(KEY_ELT)) {
            // If we were dealing with the key value for the next entry
            // of a current map object, only the state (IN_KEY) was pushed.
            popObjectState();
        }
        else if (qName.equalsIgnoreCase(STR_ELT)) {
            // If we were dealing with a string, reset the assignment state.
            assignmentState = NORMAL;
        }
        else if (qName.equalsIgnoreCase(CHAR_ELT)) {
            // If we were dealing with a char, reset the assignment state.
            assignmentState = NORMAL;
        }
        else if (qName.equalsIgnoreCase(BYTES_ELT)) {
            // If we were dealing with an array of bytes,
            // reset the assignment state.
            assignmentState = NORMAL;
        }
    } // endElement

    @Override
	@SuppressWarnings("unchecked")
    public void characters(char[] str, int start, int length)
        throws SAXException
    {
//        System.out.println("characters-- str: "
//                              + String.valueOf(str).substring(start,
//                                                              start + length)
//                              + " state: " + this.assignmentState
//                              + " start: " + start
//                              + " length: " + length);
//        System.out.flush();

        // If we are in the ASSIGNED state, we presumably have seen all
        // of the characters needed for either STRING, BYTES, or CHAR.
        // Something is wrong!
        if (assignmentState == ASSIGNED) {
            throw new IllegalStateException("incorrect state for assigning char(s)");
        }

        // Assignment can only occur if we have seen all of the characters
        // expected for the given STRING, BYTES, or CHAR value.
        // Apparently, the SAX parser buffers input, so we may see
        // several calls to "characters" in a row with substrings.
        boolean okToAssign = true;

        if ((assignmentState == IN_STRING) ||
            (assignmentState == IN_BYTES))
        {
            Object value = null;    // stupid Java compiler

            // Accumulate the value
            accumulator.append(str, start, length);

            if (assignmentState == IN_STRING) {
                // SAX parser incorrectly assumes a ']' at the end of our
                // string value introduces end-of-CDATA; thus, ClassSaver
                // always appends '@' to prevent the bug; subtract one!
                if (accumulator.length() == stringLength) {
                    value =
                        accumulator.substring(0, stringLength - 1);
                }
                else {
                    // We haven't seen enough yet; wait for a subsequent call
                    okToAssign = false;
                }
            }
            else if (assignmentState == IN_BYTES) {
                // A byte[] array was stored using base64 encoding; decode!
                if (accumulator.length() == stringLength) {
                    value = Base64.decode(accumulator.toString());
                    loadedObjects.put(new Integer(idref), value);
                }
                else {
                    // We haven't seen enough yet; wait for a subsequent call
                    okToAssign = false;
                }
            }

            // If we have seen all of the characters, assign
            if (okToAssign) {
                if (objectState == IN_OBJECT) {
                    // Need the loader for the current object being constructed
                    IObjectLoader<Object> loader =
                        (IObjectLoader<Object>) activeLoaders.peek();

                    // We don't use the clause in addObject because we have
                    // squirreled away the variable to assign to (and don't
                    // have attributes from which to [potentially] get it!)
                    loader.set(this,
                               pendingObjects.peek(),
                               pendingVariable,
                               value);
                }
                else {
                    // None of the other states require attributes.
                    addObject(null, value);
                }

                // We should see no additional characters before the endElement
                assignmentState = ASSIGNED;
            }
        }
        else if (assignmentState == IN_CHAR) {
            // SAX parser incorrectly assumes a character value of ']'
            // introduces end-of-CDATA; thus, ClassSaver always appends '@'
            // to prevent the bug.  Check for length of 2, but use only first!

            char value = '\0';  // Java requires some value (ugh!)

            // Check if buffering has split between the two characters!
            if (length < 2) {
                accumulator.append(str, start, length);

                // If this is the second (i.e., the '@'), we can assign!
                if (accumulator.length() == 2) {
                    value = accumulator.charAt(0);
                }
                else {
                    // Sure enough, buffering has split the two; wait!
                    okToAssign = false;
                }
            }
            else if (length == 2) {
                // We got both characters, so we can assign!
                value = str[0];
            }
            else {
                throw new IllegalStateException("char length incorrect");
            }

            // If we have seen the character and the trailing '@'; assign the
            // value depending on the nature of the current object being
            // reconstituted.
            if (okToAssign) {
                switch (objectState) {
                    case IN_OBJECT: {
                        IObjectLoader<Object> loader =
                            (IObjectLoader<Object>) activeLoaders.peek();

                        // Assign to the appropriate (saved) instance variable
                        loader.set(this,
                                   pendingObjects.peek(),
                                   pendingVariable,
                                   value);

                        break;
                    }
                    case IN_ARRAY: {
                        // Assign to the next index in the current array
                        Array.setChar(pendingObjects.peek(),
                                      nextArrayIndex(),
                                      value);
                        break;
                    }
                    case IN_COLLECTION: {
                        IAggregateLoader loader =
                            (IAggregateLoader) activeLoaders.peek();

                        // Add the value as an object to the current collection
                        loader.addToCollection(this,
                                               (Collection<Object>)
                                                   pendingObjects.peek(),
                                               new Character(value));
                        break;
                    }
                    case IN_KEY: {
                        // Remember the value (as an object) as the key
                        // for the next key-value pair in the current mapping
                        entryKey = new Character(value);
                        break;
                    }
                    case IN_MAP: {
                        IAggregateLoader loader =
                            (IAggregateLoader) activeLoaders.peek();

                        // Assign the remembered key and this value (as an
                        // object) as the next key-value pair in the current
                        // mapping
                        loader.putInMap(this,
                                        (Map<Object, Object>)
                                            pendingObjects.peek(),
                                        entryKey,
                                        new Character(value));
                        break;
                    }
                    default: {
                        throw new IllegalStateException("Unknown state");
                    }
                }

                // We should see no additional characters before the endElement
                assignmentState = ASSIGNED;
            }
        }
        // else, we are in the NORMAL state and can ignore this call
        // (it should be all white space!)
    } // characters

    @Override
    public void endDocument()
        throws SAXException
    {
//        System.out.println("endDocument");
    }

    @Override
    public void error(SAXParseException e)
        throws SAXException
    {
        System.out.println("error");
    }

    @Override
    public void fatalError(SAXParseException e)
        throws SAXException
    {
        System.out.println("fatalError");
    }

    @Override
    public void warning(SAXParseException e)
        throws SAXException
    {
        System.out.println("warning");
    }
}
