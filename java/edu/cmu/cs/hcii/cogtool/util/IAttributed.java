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
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Provides a mechanism for Attribute-Value behavior for properties on an
 * object, i.e. properties that can be explicitly set to a particular value or
 * set to use a "default value" as defined by an external authority.
 *
 * @see edu.cmu.cs.hcii.cogtool.util.Attributed
 */
public interface IAttributed extends IAlerter
{
    /**
     * Alert raised when setAuthority is invoked
     */
    public static class AuthorityChange extends EventObject
    {
        /**
         * The previous value of the authority
         */
        public IAttributed oldAuthority;

        public AuthorityChange(IAttributed attributed,
                               IAttributed prevAuthority)
        {
            super(attributed);

            oldAuthority = prevAuthority;
        }
    }

    /**
     * Alert raised when setAttribute or unsetAttribute is invoked
     */
    public static class AttributeChange extends EventObject
    {
        /**
         * Set to true if setAttribute; false if unsetAttribute
         */
        public boolean isSet;

        /**
         * Which attribute was set/unset
         */
        public String attribute;

        public AttributeChange(IAttributed attributed,
                               String attributeName,
                               boolean setting)
        {
            super(attributed);

            attribute = attributeName;
            isSet = setting;
        }
    }

    /**
     * Object to hold an attribute's definition (should be protected,
     * but Java won't allow it).
     */
    public static class AttributeDefinition<T>
    {
        public final String attrName;
        public final Class<T> attrType;
        public final T defaultValue;

        public AttributeDefinition(String name, Class<T> type, T value)
        {
            if ((name == null) || (name.equals(""))) {
                throw new IllegalArgumentException("Attribute name may not be null or empty");
            }

            if (type == null) {
                throw new IllegalArgumentException("Attribute type may not be null");
            }

            this.attrName = name;
            this.attrType = type;
            this.defaultValue = value;
        }

        /**
         * Default implementation for fetching the component type
         * when the attribute value is an array.  Used during the loading
         * of attribute values when using ObjectSaver/ObjectLoader
         * persistence support.
         *
         * Subclasses may override to support type evolution.
         */
        protected Class<?> getArrayEltType()
        {
            return this.attrType.getComponentType();
        }

        /**
         * Default implementation for fetching the aggregate value to
         * hold the component values when loading attribute values using
         * ObjectSaver/ObjectLoader persistence support.
         *
         * If the attribute value is not an array, the registered subclass
         * instance must override this method to create and return the
         * appropriate aggregate value (e.g., ArrayList(), HashMap(), etc.)
         */
        public Object createAggregate(int count)
        {
            if (this.attrType.isArray()) {
                return Array.newInstance(getArrayEltType(), count);
            }

            throw new IllegalStateException("Unknown aggregate type");
        }
    }

    /**
     * Global registry to hold attribute definitions
     */
    public static class AttributeRegistry
    {
        public static AttributeRegistry ONLY = new AttributeRegistry();

        // Maps name to AttributeDefinition
        protected Map<String, AttributeDefinition<?>> attributeDefns =
            new HashMap<String, AttributeDefinition<?>>();

        protected AttributeRegistry() { }

        public AttributeDefinition<?> getAttributeDefn(String attr)
        {
            return attributeDefns.get(attr);
        }

        /**
         * Defines a new attribute and installs a default value.  Call only
         * once for each attribute type.
         *
         * @param attr a string identifier for the attribute to act upon
         * @param type the value type allowed for this attribute
         * @param value the default value for this attribute (could be null)
         * @throws IllegalStateException if this attribute is already defined
         * @throws IllegalArgumentException if attr == null, attr is empty,
         *                                  or type == null
         */
        public <T> void defineAttribute(String attr,
                                        Class<T> type,
                                        T defaultValue)
        {
            AttributeDefinition<?> defn = getAttributeDefn(attr);

            if (defn != null) {
                throw new IllegalStateException("Cannot redefine attribute type: "
                                                    + attr);
            }

            attributeDefns.put(attr,
                                    new AttributeDefinition<T>(attr,
                                                               type,
                                                               defaultValue));
        }

        /**
         * Method to register an attribute definition; generally,
         * defineAttribute should be used; however, to support non-array
         * aggregate attribute values (or array values that must "evolve"),
         * registration must use an instance of an AttributeDefinition subclass.
         *
         * @param defn the instance of an AttributeDefinition subclass
         * @throws IllegalStateException if this attribute is already defined
         */
        public void registerAttribute(AttributeDefinition<?> defn)
        {
            AttributeDefinition<?> existingDefn =
                getAttributeDefn(defn.attrName);

            if (existingDefn != null) {
                throw new IllegalStateException("Cannot redefine attribute type: "
                                                    + defn.attrName);
            }

            attributeDefns.put(defn.attrName, defn);
        }

        /**
         * Determine whether an attribute of the given name has been registered
         * (that is, subsequent calls to getAttributeDefaultValue and
         * getAttributeType will succeed.
         */
        public boolean isDefined(String attr)
        {
            return attributeDefns.containsKey(attr);
        }

        /**
         * Accessor for the stored value type of a named attribute.
         *
         * @param attr a string identifier for the attribute to act upon
         * @return the Class of the value type for attr
         * @throws IllegalStateException if the attribute is not defined
         */
        public Class<?> getAttributeType(String attr)
        {
            AttributeDefinition<?> defn = getAttributeDefn(attr);

            if (defn == null) {
                throw new IllegalStateException("Undefined attribute type: "
                                                    + attr);
            }

            return defn.attrType;
        }

        /**
         * Accessor for the stored default value of a named attribute.
         *
         * @param attr a string identifier for the attribute to act upon
         * @return the default value for attr
         * @throws IllegalStateException if the attribute is not defined
         */
        public Object getAttributeDefaultValue(String attr)
        {
            AttributeDefinition<?> defn = getAttributeDefn(attr);

            if (defn == null) {
                throw new IllegalStateException("Undefined attribute type: "
                                                    + attr);
            }

            return defn.defaultValue;
        }

        /**
         * Fetch the requested attribute value from the given IAttributed
         * but, if it is <code>null</code>, then return the attribute's
         * default value.
         */
        public Object getAttribute(IAttributed attributed, String attrName)
        {
            if (attributed != null) {
                return attributed.getAttribute(attrName);
            }

            return getAttributeDefaultValue(attrName);
        }
    }

    /**
     * Accesses the authority for the actual value of undefined attributes,
     * i.e. the "parent" in the attribute-definition hierarchy.  If this is an
     * attribute hierarchy root, null is returned.
     *
     * @return the source of actual values of "default value" attributes
     */
    public IAttributed getAuthority();

    /**
     * Mutates the authority for actual values of attributes not set locally.
     * Ordinarily, this is set only once in the lifetime of an
     * IAttributed, but it is possible to dynamically change the attribute-
     * definition hierarchy at runtime.  Note that changing the authority implies
     * a possible change of value for all attributes currently using attributes
     * undefined locally, which must be broadcast to all appropriate listeners.
     *
     * If the authority hierarchy does not provide a value, getAttribute will
     * return the default value associated with the attribute.
     *
     * Raises an AuthorityChange alert.
     *
     * @param auth a source of actual values for "default value" attributes
     */
    public void setAuthority(IAttributed auth);

    /**
     * Checks who in the authority hierarchy defines the given attribute.
     * A return value of <code>null</code> indicates that a getAttribute
     * would return the attribute type's default value.  If the return value
     * is equal to the receiver (i.e., <code>this</code>), then the value
     * is defined "locally".
     *
     * @param attr a string identifier for the attribute to act upon
     * @return the object in the authority hierarchy of the method receiver
     *         that defines a value for the given attribute; null if none
     * @throws IllegalStateException if the attribute is not defined
     */
    public IAttributed getDefiner(String attr);

    /**
     * Accessor for the value of the named attribute.  This method always
     * returns an "actual" value, which may be null.
     *
     * Note that the value returned here is by reference.  Clients should code
     * defensively to avoid unintended consequences.
     *
     * @param attr a string identifier for the attribute to act upon
     * @return an actual stored value for the named attribute, which may be null
     * @throws IllegalStateException if the attribute is not defined
     */
    public Object getAttribute(String attr);

    /**
     * Determine if the value of the given attribute for the receiver
     * is the default value.
     *
     * @param attr a string identifier for the attribute
     * @return true if and only if the value returned is the same as
     *         the default value associated with the attribute
     * @throws IllegalStateException if the attribute is not defined
     */
    public boolean isAttributeDefault(String attr);

    /**
     * Mutator for the value of the named attribute.
     * If the provided value does not match the value type defined for this
     * attribute via the defineAttribute() method, an IllegalArgumentException
     * will be thrown.
     *
     * Note that value is stored by reference.  Clients should code defensively
     * to avoid unintended consequences. Also note that the semantics of change
     * notification for collections may vary between implementations due to
     * performance concerns.
     *
     * Raises an AttributeChange alert.
     *
     * @param attr a string identifier for the attribute to act upon
     * @param value the new value for the named attribute
     * @throws IllegalStateException if the attribute is not defined
     * @throws IllegalArgumentException iff the value type defined via
     *         defineAttribute(attr, ...) does not match the type of value
     */
    public void setAttribute(String attr, Object value);

    /**
     * Because <code>null</code> is a valid attribute value, this method
     * should be used to remove an attribute from being defined locally.
     *
     * Raises an AttributeChange alert.
     *
     * @param attr a string identifier for the attribute to act upon
     * @throws IllegalStateException if the attribute is not defined
     */
    public void unsetAttribute(String attr);

    /**
     * Enumerate the names of the attributes defined locally for this object.
     */
    public Iterator<AttributeDefinition<?>> getLocalAttributeNames();

    /**
     * Copy the locally defined attributes from the argument to the receiver.
     */
    public void copyAttributes(IAttributed fromAttributed);
}
