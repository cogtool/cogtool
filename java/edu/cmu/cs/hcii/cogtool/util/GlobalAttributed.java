/*******************************************************************************
 * CogTool Copyright Notice and Distribution Terms
 * CogTool 1.2, Copyright (c) 2005-2012 Carnegie Mellon University
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
 * Eclipse SWT
 * Eclipse GEF Draw2D
 * 
 * Unless otherwise indicated, all Content made available by the Eclipse 
 * Foundation is provided to you under the terms and conditions of the Eclipse 
 * Public License Version 1.0 ("EPL"). A copy of the EPL is provided with this 
 * Content and is also available at http://www.eclipse.org/legal/epl-v10.html.
 * 
 * CLISP
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
 * The J2SE(TM) Java Runtime Environment
 * 
 * Copyright 2009 Sun Microsystems, Inc., 4150
 * Network Circle, Santa Clara, California 95054, U.S.A.  All
 * rights reserved. U.S.  
 * See the LICENSE file in the jre folder for more information.
 ******************************************************************************/

package edu.cmu.cs.hcii.cogtool.util;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class GlobalAttributed extends Alerter implements IAttributed
{
    public static final int edu_cmu_cs_hcii_cogtool_model_GlobalAttributed_version = 0;

    protected static final String authorityVAR = "authority";
    protected static final String attributePREFIX = "__";

    private static ObjectSaver.IDataSaver<GlobalAttributed> SAVER =
        new ObjectSaver.ADataSaver<GlobalAttributed>() {
            @Override
            public int getVersion()
            {
                return edu_cmu_cs_hcii_cogtool_model_GlobalAttributed_version;
            }

            @Override
            public void saveData(GlobalAttributed v, ObjectSaver saver)
                throws IOException
            {
                saver.saveObject(saver.filterObject(v.authority), authorityVAR);

                if (v.attrValues != null) {
                    Iterator<Map.Entry<AttributeDefinition<?>, Object>> allAttrs =
                        v.attrValues.entrySet().iterator();

                    while (allAttrs.hasNext()) {
                        Map.Entry<AttributeDefinition<?>, Object> attrValue =
                            allAttrs.next();
                        AttributeDefinition<?> defn = attrValue.getKey();

                        // TODO: Currently, all attribute values must be able
                        // to be persisted; ObjectSaver throws an
                        // IllegalStateException if no IDataSaver has been
                        // registered for the class of the attribute value.
                        saver.saveObject(saver.filterObject(attrValue.getValue()),
                                         attributePREFIX + defn.attrName);
                    }
                }
            }
        };

    public static void registerSaver()
    {
        ObjectSaver.registerSaver(GlobalAttributed.class.getName(), SAVER);
    }

    protected static class GlobalAttributedLoader
                           extends ObjectLoader.AObjectLoader<GlobalAttributed>
    {
        protected final int PREFIX_LENGTH = attributePREFIX.length();

        @Override
        public GlobalAttributed createObject()
        {
            return new GlobalAttributed();
        }

        /**
         * Returns the AttributeDefinition corresponding to the saved
         * "variable" name, which is the name of an attribute definition
         * with a prefix used to ensure persistent value uniqueness.
         *
         * @param prefixedAttribute the saved "variable" name
         * @return the attribute definition corresponding to the given name
         */
        protected AttributeDefinition<?> getAttrDefn(String prefixedAttribute)
        {
            // First remove the prefix, then get the registered definition
            String attribute = prefixedAttribute.substring(PREFIX_LENGTH);
            AttributeDefinition<?> defn = getAttributeDefn(attribute);

            if (defn == null) {
                throw new IllegalStateException("Undefined attribute type: "
                                                   + attribute);
            }

            return defn;
        }

        protected void setAttributeValue(GlobalAttributed v,
                                         AttributeDefinition<?> defn,
                                         Object value)
        {
            // Ensure that the GlobalAttributed is ready to accept
            // an attribute value.
            if (v.attrValues == null) {
                v.attrValues = new HashMap<AttributeDefinition<?>, Object>();
            }

            v.attrValues.put(defn, value);
        }

        protected void setAttribute(GlobalAttributed v,
                                    String prefixedAttribute,
                                    Object value)
        {
            setAttributeValue(v, getAttrDefn(prefixedAttribute), value);
        }

        protected Object createAggregateAttribute(GlobalAttributed v,
                                                  String prefixedAttribute,
                                                  int size)
        {
            AttributeDefinition<?> defn = getAttrDefn(prefixedAttribute);
            Object aggregate = defn.createAggregate(size);

            setAttributeValue(v, defn, aggregate);

            return aggregate;
        }

        @Override
        public void set(GlobalAttributed target, String variable, Object value)
        {
            if (variable != null) {
                if (variable.equals(authorityVAR)) {
                    GlobalAttributed newVal = (GlobalAttributed) value;
                    if (newVal == target) {
                        throw new IllegalStateException(
                            "Cannot set authority to self (" +
                            target +
                            ")");
                    }
                    target.authority = (GlobalAttributed) value;
                }
                else if (variable.startsWith(attributePREFIX)) {
                    setAttribute(target, variable, value);
                }
            }
        }

        @Override
        public void set(GlobalAttributed target, String variable, int value)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    setAttribute(target,
                                 variable,
                                 new Integer(value));
                }
            }
        }

        @Override
        public void set(GlobalAttributed target, String variable, double value)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    setAttribute(target,
                                 variable,
                                 new Double(value));
                }
            }
        }

        @Override
        public void set(GlobalAttributed target, String variable, boolean value)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    setAttribute(target,
                                 variable,
                                 Boolean.valueOf(value));
                }
            }
        }

        @Override
        public void set(GlobalAttributed target, String variable, char value)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    setAttribute(target,
                                 variable,
                                 new Character(value));
                }
            }
        }

        @Override
        public Map<?, ?> createMap(GlobalAttributed target,
                                   String variable,
                                   int size)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    return (Map<?, ?>) createAggregateAttribute(target,
                                                                variable,
                                                                size);
                }
            }

            return super.createMap(target, variable, size);
        }

        @Override
        public Collection<?> createCollection(GlobalAttributed target,
                                              String variable,
                                              int size)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    return (Collection<?>) createAggregateAttribute(target,
                                                                    variable,
                                                                    size);
                }
            }

            return super.createCollection(target, variable, size);
        }

        @Override
        public Class<?> getArrayEltType(String variable, String className)
        {
            if (variable != null) {
                if (variable.startsWith(attributePREFIX)) {
                    AttributeDefinition<?> defn = getAttrDefn(variable);

                    return defn.getArrayEltType();
                }
            }

            return super.getArrayEltType(variable, className);
        }
    }
    private static ObjectLoader.IObjectLoader<GlobalAttributed> LOADER =
        new GlobalAttributedLoader();

    public static void registerLoader()
    {
        ObjectLoader.registerLoader(GlobalAttributed.class.getName(),
                                    edu_cmu_cs_hcii_cogtool_model_GlobalAttributed_version,
                                    LOADER);
    }

    protected static AttributeDefinition<?> getAttributeDefn(String attr)
    {
        return AttributeRegistry.ONLY.getAttributeDefn(attr);
    }

    protected IAttributed authority = null;

    // Maps AttributeDefinition to Object value
    protected Map<AttributeDefinition<?>, Object> attrValues = null;

    public IAttributed getAuthority()
    {
        return authority;
    }

    public void setAuthority(IAttributed auth)
    {
        if (auth == this) {
            throw new IllegalArgumentException(
                "Cannot set authority to self (" +
                this +
                ")");
        }

        IAttributed prevAuthority = authority;

        authority = auth;

        raiseAlert(new AuthorityChange(this, prevAuthority));
    }

    protected IAttributed getDefiner(AttributeDefinition<?> defn)
    {
        if ((attrValues != null) && attrValues.containsKey(defn)) {
            return this;
        }

        if (authority != null) {
            if (authority instanceof GlobalAttributed) {
                if (authority == this) {
                    throw new IllegalStateException(
                        "Authority should never be self (" +
                        this +
                        ")");
                }
                return ((GlobalAttributed) authority).getDefiner(defn);
            }

            return authority.getDefiner(defn.attrName);
        }

        return null;
    }

    public IAttributed getDefiner(String attr)
    {
        AttributeDefinition<?> defn = getAttributeDefn(attr);

        if (defn == null) {
            throw new IllegalStateException("Undefined attribute type: " + attr);
        }

        return getDefiner(defn);
    }

    protected Object getAttribute(AttributeDefinition<?> defn)
    {
        if ((attrValues != null) && attrValues.containsKey(defn)) {
            return attrValues.get(defn);
        }

        if (authority != null) {
            if (authority instanceof GlobalAttributed) {
                if (authority == this) {
                    throw new IllegalStateException(
                        "Authority should never be self (" + this + ")");
                }
                return ((GlobalAttributed) authority).getAttribute(defn);
            }

            return authority.getAttribute(defn.attrName);
        }

        return defn.defaultValue;
    }

    public Object getAttribute(String attr)
    {
        AttributeDefinition<?> defn = getAttributeDefn(attr);

        if (defn == null) {
            throw new IllegalStateException("Undefined attribute type: " + attr);
        }

        return getAttribute(defn);
    }

    public boolean isAttributeDefault(String attr)
    {
        AttributeDefinition<?> defn = getAttributeDefn(attr);

        if (defn == null) {
            throw new IllegalStateException("Undefined attribute type: " + attr);
        }

        Object value = getAttribute(defn);

        if ((value == null) && (defn.defaultValue == null)) {
            return true;
        }

        // At this point, we know that value is not null
        return value.equals(defn.defaultValue);
    }

    public void setAttribute(String attr, Object value)
    {
        AttributeDefinition<?> defn = getAttributeDefn(attr);

        if (defn == null) {
            throw new IllegalStateException("Undefined attribute type: " + attr);
        }

        if (value != null) {
            if (! defn.attrType.isAssignableFrom(value.getClass())) {
                throw new IllegalArgumentException("Attribute value is of the wrong type");
            }
        }

        if (attrValues == null) {
            attrValues = new HashMap<AttributeDefinition<?>, Object>();
        }

        attrValues.put(defn, value);

        raiseAlert(new AttributeChange(this, attr, true));
    }

    public void unsetAttribute(String attr)
    {
        AttributeDefinition<?> defn = getAttributeDefn(attr);

        if (defn == null) {
            throw new IllegalStateException("Undefined attribute type: " + attr);
        }

        if (attrValues != null) {
            attrValues.remove(defn);

            raiseAlert(new AttributeChange(this, attr, false));
        }
    }

    public Iterator<AttributeDefinition<?>> getLocalAttributeNames()
    {
        if (attrValues == null) {
            return new EmptyIterator<AttributeDefinition<?>>();
        }

        return attrValues.keySet().iterator();
    }

    /**
     * Copy the locally defined attributes from the argument to the receiver.
     */
    public void copyAttributes(IAttributed fromAttributed)
    {
        if (fromAttributed != null) {
            Iterator<AttributeDefinition<?>> attrDefs =
                fromAttributed.getLocalAttributeNames();

            while (attrDefs.hasNext()) {
                String attr = attrDefs.next().attrName;

                setAttribute(attr, fromAttributed.getAttribute(attr));
            }
        }
    }
}
