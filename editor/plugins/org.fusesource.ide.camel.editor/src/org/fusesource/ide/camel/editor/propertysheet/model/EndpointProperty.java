/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.fusesource.ide.camel.editor.propertysheet.model;

/**
 * @author lhein
 */
public class EndpointProperty {
    private String name;
    private String type;
    
    private String descriptionKey;
    private String label;
    private String description;
    private Object defaultValue;
    private EndpointPropertyKind kind;
    
    /**
     * 
     */
    public EndpointProperty(String name, String type, Object defaultValue, EndpointPropertyKind kind) {
        this.name = name;
        this.type = type;
        this.defaultValue = defaultValue;
        this.kind = kind;
    }
    
    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }
    
    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }
    
    /**
     * @return the type
     */
    public String getType() {
        return this.type;
    }
    
    /**
     * @param type the type to set
     */
    public void setType(String type) {
        this.type = type;
    }
    
    /**
     * @return the descriptionKey
     */
    public String getDescriptionKey() {
        return this.descriptionKey;
    }
    
    /**
     * @param descriptionKey the descriptionKey to set
     */
    public void setDescriptionKey(String descriptionKey) {
        this.descriptionKey = descriptionKey;
    }
    
    /**
     * @return the label
     */
    public String getLabel() {
        return this.label;
    }
    
    /**
     * @param label the label to set
     */
    public void setLabel(String label) {
        this.label = label;
    }
    
    /**
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }
    
    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }
    
    /**
     * @return the defaultValue
     */
    public Object getDefaultValue() {
        return this.defaultValue;
    }
    
    /**
     * @param defaultValue the defaultValue to set
     */
    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }
    
    /**
     * @return the kind
     */
    public EndpointPropertyKind getKind() {
        return this.kind;
    }

    /**
     * @param kind the kind to set
     */
    public void setKind(EndpointPropertyKind kind) {
        this.kind = kind;
    }
}
