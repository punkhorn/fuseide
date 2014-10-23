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

import java.util.List;

import org.fusesource.ide.camel.model.connectors.Connector;

/**
 * @author lhein
 */
public class EndpointPropertyModel {
    
    public static final String PROTOCOL_PROPERTY = "endpointprotocol";
    
    private List<EndpointProperty> properties;
    private Connector connector;
    private EndpointProperty protocolProperty;
    private String protocol;
    
    /**
     * 
     */
    public EndpointPropertyModel(Connector connector) {
        this.connector = connector;
    }
    
    public EndpointPropertyModel(String protocol) {
        this.protocol = protocol;
    }
    
    /**
     * @return the connector
     */
    public Connector getConnector() {
        return this.connector;
    }
    
    /**
     * @return the protocol
     */
    public String getProtocol() {
        return this.protocol;
    }
    
    /**
     * checks if there is a property with the given name
     * 
     * @param propertyName
     * @return
     */
    public boolean hasProperty(String propertyName) {
        for (EndpointProperty p : properties) {
            if (p.getName().equals(propertyName)) return true;
        }
        return false;
    }
    
    /**
     * @return the properties
     */
    public List<EndpointProperty> getProperties() {
        if (!hasProperty(PROTOCOL_PROPERTY)) {
            if (this.protocolProperty == null) {
                this.protocolProperty = new EndpointProperty(PROTOCOL_PROPERTY, CamelComponentUtils.buildChoice(getConnector(), getProtocol()), null, EndpointPropertyKind.BOTH);
            }
            properties.add(0, protocolProperty);
        }
        return this.properties;
    }
    
    /**
     * @param properties the properties to set
     */
    public void setProperties(List<EndpointProperty> properties) {
        this.properties = properties;
    }
}
