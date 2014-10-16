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

/**
 * @author lhein
 */
public class EndpointPropertyModel {
    
    private String endpointProtocol;
    private List<EndpointProperty> properties;
    
    /**
     * @return the endpointProtocol
     */
    public String getEndpointProtocol() {
        return this.endpointProtocol;
    }
    
    /**
     * @param endpointProtocol the endpointProtocol to set
     */
    public void setEndpointProtocol(String endpointProtocol) {
        this.endpointProtocol = endpointProtocol;
    }
    
    /**
     * @return the properties
     */
    public List<EndpointProperty> getProperties() {
        return this.properties;
    }
    
    /**
     * @param properties the properties to set
     */
    public void setProperties(List<EndpointProperty> properties) {
        this.properties = properties;
    }
}
