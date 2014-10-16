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

import java.util.ArrayList;

/**
 * @author lhein
 */
public final class EndpointPropertiesUtils {

    private static final EndpointPropertyModel DUMMY_MODEL;
    
    static {
        ArrayList<EndpointProperty> propertiesList = new ArrayList<EndpointProperty>();
        propertiesList.add(new EndpointProperty("autoCreate", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("recursive",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("bufferSize", "int", 128000, EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("forceWrites", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.PRODUCER));
                
        DUMMY_MODEL = new EndpointPropertyModel();
        DUMMY_MODEL.setEndpointProtocol("file");
        DUMMY_MODEL.setProperties(propertiesList);
    }
    
    /**
     * returns the properties model for a given protocol
     * 
     * @param protocol  the protocol to get the properties for
     * @return  the properties model or null if not available
     */
    public static EndpointPropertyModel getPropertiesForEndpoint(String protocol) {
        if (protocol.equalsIgnoreCase(DUMMY_MODEL.getEndpointProtocol())) return DUMMY_MODEL;
        return null;
    }
    
    public static boolean isBooleanProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("boolean") || 
                p.getType().equalsIgnoreCase("java.lang.Boolean");
    }
    
    public static boolean isTextProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("String") || 
                p.getType().equalsIgnoreCase("java.lang.String") || 
                p.getType().equalsIgnoreCase("Text");
    }
    
    public static boolean isNumberProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("int") || 
                p.getType().equalsIgnoreCase("java.lang.Integer") || 
                p.getType().equalsIgnoreCase("long") || 
                p.getType().equalsIgnoreCase("java.lang.Long") || 
                p.getType().equalsIgnoreCase("double") || 
                p.getType().equalsIgnoreCase("java.lang.Double") ||
                p.getType().equalsIgnoreCase("float") || 
                p.getType().equalsIgnoreCase("java.lang.Float") || 
                p.getType().equalsIgnoreCase("Number");
    }
}
