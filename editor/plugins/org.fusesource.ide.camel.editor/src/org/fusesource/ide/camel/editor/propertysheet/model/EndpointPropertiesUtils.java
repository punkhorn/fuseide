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
import java.util.List;

import org.fusesource.ide.camel.editor.Activator;
import org.fusesource.ide.camel.model.connectors.ConnectorModelFactory;

/**
 * @author lhein
 */
public final class EndpointPropertiesUtils {

    private static final List<EndpointPropertyModel> knownPropertyModels = new ArrayList<EndpointPropertyModel>();
    
    static {
        ArrayList<EndpointProperty> propertiesList = new ArrayList<EndpointProperty>();
        propertiesList.add(new EndpointProperty("autoCreate", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("recursive",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("bufferSize", "int", 128000, EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("forceWrites", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.PRODUCER));
                
        EndpointPropertyModel model = new EndpointPropertyModel("file");
        model.setProperties(propertiesList);
        knownPropertyModels.add(model);
     
        propertiesList = new ArrayList<EndpointProperty>();
        propertiesList.add(new EndpointProperty("location", "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("lat",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("lon", "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("period", "java.lang.Integer", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("headerName", "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("mode", "choice[HTML,JSON,XML]", "JSON", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("units", "choice[IMPERIAL,METRIC]", "METRIC", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("consumer.delay", "java.lang.Long", new Long(3600000), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("consumer.initialDelay", "java.lang.Long", new Long(1000), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("consumer.userFixedDelay", "boolean", false, EndpointPropertyKind.CONSUMER));
                
        model = new EndpointPropertyModel(ConnectorModelFactory.getModelForVersion(Activator.getDefault().getCamelVersion()).getConnectorForComponent("weather"));
        model.setProperties(propertiesList);
        knownPropertyModels.add(model);
    }
    
    /**
     * returns the properties model for a given protocol
     * 
     * @param protocol  the protocol to get the properties for
     * @return  the properties model or null if not available
     */
    public static EndpointPropertyModel getPropertiesForEndpoint(String protocol) {
        for (EndpointPropertyModel model : knownPropertyModels) {
            if ((model.getConnector() != null && model.getConnector().supportsProtocol(protocol)) ||
                (model.getProtocol() != null && model.getProtocol().equalsIgnoreCase(protocol))) return model;    
        }
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
    
    public static boolean isChoiceProperty(EndpointProperty p) {
        return p.getType().toLowerCase().startsWith("choice[");
    }
    
    public static String[] getChoices(EndpointProperty p) {
        String rawChoices = p.getType().substring(p.getType().indexOf('[')+1, p.getType().indexOf(']'));
        return rawChoices.split(",");
    }
}
