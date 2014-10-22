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

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.apache.maven.model.Dependency;
import org.apache.maven.model.InputLocation;
import org.apache.maven.model.Model;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.internal.project.registry.MavenProjectManager;
import org.fusesource.ide.camel.editor.Activator;
import org.fusesource.ide.camel.editor.editor.RiderDesignEditor;
import org.fusesource.ide.camel.model.connectors.Connector;
import org.fusesource.ide.camel.model.connectors.ConnectorModelFactory;
import org.fusesource.ide.camel.model.connectors.ConnectorProtocol;

/**
 * @author lhein
 */
public final class EndpointPropertiesUtils {

    private static List<EndpointPropertyModel> knownPropertyModels = new ArrayList<EndpointPropertyModel>();
    
    static {
        ArrayList<EndpointProperty> propertiesList = new ArrayList<EndpointProperty>();
        propertiesList.add(new EndpointProperty("autoCreate", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("bufferSize", "int", 128000, EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("fileName", "java.io.File", null, EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("flatten", "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("charset", "java.lang.String", null, EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("copyAndDeleteOnRenameFail", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("renameUsingCopy", "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.BOTH));
        propertiesList.add(new EndpointProperty("initialDelay",  "long", 1000l, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("delay",  "long", 500l, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("useFixedDelay",  "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("runLoggingLevel",  "choice[INFO, WARN, ERROR, TRACE]", "TRACE", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("recursive",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("delete",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("noop",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("preMove",  "Expression", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("move",  "Expression", ".camel", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("moveFailed",  "Expression", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("include",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("exclude",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("antInclude",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("antExclude",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("antFilterCaseSensitive",  "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("idempotent",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("idempotentKey",  "Expression", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("idempotentRepository",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("inProgressRepository",  "java.lang.String", "memory", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("filter",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("sorter",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("sortBy",  "Expression", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLock",  "java.lang.String", "markerFile", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLockTimeout",  "long", 10000l, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLockCheckInterval",  "long", 1000l, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLockMinLength",  "int", 1, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLockLoggingLevel",  "choice[INFO, WARN, ERROR, TRACE]", "WARN", EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("readLockMarkerFile",  "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("directoryMustExist",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("doneFileName",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("exclusiveReadLockStrategy",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("maxMessagesPerPoll",  "int", 0, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("eagerMaxMessagesPerPoll",  "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("minDepth",  "int", 0, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("maxDepth",  "int", Integer.MAX_VALUE, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("processStrategy",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("startingDirectoryMustExist",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("pollStrategy",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("sendEmptyMessageWhenIdle",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("consumer.bridgeErrorHandler",  "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("scheduledExecutorService",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("scheduler",  "java.lang.String", null, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("backoffMultiplier",  "int", 0, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("backoffIdleThreshold",  "int", 0, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("backoffErrorThreshold",  "int", 0, EndpointPropertyKind.CONSUMER));
        propertiesList.add(new EndpointProperty("fileExist", "choice[Override,Append,Fail,Ignore,Move,TryRename]", "Override", EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("tempPrefix", "java.lang.String", null, EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("tempFileName", "java.lang.String", null, EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("moveExisting", "Expression", null, EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("keepLastModified", "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("eagerDeleteTargetFile", "boolean", Boolean.TRUE.booleanValue(), EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("doneFileName", "java.lang.String", null, EndpointPropertyKind.PRODUCER));
        propertiesList.add(new EndpointProperty("allowNullBody", "boolean", Boolean.FALSE.booleanValue(), EndpointPropertyKind.PRODUCER));
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
        
        // it seems we miss a model for the given protocol...lets try creating one on the fly
        EndpointPropertyModel model = buildModelForProtocol(protocol);
        if (model != null) {
            knownPropertyModels.add(model);
        }
        
        return model;
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
    
    public static boolean isFileProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("file") ||
                p.getType().equalsIgnoreCase("java.io.file");
    }
    
    public static boolean isFolderProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("folder") ||
                p.getType().equalsIgnoreCase("path") || 
                p.getType().equalsIgnoreCase("directory");
    }
    
    public static boolean isExpressionProperty(EndpointProperty p) {
        return  p.getType().equalsIgnoreCase("expression");
    }
    
    public static String[] getChoices(EndpointProperty p) {
        String rawChoices = p.getType().substring(p.getType().indexOf('[')+1, p.getType().indexOf(']'));
        return rawChoices.split(",");
    }
    
    public static String buildChoice(Connector connector, String protocol) {
        String result = "choice[";
        
        if (connector != null) {
            boolean first = true;
            for (ConnectorProtocol p : connector.getProtocols()) {
                if (first) {
                    first = false;
                } else {
                    result += ",";
                }
                result += p.getProtocol();
            }        
        } else {
            result += protocol;
        }
        result += "]";
        
        return result;
    }
    
    /**
     * tries to build the model by parsing the camel configuration class annotations and elements
     * 
     * @param protocol
     * @return
     */
    protected static EndpointPropertyModel buildModelForProtocol(String protocol) {
        EndpointPropertyModel resModel = null;

        IProject project = Activator.getDiagramEditor().getCamelContextFile().getProject();
                
        try {
            IJavaProject javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
            IClasspathEntry[] rawClasspath = javaProject.getRawClasspath();
            for(IClasspathEntry cpEntry : rawClasspath){
                System.err.println("CP ENTRY: " + cpEntry.getPath().toOSString());
            }
        } catch (CoreException e) {
            e.printStackTrace();
        }
        
        
        // search classpath for a file called like "protocol" value in a folder called /META-INF/services/org/apache/camel/component/
        
        // extract the value of key "class" from that property file
    
        return resModel;
    }
    
}
