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
public final class CamelComponentUtils {

    private static List<CamelComponentModel> knownPropertyModels = new ArrayList<CamelComponentModel>();
    
    static {
        ArrayList<CamelComponentUriParameter> propertiesList = new ArrayList<CamelComponentUriParameter>();
        propertiesList.add(new CamelComponentUriParameter("autoCreate", "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("bufferSize", "int", 128000, CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("fileName", "java.io.File", null, CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("flatten", "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("charset", "java.lang.String", null, CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("copyAndDeleteOnRenameFail", "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("renameUsingCopy", "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.BOTH));
        propertiesList.add(new CamelComponentUriParameter("initialDelay",  "long", 1000l, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("delay",  "long", 500l, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("useFixedDelay",  "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("runLoggingLevel",  "choice[INFO, WARN, ERROR, TRACE]", "TRACE", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("recursive",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("delete",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("noop",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("preMove",  "Expression", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("move",  "Expression", ".camel", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("moveFailed",  "Expression", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("include",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("exclude",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("antInclude",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("antExclude",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("antFilterCaseSensitive",  "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("idempotent",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("idempotentKey",  "Expression", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("idempotentRepository",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("inProgressRepository",  "java.lang.String", "memory", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("filter",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("sorter",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("sortBy",  "Expression", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLock",  "java.lang.String", "markerFile", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLockTimeout",  "long", 10000l, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLockCheckInterval",  "long", 1000l, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLockMinLength",  "int", 1, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLockLoggingLevel",  "choice[INFO, WARN, ERROR, TRACE]", "WARN", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("readLockMarkerFile",  "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("directoryMustExist",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("doneFileName",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("exclusiveReadLockStrategy",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("maxMessagesPerPoll",  "int", 0, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("eagerMaxMessagesPerPoll",  "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("minDepth",  "int", 0, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("maxDepth",  "int", Integer.MAX_VALUE, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("processStrategy",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("startingDirectoryMustExist",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("pollStrategy",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("sendEmptyMessageWhenIdle",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("consumer.bridgeErrorHandler",  "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("scheduledExecutorService",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("scheduler",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("backoffMultiplier",  "int", 0, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("backoffIdleThreshold",  "int", 0, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("backoffErrorThreshold",  "int", 0, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("fileExist", "choice[Override,Append,Fail,Ignore,Move,TryRename]", "Override", CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("tempPrefix", "java.lang.String", null, CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("tempFileName", "java.lang.String", null, CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("moveExisting", "Expression", null, CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("keepLastModified", "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("eagerDeleteTargetFile", "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("doneFileName", "java.lang.String", null, CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("allowNullBody", "boolean", Boolean.FALSE.booleanValue(), CamelComponentUriParameterKind.PRODUCER));
        propertiesList.add(new CamelComponentUriParameter("forceWrites", "boolean", Boolean.TRUE.booleanValue(), CamelComponentUriParameterKind.PRODUCER));
        
        CamelComponentModel model = new CamelComponentModel("file");
        model.setProperties(propertiesList);
        knownPropertyModels.add(model);
     
        propertiesList = new ArrayList<CamelComponentUriParameter>();
        propertiesList.add(new CamelComponentUriParameter("location", "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("lat",  "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("lon", "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("period", "java.lang.Integer", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("headerName", "java.lang.String", null, CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("mode", "choice[HTML,JSON,XML]", "JSON", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("units", "choice[IMPERIAL,METRIC]", "METRIC", CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("consumer.delay", "java.lang.Long", new Long(3600000), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("consumer.initialDelay", "java.lang.Long", new Long(1000), CamelComponentUriParameterKind.CONSUMER));
        propertiesList.add(new CamelComponentUriParameter("consumer.userFixedDelay", "boolean", false, CamelComponentUriParameterKind.CONSUMER));
                
        model = new CamelComponentModel(ConnectorModelFactory.getModelForVersion(Activator.getDefault().getCamelVersion()).getConnectorForComponent("weather"));
        model.setProperties(propertiesList);
        knownPropertyModels.add(model);
    }
    
    /**
     * returns the properties model for a given protocol
     * 
     * @param protocol  the protocol to get the properties for
     * @return  the properties model or null if not available
     */
    public static CamelComponentModel getPropertiesForEndpoint(String protocol) {
        for (CamelComponentModel model : knownPropertyModels) {
            if ((model.getConnector() != null && model.getConnector().supportsProtocol(protocol)) ||
                (model.getProtocol() != null && model.getProtocol().equalsIgnoreCase(protocol))) return model;    
        }
        
        // it seems we miss a model for the given protocol...lets try creating one on the fly
        CamelComponentModel model = buildModelForProtocol(protocol);
        if (model != null) {
            knownPropertyModels.add(model);
        }
        
        return model;
    }
    
    public static boolean isBooleanProperty(CamelComponentUriParameter p) {
        return  p.getType().equalsIgnoreCase("boolean") || 
                p.getType().equalsIgnoreCase("java.lang.Boolean");
    }
    
    public static boolean isTextProperty(CamelComponentUriParameter p) {
        return  p.getType().equalsIgnoreCase("String") || 
                p.getType().equalsIgnoreCase("java.lang.String") || 
                p.getType().equalsIgnoreCase("Text");
    }
    
    public static boolean isNumberProperty(CamelComponentUriParameter p) {
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
    
    public static boolean isChoiceProperty(CamelComponentUriParameter p) {
        return p.getType().toLowerCase().startsWith("choice[");
    }
    
    public static boolean isFileProperty(CamelComponentUriParameter p) {
        return  p.getType().equalsIgnoreCase("file") ||
                p.getType().equalsIgnoreCase("java.io.file");
    }
    
    public static boolean isFolderProperty(CamelComponentUriParameter p) {
        return  p.getType().equalsIgnoreCase("folder") ||
                p.getType().equalsIgnoreCase("path") || 
                p.getType().equalsIgnoreCase("directory");
    }
    
    public static boolean isExpressionProperty(CamelComponentUriParameter p) {
        return  p.getType().equalsIgnoreCase("expression");
    }
    
    public static String[] getChoices(CamelComponentUriParameter p) {
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
    protected static CamelComponentModel buildModelForProtocol(String protocol) {
        CamelComponentModel resModel = null;

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
