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
package org.fusesource.ide.camel.model.connectors;

import java.io.InputStream;
import java.util.ArrayList;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.fusesource.ide.camel.model.Activator;
import org.xml.sax.InputSource;

/**
 * @author lhein
 */
@XmlRootElement(name="connectors")
public class ConnectorModel {
	
	private ArrayList<Connector> supportedConnectors;
	private String camelVersion;
	
	/**
	 * @return the camelVersion
	 */
	public String getCamelVersion() {
		return this.camelVersion;
	}
	
	/**
	 * @param camelVersion the camelVersion to set
	 */
	public void setCamelVersion(String camelVersion) {
		this.camelVersion = camelVersion;
	}
	
	/**
	 * @return the supportedConnectors
	 */
	@XmlElement(name = "connector")
	public ArrayList<Connector> getSupportedConnectors() {
		return this.supportedConnectors;
	}
	
	/**
	 * @param supportedConnectors the supportedConnectors to set
	 */
	public void setSupportedConnectors(ArrayList<Connector> supportedConnectors) {
		this.supportedConnectors = supportedConnectors;
	}
		
	/**
	 * creates the backlog tracer event message for a given xml dump
	 * 
	 * @param stream	the xml stream
	 * @return	the message object or null on errors
	 */
	public static ConnectorModel getConnectorFactoryInstance(InputStream stream, String camelVersion) {
		try {
			// create JAXB context and instantiate marshaller
		    JAXBContext context = JAXBContext.newInstance(ConnectorModel.class, Connector.class, ConnectorDependency.class, ConnectorProtocol.class);
		    Unmarshaller um = context.createUnmarshaller();
		    ConnectorModel model = (ConnectorModel) um.unmarshal(new InputSource(stream));
		    model.setCamelVersion(camelVersion);
		    return model;
		} catch (JAXBException ex) {
			Activator.getLogger().error(ex);
		}
		return null;
	}
}
