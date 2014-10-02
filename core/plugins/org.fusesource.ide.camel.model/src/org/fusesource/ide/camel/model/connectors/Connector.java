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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author lhein
 */
@XmlRootElement(name = "connector")
public class Connector {
	
	private String id;
	private ArrayList<ConnectorProtocol> protocols;
	private ConnectorDependency dependency;
	
	/**
	 * @return the id
	 */
	@XmlElement(name = "id")
	public String getId() {
		return this.id;
	}
	
	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}
	
	/**
	 * @return the protocols
	 */
	@XmlElementWrapper(name = "protocols")
	@XmlElement(name = "protocol")
	public ArrayList<ConnectorProtocol> getProtocols() {
		return this.protocols;
	}
	
	/**
	 * @param protocols the protocols to set
	 */
	public void setProtocols(ArrayList<ConnectorProtocol> protocols) {
		this.protocols = protocols;
	}
	
	/**
	 * @return the dependency
	 */
	@XmlElement(name = "dependency")
	public ConnectorDependency getDependency() {
		return this.dependency;
	}
	
	/**
	 * @param dependency the dependency to set
	 */
	public void setDependency(ConnectorDependency dependency) {
		this.dependency = dependency;
	}
}
