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
package org.fusesource.ide.camel.editor.propertysheet;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.forms.IFormColors;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.forms.widgets.FormsResources;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;
import org.fusesource.ide.camel.editor.AbstractNodes;
import org.fusesource.ide.camel.editor.propertysheet.model.EndpointPropertiesUtils;
import org.fusesource.ide.camel.editor.propertysheet.model.EndpointProperty;
import org.fusesource.ide.camel.editor.propertysheet.model.EndpointPropertyKind;
import org.fusesource.ide.camel.editor.propertysheet.model.EndpointPropertyModel;
import org.fusesource.ide.camel.editor.utils.DiagramUtils;
import org.fusesource.ide.camel.model.AbstractNode;
import org.fusesource.ide.camel.model.Endpoint;
import org.fusesource.ide.commons.ui.Selections;

/**
 * @author lhein
 */
public class AdvancedEndpointPropertiesSection extends AbstractPropertySection {

    private FormToolkit toolkit;
    private Form form;
    private CTabFolder tabFolder;
    private CTabItem commonTab;
    private CTabItem consumerTab;
    private CTabItem producerTab;
    private Endpoint selectedEP;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#dispose()
     */
    @Override
    public void dispose() {
        if (toolkit != null) {
            toolkit.dispose();
            toolkit = null;
        }
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput
     * (org.eclipse.ui.IWorkbenchPart, org.eclipse.jface.viewers.ISelection)
     */
    @Override
    public void setInput(IWorkbenchPart part, ISelection selection) {
        super.setInput(part, selection);
        
        Object o = Selections.getFirstSelection(selection);
        AbstractNode n = AbstractNodes.toAbstractNode(o);
        
        if (n != null && n.equals(selectedEP)) {
            // same endpoint - no need to do something
            return;
        }
        
        if (n instanceof Endpoint) {
            this.selectedEP = (Endpoint) n;
            form.setText("Advanced Properties - " + DiagramUtils.filterFigureLabel(selectedEP.getDisplayText()));
        } else {
            this.selectedEP = null;
            form.setText("Advanced Properties");
        }
        
        int idx = Math.max(tabFolder.getSelectionIndex(), 0);

        if (commonTab != null)      commonTab.dispose();
        if (consumerTab != null)    consumerTab.dispose();
        if (producerTab != null)    producerTab.dispose();
        
        // now generate the tab contents
        createCommonsTab(tabFolder);
        createConsumerTab(tabFolder);
        createProducerTab(tabFolder);
        
        tabFolder.setSelection(Math.min(idx, tabFolder.getItemCount()-1));
    }
    
    /**
     * updates the uri for the changed value
     * 
     * @param p
     * @param value
     */
    protected void updateURI(EndpointProperty p, Object value) {
        if (p.getName().equals(EndpointPropertyModel.PROTOCOL_PROPERTY) && EndpointPropertiesUtils.isChoiceProperty(p)) {
            String oldProtocol = getUsedProtocol();
            if (oldProtocol.equalsIgnoreCase(value.toString()) == false) {
                // protocol changed - update uri
                selectedEP.setUri(selectedEP.getUri().replaceFirst(oldProtocol, value.toString()));
            }
        } else {
            String val = getPropertyFromUri(p);
            if (val != null) {
                selectedEP.setUri(selectedEP.getUri().replaceFirst(String.format("%s=%s", p.getName(), val), String.format("%s=%s", p.getName(), value.toString())));
            } else {
                String newUri = selectedEP.getUri();
                if (selectedEP.getUri().indexOf('?') == -1) {
                    newUri += '?';
                }
                if (selectedEP.getUri().indexOf('=') != -1) {
                    newUri += '&';
                }
                newUri += String.format("%s=%s", p.getName(), value.toString());
                selectedEP.setUri(newUri);
            }
        }
    }
    
    protected String getUsedProtocol() {
        return selectedEP.getUri().substring(0, selectedEP.getUri().indexOf(':'));
    }

    /**
     * 
     * @param props
     * @param page
     */
    protected void generateTabContents(List<EndpointProperty> props, Composite page) {
        for (EndpointProperty p : props) {
            final EndpointProperty prop = p;
            
            Label l = toolkit.createLabel(page, p.getName());            
            l.setLayoutData(new GridData());
            
            if (EndpointPropertiesUtils.isBooleanProperty(prop)) {
                Button checkBox = toolkit.createButton(page, "", SWT.CHECK | SWT.BORDER);
                Boolean b = (Boolean)getTypedPropertyFromUri(prop);
                checkBox.setSelection(b);
                checkBox.addSelectionListener(new SelectionAdapter() {
                    /* (non-Javadoc)
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        updateURI(prop, ((Button)e.getSource()).getSelection());
                    }
                });
                checkBox.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
            } else if (EndpointPropertiesUtils.isTextProperty(prop)) {
                Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
            } else if (EndpointPropertiesUtils.isNumberProperty(prop)) {
                Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.RIGHT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        for (int i = 0; i<txt.getText().length(); i++) {
                            char c = txt.getText().charAt(i);
                            if (!Character.isDigit(c)) {
                                // invalid character found
                                txt.setBackground(ColorConstants.red);
                                return;
                            }
                        }
                        txt.setBackground(ColorConstants.white);
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
            } else if (EndpointPropertiesUtils.isChoiceProperty(prop)) {
                CCombo choiceCombo = new CCombo(page, SWT.BORDER | SWT.FLAT | SWT.READ_ONLY | SWT.SINGLE);
                toolkit.adapt(choiceCombo, true, true);
                choiceCombo.setEditable(false);
                choiceCombo.setItems(EndpointPropertiesUtils.getChoices(prop));
                for (int i=0; i < choiceCombo.getItems().length; i++) {
                    if (choiceCombo.getItem(i).equals(getUsedProtocol())) {
                        choiceCombo.select(i);
                        break;
                    }
                }
                choiceCombo.addSelectionListener(new SelectionAdapter() {
                    /* (non-Javadoc)
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        CCombo choice = (CCombo)e.getSource();
                        updateURI(prop, choice.getText());
                    }
                });
                choiceCombo.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
            }
        }
    }

    private void createCommonsTab(CTabFolder folder) {
        List<EndpointProperty> props = getPropertiesFor(EndpointPropertyKind.BOTH);

        if (props.isEmpty()) return;
        
        commonTab = new CTabItem(tabFolder, SWT.NONE, 0);
        commonTab.setText("General");

        Composite page = toolkit.createComposite(folder);
        page.setLayout(new GridLayout(3, false));
                
        generateTabContents(props, page);

        commonTab.setControl(page);
    }

    private void createConsumerTab(CTabFolder folder) {
        List<EndpointProperty> props = getPropertiesFor(EndpointPropertyKind.CONSUMER);
        
        if (props.isEmpty()) return;
        
        consumerTab = new CTabItem(tabFolder, SWT.NONE, 1);
        consumerTab.setText("Consumer");

        Composite page = toolkit.createComposite(folder);
        page.setLayout(new GridLayout(3, false));
                
        generateTabContents(props, page);        
        
        consumerTab.setControl(page);
    }

    private void createProducerTab(CTabFolder folder) {
        List<EndpointProperty> props = getPropertiesFor(EndpointPropertyKind.PRODUCER);
        
        if (props.isEmpty()) return;
        
        producerTab = new CTabItem(tabFolder, SWT.NONE, 2);
        producerTab.setText("Producer");
        
        Composite page = toolkit.createComposite(folder);
        page.setLayout(new GridLayout(3, false));
                
        generateTabContents(props, page);
        
        producerTab.setControl(page);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls
     * (org.eclipse.swt.widgets.Composite,
     * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
     */
    @Override
    public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
        this.toolkit = new FormToolkit(parent.getDisplay());
        super.createControls(parent, aTabbedPropertySheetPage);

        // now setup the file binding properties page
        parent.setLayout(new GridLayout());
        parent.setLayoutData(new GridData(GridData.FILL_BOTH));

        form = toolkit.createForm(parent);
        form.setLayoutData(new GridData(GridData.FILL_BOTH));
        form.getBody().setLayout(new GridLayout(1, false));

        Composite sbody = form.getBody();

        tabFolder = new CTabFolder(sbody, SWT.TOP | SWT.FLAT);
        toolkit.adapt(tabFolder, true, true);
        tabFolder.setLayoutData(new GridData(GridData.FILL_BOTH));

        Color selectedColor = toolkit.getColors().getColor(IFormColors.SEPARATOR);
        tabFolder.setSelectionBackground(new Color[] { selectedColor, toolkit.getColors().getBackground() }, new int[] { 20 }, true);
        tabFolder.setCursor(FormsResources.getHandCursor());
        toolkit.paintBordersFor(tabFolder);

        form.setText("Advanced Properties");
        toolkit.decorateFormHeading(form);
        
        form.layout();
        tabFolder.setSelection(0);
    }

    /**
     * 
     * @param kind
     * @return
     */
    protected List<EndpointProperty> getPropertiesFor(EndpointPropertyKind kind) {
        ArrayList<EndpointProperty> result = new ArrayList<EndpointProperty>();

        if (selectedEP != null && selectedEP.getUri() != null) {
            int protocolSeparatorIdx = selectedEP.getUri().indexOf(":");
            if (protocolSeparatorIdx != -1) {
                EndpointPropertyModel model = EndpointPropertiesUtils.getPropertiesForEndpoint(selectedEP.getUri().substring(0, protocolSeparatorIdx));
                if (model != null) {
                    for (EndpointProperty p : model.getProperties()) {
                        if (p.getKind().equals(kind)) {
                            result.add(p);
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * 
     * @param p
     * @return
     */
    protected String getPropertyFromUri(EndpointProperty p) {
        int idx = selectedEP.getUri().indexOf(p.getName() + "=");
        if (idx != -1) {
            return selectedEP.getUri().substring(idx + (p.getName() + "=").length(),
                    selectedEP.getUri().indexOf('&', idx + 1) != -1 ? selectedEP.getUri().indexOf('&', idx + 1) : selectedEP.getUri().length());
        }
        return null;
    }

    /**
     * 
     * @param p
     * @return
     */
    protected Object getTypedPropertyFromUri(EndpointProperty p) {
        String val = getPropertyFromUri(p);

        if (EndpointPropertiesUtils.isBooleanProperty(p)) {
            return Boolean.parseBoolean(val);
        }

        if (EndpointPropertiesUtils.isTextProperty(p)) {
            return val;
        }

        if (EndpointPropertiesUtils.isNumberProperty(p)) {
            return val;
        }

        return null;
    }
}
