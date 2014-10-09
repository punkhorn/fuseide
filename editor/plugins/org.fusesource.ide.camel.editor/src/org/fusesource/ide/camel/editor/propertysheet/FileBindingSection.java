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

import java.io.File;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;
import org.fusesource.ide.camel.editor.AbstractNodes;
import org.fusesource.ide.camel.model.AbstractNode;
import org.fusesource.ide.camel.model.Endpoint;
import org.fusesource.ide.commons.ui.Selections;

/**
 * @author lhein
 */
public class FileBindingSection extends AbstractPropertySection {

    private FormToolkit toolkit;
    private Form form;
    private DataBindingContext bindingContext;
    private Composite parent;
    
    private Text txtPath;
    private Button btnPathBrowser;
    
    private Endpoint selectedEP;
    
    /**
     * 
     */
    public FileBindingSection() {
        bindingContext = new DataBindingContext();
    }
    
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#dispose()
     */
    @Override
    public void dispose() {
        if (toolkit != null) {
            toolkit.dispose();
            toolkit = null;
        }
        super.dispose();
    }
    
    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart, org.eclipse.jface.viewers.ISelection)
     */
    @Override
    public void setInput(IWorkbenchPart part, ISelection selection) {
        super.setInput(part, selection);
        Object o = Selections.getFirstSelection(selection);
        AbstractNode n = AbstractNodes.toAbstractNode(o);
        if (n instanceof Endpoint) {
            this.selectedEP = (Endpoint)n;
        } else {
            this.selectedEP = null;
        }
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite, org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
     */
    @Override
    public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
        this.parent = parent;
        this.toolkit = new FormToolkit(parent.getDisplay());
        super.createControls(parent, aTabbedPropertySheetPage);        
        
        // now setup the file binding properties page
        parent.setLayout(new GridLayout());
        parent.setLayoutData(new GridData(GridData.FILL_BOTH));

        form = toolkit.createForm(parent);
        form.setLayoutData(new GridData(GridData.FILL_BOTH));
        form.setText("Filesystem Connector");
        toolkit.decorateFormHeading(form);

        form.getBody().setLayout(new GridLayout(3, false));

        Composite sbody = form.getBody();
        
        toolkit.createLabel(sbody, "Path:");
        this.txtPath = toolkit.createText(sbody, "", SWT.BORDER | SWT.RIGHT);
        this.txtPath.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
        this.btnPathBrowser = toolkit.createButton(sbody, "...", SWT.BORDER | SWT.PUSH);
        
        this.btnPathBrowser.addSelectionListener(new SelectionAdapter() {
            /* (non-Javadoc)
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                String path = new DirectoryDialog(Display.getDefault().getActiveShell()).open();
                if (path != null) {
                    txtPath.setText(path);
                }
            }
        });
        
        this.txtPath.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                String path = txtPath.getText();
                File f = new File(path);
                if (selectedEP != null) {
                    selectedEP.setUri(f.toURI().toString());
                }
            }
        });
        
        form.layout();
    }
}
