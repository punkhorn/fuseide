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

import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.corext.util.JavaConventionsUtil;
import org.eclipse.jdt.internal.ui.wizards.NewClassCreationWizard;
import org.eclipse.jdt.ui.wizards.NewClassWizardPage;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
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
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.IFormColors;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.forms.widgets.FormsResources;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;
import org.fusesource.ide.camel.editor.AbstractNodes;
import org.fusesource.ide.camel.editor.Activator;
import org.fusesource.ide.camel.editor.propertysheet.model.CamelComponent;
import org.fusesource.ide.camel.editor.propertysheet.model.CamelComponentUriParameter;
import org.fusesource.ide.camel.editor.propertysheet.model.CamelComponentUriParameterKind;
import org.fusesource.ide.camel.editor.propertysheet.model.CamelComponentUtils;
import org.fusesource.ide.camel.editor.utils.DiagramUtils;
import org.fusesource.ide.camel.model.AbstractNode;
import org.fusesource.ide.camel.model.Endpoint;
import org.fusesource.ide.commons.ui.Selections;
import org.fusesource.ide.commons.util.Strings;

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

        tabFolder.setSingle(tabFolder.getItemCount()==1);
        
        tabFolder.setSelection(Math.min(idx, tabFolder.getItemCount()-1));
    }
    
    /**
     * updates the uri for the changed value
     * 
     * @param p
     * @param value
     */
    protected void updateURI(CamelComponentUriParameter p, Object value) {
//        if (p.getName().equals(EndpointPropertyModel.PROTOCOL_PROPERTY) && CamelComponentUtils.isChoiceProperty(p)) {
//            String oldProtocol = getUsedProtocol();
//            if (oldProtocol.equalsIgnoreCase(value.toString()) == false) {
//                // protocol changed - update uri
//                selectedEP.setUri(selectedEP.getUri().replaceFirst(oldProtocol, value.toString()));
//            }
//        } else {
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
//        }
    }
    
    protected String getUsedProtocol() {
        return selectedEP.getUri().substring(0, selectedEP.getUri().indexOf(':'));
    }

    /**
     * 
     * @param props
     * @param page
     */
    protected void generateTabContents(List<CamelComponentUriParameter> props, final Composite page) {
        // display all the properties in alphabetic order - sorting needed
        Collections.sort(props, new Comparator<CamelComponentUriParameter>() {
            /* (non-Javadoc)
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare(CamelComponentUriParameter o1, CamelComponentUriParameter o2) {
                return o1.getName().compareTo(o2.getName());
            }
        }); 
        
        for (CamelComponentUriParameter p : props) {
            final CamelComponentUriParameter prop = p;
            
            Label l = toolkit.createLabel(page, Strings.humanize(p.getName()));            
            l.setLayoutData(new GridData());
            
            if (CamelComponentUtils.isBooleanProperty(prop)) {
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
                
            } else if (CamelComponentUtils.isTextProperty(prop)) {
                Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
                
            } else if (CamelComponentUtils.isNumberProperty(prop)) {
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
                
            } else if (CamelComponentUtils.isChoiceProperty(prop)) {
                CCombo choiceCombo = new CCombo(page, SWT.BORDER | SWT.FLAT | SWT.READ_ONLY | SWT.SINGLE);
                toolkit.adapt(choiceCombo, true, true);
                choiceCombo.setEditable(false);
                choiceCombo.setItems(CamelComponentUtils.getChoices(prop));
                for (int i=0; i < choiceCombo.getItems().length; i++) {
                    if (choiceCombo.getItem(i).equalsIgnoreCase(getPropertyFromUri(prop))) {
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
                
            } else if (CamelComponentUtils.isFileProperty(prop)) {
                final Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));
                
                Button btn_browse = toolkit.createButton(page, "...", SWT.BORDER | SWT.PUSH);
                btn_browse.addSelectionListener(new SelectionAdapter() {
                    /* (non-Javadoc)
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        FileDialog fd = new FileDialog(page.getShell());
                        String fileName = fd.open();
                        if (fileName != null) {
                            txtField.setText(fileName);
                        }
                    }
                });
                btn_browse.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
                
            } else if (CamelComponentUtils.isFolderProperty(prop)) {
                final Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));
                
                Button btn_browse = toolkit.createButton(page, "...", SWT.BORDER | SWT.PUSH);
                btn_browse.addSelectionListener(new SelectionAdapter() {
                    /* (non-Javadoc)
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        DirectoryDialog dd = new DirectoryDialog(page.getShell());
                        String pathName = dd.open();
                        if (pathName != null) {
                            txtField.setText(pathName);
                        }
                    }
                });
                btn_browse.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));

            } else if (CamelComponentUtils.isExpressionProperty(prop)) {
                Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 2, 1));
                
            } else {
                // must be some class as all other options were missed
                final Text txtField = toolkit.createText(page, getPropertyFromUri(prop), SWT.SINGLE | SWT.BORDER | SWT.LEFT);
                txtField.addModifyListener(new ModifyListener() {
                    @Override
                    public void modifyText(ModifyEvent e) {
                        Text txt = (Text)e.getSource();
                        updateURI(prop, txt.getText());
                    }
                });
                txtField.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false, 1, 1));
                
                Button btn_browse = toolkit.createButton(page, "...", SWT.BORDER | SWT.PUSH);
                btn_browse.addSelectionListener(new SelectionAdapter() {
                    /* (non-Javadoc)
                     * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                     */
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        URLClassLoader child = CamelComponentUtils.getProjectClassLoader();
                        Class classToLoad = null;
                        try {
                            classToLoad = child.loadClass(prop.getType());
                        } catch (ClassNotFoundException ex) {
                            Activator.getLogger().warning("Cannot find class " + prop.getType() + " on classpath.", ex);
                            return;
                        }
                        
                        IProject project = Activator.getDiagramEditor().getCamelContextFile().getProject();
                        NewClassCreationWizard wiz = new NewClassCreationWizard();
                        wiz.addPages();
                        wiz.init(PlatformUI.getWorkbench(), null);
                        NewClassWizardPage wp = (NewClassWizardPage)wiz.getStartingPage();
                        WizardDialog wd = new WizardDialog(e.display.getActiveShell(), wiz);
                        if (classToLoad.isInterface()) {
                            wp.setSuperInterfaces(Arrays.asList(classToLoad.getName()), true);
                        } else {
                            wp.setSuperClass(classToLoad.getName(), true);
                        }
                        wp.setAddComments(true, true);
                        IPackageFragmentRoot fragroot = null;
                        try {
                            IJavaProject javaProject = (IJavaProject)project.getNature(JavaCore.NATURE_ID);
                            IMavenProjectFacade facade = MavenPlugin.getMavenProjectRegistry().create(project, new NullProgressMonitor());
                            IPath[] paths = facade.getCompileSourceLocations();
                            if (paths != null && paths.length>0) {
                                for (IPath p :paths) {
                                    if (p == null) continue; 
                                    IResource res = project.findMember(p);
                                    fragroot = javaProject.getPackageFragmentRoot(res);
                                    break;
                                }
                                if (fragroot != null) wp.setPackageFragmentRoot(fragroot, true);   
                                wp.setPackageFragment(getPackage(javaProject, fragroot), true);
                            }
                        } catch (Exception ex) {
                            Activator.getLogger().error(ex);
                        }
                        if (Window.OK == wd.open()) {
                            String value = wp.getCreatedType().getFullyQualifiedName();
                            if (value != null) txtField.setText(value);
                        }
                    }
                });
                btn_browse.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
            }
        }
    }
    
    /**
     * Checks if the package field has to be pre-filled in this page and returns the package
     * fragment to be used for that. The package fragment has the name of the project if the source
     * folder does not contain any package and if the project name is a valid package name. If the
     * source folder contains exactly one package then the name of that package is used as the
     * package fragment's name. <code>null</code> is returned if none of the above is applicable.
     * 
     * @param javaProject the containing Java project of the selection used to initialize this page
     * 
     * @return the package fragment to be pre-filled in this page or <code>null</code> if no
     *         suitable package can be suggested for the given project
     * 
     * @since 3.9
     */
    private IPackageFragment getPackage(IJavaProject javaProject, final IPackageFragmentRoot pkgFragmentRoot) {
        String packName= null;
        IJavaElement[] packages= null;
        try {
            if (pkgFragmentRoot != null && pkgFragmentRoot.exists()) {
                packages= pkgFragmentRoot.getChildren();
                if (packages.length == 1) { // only default package -> use Project name
                    packName= javaProject.getElementName();
                    // validate package name
                    IStatus status= validatePackageName(packName, javaProject);
                    if (status.getSeverity() == IStatus.OK) {
                        return pkgFragmentRoot.getPackageFragment(packName);
                    }
                } else {
                    int noOfPackages= 0;
                    IPackageFragment thePackage= null;
                    for (final IJavaElement pack : packages) {
                        IPackageFragment pkg= (IPackageFragment) pack;
                        // ignoring empty parent packages and default package
                        if ((!pkg.hasSubpackages() || pkg.hasChildren()) && !pkg.isDefaultPackage()) {
                            noOfPackages++;
                            thePackage= pkg;
                            if (noOfPackages > 1) {
                                return null;
                            }
                        }
                    }
                    if (noOfPackages == 1) { // use package name
                        packName= thePackage.getElementName();
                        return pkgFragmentRoot.getPackageFragment(packName);
                    }
                }
            }
        } catch (JavaModelException e) {
            // fall through
        }
        return null;
    }

    private static IStatus validatePackageName(String text, IJavaProject project) {
        if (project == null || !project.exists()) {
            return JavaConventions.validatePackageName(text, JavaCore.VERSION_1_3, JavaCore.VERSION_1_3);
        }
        return JavaConventionsUtil.validatePackageName(text, project);
    }
        
    private void createCommonsTab(CTabFolder folder) {
        List<CamelComponentUriParameter> props = getPropertiesFor(CamelComponentUriParameterKind.BOTH);

        if (props.isEmpty()) return;
        
        commonTab = new CTabItem(tabFolder, SWT.NONE);
        commonTab.setText("General");

        Composite page = toolkit.createComposite(folder);
        page.setLayout(new GridLayout(3, false));
                
        generateTabContents(props, page);

        commonTab.setControl(page);
    }

    private void createConsumerTab(CTabFolder folder) {
        List<CamelComponentUriParameter> props = getPropertiesFor(CamelComponentUriParameterKind.CONSUMER);
        
        if (props.isEmpty()) return;
        
        consumerTab = new CTabItem(tabFolder, SWT.NONE);
        consumerTab.setText("Consumer");

        Composite page = toolkit.createComposite(folder);
        page.setLayout(new GridLayout(3, false));
                
        generateTabContents(props, page);        
        
        consumerTab.setControl(page);
    }

    private void createProducerTab(CTabFolder folder) {
        List<CamelComponentUriParameter> props = getPropertiesFor(CamelComponentUriParameterKind.PRODUCER);
        
        if (props.isEmpty()) return;
        
        producerTab = new CTabItem(tabFolder, SWT.NONE);
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
    protected List<CamelComponentUriParameter> getPropertiesFor(CamelComponentUriParameterKind kind) {
        ArrayList<CamelComponentUriParameter> result = new ArrayList<CamelComponentUriParameter>();

        if (selectedEP != null && selectedEP.getUri() != null) {
            int protocolSeparatorIdx = selectedEP.getUri().indexOf(":");
            if (protocolSeparatorIdx != -1) {
                CamelComponent componentModel = CamelComponentUtils.getComponentModel(selectedEP.getUri().substring(0, protocolSeparatorIdx));
                if (componentModel != null) {
                    for (CamelComponentUriParameter p : componentModel.getUriParameters()) {
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
    protected String getPropertyFromUri(CamelComponentUriParameter p) {
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
    protected Object getTypedPropertyFromUri(CamelComponentUriParameter p) {
        String val = getPropertyFromUri(p);

        if (CamelComponentUtils.isBooleanProperty(p)) {
            return Boolean.parseBoolean(val);
        }

        if (CamelComponentUtils.isTextProperty(p)) {
            return val;
        }

        if (CamelComponentUtils.isNumberProperty(p)) {
            return val;
        }

        return null;
    }
}
