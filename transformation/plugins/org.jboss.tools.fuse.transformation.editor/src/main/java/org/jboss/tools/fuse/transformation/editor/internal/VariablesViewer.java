/******************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials are
 * made available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: JBoss by Red Hat - Initial implementation.
 *****************************************************************************/
package org.jboss.tools.fuse.transformation.editor.internal;

import java.util.Iterator;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.jboss.mapper.Variable;
import org.jboss.tools.fuse.transformation.editor.Activator;
import org.jboss.tools.fuse.transformation.editor.internal.util.TransformationConfig;
import org.jboss.tools.fuse.transformation.editor.internal.util.Util.Colors;
import org.jboss.tools.fuse.transformation.editor.internal.util.Util.Decorations;
import org.jboss.tools.fuse.transformation.editor.internal.util.Util.Images;

/**
 *
 */
public final class VariablesViewer extends Composite {

    final TableViewer tableViewer;

    /**
     * @param config
     * @param parent
     */
    public VariablesViewer(final TransformationConfig config,
                           final Composite parent) {
        super(parent, SWT.NONE);

        setLayout(GridLayoutFactory.fillDefaults().create());
        setBackground(parent.getParent().getParent().getBackground());

        // Create tool bar
        final ToolBar toolBar = new ToolBar(this, SWT.NONE);
        final ToolItem addButton = new ToolItem(toolBar, SWT.PUSH);
        addButton.setImage(new DecorationOverlayIcon(Images.VARIABLE,
                                                     Decorations.ADD,
                                                     IDecoration.TOP_RIGHT).createImage());
        addButton.setToolTipText("Add a new variable");
        final ToolItem deleteButton = new ToolItem(toolBar, SWT.PUSH);
        deleteButton.setImage(PlatformUI.getWorkbench()
                                        .getSharedImages()
                                        .getImage(ISharedImages.IMG_ETOOL_DELETE));
        deleteButton.setToolTipText("Delete the selected variable(s)");
        deleteButton.setEnabled(false);

        // Create table
        tableViewer = new TableViewer(this);
        tableViewer.getTable().setHeaderVisible(true);
        tableViewer.getTable().setLayoutData(GridDataFactory.fillDefaults()
                                                            .grab(true, true)
                                                            .create());
        tableViewer.setComparator(new ViewerComparator());
        tableViewer.setContentProvider(ArrayContentProvider.getInstance());
        // Create columns
        // TODO add support for changing variable names
        final TableViewerColumn nameColumn = new TableViewerColumn(tableViewer, SWT.NONE);
        nameColumn.getColumn().setText("Name");
        nameColumn.getColumn().setImage(Images.VARIABLE);
        nameColumn.setLabelProvider(new ColumnLabelProvider() {

            @Override
            public Color getForeground(Object element) {
                return Colors.VARIABLE;
            }

            @Override
            public Image getImage(final Object element) {
                final Image img = Images.VARIABLE;
                if (config.mapped((Variable) element))
                    return new DecorationOverlayIcon(img,
                                                     Decorations.MAPPED,
                                                     IDecoration.BOTTOM_RIGHT).createImage();
                return img;
            }

            @Override
            public String getText(final Object element) {
                return ((Variable) element).getName();
            }
        });
        final TableViewerColumn valueColumn = new TableViewerColumn(tableViewer, SWT.NONE);
        valueColumn.getColumn().setText("Value");
        valueColumn.setLabelProvider(new ColumnLabelProvider() {

            @Override
            public String getText(final Object element) {
                return "\"" + ((Variable) element).getValue() + "\"";
            }
        });
        valueColumn.setEditingSupport(new EditingSupport(tableViewer) {

            private final TextCellEditor cellEditor = new TextCellEditor(tableViewer.getTable());

            @Override
            protected boolean canEdit(final Object element) {
                return true;
            }

            @Override
            protected CellEditor getCellEditor(final Object element) {
                return cellEditor;
            }

            @Override
            protected Object getValue(final Object element) {
                return ((Variable) element).getValue();
            }

            @Override
            protected void setValue(final Object element,
                                    final Object value) {
                Variable variable = (Variable) element;
                config.setValue(variable, value.toString());
                try {
                    config.save();
                    tableViewer.setInput(config.getVariables());
                } catch (final Exception e) {
                    Activator.error(e);
                }
            }
        });

        // Wire
        tableViewer.addDragSupport(DND.DROP_MOVE,
                new Transfer[] {LocalSelectionTransfer.getTransfer()}, new DragSourceAdapter() {

                    @Override
                    public void dragStart(final DragSourceEvent event) {
                        LocalSelectionTransfer.getTransfer()
                                              .setSelection(tableViewer.getSelection());
                    }
                });
        addButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent event) {
                final InputDialog dlg = new InputDialog(getShell(),
                                                        "Add Variable",
                                                        "Enter a new variable name",
                                                        null,
                                                        new IInputValidator() {

                    @Override
                    public String isValid(final String text) {
                        for (final Variable variable : config.getVariables()) {
                            if (variable.getName().equals(text))
                                return "Variable already exists";
                        }
                        return null;
                    }
                });
                if (dlg.open() != Window.OK) return;
                config.addVariable(dlg.getValue(), dlg.getValue());
                try {
                    config.save();
                    tableViewer.setInput(config.getVariables());
                } catch (final Exception e) {
                    Activator.error(e);
                }
            }
        });
        tableViewer.addSelectionChangedListener(new ISelectionChangedListener() {

            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                deleteButton.setEnabled(!event.getSelection().isEmpty());
            }
        });
        deleteButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(final SelectionEvent event) {
                boolean deleteAll = false;
                try {
                    for (final Iterator<?> iter =
                            ((IStructuredSelection) tableViewer.getSelection()).iterator();
                         iter.hasNext();) {
                        final Variable variable = (Variable) iter.next();
                        if (config.mapped(variable)) {
                            if (!deleteAll) {
                                final MessageDialog dlg =
                                        new MessageDialog(getShell(),
                                                          "Confirm",
                                                          null,
                                                          "Variable \"" + variable.getName()
                                                          + "\" is being used in one or more mappings.\n\n"
                                                          + "Are you sure you want to delete it?",
                                                          MessageDialog.WARNING,
                                                          new String[] {
                                                              "Cancel",
                                                              "Yes",
                                                              "Yes For All",
                                                              "No"
                                                          },
                                                          3);
                                final int button = dlg.open();
                                if (button == 2) deleteAll = true;
                                else if (button == 3) continue;
                                else if (button < 1) return;
                            }
                        }
                        config.removeVariable(variable);
                        tableViewer.remove(variable);
                    }
                    config.save();
                    tableViewer.setInput(config.getVariables());
                } catch (final Exception e) {
                    Activator.error(e);
                }
            }
        });

        // Populate
        tableViewer.setInput(config.getVariables());

        // Expand name and value columns to fill initial width of table each time table is resized
        tableViewer.getTable().addControlListener(new ControlAdapter() {

            @Override
            public void controlResized(final ControlEvent event) {
                final int width = tableViewer.getTable().getSize().x / 2;
                nameColumn.getColumn().setWidth(width);
                valueColumn.getColumn().setWidth(width);
            }
        });
    }
}
