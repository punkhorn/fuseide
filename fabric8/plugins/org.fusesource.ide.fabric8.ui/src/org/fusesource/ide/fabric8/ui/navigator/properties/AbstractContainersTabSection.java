/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 *  All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 *
 ******************************************************************************/
package org.fusesource.ide.fabric8.ui.navigator.properties;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySource;
import org.fusesource.ide.commons.Viewers;
import org.fusesource.ide.commons.jobs.Jobs;
import org.fusesource.ide.commons.tree.Refreshables;
import org.fusesource.ide.commons.ui.Selections;
import org.fusesource.ide.commons.ui.Shells;
import org.fusesource.ide.commons.ui.actions.ActionSupport;
import org.fusesource.ide.commons.ui.actions.SeparatorFactory;
import org.fusesource.ide.commons.util.Objects;
import org.fusesource.ide.fabric8.core.dto.ContainerDTO;
import org.fusesource.ide.fabric8.core.dto.ProfileStatusDTO;
import org.fusesource.ide.fabric8.core.dto.VersionDTO;
import org.fusesource.ide.fabric8.ui.FabricPlugin;
import org.fusesource.ide.fabric8.ui.actions.CreateChildContainerAction;
import org.fusesource.ide.fabric8.ui.actions.CreateSshContainerAction;
import org.fusesource.ide.fabric8.ui.actions.Messages;
import org.fusesource.ide.fabric8.ui.actions.jclouds.CreateJCloudsContainerAction;
import org.fusesource.ide.fabric8.ui.navigator.ContainerNode;
import org.fusesource.ide.fabric8.ui.navigator.ContainersNode;
import org.fusesource.ide.fabric8.ui.navigator.Fabric;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public abstract class AbstractContainersTabSection extends ContainerTableView {

	protected static SeparatorFactory separatorFactory = new SeparatorFactory(
			ContainerTableSheetPage.VIEW_ID);

	private Fabric fabric;
	private Object current;
	private Action openTerminalAction;
	private Action startAction;
	private Action stopAction;
	private Action destroyAction;
	private ActionWrapper createContainerAction;
	private ActionWrapper createCloudAction;
	private ActionWrapper createSshAction;
	private CreateChildContainerAction createChildContainerAction;
	private CreateJCloudsContainerAction createCloudContainerAction;
	private CreateSshContainerAction createSshContainerAction;
	private MenuManager setVersionMenu;
	private Separator separator1 = separatorFactory.createSeparator();
	private Separator separator2 = separatorFactory.createSeparator();
	private Separator separator3 = separatorFactory.createSeparator();
	private Separator separator4 = separatorFactory.createSeparator();
	private Runnable refreshRunnable = new Runnable() {
		@Override
		public void run() {
			Viewers.async(new Runnable() {
				@Override
				public void run() {
					if (fabric == null) {
						return;
					}
					final Set<String> selectedIds = getSelectedIds();
					Refreshables.refresh(fabric.getContainersNode());
					updateData();
					Refreshables.refresh(this);
					setSelectedContainerIds(selectedIds);
					updateActionStatus();
				}
			});
		}
	};

	public AbstractContainersTabSection() {
		super(ContainerTableSheetPage.VIEW_ID);
		openTerminalAction = new ActionSupport(Messages.openTerminalLabel,
				Messages.openTerminalToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("terminal_view.gif")) {
			@Override
			public void run() {
				List<ContainerDTO> selectedContainers = getSelectedContainers();
				if (selectedContainers.size() > 0) {
					ContainerDTO container = selectedContainers.get(0);
					if (container != null) {
						ContainerNode
								.openTerminal(getFabric(), container, null);
					}
				}
			}
		};

		startAction = new ActionSupport(Messages.StartAgentAction,
				Messages.StartAgentActionToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("start_task.gif")) {
			@Override
			public void run() {
				start();
			}

			@Override
			public boolean isEnabled() {
				List<ContainerDTO> selectedContainers = getSelectedContainers();
				if (selectedContainers.size() > 0) {
					ContainerDTO container = selectedContainers.get(0);
					if (container != null && container.isRoot()) {
						return false;
					}
				}
				return true;
			}
		};

		stopAction = new ActionSupport(Messages.StopAgentAction,
				Messages.StopAgentActionToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("stop_task.gif")) {
			@Override
			public void run() {
				stop();
			}

			@Override
			public boolean isEnabled() {
				List<ContainerDTO> selectedContainers = getSelectedContainers();
				if (selectedContainers.size() > 0) {
					ContainerDTO container = selectedContainers.get(0);
					if (container != null && container.isRoot()) {
						return false;
					}
				}
				return true;
			}
		};

		destroyAction = new ActionSupport(Messages.DestroyContainerAction,
				Messages.DestroyContainerActionToolTip, FabricPlugin
						.getDefault().getImageDescriptor("delete.gif")) {
			@Override
			public void run() {
				destroy();
			}

			@Override
			public boolean isEnabled() {
				List<ContainerDTO> selectedContainers = getSelectedContainers();
				if (selectedContainers.size() > 0) {
					ContainerDTO container = selectedContainers.get(0);
					if (container != null && container.isRoot()) {
						return false;
					}
				}
				return true;
			}
		};

		setVersionMenu = new MenuManager("Set Version", FabricPlugin
				.getDefault().getImageDescriptor("version.png"),
				"org.fusesource.ide.actions.update.version");
		setVersionMenu.setRemoveAllWhenShown(true);
		setVersionMenu.addMenuListener(new IMenuListener() {

			@Override
			public void menuAboutToShow(IMenuManager manager) {
				List<VersionDTO> versions = getFabric().getFabricService()
						.getVersions();
				Set<String> selectedVersionNames = getSelectedVersionNames();
				for (final VersionDTO version : versions) {
					Action action = new Action(version.getId()) {
						@Override
						public void run() {
							setSelectedContainersVersion(version);
						}

					};
					action.setEnabled(hasVersionApartFrom(selectedVersionNames,
							version));
					setVersionMenu.add(action);
				}
			}
		});
		createContainerAction = new ActionWrapper(
				Messages.createChildAgentMenuLabel,
				Messages.createChildAgentToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("add_obj.gif")) {
			@Override
			protected Action getDelegate() {
				return createChildContainerAction;
			}
		};
		createCloudAction = new ActionWrapper(
				Messages.createJCloudsAgentMenuLabel,
				Messages.createJCloudsAgentToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("new_cloud_container.png")) {
			@Override
			protected Action getDelegate() {
				return createCloudContainerAction;
			}
		};
		createSshAction = new ActionWrapper(Messages.createSshAgentMenuLabel,
				Messages.createSshAgentToolTip, FabricPlugin.getDefault()
						.getImageDescriptor("new_ssh_container.png")) {
			@Override
			protected Action getDelegate() {
				return createSshContainerAction;
			}
		};
	}

	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		final Object containers = Selections.getFirstSelection(selection);
		if (containers == current) {
			return;
		}
		if (fabric != null) {
			fabric.removeFabricUpdateRunnable(refreshRunnable);
		}
		current = containers;
		fabric = getFabricForNode(containers);
		if (fabric != null) {
			fabric.addFabricUpdateRunnable(refreshRunnable);
		}
		
        Job loadJob = new Job("Loading " + fabric.toString() + " data...") {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				final List<?> propertySources = getPropertySourcesForNode(containers);
		        Display.getDefault().asyncExec(new Runnable() {
					
					@Override
					public void run() {
						setPropertySources(propertySources);
						if (getViewer() == null)
							createViewer();
						getViewer().setInput(propertySources);
						recreateColumns();
						getViewer().refresh(true);
						createChildContainerAction = createChildContainerAction(current);
						createCloudContainerAction = createCloudContainerAction(current);
						createSshContainerAction = createSshContainerAction(current);
						updateActionStatus();
					}
				});
		        return Status.OK_STATUS;
			}
		}; 
		loadJob.schedule();
	}

	protected abstract Fabric getFabricForNode(Object node);

	protected abstract List<?> getPropertySourcesForNode(Object node);

	@Override
	protected void recreateColumns() {
		if (current != null) {
			super.recreateColumns();
		}
	}

	@Override
	public void dispose() {
		if (fabric != null) {
			fabric.removeFabricUpdateRunnable(refreshRunnable);
		}
		super.dispose();
	}

	protected void stop() {
		final List<ContainerDTO> containers = getSelectedContainers();
		String message = Objects.makeString("Stopping ", ", ", "",
				getSelectedIds());
		Jobs.schedule(message, new Runnable() {
			@Override
			public void run() {
				for (ContainerDTO container : containers) {
					if (container.isAlive()) {
						container.stop();
					}
				}
				Display.getDefault().syncExec(new Runnable() {
					@Override
					public void run() {
						fabric.getContainersNode().refresh();
						updateData();
						refresh();
					}
				});
			}
		});
	}

	protected void start() {
		final List<ContainerDTO> containers = getSelectedContainers();
		String message = Objects.makeString("Starting ", ", ", "",
				getSelectedIds());
		Jobs.schedule(message, new Runnable() {

			@Override
			public void run() {
				for (ContainerDTO container : containers) {
					if (!container.isAlive()) {
						container.start();
					}
				}
				Display.getDefault().syncExec(new Runnable() {
					@Override
					public void run() {
						fabric.getContainersNode().refresh();
						updateData();
						refresh();
					}
				});
			}
		});
	}

	protected void destroy() {
		final List<ContainerDTO> containers = getSelectedContainers();
		boolean confirm = MessageDialog
				.openConfirm(
						Shells.getShell(),
						"Destroy Container(s)",
						Objects.makeString(
								"Do you really want to destroy the selected container(s) ",
								", ",
								"?\nThis will terminate the container process and removes it from Fabric!",
								getSelectedIds()));
		if (confirm) {
			Jobs.schedule(Objects.makeString("Destroying container(s) ", ", ",
					"", getSelectedIds()), new Runnable() {

				@Override
				public void run() {
					for (ContainerDTO container : containers) {
						container.destroy();
					}
					Display.getDefault().syncExec(new Runnable() {
						@Override
						public void run() {
							fabric.getContainersNode().refresh();
							updateData();
							refresh();
						}
					});
				}
			});
		}
	}

	public Fabric getFabric() {
		return fabric;
	}

	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);

		addLocalMenuActions(separator1, openTerminalAction, startAction,
				stopAction, destroyAction, separator2, setVersionMenu,
				separator3, createContainerAction, createCloudAction,
				createSshAction, separator4);

		addToolBarActions(openTerminalAction, setVersionMenu, startAction,
				stopAction, destroyAction);

		setDoubleClickAction(new Action() {
			@Override
			public void run() {
				if (fabric == null) {
					return;
				}
				ContainersNode containersNode = fabric.getContainersNode();
				if (containersNode != null) {
					List<ContainerDTO> selectedContainers = getSelectedContainers();
					if (!selectedContainers.isEmpty()) {
						ContainerDTO container = selectedContainers.get(0);
						ContainerNode containerNode = containersNode
								.getContainerNode(container.getId());
						if (containerNode != null) {
							Selections.setSingleSelection(
									fabric.getRefreshableUI(), containerNode);
						}
					}
				}
			}

		});

		if (getViewer() == null) {
			createViewer();
		}

		getViewer().addSelectionChangedListener(
				new ISelectionChangedListener() {
					@Override
					public void selectionChanged(SelectionChangedEvent event) {
						updateActionStatus();
					}
				});
	}

	protected int countStarted(List<ContainerDTO> containers, boolean flag) {
		int counter = 0;
		for (ContainerDTO container : containers) {
			boolean alive = container.isAlive();
			if (alive == flag) {
				counter++;
			}
		}
		return counter;
	}

	protected List<ContainerDTO> getSelectedContainers() {
		List<ContainerDTO> containers = new ArrayList<ContainerDTO>();
		IStructuredSelection selection = getSelection();
		if (selection != null) {
			boolean changed = false;
			Iterator iterator = selection.iterator();
			while (iterator.hasNext()) {
				ContainerDTO container = ContainerNode
						.toContainer(iterator.next());
				if (container != null) {
					containers.add(container);
				}
			}
		}
		return containers;
	}

	protected IStructuredSelection getSelection() {
		return Selections.getStructuredSelection(getViewer());
	}

	protected Set<String> getSelectedVersionNames() {
		Set<String> answer = new HashSet<String>();
		List<ContainerDTO> containers = getSelectedContainers();
		for (ContainerDTO container : containers) {
			String name = container.getVersionId();
			if (name != null) {
				answer.add(name);
			}
		}
		return answer;
	}

	protected Set<String> getSelectedIds() {
		Set<String> answer = new HashSet<String>();
		List<ContainerDTO> containers = getSelectedContainers();
		for (ContainerDTO container : containers) {
			String id = container.getId();
			if (id != null) {
				answer.add(id);
			}
		}
		return answer;
	}

	protected void setSelectedContainerIds(Set<String> selectedIds) {
		TableViewer viewer = getViewer();
		if (viewer != null) {
			List<?> propertySources = getPropertySources();
			List selected = new ArrayList();
			for (Object object : propertySources) {
				if (object instanceof IPropertySource) {
					IPropertySource source = (IPropertySource) object;
					Object value = source.getPropertyValue("id");
					if (value instanceof String
							&& selectedIds.contains(value.toString())) {
						selected.add(object);
					}
				}
			}
			viewer.setSelection(new StructuredSelection(selected));
			if (selected.size() == 1) {
				Object first = selected.get(0);
				viewer.reveal(first);
			}
		}

	}

	protected void setSelectedContainersVersion(VersionDTO version) {
		IStructuredSelection selection = getSelection();
		if (selection != null) {
			boolean changed = false;
			Iterator iterator = selection.iterator();
			while (iterator.hasNext()) {
				ContainerNode agentNode = ContainerNode
						.toContainerNode(iterator.next());
				if (agentNode != null) {
					if (!agentNode.matches(version)) {
						agentNode.getContainer().setVersion(version);
						changed = true;
					}
				}
			}

			if (changed) {
				refresh();
				getFabric().getContainersNode().refresh();
				getFabric().getVersionsNode().refresh();
			}
		}
	}

	protected void updateData() {
		setPropertySources(getPropertySourcesForNode(current));
	}

	protected boolean hasVersionApartFrom(Set<String> names, VersionDTO version) {
		int minSize = names.contains(version.getId()) ? 2 : 1;
		return names.size() >= minSize;
	}

	protected abstract CreateJCloudsContainerAction createCloudContainerAction(
			Object current);

	protected abstract CreateSshContainerAction createSshContainerAction(
			Object current);

	protected abstract CreateChildContainerAction createChildContainerAction(
			Object current);

	protected void updateActionStatus() {
		List<ContainerDTO> selectedContainers = getSelectedContainers();

		ContainerNode containerNode = getSingleSelectedRootContainerNode(selectedContainers);
		createChildContainerAction.setContainerNode(containerNode);
		createChildContainerAction.updateEnabled();

		int selectedContainerSize = selectedContainers.size();
		openTerminalAction.setEnabled(selectedContainerSize > 0);

		boolean isRootContainerSelected = false;
		if (selectedContainers.size() > 0 && selectedContainers.get(0).isRoot()) {
			isRootContainerSelected = true;
		}
		startAction.setEnabled(!isRootContainerSelected);
		stopAction.setEnabled(!isRootContainerSelected);
		destroyAction.setEnabled(!isRootContainerSelected);
	}

	protected ContainerNode getSingleSelectedRootContainerNode(
			List<ContainerDTO> selectedContainers) {
		ArrayList<ContainerDTO> rootContainers = Lists.newArrayList(Iterables
				.filter(selectedContainers, new Predicate<ContainerDTO>() {

					@Override
					public boolean apply(ContainerDTO container) {
						return container != null && container.isRoot();
					}
				}));

		if (rootContainers.size() == 1 && fabric != null) {
			ContainerDTO rootContainer = rootContainers.get(0);
			ContainersNode containersNode = fabric.getContainersNode();
			if (containersNode != null) {
				return containersNode.getContainerNode(rootContainer.getId());
			}
		}
		return null;
	}

	private abstract static class ActionWrapper extends ActionSupport {

		public ActionWrapper(String text, String tooltip, ImageDescriptor image) {
			super(text, tooltip, image);
		}

		@Override
		public boolean isEnabled() {
			return getDelegate() != null && getDelegate().isEnabled();
		}

		@Override
		public void run() {
			if (getDelegate() == null) {
				return;
			}
			getDelegate().run();
		}

		protected abstract Action getDelegate();
	}
}
