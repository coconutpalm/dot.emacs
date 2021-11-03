/*******************************************************************************
 * Copyright (c) 2003, 2015 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Tobias Baumann <tobbaumann@gmail.com> - Bug 428323 - Correct lock tool bar action definition
 *******************************************************************************/
package org.eclipse.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.events.HelpListener;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.SharedImages;
import org.eclipse.ui.part.WorkbenchMessages;

/**
 * Access to standard actions provided by the workbench.
 * <p>
 * Most of the functionality of this class is provided by static methods and
 * fields. Example usage:
 * </p>
 *
 * <pre>
 * MenuManager menu = ...;
 * ActionFactory.IWorkbenchAction closeEditorAction = ActionFactory.CLOSE.create(window);
 * menu.add(closeEditorAction);
 * </pre>
 * <p>
 * Clients may declare other classes that provide additional
 * application-specific action factories.
 * </p>
 *
 * @since 3.0
 */
public abstract class ActionFactory {

	/**
	 * Interface for a workbench action.
	 */
	public interface IWorkbenchAction extends IAction {
		/**
		 * Disposes of this action. Once disposed, this action cannot be used. This
		 * operation has no effect if the action has already been disposed.
		 */
		void dispose();
	}
	
	private static class WorkbenchCommandAction extends CommandAction implements IWorkbenchAction {
		/**
		 * Creates the action backed by a command. For commands that don't take
		 * parameters.
		 *
		 * @param commandIdIn the command id. Must not be null.
		 * @param window      the workbench window
		 */
		public WorkbenchCommandAction(String commandIdIn, IWorkbenchWindow window) {
			super(window, commandIdIn);
		}

		@Override
		public void addPropertyChangeListener(IPropertyChangeListener listener) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public int getAccelerator() {
			// TODO Auto-generated method stub
			return 0;
		}

		@Override
		public String getActionDefinitionId() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public String getDescription() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public ImageDescriptor getDisabledImageDescriptor() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public HelpListener getHelpListener() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public ImageDescriptor getHoverImageDescriptor() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public String getId() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public ImageDescriptor getImageDescriptor() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public IMenuCreator getMenuCreator() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public int getStyle() {
			// TODO Auto-generated method stub
			return 0;
		}

		@Override
		public String getText() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public String getToolTipText() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public boolean isChecked() {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean isEnabled() {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean isHandled() {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public void removePropertyChangeListener(IPropertyChangeListener listener) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void run() {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void runWithEvent(Event event) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setActionDefinitionId(String id) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setChecked(boolean checked) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setDescription(String text) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setDisabledImageDescriptor(ImageDescriptor newImage) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setEnabled(boolean enabled) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setHelpListener(HelpListener listener) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setHoverImageDescriptor(ImageDescriptor newImage) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setId(String id) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setImageDescriptor(ImageDescriptor newImage) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setMenuCreator(IMenuCreator creator) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setText(String text) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setToolTipText(String text) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void setAccelerator(int keycode) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void dispose() {
			// TODO Auto-generated method stub
			
		}
	}

	/**
	 * Workbench action (id: "back", commandId: "org.eclipse.ui.navigate.back"):
	 * Back. This action is a {@link RetargetAction} with id "back". This action
	 * maintains its enablement state.
	 */
	public static final ActionFactory BACK = new ActionFactory("back", //$NON-NLS-1$
			IWorkbenchCommandConstants.NAVIGATE_BACK) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_back);
			action.setToolTipText(WorkbenchMessages.Workbench_backToolTip);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};

	/**
	 * Workbench action (id: "print").
	 * This action is a {@link RetargetAction} with id "print". This action
	 * maintains its enablement state.
	 */
	public static final ActionFactory PRINT = new ActionFactory("print", //$NON-NLS-1$
			IWorkbenchCommandConstants.NAVIGATE_PRINT) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_print);
			action.setToolTipText(WorkbenchMessages.Workbench_printToolTip);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};

	/**
	 * Workbench action (id: "close", commandId: "org.eclipse.ui.file.close"): Close
	 * the active editor. This action maintains its enablement state.
	 */
	public static final ActionFactory CLOSE = new ActionFactory("close", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_CLOSE) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			WorkbenchCommandAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setId(getId());
			action.setText(WorkbenchMessages.CloseEditorAction_text);
			action.setToolTipText(WorkbenchMessages.CloseEditorAction_toolTip);
			return action;
		}
	};

	/**
	 * Workbench action (id: "closeAll", commandId: "org.eclipse.ui.file.closeAll"):
	 * Close all open editors. This action maintains its enablement state.
	 */
	public static final ActionFactory CLOSE_ALL = new ActionFactory("closeAll", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_CLOSE_ALL) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			WorkbenchCommandAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setId(getId());
			action.setText(WorkbenchMessages.CloseAllAction_text);
			action.setToolTipText(WorkbenchMessages.CloseAllAction_toolTip);
			return action;
		}
	};

	/**
	 * Workbench action (id: "closeOthers", commandId:
	 * "org.eclipse.ui.file.closeOthers"): Close all editors except the one that is
	 * active. This action maintains its enablement state.
	 *
	 * @since 3.2
	 */
	public static final ActionFactory CLOSE_OTHERS = new ActionFactory("closeOthers", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_CLOSE_OTHERS) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			WorkbenchCommandAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setId(getId());
			action.setText(WorkbenchMessages.CloseOthersAction_text);
			action.setToolTipText(WorkbenchMessages.CloseOthersAction_toolTip);
			return action;
		}
	};

	/**
	 * Workbench action (id: "copy", commandId: "org.eclipse.ui.edit.copy"): Copy.
	 * This action is a {@link RetargetAction} with id "copy". This action maintains
	 * its enablement state.
	 */
	public static final ActionFactory COPY = new ActionFactory("copy", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_COPY) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_copy);
			action.setToolTipText(WorkbenchMessages.Workbench_copyToolTip);
			action.setActionDefinitionId(getCommandId());
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED));
			return action;
		}
	};

	/**
	 * Workbench action (id: "cut", commandId: "org.eclipse.ui.edit.cut"): Cut. This
	 * action is a {@link RetargetAction} with id "cut". This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory CUT = new ActionFactory("cut", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_CUT) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_cut);
			action.setToolTipText(WorkbenchMessages.Workbench_cutToolTip);
			action.setActionDefinitionId(getCommandId());
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_CUT_DISABLED));
			return action;
		}
	};

	/**
	 * Workbench action (id: "cut", commandId: "org.eclipse.ui.edit.cut"): Cut. This
	 * action is a {@link RetargetAction} with id "cut". This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory PASTE = new ActionFactory("cut", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_PASTE) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_paste);
			action.setToolTipText(WorkbenchMessages.Workbench_pasteToolTip);
			action.setActionDefinitionId(getCommandId());
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_PASTE));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_PASTE_DISABLED));
			return action;
		}
	};

	/**
	 * Workbench action (id: "delete", commandId: "org.eclipse.ui.edit.delete"):
	 * Delete. This action is a {@link RetargetAction} with id "delete". This action
	 * maintains its enablement state.
	 */
	public static final ActionFactory DELETE = new ActionFactory("delete", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_DELETE) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_delete);
			action.setToolTipText(WorkbenchMessages.Workbench_deleteToolTip);
			action.setActionDefinitionId(getCommandId());
			action.enableAccelerator(false);
//			window.getWorkbench().getHelpSystem().setHelp(action, IWorkbenchHelpContextIds.DELETE_RETARGET_ACTION);
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE_DISABLED));
			return action;
		}
	};
	/**
	 * Workbench action (id: "find", commandId: "org.eclipse.ui.edit.findReplace"):
	 * Find. This action is a {@link RetargetAction} with id "find". This action
	 * maintains its enablement state.
	 */
	public static final ActionFactory FIND = new ActionFactory("find", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_FIND_AND_REPLACE) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_findReplace);
			action.setToolTipText(WorkbenchMessages.Workbench_findReplaceToolTip);
			action.setActionDefinitionId(getCommandId());
			// Find's images are commented out due to a conflict with Search.
			// See bug 16412.
			// action.setImageDescriptor(WorkbenchImages.getImageDescriptor(IWorkbenchGraphicConstants.IMG_ETOOL_SEARCH_SRC));
			// action.setDisabledImageDescriptor(WorkbenchImages.getImageDescriptor(IWorkbenchGraphicConstants.IMG_ETOOL_SEARCH_SRC_DISABLED));
			return action;
		}
	};

	/**
	 * Workbench action (id: "forward", commandId:
	 * "org.eclipse.ui.navigate.forward"): Forward. This action is a
	 * {@link RetargetAction} with id "forward". This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory FORWARD = new ActionFactory("forward", //$NON-NLS-1$
			IWorkbenchCommandConstants.NAVIGATE_FORWARD) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_forward);
			action.setToolTipText(WorkbenchMessages.Workbench_forwardToolTip);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};
	/**
	 * Workbench action (id: "goInto", commandId: "org.eclipse.ui.navigate.goInto"):
	 * Go Into. This action is a {@link RetargetAction} with id "goInto". This
	 * action maintains its enablement state.
	 */
	public static final ActionFactory GO_INTO = new ActionFactory("goInto", //$NON-NLS-1$
			IWorkbenchCommandConstants.NAVIGATE_GO_INTO) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_goInto);
			action.setToolTipText(WorkbenchMessages.Workbench_goIntoToolTip);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};
	/**
	 * Workbench action (id: "previousEditor", commandId:
	 * "org.eclipse.ui.window.previousEditor"): Previous editor. This action
	 * maintains its enablement state.
	 * <p>
	 * <code>NEXT_EDITOR</code> and <code>PREVIOUS_EDITOR</code> form a cycle action
	 * pair. For a given window, use {@link ActionFactory#linkCycleActionPair
	 * ActionFactory.linkCycleActionPair} to connect the two.
	 * </p>
	 */
	public static final ActionFactory PREVIOUS_EDITOR = new ActionFactory("previousEditor", //$NON-NLS-1$
			IWorkbenchCommandConstants.WINDOW_PREVIOUS_EDITOR) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			IWorkbenchAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setId(getId());
			action.setText(WorkbenchMessages.CycleEditorAction_prev_text);
			action.setToolTipText(WorkbenchMessages.CycleEditorAction_prev_toolTip);
			// @issue missing action ids
//			window.getWorkbench().getHelpSystem().setHelp(action,
//					IWorkbenchHelpContextIds.CYCLE_EDITOR_BACKWARD_ACTION);

			return action;
		}
	};

	/**
	 * Workbench action (id: "properties", commandId:
	 * "org.eclipse.ui.file.properties"): Properties. This action is a
	 * {@link RetargetAction} with id "properties". This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory PROPERTIES = new ActionFactory("properties", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_PROPERTIES) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_properties);
			action.setToolTipText(WorkbenchMessages.Workbench_propertiesToolTip);
//			window.getPartService().addPartListener(action);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};
	/**
	 * Workbench action (id: "redo", commandId: "org.eclipse.ui.edit.redo"): Redo.
	 * This action is a {@link RetargetAction} with id "redo". This action maintains
	 * its enablement state.
	 */
	public static final ActionFactory REDO = new ActionFactory("redo", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_REDO) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			LabelRetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_redo);
			action.setToolTipText(WorkbenchMessages.Workbench_redoToolTip);
//			window.getPartService().addPartListener(action);
			action.setActionDefinitionId(getCommandId());
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_REDO));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_REDO_DISABLED));
			return action;
		}
	};
	/**
	 * Workbench action (id: "save", commandId: "org.eclipse.ui.file.save"): Save
	 * the active editor. This action maintains its enablement state.
	 */
	public static final ActionFactory SAVE = new ActionFactory("save", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_SAVE) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			WorkbenchCommandAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setText(WorkbenchMessages.SaveAction_text);
			action.setToolTipText(WorkbenchMessages.SaveAction_toolTip);
			action.setId(getId());
//			window.getWorkbench().getHelpSystem().setHelp(action, IWorkbenchHelpContextIds.SAVE_ACTION);
			return action;
		}
	};

	/**
	 * Workbench action (id: "saveAll", commandId: "org.eclipse.ui.file.saveAll"):
	 * Save all open editors with unsaved changes. This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory SAVE_ALL = new ActionFactory("saveAll", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_SAVE_ALL) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			IWorkbenchAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setText(WorkbenchMessages.SaveAll_text);
			action.setToolTipText(WorkbenchMessages.SaveAll_toolTip);
//			window.getWorkbench().getHelpSystem().setHelp(action, IWorkbenchHelpContextIds.SAVE_ALL_ACTION);
			action.setId(getId());
			return action;
		}
	};

	/**
	 * Workbench action (id: "saveAs", commandId: "org.eclipse.ui.file.saveAs"):
	 * Save As for the active editor. This action maintains its enablement state.
	 */
	public static final ActionFactory SAVE_AS = new ActionFactory("saveAs", //$NON-NLS-1$
			IWorkbenchCommandConstants.FILE_SAVE_AS) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			IWorkbenchAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setText(WorkbenchMessages.SaveAs_text);
			action.setToolTipText(WorkbenchMessages.SaveAs_toolTip);
//			window.getWorkbench().getHelpSystem().setHelp(action, IWorkbenchHelpContextIds.SAVE_AS_ACTION);
			action.setId(getId());
			return action;
		}
	};

	/**
	 * Workbench action (id: "selectAll", commandId:
	 * "org.eclipse.ui.edit.selectAll"): Select All. This action is a
	 * {@link RetargetAction} with id "selectAll". This action maintains its
	 * enablement state.
	 */
	public static final ActionFactory SELECT_ALL = new ActionFactory("selectAll", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_SELECT_ALL) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			RetargetAction action = new RetargetAction(getId(), WorkbenchMessages.Workbench_selectAll);
			action.setToolTipText(WorkbenchMessages.Workbench_selectAllToolTip);
//			window.getPartService().addPartListener(action);
			action.setActionDefinitionId(getCommandId());
			return action;
		}
	};

	/**
	 * Workbench action (id: "undo", commandId: "org.eclipse.ui.edit.undo"): Undo.
	 * This action is a {@link RetargetAction} with id "undo". This action maintains
	 * its enablement state.
	 */
	public static final ActionFactory UNDO = new ActionFactory("undo", //$NON-NLS-1$
			IWorkbenchCommandConstants.EDIT_UNDO) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}
			LabelRetargetAction action = new LabelRetargetAction(getId(), WorkbenchMessages.Workbench_undo);
			action.setToolTipText(WorkbenchMessages.Workbench_undoToolTip);
//			window.getPartService().addPartListener(action);
			action.setActionDefinitionId(getCommandId());
			ISharedImages sharedImages = SharedImages.get();
			action.setImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_UNDO));
			action.setDisabledImageDescriptor(sharedImages.getImageDescriptor(ISharedImages.IMG_TOOL_UNDO_DISABLED));
			return action;
		}
	};

	/**
	 * Workbench action (id: "newEditor", commandId:
	 * "org.eclipse.ui.window.newEditor"): Open a new editor on the active editor's
	 * input. This action maintains its enablement state.
	 *
	 * @since 3.1
	 */
	public static final ActionFactory NEW_EDITOR = new ActionFactory("newEditor", //$NON-NLS-1$
			IWorkbenchCommandConstants.WINDOW_NEW_EDITOR) {

		@Override
		public IWorkbenchAction create(IWorkbenchWindow window) {
			if (window == null) {
				throw new IllegalArgumentException();
			}

			WorkbenchCommandAction action = new WorkbenchCommandAction(getCommandId(), window);
			action.setId(getId());
			action.setText(WorkbenchMessages.NewEditorAction_text);
			action.setToolTipText(WorkbenchMessages.NewEditorAction_tooltip);

			return action;
		}
	};


	/**
	 * Establishes bi-direction connections between the forward and backward actions
	 * of a cycle pair.
	 * <p>
	 * Example usage:
	 * </p>
	 *
	 * <pre>
	 * ActionFactory.IWorkbenchAction nextEditorAction = ActionFactory.NEXT_EDITOR.create(window);
	 * ActionFactory.IWorkbenchAction previousEditorAction = ActionFactory.PREVIOUS_EDITOR.create(window);
	 * ActionFactory.linkCycleActionPair(nextEditorAction, previousEditorAction);
	 * </pre>
	 *
	 * @param next     the action that moves forward
	 * @param previous the action that moves backward
	 */
	public static void linkCycleActionPair(IWorkbenchAction next, IWorkbenchAction previous) {
	}

	/**
	 * Id of actions created by this action factory.
	 */
	private final String actionId;

	/**
	 * Optional ID for this action.
	 */
	private final String commandId;

	/**
	 * Creates a new workbench action factory with the given id.
	 *
	 * @param actionId the id of actions created by this action factory
	 */
	protected ActionFactory(String actionId) {
		this(actionId, null);
	}

	/**
	 * Create a new workbench action factory with the given IDs.
	 *
	 * @param actionId  the id of actions created by this action factory
	 * @param commandId the matching command id
	 * @since 3.5
	 */
	protected ActionFactory(String actionId, String commandId) {
		this.actionId = actionId;
		this.commandId = commandId;
	}

	/**
	 * Creates a new standard action for the given workbench window. The action has
	 * an id as specified by the particular factory.
	 * <p>
	 * Actions automatically register listeners against the workbench window so that
	 * they can keep their enablement state up to date. Ordinarily, the window's
	 * references to these listeners will be dropped automatically when the window
	 * closes. However, if the client needs to get rid of an action while the window
	 * is still open, the client must call {@link IWorkbenchAction#dispose
	 * dispose}to give the action an opportunity to deregister its listeners and to
	 * perform any other cleanup.
	 * </p>
	 *
	 * @param window the workbench window
	 * @return the workbench action
	 */
	public abstract IWorkbenchAction create(IWorkbenchWindow window);

	/**
	 * Returns the id of this action factory.
	 *
	 * @return the id of actions created by this action factory
	 */
	public String getId() {
		return actionId;
	}

	/**
	 * Return the command id of this action factory.
	 *
	 * @return the command id of the action created by this action factory. May be
	 *         <code>null</code>.
	 * @since 3.5
	 */
	public String getCommandId() {
		return commandId;
	}
}
