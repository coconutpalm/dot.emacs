/*******************************************************************************
 * Copyright (c) 2000, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.gef.ui.actions;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.actions.ActionFactory;

import java.util.Map;

import org.eclipse.gef.internal.GEFMessages;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

/**
 * An action to save the editor's current state.
 */
public class SaveAction extends EditorPartAction {

	/**
	 * Constructs a <code>SaveAction</code> and associates it with the given
	 * editor.
	 * 
	 * @param editor
	 *            the IEditorPart
	 */
	public SaveAction(IEditorPart editor) {
		super(editor);
		setLazyEnablementCalculation(false);
	}

	/**
	 * @see org.eclipse.gef.ui.actions.WorkbenchPartAction#calculateEnabled()
	 */
	protected boolean calculateEnabled() {
		return getEditorPart().isDirty();
	}

	/**
	 * Initializes this action's text.
	 */
	protected void init() {
		setId(ActionFactory.SAVE.getId());
		setText(GEFMessages.SaveAction_Label);
		setToolTipText(GEFMessages.SaveAction_Tooltip);
	}

	/**
	 * Saves the state of the associated editor.
	 */
	public void run() {
//		getEditorPart().getSite().getPage().saveEditor(getEditorPart(), false);
	}

	@Override
	public void addPropertyListener(IPropertyListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void createPartControl(Composite parent) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getTitle() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Image getTitleImage() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getTitleToolTip() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void removePropertyListener(IPropertyListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public <T> T getAdapter(Class<T> adapter) {
		return null;
	}

	@Override
	public String getPartName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getContentDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addPartPropertyListener(IPropertyChangeListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void removePartPropertyListener(IPropertyChangeListener listener) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String getPartProperty(String key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setPartProperty(String key, String value) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Map<String, String> getPartProperties() {
		// TODO Auto-generated method stub
		return null;
	}

}
