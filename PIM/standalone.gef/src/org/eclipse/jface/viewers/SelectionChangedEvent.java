/*******************************************************************************
 * Copyright (c) 2000, 2015 IBM Corporation and others.
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
 *******************************************************************************/
package org.eclipse.jface.viewers;

import java.util.EventObject;

import org.eclipse.core.runtime.Assert;

/**
 * Event object describing a selection change. The source of these
 * events is a selection provider.
 *
 * @see ISelection
 * @see ISelectionProvider
 * @see ISelectionChangedListener
 */
public class SelectionChangedEvent extends EventObject {

	/**
	 * Generated serial version UID for this class.
	 * @since 3.1
	 */
	private static final long serialVersionUID = 3835149545519723574L;

	/**
	 * The selection.
	 */
	protected ISelection selection;

	/**
	 * Creates a new event for the given source and selection.
	 *
	 * @param source the selection provider
	 * @param selection the selection
	 */
	public SelectionChangedEvent(ISelectionProvider source, ISelection selection) {
		super(source);
		Assert.isNotNull(selection);
		this.selection = selection;
	}

	/**
	 * Returns the selection.
	 *
	 * @return the selection
	 */
	public ISelection getSelection() {
		return selection;
	}

	/**
	 * Returns the selection.
	 *
	 * @return IStructuredSelection
	 * @throws ClassCastException
	 *             if the selection is not an instance of IStructuredSelection
	 * @since 3.13
	 */
	public IStructuredSelection getStructuredSelection() throws ClassCastException {
		ISelection selection = getSelection();
		if (selection instanceof IStructuredSelection) {
			return (IStructuredSelection) selection;
		}
		throw new ClassCastException(
				"ISelection is not an instance of IStructuredSelection."); //$NON-NLS-1$
	}

	/**
	 * Returns the selection provider that is the source of this event.
	 *
	 * @return the originating selection provider
	 */
	public ISelectionProvider getSelectionProvider() {
		return (ISelectionProvider) getSource();
	}
}
