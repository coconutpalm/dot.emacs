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

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;

/**
 * @author Eric Bordeau
 */
public class CopyRetargetAction {
	
	public static IWorkbenchAction copyRetargetAction(IWorkbenchWindow workbenchWindow) {
		return ActionFactory.COPY.create(workbenchWindow);
	}

	/**
	 * Constructs a new CopyRetargetAction with the default ID, label and image.
	 */
	public CopyRetargetAction() {
		throw new IllegalStateException("Use CopyRetargetAction.create(IWorkbenchWindow) instead.");
	}

}
