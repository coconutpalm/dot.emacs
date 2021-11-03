/*******************************************************************************
 * Copyright (c) 2004, 2015 IBM Corporation and others.
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

import org.eclipse.swt.graphics.Color;

/**
 * The IColorDecorator is an interface for objects that return a color to
 * decorate either the foreground and background colors for displaying an
 * an object.
 *
 * If an IColorDecorator decorates a foreground or background in an object
 * that also has an IColorProvider the IColorDecorator will take precedence.
 * @see IColorProvider
 *
 * @since 3.1
 */
public interface IColorDecorator {

	/**
	 * Return the foreground Color for element or <code>null</code> if there
	 * is not one.
	 * @param element the element to get foreground color for
	 * @return Color or <code>null</code>
	 */
	public Color decorateForeground(Object element);

	/**
	 * Return the background Color for element or <code>null</code> if there
	 * is not one.
	 * @param element the element to get background color for
	 * @return Color or <code>null</code>
	 */
	public Color decorateBackground(Object element);

}
