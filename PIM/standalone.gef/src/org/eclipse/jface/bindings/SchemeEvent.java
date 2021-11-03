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

package org.eclipse.jface.bindings;

/**
 * An instance of this class describes changes to an instance of
 * <code>IScheme</code>.
 * <p>
 * This class is not intended to be extended by clients.
 * </p>
 *
 * @since 3.1
 * @see ISchemeListener#schemeChanged(SchemeEvent)
 */
public final class SchemeEvent {

	/**
	 * The bit used to represent whether the particular field has changed.
	 */
	private static final int CHANGED_PARENT_ID = 1;
	private static final int CHANGED_NAME = (1 << 1);
	private static final int CHANGED_DESCRIPTION = (1 << 2);
	private static final int CHANGED_DEFINED = (1 << 3);
	
	/**
	 * A bit mask indicating what values in the Scheme changed.
	 */
	public final int changedValues;

	/**
	 * The scheme that has changed; this value is never <code>null</code>.
	 */
	public final Scheme scheme;

	/**
	 * Creates a new instance of this class.
	 *
	 * @param scheme
	 *            the instance of the interface that changed; must not be
	 *            <code>null</code>.
	 * @param definedChanged
	 *            true, iff the defined property changed.
	 * @param nameChanged
	 *            true, iff the name property changed.
	 * @param descriptionChanged
	 *            <code>true</code> if the description property changed;
	 *            <code>false</code> otherwise.
	 * @param parentIdChanged
	 *            true, iff the parentId property changed.
	 */
	public SchemeEvent(Scheme scheme, boolean definedChanged,
			boolean nameChanged, boolean descriptionChanged,
			boolean parentIdChanged) {

		if (scheme == null) {
			throw new NullPointerException();
		}
		this.scheme = scheme;

		int changedFields=0;
		if (parentIdChanged) {
			changedFields |= CHANGED_PARENT_ID;
		}
		if (nameChanged) {
			changedFields |= CHANGED_NAME;
		}
		if (descriptionChanged) {
			changedFields |= CHANGED_DESCRIPTION;
		}
		if (definedChanged) {
			changedFields |= CHANGED_DEFINED;
		}
		changedValues=changedFields;
	}

}
