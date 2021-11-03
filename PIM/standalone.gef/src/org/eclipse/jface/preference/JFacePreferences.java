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
package org.eclipse.jface.preference;

/**
 *
 * JFacePreferences is a class used to administer the preferences used by JFace
 * objects.
 */
public final class JFacePreferences {

	/**
	 * Identifier for the Error Color
	 */
	public static final String ERROR_COLOR = "ERROR_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the Hyperlink Color
	 */
	public static final String HYPERLINK_COLOR = "HYPERLINK_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the Active Hyperlink Colour
	 */
	public static final String ACTIVE_HYPERLINK_COLOR = "ACTIVE_HYPERLINK_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the color used to show extra informations in labels, as a
	 * qualified name. For example in 'Foo.txt - myproject/bar', the qualifier
	 * is '- myproject/bar'.
	 *
	 * @since 3.4
	 */
	public static final String QUALIFIER_COLOR = "QUALIFIER_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the color used to show label decorations For example in
	 * 'Foo.txt [1.16]', the decoration is '[1.16]'.
	 *
	 * @since 3.4
	 */
	public static final String DECORATIONS_COLOR = "DECORATIONS_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the color used to counter informations For example in
	 * 'Foo.txt (2 matches)', the counter information is '(2 matches)'.
	 *
	 * @since 3.4
	 */
	public static final String COUNTER_COLOR = "COUNTER_COLOR"; //$NON-NLS-1$


	/**
	 * Identifier for the color used for the background of content assist
	 * popup dialogs.
	 *
	 * @since 3.4
	 */
	public static final String CONTENT_ASSIST_BACKGROUND_COLOR = "CONTENT_ASSIST_BACKGROUND_COLOR"; //$NON-NLS-1$

	/**
	 * Identifier for the color used for the foreground of content assist
	 * popup dialogs.
	 *
	 * @since 3.4
	 */
	public static final String CONTENT_ASSIST_FOREGROUND_COLOR = "CONTENT_ASSIST_FOREGROUND_COLOR"; //$NON-NLS-1$

	/**
	 * The color used for the background of controls that provide information
	 * for reading. E.g. Hover boxes with information like javadoc or view parts.
	 *
	 * @since 3.14
	 */
	public static final String INFORMATION_BACKGROUND_COLOR = "org.eclipse.ui.workbench.INFORMATION_BACKGROUND"; //$NON-NLS-1$

	/**
	 * The color used for text for controls that provide information for
	 * reading. E.g. Hover boxes with information like javadoc or view parts.
	 *
	 * @since 3.14
	 */
	public static final String INFORMATION_FOREGROUND_COLOR = "org.eclipse.ui.workbench.INFORMATION_FOREGROUND"; //$NON-NLS-1$

	/**
	 * The color used for the background of the newest revision when revisions are
	 * shown in a ruler. Together with {@link #REVISION_OLDEST_COLOR} a gradient is
	 * defined.
	 *
	 * @since 3.19
	 */
	public static final String REVISION_NEWEST_COLOR = "org.eclipse.jface.REVISION_NEWEST_COLOR"; //$NON-NLS-1$

	/**
	 * The color used for the background of the oldest revision when revisions are
	 * shown in a ruler. Together with {@link #REVISION_NEWEST_COLOR} a gradient is
	 * defined.
	 *
	 * @since 3.19
	 */
	public static final String REVISION_OLDEST_COLOR = "org.eclipse.jface.REVISION_OLDEST_COLOR"; //$NON-NLS-1$

	private static IPreferenceStore preferenceStore;

	/**
	 * Prevent construction.
	 */
	private JFacePreferences() {
	}

	/**
	 * Return the preference store for the receiver.
	 *
	 * @return IPreferenceStore or null
	 */
	public static IPreferenceStore getPreferenceStore() {
		return preferenceStore;
	}

	/**
	 * Set the preference store for the receiver.
	 *
	 * @param store
	 *            IPreferenceStore
	 */
	public static void setPreferenceStore(IPreferenceStore store) {
		preferenceStore = store;
	}

}
