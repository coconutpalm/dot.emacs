/*******************************************************************************
 * Copyright (c) 2005, 2016 IBM Corporation and others.
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

package org.eclipse.core.commands.common;

import org.eclipse.core.runtime.ListenerList;

/**
 * <p>
 * A manager to which listeners can be attached. This handles the management of
 * a list of listeners -- optimizing memory and performance. All the methods on
 * this class are guaranteed to be thread-safe.
 * </p>
 * <p>
 * Clients may extend.
 * </p>
 * <p>
 * <b>Warning:</b> Do not use this class! Use {@link ListenerList} directly. See
 * <a href="https://bugs.eclipse.org/486067">bug 486067</a>.
 * </p>
 *
 * @since 3.2
 */
public abstract class EventManager {

	/**
	 * An empty array that can be returned from a call to
	 * {@link #getListeners()} when {@link #listenerList} is <code>null</code>.
	 */
	private static final Object[] EMPTY_ARRAY = new Object[0];

	/**
	 * A collection of objects listening to changes to this manager. This
	 * collection is <code>null</code> if there are no listeners.
	 */
	private volatile transient ListenerList<Object> listenerList = null;

	/**
	 * Adds a listener to this manager that will be notified when this manager's
	 * state changes. This method has no effect if the same listener is already
	 * registered.
	 *
	 * @param listener
	 *            The listener to be added; must not be <code>null</code>.
	 */
	protected synchronized final void addListenerObject(final Object listener) {
		if (listener == null) {
			throw new IllegalArgumentException();
		}

		if (listenerList == null) {
			listenerList = new ListenerList<>(ListenerList.IDENTITY);
		}

		listenerList.add(listener);
	}

	/**
	 * Clears all of the listeners from the listener list.
	 */
	protected final void clearListeners() {
		listenerList = null;
	}

	/**
	 * Returns an array containing all the listeners attached to this event
	 * manager. The resulting array is unaffected by subsequent adds or removes.
	 * If there are no listeners registered, the result is an empty array. Use
	 * this method when notifying listeners, so that any modifications to the
	 * listener list during the notification will have no effect on the
	 * notification itself.
	 * <p>
	 * Note: Callers of this method <b>must not</b> modify the returned array.
	 * </p>
	 *
	 * @return The listeners currently attached; may be empty, but never
	 *         <code>null</code>
	 */
	protected final Object[] getListeners() {
		final ListenerList<Object> list = listenerList;
		if (list == null) {
			return EMPTY_ARRAY;
		}

		return list.getListeners();
	}

	/**
	 * Whether one or more listeners are attached to the manager.
	 *
	 * @return <code>true</code> if listeners are attached to the manager;
	 *         <code>false</code> otherwise.
	 */
	protected final boolean isListenerAttached() {
		return listenerList != null;
	}

	/**
	 * Removes a listener from this manager. Has no effect if the same listener
	 * was not already registered.
	 *
	 * @param listener
	 *            The listener to be removed; must not be <code>null</code>.
	 */
	protected synchronized final void removeListenerObject(final Object listener) {
		if (listener == null) {
			throw new IllegalArgumentException();
		}

		if (listenerList != null) {
			listenerList.remove(listener);

			if (listenerList.isEmpty()) {
				listenerList = null;
			}
		}
	}
}
