/*******************************************************************************
 * Copyright (c) 2000, 2018 IBM Corporation and others.
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
package org.eclipse.ui;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.widgets.Shell;

/**
 * A workbench window is a top level window in a workbench. Visually, a
 * workbench window has a menubar, a toolbar, a status bar, and a main area for
 * displaying a single page consisting of a collection of views and editors.
 * <p>
 * Each workbench window has a collection of 0 or more pages; the active page is
 * the one that is being presented to the end user; at most one page is active
 * in a window at a time.
 * </p>
 * <p>
 * The workbench window supports a few {@link IServiceLocator services} by
 * default. If these services are used to allocate resources, it is important to
 * remember to clean up those resources after you are done with them. Otherwise,
 * the resources will exist until the workbench window is closed. The supported
 * services are:
 * </p>
 * <ul>
 * <li>{@link ICommandService}</li>
 * <li>{@link IContextService}</li>
 * <li>{@link IHandlerService}</li>
 * <li>{@link IBindingService}. Resources allocated through this service will
 * not be cleaned up until the workbench shuts down.</li>
 * </ul>
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 *
 * @see IWorkbenchPage
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface IWorkbenchWindow extends IRunnableContext, IShellProvider {
	/**
	 * Closes this workbench window.
	 * <p>
	 * If the window has an open editor with unsaved content, the user will be given
	 * the opportunity to save it.
	 * </p>
	 *
	 * @return <code>true</code> if the window was successfully closed, and
	 *         <code>false</code> if it is still open
	 */
	boolean close();

	/**
	 * Returns this workbench window's shell.
	 *
	 * @return the shell containing this window's controls or <code>null</code> if
	 *         the shell has not been created yet or if the window has been closed
	 */
	@Override
	Shell getShell();

	/**
	 * This specialization of IRunnableContext#run(boolean, boolean,
	 * IRunnableWithProgress) blocks until the runnable has been run, regardless of
	 * the value of <code>fork</code>. It is recommended that <code>fork</code> is
	 * set to true in most cases. If <code>fork</code> is set to <code>false</code>,
	 * the runnable will run in the UI thread and it is the runnable's
	 * responsibility to call <code>Display.readAndDispatch()</code> to ensure UI
	 * responsiveness.
	 *
	 * @since 3.2
	 */
	@Override
	void run(boolean fork, boolean cancelable, IRunnableWithProgress runnable)
			throws InvocationTargetException, InterruptedException;

/**
	 * Returns a boolean indicating whether the workbench window is in the process
	 * of closing.
	 *
	 * @return <code>true</code> if the workbench window is in the process of
	 *         closing, <code>false</code> otherwise
	 * @since 3.112
	 */
	boolean isClosing();
}
