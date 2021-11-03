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
 *     Gunnar Wagenknecht <gunnar@wagenknecht.org> - Fix for bug 265445
 *     Benjamin Cabe <benjamin.cabe@anyware-tech.com> - Fix for bug 265532
 *     Lars Vogel<Lars.Vogel@vogella.com> - Bug 478768
 *******************************************************************************/
package org.eclipse.core.runtime;

import org.eclipse.core.internal.runtime.AdapterManager;

/**
 * The central class of the Eclipse Platform Runtime. This class cannot
 * be instantiated or subclassed by clients; all functionality is provided
 * by static methods.  Features include:
 * <ul>
 * <li>the platform registry of installed plug-ins</li>
 * <li>the platform adapter manager</li>
 * <li>the platform log</li>
 * <li>the authorization info management</li>
 * </ul>
 * <p>
 * Most users don't have to worry about Platform's lifecycle. However, if your
 * code can call methods of this class when Platform is not running, it becomes
 * necessary to check {@link #isRunning()} before making the call. A runtime
 * exception might be thrown or incorrect result might be returned if a method
 * from this class is called while Platform is not running.
 * </p>
 */
public final class Platform {

	/**
	 * The unique identifier constant (value "<code>org.eclipse.core.runtime</code>")
	 * of the Core Runtime (pseudo-) plug-in.
	 */
	public static final String PI_RUNTIME = "org.eclipse.core.runtime"; //$NON-NLS-1$

	/**
	 * The simple identifier constant (value "<code>applications</code>") of
	 * the extension point of the Core Runtime plug-in where plug-ins declare
	 * the existence of runnable applications. A plug-in may define any
	 * number of applications; however, the platform is only capable
	 * of running one application at a time.
	 *
	 */
	public static final String PT_APPLICATIONS = "applications"; //$NON-NLS-1$

	/**
	 * The simple identifier constant (value "<code>adapters</code>") of
	 * the extension point of the Core Runtime plug-in where plug-ins declare
	 * the existence of adapter factories. A plug-in may define any
	 * number of adapters.
	 *
	 * @see IAdapterManager#hasAdapter(Object, String)
	 * @since 3.0
	 */
	public static final String PT_ADAPTERS = "adapters"; //$NON-NLS-1$

	/**
	 * The simple identifier constant (value "<code>preferences</code>") of
	 * the extension point of the Core Runtime plug-in where plug-ins declare
	 * extensions to the preference facility. A plug-in may define any number
	 * of preference extensions.
	 *
	 * @see #getPreferencesService()
	 * @since 3.0
	 */
	public static final String PT_PREFERENCES =  "preferences"; //$NON-NLS-1$

	/**
	 * The simple identifier constant (value "<code>products</code>") of
	 * the extension point of the Core Runtime plug-in where plug-ins declare
	 * the existence of a product. A plug-in may define any
	 * number of products; however, the platform is only capable
	 * of running one product at a time.
	 *
	 * @see #getProduct()
	 * @since 3.0
	 */
	public static final String PT_PRODUCT = "products"; //$NON-NLS-1$

	/**
	 * Debug option value denoting the time at which the platform runtime
	 * was started.  This constant can be used in conjunction with
	 * <code>getDebugOption</code> to find the string value of
	 * <code>System.currentTimeMillis()</code> when the platform was started.
	 */
	public static final String OPTION_STARTTIME = PI_RUNTIME + "/starttime"; //$NON-NLS-1$

	/**
	 * Name of a preference for configuring the performance level for this system.
	 *
	 * <p>
	 * This value can be used by all components to customize features to suit the
	 * speed of the user's machine.  The platform job manager uses this value to make
	 * scheduling decisions about background jobs.
	 * </p>
	 * <p>
	 * The preference value must be an integer between the constant values
	 * MIN_PERFORMANCE and MAX_PERFORMANCE
	 * </p>
	 * @see #MIN_PERFORMANCE
	 * @see #MAX_PERFORMANCE
	 * @since 3.0
	 */
	public static final String PREF_PLATFORM_PERFORMANCE = "runtime.performance"; //$NON-NLS-1$

	/**
	 * Constant (value "line.separator") name of the preference used for storing
	 * the line separator.
	 *
	 * @see #knownPlatformLineSeparators
	 * @since 3.1
	 */
	public static final String PREF_LINE_SEPARATOR = "line.separator"; //$NON-NLS-1$

	/**
	 * Constant (value 1) indicating the minimum allowed value for the
	 * <code>PREF_PLATFORM_PERFORMANCE</code> preference setting.
	 * @since 3.0
	 */
	public static final int MIN_PERFORMANCE = 1;

	/**
	 * Constant (value 5) indicating the maximum allowed value for the
	 * <code>PREF_PLATFORM_PERFORMANCE</code> preference setting.
	 * @since 3.0
	 */
	public static final int MAX_PERFORMANCE = 5;

	/**
	 * Status code constant (value 1) indicating a problem in a plug-in
	 * manifest (<code>plugin.xml</code>) file.
	 */
	public static final int PARSE_PROBLEM = 1;

	/**
	 * Status code constant (value 2) indicating an error occurred while running a plug-in.
	 */
	public static final int PLUGIN_ERROR = 2;

	/**
	 * Status code constant (value 3) indicating an error internal to the
	 * platform has occurred.
	 */
	public static final int INTERNAL_ERROR = 3;

	/**
	 * Status code constant (value 4) indicating the platform could not read
	 * some of its metadata.
	 */
	public static final int FAILED_READ_METADATA = 4;

	/**
	 * Status code constant (value 5) indicating the platform could not write
	 * some of its metadata.
	 */
	public static final int FAILED_WRITE_METADATA = 5;

	/**
	 * Status code constant (value 6) indicating the platform could not delete
	 * some of its metadata.
	 */
	public static final int FAILED_DELETE_METADATA = 6;

	/**
	 * Constant string (value "win32") indicating the platform is running on a
	 * Window 32-bit operating system (e.g., Windows 98, NT, 2000).
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String OS_WIN32 = "win32";//$NON-NLS-1$

	/**
	 * Constant string (value "linux") indicating the platform is running on a
	 * Linux-based operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String OS_LINUX = "linux";//$NON-NLS-1$

	/**
	 * Constant string (value "aix") indicating the platform is running on an
	 * AIX-based operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 *
	 * @since 3.0
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String OS_AIX = "aix";//$NON-NLS-1$

	/**
	 * Constant string (value "solaris") indicating the platform is running on a
	 * Solaris-based operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String OS_SOLARIS = "solaris";//$NON-NLS-1$

	/**
	 * Constant string (value "hpux") indicating the platform is running on an
	 * HP/UX-based operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String OS_HPUX = "hpux";//$NON-NLS-1$

	/**
	 * Constant string (value "qnx") indicating the platform is running on a
	 * QNX-based operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String OS_QNX = "qnx";//$NON-NLS-1$

	/**
	 * Constant string (value "macosx") indicating the platform is running on a
	 * Mac OS X operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String OS_MACOSX = "macosx";//$NON-NLS-1$
	
	/**
	 * Constant string (value "unknown") indicating the platform is running on a
	 * machine running an unknown operating system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String OS_UNKNOWN = "unknown";//$NON-NLS-1$

	// FIXME: Hard-coded harckery for now!
	public static String getOS() { return OS_LINUX; }
	
	/**
	 * Constant string (value "x86") indicating the platform is running on an
	 * x86-based architecture.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String ARCH_X86 = "x86";//$NON-NLS-1$

	/**
	 * Constant string (value "PA_RISC") indicating the platform is running on an
	 * PA_RISC-based architecture.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 *
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String ARCH_PA_RISC = "PA_RISC";//$NON-NLS-1$

	/**
	 * Constant string (value "ppc") indicating the platform is running on an
	 * PowerPC-based architecture.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 *
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String ARCH_PPC = "ppc";//$NON-NLS-1$

	/**
	 * Constant string (value "sparc") indicating the platform is running on an
	 * Sparc-based architecture.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 *
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String ARCH_SPARC = "sparc";//$NON-NLS-1$

	/**
	 * Constant string (value "x86_64") indicating the platform is running on an
	 * x86 64bit-based architecture.
	 *
	 * @since 3.1
	 */
	public static final String ARCH_X86_64 = "x86_64";//$NON-NLS-1$

	/**
	 * Constant string (value "aarch64") indicating the platform is running on an
	 * AARCH64bit-based architecture.
	 *
	 * @since 3.22
	 */
	public static final String ARCH_AARCH64 = "aarch64";//$NON-NLS-1$

	/**
	 * Constant string (value "amd64") indicating the platform is running on an
	 * AMD64-based architecture.
	 *
	 * @since 3.0
	 * @deprecated use <code>ARCH_X86_64</code> instead. Note the values
	 * has been changed to be the value of the <code>ARCH_X86_64</code> constant.
	 */
	@Deprecated
	public static final String ARCH_AMD64 = ARCH_X86_64;

	/**
	 * Constant string (value "ia64") indicating the platform is running on an
	 * IA64-based architecture.
	 *
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String ARCH_IA64 = "ia64"; //$NON-NLS-1$

	/**
	 * Constant string (value "ia64_32") indicating the platform is running on an
	 * IA64 32bit-based architecture.
	 *
	 * @since 3.1
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String ARCH_IA64_32 = "ia64_32";//$NON-NLS-1$

	/**
	 * Constant string (value "win32") indicating the platform is running on a
	 * machine using the Windows windowing system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String WS_WIN32 = "win32";//$NON-NLS-1$

	/**
	 * Constant string (value "motif") indicating the platform is running on a
	 * machine using the Motif windowing system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 *
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String WS_MOTIF = "motif";//$NON-NLS-1$

	/**
	 * Constant string (value "gtk") indicating the platform is running on a
	 * machine using the GTK windowing system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String WS_GTK = "gtk";//$NON-NLS-1$

	/**
	 * Constant string (value "photon") indicating the platform is running on a
	 * machine using the Photon windowing system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 * @deprecated not supported anymore
	 */
	@Deprecated
	public static final String WS_PHOTON = "photon";//$NON-NLS-1$

	/**
	 * Constant string (value "carbon") indicating the platform is running on a
	 * machine using the Carbon windowing system (Mac OS X).
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 *
	 * @deprecated not supported anymore
	 *
	 * @since 3.0
	 */
	@Deprecated
	public static final String WS_CARBON = "carbon";//$NON-NLS-1$

	/**
	 * Constant string (value "cocoa") indicating the platform is running on a
	 * machine using the Cocoa windowing system (Mac OS X).
	 * @since 3.5
	 */
	public static final String WS_COCOA = "cocoa";//$NON-NLS-1$

	/**
	 * Constant string (value "wpf") indicating the platform is running on a
	 * machine using the WPF windowing system.
	 * @since 3.3
	 */
	public static final String WS_WPF = "wpf";//$NON-NLS-1$

	/**
	 * Constant string (value "unknown") indicating the platform is running on a
	 * machine running an unknown windowing system.
	 * <p>
	 * Note this constant has been moved from the deprecated
	 * org.eclipse.core.boot.BootLoader class and its value has not changed.
	 * </p>
	 * @since 3.0
	 */
	public static final String WS_UNKNOWN = "unknown";//$NON-NLS-1$

	// private constants for platform line separators and their associated platform names
//	private static final String LINE_SEPARATOR_KEY_UNIX = Messages.line_separator_platform_unix;
//	private static final String LINE_SEPARATOR_KEY_WINDOWS = Messages.line_separator_platform_windows;

	private static final String LINE_SEPARATOR_VALUE_LF = "\n"; //$NON-NLS-1$
	private static final String LINE_SEPARATOR_VALUE_CRLF = "\r\n"; //$NON-NLS-1$

	/**
	 * Private constructor to block instance creation.
	 */
	private Platform() {
		super();
	}


	/**
	 * Returns the adapter manager used for extending
	 * <code>IAdaptable</code> objects.
	 *
	 * @return the adapter manager for this platform
	 * @see IAdapterManager
	 */
	public static IAdapterManager getAdapterManager() {
		return AdapterManager.getDefault();
	}
}
