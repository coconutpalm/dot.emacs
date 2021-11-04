/*******************************************************************************
 * Copyright (c) 2004, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.gef.ui.parts;

import org.eclipse.gef.DefaultEditDomain;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.ui.palette.FlyoutPaletteComposite;
import org.eclipse.gef.ui.palette.PaletteViewer;
import org.eclipse.gef.ui.palette.PaletteViewerProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * This class serves as a quick starting point for clients who are new to GEF.
 * It will create an Editor with a flyout palette. The flyout palette will only
 * be visible when the palette view is not open.
 * <p>
 * <EM>IMPORTANT</EM>This class should only be used as a reference for creating
 * your own EditorPart implementation. This class will not suit everyone's
 * needs, and may change in the future. Clients may copy the implementation.
 * 
 * @author Pratik Shah
 * @since 3.0
 */
public abstract class GraphicalEditorWithFlyoutPalette extends GraphicalEditor {

	private PaletteViewerProvider provider;
	private FlyoutPaletteComposite splitter;
	private CustomPalettePage page;
	public PaletteViewer viewer;

	/**
	 * @see GraphicalEditor#initializeGraphicalViewer()
	 */
	protected void initializeGraphicalViewer() {
		splitter.hookDropTargetListener(getGraphicalViewer());
	}

	/**
	 * Creates a PaletteViewerProvider that will be used to create palettes for
	 * the view and the flyout.
	 * 
	 * @return the palette provider
	 */
	protected PaletteViewerProvider createPaletteViewerProvider() {
		return new PaletteViewerProvider(getEditDomain());
	}

	/**
	 * @return a newly-created {@link CustomPalettePage}
	 */
	protected CustomPalettePage createPalettePage() {
		return new CustomPalettePage(getPaletteViewerProvider());
	}

	/**
	 * @see GraphicalEditor#createPartControl(Composite)
	 */
	public void createPartControl(Composite parent) {
		splitter = createPaletteComposite(parent);
		super.createPartControl(splitter);
		splitter.setGraphicalControl(getGraphicalControl());
		if (page != null) {
			splitter.setExternalViewer(page.getPaletteViewer());
			page = null;
		}
	}

	/**
	 * Creates a new {@link FlyoutPaletteComposite} to be used by this
	 * {@link GraphicalEditorWithFlyoutPalette}
	 * 
	 * @param parent
	 *            The {@link Composite}, which should serve as the container for
	 *            the to be created {@link FlyoutPaletteComposite}.
	 * @return The {@link FlyoutPaletteComposite} used by this
	 *         {@link GraphicalEditorWithFlyoutPalette}.
	 * @since 3.10
	 */
	protected FlyoutPaletteComposite createPaletteComposite(Composite parent) {
		return new FlyoutPaletteComposite(parent, SWT.NONE,
				getPaletteViewerProvider());
	}

	/**
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	public Object getAdapter(Class type) {
		if (type == CustomPalettePage.class) {
			if (splitter == null) {
				page = createPalettePage();
				return page;
			}
			return createPalettePage();
		}
		return super.getAdapter(type);
	}

	/**
	 * @return the graphical viewer's control
	 */
	protected Control getGraphicalControl() {
		return getGraphicalViewer().getControl();
	}

	/**
	 * Returns the PaletteRoot for the palette viewer.
	 * 
	 * @return the palette root
	 */
	protected abstract PaletteRoot getPaletteRoot();

	/**
	 * Returns the palette viewer provider that is used to create palettes for
	 * the view and the flyout. Creates one if it doesn't already exist.
	 * 
	 * @return the PaletteViewerProvider that can be used to create
	 *         PaletteViewers for this editor
	 * @see #createPaletteViewerProvider()
	 */
	protected final PaletteViewerProvider getPaletteViewerProvider() {
		if (provider == null)
			provider = createPaletteViewerProvider();
		return provider;
	}

	/**
	 * Sets the edit domain for this editor.
	 * 
	 * @param ed
	 *            The new EditDomain
	 */
	protected void setEditDomain(DefaultEditDomain ed) {
		super.setEditDomain(ed);
		getEditDomain().setPaletteRoot(getPaletteRoot());
	}

	/**
	 * A custom PalettePage that helps GraphicalEditorWithFlyoutPalette keep the
	 * two PaletteViewers (one displayed in the editor and the other displayed
	 * in the PaletteView) in sync when switching from one to the other (i.e.,
	 * it helps maintain state across the two viewers).
	 * 
	 * @author Pratik Shah
	 * @since 3.0
	 */
	protected class CustomPalettePage extends PaletteViewerPage {
		/**
		 * Constructor
		 * 
		 * @param provider
		 *            the provider used to create a PaletteViewer
		 */
		public CustomPalettePage(PaletteViewerProvider provider) {
			super(provider);
		}

		/**
		 * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
		 */
		public void createControl(Composite parent) {
			super.createControl(parent);
			if (splitter != null)
				splitter.setExternalViewer(viewer);
		}

		/**
		 * @see org.eclipse.ui.part.IPage#dispose()
		 */
		public void dispose() {
			if (splitter != null)
				splitter.setExternalViewer(null);
			super.dispose();
		}

		/**
		 * @return the PaletteViewer created and displayed by this page
		 */
		public PaletteViewer getPaletteViewer() {
			return viewer;
		}
	}

}