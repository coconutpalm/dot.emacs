/*******************************************************************************
 * Copyright (c) 2000, 2020 IBM Corporation and others.
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
 *     Patrik Suzzi <psuzzi@gmail.com> - Bug 483465
 *     Christoph Läubrich - Bug 567898 - [JFace][HiDPI] ImageDescriptor support alternative naming scheme for high dpi 
 *******************************************************************************/
package org.eclipse.jface.resource;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.internal.InternalPolicy;
import org.eclipse.jface.util.Policy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageDataProvider;
import org.eclipse.swt.graphics.ImageFileNameProvider;

/**
 * An ImageDescriptor that gets its information from a URL. This class is not
 * public API. Use ImageDescriptor#createFromURL to create a descriptor that
 * uses a URL.
 */
class URLImageDescriptor extends ImageDescriptor {

	private static class URLImageFileNameProvider implements ImageFileNameProvider {
		private String url;

		public URLImageFileNameProvider(String url) {
			this.url = url;
		}

		@Override
		public String getImagePath(int zoom) {
			URL tempURL = getURL(url);
			if (tempURL != null) {
				URL xUrl = getxURL(tempURL, zoom);
				if (xUrl != null) {
					return getFilePath(xUrl, zoom == 100);
				}
			}
			return null;
		}
	}

	private static class URLImageDataProvider implements ImageDataProvider {
		private String url;

		public URLImageDataProvider(String url) {
			this.url = url;
		}

		@Override
		public ImageData getImageData(int zoom) {
			URL tempURL = getURL(url);
			if (tempURL != null) {
				URL xUrl = getxURL(tempURL, zoom);
				if (xUrl != null) {
					ImageData xdata = URLImageDescriptor.getImageData(xUrl);
					if (xdata != null) {
						return xdata;
					}
				}
				String xpath = FileImageDescriptor.getxPath(url, zoom);
				if (xpath != null) {
					URL xPathUrl = getURL(xpath);
					if (xPathUrl != null) {
						return URLImageDescriptor.getImageData(xPathUrl);
					}
				}
			}
			return null;
		}
	}

	private static long cumulativeTime;

	/**
	 * Constant for the file protocol for optimized loading
	 */
	private static final String FILE_PROTOCOL = "file"; //$NON-NLS-1$
	private String url;

	/**
	 * Creates a new URLImageDescriptor.
	 *
	 * @param url The URL to load the image from. Must be non-null.
	 */
	URLImageDescriptor(URL url) {
		this.url = url.toExternalForm();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof URLImageDescriptor)) {
			return false;
		}
		return ((URLImageDescriptor) o).url.equals(this.url);
	}

	@Deprecated
	@Override
	public ImageData getImageData() {
		return getImageData(getURL(url));
	}

	@Override
	public ImageData getImageData(int zoom) {
		return new URLImageDataProvider(url).getImageData(zoom);
	}

	private static ImageData getImageData(URL url) {
		ImageData result = null;
		try (InputStream in = getStream(url)) {
			if (in != null) {
				result = new ImageData(in);
			}
		} catch (SWTException e) {
			if (e.code != SWT.ERROR_INVALID_IMAGE) {
				throw e;
				// fall through otherwise
			}
		} catch (IOException e) {
			Policy.getLog().log(new Status(IStatus.ERROR, Policy.JFACE, e.getLocalizedMessage(), e));
		}
		return result;
	}

	/**
	 * Returns a stream on the image contents. Returns null if a stream could not be
	 * opened.
	 *
	 * @return the stream for loading the data
	 */
	protected InputStream getStream() throws IOException {
		return getStream(getURL(url));
	}

	private static InputStream getStream(URL url) throws IOException {
		return new BufferedInputStream(url.openStream());
	}

	@Override
	public int hashCode() {
		return url.hashCode();
	}

	/**
	 * The <code>URLImageDescriptor</code> implementation of this
	 * <code>Object</code> method returns a string representation of this object
	 * which is suitable only for debugging.
	 */
	@Override
	public String toString() {
		return "URLImageDescriptor(" + url + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static URL getxURL(URL url, int zoom) {
		if (zoom == 100) {
			return url;
		}
		String path = url.getPath();
		int dot = path.lastIndexOf('.');
		if (dot != -1 && (zoom == 150 || zoom == 200)) {
			String lead = path.substring(0, dot);
			String tail = path.substring(dot);
			if (InternalPolicy.DEBUG_LOAD_URL_IMAGE_DESCRIPTOR_2x_PNG_FOR_GIF && ".gif".equalsIgnoreCase(tail)) { //$NON-NLS-1$
				tail = ".png"; //$NON-NLS-1$
			}
			String x = zoom == 150 ? "@1.5x" : "@2x"; //$NON-NLS-1$ //$NON-NLS-2$
			try {
				String file = lead + x + tail;
				if (url.getQuery() != null) {
					file += '?' + url.getQuery();
				}
				return new URL(url.getProtocol(), url.getHost(), url.getPort(), file);
			} catch (MalformedURLException e) {
				Policy.getLog().log(new Status(IStatus.ERROR, Policy.JFACE, e.getLocalizedMessage(), e));
			}
		}
		return null;

	}

	/**
	 * Returns the filename for the ImageData.
	 *
	 * @return {@link String} or <code>null</code> if the file cannot be found
	 */
	private static String getFilePath(URL url, boolean logIOException) {
		if (FILE_PROTOCOL.equalsIgnoreCase(url.getProtocol()))
			return new Path(url.getFile()).toOSString();
		return null;
	}

	@Override
	public Image createImage(boolean returnMissingImageOnError, Device device) {

		long start = 0;
		if (InternalPolicy.DEBUG_TRACE_URL_IMAGE_DESCRIPTOR) {
			start = System.nanoTime();
		}
		try {

			if (InternalPolicy.DEBUG_LOAD_URL_IMAGE_DESCRIPTOR_2x) {
				if (!InternalPolicy.DEBUG_LOAD_URL_IMAGE_DESCRIPTOR_DIRECTLY) {
					try {
						return new Image(device, new URLImageFileNameProvider(url));
					} catch (SWTException | IllegalArgumentException exception) {
						// If we fail fall back to the slower input stream method.
					}
				}

				Image image = null;
				try {
					image = new Image(device, new URLImageDataProvider(url));
				} catch (SWTException e) {
					if (e.code != SWT.ERROR_INVALID_IMAGE) {
						throw e;
					}
				} catch (IllegalArgumentException e) {
					// fall through
				}
				if (image == null && returnMissingImageOnError) {
					try {
						image = new Image(device, DEFAULT_IMAGE_DATA);
					} catch (SWTException nextException) {
						return null;
					}
				}
				return image;

			}
			if (InternalPolicy.DEBUG_LOAD_URL_IMAGE_DESCRIPTOR_DIRECTLY) {
				return super.createImage(returnMissingImageOnError, device);
			}

			// Try to see if we can optimize using SWTs file based image support.
			URL pathURL = getURL(url);
			if (pathURL != null) {
				String path = getFilePath(pathURL, true);
				if (path != null) {
					try {
						return new Image(device, path);
					} catch (SWTException exception) {
						// If we fail fall back to the slower input stream
						// method.
					}
				}
			}
			return super.createImage(returnMissingImageOnError, device);
		} finally {
			if (InternalPolicy.DEBUG_TRACE_URL_IMAGE_DESCRIPTOR) {
				long time = System.nanoTime() - start;
				cumulativeTime += time;
				System.out.println(
						"Accumulated time (ms) to load URLImageDescriptor images: " + cumulativeTime / 1000000); //$NON-NLS-1$
			}
		}
	}

	private static URL getURL(String urlString) {
		URL result = null;
		try {
			result = new URL(urlString);
		} catch (MalformedURLException e) {
			Policy.getLog().log(new Status(IStatus.ERROR, Policy.JFACE, e.getLocalizedMessage(), e));
		}
		return result;
	}
}
