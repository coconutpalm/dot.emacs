/**
 * Copyright © 2019-2021 Jesse Gallagher
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openntf.maven.p2.layout;

import java.io.IOException;
import java.text.MessageFormat;

import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.spi.connector.layout.RepositoryLayout;
import org.eclipse.aether.spi.connector.layout.RepositoryLayoutFactory;
import org.eclipse.aether.spi.log.LoggerFactory;
import org.eclipse.aether.spi.log.Logger;
import org.eclipse.aether.transfer.NoRepositoryLayoutException;
import org.openntf.maven.p2.Messages;
import org.openntf.maven.p2.util.StdoutLogger;

@Named("p2")
public class P2RepositoryLayoutFactory implements RepositoryLayoutFactory {
	public final Logger log;

	public P2RepositoryLayoutFactory(Logger log) {
		this.log = log;
		log.debug("P2RepositoryLayoutFactory Constructor");
	}

	@Inject
	public P2RepositoryLayoutFactory(LoggerFactory loggerFactory) {
		this.log = loggerFactory.getLogger(getClass().getPackage().getName());
		log.debug("P2RepositoryLayoutFactory Constructor");
	}

	public P2RepositoryLayoutFactory() {
		this.log = StdoutLogger.log;
		log.debug("P2RepositoryLayoutFactory Constructor");
	}

	@Override
	public RepositoryLayout newInstance(RepositorySystemSession session, RemoteRepository repository)
			throws NoRepositoryLayoutException {
		if (!"p2".equals(repository.getContentType())) { //$NON-NLS-1$
			throw new NoRepositoryLayoutException(repository);
		}
		log.debug("P2RepositoryLayoutFactory newInstance");

		if (log.isDebugEnabled()) {
			log.debug(MessageFormat.format(Messages.getString("P2RepositoryLayoutFactory.creatingNew"), //$NON-NLS-1$
					repository.getUrl()));
		}

		try {
			return new P2RepositoryLayout(repository.getId(), repository.getUrl(), log);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public float getPriority() {
		return 1;
	}

}
