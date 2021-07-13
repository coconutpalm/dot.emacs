# This build step depends on the specified source artifacts

# Use Bash for Make's shell and turn on wildcard expansion
SHELL:=/bin/bash

# Convert these to absolute paths
SHELLNAME = $(basename ${SHELL})
SHELLRC = .$(SHELLNAME)rc

HERE=`pwd`

BIN_SOURCEDIR = ${HERE}/../scripts

RCDIR = ${HOME}/.devrc
USERSTATE = ${RCDIR}/home/${USER}
USERDOCS = ${RCDIR}/docs
USERCONF = ${RCDIR}/conf

All: .local-env-ok ${RCDIR} sync-config-contents binaries ~/code ~/.aws


# Confirm that XQuartz and Docker are installed and tweak some default settings.
# We only want to repeat this step if someone explicitly deletes `.local-env-ok`
.local-env-ok:
	./install.sh
	touch .local-env-ok


${RCDIR}:
	mkdir -p ${RCDIR}
	mkdir -p ${USERSTATE}
	mkdir -p ${USERDOCS}
	mkdir -p ${USERCONF}


# Don't overwrite what's there but if new files are added during an upgrade, install them
sync-config-contents:
	rsync -raq --progress --ignore-existing rc-skel/home/_user ${USERSTATE}
	rsync -raq --progress --ignore-existing rc-skel/docs/. ${USERDOCS}
	rsync -raq --progress --ignore-existing rc-skel/conf/. ${USERCONF}
	rsync -raq --progress ~/.ssh/. ${USERSTATE}/.ssh


BINARIES = ~/bin/dev ~/bin/devenv-utils ~/bin/devenv-environment
binaries: clean ~/bin ${BINARIES}

~/bin:
	echo "***** ~/bin directory for user scripts does not exist."
	echo "***** Creating, then adding to PATH inside ${SHELLRC}"
	mkdir ~/bin
	echo 'export PATH="$HOME/bin:$PATH"' >> ~/${SHELLRC}

~/bin/dev:
	ln -s "${BIN_SOURCEDIR}/dev" ~/bin/dev

~/bin/devenv-utils:
	ln -s "${BIN_SOURCEDIR}/devenv-utils" ~/bin/devenv-utils

~/bin/devenv-environment:
	ln -s "${BIN_SOURCEDIR}/dev" ~/bin/devenv-environment

~/code:
	echo "***** ~/code directory for sources/projects does not exist.  Linking into container."
	ln -s ${USERSTATE}/code ~/code

~/.aws:
	echo "***** ~/.aws configuration does not exist.  Creating and adding stub configuation files."
	mkdir -p ~/.aws
	cp -R aws-skel/. ~/.aws
	./configure-aws.sh

clean:
	rm -f ${BINARIES}
	rm -f ${RCDIR}/docs/.welcomed

# This target can only be invoked manually from the prereqs project  Obviously, don't
# use it unless you really need to start from scratch.  Also read the source of `devenv-utils`
# and make sure the `dgc` garbage-collect command won't blow away anything you want to keep.
nuke:
	bash -c 'bin/rm.sh -f; exit 0'
	bash -c "source ${BIN_SOURCEDIR}/devenv-utils; dgc; exit 0"
	rm -f ${BINARIES}
	rm -f ${RCDIR}/docs/.welcomed
	rm -f .local-env-ok
