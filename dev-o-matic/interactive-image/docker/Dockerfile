## -*- docker-image-name: "connect-devenv" -*-

FROM docker-devenv-base

# Make sure everything is up-to-date since base image was (re)built
RUN apt-get update && apt-get -y upgrade

# USER, GID, etc., are the developer's name/creds on the host
ARG USER_NAME
ARG USER_EMAIL
ARG USER
ARG UID
ARG ADDUSER_GROUPS
ARG TZ

# Set up /etc/alternatives for Python
#RUN update-alternatives --install /usr/bin/python python /usr/bin/python2 2 && \
#  update-alternatives --install /usr/bin/python python /usr/bin/python3 1

# We control login so stop systemd/dbus from squalking about it
RUN apt remove -y blueman

# Install packages
ENV DEBIAN_FRONTEND noninteractive

# Giiit the scriiifts
COPY build /build

# Setup locale
ENV LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
RUN printf '%s\n' "${LANG:?} UTF-8" > /etc/locale.gen \
	&& localedef -c -i "${LANG%%.*}" -f UTF-8 "${LANG:?}" ||:

# Setup timezone
ENV TZ=${TZ}
RUN printf '%s\n' "${TZ:?}" > /etc/timezone \
	&& ln -snf "/usr/share/zoneinfo/${TZ:?}" /etc/localtime

# Setup PATH
ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV PATH=${PATH}:/usr/games:/usr/local/games

# Setup D-Bus
RUN mkdir /run/dbus/ && chown messagebus:messagebus /run/dbus/
RUN dbus-uuidgen > /etc/machine-id
RUN ln -sf /etc/machine-id /var/lib/dbus/machine-id

# Create socket directory for X server
RUN mkdir /tmp/.X11-unix/ && chmod 1777 /tmp/.X11-unix/

# Copy and enable services
COPY service/ /etc/sv/
RUN ln -sv /etc/sv/sshd /etc/service/sshd && \
   ln -sv /etc/sv/dbus-daemon /etc/service/dbus-daemon && \
   ln -sv /etc/sv/vncserver /etc/service/vncserver

# Root configuration
RUN rm -fr /etc/skel && \
   mkdir -p /etc/skel && \
   cp -R build/_user/. /etc/skel
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod ugo+x /usr/local/bin/entrypoint.sh


# Set up a new Linux account matching user's Mac credentials
#   Macs assign users to a generic group like "staff"
#   Linuxes create a custom group named after each user initially containing just that user
# We add the user to both the Mac group name and Mac GID in addition to the typical Linux behavior

RUN useradd --badnames -m -u "$UID" -U -G "$ADDUSER_GROUPS" -s /bin/bash "$USER" && \
   echo "${USER} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers && \
   ln -s /home/${USER} /home/ubuntu

# Create /run/user/${UNPRIVILEGED_USER_UID}/dbus-1/ directory
RUN mkdir -p /run/user/"${UID:?}"/dbus-1/ && \
   chmod -R 700 /run/user/"${UID:?}"/ && \
   chown -R "${USER:?}:" /run/user/"${UID:?}"/

# Configure SSHd
RUN mkdir /run/sshd
RUN mkdir /home/${USER}/.ssh; echo "Ensure ~/.ssh"
COPY /build/.ssh/ /home/${USER}/.ssh
RUN chown -R ${USER}:${USER} /home/${USER}/.ssh && chmod 700 /home/${USER}/.ssh


# Become the user
ENV USER ${USER}
USER ${USER}

ENV HOME /home/${USER}
ENV HOST_IP $HOST_IP
ENV DISPLAY host.docker.internal:0

# Inside the user's environment...
WORKDIR ${HOME}
RUN bash -lc '[ -f .config ] && mv .config .config.default; exit 0'


# Cleanup
USER 0

# Move user's skeleton home directory out of the way
RUN mv /home/${USER} /home/${USER}-skel && \
    mkdir /home/${USER} && \
    chown ${USER}:${USER} /home/${USER}

# Dump environment variables
RUN env | grep -Ev '^(PWD|HOME|USER|USERNAME|SHELL|TERM|SHLVL)=' | sort > /etc/environment
RUN /build/set-password.sh ${USER}
RUN rm -fr /build

EXPOSE 22 5901-5909 8000-10000 6000

# Launch
#ENTRYPOINT tini -s -g -- runsvdir -P /etc/service/

ENTRYPOINT tini -s -g -- /usr/local/bin/entrypoint.sh ${USER} ${HOME} ${USER_NAME} ${USER_EMAIL}
