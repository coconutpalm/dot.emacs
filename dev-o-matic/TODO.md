# The Repeatable Extensible Devbox

Repeatable, beautiful, extensible dev environments based on Docker, and XFCE. Batteries included; ready to go (but you can customize also).

## "now" wishlist

- BUG: Instead of putting binaries in ~/bin, use ~/.devrc/bin and put that on the path

- Install `brew-packages.lst`, `vscode-extensions.lst`, and `extra-packages.lst` in `~/.devrc/conf` and concatinate those onto the end of the stock `.lst` files during build.  Make `merge-vscode-extensions` generic and able to push custom packages to the appropriate file in the stock environment.
  - e.g. something like:

  ```bash
  cat dev-o-matic/.../brew-packages.lst brew-packages.lst | \
       sort --field-separator=. --key=2 | \
       uniq \
       > /tmp/brew-packages.lst && \
  cp /tmp/brew-packages.lst devenv-o-matic/.../brew-packages.lst
  ```

- Add osxfuse **on the Mac side** and mount inside container
  - (This should enable AppImage support inside the container)
  - brew cask install osxfuse
    - Reboot machine
      - (Can we reboot from a script?)
      - Inside the Prereqs tasks/Makefile
  - Add `--device /dev/fuse` to docker run
  - <https://stackoverflow.com/questions/48402218/fuse-inside-docker>

- Speed! Speed! <https://www.cyberciti.biz/faq/unison-file-synchronizer-tool/>
  - Or maybe use Docker Compose and <https://github.com/cweagans/docker-bg-sync/>
- Emacs: Figure out the long pause when Emacs first launches
- Install VPN certs: See the starter `get-vpn-certificates` script in `rc-skel/.../bin`; see also [Install certs](https://unix.stackexchange.com/questions/304503/how-to-pull-vpn-certificate-to-put-in-cert-store)


## Audio/video?

- Video support in-container
  - On Mac: `brew install ffmpeg`
  - Then:
    - https://stackoverflow.com/questions/37960828/webcam-streaming-from-mac-using-ffmpeg
    - https://unix.stackexchange.com/questions/2302/can-i-pipe-dev-video-over-ssh
- Audio support in-container
  - On Mac: `brew install pulseaudio`
  - Then:
    - https://stackoverflow.com/questions/40136606/how-to-expose-audio-from-docker-container-to-a-mac
    - https://github.com/mviereck/x11docker/wiki/Container-sound:-ALSA-or-Pulseaudio
      - Useful techniques for getting IPs


## More Things

- Move vscode install out of base-image into interactive-image or on-container-start

- Update and document some keybinding defaults to work nicely with TigerVNCViewer for Mac
  - Remove shift-alt-super-l as screen lock. Ctrl-alt-del is still there.

- Installer quality-of-life things
  - `curl https://raw.github.com/.../install.sh | bash`: one-step install script
  - Open prereq web pages, instruct, then "press enter to continue"
  - Create ~/.devrc/plugins/_user.name plugin from some initial template (see plugin arch notes below)
  - Remember to rm ~/bin/maven-latest.tgz after installing; maybe parallel install w/ download also
- Add `~/.devrc/conf/on-container-firststart` script support
- `dev help <cmd>` extracts/prints detailed usage from `<cmd>.sh`
- `dev sh <cmd> [args, ...]` runs `cmd arg1, arg2, ...` inside the container if it exists else errors


## Someday-maybe

- 3 Docker images
  - Named:
    - ${IMAGENAME}-base Ubuntu + XFCE
    - ${IMAGENAME}-interactive Additional infra for `run -ti`, SSH, and XQuartz
    - your-user-name After applying plugins
- Three plugins in MVP release
  - devenv-employer - employer-specific config
  - devenv-plugin-directory
  - Example user plugin
- ~/.devrc/plugins
- Prereqs:
  - ~~Symlinks ~/bin/dev and ~/bin/dev-utils to itself~~ implemented via `cp`
- dev script
  - ~~subcommands are defined by mydir/bin/\*.sh~~; help lines are pulled from comments in the `.sh` file
    - e.g.: `# Usage: description/synopsis`
    - Subsequent comment lines are included also
    - An empty line ends the description printed for the subcommand
  - `dev run` by itself runs or attaches to your container and gives you a command prompt
  - `dev <subcommand>` searches `bin` folders in itself and plugins for `subcommand.sh` then runs `subcommand.sh $@`
- _IMAGENAME-base_ builds
- ~/.devrc contents
  - For reproducability, `install`, `upgrade` will blow away the `your-user-name` container
  - `DOCKERDEV_HOME` envar will always point to the main install dir
  - ~/.devrc/mounts (text file)
    - /host/dir/or/file /bindmount/location/in/container
    - Move hard-coded ~/Documents, ~/Pictures, etc., bindmounts here
  - ~/.devrc/homefiles (dir)
    - /.bashrc, .zshrc, etc.
      - Files in this directory will be bind-mounted inside your home directory in the container
    - /.on-container-start executed on container launch after plugins before login shell
      - bind-mounted to ~/.on-conainer-start inside the container
      - (A place to put install config you don't want to lose before you build a plugin for it)


## Implement plugin architecture

- ~/.devrc/plugins/_user.name
  - The user's plugin
  - A Git repository; not shared by default
  - Since plugins can depend on other plugins in a particular order, this one just lists the plugins needed to build the user's custom environment.
  - A natural, built-in place for the user to store his/her own customizations
- ~/.devrc/plugins/orgname.plugindir
  - During an installation or upgrade, plugin is `git clone`'d or `git pull`'d to ~/.devrc/plugins/org.name
  - /Makefile
    - Will be called with no arguments during image build _from inside the running container_
    - _MUST_ also implement `clean`, and `powerwash` subcommands for resetting state
  - /bin/[subcommand1.sh, subcommand2.sh, ...] (optional feature)
    - Each `.sh` file adds a subcommand to the `dev` script
- Can contribute additional subcommands to `dev` by including a `bin` folder with `subcommand.sh` files inside
- _SHOULD_ normally have a **single** subcommand with subsub commands implemented as needed

## Plugin directory plugin

- A plugin including a directory of plugins
