#
# ~/.bash_profile
#
# All CLI configuration starts here; this file executes the other config files if
# they exist.
#

# Ensure all the standard things on the path
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:$HOME/bin:$HOME/.local/bin:/snap/bin:$PATH"


# We source ~/.profile because some install scripts add envars there
[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.bashrc ]] && . ~/.bashrc


# Command/environment configuration

# Prettify the `less` command
export LESS='IFRS'
export VISUAL=vi
export EDITOR=emacs

# Make emacs-webkit work
export WEBKIT_FORCE_SANDBOX=0

# Postgres
export PGHOST="localhost"  # FIXME: connect-docker-compose will put postgres on a different IP; maybe it can help?
