# .bash_profile file
# Modified by dfeagans, originally created by Balaji S. Srinivasan
#
# Concepts:
# http://www.joshstaiger.org/archives/2005/07/bash_profile_vs.html
#
#    1) .bash_profile is the *login* config for bash, launched upon first
#        connection (in Ubuntu).
#
#    2) .bashrc is the *non-login* config for bash, run in scripts and after
#        first connection. *non-login* means it's run when a new screen
#        instance is initiated. It doesn't require you to actually log-in,
#        but of course you want your environment set-up (hence "non-login").
#
#    3) .bash_profile imports .bashrc, but not vice versa.
#
# When Bash starts, it executes the commands in a variety of different scripts.
#
#   1) When Bash is invoked as an interactive login shell, it first reads
#      and executes commands from the file /etc/profile, if that file
#      exists. After reading that file, it looks for ~/.bash_profile,
#      ~/.bash_login, and ~/.profile, in that order, and reads and executes
#      commands from the first one that exists and is readable.
#
#   2) When a login shell exits, Bash reads and executes commands from the
#      file ~/.bash_logout, if it exists.
#
#   3) When an interactive shell that is not a login shell is started
#      (e.g. a GNU screen session), Bash reads and executes commands from
#      ~/.bashrc, if that file exists. This may be inhibited by using the
#      --norc option. The --rcfile file option will force Bash to read and
#      execute commands from file instead of ~/.bashrc.

## -----------------------
## -- 1) Import .bashrc --
## -----------------------

# Factor out all repeated profile initialization into .bashrc
#  - All non-login shell parameters go there
#  - All declarations repeated for each screen session go there
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

## -----------------------
## --- 2) Config $PATH ---
## -----------------------

#  - These are line by line so that you can kill one without affecting the others.
#  - Lowest priority first, highest priority last.
export PATH=$PATH
export PATH=$HOME/bin:$PATH
export PATH=/usr/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
# - The below line was to put the Anaconda/Conda Python Package Management Tool in the path
export PATH=$HOME/anaconda3/bin:$PATH
