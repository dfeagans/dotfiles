dotfiles.git
============
Clone this repo to get my configuration dotfiles for setting up my
work environment for: bash, git, emacs, and nvm.

Ideally, git, nvm, and emacs will have already been installed using my
setup.sh. In case I just want to pull this repo down just to get the
.bashrc, I've tried to include logic to ignore the program specific
configuration items when the programs aren't installed.

The main benefit of using this method is that ALL configuration files
will be located in ~/dotfiles. Continue to add configuration files from
other programs to this directory and push back up to the github repo
to ease any deployments on other computers.

```sh
cd $HOME
# git clone will fail if dotfiles directory already exists, so mv dotfiles dotfiles.old if necessary.
git clone https://github.com/dfeagans/dotfiles
ln -sb dotfiles/.screenrc .
ln -sb dotfiles/.bash_profile .
ln -sb dotfiles/.bashrc .
ln -sb dotfiles/.gitconfig .
ln -sb dotfiles/.gitignore_global .
ln -sb ~/dotfiles/init.el ~/.emacs.d/.
ln -sb ~/dotfiles/my-packages.el ~/.emacs.d/.
```
The emacs configuration is completely handled by init.el and my-packages.el.
init.el contains the configuration items that don't require external packages/libraries.
my-packages.el specifies what libraries to install, installs them, AND configures them.
