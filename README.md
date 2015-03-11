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
git clone https://github.com/dfeagans/dotfiles.git
ln -sb dotfiles/.screenrc .
ln -sb dotfiles/.bash_profile .
ln -sb dotfiles/.bashrc .
ln -sb dotfiles/.gitconfig .
ln -sb dotfiles/.gitignore_global .
```
Once I figure out an intelligent method for deploying the emacs environment
the relevant details will be added. The start-up class included the entire
.emacs.d directory and init.el in the repo. Then it implemented it using:

```sh
mv .emacs.d .emacs.d~
ln -s dotfiles/.emacs.d .
```

I'd like to come up with a more elegent method that avoids freezing it in
time. Right now, my init.el is included in the repo for reference, but note
it's deliberately absent in the sym-linking instructions above.
