#!/bin/bash

#oh my zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

#brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#emacs
brew uninstall emacs
brew cleanup
brew install coursier
brew install giter8
brew install httpie

#brew doctor
brew update
brew upgrade
brew install emacs --with-cocoa --with-gnutls --with-rsvg --with-imagemagick
brew linkapps emacs

#libraries
gem install lolcat
gem install tree
brew install coursier
brew install giter8
brew install httpie

#Create symbolic links
rm -r $HOME/.emacs.d
ln -s $HOME/.dotfiles/emacs.d $HOME/.emacs.d
rm $HOME/.zshrc
ln -s $HOME/.dotfiles/zshrc $HOME/.zshrc
ln -s $HOME/.dotfiles/bin/my_alias $HOME/.my_alias

#git
ln -s $HOME/.dotfiles/git/gitconfig $HOME/.gitconfig
ln -s $HOME/.dotfiles/git/gitignore $HOME/.gitignore_global
git config --global core.excludesfile $HOME/.gitignore_global
