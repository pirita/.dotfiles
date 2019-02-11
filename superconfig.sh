#!/bin/bash

#oh my zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

#brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#lolcat
gem install lolcat

#emacs
brew uninstall emacs
brew cleanup

#brew doctor
brew update
brew upgrade
brew install emacs --with-cocoa --with-gnutls --with-rsvg --with-imagemagick
brew linkapps emacs

#Create symbolic links
rm -r $HOME/.emacs.d
ln -s $HOME/.dotfiles/emacs.d $HOME/.emacs.d
rm $HOME/.zshrc
ln -s $HOME/.dotfiles/zshrc $HOME/.zshrc
gem install lolcat

#git
ln -s $HOME/.dotfiles/git/gitconfig $HOME/.gitconfig
ln -s $HOME/.dotfiles/git/gitignore $HOME/.gitignore_global
git config --global core.excludesfile $HOME/.gitignore_global
