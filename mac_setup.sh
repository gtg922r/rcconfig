#!/bin/bash
echo "RCConfig - Auto Setup"

# link bash profiles?
# Create / Connect Google Drive- ln -s ~/Google\ Drive/Notes ~/Documents/w

echo "Linking dot files:"
ln -s ~/.rcconfig/.bash_profile ~/
echo "Linked .bash_profile"
ln -s ~/.rcconfig/.bashrc ~/
echo "Linked .bashrc"


mkdir ~/Library/Keybindings
ln -s ~/.rcconfig/DefaultKeyBinding.dict ~/Library/Keybindings/
echo "Linked DefaultKeyBindings"

echo "Installing XCode Tools..."
xcode-select --install

echo "Installing Homebrew..."
#ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
cd ~
mkdir homebrew && curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
cd .rcconfig

echo "Installing Homebrew Cask"
#brew install caskroom/cask/brew-cask
brew tap caskroom/cask

echo "Install emacs gui"
brew cask install emacs
echo "Install emacs cli" #mainly just keeps brew happy
brew install emacs --with-gnutls --with-cocoa
echo "Install iterm2"
brew cask install iterm2

# brew linkapps emacs
# mv /usr/local/opt/emacs/Emacs.app /Applications

echo "Emacs Setup..."
brew tap caskroom/fonts
brew cask install font-anonymous-pro
python install_emacs.py

echo "Installing Google Drive"
brew cask install google-drive-file-stream
echo "Google Drive Installed. Save notes for offline and link to Documents notes"
echo "ln -s /Volumes/GoogleDrive/My\ Drive/Notes ~/Documents/Notes"
#export PATH="/Users/ryan/.cask/bin:$PATH" ?

# May want to config terminal but storing profile and loading it:
## > ~/Library/Preferences/com.apple.Terminal.plist

echo "Installing Brew Cask Apps"
brew cask install spotify
brew cask install skype
brew cask install vlc

# echo "Installing for Work is limited to Spotify, Skype, and VLC..."
# read -n 1 -p "Install for (W)ork or (H)ome? " wh
# case $wh in
#     W|w ) exit;;
#     H|h )
# 	# Commenting Out for Work Installations
# 	brew cask install google-chrome
# 	brew cask install dropbox
# 	brew cask install google-drive
# 	brew cask install lastpass
# 	brew cask install alfred
# 	brew cask install sweet-home3d
# 	brew cask install air-video-server-hd
# 	brew cask install appcleaner
# 	brew cask install brackets
# 	brew cask install cyberduck
# 	brew cask install disk-inventory-x
# 	brew cask install github
# 	brew cask install sabnzbd
# 	brew cask install transmission
# 	brew cask install steam
# 
# 	echo "Installing Brew Python Packages"
# 	brew tap homebrew/python
# 	brew install numpy
# 	brew install scipy
# 	brew install seaborn 
# 	 
# 	echo "Updating pip and installing pip python packages"
# 	pip install --upgrade pip
# 	pip install ipython
# 	pip install pandas
# 	exit;;
# esac
