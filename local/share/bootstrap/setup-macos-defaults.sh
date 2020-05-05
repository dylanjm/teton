#!/bin/bash

# Close any open System Preferences panes, to prevent them from overriding
# settings we’re about to change
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Disable Auto-Correct
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false

# Increase window resize speed for Cocoa applications
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10

defaults write com.apple.screencapture disable-shadow -bool true


# Show the ~/Library folder
chflags nohidden ~/Library

# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false


defaults write -g QLPanelAnimationDuration -float 0
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.dock launchanim -bool false
defaults write com.apple.dock expose-animation-duration -float 0.1
defaults write com.apple.Dock autohide-delay -float 0
defaults write -g NSAutomaticQuoteSubstitutionEnabled -bool false

defaults write -g CGFontRenderingFontSmoothingDisabled -bool FALSE
defaults -currentHost write -globalDomain AppleFontSmoothing -int 1
# defaults write -g InitialKeyRepeat -float 10.5
# defaults write -g KeyRepeat -float 1.5
defaults write -g ApplePressAndHoldEnabled -bool false

defaults write com.apple.dock springboard-columns -int 8
defaults write com.apple.dock springboard-rows -int 6
defaults write com.apple.dock springboard-columns -int 10
defaults write com.apple.dock springboard-rows -int 10
defaults write com.apple.dock ResetLaunchPad -bool TRUE
killall Dock

defaults write com.apple.finder QuitMenuItem -bool false
killall Finder
