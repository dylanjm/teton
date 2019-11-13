#!/bin/bash

defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
defaults write -g QLPanelAnimationDuration -float 0
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.dock launchanim -bool false
defaults write com.apple.dock expose-animation-duration -float 0.1
defaults write com.apple.Dock autohide-delay -float 0
defaults write -g NSAutomaticQuoteSubstitutionEnabled -bool false

defaults write -g CGFontRenderingFontSmoothingDisabled -bool FALSE
defaults -currentHost write -globalDomain AppleFontSmoothing -int 2
defaults write -g InitialKeyRepeat -float 12.5
defaults write -g KeyRepeat -float 1.5
defaults write -g ApplePressAndHoldEnabled -bool false

defaults write com.apple.dock springboard-columns -int 8
defaults write com.apple.dock springboard-rows -int 6
defaults write com.apple.dock springboard-columns -int 10
defaults write com.apple.dock springboard-rows -int 10
defaults write com.apple.dock ResetLaunchPad -bool TRUE
killall Dock

defaults write com.apple.finder QuitMenuItem -bool false
killall Finder

defaults write org.gnu.Emacs Emacs.cursorType bar
defaults write org.gnu.Emacs Emacs.toolBar -bool false
defaults write org.gnu.Emacs FontBackend ns

defaults write org.gnu.Emacs TransparentTitleBar DARK
defaults write org.gnu.Emacs Font "Iosevka Nerd Font Mono-8:style=thin"
