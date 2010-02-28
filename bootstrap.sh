#!/bin/sh
INSTALL_DIR=$HOME/.emacs.d/
AUTO_INSTALL_URL=http://www.emacswiki.org/emacs/download/auto-install.el
wget $AUTO_INSTALL_URL

# install auto-install
mkdir -p $INSTALL_DIR/auto-install
mv auto-install.el $INSTALL_DIR/auto-install

