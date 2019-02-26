# Emacs-dotfiles

## Install Emacs
    # Uninstall old emacs
    $ brew uninstall emacs
    # Install it with this command:
    $ brew cask install emacs

## Run Emacs in terminal
    $ emacs -nw

## Install ag search
    brew install ag

## Install fzf search
    $ git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf 
    $ ~/.fzf/install

## Install dotfiles
    git clone git@github.com:dangnm/w_emacs_dotfiles.git

## Setup Emacs dotfiles
    Run sh ~/w_emacs_dotfiles/link.sh to toggle
    OR
    ln -sf ~/w_emacs_dotfiles/init.el ~/.emacs.d/init.el

## Restore default Emacs (optional)
    Run sh ~/w_emacs_dotfiles/link.sh to toggle

## Update melpa package list
    # In case you have a problem with install packages.
    # In emacs, run the following command.
    $ M-x package-refresh-contents

## Run Emacs in 0.1s
### Easy way
To install the script: chmod +x install_em.sh && ./install_em.sh
To uninstall the script: install_em.sh uninstall
### Hard way
Add this code to .zshenv and run emacs by typing "em"
```
unalias em >/dev/null 2>&1
em() {
    if [[ "$@" == "stop" ]]; then
      kill -9 $(ps -ef | grep '.*emacs.*daemon.*' | grep -v grep | awk '{print $2}')
      return
    fi
    if [[ $(ps aux | grep -w ".*emacs.*daemon.*" | grep -v grep | wc -l) -gt 0 ]]; then
        echo "daemon is running"
        if [[ $(ps aux | grep -w ".*emacs.*daemon.*" | grep -v grep | wc -l) -gt 1 ]]; then
            kill -9 $(ps aux | grep '.*emacs.*daemon.*' | grep -v 'grep' | awk '{print $2}')
            emacs --daemon
        fi
    else
        echo "daemon is starting"
        emacs --daemon
    fi

    if [[ "$@" == "" ]]; then
      emacsclient -create-frame --alternate-editor="" .
    else
      emacsclient -create-frame --alternate-editor="" "$@"
    fi
}
```
### Run emacs & start server (if server has not been started)
Type em
### Stop server
em stop

## More plugins
Check https://github.com/emacs-tw/awesome-emacs

## Update brew (optional)
    # IMPORTANT: use bellow commands carefully and make sure you know what you're doing
    # Clean out any previously downloaded source files
    $ brew cleanup
    # Check your brew installation is OK
    $ brew doctor
    # Update brew
    $ brew update 
    $ brew upgrade
