# Emacs-dotfiles

## Install Emacs
    # Install it with this command:
    $ brew install emacs --HEAD --use-git-head --cocoa --srgb
    # To get Emacs into Application folder 
    $ brew linkapps
    # Update alias to make homebrew emacs default emacs by adding the following line to .bash_profile
    alias emacs="/usr/local/Cellar/emacs/HEAD/bin/emacs -nw --insecure"

## Install ag search
    brew install ag

## Install dotfiles
    git clone git@github.com:dangnm/w_emacs_dotfiles.git
## Setup Spacemacs
    Run sh ~/w_emacs_dotfiles/link.sh to toggle

## Restore default emacs (optional)
    Run sh ~/w_emacs_dotfiles/link.sh to toggle

## Run spacemacs in 0.1s
Add this code to .zshenv and run emacs by typing "em"

```
  unalias em >/dev/null 2>&1
  em() {
    if [[ $(ps aux | grep -w "emacs --daemon" | grep -v grep | wc -l) -gt 0 ]]; then
        echo "daemon is running"
        if [[ $(ps aux | grep -w "emacs --daemon" | grep -v grep | wc -l) -gt 1 ]]; then
            kill -9 $(ps aux | grep 'emacs --daemon' | grep -v 'grep' | awk '{print $2}')
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



