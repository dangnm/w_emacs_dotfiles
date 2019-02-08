read -r -d '' EMACSSERVERCODE << "EOM"
# Begin script: running emacs faster
unalias em >/dev/null 2>&1
em() {
    if [[ "$@" == "stop" ]]; then
      kill -9 $(ps -ef | grep '.*emacs.*daemon.*' | grep -v grep | awk '{print $2}') >/dev/null 2>&1
      kill -9 $(ps -ef | grep '.*Emacs\.app.*daemon.*' | grep -v grep | awk '{print $2}') >/dev/null 2>&1
      return
    fi
    if [[ $(ps aux | grep -w ".*emacs.*daemon.*" | grep -v grep | wc -l) -gt 0 ]]; then
        echo "daemon is running"
        if [[ $(ps aux | grep -w ".*emacs.*daemon.*" | grep -v grep | wc -l) -gt 1 ]]; then
            kill -9 $(ps aux | grep '.*emacs.*daemon.*' | grep -v 'grep' | awk '{print $2}')
            emacs --daemon
        fi
    elif [[ $(ps aux | grep -w ".*Emacs\.app.*daemon.*" | grep -v grep | wc -l) -gt 0 ]]; then
        echo "daemon is running"
        if [[ $(ps aux | grep -w ".*Emacs\.app.*daemon.*" | grep -v grep | wc -l) -gt 1 ]]; then
            kill -9 $(ps aux | grep '.*Emacs\.app.*daemon.*' | grep -v 'grep' | awk '{print $2}')
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
# End script: running emacs faster
EOM



declare file=".zshenv"
touch ~/$file
declare regex="source.*emacs_server.sh"
declare file_content=$( cat ~/"${file}" )
if [[ " $file_content " =~ $regex ]] # please note the space before and after the file content
then
  echo "Updating config..."
  sed -i '' "/$regex/d" ~/$file
else
  echo "Adding new config"
fi

rm ~/.emacs.d/emacs_server.sh
touch ~/.emacs.d/emacs_server.sh
echo "$EMACSSERVERCODE" >> ~/.emacs.d/emacs_server.sh
echo "source ~/.emacs.d/emacs_server.sh" >> ~/$file
source ~/$file
if [[ "$@" == "uninstall" ]]; then
  rm ~/.emacs.d/emacs_server.sh 2>/dev/null
  sed -i '' "/$regex/d" ~/$file
fi
