#!/bin/env zsh

create_session() {
  tmux new-session -d -s scratch
  tmux neww -d -n "httpyac" -c "~" nix-shell httpyac.nix
  tmux attach-session -t scratch
}

start_custom_tmux() {
	set -x
if [[ $- == *i* ]] && \
   [[ -z "$TMUX" ]]
then
  tmux attach-session -t scratch || create_session
fi

}

