source ~/.git-completion.bash
source ~/.git-prompt.sh

export HISTFILE=/export/home/jdoyle/.bash_history
export HISTFILESIZE=1000000
export HISTSIZE=100000
export PS1='\w$(__git_ps1 " (%s)")\$ '
export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### $cwd"; history -s "$hpwd"; history -a'
export PROMPT_DIRTRIM=3
shopt -s histappend
