# if [ -f /bin/zsh ]
# then

# /bin/zsh $*

# else

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi

if [ -f ~/.git-prompt.sh ]; then
    . ~/.git-prompt.sh
    export PS1='\w$(__git_ps1 " (%s)")\$ '
else
    export PS1='\w\$ '
fi

if [ -f /usr/share/bash-completion/completions/git ]; then
    source /usr/share/bash-completion/completions/git
fi

if [ -f ~/.support_funcs.sh ]
then
    source ~/.support_funcs.sh
fi

export HISTFILE=~/.bash_history
export HISTFILESIZE=1000000
export HISTSIZE=10000
export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### `hostname -s`:$cwd"; history -d -1; history -s "$hpwd"; history -a'
#export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### `hostname -s`:$cwd"; history -s "$hpwd"; history -a'
export PROMPT_DIRTRIM=3
#shopt -s histappend

export LESS=FSRX

export PATH=/var/lib/snapd/snap/bin:${PATH}
# more on the path? /snap/emacs/current/usr/bin

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/jdoyle/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/jdoyle/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/jdoyle/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/jdoyle/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

