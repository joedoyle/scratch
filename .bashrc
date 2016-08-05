# if [ -f /bin/zsh ]
# then

# /bin/zsh $*

# else

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.sh
fi

if [ -f ~/.git-prompt.sh ]; then
    . ~/.git-prompt.sh
    export PS1='\w$(__git_ps1 " (%s)")\$ '
else
    export PS1='\w\$ '
fi

if [ -f ~/.support_funcs.sh ]
then
    source ~/.support_funcs.sh
fi

export HISTFILE=~/.bash_history
export HISTFILESIZE=1000000
export HISTSIZE=10000
export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### `hostname -s`:$cwd"; history -s "$hpwd"; history -a'
export PROMPT_DIRTRIM=3
shopt -s histappend

export LESS=FSRX

# export PATH=/opt/centos/devtoolset-3.1/root/usr/bin:${PATH}
