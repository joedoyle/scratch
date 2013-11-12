# if [ -f /bin/zsh ]
# then

# /bin/zsh $*

# else

source ~/.git-completion.bash
source ~/.git-prompt.sh

export HISTFILE=~/.bash_history
export HISTFILESIZE=1000000
export HISTSIZE=10000
export PS1='\w$(__git_ps1 " (%s)")\$ '
export PROMPT_COMMAND='hpwd=$(history 1); hpwd="${hpwd# *[0-9]*  }"; if [[ ${hpwd%% *} == "cd" ]]; then cwd=$OLDPWD; else cwd=$PWD; fi; hpwd="${hpwd% ### *} ### $cwd"; history -s "$hpwd"; history -a'
export PROMPT_DIRTRIM=3
shopt -s histappend

export LESS=FSRX

# fi
export NEBULA_HOME=/tmp/jdoyle/dist/nebula

alias bldnebulaopt='scons -u -j4 --verbose var=opt /tmp/jdoyle/dist/Nebula/  && scons -u -j4 var=opt $NEBULA_HOME && scons ETAGS'

alias bldnebuladbg='export NEBULA_HOME=/tmp/jdoyle/dist/nebula; scons -u -j4 --verbose var=dbg /tmp/jdoyle/Nebula/  && scons -u -j4 var=dbg $NEBULA_HOME && scons ETAGS'
