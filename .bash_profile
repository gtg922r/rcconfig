export PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/opt/local/bin:/opt/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/local/go/bin:/usr/texbin

# added by Anaconda 1.8.0 installer
export PATH="/Users/rcwork/anaconda/bin:$PATH"

# Added by Ryan
# export TERM=xterm-256color
alias ls='ls -laGh'
alias cdw='cd ~/wswork/dds_working'
export PS1="\[$(tput setab 8)\]\[$(tput setaf 10)\](\t) \[$(tput setaf 11)\]\u\[$(tput setaf 10)\]: \[$(tput setaf 4)\]\w \[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "  
export CLICOLOR=1
export EDITOR=/usr/local/bin/emacs

# 
#  svn () {
#   
#    #bail if the user didnt specify which subversion command to invoke
#    if [ $# -lt 1 ]; then
#      command svn
#      return
#    fi
#   
#    local sub_cmd=$1
#    shift
#   
#    #intercept svn diff commands
#    if [ $sub_cmd == diff ]; then
#   
#      #colorize the diff
#      #remove stupid ^M dos line endings
#      #page it if there's more one screen
#      command svn diff "$@" | colordiff
#   
#    #let svn handle it as normal
#    else
#      command svn $sub_cmd "$@"
#    fi
#  }
