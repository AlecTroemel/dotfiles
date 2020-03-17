#### All the zsh plugins and stuff ####
source ~/antigen.zsh
antigen use oh-my-zsh
antigen bundle git
antigen bundle pip
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle superbrothers/zsh-kubectl-prompt
antigen bundle Tarrasch/zsh-bd
antigen apply

autoload -U colors; colors

# set up git branch viewing
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%b'

PS1='%F{blue}(%~%f%F{blue}) %F{yellow}( ${vcs_info_msg_0_}) %F{green}(ﴱ $ZSH_KUBECTL_PROMPT)'$'\n''%(?.%F{magenta}.%F{red})> %f'

#### My aliases ####

# colorized cat with this python package (installed with pipx)
alias ccat="pygmentize"

# run meteor with settings file
alias mr="meteor run --settings settings.json"

# print this file out (in case I forget stuff)
alias als="cat ~/.my_alias"

# alias cat='ccat'

# want the gnu emacs
alias emacs='/usr/local/bin/emacs'

# lets not type kubectl, or other common commands every time!
alias kc='kubectl'
alias kcg='kubectl get'
alias kcd='kubectl describe'
alias kcdel='kubectl delete'
alias kced='kubectl edit'
alias kcex='kubectl edit'
alias kcgp='kubectl get pod'
alias kcdp='kubectl describe pod'
alias kcop='kubectl get pod -o yaml'
alias kcgs='kubectl get service'
alias kcgi='kubectl get ingress'
alias kcgce='kubectl get certs'
alias kcdce='kubectl describe certs'
alias kcgcr='kubectl get cronjobs'
alias kcl='kubectl logs -f'
alias k='kapn'
alias h='helm'
alias g='git'

# microk8s aliases
alias mkc='microk8s.kubectl'
alias mhelm='helm --kubeconfig ~/snap/helm/common/kube/config'

# python virtual environment
alias pve="source .pyvenv/bin/activate"

# jekyll commands
alias blog='cd ~/Documents/personal_projects/alectroemel.github.io'
alias blog-start='sudo bundle exec jekyll serve'
alias blog-new='jekyll-post'

# move to common directories
alias dms='cd ~/Documents/mirus/statefarm/'
alias dmd='cd ~/Documents/mirus/desjardin/'
alias dmm='cd ~/Documents/mirus/microservices/'
alias dmde='cd ~/Documents/mirus/dev/'
alias whomstami='whoami'

# add user base to python path
# export PYTHONPATH=$PYTHONPATH:$(python -c "import site, os; print(os.path.join(site.USER_BASE, 'lib', 'python2.7', 'site-packages'))")

#### Other config ####
export EDITOR=/usr/bin/emacs


# export TERM=xterm-16color
# source $HOME/.cargo/env"rofi" /snap/bin
# export GOPATH=$HOME/Documents/Mirus/go_workspace/
# export PATH="/usr/local/opt/openssl/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# HLA stuff
export hlalib=~/Documents/personal/art_of_assembly/usr/hla/hlalib
export hlainc=~/Documents/personal/art_of_assembly/usr/hla/include

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH=$PYENV_ROOT/bin:~/Documents/pico-8/:/var/lib/flatpak/exports/bin:~/.cargo/bin:~/.local/bin:/snap/bin:~/.nvm/:~/Downloads/google-cloud-sdk/bin:~/docker_slim:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/etc/paths.d/postgresapp:/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH

eval "$(pyenv init -)"
