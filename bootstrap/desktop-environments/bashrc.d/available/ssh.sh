sadd() {
    if ! ssh-add -l | grep -q $1
    then
        ssh-add $1
    fi
}

ssh_login() {
    echo ssh -A $@
    ssh -A $@
    #ssh -A -R ~/.gnupg/S.gpg-agent:~/.gnupg/S.remote-agent -o "StreamLocalBindUnlink=yes" ${@/gpg/}
}

mosh_login() {
    ssh_cmd="ssh -A ${@:1:$#-1}"
    mosh --ssh="$ssh_cmd" ${!#}
}

for method in ssh mosh; do
    hosts=~/.ssh/${method}.hosts
    if [ -r $hosts ]; then
        while read line; do
            if [[ $line == "#"* ]]; then
                continue
            fi
            name=$(cut -d " " -f1 <<< $line)
            ssh_args=$(cut -d " " -f2- <<< $line)
            eval "$name() {
${method}_login \$@ $ssh_args
}"
        done < $hosts
    fi
    unset hosts
done
