export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# export SSH_ASKPASS=ssh-askpass
export SSH_ASKPASS=/usr/bin/ksshaskpass
export SSH_ASKPASS_REQUIRE=prefer
