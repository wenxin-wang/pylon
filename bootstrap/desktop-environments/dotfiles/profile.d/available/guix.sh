# export GUIX_DAEMON_SOCKET=guix://at111

export GUIX_PROFILE="$HOME/.guix-profile"
if [ ! -d $GUIX_PROFILE ]; then
    unset GUIX_PROFILE
    return
fi
# . "$GUIX_PROFILE/etc/profile"
export MANPATH=$MANPATH:/usr/share/man
export INFOPATH=$INFOPATH:/usr/share/info
