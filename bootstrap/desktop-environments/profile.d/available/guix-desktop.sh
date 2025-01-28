export GUIX_PROFILE_DESKTOP="$HOME/.guix-extra-profiles/desktop/desktop"
if [ ! -d $GUIX_PROFILE_DESKTOP ]; then
    unset GUIX_PROFILE_DESKTOP
    return
fi

GUIX_PROFILE=$GUIX_PROFILE_DESKTOP

. "$GUIX_PROFILE_DESKTOP/etc/profile"

export XDG_CONFIG_DIRS=$GUIX_PROFILE_DESKTOP/etc/xdg:$XDG_CONFIG_DIRS
export XDG_DATA_DIRS=$GUIX_PROFILE_DESKTOP/share:$XDG_DATA_DIRS

unset GUIX_PROFILE
