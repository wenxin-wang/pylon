export GUIX_PROFILE_BASE="$HOME/.guix-extra-profiles/base/base"
if [ ! -d $GUIX_PROFILE_BASE ]; then
    unset GUIX_PROFILE_BASE
    return
fi

GUIX_PROFILE=$GUIX_PROFILE_BASE
. "$GUIX_PROFILE_BASE/etc/profile"

export GUIX_LOCPATH="$GUIX_PROFILE_BASE/lib/locale:$GUIX_LOCPATH"

# export SSL_CERT_DIR="$GUIX_PROFILE_BASE/etc/ssl/certs"
# export SSL_CERT_FILE="$GUIX_PROFILE_BASE/etc/ssl/certs/ca-certificates.crt"
# export GIT_SSL_CAINFO="$SSL_CERT_FILE"

unset SSL_CERT_DIR
unset SSL_CERT_FILE
unset SSL_CERT_CAINFO

export XDG_CONFIG_DIRS=$GUIX_PROFILE_BASE/etc/xdg:$XDG_CONFIG_DIRS
export XDG_DATA_DIRS=$GUIX_PROFILE_BASE/share:$XDG_DATA_DIRS
unset GUIX_PROFILE
