if [ -r '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

if [ -r $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]; then
  . $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
fi

if [ -d $HOME/.nix-profile/share ]; then
  export XDG_DATA_DIRS=$HOME/.nix-profile/share:$XDG_DATA_DIRS
fi

export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
