{ config, ... }:

# Copied from https://github.com/blahgeek/emacs.d/blob/master/nix/home.nix.
let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.11.tar.gz") {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
    overlays = [];
  };
  unstable = import <nixpkgs-unstable> {};
  isOldUbuntu = builtins.getEnv "DESKTOP_SESSION" == "ubuntu";
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # AI.
    aider-chat-full
    claude-code
    claude-code-acp
    code-cursor
    # TODO(wenxin): Maybe consider https://github.com/ericc-ch/copilot-api, it may
    # expose more models as it mimics the behavior of the copilot VS Code plugin.
    copilot-language-server
    unstable.opencode
    litellm

    jigmo
    hanazono
    jetbrains-mono
    sarasa-gothic
    # https://wiki.nixos.org/wiki/Fonts#Patching_nerdfonts_into_fonts
    #   (sarasa-gothic.overrideAttrs (o: {
    #     nativeBuildInputs = [ pkgs.unzip pkgs.nerd-font-patcher ];
    #     postInstall = ''
    #   mkdir -p $out/share/fonts/truetype/{sarasa-gothic,sarasa-gothic-nerd}
    #   mv $out/share/fonts/truetype/*.ttc $out/share/fonts/truetype/sarasa-gothic/
    #   for f in $out/share/fonts/truetype/sarasa-gothic/*.ttc; do
    #     nerd-font-patcher --complete --outputdir $out/share/fonts/truetype/sarasa-gothic-nerd/ $f
    #   done
    # '';
    #   }))
    # nerd-fonts.ubuntu-mono
    nerd-fonts.symbols-only

    protobuf
    mypy-protobuf

    (dyalog.override { acceptLicense = true; })
    ride

    marimo

    git-remote-gcrypt

    jsonnet-language-server
    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    (rustPlatform.buildRustPackage rec {
      pname = "emacs-lsp-booster";
      version = "5f702a26";
      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = "emacs-lsp-booster";
        rev = "5f702a2699f306a3958ff1996a2b1a625f0cee0b";
        hash = "sha256-R9v+hCma/FfYdR+fvZ0vmtVk4dm+bPBacwV1QCc6X+8=";
      };
      cargoHash = "sha256-qchwxW3KITQcv6EFzR2BSISWB2aTW9EdCN/bx5m0l48=";
      doCheck = false;
    })
  ]
  ++
  (if isOldUbuntu then with pkgs; [
    emacs

    fd
    prettier

    python312
    pyright
    ruff
    clang-tools

    mitmproxy

    zotero
  ] else []);

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/wenxin/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  }
  //
  (if isOldUbuntu then {
    GTK_IM_MODULE = "ibus";
    QT_IM_MODULE = "ibus";
    XMODIFIERS = "@im=ibus";
  } else {});

  systemd.user.services.litellm = {
    Unit = {
      Description = "LiteLLM proxy";
      ConditionPathExists = "%h/.config/litellm/config.yml";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      ExecStart = "${pkgs.litellm}/bin/litellm -c %h/.config/litellm/config.yml";
    };
  };

  fonts.fontconfig.enable = true;

  targets.genericLinux.enable = pkgs.stdenv.isLinux;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
