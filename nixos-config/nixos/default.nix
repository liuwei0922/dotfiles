# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL

{ config, lib, pkgs, ... }:

{
  imports = [
    # include NixOS-WSL modules
    #<nixos-wsl/modules>
  ];

  # the nixConfig here only affects the flake itself, not the system configuration!
  #nixConfig = {
  #  # override the default substituters
  #  substituters = [
  #    # cache mirror located in China
  #    # status: https://mirror.sjtu.edu.cn/
  #    "https://mirror.sjtu.edu.cn/nix-channels/store"
  #    # status: https://mirrors.ustc.edu.cn/status/
  #    #"https://mirrors.ustc.edu.cn/nix-channels/store"
  #    "https://cache.nixos.org"
  #    # nix community's cache server
  #    "https://nix-community.cachix.org"
  #  ];
  #};

  # 启用 Flakes 特性以及配套的船新 nix 命令行工具
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users=["moxian"];
  nix.settings.substituters = [ "https://mirrors.ustc.edu.cn/nix-channels/store" ];
  environment.systemPackages = with pkgs; [
    # Flakes 通过 git 命令拉取其依赖项，所以必须先安装好 git
    git
    vim
    wget
    curl
    which 
    file
    tree
    util-linux
  ];
  
  
  time.timeZone = "Asia/Shanghai";

  environment.variables = {
    GDK_DPI_SCALE="2";
    GDK_SCALE="0.5";
    EDITOR = "vim";
  };

  systemd.globalEnvironment = {
    #GDK_DPI_SCALE="2";
    #GDK_SCALE="0.5";
    LANG = "zh_CN.UTF-8";
    LANGUAGE = "zh_CN.UTF-8";
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  # do garbage collection weekly to keep disk usage low
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 1w";
  };

  # Optimise storage
  # you can also optimise the store manually via:
  #    nix-store --optimise
  # https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-auto-optimise-store
  nix.settings.auto-optimise-store = true;

  security.sudo.enable = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
