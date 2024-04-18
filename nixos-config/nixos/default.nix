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

  # 启用 Flakes 特性以及配套的船新 nix 命令行工具
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users=["moxian"];
  environment.systemPackages = with pkgs; [
    # Flakes 通过 git 命令拉取其依赖项，所以必须先安装好 git
    git
    vim
    wget
    curl
    which 
    file
    tree
  ];
  
  #users.users.moxian = {
  #  isNormalUser = true;
  #  description = "ryan";
  #  extraGroups = [ "networkmanager" "wheel" ];
  #};
  time.timeZone = "Asia/Shanghai";

  environment.variables = {
    GDK_DPI_SCALE="2";
    GDK_SCALE="0.5";
    EDITOR = "vim";
  };

  programs.bash = {
    shellInit = ''export LANG=zh_CN.UTF-8; '';
    interactiveShellInit= ''export LANG=zh_CN.UTF-8; '';
  };
  
  nixpkgs.config = {
    allowUnfree = true;
  };
  #systemd.globalEnvironment = {
  #  GDK_DPI_SCALE="2";
  #  GDK_SCALE="0.5";
  #};
  
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
