{ config, pkgs, ... }:

{

  imports = [
    #./systemd
    ./shell
    ./emacs
    ./rime
  ];
  
  # 注意修改这里的用户名与用户目录
  home.username = "moxian";
  home.homeDirectory = "/home/moxian";

  #home.profileDirectory = "/home/moxian/.nix-profile";

  # 直接将当前文件夹的配置文件，链接到 Home 目录下的指定位置
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # 递归将某个文件夹中的文件，链接到 Home 目录下的指定位置
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # 递归整个文件夹
  #   executable = true;  # 将其中所有文件添加「执行」权限
  # };

  # 直接以 text 的方式，在 nix 配置文件中硬编码文件内容
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # 设置鼠标指针大小以及字体 DPI（适用于 4K 显示器）
  #xresources.properties = {
  #  "Xcursor.size" = 16;
  #  "Xft.dpi" = 172;
  #};

  # 通过 home.packages 安装一些常用的软件
  # 这些软件将仅在当前用户下可用，不会影响系统级别的配置
  # 建议将所有 GUI 软件，以及与 OS 关系不大的 CLI 软件，都通过 home.packages 安装
  home.packages = with pkgs;[
    # 如下是我常用的一些命令行工具，你可以根据自己的需要进行增删
    neofetch
    nnn # terminal file manager
    gnome.adwaita-icon-theme
    gcc13
    gnumake

    #emacs29-pgtk
    librime
    wl-clipboard

    # archives
    zip
    xz
    unzip
    p7zip

    gnused
    gnutar
    gawk
    zstd
    gnupg
    # system call monitoring
    strace # system call monitoring
    ltrace # library call monitoring
    lsof # list open files

    # system tools
    sysstat
    lm_sensors # for `sensors` command
    ethtool
    pciutils # lspci
    usbutils # lsusb
    # utils
    fd
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    yq-go # yaml processor https://github.com/mikefarah/yq
    #eza # A modern replacement for ‘ls’
    fzf # A command-line fuzzy finder

    # networking tools
    #mtr # A network diagnostic tool
    #iperf3
    #dnsutils  # `dig` + `nslookup`
    #ldns # replacement of `dig`, it provide the command `drill`
    #aria2 # A lightweight multi-protocol & multi-source command-line download utility
    #socat # replacement of openbsd-netcat
    #nmap # A utility for network discovery and security auditing
    ipcalc  # it is a calculator for the IPv4/v6 addresses

    # misc
    #cowsay
    #nix-bash-completions
    
    # nix related
    #
    # it provides the command `nom` works just like `nix`
    # with more details log output
    nix-output-monitor

    # productivity
    #hugo # static site generator
    #glow # markdown previewer in terminal

    #btop  # replacement of htop/nmon
    #iotop # io monitoring
    #iftop # network monitoring
  ];

  # git 相关配置
  programs.git = {
    enable = true;
    userName = "qinmoxiao";
    userEmail = "qinmoxiao@qq.com";
    extraConfig = {
      https.proxy = "http://127.0.0.1:10809";
    };
  };

  # 启用 starship，这是一个漂亮的 shell 提示符
  #programs.starship = {
  #  enable = false;
  #  # 自定义配置
  #  settings = {
  #    add_newline = false;
  #    aws.disabled = true;
  #    gcloud.disabled = true;
  #    line_break.disabled = true;
  #  };
  #};


  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
