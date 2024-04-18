{config,pkgs,...}:

{
  # 递归将某个文件夹中的文件，链接到 Home 目录下的指定位置
  home.file.".config/emacs" = {
    source = ../../../emacs;
    recursive = true;   # 递归整个文件夹
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };
  
  services.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    client.enable=true;
  };

  systemd.user.services.emacs.Service = {
    Environment="GDK_DPI_SCALE=2 GDK_SCALE=0.5";
  };

}
