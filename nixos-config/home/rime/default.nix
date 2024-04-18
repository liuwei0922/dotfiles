{config,pkgs,...}:{

  home.file.".config/fcitx5/rime" = {
    source = ../../../flypy.config;
    recursive = true;   # 递归整个文件夹
  };


}
