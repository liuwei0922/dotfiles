{config,pkgs,libs,...}:

{
  i18n.supportedLocales=["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" ];
  i18n.defaultLocale = "zh_CN.UTF-8";
  i18n.extraLocaleSettings = {
    #LC_ALL = "zh_CN.UTF-8";
  };

  fonts.fontconfig.defaultFonts = {
    sansSerif = [ "LXGW Wenkai" "DejaVu Sans" ];
    serif = [ "DejaVu Serif" "LXGW Wenkai" ];
    monospace = [ "FiraCode Nerd Font Mono" ];
  };

  fonts.packages = with pkgs;[
    sarasa-gothic
    fira-code-nerdfont
    noto-fonts
    lxgw-wenkai
    wqy_zenhei
    symbola
    dejavu_fonts
  ];
}
