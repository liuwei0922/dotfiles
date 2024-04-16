{config,pkgs,libs,...}:

{
  i18n.supportedLocales=["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" ];
  i18n.defaultLocale = "en_US.UTF-8";

  fonts.packages = with pkgs;[
    sarasa-gothic
    fira-code-nerdfont
    noto-fonts
    lxgw-wenkai
  ];
}
