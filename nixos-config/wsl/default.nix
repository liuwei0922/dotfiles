{
  pkgs,
  config,
  ...
}: {
  # 主要区别就是这个 wsl 模块
  wsl = {
    enable = true;
    defaultUser = "moxian";
    nativeSystemd = true;
    interop.includePath = false;
    # 创建软件的桌面快捷方式
    #startMenuLaunchers = true;
  };

  environment.systemPackages = with pkgs; [
    wsl-open
  ];

  system.stateVersion = "23.11";
}
