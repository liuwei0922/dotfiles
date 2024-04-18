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
    #interop.includePath = false;
    wslConf.interop.appendWindowsPath= false;
    # 创建软件的桌面快捷方式
    startMenuLaunchers = true;
    wslConf.user.default = "moxian";
  };

  #users.users."moxian".shell = pkgs.bash;


  environment.systemPackages = with pkgs; [
    wsl-open
    wslu
  ];

  system.stateVersion = "23.11";
}
