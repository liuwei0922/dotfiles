{
  pkgs,
  config,
  lib,
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

  environment.systemPackages = with pkgs; [
    wsl-open
    wslu
    # (
    #   let packages = with pkgs; [ bashInteractive ]; in
    #   pkgs.runCommand "dev-shell" {
    #     buildInputs=packages;
    #     nativeBuildInputs = [ pkgs.makeWrapper ];
    #   } ''
    #   mkdir -p $out/bin/
    #   ln -sf ${pkgs.bashInteractive}/bin/bash $out/bin/dev-shell
    #   wrapProgram $out/bin/dev-shell --prefix PATH : ${pkgs.lib.makeBinPath packages}
    # '' 
    # )
  ];

  users.users."moxian" = {
    isNormalUser = true;
    shell = pkgs.bashInteractive;
  };
  
  # 控制中文不要乱码，因为只有第一次的 login shell 是会乱码的
  programs.bash.loginShellInit =''
    
    tst0="$(pidof bash)";
    tst1="$(echo $tst0 |awk '{print $1}')";
    if [[ "$(ps -p $tst1 -o command=)" == "-bash" ]]; then
    exec bash
    fi
    
    '';

  
  #environment.loginShellInit="exec bash";
  #environment.extraInit=''exec bash'';


  system.stateVersion = "24.05";
}
