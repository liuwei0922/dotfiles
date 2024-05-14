{
  description = "A simple NixOS flake";

  inputs = {
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    nixpkgs.url="https://mirrors.ustc.edu.cn/nix-channels/nixpkgs-unstable/nixexprs.tar.xz";
  };

  outputs = { self,nixpkgs,nixos-wsl,home-manager,... }@inputs: {
    # 因此请将下面的 my-nixos 替换成你的主机名称
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        # 这里导入之前我们使用的 configuration.nix，
        # 这样旧的配置文件仍然能生效
        nixos-wsl.nixosModules.wsl 
        home-manager.nixosModules.home-manager
        ./nixos
        ./font
        ./wsl
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;

          # 这里的 ryan 也得替换成你的用户名
          # 这里的 import 函数在前面 Nix 语法中介绍过了，不再赘述
          home-manager.users.moxian = import ./home;

          # 使用 home-manager.extraSpecialArgs 自定义传递给 ./home.nix 的参数
          # 取消注释下面这一行，就可以在 home.nix 中使用 flake 的所有 inputs 参数了
          home-manager.extraSpecialArgs = inputs;
        }
      ];
    };
  };
}
