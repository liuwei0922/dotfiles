{ config , pkgs,...}:
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    # TODO 在这里添加你的自定义 bashrc 内容
    #bashrcExtra = '''';
    # export GDK_DPI_SCALE=2;    export GDK_SCALE=0.5;
    profileExtra = ''
    export LEDGER_FILE="~/org/account/account.journal";
    export PATH="$PATH:$HOME/bin:$HOME/.local/bin:$HOME/go/bin";
'';

    # TODO 设置一些别名方便使用，你可以根据自己的需要进行增删
    shellAliases = {
      la = "ls -a --color";
      ll = "ls -l --color";
      k = "kubectl";
      setss="export all_proxy=\"http://127.0.0.1:10809\"";
      unsetss="export all_proxy=";
      sudoet="SUDO_EDITOR=\"emacsclient\" sudo -e";
      urldecode = "python3 -c 'import sys, urllib.parse as ul; print(ul.unquote_plus(sys.stdin.read()))'";
      urlencode = "python3 -c 'import sys, urllib.parse as ul; print(ul.quote_plus(sys.stdin.read()))'";
    };
  };

}