{config,pkgs,...}:
{
  systemd.user.sessionVariables = {
    GDK_DPI_SCALE="2";
    GDK_SCALE="0.5";
  };
}
