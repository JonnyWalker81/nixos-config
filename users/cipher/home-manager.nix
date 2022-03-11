{ config, lib, pkgs, ... }: 
{

   home.packages = {
     pkgs.fd
     pkgs.go
     pkgs.gopls
   };

   programs.git = {
     enable = true;
   };

}
