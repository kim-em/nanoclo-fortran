{
  description = "Fortran toolchain for nanoclo-fortran";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ffd8177a246bbf499a2ac73ac60498f4970fa5de";
  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let pkgs = import nixpkgs { system = "x86_64-linux"; };
      in pkgs.mkShell {
        packages = [ pkgs.gfortran pkgs.gnumake pkgs.python3 ];
      };
  };
}
