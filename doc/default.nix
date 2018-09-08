let
  rev    = "ce0d9d638ded6119f19d87e433e160603683fb1b"; # nixos-18.03
  sha256 = "0na6kjk4xw6gqrn3a903yv3zfa64bspq2q3kd6wyf52y44j3s8sx";

  repo = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${rev}.tar.gz";
    inherit sha256;
  };
  nixpkgs = import repo {};

in with nixpkgs; stdenv.mkDerivation rec {
  file = "mastermind";
  
  name = "${file}.pdf";
  
  src = ./.;
  
  buildInputs = [
    (texlive.combine { inherit (texlive)
      scheme-basic
      babel-french
      tabu varwidth
      xcolor
      parskip enumitem sectsty
      footmisc
      tocloft
      mathtools
      cancel
      libertine xkeyval fontaxes
      inconsolata upquote
      euler
      tikz-qtree pgf
      biblatex etoolbox logreq xstring
    ;})
  ];

  #HOME = ".";
  
  buildPhase = ''
    pdflatex -draftmode ${file}.tex
    bibtex              ${file}
    pdflatex -draftmode ${file}.tex
    pdflatex            ${file}.tex
  '';
  
  installPhase = "cp ${file}.pdf $out";
}
