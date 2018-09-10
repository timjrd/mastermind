## Setup
Install the [Nix package manager](https://nixos.org/nix/):
```
curl https://nixos.org/nix/install | sh
```

## Build the PDF
```
nix-build
```
`result` is now a symlink to the PDF file.
