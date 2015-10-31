# TODO: Make directory independent, cd must now be "HaskRel/examples/TIP"
ghci -i../src:../src-rec -XDataKinds -XViewPatterns AlgebraExample $@
