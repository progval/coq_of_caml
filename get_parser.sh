rm -Rf ocaml-4.01.0/ parsing/
rm -f ocaml-4.01.0.tar.xz
wget http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.xz
tar -xJf ocaml-4.01.0.tar.xz ocaml-4.01.0/
cd ocaml-4.01.0/
    ./configure
    make utils/config.ml
cd ..
mv ocaml-4.01.0/parsing/ ./
mv ocaml-4.01.0/utils/{clflags,config,misc,terminfo,warnings}.ml* parsing/
rm -R ocaml-4.01.0/
rm ocaml-4.01.0.tar.xz
