for file in programs/*.tig; do
  file2=${file%.tig}
  file3=${file2##*/}
  echo $file3
  cabal exec tiger2 -- $file3
  spim -file $file2.s > $file2.out
done