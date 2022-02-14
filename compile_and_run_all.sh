for file in programs/*.tig; do
  file2=${file%.tig}
  file3=${file2##*/}
  echo $file3
  cabal exec tiger2 -- $file3
  if [ $file3 = "merge" ]
  then
    echo "1 3 5;\n2 4 6;\n" | spim -file $file2.s > $file2.out
  else
    spim -file $file2.s > $file2.out
  fi
done