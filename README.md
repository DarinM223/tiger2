tiger2
======

A compiler written in Haskell for the Tiger language targeting MIPS.

Features
--------

* Successfully compiles and runs queens.tig and merge.tig
* Uses alex and happy for lexing and parsing
* FindEscape is completed
* Register allocator implements spilling and coalescing
* Spill cost is weighted by occurrences of uses and defs in loops
* Handles functions with > 4 parameters

How to use
----------

You need SPIM installed in order to run the compiled programs.

---

To compile and run all of the programs in the `programs/` folder:

```
sh compile_and_run_all.sh
```

---

If you want to compile a single program, like for example, `programs/queens.tig`:

```
cabal exec tiger2 -- queens
```

---

In order to generate a Happy info table for debugging the parser:

```
sh gen_info_table.sh
```

The resulting info table will be in `src/Tiger/Grammar.info`.

---

You can also experiment in the REPL. First run:

```
chmod 644 .ghci
```

in order to turn off the write permissions for `.ghci` (ghci ignores that file otherwise). Then run:

```
cabal repl
```

and enter `:ss` in the prompt:

```
ghci> :ss
```

This unpacks all the functions in each compiler "module" to the toplevel along
with everything in the `Tiger.State` record so
you can run things like `newTemp` and `allocLocal` directly.

```
ghci> newTemp
25
ghci> regRa
4
ghci> :t allocLocal
allocLocal :: MipsFrame -> Bool -> IO (F.Access MipsFrame)
```

Also preexisting testing functions `testParse`, `testTc`, `testTrans`, `testCanon`, `testCodegen`, and `compile` are available from the REPL toplevel.

```
ghci> testTc "let type rec = { a: int, b: string } in rec { a = 1, b = \"hello\" } end"
(ESeqExp (SeqStm (MoveStm (TempExp 26) (CallExp (NameExp allocRecord) [ConstExp 8])) (SeqStm (MoveStm (MemExp (BinOpExp Plus (TempExp 26) (ConstExp 0))) (ConstExp 1)) (MoveStm (MemExp (BinOpExp Plus (TempExp 26) (ConstExp 4))) (NameExp L25)))) (TempExp 26),RecordTy [(a,IntTy),(b,StringTy)] Unique)
```