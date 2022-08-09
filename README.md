# salad2

## Building

```
cargo build --release
```

## Running/Usage

With `salad2` as the built binary, the usage is

```
./salad2 <test file> <program file>
```

e.g.

```
./salad2 test.txt myprogram.hcl
```

It is recommended to only run salad2 on program files that have been checked by HCLRS; salad2 only uses HCLRS to parse the actual program statements and does not check even very basic things, e.g. that all named wires are actually valid, declared wires.
