# riscv-prolog

risc-vをprologで実装します....

```prolog
?- riscv(
  [0,0,0,0,0, 0,0, 0,0,0,0,0, 0,0,0,0,1, 0,0,0, 0,0,0,1,0, 0,1,1,0,0, 1,1,
   0,1,0,0,0, 0,0, 0,0,0,0,0, 0,0,0,1,0, 0,0,0, 0,0,0,1,1, 0,1,1,0,0, 1,1], 
  0, [10,20,0,0,0]).
```
```
Fetch Stage----
Inst : [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,1,1]
Execute Stage--
add(rs2,rs1,rd) : 0,1,2
20,10,2
Result---------
regs  : [10,20,30,0,0]
pc_reg: 32
Fetch Stage----
Inst : [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,1,1]
Execute Stage--
sub(rs2,rs1,rd) : 0,2,3
30,10,3
Result---------
regs  : [10,20,30,20,0]
pc_reg: 64
Fetch Stage----
Inst : []
Execute Stage--
false.
```

## Instructions

* add
* sub
* ld (not sext)
