take(_, 0, []).
take([], N, []) :- N > 0.
take([X | Xs], N, [X | Ys]) :-
    N > 0,
	N1 is N - 1,
	take(Xs, N1, Ys).

drop(Xs, 0, Xs).
drop([], N, []) :- N > 0.
drop([_ | Xs], N, Ys) :-
    N > 0,
	N1 is N - 1,
	drop(Xs, N1, Ys).

subseq(_, N, M, []) :- N > M.
subseq(Xs, N, M, Ys) :-
    N > 0,
	M >= N,
	N1 is N - 1,
	K is M - N1,
	drop(Xs, N1, Zs),
	take(Zs, K, Ys).

replace([_ | T], 0, X, [X | T]).
replace([H | T], I, X, [H | R]) :-
	I > 0,
	I2 is I - 1,
	replace(T, I2, X, R), !.
replace(L, _, _, L).

access([A | _], 0, A).
access([_ | T], I, R) :-
	I > 0,
	I2 is I - 1,
	access(T, I2, R).

setreg(Regs, Key, Value, RegsNew) :-
	replace(Regs, Key, Value, RegsNew).

getreg(Regs, Key, Value) :-
	access(Regs, Key, Value).

bin2decimal([], 0).
bin2decimal([A|Z], R) :-
	bin2decimal(Z, R2),
	R is A + R2 * 2.

bit2byte(Bit, Byte) :-
	Byte is Bit / 8.

byte2bit(Byte, Bit) :-
	Bit is Byte * 8.

fetch(_, RIP, []) :- RIP < 0.
fetch(Stack, Rip, Inst) :-
	Rip >= 0,
	N1 is Rip + 1,
	Next is Rip + 32,
	write("Fetch Stage----"),nl,
	subseq(Stack, N1, Next, Inst).

id_rs_value(Regs, [R0,R1,R2,R3,R4], R_addr, Value) :-
	bin2decimal([R0,R1,R2,R3,R4], R_addr),
	getreg(Regs, R_addr, Value).

execute_inst(Inst, Stack, NextStack, Regs, NextRegs) :-
	inst_add(Inst, Stack, NextStack, Regs, NextRegs);
	inst_sub(Inst, Stack, NextStack, Regs, NextRegs);
	inst_ld(Inst, Stack, NextStack, Regs, NextRegs).


execute(Inst, Stack, NextStack, Rip, NextRip, Regs, NextRegs) :-
	Rip >= 0,
	write("Execute Stage--"),nl,
	execute_inst(Inst, Stack, NextStack, Regs, NextRegs),
	NextRip is Rip + 32.

load64bit(Stack, Addr, Data) :-
	Addr  >= 0,
	byte2bit(Addr, AddrBit),
	AddrS is AddrBit + 1,
	AddrE is AddrBit + 64,
	subseq(Stack, AddrS, AddrE, DataBit),
	bin2decimal(DataBit, Data).

inst_add([
		0,0,0,0,0,
		0,0,
		Rs2_4,Rs2_3,Rs2_2,Rs2_1,Rs2_0,
		Rs1_4,Rs1_3,Rs1_2,Rs1_1,Rs1_0,
		0,0,0,
		Rd_4,Rd_3,Rd_2,Rd_1,Rd_0,
		0,1,1,0,0,
		1,1], Stack, Stack, Regs, RegsNew) :-
	id_rs_value(Regs, [Rs2_0,Rs2_1,Rs2_2,Rs2_3,Rs2_4], Rs2_addr, Rs2_data),
	id_rs_value(Regs, [Rs1_0,Rs1_1,Rs1_2,Rs1_3,Rs1_4], Rs1_addr, Rs1_data),
	id_rs_value(Regs, [Rd_0,Rd_1,Rd_2,Rd_3,Rd_4], Rd_addr, _),
	Result is Rs1_data + Rs2_data,
	write("add(rs2,rs1,rd) : "),write(Rs2_addr),write(","),write(Rs1_addr),write(","),write(Rd_addr),nl,
	write(Rs1_data),write(","),write(Rs2_data),write(","),write(Rd_addr),nl,
	setreg(Regs, Rd_addr, Result, RegsNew).

inst_sub([
		0,1,0,0,0,
		0,0,
		Rs2_4,Rs2_3,Rs2_2,Rs2_1,Rs2_0,
		Rs1_4,Rs1_3,Rs1_2,Rs1_1,Rs1_0,
		0,0,0,
		Rd_4,Rd_3,Rd_2,Rd_1,Rd_0,
		0,1,1,0,0,
		1,1], Stack, Stack, Regs, RegsNew) :-
	id_rs_value(Regs, [Rs2_0,Rs2_1,Rs2_2,Rs2_3,Rs2_4], Rs2_addr, Rs2_data),
	id_rs_value(Regs, [Rs1_0,Rs1_1,Rs1_2,Rs1_3,Rs1_4], Rs1_addr, Rs1_data),
	id_rs_value(Regs, [Rd_0,Rd_1,Rd_2,Rd_3,Rd_4], Rd_addr, _),
	Result is Rs1_data - Rs2_data,
	write("sub(rs2,rs1,rd) : "),write(Rs2_addr),write(","),write(Rs1_addr),write(","),write(Rd_addr),nl,
	write(Rs1_data),write(","),write(Rs2_data),write(","),write(Rd_addr),nl,
	setreg(Regs, Rd_addr, Result, RegsNew).

inst_ld([
		O_11,O_10,O_9,O_8,O_7,O_6,O_5,O_4,O_3,O_2,O_1,O_0,
		Rs1_4,Rs1_3,Rs1_2,Rs1_1,Rs1_0,
		0,1,1,
		Rd_4,Rd_3,Rd_2,Rd_1,Rd_0,
		0,0,0,0,0,
		1,1], Stack, Stack, Regs, RegsNew) :-
	id_rs_value(Regs, [Rs1_0,Rs1_1,Rs1_2,Rs1_3,Rs1_4], _, Rs1_data),
	id_rs_value(Regs, [Rd_0,Rd_1,Rd_2,Rd_3,Rd_4], Rd_addr, _),
	bin2decimal([O_0,O_1,O_2,O_3,O_4,O_5,O_6,O_7,O_8,O_9,O_10,O_11], Offset),
	Addr is Offset + Rs1_data,
	write("ld(rs1_data, rd, offset) : "),write(Rs1_data),write(","),write(Rd_addr),write(","),write(Offset),nl,
	load64bit(Stack, Addr, Data),
	write("ld(data) : "),write(Data),nl,
	setreg(Regs, Rd_addr, Data, RegsNew).
		

#riscv(_, 32, _) :- write("end"), nl.
riscv(Stack, Rip, Regs) :-
	fetch(Stack, Rip, Inst),
	write("Inst : "),write(Inst),nl,
	execute(Inst, Stack, NextStack, Rip, NextRip, Regs, NextRegs),
	write("Result---------"),nl,
	write("regs  : "),write(NextRegs),nl,
	write("pc_reg: "),write(NextRip),nl,
	write("stack : "),write(NextStack),nl,
	riscv(NextStack, NextRip, NextRegs).

