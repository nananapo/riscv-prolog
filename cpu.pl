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

fetch(_, RIP, []) :- RIP < 0.
fetch(Stack, Rip, Inst) :-
	Rip >= 0,
	N1 is Rip + 1,
	Next is Rip + 32,
	write("Fetch Stage----"),nl,
	subseq(Stack, N1, Next, Inst).

execute(Inst, _, Rip, NextRip, Regs, NextRegs) :-
	Rip >= 0,
	write("Execute Stage--"),nl,
	NextRip is Rip + 32,
	inst_add(Inst, Regs, NextRegs).

inst_add([
		0,0,0,0,0,
		0,0,
		Rs2_0,Rs2_1,Rs2_2,Rs2_3,Rs2_4,
		Rs1_0,Rs1_1,Rs1_2,Rs1_3,Rs1_4,
		0,0,0,
		Rd_0,Rd_1,Rd_2,Rd_3,Rd_4,
		0,1,1,0,0,
		1,1], Regs, RegsNew) :-
	bin2decimal([Rs2_0,Rs2_1,Rs2_2,Rs2_3,Rs2_4], Rs2),
	bin2decimal([Rs1_0,Rs1_1,Rs1_2,Rs1_3,Rs1_4], Rs1),
	bin2decimal([Rd_0,Rd_1,Rd_2,Rd_3,Rd_4], Rd),
	getreg(Regs, Rs2, VRs2),
	getreg(Regs, Rs1, VRs1),
	Result is VRs1 + VRs2,
	write("Add(rs2,rs1,rd) : "),write(Rs2),write(","),write(Rs1),write(","),write(Rd),nl,
	setreg(Regs, Rd, Result, RegsNew).

#riscv(_, 32, _) :- write("end"), nl.
riscv(Stack, Rip, Regs) :-
	fetch(Stack, Rip, Inst),
	write("Inst : "),write(Inst),nl,
	execute(Inst, Stack, Rip, NextRip, Regs, NextRegs),
	write("Result---------"),nl,
	write("regs  : "),write(NextRegs),nl,
	write("pc_reg: "),write(NextRip),nl,
	riscv(Stack, NextRip, NextRegs).

