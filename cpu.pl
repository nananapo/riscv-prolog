fact(0, 1).
fact(X, Sum) :-
    X > 0,
	X1 is X - 1,
	fact(X1, Sum1),
	Sum is X * Sum1. 

# Add
cpu([0, X, Y | _]) :- Add is X + Y,
					write(Add).
