/* Male */

male('Aldo Burrows').
male('Lincoln Burrows').
male('Michael Scofield').
male('LJ Burrows').

/* Female */

female('Christina Rose Scofield').
female('Sara Tancredi').
female('Ella Scofield').

/* Parents */

parent('LJ Burrows', 'Lisa Rix').
parent('LJ Burrows', 'Lincoln Burrows').

parent('Lincoln Burrows', 'Aldo Burrows').
parent('Lincoln Burrows', 'Christina Rose Scofield').

parent('Ella Scofield', 'Michael Scofield').
parent('Ella Scofield', 'Sara Tancredi').

/* 1. a) */

?- parent(X, 'Michael Scofield').

/* 1. b) */