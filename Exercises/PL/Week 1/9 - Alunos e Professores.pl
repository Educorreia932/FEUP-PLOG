aluno(joao, paradigmas).
aluno(maria, paradigmas).
aluno(joel, lab2).
aluno(joel, estruturas).

frequenta(joao, feup).
frequenta(maria, feup).
frequenta(joel, ist).

professor(carlos, paradigmas).
professor(ana_paula, estruturas).
professor(pedro, lab2).

funcionario(pedro, ist).
funcionario(ana_paula, feup).
funcionario(carlos, feup). 

/* 9. a) */

alunos_do_professor(_X) :-
    aluno(Aluno, _Cadeira),
    professor(_X, _Cadeira).

/* 9. b) */

pessoas_da_universidade(_X) :-
    funcionario(Professor, _X).

pessoas_da_universidade(_X) :-
    frequenta(Aluno, _X).

/* 9. c) */

colega(_Aluno1, _Aluno2) :-
    aluno(_Aluno1, _Cadeira),
    aluno(_Aluno2, _Cadeira),
    frequenta(_Aluno1, _Universidade),
    frequenta(_Aluno2, _Universidade),
    Aluno1 \== Aluno2.

colega(_Professor1, _Professor2) :-
    funcionario(_Professor1, _Universidade),
    funcionario(_Professor2, _Universidade),
    _Professor1 \== _Professor2.
