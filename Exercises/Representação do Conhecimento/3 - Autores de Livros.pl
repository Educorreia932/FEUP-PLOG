/* Nacionality */

nacionality('Eça de Queiroz', 'portuguese').
nacionality('Camilo Castelo Branco', 'portuguese').
nacionality('George Orwell', 'english').
nacionality('Fernando Pessoa', 'portuguese').
nacionality('J. R. R. Tolkien', 'english').

/* Genre */ 
genre('Os Maias', 'romance').
genre('Amor de Perdição', 'romance').
genre('1984', 'novel').
genre('Mensagem', 'poetry').
genre('Lord of the Rings', 'fiction').

/* Author */

author('Os Maias', 'Eça de Queiroz').
author('Amor de Perdição', 'Camilo Castelo Branco').
author('1984', 'George Orwell').
author('Mensagem', 'Fernando Pessoa').
author('Lord of the Rings', 'J. R. R. Tolkien').

/* 3. a) */

?- author('Os Maias', X).

/* 3. b) */

?- nacionality(X, 'portuguese'), author(_Y, X), genre(_, 'romance').

/* 3. c) */

?- genre(_X, 'fiction'), author(_X, Y), author(_Z, Y), \+genre(_Z, 'fiction').
