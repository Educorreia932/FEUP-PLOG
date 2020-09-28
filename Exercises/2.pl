/* Pilots (may be removed?) */

pilot('Lamb').
pilot('Besenyei').
pilot('Chambliss').
pilot('MacLean').
pilot('Mangold').
pilot('Jones').
pilot('Bonhomme').

/* Teams */

team('Breitling', 'Lamb').
team('Besenyei', 'Red Bull').
team('Chambliss', 'Red Bull').
team('MacLean', 'Meditarrean Racing Team').
team('Mangold ', 'Cobra').
team('Jones ', 'Matador').
team('Bonhomme ', 'Matador').

/* Planes */

plane('Lamb', 'MX2').
plane('Besenyei', 'Edge540').
plane('Chambliss', 'Edge540').
plane('MacLean', 'Edge540').
plane('Mangold ', 'Edge540').    
plane('Jones ', 'Edge540').
plane('Bonhomme ', 'Edge540').

/* Circuit (may be removed?) */ 

circuit('Istanbul').
circuit('Budapest').
circuit('Porto').

/* Pilot victories */

pilot_victory('Porto', 'Jones').
pilot_victory('Budapest', 'Mangold').
pilot_victory('Istanbul', 'Mangold').

/* Number of gates */

gates('Istanbul', 9).
gates('Budapest', 6).
gates('Porto', 5).

/* Team victory conditions */

team_victory(X, Y) :- 
    pilot_victory(X, Z), team(Z, X).

/* 2. a) */

?- pilot_victory('Porto', X).

/* 2. b) */

?- team_victory('Porto', X).

/* 2. c) */

/* 2. d) */

/* 2. e) */