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

team_victory(Team, Circuit) :- 
    pilot_victory(Pilot, Circuit), team(Pilot, Team).

/* 2. a) */

?- pilot_victory('Porto', X).

/* 2. b) */

?- team_victory('Porto', X).

/* 2. c) */

?- pilot_victory(Circuit1, Pilot), pilot_victory(Circuit2, Pilot), Circuit1 \== Circuit2.

/* 2. d) */

?- gates(Circuit, X), X > 8.

/* 2. e) */

?- \+plane(Pilot, 'Edge540').