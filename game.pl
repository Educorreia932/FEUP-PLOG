play()
display_game(+GameState, +Player)
initial(-GameState)
valid_moves(+GameState, +Player, -ListOfMoves).
move(+GameState, +Move, -NewGameState).
game_over(+GameState, -Winner).
value(+GameState, +Player, -Value).
choose_move(+GameState, +Player, +Level, -Move).

% Initial Configuration of Board
initialBoard([
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e],
    [e, e, e, e, e, e]
]).

piece(e, ' '). % Empty
piece(b, 'B'). % Black
piece(g, 'G'). % Green
piece(w, 'W'). % White    
