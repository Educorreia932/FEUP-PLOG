# Green-Greener-Greenest ⚪🟢⚫

![Green-Greener-Greenest Title](https://i.imgur.com/4i4ARI1.png)

## PLOG Project

This project was developed by *Greener1 / Class 4*, of which the members are:

- Ana Inês Oliveira de Barros (*up201806593*)
- Eduardo da Costa Correia (*up201806433*)

## Material

**Basic:** a 6×6 board, 15 black pyramids, 20 green pyramids, 15 white pyramids.

## Definition

A **stack** is either one pyramid or several pyramids stacked on top of each other.
It is controlled by the colour of the topmost pyramid. 
So a White ⚪ stack is a stack of any height with a white pyramid on top, and so on...
![Green Piece](images/GreenPiece.png) ![White Stack](images/WhiteStack.png)

## Greener
![Greener Title](images/Greener.png)

**Greener** is a capturing game for 2 players, where both must capture the same colour.

### Setup

The board starts full of pyramids, randomly placed. 
Each player has an allocated colour (Black ⚫ / White ⚪) and they control every piece or stack of their respective color.
Green 🟢 is a neutral colour.

![Setup Example](https://i.imgur.com/1dL0lk8.png)

### How to play

Players take turns (starting by Black ⚫) capturing pyramids or stacks of any colour orthogonally (on the same row or collumn and with no stacks between them).
On your turn you must make one capture if possible, otherwise you pass the turn.
The game ends when all players pass in succession.
The player with the most green  pyramids captured (being part of stacks they control) wins the game. 
In case of a tie, the player with the highest stack wins. If the tie persists, play again.

### Gameplay Overview

Here's an example of a typical turn in the game.
In this case, it's the black player's turn, so they could move either the stack **c** or **a**.
In this case, they could capture the stack  **b**, **c**,, **d** or **e**, but not **f** because **e** is in between.
Capturing **b** is the best move Black ⚫ can make, since it not only gives him one more green piece, it also forfeits White's ⚪ control of that stack.

![Capturing](https://i.imgur.com/uhzJ3N3.png)

## Useful Links

- [BoardGameGeek entry](https://boardgamegeek.com/boardgame/227145/greengreenergreenest)
- [Official Rules](https://nestorgames.com/rulebooks/GREENGREENERGREENEST_EN.pdf)

## Game Structure

To internally store the gamestate we used a list of lists that represents the board cells and the stacks in each one of them.  




## Game Representation

Our representation uses several Unicode characters and has to be monospaced font, so we recommend using Cascadia Mono, that already comes with the default SicStus installation.

We included some mockups for comparison and better visualization.

### Legend 

- △ - White Piece
- ◭ - Green Piece
- ▲ - Black Piece

![](images/1.png)
![](images/2.png)
![](images/3.png)
