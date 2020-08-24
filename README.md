# diplo

A haskell implementation of a [diplomacy] order checker. Can be used to validate and execute orders. Also contains functionality to initialize and validate the board and the game state.

Does not contain functionality to resolve which orders get executed and which do not (due to another order with more supporting units for example).

## Install
Can be installed with [stack] by running

    stack install diplo


## Orders

Some examples of correct orders in the movement phase:

```
Aus F Tri holds
Ita A Ven supports (Aus F Tri holds)
Aus A Vie-Gal
Aus A Bud supports (Aus A Vie-Gal)
Eng F NTH convoys Eng A from Lon to Kie
Eng F HEL convoys Eng A from Lon to Kie
Eng A Lon via NTH HEL to Kie
```

Retreat phase:

```
Ita A Ven retreats to Pie
Aus F Vie disbands 
```

Build/disband phase:

```
Aus builds Tri F
Ita disbands Nap
```


## Examples

```bash
$ # check functionality
$ diplo-exe
usage: diplo-exe (init | board | state | orders | retreat-orders | build-orders)

$ diplo-exe init
usage: diplo-exe init (board | state | all)

$ diplo-exe init all

$ # Validate generated state - this prints the state back to the user if the file is valid
$ diplo-exe state check state.txt
Spring 1901, status:

Provinces:
Tri, occupied by Austria Fleet
Bud, occupied by Austria Army
Vie, occupied by Austria Army
Edi, occupied by England Fleet
Lon, occupied by England Fleet
Lvp, occupied by England Army
Bre, occupied by France Fleet
...

$ diplo-exe orders
usage: diplo-exe orders (check | echo | execute) orders.txt

$ # Validate orders w.r.t. board.txt and state.txt
$ # prints the orders back if they are all valid
$ cat orders1.txt
Aus A Vie-Tyr
Aus F Tri holds
Aus A Bud holds
Ger F Kie-Den
Ger A Ber-Kie

$ diplo-exe orders check orders1.txt
Austria Fleet Tri holds
Austria Army Bud holds
Austria Army Vie to Tyr
Germany Fleet Kie to Den
Germany Army Ber to Kie


$ # Typo in the unit/province, Austria has armies only in Bud and Vie
$ cat order_error1.txt
Aus F Vie-Gal
$ diplo-exe orders check orders2.txt
orders2.txt:2:7: error: expected: "Bud", "Vie"
2 | Aus A Tyr supports (Aus A Tri-Ven) 
  |       ^                            


$ # Typo in the order
$ cat order_error2.txt
Aus F Tri hlds
$ diplo-exe orders check order_error2.txt
order_error2.txt:1:10: error: expected: " convoys", " holds", " supports",
    " to", "-"
1 | Aus F Tri hlds 
  |          ^     


$ # Execute some orders
$ diplo-exe orders execute orders1.txt > state1.txt
$ mv state1.txt state.txt

$ # attack, Fall 1901
$ cat orders2.txt
Aus F Tri-Ven
Aus A Tyr supports (Aus A Tri-Ven)
Tur A Smy-Syr
Tur F Ank-BLA
$ diplo-exe orders execute orders2.txt > state2.txt
$ mv state2.txt state.txt

$ # retreat, Fall 1901
$ cat orders3.txt
Ita A Ven disbands
Ita A Ven retreats to Pie
$ diplo-exe retreat-orders execute orders3.txt > state3.txt
$ mv state3.txt state.txt

$ # build, Winter 1901
$ cat orders4.txt
Ita disbands Nap
Aus builds Tri F
$ diplo-exe build-orders execute orders4.txt > state4.txt
$ mv state4.txt state.txt

$ cat state.txt
Spring 1902, status:

Provinces:
Tri, occupied by Austria Fleet
Ven, occupied by Austria Fleet
Bud, occupied by Austria Army
Tyr, occupied by Austria Army
Edi, occupied by England Fleet
Lon, occupied by England Fleet
Lvp, occupied by England Army
...
```

## Todo

- Tests and CI
- Use RIO for IO
- Functionality to infer which orders will be executed
- Better error messages, e.g. when trying to issue a support order when no units are nearby

[diplomacy]: https://en.wikipedia.org/wiki/Diplomacy_(game)
[stack]: https://github.com/commercialhaskell/stack

