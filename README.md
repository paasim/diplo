# diplo

A haskell implementation of a [diplomacy] order checker. Can be used to validate movement phase orders. Also contains functionality to initialize and validate the board and the game state.

## Install
Can be installed with [stack] by running

    stack install diplo


## Orders

Some examples of correct orders:

```
Aus F Tri holds
Ita A Ven supports (Aus F Tri holds)
Aus A Vie-Gal
Aus A Bud supports (Aus a Vie-Gal)
Eng F NTH convoys Eng A from Lon to Kie
Eng F HEL convoys Eng A from Lon to Kie
Eng A Lon via NTH HEL to Kie
```


## Examples

```bash
$ # Initialize board and state, this writes two files, board.txt and state.txt
$ diplo-exe --init

$ # Validate generated state - this prints the state back to the user if the file is valid
$ diplo-exe --state state.txt
1901 Spring, status:

Spaces:
Ank, occupied by Turkey Fleet
Ber, occupied by Germany Army
Bre, occupied by France Fleet
...


$ # Validate orders w.r.t. board.txt and state.txt - prints the orders back if they are all valid
$ head orders.txt
Aus A Vie-Gal
Aus A Bud supports (Aus A Vie-Gal)
Aus F Tri holds
Ger F Kie-Den
Ger A Ber-Kie
$ diplo-exe --orders orders.txt
Aus A Vie-Gal
Aus A Bud supports (Aus A Vie-Gal)
Aus F Tri holds
Ger F Kie-Den
Ger A Ber-Kie
...

$ # Typo in the unit type, a fleet cannot be in Vienna
$ cat orders2.txt
Aus F Vie-Gal
$ diplo-exe --orders orders2.txt
ErrInfo {_errDoc = orders2.txt:1:7: error: expected: "ADR", "AEG", "Alb", "Ank", "Apu", "Arm",
"BAL", "BAR", "BLA", "BOT", "Bel", "Ber", "Bre", "BulEC", "BulSC", "Cly",
"Con", "Den", "EAS", "ENG", "Edi", "Fin", "Gas", "Gre", "HEL", "Hol", "ION",
"IRI", "Kie", "LYO", "Lon", "Lvn", "Lvp", "MAO", "Mar", "NAO", "NTH", "NWG",
"Naf", "Nap", "Nwy", "Pic", "Pie", "Por", "Pru", "Rom", "Rum", "SKA", "Sev",
"Smy", "SpaNC", "SpaSC", "StpNC", "StpSC", "Swe", "Syr", "TYS", "Tri",
"Tun", "Tus", "Ven", "WES", "Wal", "Yor"
1 | Aus F Vie-Gal
  |       ^       , _errDeltas = [Directed "orders2.txt" 0 6 6 6]}

$ # Typo in the order
$ cat orders3.txt
Aus F Tri hlds
$ diplo-exe --orders orders3.txt
ErrInfo {_errDoc = orders3.txt:1:10: error: expected: " convoys ", " holds", " supports (", " to ",
    " via ", "-"
1 | Aus F Tri hlds
  |          ^     , _errDeltas = [Directed "orders3.txt" 0 9 9 9]}

$ # Trying to issue an order for a unit that does not occupy the given space
$ cat orders4.txt
Eng A Ank holds
$ diplo-exe --orders orders4.txt
Validation errors:
Unit 'England Army' does not occupy 'Ank, Coast'.
```

## Todo

- Functionality to read and validate orders in other phases than movement phase
- Functionality to infer which orders will be executed
- Functionality to update state according to orders
- Use RIO for IO
- Better error messages
- Tests and CI

[diplomacy]: https://en.wikipedia.org/wiki/Diplomacy
[stack]: https://github.com/commercialhaskell/stack

