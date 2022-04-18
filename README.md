# hangman

Another Hangman game written in Haskel.

```
Total words: 2882
Wins: 7 Fails: 1                                          +----+   
A B C   E     H     K   M N   P Q   S     V W X Y Z       |    |   
                                                          |    O   
_ _ _ r r _                                               |   /|\  
                                                          |   / \  
c h e r r y                                              _|_______ 
                                                        |_________|
Game Over!
Do you want to continue? [Y/n] 
```

## Install 

### From Source

```bash
make install
```

Add `~/.cabal/bin` into `PATH` environment variable (If not exists yet) and 
run:

```bash
hangman
```

## Help?

```bash
hangman --help
hangman -h
```

```bash
Usage: hangman [--dbfile FILENAME] [-s|--skill SKILL] [-c|--category SKILL]

  Hangman game

Available options:
  --dbfile FILENAME        Text file, one word per line
  -s,--skill SKILL         easy, medium, hard. (default: "medium")
  -c,--category SKILL      Categories: all, fruites, animals, colours, places,
                           time, weather, misc. (default: "fruites")
  -h,--help                Show this help text
```
