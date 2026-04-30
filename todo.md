# Features

## misc
- [ ] create a log file (somewhere?)
- [ ] use socket to open kitty tab (should account for -pid suffix on file name)

## fuzzy search
- [x] smith waterson algo
- [ ] match dirs not just file name
- [ ] account for underscore, camel case, dashes, etc...
- [ ] typo resistent?

## config file
- [x] sexp config file in ~/.config/project-manager
- [ ] asks user for values if none are set
- [ ] os specific / fallbacks
- [ ] optionally can be json
- [x] starting code directory 
- [ ] include window size in config
- [ ] custom parser for sexp config so don't have to name every field

## Get Git Directories Improvements
- [ ] index/cache results
- [ ] asynrounesly parse the directories

# glitches
- [x] sorting doesn't change actual selection
- [x] app should close when user selects option
- [x] app should only calc on input change

# Big Ideas
## make a text editor box
- [ ] can change cursor location
- [ ] can backspace anywhere 
- [ ] can select text and copy and paste
- [ ] eventually use real text editor datastructure

## raycast-like program
- format json
- open bookmarks?
- list of commands / sub programs that you can run

## layout for program
- can create a DOM that uses CLAY's layout algorthm
- Knuth pass algo for text wrapping

## Full on text editor
- logseq like?

## libs/tech
- use [eio](https://github.com/ocaml-multicore/eio) of operation system IO
- maybe eventually use FFF instead


