# Design
- [x] be able to type in an input box
- [x] upon text change the options dynamically update
- [x] selections are searched by looking for git directories
- [x] making a selection opens kitty
- [x] be able to select the option with arrow keys + enter
- [x] update to use Janestreet Core instead of the stdlib
- [x] open a kitty window in the selected directory
    - [x] optionally open a kitty tab?
- [ ] pass in the starting code directory as a command line flag
- [x] close after selection
- [x] create an executable and create a hot key to run the script
- [ ] optimized fuzzy search accuracy
    - [x] smith waterson algo
    - [ ] match dirs
    - [ ] account for underscore and camel case
- [ ] sexp config file in ~/.config/project-manager
    - [ ] asks user for values if none are set
    - [ ] os specific / fallbacks
    - [ ] optionally can be json
- [ ] create a log file (somewhere?)
- [ ] use socket to open kitty tab (should account for -pid suffix on file name)

# glitches
- [x] sorting doesn't change actual selection
- [x] app should close when user selects option
- [ ] app should only calc on input change

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


