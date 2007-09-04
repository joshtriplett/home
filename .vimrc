filetype plugin indent on
set bg=dark
set laststatus=2
set showcmd
syn on

" Make the up and down arrow keys move based on displayed lines rather than
" physical lines.
map <Up> gk
imap <Up> <C-o>gk
map <Down> gj
imap <Down> <C-o>gj
