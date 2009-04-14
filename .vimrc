set nocompatible
filetype plugin indent on
set bg=dark
set history=1000
set laststatus=2
set listchars=tab:>-
set scrolloff=4
set shortmess+=I
set showcmd
set wildmode=longest,list,full
syn on

set expandtab
set shiftwidth=4
set softtabstop=4
command! T setlocal noexpandtab shiftwidth=8 softtabstop=0
command! S setlocal expandtab shiftwidth=4 softtabstop=4
au BufNewFile,BufRead */linux-2.6/* T

let g:debchangelog_fold_enable = 1
let g:debcontrol_fold_enable = 1
let g:xml_syntax_folding = 1
au FileType xml setlocal foldmethod=syntax

hi link localWhitespaceError Error
au Syntax * syn match localWhitespaceError /\(\zs\%#\|\s\)\+$/ display
au Syntax * syn match localWhitespaceError / \+\ze\t/ display

function! s:headertemplate(name)
    let guard=toupper(substitute(substitute(a:name, '[^[:alnum:]]', '_', 'g'), '^[^[:alpha:]]', '_&', ''))
    return "#ifndef " . guard . "\n#define " . guard . "\n\n\n\n#endif /* " . guard . " */"
endfunction
au BufNewFile *.h s@^$@\=s:headertemplate(expand('<afile>'))@ | 4

iabbrev jj Josh Triplett and Jamey Sharp
iabbrev cjj Commit by Josh Triplett and Jamey Sharp
