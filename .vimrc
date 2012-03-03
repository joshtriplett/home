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
syntax on

set expandtab
set shiftwidth=4
set softtabstop=4
function Spaces(...)
    if a:0 == 1
        let l:width = a:1
    else
        let l:width = 4
    endif
    setlocal expandtab
    let &l:shiftwidth = l:width
    let &l:softtabstop = l:width
endfunction
command! T setlocal noexpandtab shiftwidth=8 softtabstop=0
command! -nargs=? S call Spaces(<args>)
autocmd BufNewFile,BufRead ~/src/linux-2.6/* T
autocmd BufNewFile,BufRead ~/src/git/* T
autocmd FileType html S 2
autocmd FileType tex S 2

let g:debchangelog_fold_enable = 1
let g:debcontrol_fold_enable = 1
let g:xml_syntax_folding = 1
autocmd FileType xml setlocal foldmethod=syntax

let g:netrw_dirhistmax = 0

highlight link localWhitespaceError Error
autocmd Syntax * syntax match localWhitespaceError excludenl /\s\+\%#\@<!$\| \+\ze\t/ display containedin=ALL

function! s:headertemplate(name)
    let guard=toupper(substitute(substitute(a:name, '[^[:alnum:]]', '_', 'g'), '^[^[:alpha:]_]', '_&', ''))
    return "#ifndef " . guard . "\n#define " . guard . "\n\n\n\n#endif /* " . guard . " */"
endfunction
autocmd BufNewFile *.h s@^$@\=s:headertemplate(expand('<afile>:t'))@ | 4

iabbrev jj Josh Triplett and Jamey Sharp
iabbrev cjj Commit by Josh Triplett and Jamey Sharp
