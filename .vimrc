set nocompatible
filetype plugin indent on
set bg=dark
set diffopt+=vertical
set fillchars=vert:\ " (space)
set hlsearch
set laststatus=2
set listchars=tab:>-
set noincsearch
set nomodeline
set mouse=
set ruler
set scrolloff=6
set shortmess+=I
set showcmd
set viminfo=
set wildmode=longest,list,full
set wildmenu
set winheight=5
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
autocmd BufNewFile,BufRead ~/src/linux/* T
autocmd BufNewFile,BufRead ~/src/git/* T
autocmd FileType html S 2
autocmd FileType tex S 2

set ttimeout
set ttimeoutlen=100
set <C-Left>=[1;5D
set <C-Right>=[1;5C

runtime ftplugin/man.vim
runtime macros/matchit.vim

let g:debchangelog_fold_enable = 1
let g:debcontrol_fold_enable = 1
let g:ft_man_open_mode = "vert"

let g:xml_syntax_folding = 1
autocmd FileType xml setlocal foldmethod=syntax

let g:netrw_dirhistmax = 0

highlight DiffText ctermbg=DarkRed

highlight link localWhitespaceError Error
autocmd Syntax * syntax match localWhitespaceError excludenl /\s\+\%#\@<!$\| \+\ze\t/ display containedin=ALL

autocmd FileType gitcommit,mail call s:gitabbrevs()
function s:gitabbrevs()
    iabbrev <buffer> sb Signed-off-by: <C-R>=$NAME . " <" . $EMAIL . ">"<CR>
    iabbrev <buffer> ab Acked-by: <C-R>=$NAME . " <" . $EMAIL . ">"<CR>
    iabbrev <buffer> rb Reviewed-by: <C-R>=$NAME . " <" . $EMAIL . ">"<CR>
endfunction

noremap <C-n> <Cmd>cnext<CR>
noremap <C-p> <Cmd>cprev<CR>

if has('nvim')
    let &packpath=&runtimepath
    set shada=

    tnoremap <Esc> <C-\><C-n>
    autocmd TermOpen * setlocal statusline=%{b:term_title}
    autocmd TermOpen * startinsert
endif
