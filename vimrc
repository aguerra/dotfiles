" vimrc

" Required
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim' " required
Plugin 'a.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'sirver/ultisnips'
Plugin 'fatih/vim-go'

" Required
call vundle#end()
filetype plugin indent on

" Look and feel
syntax on
set background=dark
set number
set laststatus=2
highlight colorcolumn ctermbg=magenta

" statusline
set statusline=%<%f\ %y\ %h%m%r
set statusline+=\ %{tagbar#currenttag('[%s]','')}
set statusline+=\ %{fugitive#statusline()}
set statusline+=\ (%c%V,\ %P)
set statusline+=\ [%p%%,\ %L\ lines]

" Misc options
set showmatch
set textwidth=79
set ignorecase
set colorcolumn=+1
set listchars=tab:>_,trail:. 
set list
let g:is_posix = 1 " Fix shell command substitution syntax
set completeopt=menu,preview,longest

" Indentation
set shiftwidth=8
set softtabstop=8

" Make customization
if isdirectory('build')
	let &makeprg .= ' -C build'
endif
if filereadable('/proc/cpuinfo')
	let &makeprg .= ' -j' . (system('grep -c ^processor /proc/cpuinfo'))
endif

nnoremap <Leader>m :silent make!<CR>: echo 'make done'<CR>
autocmd quickfixcmdpost make botright cwindow 15
autocmd filetype qf call UnsetNuAndCc()

" tagbar
"autocmd filetype * nested :call tagbar#autoopen(0)
"let g:tagbar_width = 35

" Tags
set tags=./tags,tags
autocmd filetype python set tags+=~/tags/python
autocmd filetype c,cpp  set tags+=~/tags/c
autocmd filetype cpp    set tags+=~/tags/cpp

" Misc autocmds
autocmd filetype help call UnsetNuAndCc()
autocmd cursormovedi * if pumvisible() == 0 | pclose | endif
autocmd insertleave  * if pumvisible() == 0 | pclose | endif

" Misc mappings
inoremap <silent> jj        <Esc>
nnoremap <silent> <Leader>t :call Ctags()<CR>
nnoremap <silent> <Leader>q :q<CR>
inoremap <silent> <C-O>     <C-X><C-O>
noremap  <silent> <C-Down>  <C-W>j
noremap  <silent> <C-Up>    <C-W>k
noremap  <silent> <C-Left>  <C-W>h
noremap  <silent> <C-Right> <C-W>l
nnoremap <silent> <Leader>a :A<CR>

" Functions
function! Ctags()
	let output = system('ctags')
	if !empty(output)
		echo output
	else
		echo 'ctags done'
	endif
endfunction

function! UnsetNuAndCc()
	setlocal nonumber
	setlocal colorcolumn=
endfunction
