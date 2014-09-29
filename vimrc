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
Plugin 'tomasr/molokai'
Plugin 'bling/vim-airline'

" Required
call vundle#end()
filetype plugin indent on

" Look and feel
syntax on
set title
set background=dark
set number
set cursorline
set laststatus=2
set guioptions-=T
set guioptions-=r
set guifont=DejaVu\ Sans\ Mono\ 11

if has('gui_running')
	colorscheme molokai
endif

" Misc options
set hidden
set history=1000
set wildmenu
set showmatch
set textwidth=79
set ignorecase
set smartcase
set scrolloff=3
set colorcolumn=+1
set directory=~/.vim-tmp,~/tmp,/var/tmp,/tmp
set hlsearch
set incsearch
set visualbell
set shortmess=atI
set listchars=tab:..
set list
let g:is_posix = 1 " Fix shell command substitution syntax
set completeopt=menu,preview,longest

" Make customization
if isdirectory('build')
	let &makeprg .= ' -C build'
endif
if filereadable('/proc/cpuinfo')
	let &makeprg .= ' -j' . (system('grep -c ^processor /proc/cpuinfo'))
endif

nnoremap <Leader>m :silent make!<CR>: echo 'make done'<CR>
autocmd quickfixcmdpost make botright cwindow
autocmd filetype qf call UnsetNuAndCc()

" ctrlp
let g:ctrlp_cmd = 'CtrlPMRU'

" ycm
let g:ycm_collect_identifiers_from_tags_files = 1

" airline
let g:airline#extensions#whitespace#checks = [ 'trailing' ]
let g:airline#extensions#tabline#enabled = 1

" Tags
set tags=./tags,tags
autocmd filetype python set tags+=~/tags/python
autocmd filetype c,cpp  set tags+=~/tags/c
autocmd filetype cpp    set tags+=~/tags/cpp

" Misc autocmds
autocmd filetype help call UnsetNuAndCc()
autocmd cursormovedi * if pumvisible() == 0 | pclose | endif
autocmd insertleave  * if pumvisible() == 0 | pclose | endif
autocmd guienter * call system('wmctrl -i -b add,maximized_vert,maximized_horz -r '.v:windowid)

" Misc mappings
nnoremap <silent> <Leader>t  :call Ctags()<CR>
nnoremap <silent> <Leader>q  :q<CR>
noremap  <silent> <C-Down>   <C-W>j
noremap  <silent> <C-Up>     <C-W>k
noremap  <silent> <C-Left>   <C-W>h
noremap  <silent> <C-Right>  <C-W>l
nnoremap <silent> <Leader>a  :A<CR>
nnoremap <silent> <Leader>gs :Gstatus<CR>
nnoremap <silent> <Leader>gd :Gdiff<CR>

" vim-go
let g:go_disable_autoinstall = 1

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
