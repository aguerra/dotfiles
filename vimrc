" vimrc

" Required
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
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
Plugin 'honza/vim-snippets'
Plugin 'nvie/vim-flake8'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'

call vundle#end()
filetype plugin indent on

" Look and feel
let python_highlight_all = 1
syntax on
set background=dark
set number
set relativenumber
set cursorline
set laststatus=2
set guioptions-=T
set guioptions-=m
set guifont=Ubuntu\ Mono\ 12

if has('gui_running')
	colorscheme molokai
endif

" Misc options
set hidden
set history=1000
set wildmenu
set showmatch
set textwidth=99
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
set clipboard=unnamed,unnamedplus
set showcmd

if v:version == 704 && has('patch399')
	set cryptmethod=blowfish2
else
	set cryptmethod=blowfish
endif

" Folding
set foldenable
set foldlevelstart=3
set foldnestmax=10

" Make customization
let &makeprg .= ' -j' . system('getconf _NPROCESSORS_ONLN')

" ycm
let g:ycm_collect_identifiers_from_tags_files = 1

" airline
let g:airline#extensions#default#layout = [['a', 'b', 'c', 'x', 'y', 'z'], ['warning']]

" vim-go
let g:go_disable_autoinstall = 1

" vim-snippets
let g:ultisnips_python_style = "google"

" ultisnips
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

" NERDTree
let NERDTreeIgnore = ["\.pyc$"]

" syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_quiet_messages = {"type": "style"}

" Tags
set tags=./tags,tags

" Autocmds
autocmd quickfixcmdpost make botright cwindow
autocmd filetype help,qf call UnsetNumberAndColorColumn()
autocmd completedone * pclose
autocmd guienter * call system('wmctrl -i -b add,maximized_vert,maximized_horz
                               \ -r ' . v:windowid)
autocmd vimenter * nested :TagbarOpen
autocmd vimenter * NERDTree | wincmd p
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q |
                   \ endif

augroup config_languages
    autocmd!
    autocmd filetype python setlocal foldmethod=indent
augroup END

" Mappings
nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>fq :q!<cr>
noremap  <silent> <c-down>   <c-w>j
noremap  <silent> <c-up>     <c-w>k
noremap  <silent> <c-left>   <c-w>h
noremap  <silent> <c-right>  <c-w>l
nnoremap <silent> <leader>a  :A<cr>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>gc :Gcommit<cr>
nnoremap <silent> <leader>sp :setlocal spell spelllang=pt_br<cr>
nnoremap <silent> <leader>se :setlocal spell spelllang=en_us<cr>
nnoremap <silent> <leader>d  :YcmCompleter GoToDefinitionElseDeclaration<cr>
nnoremap <silent> <leader>/  :nohlsearch<cr>
noremap  <silent> <f2>       :set invpaste<cr>
nnoremap <silent> <leader>ev :e  $MYVIMRC<cr>
nnoremap <silent> <leader>rv :so $MYVIMRC<cr>
nnoremap <silent> <leader><space> za

" Functions
function! GenerateCtags()
	let output = system('ctags')
	if empty(output)
		echo 'ctags done'
	else
		echo output
	endif
endfunction

function! UnsetNumberAndColorColumn()
	setlocal nonumber
	setlocal colorcolumn=
endfunction
