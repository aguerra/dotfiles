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
Plugin 'vim-airline/vim-airline-themes'
Plugin 'sjl/gundo.vim'
Plugin 'rking/ag.vim'

call vundle#end()
filetype plugin indent on

" Look and feel
syntax on
set background=dark
set number
set relativenumber
set cursorline
set laststatus=2
set guioptions-=T
set guioptions-=m
set guifont=Ubuntu\ Mono\ 12
let python_highlight_all = 1

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
set completeopt=menu,preview,longest
set clipboard=unnamed,unnamedplus
set showcmd
set cryptmethod=blowfish2
set foldenable
set foldlevelstart=10
set foldnestmax=10
let g:is_posix = 1  " Fix shell command substitution syntax
let mapleader = ','
let &makeprg .= ' -j' . system('getconf _NPROCESSORS_ONLN')
set tags=./tags,tags

" Plugins options
let g:ctrlp_working_path_mode = 0
let g:go_disable_autoinstall = 1
let g:gundo_prefer_python3 = 1
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
let g:UltiSnipsExpandTrigger = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
let g:UltiSnipsJumpForwardTrigger = '<c-j>'
let g:ultisnips_python_style = 'sphinx'
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_server_python_interpreter = '/usr/bin/python3'

" Tagbar gotags integration
let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }

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
nnoremap <silent> <leader>v :call ActivateVenv()<cr>

nnoremap gV `[v`]
nnoremap j gj
nnoremap k gk
nnoremap <f4> :GundoToggle<cr>
nnoremap <f6> :NERDTreeFind<cr>
nnoremap <leader>b :Ag
nnoremap <leader>s :mksession

" Functions

function! ActivateVenv()
    let l:venv = input('Virtualenv: ')
    let l:dir = $HOME . '/.virtualenvs'
    if !empty($WORKON_HOME)
        let l:dir = $WORKON_HOME
    endif
    let l:dir .=  '/' . l:venv . '/bin'
    let $PATH .= ':' . l:dir
    let l:python = l:dir . '/python'
    execute 'YcmCompleter RestartServer ' . l:python
endfunction

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
