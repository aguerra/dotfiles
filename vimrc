" vimrc

" Required
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'a.vim'
Plugin 'bling/vim-airline'
Plugin 'fatih/vim-go'
Plugin 'gmarik/Vundle.vim'
Plugin 'honza/vim-snippets'
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'nvie/vim-flake8'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'sirver/ultisnips'
Plugin 'sjl/gundo.vim'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'vim-airline/vim-airline-themes'

call vundle#end()
filetype plugin indent on

" Look and feel
syntax on
set background=dark
set cursorline
set guifont=Ubuntu\ Mono\ 12
set guioptions-=T
set guioptions-=m
set laststatus=2
set number
set relativenumber

let python_highlight_all = 1

if has('gui_running')
    colorscheme molokai
endif

" Misc options
set clipboard=unnamed,unnamedplus
set cryptmethod=blowfish2
set colorcolumn=+1
set completeopt=menu,preview,longest
set directory=~/.vim-tmp,~/tmp,/var/tmp,/tmp
set foldenable
set foldlevelstart=10
set foldnestmax=10
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set list
set listchars=tab:..
set scrolloff=3
set shortmess=atI
set showcmd
set showmatch
set smartcase
set textwidth=79
set visualbell
set wildmenu

let g:is_posix = 1  " Fix shell command substitution syntax
let &makeprg .= ' -j' . system('getconf _NPROCESSORS_ONLN')
let mapleader = ','
set tags=./tags,tags

" Plugins options
let g:ctrlp_working_path_mode = 0
let g:go_disable_autoinstall = 1
let g:gundo_prefer_python3 = 1
let g:UltiSnipsExpandTrigger = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
let g:UltiSnipsJumpForwardTrigger = '<c-j>'
let g:ultisnips_python_style = 'sphinx'
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_server_python_interpreter = '/usr/bin/python3'
let NERDTreeIgnore = ['\.pyc$', '__pycache__']

" Autocmds
augroup config_languages
    autocmd!
    autocmd filetype python setlocal foldmethod=indent
augroup END

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q |
                   \ endif
autocmd completedone * pclose
autocmd filetype help,qf call UnsetNumberAndColorColumn()
autocmd guienter * call system('wmctrl -i -b add,maximized_vert,maximized_horz
                               \ -r ' . v:windowid)
autocmd quickfixcmdpost make botright cwindow
autocmd vimenter * NERDTree | wincmd p
autocmd vimenter * nested :TagbarOpen

" Mappings
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>fq :q!<cr>
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
    execute 'YcmCompleter RestartServer ' . l:dir . '/python'
    echo ' done'
endfunction

function! GenerateCtags()
    let l:output = system('ctags')

    if empty(l:output)
        echo 'ctags done'
    else
        echo l:output
    endif
endfunction

function! UnsetNumberAndColorColumn()
    setlocal nonumber
    setlocal colorcolumn=
endfunction
