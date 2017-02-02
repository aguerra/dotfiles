" vimrc

" Auto install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.vim/plugged')

" Plugin list
Plug 'ctrlpvim/ctrlp.vim', { 'tag': '1.80' }
Plug 'fatih/vim-go', { 'tag': 'v1.10' }
Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar', { 'tag': 'v2.6.1' }
Plug 'mileszs/ack.vim', { 'tag': '1.0.9' }
Plug 'nvie/vim-flake8', { 'tag': '1.6' }
Plug 'scrooloose/nerdtree', { 'tag': '5.0.0' }
Plug 'sirver/ultisnips', { 'tag': '3.1' }
Plug 'tomasr/molokai'
Plug 'tpope/vim-fugitive', { 'tag': 'v2.2' }
Plug 'tpope/vim-surround', { 'tag': 'v2.1' }
Plug 'valloric/youcompleteme'
Plug 'vim-airline/vim-airline', { 'tag': 'v0.8' }
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/a.vim', { 'tag': '2.18' }

call plug#end()

" Look and feel
autocmd insertEnter,insertLeave * set cursorline! " highlight on insert mode

if has('gui_running')
  colorscheme molokai
endif

let python_highlight_all = 1 " enable all features

set background=dark
set guifont=Ubuntu\ Mono\ 12
set guioptions-=T            " disable the tool bar
set laststatus=2             " always show the status line
set number                   " show line numbers
set relativenumber           " show number relative to the line with the cursor

" General options
let g:is_posix = 1  " fix shell command substitution syntax
let mapleader = ',' " define new <Leader>

let &makeprg .= ' -j' . system('getconf _NPROCESSORS_ONLN') " parallel make

set clipboard=unnamed,unnamedplus            " use + register for copy-paste
set colorcolumn=+1                           " highlight textwidth + 1
set completeopt=menu,preview,longest         " menu + doc + longest common text
set cryptmethod=blowfish2                    " strong encryption
set directory=~/.vim-tmp,~/tmp,/var/tmp,/tmp " dirs for the swap file
set foldlevelstart=10                        " initial fold level
set foldnestmax=10                           " maximum nesting of folds
set hidden                                   " hide abandoned buffers
set history=1000                             " for commands and search patterns
set hlsearch                                 " highlight matches
set ignorecase                               " in search patterns
set incsearch                                " update screen with the results
set list                                     " show tabs and end of lines
set listchars=tab:..                         " only show tabs
set scrolloff=3                              " min lines above and below cursor
set shell=/bin/sh                            " use posix shell
set shortmess+=filmnrxoOtT                   " helps to avoid hit-enter prompts
set showcmd                                  " show partial commands in st line
set showmatch                                " show matching brackets/parens
set smartcase                                " case sensitive when uc present
set spelllang=en_us,pt_br                    " word list names
set tags=./tags,tags                         " dir of the cur file and cur dir
set textwidth=79                             " max width of text inserted text
set visualbell                               " visual bell instead of beeping
set wildmenu                                 " cmdline completion enhanced mode

" Plugins options
let g:ctrlp_cmd = 'CtrlPBuffer' " open in find buffer mode

" remap for compatibility with ycm
let g:UltiSnipsExpandTrigger = '<C-j>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

let g:ycm_collect_identifiers_from_tags_files = 1        " use tag files
let g:ycm_server_python_interpreter = '/usr/bin/python3' " no more python 2

let NERDTreeIgnore = [
  \'\.pyc$',
  \'__pycache__',
  \]

" Autocmds
augroup config_languages
    autocmd!
    autocmd filetype python setlocal foldmethod=indent
augroup END

autocmd completedone * pclose
autocmd filetype help,qf call UnsetNumberAndColorColumn()
autocmd guienter * call system('wmctrl -i -b add,maximized_vert,maximized_horz
                               \ -r ' . v:windowid)
autocmd quickfixcmdpost make botright cwindow
autocmd vimenter * NERDTree | wincmd p

" Mappings
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <silent> <f2> :set invpaste<cr>
nnoremap <silent> <f3> :nohlsearch<cr>
nnoremap <silent> <f4> :GundoToggle<cr>
nnoremap <silent> <f5> :setlocal spell!<cr>
nnoremap <silent> <f6> :NERDTreeFind<cr>

nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>fq :q!<cr>
nnoremap <silent> <leader>a  :A<cr>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>gc :Gcommit<cr>
nnoremap <silent> <leader>d  :YcmCompleter GoToDefinitionElseDeclaration<cr>
nnoremap <silent> <leader>ev :e  $MYVIMRC<cr>
nnoremap <silent> <leader>rv :so $MYVIMRC<cr>
nnoremap <silent> <leader><space> za
nnoremap <silent> <leader>v :call ActivateVenv()<cr>

nnoremap gV `[v`]
nnoremap j gj
nnoremap k gk
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
