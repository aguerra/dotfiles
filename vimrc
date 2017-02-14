" vimrc

" Auto install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.vim/plugged')

" Plugin list
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
set foldmethod=syntax                        " no manual folding
set foldnestmax=10                           " maximum nesting of folds
set hidden                                   " hide abandoned buffers
set history=1000                             " for commands and search patterns
set hlsearch                                 " highlight matches
set ignorecase                               " in search patterns
set incsearch                                " update screen with the results
set list                                     " show tabs and end of lines
set listchars=tab:..                         " only show tabs
" Search directory of the current file, current dir and down into subfolders
set path=.,,**
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
let g:ackprg = 'ag --vimgrep' " ag is faster than ack

" golang beautification
let g:go_highlight_build_constraints = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1

" remap for compatibility with ycm
let g:UltiSnipsExpandTrigger = '<C-j>'
let g:UltiSnipsJumpForwardTrigger = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'

let g:ultisnips_python_quoting_style = 'single'

let g:ycm_collect_identifiers_from_tags_files = 1        " use tag files
let g:ycm_server_python_interpreter = '/usr/bin/python3' " no more python 2

let NERDTreeIgnore = [
  \'\.pyc$',
  \'__pycache__',
  \]

" Autocmds
augroup action
  autocmd!
  autocmd completedone * pclose
  autocmd guienter * call system(
    \'wmctrl -i -b add,maximized_vert,maximized_horz -r ' . v:windowid
    \)
  autocmd quickfixcmdpost make botright cwindow
  autocmd vimenter * NERDTree | wincmd p
augroup END

augroup config
  autocmd!
  autocmd filetype help,qf call UnsetNumberAndColorColumn()
augroup END

augroup c
  autocmd filetype c nnoremap <leader>h :A<cr>
augroup END

augroup go
  autocmd filetype go nmap <leader>b <plug>(go-build)
  autocmd filetype go nmap <leader>r <plug>(go-run)
augroup END

augroup python
  autocmd!
  autocmd filetype python setlocal foldmethod=indent
  autocmd filetype python nnoremap <silent> <leader>v :call ActivateVenv()<cr>
augroup END

" Mappings
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-m> :cprevious<cr>
noremap <C-n> :cnext<cr>

nnoremap <f2> :set invpaste<cr>
nnoremap <f3> :nohlsearch<cr>
nnoremap <f5> :setlocal spell!<cr>
nnoremap <f6> :NERDTreeFind<cr>

" move to next row
nnoremap j gj
nnoremap k gk

nnoremap <leader>a :cclose<cr>
nnoremap <leader>b :buffer<space>
nnoremap <leader>f :find<space>

nnoremap <leader>c :Ack!<space>
nnoremap <leader>d :YcmCompleter GoToDefinitionElseDeclaration<cr>

nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>gc :Gcommit<cr>
nnoremap <silent> <leader>ev :e  $MYVIMRC<cr>
nnoremap <silent> <leader>rv :so $MYVIMRC<cr>
nnoremap <silent> <leader><space> za

nnoremap gV `[v`]
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
