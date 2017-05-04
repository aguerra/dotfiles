" vimrc

" Auto install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.vim/plugged')

" Plugin list
Plug 'drgarcia1986/python-compilers.vim', {'for': 'python'}
Plug 'fatih/vim-go', {'tag': 'v1.11'}
Plug 'tomasr/molokai'
Plug 'tpope/vim-dispatch', {'tag': 'v1.4'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Look and feel
autocmd insertenter,insertleave * set cursorline! " highlight on insert mode
highlight cursorline cterm=none ctermbg=236

let python_highlight_all = 1 " enable all features

set background=dark
set laststatus=2    " always show the status line
set number          " show line numbers
set relativenumber  " show number relative to the line with the cursor

" General options
let mapleader = ',' " define new <leader>

" Parallel make
let &makeprg = 'make -j' . systemlist('getconf _NPROCESSORS_ONLN')[0]

set clipboard=unnamed,unnamedplus " use the system clipboard
set colorcolumn=+1                " highlight col after textwidth
set cryptmethod=blowfish2         " strong encryption
set directory=~/.vim-tmp          " for the swap files
set foldlevelstart=10             " initial fold level
set foldmethod=syntax             " for C style languages
set foldnestmax=10                " maximum nesting of folds
set grepprg=ag\ --vimgrep         " ag is faster than grep
set hidden                        " hide abandoned buffers
set history=1000                  " for commands and search patterns
set hlsearch                      " highlight matches
set ignorecase                    " in search patterns
set incsearch                     " update screen with the results
set list                          " show tabs and end of lines
set listchars=tab:..              " only show tabs
set path=.,,**                    " dir of the cur file, cur dir and subdirs
set scrolloff=3                   " min lines above and below cursor
set shell=/bin/sh                 " use posix shell
set shortmess+=filmnrxoOtT        " helps to avoid hit-enter prompts
set showcmd                       " show partial commands in status line
set showmatch                     " show matching brackets/parens
set smartcase                     " case sensitive when uppercase present
set spelllang=en_us,pt_br         " word lists names
set tags=./tags,tags              " dir of the current file and current dir
set textwidth=79                  " max width of inserted text
set visualbell                    " visual bell instead of beeping
set wildmenu                      " cmdline completion enhanced mode

" Plugins options
let g:go_fmt_command = 'goimports'
let g:go_get_update = 0
let g:go_highlight_build_constraints = 1
let g:go_highlight_functions = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1

" Autocmds
augroup action
  autocmd!
  autocmd completedone * pclose " close preview after completion
  autocmd guienter * call system(
    \'wmctrl -i -b add,maximized_vert,maximized_horz -r ' . v:windowid
  \)
  autocmd quickfixcmdpost grep,make botright cwindow " open full qf at bottom
augroup END

augroup config
  autocmd!
  autocmd filetype help,qf call UnsetNumbersAndColorColumn()
augroup END

augroup go
  autocmd!
  autocmd filetype go nmap <leader>c <plug>(go-build)
  autocmd filetype go nmap <leader>r <plug>(go-run)
augroup END

augroup python
  autocmd!
  autocmd bufwrite *.py :Dispatch
  autocmd filetype python compiler flake8
  autocmd filetype python setlocal foldmethod=indent
  autocmd filetype python setlocal omnifunc=python3complete#Complete
  autocmd filetype python nnoremap <silent> <leader>v :call ActivateVenv()<cr>
augroup END

" Mappings
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-n> :cnext<cr>
noremap <C-p> :cprevious<cr>

nnoremap <f2> :set invpaste<cr>
nnoremap <f3> :nohlsearch<cr>
nnoremap <f5> :setlocal spell!<cr>

" move to next row
nnoremap j gj
nnoremap k gk

nnoremap <leader>a :cclose<cr>
nnoremap <leader>b :buffer<space>
nnoremap <leader>f :find<space>
nnoremap <leader>g :silent grep!<space>

nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>ev :e  $MYVIMRC<cr>
nnoremap <silent> <leader>rv :so $MYVIMRC<cr>
nnoremap <silent> <leader><space> za

nnoremap gV `[v`]
nnoremap <leader>s :mksession

" Functions
function! GenerateCtags()
  let l:output = system('ctags')
  if empty(l:output)
    echo 'ctags done'
  else
    echo l:output
  endif
endfunction

function! UnsetNumbersAndColorColumn()
  setlocal nonumber
  setlocal norelativenumber
  setlocal colorcolumn=
endfunction

function! ActivateVenv()
  let l:venv = input('Virtualenv: ')
  let l:dir = $HOME . '/.virtualenvs'
  if !empty($WORKON_HOME)
    let l:dir = $WORKON_HOME
  endif
  let $PATH .= ':' . l:dir . '/' . l:venv . '/bin'
  echo ' done'
endfunction
