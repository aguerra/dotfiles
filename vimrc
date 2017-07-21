" vimrc

" Auto install vim-plug and plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd vimenter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Plugin list
Plug 'ctrlpvim/ctrlp.vim', {'tag': '1.80'}
Plug 'fatih/vim-go', {'tag': 'v1.13'}
Plug 'majutsushi/tagbar', {'tag': 'v2.7'}
Plug 'mileszs/ack.vim', {'tag': '1.0.9'}
Plug 'morhetz/gruvbox'
Plug 'python-mode/python-mode', {'tag': '0.9.2'}
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-fugitive', {'tag': 'v2.2'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'

call plug#end()

" Look and feel
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italicize_comments = 0

if has('gui_running')
  colorscheme gruvbox
endif

set background=dark
set cursorline
set guifont=Ubuntu\ Mono\ 12
set guioptions-=T            " disable the tool bar
set laststatus=2             " always show the status line
set number
set relativenumber

" General options
let mapleader = ','
let &makeprg = 'make -j' . systemlist('getconf _NPROCESSORS_ONLN')[0]

set clipboard=unnamed,unnamedplus " use the system clipboard
set colorcolumn=+1
set cryptmethod=blowfish2
set directory=~/.vim-tmp
set foldlevelstart=10
set foldmethod=syntax
set foldnestmax=10
set hidden                   " hide abandoned buffers
set history=1000
set hlsearch
set ignorecase
set incsearch
set list                     " show tabs and end of lines
set listchars=tab:-\         " only show tabs
set scrolloff=3              " min lines above and below cursor
set shell=/bin/sh
set shortmess+=filmnrxoOtT   " helps to avoid hit-enter prompts
set showcmd                  " show partial commands in status line
set showmatch
set smartcase                " case sensitive when uppercase present
set spelllang=en_us,pt_br
set splitright
set tags=./tags;/,~/.vimtags
set textwidth=79
set visualbell
set wildmenu                 " cmdline completion enhanced mode

silent! call mkdir(&directory, "p")

" Plugins options
let g:ackprg = 'ag --nogroup --nocolor --column --smart-case'
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:go_fmt_command = 'goimports'
let g:go_get_update = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:pymode_lint = 0
let g:pymode_python = 'python3'

" Autocmds
augroup general
  autocmd!
  autocmd bufnewfile,bufreadpost *.md set filetype=markdown
  autocmd completedone * pclose
  autocmd filetype help,qf setlocal nonumber | setlocal norelativenumber
  autocmd guienter * call system(
    \'wmctrl -i -b add,maximized_vert,maximized_horz -r ' . v:windowid
  \)
  autocmd quickfixcmdpost grep,make botright cwindow
augroup end

augroup go
  autocmd!
  autocmd filetype go setlocal colorcolumn= | setlocal textwidth=0
  autocmd filetype go nmap <leader>b <plug>(go-build)
  autocmd filetype go nmap <leader>r <plug>(go-run)
augroup end

augroup python
  autocmd!
  autocmd filetype python setlocal foldmethod=indent
  autocmd filetype python nnoremap <silent> <leader>v :call ActivateVenv()<cr>
augroup end

" Mappings
nnoremap <leader>a :Ack!<space>
nnoremap <leader>c :cclose<cr>

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

nnoremap <leader>b :buffer<space>
nnoremap <leader>f :find<space>

nnoremap <silent> <leader>m  :silent make!<cr>: echo 'make done'<cr>
nnoremap <silent> <leader>t  :call GenerateCtags()<cr>
nnoremap <silent> <leader>q  :q<cr>
nnoremap <silent> <leader>wq :wq<cr>
nnoremap <silent> <leader>ev :e  $MYVIMRC<cr>
nnoremap <silent> <leader>rv :so $MYVIMRC<cr>
nnoremap <silent> <leader><space> za

nnoremap gV `[v`]
nnoremap <leader>s :mksession

nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>gc :Gcommit<cr>

" Functions
function! GenerateCtags()
  let l:output = system('ctags')
  if empty(l:output)
    echo 'ctags done'
  else
    echo l:output
  endif
endfunction

function! ActivateVenv()
  let l:venv = input('Virtualenv: ')
  let l:dir = $HOME . '/.virtualenvs/' . l:venv
  PymodeVirtualenv l:dir
endfunction
