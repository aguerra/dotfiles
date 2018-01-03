" vimrc

" Auto install plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd vimenter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Plugin list
Plug 'ctrlpvim/ctrlp.vim', {'tag': '1.80'}
Plug 'fatih/vim-go', {'tag': 'v1.16'}
Plug 'honza/vim-snippets'
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
  set cursorline
endif

set background=dark
set guifont=Monospace\ Regular\ 13
set guioptions-=T  " disable the tool bar
set laststatus=2   " always show the status line
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
set pastetoggle=<f2>
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
map <c-h> <c-w>h<c-w>_
map <c-j> <c-w>j<c-w>_
map <c-k> <c-w>k<c-w>_
map <c-l> <c-w>l<c-w>_

nnoremap <c-m> :cprevious<cr>
nnoremap <c-n> :cnext<cr>

nnoremap <f3> :set invhlsearch<cr>
nnoremap <f5> :setlocal spell!<cr>

nnoremap <leader><space> za
nnoremap <leader>a :Ack!<space>
nnoremap <leader>c :cclose<cr>
nnoremap <leader>e :e $MYVIMRC<cr>
nnoremap <silent> <leader>gc :Gcommit<cr>
nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>ge :Gedit<cr>
nnoremap <silent> <leader>gl :Glog<cr>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gw :Gwrite<cr>
nnoremap <leader>m :make!<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>s :source $MYVIMRC<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>z :wq<cr>

nnoremap j gj
nnoremap k gk

" Functions
function! ActivateVenv()
  let l:venv = input('Virtualenv: ')
  let l:dir = $HOME . '/.virtualenvs/' . l:venv
endfunction
