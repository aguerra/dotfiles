" vimrc

" Install plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim', {'tag': '1.80'}
Plug 'fatih/vim-go', {'tag': 'v1.18'}
Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar', {'tag': 'v2.7'}
Plug 'mileszs/ack.vim', {'tag': '1.0.9'}
Plug 'python-mode/python-mode', {'branch': 'develop'}
Plug 'SirVer/ultisnips'
Plug 'tomasr/molokai'
Plug 'tpope/vim-fugitive', {'tag': 'v2.4'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

" Look and feel
if has('gui_running')
  colorscheme molokai
endif

set background=dark
set cursorline
set guicursor+=a:blinkon0
set guifont=Ubuntu\ Mono\ 15
set guioptions-=T
set laststatus=2
set number
set relativenumber

" General options
let mapleader = ','
let &makeprg = 'make -j' . systemlist('getconf _NPROCESSORS_ONLN')[0]

set autoindent
set clipboard^=unnamed,unnamedplus
set colorcolumn=+1
set cryptmethod=blowfish2
set directory=~/.vim-tmp
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set list
set listchars=tab:-\  " quoted space
set pastetoggle=<F2>
set scrolloff=5
set shortmess=aoOtT
set showcmd
set showmatch
set smartcase
set spelllang=en_us,pt_br
set splitright
set tags+=~/.tags
set textwidth=79
set visualbell
set wildmenu

silent! call mkdir(&directory, "p")

" Plugins options
let g:ackprg = 'ag --vimgrep'
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:go_fmt_command = 'goimports'
let g:go_get_update = 0
let g:go_highlight_functions = 1
let g:netrw_liststyle = 3
let g:netrw_winsize = 25
let g:pymode_python = 'python3'

" Autocmds
augroup general
  autocmd!
  autocmd completedone * pclose
  autocmd filetype help,qf setlocal colorcolumn=
augroup end

augroup go
  autocmd!
  autocmd filetype go setlocal colorcolumn= | setlocal textwidth=0
  autocmd filetype go nmap <leader>b <plug>(go-build)
  autocmd filetype go nmap <leader>r <plug>(go-run)
  autocmd filetype go nmap <leader>t <plug>(go-test)
  autocmd filetype go nmap <leader>l <plug>(go-alternate-edit)
  autocmd filetype go nmap <leader>d <plug>(go-def)
augroup end

augroup python
  autocmd!
  autocmd filetype python setlocal foldlevelstart=10
augroup end

" Mappings
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <c-m> :cprevious<cr>
nnoremap <c-n> :cnext<cr>
nnoremap <f3> :set invhlsearch<cr>
nnoremap <f4> :call ToggleNumbers()<cr>
nnoremap <f5> :setlocal spell!<cr>
nnoremap j gj
nnoremap k gk
nnoremap <leader>a :Ack!<space>
nnoremap <leader>c :cclose<cr>
nnoremap <leader>e :edit $MYVIMRC<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gd :Gvdiff<cr>
nnoremap <leader>gs :Gstatus<cr
nnoremap <leader>h :help<space>
nnoremap <leader>k :Ack! -w "<cword>"<cr>
nnoremap <leader>m :make<space>
nnoremap <leader>q :q<cr>
nnoremap <leader>s :source $MYVIMRC<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>z :wq<cr>

" Functions
function! ToggleNumbers()
  setlocal number!
  setlocal relativenumber!
endfunction
