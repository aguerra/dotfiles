" vimrc

" Install plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd vimenter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Plugin list
Plug 'fatih/vim-go', {'tag': 'v1.17'}
Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', {'tag': '0.17.4'}
Plug 'junegunn/fzf.vim'
Plug 'python-mode/python-mode', {'branch': 'develop'}
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-dispatch', {'tag': 'v1.5'}
Plug 'tpope/vim-fugitive', {'tag': 'v2.3'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Look and feel
colorscheme desert

set background=dark
set cursorline
set laststatus=2
set number
set relativenumber

highlight colorcolumn ctermbg=233

" General options
let mapleader = ','
let &makeprg = 'make -j' . systemlist('getconf _NPROCESSORS_ONLN')[0]

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
set pastetoggle=<f2>
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
let g:go_fmt_command = 'goimports'
let g:go_get_update = 0
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_arguments = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1
let g:pymode_python = 'python3'

" Autocmds
augroup general
  autocmd!
  autocmd completedone * pclose
  autocmd filetype help,qf setlocal nonumber | setlocal norelativenumber
    \ | setlocal colorcolumn=
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
  autocmd filetype python setlocal foldlevelstart=10
augroup end

" Mappings
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <f3> :set invhlsearch<cr>
nnoremap <f4> :call ToggleNumbers()<cr>
nnoremap <f5> :setlocal spell!<cr>
nnoremap <leader>a :Ag<space>
nnoremap <leader>c :cclose<cr>
nnoremap <leader>e :edit $MYVIMRC<cr>
nnoremap <leader>f :Files<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gd :Gvdiff<cr>
nnoremap <leader>gs :Gstatus<cr

nnoremap <leader>h :Snippets<cr>
nnoremap <leader>r :Tags<cr>
nnoremap <leader>m :make!<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>s :source $MYVIMRC<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>z :wq<cr>

nnoremap ; :Buffers<cr>
nnoremap j gj
nnoremap k gk

nmap <M-k> :Ack! "\b<cword>\b" <CR>

" Functions
function! ToggleNumbers()
  setlocal number!
  setlocal relativenumber!
endfunction
