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
Plug 'easymotion/vim-easymotion', {'tag': 'v3.0.1'}
Plug 'fatih/vim-go', {'tag': 'v1.13'}
Plug 'godlygeek/tabular', {'tag': '1.0.0'}
Plug 'jiangmiao/auto-pairs', {'tag': 'v1.3.1'}
Plug 'majutsushi/tagbar', {'tag': 'v2.7'}
Plug 'mbbill/undotree', {'tag': 'rel_5.0'}
Plug 'mhinz/vim-signify', {'tag': 'v1.9'}
Plug 'mileszs/ack.vim', {'tag': '1.0.9'}
Plug 'morhetz/gruvbox'
Plug 'python-mode/python-mode', {'tag': '0.9.2'}
Plug 'reedes/vim-litecorrect', {'tag': 'v1.1'}
Plug 'rhysd/conflict-marker.vim'
Plug 'scrooloose/nerdtree', {'tag': '5.0.0'}
Plug 'Shougo/neocomplete.vim', {'tag': 'ver.2.1'}
Plug 'Shougo/neosnippet', {'tag': 'ver.4.2'}
Plug 'Shougo/neosnippet-snippets'
Plug 'terryma/vim-multiple-cursors', {'tag': 'v2.2'}
Plug 'tpope/vim-commentary', {'tag': 'v1.3'}
Plug 'tpope/vim-fugitive', {'tag': 'v2.2'}
Plug 'tpope/vim-repeat', {'tag': 'v1.1'}
Plug 'tpope/vim-surround', {'tag': 'v2.1'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/a.vim', {'tag': '2.18'}
Plug 'vim-scripts/python.vim', {'tag': '1.13'}
Plug 'vim-scripts/python_match.vim', {'tag': '0.5'}
Plug 'vim-scripts/sessionman.vim', {'tag': '1.07'}
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
set grepprg=ag\ --vimgrep
set hidden                 " hide abandoned buffers
set history=1000
set hlsearch
set ignorecase
set incsearch
set list                   " show tabs and end of lines
set listchars=tab:-\       " only show tabs
set path=,,**              " current dir and subdirs
set scrolloff=3            " min lines above and below cursor
set shell=/bin/sh
set shortmess+=filmnrxoOtT " helps to avoid hit-enter prompts
set showcmd                " show partial commands in status line
set showmatch
set smartcase              " case sensitive when uppercase present
set spelllang=en_us,pt_br
set tags=./tags,tags       " dir of the current file and current dir
set textwidth=79
set visualbell
set wildmenu               " cmdline completion enhanced mode

if !isdirectory(&directory)
    call mkdir(&directory, "p")
endif

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

let g:airline#extensions#tabline#enabled = 1
let g:neocomplete#enable_at_startup = 1

" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

autocmd bufnewfile,bufreadpost *.md set filetype=markdown
