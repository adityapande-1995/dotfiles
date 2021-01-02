" *******************  Plugins and their settings  **********
if has('nvim')
	call plug#begin()
	Plug 'preservim/NERDTree'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'rking/ag.vim'
	Plug 'crusoexia/vim-monokai'
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
	Plug 'mhinz/vim-startify'
	Plug 'preservim/tagbar'
	call plug#end()

	let g:airline_theme = 'luna'
	let g:airline_powerline_fonts = 1
	let g:airline#extensions#tabline#enabled = 1

	colorscheme monokai
endif

" ******** General settings, without any plugins *****
set t_Co=256
syntax on
set termguicolors
set number
set mouse=a
set ignorecase

" Fuzzy search without fzf
set path+=**
set wildmenu

" Use system clipboard
set clipboard=unnamedplus
" Cursor change in insert mode
:autocmd InsertEnter,InsertLeave * set cul!
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

" Code fold
filetype plugin on
filetype indent on
set foldmethod=syntax
set foldlevel=99

" Remap esc
imap jj <Esc>
