" *******************  Plugins and their settings  **********
if has('nvim')
	call plug#begin()
	Plug 'preservim/NERDTree'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'rking/ag.vim'

	Plug 'crusoexia/vim-monokai'
	Plug 'dracula/vim', { 'as': 'dracula' }

	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
	Plug 'mhinz/vim-startify'
	Plug 'preservim/tagbar'
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	call plug#end()

	let g:airline_theme = 'luna'
	let g:airline_powerline_fonts = 1
	let g:airline#extensions#tabline#enabled = 1

	colorscheme dracula

	" coc vim config from its github page"
	source ~/coc-vim-config

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

" Cycle buffers
set wildchar=<Tab> wildmenu wildmode=full
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>
" move among buffers with CTRL
map <C-J> :bnext<CR>
map <C-K> :bprev<CR>


" Autoclose brackets
inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

