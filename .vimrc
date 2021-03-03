" 
"  ______________________
" / ░█░█░▀█▀░█▄█░█▀▄░█▀▀ \
" | ░▀▄▀░░█░░█░█░█▀▄░█░░ |
" \ ░░▀░░▀▀▀░▀░▀░▀░▀░▀▀▀ /
"  ----------------------
"         \   ^__^
"          \  (oo)\_______
"             (__)\       )\/\
"                 ||----w |
"                 ||     ||
" 

" *** TODO ***
" vim-which-key , leader key mapping

" *******************  Plugins and their settings  **********
if has('nvim')
	call plug#begin()
	Plug 'preservim/NERDTree'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'rking/ag.vim'

	Plug 'crusoexia/vim-monokai'
	Plug 'dracula/vim', { 'as': 'dracula' }

	""Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	""Plug 'junegunn/fzf.vim'
	Plug 'mhinz/vim-startify'
	Plug 'preservim/tagbar'
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'ryanoasis/vim-devicons'
	Plug 'kien/ctrlp.vim'
	" Dealing with tables
	"Plug 'godlygeek/tabular'
	Plug 'junegunn/vim-easy-align'
	Plug 'dhruvasagar/vim-table-mode'
	Plug 'vim-scripts/sherlock.vim'
	Plug 'tpope/vim-fugitive'
	Plug 'chrisbra/csv.vim'
	call plug#end()

	let g:airline_theme = 'luna'
	let g:airline_powerline_fonts = 1
	let g:airline#extensions#branch#enabled=1
	let g:airline#extensions#tabline#enabled = 1

	colorscheme dracula

	" coc vim config from its github page"
	source ~/coc-vim-config

	" Key bindings for plugins
	nmap <F6> :TableModeToggle<CR>
	nmap <F7> :TagbarToggle<CR>
	nmap <F8> :NERDTreeToggle<CR>
	
endif

" ******** General settings, without any plugins *****
set showcmd
set t_Co=256
syntax on
set termguicolors
set number
set mouse=a
set ignorecase
set splitbelow splitright

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
autocmd BufNewFile,BufRead *.py   set foldmethod=indent
set foldlevel=99

" Remap esc
imap jj <Esc>

" Spellcheck
:map <F6> :setlocal spell! spelllang=en_us<CR>

" Use Ctrl s to save
:nmap <c-s> :w<CR>
:imap <c-s> <Esc>:w<CR>a

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

" Bulk comment functions -- custom
function! Comment()
  let ext = tolower(expand('%:e'))
  if ext == 'php' || ext == 'rb' || ext == 'sh' || ext == 'py'
    :norm I# 
  elseif ext == 'js' || ext == 'c' || ext == 'cpp' || ext == 'h' || ext == 'hpp'
    :norm I// 
  endif
endfunction

function! Uncomment()
  let ext = tolower(expand('%:e'))
  if ext == 'php' || ext == 'rb' || ext == 'sh' || ext == 'py'
    :norm ^xx
  elseif ext == 'js'|| ext == 'c' || ext == 'cpp' || ext == 'h' || ext == 'hpp'
    :norm ^xxx
  endif
endfunction

" Comment uncomment triggers
vnoremap <C-a> :call Comment()<CR>
vnoremap <C-b> :call Uncomment()<CR>


