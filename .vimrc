call plug#begin()
Plug 'preservim/NERDTree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rking/ag.vim'
Plug 'crusoexia/vim-monokai'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
call plug#end()

let g:airline_theme = 'luna'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
set t_Co=256
syntax on
colorscheme monokai
set termguicolors
set number
set clipboard=unnamedplus
