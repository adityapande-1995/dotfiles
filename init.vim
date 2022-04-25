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
"
scriptencoding utf-8
set encoding=utf-8

:set number
:set autoindent
:set tabstop=4
:set shiftwidth=4
:set smarttab
:set softtabstop=4
:set mouse=a
:set clipboard+=unnamedplus

call plug#begin()

Plug 'http://github.com/tpope/vim-surround' " Surrounding ysw)
Plug 'https://github.com/preservim/nerdtree' " NerdTree
Plug 'https://github.com/tpope/vim-commentary' " For Commenting gcc & gc
Plug 'https://github.com/vim-airline/vim-airline' " Status bar
Plug 'https://github.com/rafi/awesome-vim-colorschemes' " Retro Scheme
Plug 'https://github.com/neoclide/coc.nvim'  " Auto Completion
Plug 'https://github.com/ryanoasis/vim-devicons' " Developer Icons
Plug 'https://github.com/tc50cal/vim-terminal' " Vim Terminal
Plug 'https://github.com/preservim/tagbar' " Tagbar for code navigation
Plug 'https://github.com/terryma/vim-multiple-cursors' " CTRL + N for multiple cursors

Plug 'https://github.com/tpope/vim-sleuth' " Automatically figures out whether to use tabs or spaces
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'glepnir/dashboard-nvim'

Plug 'kyazdani42/nvim-web-devicons' " Recommended (for coloured icons)
Plug 'akinsho/bufferline.nvim', { 'tag': '*' }

call plug#end()

" Run :PlugInstall to install these plugins

" Shortcuts
" ---------
let g:mapleader="\<Space>"

nnoremap <F7> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>

nnoremap <C-l> :call CocActionAsync('jumpDefinition')<CR>

" Ctrl + s to save file
:nmap <c-s> :w<CR>
:imap <c-s> <Esc>:w<CR>a

nmap <Leader>ss :<C-u>SessionSave<CR>
nmap <Leader>sl :<C-u>SessionLoad<CR>
nnoremap <silent> <Leader>fh :DashboardFindHistory<CR>
nnoremap <silent> <Leader>ff :DashboardFindFile<CR>
nnoremap <silent> <Leader>tc :DashboardChangeColorscheme<CR>
nnoremap <silent> <Leader>fa :DashboardFindWord<CR>
nnoremap <silent> <Leader>fb :DashboardJumpMark<CR>
nnoremap <silent> <Leader>cn :DashboardNewFile<CR>

" Open the vimrc file using SPACE: f e d
let g:vimrc_path = expand('<sfile>')
:nnoremap <Leader>fed :exec "e ".g:vimrc_path<cr>

" Settings
" --------
":set completeopt-=preview " For No Previews

:colorscheme jellybeans

let g:NERDTreeDirArrowExpandable="+"
let g:NERDTreeDirArrowCollapsible="~"

" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

let g:dashboard_default_executive ='fzf'
let g:dashboard_custom_shortcut={
\ 'last_session'       : 'SPC s l',
\ 'find_history'       : 'SPC f h',
\ 'find_file'          : 'SPC f f',
\ 'new_file'           : 'SPC c n',
\ 'change_colorscheme' : 'SPC t c',
\ 'find_word'          : 'SPC f a',
\ 'book_marks'         : 'SPC f b',
\ 'Open init.vim'      : 'SPC f e d',
\ }


" Display tabs and spaces
set list
set listchars=tab:▸·,trail:~,extends:>,precedes:<,space:-


" NOTES
" -----
" :map-- to show all mappings
"
"  ** commentary.vim
"  Command: gc, gcc

" ** Coc
" pip3 install jedi
" Run yarn install and yarn build inside plugged/coc.nvim
" :CocInstall coc-python
" :CocInstall coc-clangd
" :CocCommand clangd.install