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
:set expandtab
set cursorline
highlight clear CursorLine

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
Plug 'tpope/vim-fugitive'

Plug 'kyazdani42/nvim-web-devicons' " Recommended (for coloured icons)
Plug 'akinsho/bufferline.nvim', { 'tag': '*' }

Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'folke/which-key.nvim'
Plug 'vimwiki/vimwiki'
Plug 'dominikduda/vim_current_word'

call plug#end()

" Run :PlugInstall to install these plugins

" Shortcuts
" ---------
let g:mapleader="\<Space>"


nnoremap <C-l> :call CocActionAsync('jumpDefinition')<CR>

" Ctrl + s to save file
:nmap <c-s> :w<CR>
:imap <c-s> <Esc>:w<CR>a

nmap <Leader>ss :<C-u>SessionSave<CR>
nmap <Leader>sl :<C-u>SessionLoad<CR>
nnoremap <silent> <Leader>fh :DashboardFindHistory<CR>
nnoremap <silent> <Leader>ff :DashboardFindFile<CR>
nnoremap <silent> <Leader>tc :DashboardChangeColorscheme<CR>
nnoremap <silent> <Leader>fw :DashboardFindWord<CR>
nnoremap <silent> <Leader>fb :DashboardJumpMark<CR>
nnoremap <silent> <Leader>nf :DashboardNewFile<CR>
nnoremap <silent> <Leader>fd :Dashboard<CR>

" Open the vimrc file using SPACE: f e d
let g:vimrc_path = expand('<sfile>')
:nnoremap <Leader>fc :exec "e ".g:vimrc_path<cr>
:nnoremap <silent> <Leader>fcr :source $MYVIMRC<cr>

nnoremap <silent> <Leader>tn :NERDTreeToggle<CR>
nnoremap <silent> <Leader>tt :TagbarToggle<CR>

" fzf power 
nnoremap <silent> <Leader>fzb :BLines<CR>
nnoremap <silent> <Leader>fzl :Lines<CR>

" Leader mappings
:nnoremap <Leader><Leader> :Maps<cr>

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
\ 'new_file'           : 'SPC n f',
\ 'change_colorscheme' : 'SPC t c',
\ 'find_word'          : 'SPC f w',
\ 'book_marks'         : 'SPC f b',
\ 'Open vim config'    : 'SPC f c',
\ 'Ranger'             : 'SPC f',
\ }


" Display tabs and spaces
set list
set listchars=tab:▸·,trail:~,extends:>,precedes:<,space:-

" In your init.lua or init.vim
set termguicolors
lua << EOF
require("bufferline").setup{}
EOF

" These commands will navigate through buffers in order regardless of which mode you are using
" e.g. if you change the order of buffers :bnext and :bprevious will not respect the custom ordering
nnoremap <silent>[b :BufferLineCycleNext<CR>
nnoremap <silent>b] :BufferLineCyclePrev<CR>

" These commands will move the current buffer backwards or forwards in the bufferline
nnoremap <silent><mymap> :BufferLineMoveNext<CR>
nnoremap <silent><mymap> :BufferLineMovePrev<CR>

" These commands will sort buffers by directory, language, or a custom criteria
nnoremap <silent>be :BufferLineSortByExtension<CR>
nnoremap <silent>bd :BufferLineSortByDirectory<CR>
nnoremap <silent><mymap> :lua require'bufferline'.sort_buffers_by(function (buf_a, buf_b) return buf_a.id < buf_b.id end)<CR>


lua << EOF
  require("which-key").setup {
  }
EOF

" ==============================================================================
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

" Folding
" :setlocal foldmethod=syntax
