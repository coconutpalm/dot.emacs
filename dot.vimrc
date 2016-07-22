
" TODO
" - Move Unite settings into separate file
" - Move vim sexp into separate file
        " move the meta fix to modifiers.vim and source it here
        " Set modifiers and load modifier mapping functions
        " source ~/.vim/local/modifiers.vim
" - Add unite_source_menu_menus to a separate file
"
" USEFULL
" -  use vim -V9myVimLog for debugging
"


" Install neobundle if not there, test this vs the fetch
" on a new machine
" if !filereadable(expand('~/.vim/bundle/'))
"   echo "Installing NeoBundle..."
"   echo ""
"   silent !mkdir -p ~/.vim/bundle
"   silent !git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim/
" endif


"Neobundle: Skip initialization for vim-tiny or vim-small.
 if !1 | finish | endif
" Required:
 if has('vim_starting')
   set nocompatible               " Be iMproved
   set runtimepath+=~/.vim/bundle/neobundle.vim/
 endif

 call neobundle#begin(expand('~/.vim/bundle/'))

 " Let NeoBundle manage NeoBundle (Required)
 NeoBundleFetch 'Shougo/neobundle.vim'

"***************************************************
"Stop message 'Press enter or command to continue'
 set shortmess=a
"***************************************************
" BUNDLES

NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

NeoBundle 'Shougo/Unite.vim'

NeoBundle 'Shougo/echodoc.vim'
" Echodoc stuff from here:
" https://github.com/Shougo/neocomplete.vim/issues/287
" if has('lua')
"  NeoBundle 'Shougo/neocomplete.vim'
"  let g:neocomplete#enable_at_startup = 1
"  " Echo doc completion
"  let g:echodoc_enable_at_startup = 1
"  set completeopt=menu,longest,menuone
"  set cmdheight=2
"
"endif

" if !exists('g:neocomplete#sources')
"   let g:neocomplete#sources = {}
"
"   let g:neocomplete#sources.text = ['look']
"
" " <CR>: close popup and save indent.
"   inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
"
"   " <TAB>: completion.
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"
"   function! s:my_cr_function()
"     return neocomplete#smart_close_popup() . "\<CR>"
"     " For no inserting <CR> key.
"     return pumvisible() ? neocomplete#close_popup() : "\<CR>"
"   endfunction
" else
"   let g:neocomplcache_enable_at_startup = 1
" endif

" NeoBundle 'fatih/vim-go'


"************************************"
" Clojure Setup"


if has("gui_macvim")
  :set macmeta
endif

" THE META FIX FROM:
" this answer: https://github.com/guns/vim-sexp/issues/18
" Full file: https://github.com/guns/haus/blob/master/etc/vim/local/modifiers.vim

" Timeout quickly on key codes to differentiate from normal <Esc>
" set ttimeout ttimeoutlen=0

" Special named keys that cause problems when used literally
" let namedkeys = { ' ': 'Space', '\': 'Bslash', '|': 'Bar', '<': 'lt' }

" Map Alt + ASCII printable chars
" for n in range(0x20, 0x7e)
"     let char = nr2char(n)
"     let key  = char
"
"     if has_key(namedkeys, char)
"         let char = namedkeys[char]
"         let key  = '<' . char . '>'
"     endif
"
"     " Escaped Meta (i.e. not 8-bit mode)
"     "  * Esc-[ is the CSI prefix (Control Sequence Introducer)
"     "  * Esc-O is the SS3 prefix (Single Shift Select of G3 Character Set)
"     if char !=# '[' && char !=# 'O'
"         try
"             execute 'set <M-' . char . ">=\<Esc>" . key
"         catch
"             "echo string([char, key])
"         endtry
"     endif
" endfor

" unlet namedkeys n key char
" End Meta Fix

" Distraction free editing
NeoBundle 'junegunn/goyo.vim'
NeoBundle 'amix/vim-zenroom2'


NeoBundle 'guns/vim-clojure-static'

NeoBundle 'guns/vim-sexp'
let g:sexp_insert_after_wrap = 1

"NeoBundle 'tpope/vim-fireplace'
NeoBundle 'luochen1990/rainbow'
let g:rainbow_active = 1


"****************************************************** Send commands to terminal

" NeoBundle 'jpalardy/vim-slime' "Emacs like send to terminal, useful for pry from vim
" let g:slime_target = "tmux"
" let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
" let g:slime_paste_file = "$HOME/.slime_paste"
" let g:slime_dont_ask_default = 1

"****************************************************** Fancy Selection
NeoBundle 'terryma/vim-expand-region'
"****************************************************** Navigation

NeoBundle 'easymotion/vim-easymotion'
" Figure out the mapping here"
" map <Leader>f <Plug>(easymotion-prefix)

NeoBundle 'terryma/vim-multiple-cursors'

" Highlight but don't jump
nnoremap * *``
nnoremap # #``

" Works exactly like you want, except that it adds a jump in the jump list. To prevent that you need:
" nnoremap * :keepjumps normal *``<cr>

"****************************************************** Snippets
"
"Snippet binding.pry
iabbrev bpry  require 'pry'; binding.pry;


"******************************************************* Notes taking
"
" 80 Dashes
iabbrev ===  <Esc>70i-<Esc>

"Underline line above
iabbrev --- <Esc>kyyp<c-v>$jr-A

" Underline the current line with dashes in normal mode
"nnoremap <F5> yyp<c-v>$r-


"****************************************************** Commenting

NeoBundle 'tpope/vim-commentary'
nmap <Leader>c <Plug>CommentaryLine
xmap <Leader>c <Plug>Commentary

augroup plugin_commentary
    au!
    au FileType python setlocal commentstring=#%s
    au FileType htmldjango setlocal commentstring={#\ %s\ #}
    au FileType puppet setlocal commentstring=#\ %s
    au FileType xquery setlocal commentstring=(:\ %s\ :)
augroup END


"****************************************************** Custom Functions
"TODO: Add reference to source
function! Preserve(command)
  " preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " do the business:
  execute a:command
  " clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction "}}}

function! FixWhitespace() "{{{
  call Preserve("%s/\\s\\+$//e")
endfunction

"****************************************************** File navigation

NeoBundle 'scrooloose/nerdtree'

"****************************************************** Table like formatting

NeoBundle 'godlygeek/tabular'

"****************************************************** Save with control s + Fix any white space
:nmap <silent> <c-s> :w<CR> :call FixWhitespace()<CR>
:imap <silent> <c-s> <Esc>:w<CR>a :call FixWhitespace()<CR>
:imap <silent> <c-s> <Esc><c-s> :call FixWhitespace()<CR>
:vmap <silent> <c-s> <Esc>:w<CR>a :call FixWhitespace()<CR>
:vmap <silent> <c-s> <Esc><c-s> :call FixWhitespace()<CR>

"***************************************************** Copy/Paste System
"Copy with Ctrl+C
vmap <C-c> y:call system("pbcopy", getreg("\""))<CR>
" vmap <C-c> :w ! xsel -ib<CR>


"****************************************************** Auto paste mode toggle

if &term =~ "xterm.*"
    let &t_ti = &t_ti . "\e[?2004h"
    let &t_te = "\e[?2004l" . &t_te
    function XTermPasteBegin(ret)
        set pastetoggle=<Esc>[201~
        set paste
        return a:ret
    endfunction
    map <expr> <Esc>[200~ XTermPasteBegin("i")
    imap <expr> <Esc>[200~ XTermPasteBegin("")
    cmap <Esc>[200~ <nop>
    cmap <Esc>[201~ <nop>
endif


" Plugins Section
"**************************************"
NeoBundle 'chriskempson/vim-tomorrow-theme'
NeoBundle 'ciaranm/inkpot'

NeoBundle 'bling/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'rakr/vim-two-firewatch'
" NeoBundle 'osyo-manga/unite-airline_themes', {'autoload':{'unite_sources':'airline_themes'}}

NeoBundle 'ervandew/supertab'
NeoBundle 'rking/ag.vim'

"****************************************************** Notes

NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-notes'

"****************************************************** Gutter

NeoBundle 'airblade/vim-gitgutter'

NeoBundle 'kshenoy/vim-signature'

"****************************************************** SQL INDENT

NeoBundle 'vim-scripts/SQLUtilities'
" NeoBundle 'vim-scripts/dbext.vim'
" NeoBundle 'vim-scripts/Align'

NeoBundle 'vim-scripts/dbext.vim'
NeoBundle 'vim-scripts/Align'
NeoBundle 'vim-scripts/SQLUtilities'
autocmd FileType sql vmap <silent>sf <Plug>SQLUFormatter<CR>

"****************************************************** Zooms a window

NeoBundleLazy 'vim-scripts/zoomwintab.vim', {'autoload' :
      \{'commands' : 'ZoomWinTabToggle'}}
:nmap <C-w>o :ZoomWinTabToggle<CR>

"*************************************************** Indent Guides

NeoBundle 'Yggdroot/indentLine'

let g:indentLine_char = '┆'

" You can also use one of ¦, ┆, ︙ or │
" Vim
let g:indentLine_color_term = 239
"GVim
let g:indentLine_color_gui = '#A4E57E'
" none X terminal
let g:indentLine_color_tty_light = 7 " (default: 4)
let g:indentLine_color_dark = 1 " (default: 2)

"*************************************************** NeoBundle end and Check
call neobundle#end()

"check for new bundles
 NeoBundleCheck

 filetype plugin indent on

"*************************************************** Inplace Scrolling

"Inplace Scrolling
noremap <C-J> 1j1<C-e>
noremap <C-k> 1k1<C-y>


"*************************************************** Reveal in nerd tree
nmap ,n :NERDTreeFind<CR>
nmap ,m :NERDTreeToggle<CR>

"*************************************************** LEADER

" let mapleader = ";"
let mapleader = "\<space>"

"****************************************************** "Current" word in quote
nnoremap <Leader>" ciw"<C-r>""<Esc>
vnoremap <Leader>" c"<C-r>""<Esc>

"****************************************************** Save as root

cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

"*************************************************** MAPPINGS

"Smash escape
inoremap jk <esc>
inoremap kj <esc>

"Left right end of line movement
nnoremap H ^
vnoremap H ^
nnoremap L $
vnoremap L $

" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

set softtabstop=2 expandtab
syntax on

" Multiple repeat paste
xnoremap p pgvy

"reselect last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

"make Y consistent with C and D. See :help Y.
nnoremap Y y$

" Reverted back tabing
:imap <Tab> <C-t>
:imap <S-Tab> <C-d>

" remap arrow keys
nnoremap <up> :bprev<CR>
nnoremap <down> :bnext<CR>
nnoremap <right> :tabnext<CR>
nnoremap <left> :tabprev<CR>

" change cursor position in insert mode
inoremap <C-h> <left>
inoremap <C-l> <right>

"****************************************************** Toggle highlighted search matches
nnoremap <BS> :set hlsearch! hlsearch?<cr>

"*************************************************** Windows

set splitbelow
set splitright

"*************************************************** Color
set t_Co=256
set background=light
" color Tomorrow-Night-Eighties
color inkpot

" Set gutter color to black
highlight SignColumn ctermbg=black    " terminal Vim
highlight SignColumn guibg=black      " gVim/MacVim


"*************************************************** Airline
"
" Plugin 'tpope/vim-fugitive'
" Plugin 'airblade/vim-gitgutter'
" Plugin 'mhinz/vim-signify'

set background=light
colo two-firewatch

let g:airline_theme='twofirewatch'
set laststatus=2
set ttimeoutlen=50

let g:airline#extensions#hunks#enabled=0
let g:airline#extensions#branch#enabled=1

" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif
" let g:airline_symbols.space = "\ua0"

"*************************************************** Cursor shape
" Maybe Try this plugin: jszakmeister/vim-togglecursor"
" tmux will only forward escape sequences to the terminal if surrounded by a DCS sequence
" http://sourceforge.net/mailarchive/forum.php?thread_name=AANLkTinkbdoZ8eNR1X2UobLTeww1jFrvfJxTMfKSq-L%2B%40mail.gmail.com&forum_name=tmux-users

if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

"***************************************************** Speed up vim

set ttyfast
set lazyredraw

"*************************************************** Search
:set ignorecase

"quick buffer open
nnoremap gb :ls<cr>:e #

"****************************************************** Backspace on Os X

set backspace=indent,eol,start                      "allow backspacing everything in insert mode

"*************************************************** STUFF

"set guifont=Inconsolata:h18

"set term=linux

"*************************************************** Mouse
set ttymouse=xterm2
:set mouse=a

"*************************************************** UNITE
"Needed for unite preview window
set previewheight=25

let g:unite_data_directory='~/.vim/.cache/unite'
let g:unite_source_find_max_candidates=5000

function! s:unite_settings()
  let b:SuperTabDisabled=1
  " imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  " imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  imap <silent><buffer><expr> <C-x> unite#do_action('split')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  imap <silent><buffer><expr> <C-t> unite#do_action('tabopen')
  nmap <buffer> Q <plug>(unite_exit)
  nmap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <esc> <plug>(unite_exit)
endfunction

autocmd FileType unite call s:unite_settings()

let g:unite_split_rule = "botright"
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable=1

nnoremap <silent> <leader>l :Unite -start-insert -buffer-name=files -winheight=10 file_rec/async<cr>

" nnoremap <space>s :Unite -quick-match buffer<cr>
nnoremap <space>s :Unite buffer<cr>
" nnoremap <leader>z :Unite  -buffer-name=files   -start-insert buffer file_rec/async:!<cr>
nnoremap <silent><leader>/ :<C-u>Unite -no-quit -keep-focus grep<CR>
set previewheight=25
nnoremap <leader>f :<C-u>Unite -start-insert -auto-preview -winheight=30 -no-split file_rec/async:!<CR>
nmap <buffer> <C-i> <Plug>(unite_toggle_auto_preview)
let g:unite_prompt='» '


if executable('ag')
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts =
      \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
      \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
    let g:unite_source_grep_recursive_opt = ''
    " let g:unite_source_grep_search_word_highlight = 1
    let g:unite_split_rule = "botright"
elseif executable('ack')
    let g:unite_source_grep_command='ack'
    let g:unite_source_grep_default_opts='--no-group --no-color'
    let g:unite_source_grep_recursive_opt=''
    let g:unite_source_grep_search_word_highlight = 1
    let g:unite_split_rule = "botright"
endif
""

"************************************************ UNITE MENUS

" source ~/dotify/files/vim/unite_menus.vim

"************************************************ Ruby specific
" function! <SID>RubySettings()
"     let g:rubycomplete_include_object = 1
"     let g:rubycomplete_buffer_loading = 1
"     let g:rubycomplete_classes_in_global = 1
"     let g:rubycomplete_include_objectspace = 1

"     set expandtab
"     set tabstop=2 shiftwidth=2 softtabstop=2
"     set autoindent
" endfunction
" autocmd FileType ruby call <SID>RubySettings()

"************************************************ Ruby specific
" Distraction Free
nnoremap <Leader>d :Goyo<CR>
let g:goyo_width=120

"************************************************ Undo

function! EnsureExists(path) "{{{
  if !isdirectory(expand(a:path))
    call mkdir(expand(a:path))
  endif
endfunction "}}}
" vim file/folder management {{
" persistent undo
if exists('+undofile')
  set undofile
  set undodir=~/.vim/.cache/undo

  " backups
  " set backup
  " set backupdir=~/.vim/.cache/backup

  " " swap files
  " set directory=~/.vim/.cache/swap
  " set noswapfile

  " call EnsureExists('~/.vim/.cache')
  "call EnsureExists(&undodir)
  " call EnsureExists(&backupdir)
  " call EnsureExists(&directory)

  "Possible improvements if file does not exist
  "http://stackoverflow.com/questions/5700389/using-vims-persistent-undo

endif "}}

"************************************************ Format XML

au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

