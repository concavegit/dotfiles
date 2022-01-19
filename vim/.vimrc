" Plugin-free settings
let mapleader=' '
set acd
set ic scs
set rnu nu
set spell
set ts=4 shiftwidth=4 et
set shm+=I

call plug#begin()

" Sane defaults
Plug 'easymotion/vim-easymotion'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" IDE
Plug 'alvan/vim-closetag'
Plug 'dense-analysis/ale'
Plug 'honza/vim-snippets'
Plug 'jmcantrell/vim-virtualenv'
Plug 'mattn/emmet-vim'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/vim-lsp'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-projectionist'

" LSP dependencies
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/asyncomplete.vim'

" Theme
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Navigation
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'moll/vim-bbye'
Plug 'aymericbeaumet/vim-symlink'

" graphql
Plug 'jparise/vim-graphql'
call plug#end()

" Theme
colo base16-tomorrow-night
let g:airline_theme = 'tomorrow'
let g:airline#extensions#tabline#enabled = 1

" LSP
let g:lsp_diagnostics_enabled = 0
let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_textprop_enabled = 1
let g:lsp_highlight_references_enabled = 1
let g:lsp_semantic_enabled = 1

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> <f2> <plug>(lsp-rename)
endfunction

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"

" VirtualEnv
let g:virtualenv_auto_activate = 1

" Vimspector
let g:vimspector_enable_mappings = 'HUMAN'

set cot+=menuone

" CloseTags
let g:closetag_filenames='*.tsx'
let g:closetag_regions =  {
\ 'typescript.tsx': 'jsxRegion,tsxRegion',
\ 'javascript.jsx': 'jsxRegion',
\ }

" FZF
command! -bang -nargs=* PRg
  \ call fzf#vim#grep('rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, {'dir': system('git -C '.expand('%:p:h').' rev-parse --show-toplevel 2> /dev/null')[:-2]}, <bang>0)

" Keybindings
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>ff <plug>(fzf-compete-file)
nnoremap <leader>fh :History<cr>
nnoremap <leader>ps :PRg<cr>
nnoremap <leader>pf :GFiles<cr>
nnoremap <leader>pg :Gstatus<cr>
nnoremap <leader>cv :VirtualEnvActivate 

map gD :bd<cr>

let g:projectionist_heuristics = json_decode(join(readfile(expand('~/dotfiles/nostow/projections.json'))))
