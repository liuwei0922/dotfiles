"-----------------------------基本配置-----------------------------------------
"------------------------------------------------------------------------------
"显示行号
set number
"显示相对行号
set relativenumber
"语法高亮
syntax on
syntax enable 
"侦测文件类型
filetype on
"开启文件类型检查，载入相应类型的缩进规则
filetype indent on
"针对不同文件开启不同的插件
filetype plugin on
"启用自动补全
filetype plugin indent on 
"显示模式，命令／插入
set showmode 
"在当前模式下显示当前命令
set showcmd
"鼠标支持
set mouse=a
"启用256色
set t_Co=256
"回车后缩进与上一行相同
set autoindent
"一行80字符
set textwidth=80
"自动折行
set wrap 
"折行发生在指定符号，空格与标点
set linebreak
"折行后与编辑器边缘距离
set wrapmargin=4
"垂直滚动时，光标距离顶/低部的距离
set scrolloff=5
"总显示最后一个窗口的状态行
set laststatus=2
"状态栏显示光标位置，行/列
set ruler
"括号对齐高亮
set showmatch
"总是显示标签栏
set showtabline
"搜索高亮显示
set hlsearch
"搜索忽略大小写
set ignorecase
"帮助是中文文档
set helplang=cn
"使用vim自己的设置，不兼容vi
set nocompatible
"使用utf-8编码
set encoding=utf-8
set fenc=utf-8
set fileencodings=utf-8,gbk,cp936,latin-1
"文件格式是unix
set fileformat=unix
"其他可能的格式
set fileformats=unix,mac,dos
"底部命令自动补全
set wildmenu
"自动切换文件目录
set autochdir 
"文件监视，如果编辑中的文件被其他编辑器编辑了，发出警告
set autoread
"设置空白字符的视觉提示
set list listchars=extends:❯,precedes:❮,tab:▸\ 
"自动格式化 
set formatoptions=tcrqn


"------------------------------------------------------------------------------
"------------------------------python3环境-------------------------------------
"------------------------------------------------------------------------------
let g:python3_host_prog="/usr/bin/python3"


"------------------------------------------------------------------------------
"------------------------------vim快捷键设置-----------------------------------
"------------------------------------------------------------------------------
let mapleader=" "



"------------------------------------------------------------------------------
"------------------------------插件管理----------------------------------------
"------------------------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')
	"coc补全"
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	"git补全"
	Plug 'tpope/vim-fugitive'
	"文件管理"
	Plug 'preservim/nerdtree'
	"注释"
	Plug 'preservim/nerdcommenter'
	"状态栏"
	Plug 'vim-airline/vim-airline'
	"状态栏主题"
	Plug 'vim-airline/vim-airline-themes'
	"标签管理"
	Plug 'godlygeek/tabular'
	"markdown高亮"
	Plug 'plasticboy/vim-markdown'
	"markdown预览"
	Plug 'iamcco/markdown-preview.nvim',{ 'do': 'cd app && yarn install'  }
call plug#end()


"------------------------------------------------------------------------------
"------------------------------nerdtree快捷键设置------------------------------
"------------------------------------------------------------------------------
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>


"------------------------------------------------------------------------------
"------------------------------nerdcomments快捷键设置--------------------------
"------------------------------------------------------------------------------
" Create default mappings
let g:NERDCreateDefaultMappings = 1

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not 
let g:NERDToggleCheckAllLines = 1


"------------------------------------------------------------------------------
"------------------------------airline设置-------------------------------------
"------------------------------------------------------------------------------
"打开状态栏
let g:airline#extensions#tabline#enabled = 1
"状态栏格式
let g:airline#extensions#tabline#formatter = 'jsformatter'


"------------------------------------------------------------------------------
"------------------------------markdown高亮设置--------------------------------
"------------------------------------------------------------------------------
let g:vim_markdown_math = 1
autocmd Filetype markdown noremap <leader>m :MarkdownPreview<CR>
autocmd Filetype markdown noremap <leader>ms :MarkdownPreviewStop<CR>

"------------------------------------------------------------------------------
"------------------------------tagbar高亮设置--------------------------------
"------------------------------------------------------------------------------
"设置tagber对于markdown的支持
let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Chapter',
        \ 'i:Section',
        \ 'k:Paragraph',
        \ 'j:Subparagraph'
    \ ]
\ }
"取消显示warning部分
let g:airline_section_warning = ''
"取消显示section_b
let g:airline_section_b = ''
"section_c显示为tagbar检索出来的标题
let g:airline_section_c = airline#section#create(['tagbar'])
"section_x显示文件名
let g:airline_section_x = '%{expand("%")}'
"section_y显示时间
let g:airline_section_y = airline#section#create(['%{strftime("%D")}'])
"section_z显示日期
let g:airline_section_z = airline#section#create(['%{strftime("%H:%M")}'])
"激活tagbar扩展
let g:airline#extensions#tagbar#enabled = 1


"------------------------------------------------------------------------------
"------------------------------coc-nvim设置------------------------------------
"------------------------------------------------------------------------------
"允许在有未保存的修改时切换缓冲区，此时的修改由 vim 负责保存
set hidden
"相应时间
set updatetime=300
"不要发送信息到补全
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `<C-P>` and `<C-n>` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> <C-p> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-n> <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>


"------------------------------------------------------------------------------
"------------------------------coc-snippets快捷键设置--------------------------
"------------------------------------------------------------------------------
"设置输入下的tap键提示
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'


