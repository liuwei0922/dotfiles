"-----------------------------基本配置-----------------------------------------
"------------------------------------------------------------------------------
"管理init.vim用
set foldmethod=marker
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
"将制表符转化成空格
set expandtab
"设置编辑时制表符占用空格数
set tabstop=4
"设置格式化时制表符占用空格数
set shiftwidth=4
"关闭softtabstop 永远不要将空格和tab混合输入
set softtabstop=0
"反馈延迟
set ttimeoutlen=100
set conceallevel=1
"vim中文换行问题
set formatoptions+=m
"记录光标位置
augroup resCur
  autocmd!
  autocmd BufReadPost * call setpos(".", getpos("'\""))
augroup END
"不使用交换文件
set noswapfile


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
	"snippets"
	Plug 'honza/vim-snippets'
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
    "代码对齐
	Plug 'godlygeek/tabular'
	"markdown高亮"
	Plug 'plasticboy/vim-markdown'
	"markdown预览"
	Plug 'iamcco/markdown-preview.nvim',{ 'do': 'cd app && yarn install'  }
    "vimtex插件
    Plug 'lervag/vimtex'
    "输入法切换
    Plug 'lilydjwg/fcitx.vim', {'branch': 'fcitx5'}
    " rust插件
    Plug 'rust-lang/rust.vim'
call plug#end()


"------------------------------------------------------------------------------
"-----------------------------输入法快捷键设置------------------------------
"------------------------------------------------------------------------------
" let g:input_toggle = 0
" function! Fcitx2en()
"    let s:input_status = system("fcitx-remote")
"    if s:input_status == 2
"       let g:input_toggle = 1
"       let l:a = system("fcitx-remote -c")
"    endif
" endfunction
"
" function! Fcitx2zh()
"    let s:input_status = system("fcitx-remote")
"    if s:input_status != 2 && g:input_toggle == 1
"       let l:a = system("fcitx-remote -o")
"       let g:input_toggle = 0
"    endif
" endfunction
"
" set ttimeoutlen=150
" autocmd InsertLeave * call Fcitx2en()
" autocmd InsertEnter * call Fcitx2zh()
"
"
"------------------------------------------------------------------------------
"-----------------------------vimtex快捷键设置------------------------------
"------------------------------------------------------------------------------
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0

"------------------------------------------------------------------------------
"-----------------------------snippets快捷键设置------------------------------
"------------------------------------------------------------------------------
" assuming you want to use snipmate snippet engine
"ActivateAddons vim-snippets snipmate


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
let g:NERDCustomDelimiters = {'c': { 'left': '/**','right': '*/' },'toml':{'left': '#'}}

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
"------------------------------markdown设置------------------------------------
"------------------------------------------------------------------------------
"设置markdown下的本地leader键
let maplocalleader = "/"
let g:vim_markdown_math = 1
autocmd Filetype markdown noremap <leader>m :MarkdownPreview<CR>
autocmd Filetype markdown noremap <leader>ms :MarkdownPreviewStop<CR>
"减少手的移动，映射回车为<C-/>
autocmd Filetype markdown inoremap <C-/> <CR>
"标题快捷键
autocmd Filetype markdown inoremap <localLeader>f <Esc>/<++><CR>:nohlsearch<CR>i<Del><Del><Del><Del>
autocmd Filetype markdown inoremap <localLeader>1 <ESC>o#<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <localLeader>2 <ESC>o##<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <localLeader>3 <ESC>o###<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <localLeader>4 <ESC>o####<Space><Enter><++><Esc>kA
autocmd Filetype markdown inoremap <localLeader>5 <ESC>o#####<Space><Enter><++><Esc>kA
" 空格，代码，段落
autocmd Filetype markdown inoremap <localLeader>c ```<Enter><++><Enter>```<Enter><++><Enter><Esc>4kA
autocmd Filetype markdown inoremap <localLeader>s ``<++><Esc>F`i
autocmd Filetype markdown inoremap <localLeader>/ &emsp;<Esc>a
autocmd Filetype markdown inoremap <localLeader><CR> <br><Esc>a
"辅助实现自动编号,特意找了平时不用的键
autocmd Filetype markdown inoremap <expr> <localLeader><F11> Count('^# \+',1)
autocmd Filetype markdown inoremap <expr> <localLeader><Leader><F11> Count(' \\tag{\d\+-\d\+}',Findtitle())+1
autocmd Filetype markdown inoremap <expr> <localLeader><F12> eval(Count('\[\^\d\+\]',1)+1)
"行间公式，带自动编号
autocmd Filetype markdown imap <localLeader>q <ESC>o$$<Enter><Enter> \tag{ <localLeader><F11>-<localLeader><Leader><F11>}$$<Enter><BS><++><Esc>2kA
"插入自动编号的引用
autocmd Filetype markdown imap <localLeader>n [^<localLeader><F12>]<Esc>ya[Go<C-r>": <++><Esc><C-o>f]a
"行内公式，由snippets取代，不再用这里的定义，快捷键不变
autocmd Filetype markdown inoremap <localLeader>e $$<++><Esc>F$i
"也是公式，基本不用
autocmd Filetype markdown inoremap <localLeader>m $$$$<Enter><++><Esc>khi
"粗体
autocmd Filetype markdown inoremap <localLeader>b ****<++><Esc>F*hi
"下划线
autocmd Filetype markdown inoremap <localLeader>u <u></u><++><Esc>F/i<Left>
"斜体
autocmd Filetype markdown inoremap <localLeader>i **<++><Esc>F*i
"删除线
autocmd Filetype markdown inoremap <localLeader>d ~~~~<++><Esc>F~hi
"插入时间戳
autocmd Filetype markdown inoremap <F2> <br><br><Esc>o> *以下内容更新于<C-R>=strftime('%Y-%m-%d %H:%M:%S')<C-M>*<Down><Esc>o<CR>
"分隔线
autocmd Filetype markdown inoremap <localLeader>l <ESC>o--------<Enter>
"函数
" 计算某个pattern从startline到光标处出现的次数
function! Count(pattern,startline)
  let l:cnt = 0
  silent! exe a:startline . ',.s/' . a:pattern . '/\=execute(''let l:cnt += 1'')/gn'
  return l:cnt
endfunction
"计算markdown中一级标题出现的次数，用来给公式自动编号
function! Findtitle()
    for i in range(line('.'))
        if matchstr(getline(line('.')-i),'^# \+')!=#''
            let l:latesttitleline=line('.')-i
            break
        else
            let l:latesttitleline=line('.')
        endif
    endfor
    return l:latesttitleline
endfunction
" 计算某个pattern从startline到光标处出现的次数
function! Count(pattern,startline)
  let l:cnt = 0
  silent! exe a:startline . ',.s/' . a:pattern . '/\=execute(''let l:cnt += 1'')/gn'
  return l:cnt
endfunction
".Md文件也能被识别为markdown
autocmd BufNewFile,BufRead *.Md set filetype=markdown
"ejs识别为html
autocmd BufNewFile,BufRead *.ejs set filetype=html

"markdown-preview设置
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {},
    \ 'flowchart_diagrams': {}
    \ }
let g:mkdp_markdown_css = '/home/zihua/themes/zj.css'
" preview page title
" ${name} will be replace with the file name
let g:mkdp_page_title = '「${name}」'
"Vim-markdown设置
let g:vim_markdown_math = 2
let g:tex_conceal ='adbmg'
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_folding_level = 1
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_strikethrough = 0
let g:vim_markdown_new_list_item_indent = 0


"------------------------------------------------------------------------------
"------------------------------coc-nvim设置------------------------------------
"------------------------------------------------------------------------------
"允许在有未保存的修改时切换缓冲区，此时的修改由 vim 负责保存
set hidden
"相应时间
set updatetime=300
"不要发送信息到补全
set shortmess+=c

" always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" use tab for trigger completion with characters ahead and navigate.
" note: use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <tab>
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
nnoremap <silent><nowait><leader> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <leader> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <leader> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <leader> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait><leader>  <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <leader> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <leader> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait><leader>  <space>p  :<C-u>CocListResume<CR>


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


