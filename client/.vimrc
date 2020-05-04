" Config recommended for elm-vim
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1

let g:elm_syntastic_show_warnings = 1

" my elm config
let g:elm_make_show_warnings = 1

source .cocvimrc

nmap <F2> :w<CR>]g
nmap <F3> :w<CR>:!elm make src/Main.elm<CR><CR>
nmap <F5> <F3>:!scp ~/projects/kitchen_helper/server/index.html jsonserver@192.168.1.231:~/public<CR>:!scp ~/projects/kitchen_helper/server/db.json jsonserver@192.168.1.231:~<CR><CR>
