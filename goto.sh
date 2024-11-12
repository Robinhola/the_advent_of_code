dest=$(tree -L1 -d -i --noreport -t | tail -n +2 | fzf --layout=reverse-list)

if [ -z $dest ]; then
		dest="."
fi

cd $dest
