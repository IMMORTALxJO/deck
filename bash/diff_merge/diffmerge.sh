#!/bin/bash
src_file=$1
dst_file=$2

if [[ ! -f $src_file ]] || [[ ! -f $dst_file ]]
then
	if [[ -d $src_file ]] && [[ -d $dst_file ]]
	then
		cd $src_file
		files_list=$(find ./ -type f)
                cd -
		for src_filename in ${files_list[@]}
		do
                        src_full_path=$(echo $src_file/$src_filename | sed 's;//;/;g' | sed 's;/./;/;g')
                        dst_full_path=$(echo $dst_file/$src_filename | sed 's;//;/;g' | sed 's;/./;/;g')
			[[ -f $dst_full_path ]] &&
				[[ $(md5sum $src_full_path | cut -d ' ' -f 1 ) != $(md5sum $dst_full_path | cut -d ' ' -f 1 ) ]] &&
				$0 $src_full_path $dst_full_path
		done
		exit 0
	fi
	echo 'USAGE: diffmerge.sh src_file dst_file'
	exit 1
fi

diff_text=$( diff $dst_file $src_file )
[[ $? -eq 0 ]]	&&
	echo -e "\033[1;32mFiles are identical: $src_file => $dst_file\033[0m" &&
	exit 0

tmp_block=()
tmp_patch_file=$(mktemp)
tmp_save_file=$(mktemp)

function clear_tmp_files () {
	rm -f $tmp_patch_file $tmp_save_file
	exit 0
}
trap clear_tmp_files EXIT HUP INT QUIT PIPE TERM

function save_prev_block () {
	[[ ${#tmp_block[@]} -eq 0 ]] && return 0
	echo "$src_file => $dst_file"
	printf "\033[01;34m %s\033[1;33m\n" ${tmp_block[@]}
	read -n 1 -p 'MERGE THIS BLOCK ? (y/n) ' answer
	echo -e "\033[0m"
	[[ ${answer,,} == 'y' ]] &&
		printf "%s\n" ${tmp_block[@]} >> $tmp_patch_file
	tmp_block=()
	return 0
}

function merge () {
	[[ $( wc -l $tmp_patch_file | cut -d ' ' -f 1 ) -lt 2 ]] && return 0
	clear
	echo -e '\033[31m#### MERGE TIME ########################\033[0m'
	echo "$src_file => $dst_file"
	echo -en "\033[01;34m"
	cat $tmp_patch_file
	echo -en "\033[1;31m"
	read -n 1 -p 'COMMIT THIS CHANGES ? (y/n) ' answer
	echo -e "\033[0m"
	[[ ${answer,,} == 'y' ]] &&
		patch -s $dst_file -i $tmp_patch_file -o $tmp_save_file &&
		mv $tmp_save_file $dst_file &&
		echo -e '\033[1;32mCOMMITED =)\033[0m'
	return 0
}

IFS=$'\n'
for line in $diff_text
do
	[[ $line =~ ^([0-9]*)(\,[0-9]*)?[a-z]([0-9]*)(\,[0-9]*)?$ ]] && save_prev_block
	tmp_block+=($line)
done

save_prev_block

merge

exit 0
