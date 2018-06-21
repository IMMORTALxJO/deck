#!/bin/bash -e
src_path="$1"
dst_path="$2"

echo_usage(){
  echo "USAGE: $( basename $0 ) <src_path> <dst_path>"
}

[[ -z $src_path ]] && [[ -z $dst_path ]] &&
  echo_usage &&
  exit 1

[[ -d $src_path ]] && [[ -d $dst_path ]] && {
  cd $src_path
  files_list=$(find ./ -type f)
  cd -
  for src_filename in ${files_list[@]}
  do
    src_full_path=$( realpath $src_path/$src_filename )
    dst_full_path=$( realpath $dst_path/$src_filename )
    [[ ! -f $dst_full_path ]] && {
      echo "File $dst_full_path does not exist"
    } || {
      [[ $(md5sum $src_full_path | cut -d ' ' -f 1 ) != $(md5sum $dst_full_path | cut -d ' ' -f 1 ) ]] &&
        $0 $src_full_path $dst_full_path
    }
  done
  exit 0
}

[[ ! -f $src_path ]] || [[ ! -f $dst_path ]] &&
	echo_usage &&
	exit 1

clean_tmp_files(){
  rm -f $tmp_patch_file $tmp_save_file $tmp_edit_file
  echo -e "\033[0m"
  exit 0
}
trap clean_tmp_files EXIT HUP INT QUIT PIPE TERM

save_prev_block(){
  [[ ${#tmp_block[@]} -eq 0 ]] && return 0
  echo "$dst_path <= $src_path"
  printf "\033[01;34m %s\033[1;33m\n" ${tmp_block[@]}
  read -n 1 -p 'MERGE THIS BLOCK ? y/n or e (edit and merge) ' answer
  echo -e "\033[0m"
  [[ ${answer,,} == 'q' ]] && {
    echo -en "\033[31mExit from merging to $dst_path\033[0m"
    return 1
  }
  [[ ${answer,,} == 'e' ]] && {
    printf "%s\n" ${tmp_block[@]} > $tmp_edit_file
    editor $tmp_edit_file
    tmp_block=( $(cat $tmp_edit_file ) )
    echo -e '\033[1;32mDiff edited:\033[0m'
    save_prev_block
    return 0
  }
  [[ ${answer,,} == 'y' ]] && printf "%s\n" ${tmp_block[@]} >> $tmp_patch_file
  tmp_block=()
  return 0
}

merge(){
  [[ $( wc -l $tmp_patch_file | cut -d ' ' -f 1 ) -lt 2 ]] && return 0
  clear
  echo -e "\033[31m#### MERGE TIME ########################\033[0m\n$dst_path <= $src_path\033[01;34m"
  cat $tmp_patch_file
  echo -en '\033[1;31m'
  read -n 1 -p 'COMMIT THIS CHANGES ? (y/n) ' answer
  [[ ${answer,,} == 'y' ]] && {
    patch -s $dst_path -i $tmp_patch_file -o $tmp_save_file
    mv $tmp_save_file $dst_path
    echo -e '\033[1;32mCOMMITED =)'
  }
  return 0
}

tmp_block=()
tmp_patch_file=$(mktemp)
tmp_save_file=$(mktemp)
tmp_edit_file=$(mktemp)

set +e
diff_text=$( diff $dst_path $src_path )
set -e

IFS=$'\n'
for line in $diff_text
do
  [[ $line =~ ^([0-9]*)(\,[0-9]*)?[a-z]([0-9]*)(\,[0-9]*)?$ ]] && save_prev_block
  tmp_block+=($line)
done

save_prev_block
merge

exit 0