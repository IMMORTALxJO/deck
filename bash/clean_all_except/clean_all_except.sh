#!/bin/bash

BINS=()
NODEJS=()
DIRS=()

RM_COMMAND=('find' '/' '-type' 'f'  '!' '-path' '"/"')

function RM_COMMAND_PLUS_FILE {
  RM_COMMAND+=('!' '-path' "'${1}'")
}

function RM_COMMAND_PLUS_DIR {
  RM_COMMAND+=('!' '-path' '"'${1}'"' '!' '-path' '"'${1}'/*"')
}

RM_COMMAND_PLUS_DIR /proc
RM_COMMAND_PLUS_DIR /dev
RM_COMMAND_PLUS_DIR /sys
RM_COMMAND_PLUS_FILE /etc/hosts
RM_COMMAND_PLUS_FILE /etc/passwd
RM_COMMAND_PLUS_FILE /etc/shadow
RM_COMMAND_PLUS_FILE /etc/resolv.conf
RM_COMMAND_PLUS_FILE /etc/hostname

declare -A BINS_SCANNED

while (($#))
do
  arg=$1
  shift
  case "${arg}" in
    --bin)
      BINS+=("$1")
      shift
    ;;
    --nodejs)
      NODEJS+=("$1")
      shift
    ;;
    --dir)
      RM_COMMAND_PLUS_DIR "$1"
      shift
    ;;
    --file)
      RM_COMMAND_PLUS_FILE "$1"
      shift
    ;;
    --it-is-not-docker)
      FORCE_DOCKER_CHECK=1
    ;;
    --run-delete)
      RUN_DELETE=1
    ;;
  esac;
done

if ! cat /proc/self/cgroup | grep docker > /dev/null; then
  echo "I'm not in docker!"
  if [[ -z $FORCE_DOCKER_CHECK ]]; then
    exit 1
  fi;
fi;



function SCAN_BIN {
  local bin_path="${1}"
  if [ ${BINS_SCANNED["${bin_path}"]+_} ]; then
    echo "BIN: CACHE: ${bin_path}"
    return 0
  fi;
  echo "BIN: PARSE: ${bin_path}"
  BINS_SCANNED["${bin_path}"]='yes'
  RM_COMMAND_PLUS_FILE "${bin_path}"
  local lib_names=$(ldd "${bin_path}" | awk '{print $1;}' | xargs whereis | cut -d ':' -f2 | grep -v '^$' | sed 's/^ //g')
  for lib_name in $lib_names; do
    local lib_paths=$(whereis "${lib_name}" | cut -d' ' -f2)
    for lib_path in $lib_paths; do
      local target_path=$(readlink -f "${lib_path}") # symlink read
      if [[ "${target_path}" != "${lib_path}" ]]; then
        RM_COMMAND_PLUS_FILE "${lib_path}"
      fi;
      RM_COMMAND_PLUS_FILE "${target_path}"
      SCAN_BIN "${target_path}"
    done;
  done;
}

function SCAN_BINS {
  for f in "${BINS[@]}"; do
    SCAN_BIN "${f}"
  done;
  NO_DELETE_FILES=($( printf '%s\n' "${NO_DELETE_FILES[@]}" | sort | uniq ))
}

SCAN_BINS

RM_COMMAND+=('-delete')
echo "${RM_COMMAND[@]}"

if [[ ! -z $RUN_DELETE ]];then
  "${RM_COMMAND[@]}" > /dev/null 2>&1 || true
fi;