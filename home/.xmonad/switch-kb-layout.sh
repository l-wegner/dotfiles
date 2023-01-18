#!/bin/bash



supportedLayouts=( "$@" )
layoutcount=${#supportedLayouts[@]}

currentLayout=$(setxkbmap -query | grep layout |  awk 'END{print $2}')
echo "current $currentLayout"
for (( i=0; i<${layoutcount}; i++ ));
do
  if [ "${supportedLayouts[$i]}" = "$currentLayout" ]; then
    nextLayout=$(( (i+1)%layoutcount))
    echo "${supportedLayouts[$nextLayout]}"
    setxkbmap "${supportedLayouts[$nextLayout]}"
  fi
done
