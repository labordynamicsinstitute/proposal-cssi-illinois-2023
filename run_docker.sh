#!/bin/bash

if [[ "$1" == "-h" ]]
then
cat << EOF
$0 (tag)

will run Docker with tag (TAG), then run R files within.

If (TAG) not specified, will run latest.

EOF
exit 0
fi

PWD=$(pwd)
repo=${PWD##*/}
if [[ "$1" == "" ]]
then
  tag=latest
else
  tag=$1
fi

space=larsvilhuber
case $USER in
  *vilhuber|*herbert)
  WORKSPACE=$PWD
  ;;
  codespace)
  WORKSPACE=/workspaces
  ;;
esac
  
# build the docker if necessary

docker pull $space/$repo:$tag


docker run -e DISABLE_AUTH=true -v "$WORKSPACE":/home/rstudio -w /home/rstudio --rm  $space/$repo:$tag /bin/bash ./run.sh
