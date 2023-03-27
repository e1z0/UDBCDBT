#!/bin/bash

detectos() {
UNAME=$(uname)
RESULT=unknown
if [ "$UNAME" == "Linux" ]; then
RESULT=linux
elif [ "$UNAME" == "Darwin" ]; then
RESULT=darwin
fi
echo -n ${RESULT}
}
