#!/bin/bash
echo "Generating 1.2m Accounts"
docker run -it -v ${PWD}/files:/files data-gen-node:latest /bin/bash -c "node /src/datascript-acct.js > /files/accounts1.2m.txt"

echo "Generating 10m transactions"
docker run -it -v ${PWD}/files:/files data-gen-node:latest /bin/bash -c "node /src/datascript-trans.js > /files/transactions10m.txt"
docker run -it -v ${PWD}/files:/files data-gen-node:latest /bin/bash -c "node /src/datascript-trans.js >> /files/transactions10m.txt"
echo "10m transactions"

echo ""
echo "Dump stats"
echo ""
wc -l files/accounts1.2m.txt
wc -l files/transactions10m.txt