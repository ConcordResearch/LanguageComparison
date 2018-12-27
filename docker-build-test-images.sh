#!/bin/bash

if [ -z ${SKIP_IMAGE_PULL} ]; then
    echo ""
    echo "Pulling all required images"
    echo ""
    
    IMAGE_NAME="node:10.14"
    docker pull ${IMAGE_NAME} &> /dev/null
    echo "${IMAGE_NAME} - Done"

    IMAGE_NAME="microsoft/dotnet:2.2-sdk"
    docker pull ${IMAGE_NAME} &> /dev/null
    echo "${IMAGE_NAME} - Done"

    IMAGE_NAME="fpco/stack-build:lts-12"
    docker pull ${IMAGE_NAME} &> /dev/null
    echo "${IMAGE_NAME} - Done"

    IMAGE_NAME="rust:1.31"
    docker pull ${IMAGE_NAME} &> /dev/null
    echo "${IMAGE_NAME} - Done"

    IMAGE_NAME="elixir:1.7"
    docker pull ${IMAGE_NAME} &> /dev/null
    echo "${IMAGE_NAME} - Done"

    echo "You can skip trying to pull the next time by using this 'export SKIP_IMAGE_PULL=\"true\"'."
fi


echo ""
echo "Remove all stopped containers"
echo ""
docker container prune --force &> /dev/null

echo ""
echo "Remove unused images"
echo ""
docker image prune --force &> /dev/null


echo ""
echo "Building data_gen image"
echo ""
docker build -f Dockerfile-data_gen . -t data-gen-node:latest &> /dev/null

echo ""
echo "Generate data for tests"
echo ""
chmod +x data_gen.sh
./data_gen.sh

echo ""
echo "Building .NET Core version"
echo ""
docker build -f Dockerfile-cs . -t lang-comp-cs:latest

echo ""
echo "Building Rust version"
echo ""
docker build -f Dockerfile-rs . -t lang-comp-rs:latest

echo ""
echo "Building Haskell version"
echo ""
docker build -f Dockerfile-hs . -t lang-comp-hs:latest

echo ""
echo "Building PureScript version"
echo ""
docker build -f Dockerfile-ps . -t lang-comp-ps:latest

echo ""
echo "Building Elixir version"
echo ""
docker build -f Dockerfile-ex . -t lang-comp-ex:latest


echo ""
echo "Generated images sizes"
echo ""
docker images lang-comp-* --format '{{.Size}}\t-\t{{.Repository}}:{{.Tag}}'
