#!/bin/bash

# Find the container ID of the running container using the "siren" image
container_id=$(docker ps --filter "ancestor=siren" --format "{{.ID}}")

# Check if the container ID was found
if [ -z "$container_id" ]; then
  echo "No running container found with the image 'siren'."
  exit 1
fi

echo "Found container ID: $container_id"

# Define the source path inside the container (adjust this if necessary)
container_path="/root/siren/benchmarks"

# Define the destination path on the local host (same directory)
local_path=$(pwd)/benchmarks_ae

echo "Copying files from container ($container_id) path: $container_path to local host path: $local_path"

# Use docker cp to copy files from the container to the local machine
docker cp "$container_id:$container_path" "$local_path"

# Check if the copy was successful
if [ $? -eq 0 ]; then
  echo "Files copied successfully!"
else
  echo "Failed to copy files."
  exit 1
fi
