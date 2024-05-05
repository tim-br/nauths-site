#!/bin/bash

# Define variables for deployment details
DEPLOYMENT_LABEL="app=caddy"
CONTAINER_NAME="caddy"
DESTINATION_PATH="/usr/share/caddy/"
SOURCE_PATH="./_site/web-content"

# Check if the source directory exists and is not empty
if [ ! -d "$SOURCE_PATH" ] || [ -z "$(ls -A $SOURCE_PATH)" ]; then
  echo "Error: Source directory does not exist or is empty: $SOURCE_PATH"
  exit 1
fi

# Get the first pod name matching the deployment label
POD_NAME=$(kubectl get pods -l $DEPLOYMENT_LABEL -o jsonpath='{.items[0].metadata.name}')
if [ -z "$POD_NAME" ]; then
  echo "Error: No pods found matching the deployment label $DEPLOYMENT_LABEL."
  exit 1
fi

# Navigate to source path and copy files to the pod
pushd $SOURCE_PATH
if ! kubectl cp ./ $POD_NAME:$DESTINATION_PATH -c $CONTAINER_NAME; then
  echo "Error: Failed to copy files to pod $POD_NAME."
  popd  # Ensure the directory is popped regardless of success or failure
  exit 1
fi
popd

echo "Files successfully copied to $POD_NAME:$DESTINATION_PATH"