#!/bin/bash

echo "iCloud"

if hash icloudpd 2>/dev/null; then
    icloudpd --directory /mnt/memories/photos \
             --username $ICLOUD_USERNAME \
             --password $ICLOUD_PASSWORD
else
    echo "icloudpd is not installed, please run 'pip install icloudpd'"
fi


