#!/usr/bin/env -S bash -x

$USER=$1

[ ! -f ~/.aws/config ] && cat > ~/.aws/config <<EOF
; Put default AWS configuration into configure-aws.sh
; and rebuild the container.
EOF
