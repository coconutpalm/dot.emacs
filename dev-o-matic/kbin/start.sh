#!/usr/bin/env -S bash

cat <<\#EOF | kind create cluster --config=-
# cluster-config.yml
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
name: development

networking:
  apiServerAddress: "127.0.0.1"
  apiServerPort: 7443

nodes:
- role: control-plane

  kubeadmConfigPatches:
  - |
    kind: InitConfiguration
    nodeRegistration:
      kubeletExtraArgs:
        node-labels: "ingress-ready=true"

  extraMounts:
  - hostPath: /var/run/docker.sock
    containerPath: /var/run/docker.sock
    propogation: Bidirectional
    consistency: delegated
  - hostPath: ${DOCKER_DEV_USERSTATE}
    containerPath: target=/home
    propogation: Bidirectional
    consistency: delegated
  - hostPath: ${DOCKER_DEV_CONFDIR}
    containerPath: /tmp/devrc.rc
    propogation: Bidirectional
    consistency: delegated
  - hostPath: ${DOCKER_DEV_USERDOCS}
    containerPath: /tmp/devrc.docs
    propogation: Bidirectional
    consistency: delegated

  extraPortMappings:
  - containerPort: 30000
    hostPort: 30000
    listenAddress: 127.0.0.1
    protocol: TCP
  - containerPort: 31000
    hostPort: 31000
    listenAddress: 127.0.0.1
    protocol: TCP
  - containerPort: 22
    hostPort: 2222
    protocol: TCP
  - containerPort: 5901
    hostPort: 5901
    listenAddress: 127.0.0.1
    protocol: TCP
  - containerPort: 5902
    hostPort: 5902
    listenAddress: 127.0.0.1
    protocol: TCP
  - containerPort: 80
    hostPort: 80
    protocol: TCP
  - containerPort: 443
    hostPort: 443
    protocol: TCP

#EOF
