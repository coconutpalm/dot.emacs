# K8s dev-o-matic

## First things first

* Local Docker repository
* `dev` container working

## Baseline config

* kind create cluster
* Specify host directory for k8s storage allocation / configure storage
* Docker registry inside k8s - https://kind.sigs.k8s.io/docs/user/private-registries/ .  Registry expects to run on port 443, so in k8s one could set up a "registry" service exposing the registry on port 443 and have a nice DNS name to push to.
* Git hosting inside k8s for gitops
* Get dev environment running as a privileged pod with SSHD port-forwarded
* Postgres
* Rabbit

Test URL access to PG and Rabbit from inside dev

## See also

Custom WSL DNS settings
* https://gist.github.com/coltenkrauter/608cfe02319ce60facd76373249b8ca6
* https://superuser.com/questions/1533291/how-do-i-change-the-dns-settings-for-wsl2
* See also haproxy?

Local DNS and Kind
* https://mjpitz.com/blog/2020/10/21/local-ingress-domains-kind/

More `dnsmasq`:
* https://programmer.help/blogs/docker-sets-up-dns-server-in-1-minute.html
* https://github.com/jpillora/docker-dnsmasq
* https://blog.csainty.com/2016/09/running-dnsmasq-in-docker.html

Trow - container registry
* https://github.com/ContainerSolutions/trow
* https://blog.container-solutions.com/installing-a-registry-on-kubernetes-quickstart

Ambassador / Telepresence - Temporarily route traffic to a service dev instance
* https://www.getambassador.io/
* https://www.telepresence.io/docs/latest/reference/dns/

## Remote access

* Ingress to Guacamole
* Apache Guacamole to dev

## Quality-of-life

* Add /etc/hosts entries for Rabbit and PostgreSQL to dev
* .pim DNS suffix routing to a corresponding namespace
  * Matrix homeserver with Slack puppet bridge

# Closer `dev` integration

* If `docker` isn't available, try `podman` command
* `dev k8s <command>`

* Persistent Volumes/claims

https://stackoverflow.com/questions/62694361/how-to-reference-a-local-volume-in-kind-kubernetes-in-docker
https://mauilion.dev/posts/kind-pvc/

* `dev rebuild` or `dev promote` for promoting Docker images?

* KubeSail

(Ingress; see starter script)

* Apache Guacamole

https://github.com/thomas-illiet/k8s-guacamole
https://github.com/lorenzogirardi/kubernetes-guacamole

* Matrix homeserver

## Operators / GitOps
