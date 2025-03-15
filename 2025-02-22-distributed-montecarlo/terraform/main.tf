# main.tf

terraform {
  required_providers {
    digitalocean = {
      source = "digitalocean/digitalocean"
      version = "~> 2.0"
    }
  }
}

provider "digitalocean" {
  token = var.do_token
}

variable "do_token" {
  description = "DigitalOcean API token"
  sensitive = true
}

resource "digitalocean_kubernetes_cluster" "distributed_cluster" {
  name = "haskell-distributed"
  region = "nyc1"
  version = "1.32.1-do.0"

  node_pool {
    name = "worker-pool"
    size = "s-2vcpu-2gb"
    node_count = 10
  }
}

resource "digitalocean_container_registry" "atc_demo_docr" {
  name = "atc-demo-docr"
  subscription_tier_slug = "basic"
  region = "nyc3"
}

output "cluster_endpoint" {
  value = digitalocean_kubernetes_cluster.distributed_cluster.endpoint
}

output "kubeconfig" {
  value = digitalocean_kubernetes_cluster.distributed_cluster.kube_config[0].raw_config
  sensitive = true
}
