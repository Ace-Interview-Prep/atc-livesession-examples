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

provider "kubernetes" {
  host = digitalocean_kubernetes_cluster.ml_service_cluster.endpoint
  cluster_ca_certificate = base64decode(digitalocean_kubernetes_cluster.ml_service_cluster.kube_config.0.cluster_ca_certificate)
  token = digitalocean_kubernetes_cluster.ml_service_cluster.kube_config.0.token
}
