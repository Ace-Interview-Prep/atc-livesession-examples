
resource "digitalocean_kubernetes_cluster" "ml_service_cluster" {
  name = var.cluster_name
  region = var.region
  version = "1.32.1-do.0"

  node_pool {
    name = "worker-pool"
    size = var.node_size
    node_count = var.node_count
  }
}

resource "digitalocean_container_registry" "atc_demo_docr" {
  name = "atc-demo-docr"
  subscription_tier_slug = "basic"
  region = "nyc3"
}

resource "kubernetes_namespace" "ml_service_namespace" {
  metadata {
    name = "ml-service"
  }
}

resource "kubernetes_deployment" "ml_service_deployment" {
  metadata {
    name = "ml-service"
    namespace = kubernetes_namespace.ml_service_namespace.metadata[0].name
  }

  spec {
    replicas = 3

    selector {
      match_labels = {
        app = "ml-service"
      }
    }

    template {
      metadata {
        labels = {
          app = "ml-service"
        }
      }

      spec {
        image_pull_secrets {
          name = "do-registry-secret"
        }
        container {
          name = "ml-service"
          image = "${digitalocean_container_registry.atc_demo_docr.endpoint}/my-ml-service:latest"
          port {
            container_port = 8080
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "ml_service_service" {
  metadata {
    name = "ml-service"
    namespace = kubernetes_namespace.ml_service_namespace.metadata[0].name
  }

  spec {
    selector = {
      app = "ml-service"
    }

    port {
      port = 80
      target_port = 8080
    }

    type = "LoadBalancer"
  }
}

# resource "digitalocean_kubernetes_namespace" "ml_service_namespace" {
#   cluster_id = digitalocean_kubernetes_cluster.ml_service_cluster.id
#   name = "ml-service"
# }

# resource "digitalocean_kubernetes_deployment" "ml_service_deployment" {
#   cluster_id = digitalocean_kubernetes_cluster.ml_service_cluster.id
#   namespace = digitalocean_kubernetes_namespace.ml_service_namespace.name

#   metadata {
#     name = "ml-service"
#   }

#   spec {
#     replicas = 3

#     selector {
#       match_labels = {
#         app = "ml-service"
#       }
#     }

#     template {
#       metadata {
#         labels = {
#           app = "ml-service"
#         }
#       }

#       spec {
#         container {
#           name = "ml-service"
#           image = "${digitalocean_container_registry.ml_service_registry.endpoint}/my-ml-service:latest"
#           ports {
#             container_port = 8080
#           }
#         }
#       }
#     }
#   }
# }

# resource "digitalocean_kubernetes_service" "ml_service_service" {
#   cluster_id = digitalocean_kubernetes_cluster.ml_service_cluster.id
#   namespace = digitalocean_kubernetes_namespace.ml_service_namespace.name

#   metadata {
#     name = "ml-service"
#   }

#   spec {
#     selector = {
#       app = "ml-service"
#     }

#     port {
#       port = 80
#       target_port = 8080
#     }

#     type = "LoadBalancer"
#   }
# }
