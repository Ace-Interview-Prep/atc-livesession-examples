variable "do_token" {
  description = "DigitalOcean API token"
  type = string
  sensitive = true
}

variable "cluster_name" {
  description = "Name of the Kubernetes cluster"
  type = string
  default = "ml-service-cluster"
}

variable "region" {
  description = "DigitalOcean region"
  type = string
  default = "nyc1"
}

variable "node_size" {
  description = "Size of the Kubernetes nodes"
  type = string
  default = "s-2vcpu-2gb"
}

variable "node_count" {
  description = "Number of nodes in the Kubernetes cluster"
  type = number
  default = 2
}
