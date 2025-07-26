terraform {
  required_version = ">= 1.0"
  
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.11"
    }
  }
  
  backend "gcs" {
    bucket = "suitecrm-cobol-terraform-state"
    prefix = "gcp/terraform.tfstate"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

locals {
  project_name = "suitecrm-cobol"
  environment  = var.environment
  region       = var.region
  
  common_labels = {
    project     = "suitecrm-cobol-bridge"
    environment = var.environment
    managed-by  = "terraform"
  }
}

# Enable required APIs
resource "google_project_service" "required_apis" {
  for_each = toset([
    "compute.googleapis.com",
    "container.googleapis.com",
    "sqladmin.googleapis.com",
    "redis.googleapis.com",
    "storage.googleapis.com",
    "cloudkms.googleapis.com",
    "logging.googleapis.com",
    "monitoring.googleapis.com",
    "cloudtrace.googleapis.com",
  ])
  
  service = each.value
  
  disable_on_destroy = false
}

# VPC Network
resource "google_compute_network" "main" {
  name                    = "${local.project_name}-${local.environment}-vpc"
  auto_create_subnetworks = false
  
  depends_on = [google_project_service.required_apis]
}

# Subnets
resource "google_compute_subnetwork" "gke" {
  name          = "${local.project_name}-${local.environment}-gke-subnet"
  ip_cidr_range = var.gke_subnet_cidr
  region        = local.region
  network       = google_compute_network.main.id
  
  secondary_ip_range {
    range_name    = "gke-pods"
    ip_cidr_range = var.gke_pods_cidr
  }
  
  secondary_ip_range {
    range_name    = "gke-services"
    ip_cidr_range = var.gke_services_cidr
  }
  
  private_ip_google_access = true
}

# Cloud NAT for private GKE nodes
resource "google_compute_router" "main" {
  name    = "${local.project_name}-${local.environment}-router"
  region  = local.region
  network = google_compute_network.main.id
}

resource "google_compute_router_nat" "main" {
  name                               = "${local.project_name}-${local.environment}-nat"
  router                             = google_compute_router.main.name
  region                             = google_compute_router.main.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"
  
  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# GKE Cluster
resource "google_container_cluster" "main" {
  name     = "${local.project_name}-${local.environment}-gke"
  location = var.zone != null ? var.zone : local.region
  
  # We can't create a cluster with no node pool defined, but we want to only use
  # separately managed node pools. So we create the smallest possible default
  # node pool and immediately delete it.
  remove_default_node_pool = true
  initial_node_count       = 1
  
  network    = google_compute_network.main.name
  subnetwork = google_compute_subnetwork.gke.name
  
  # Cluster configuration
  min_master_version = var.kubernetes_version
  
  release_channel {
    channel = var.release_channel
  }
  
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }
  
  ip_allocation_policy {
    cluster_secondary_range_name  = "gke-pods"
    services_secondary_range_name = "gke-services"
  }
  
  private_cluster_config {
    enable_private_nodes    = true
    enable_private_endpoint = false
    master_ipv4_cidr_block  = var.master_ipv4_cidr_block
  }
  
  master_authorized_networks_config {
    dynamic "cidr_blocks" {
      for_each = var.authorized_networks
      content {
        cidr_block   = cidr_blocks.value.cidr_block
        display_name = cidr_blocks.value.display_name
      }
    }
  }
  
  network_policy {
    enabled  = true
    provider = "CALICO"
  }
  
  addons_config {
    http_load_balancing {
      disabled = false
    }
    
    horizontal_pod_autoscaling {
      disabled = false
    }
    
    network_policy_config {
      disabled = false
    }
    
    gce_persistent_disk_csi_driver_config {
      enabled = true
    }
  }
  
  # Security
  database_encryption {
    state    = "ENCRYPTED"
    key_name = google_kms_crypto_key.gke.id
  }
  
  # Logging and monitoring
  logging_config {
    enable_components = [
      "SYSTEM_COMPONENTS",
      "WORKLOADS",
    ]
  }
  
  monitoring_config {
    enable_components = [
      "SYSTEM_COMPONENTS",
      "WORKLOADS",
    ]
  }
  
  maintenance_policy {
    recurring_window {
      start_time = "2023-01-01T00:00:00Z"
      end_time   = "2023-01-01T04:00:00Z"
      recurrence = "FREQ=WEEKLY;BYDAY=SU"
    }
  }
  
  resource_labels = local.common_labels
  
  depends_on = [google_project_service.required_apis]
}

# Node pool for general workloads
resource "google_container_node_pool" "general" {
  name       = "general-pool"
  location   = google_container_cluster.main.location
  cluster    = google_container_cluster.main.name
  node_count = var.general_node_count
  
  autoscaling {
    min_node_count = var.general_min_node_count
    max_node_count = var.general_max_node_count
  }
  
  management {
    auto_repair  = true
    auto_upgrade = true
  }
  
  node_config {
    preemptible  = var.environment != "prod"
    machine_type = var.general_machine_type
    disk_size_gb = 100
    disk_type    = "pd-standard"
    
    metadata = {
      disable-legacy-endpoints = "true"
    }
    
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]
    
    labels = merge(local.common_labels, {
      node-pool = "general"
    })
    
    workload_metadata_config {
      mode = "GKE_METADATA"
    }
    
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }
  }
}

# Node pool for COBOL workloads
resource "google_container_node_pool" "cobol" {
  name       = "cobol-pool"
  location   = google_container_cluster.main.location
  cluster    = google_container_cluster.main.name
  node_count = var.cobol_node_count
  
  autoscaling {
    min_node_count = var.cobol_min_node_count
    max_node_count = var.cobol_max_node_count
  }
  
  management {
    auto_repair  = true
    auto_upgrade = true
  }
  
  node_config {
    preemptible  = false
    machine_type = var.cobol_machine_type
    disk_size_gb = 200
    disk_type    = "pd-ssd"
    
    metadata = {
      disable-legacy-endpoints = "true"
    }
    
    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]
    
    labels = merge(local.common_labels, {
      node-pool = "cobol"
      workload  = "cobol"
    })
    
    taint {
      key    = "workload"
      value  = "cobol"
      effect = "NO_SCHEDULE"
    }
    
    workload_metadata_config {
      mode = "GKE_METADATA"
    }
    
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }
  }
}

# Cloud SQL MySQL Instance
resource "google_sql_database_instance" "mysql" {
  name             = "${local.project_name}-${local.environment}-mysql"
  database_version = var.mysql_version
  region           = local.region
  
  settings {
    tier              = var.mysql_tier
    availability_type = var.environment == "prod" ? "REGIONAL" : "ZONAL"
    disk_size         = var.mysql_disk_size
    disk_type         = "PD_SSD"
    disk_autoresize   = true
    
    backup_configuration {
      enabled                        = true
      start_time                     = "02:00"
      location                       = local.region
      point_in_time_recovery_enabled = var.environment == "prod"
      transaction_log_retention_days = var.environment == "prod" ? 7 : 1
      
      backup_retention_settings {
        retained_backups = var.environment == "prod" ? 30 : 7
        retention_unit   = "COUNT"
      }
    }
    
    ip_configuration {
      ipv4_enabled    = false
      private_network = google_compute_network.main.id
      require_ssl     = true
    }
    
    database_flags {
      name  = "character_set_server"
      value = "utf8mb4"
    }
    
    database_flags {
      name  = "max_connections"
      value = "500"
    }
    
    database_flags {
      name  = "innodb_buffer_pool_size"
      value = "2147483648"  # 2GB
    }
    
    insights_config {
      query_insights_enabled  = true
      query_string_length     = 1024
      record_application_tags = true
      record_client_address   = true
    }
    
    maintenance_window {
      day          = 7
      hour         = 2
      update_track = "stable"
    }
    
    user_labels = local.common_labels
  }
  
  depends_on = [
    google_project_service.required_apis,
    google_service_networking_connection.private_vpc_connection
  ]
}

# Cloud SQL Database
resource "google_sql_database" "suitecrm" {
  name     = "suitecrm"
  instance = google_sql_database_instance.mysql.name
  charset  = "utf8mb4"
}

# Cloud SQL User
resource "google_sql_user" "suitecrm" {
  name     = "suitecrm"
  instance = google_sql_database_instance.mysql.name
  password = var.mysql_password
}

# Private VPC connection for Cloud SQL
resource "google_compute_global_address" "private_ip_address" {
  name          = "${local.project_name}-${local.environment}-private-ip"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.main.id
}

resource "google_service_networking_connection" "private_vpc_connection" {
  network                 = google_compute_network.main.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_address.name]
}

# Memorystore Redis Instance
resource "google_redis_instance" "main" {
  name           = "${local.project_name}-${local.environment}-redis"
  tier           = var.environment == "prod" ? "STANDARD_HA" : "BASIC"
  memory_size_gb = var.redis_memory_size_gb
  region         = local.region
  
  location_id             = "${local.region}-a"
  alternative_location_id = var.environment == "prod" ? "${local.region}-b" : null
  
  authorized_network = google_compute_network.main.id
  connect_mode       = "PRIVATE_SERVICE_ACCESS"
  
  redis_version = var.redis_version
  display_name  = "${local.project_name}-${local.environment}-redis"
  
  redis_configs = {
    maxmemory-policy = "allkeys-lru"
  }
  
  labels = local.common_labels
  
  depends_on = [google_project_service.required_apis]
}

# Cloud Storage Bucket for uploads
resource "google_storage_bucket" "uploads" {
  name          = "${local.project_name}-${local.environment}-uploads-${var.project_id}"
  location      = local.region
  force_destroy = var.environment != "prod"
  
  uniform_bucket_level_access = true
  
  versioning {
    enabled = true
  }
  
  lifecycle_rule {
    condition {
      age = 90
    }
    action {
      type = "Delete"
    }
  }
  
  encryption {
    default_kms_key_name = google_kms_crypto_key.storage.id
  }
  
  labels = local.common_labels
  
  depends_on = [google_project_service.required_apis]
}

# Cloud Storage Bucket for backups
resource "google_storage_bucket" "backups" {
  name          = "${local.project_name}-${local.environment}-backups-${var.project_id}"
  location      = local.region
  force_destroy = false
  
  uniform_bucket_level_access = true
  
  versioning {
    enabled = true
  }
  
  lifecycle_rule {
    condition {
      age = 30
    }
    action {
      type          = "SetStorageClass"
      storage_class = "NEARLINE"
    }
  }
  
  lifecycle_rule {
    condition {
      age = 365
    }
    action {
      type = "Delete"
    }
  }
  
  encryption {
    default_kms_key_name = google_kms_crypto_key.storage.id
  }
  
  labels = local.common_labels
  
  depends_on = [google_project_service.required_apis]
}

# KMS Keyring
resource "google_kms_key_ring" "main" {
  name     = "${local.project_name}-${local.environment}-keyring"
  location = local.region
  
  depends_on = [google_project_service.required_apis]
}

# KMS Keys
resource "google_kms_crypto_key" "gke" {
  name     = "${local.project_name}-${local.environment}-gke-key"
  key_ring = google_kms_key_ring.main.id
  
  rotation_period = "7776000s" # 90 days
  
  lifecycle {
    prevent_destroy = true
  }
}

resource "google_kms_crypto_key" "storage" {
  name     = "${local.project_name}-${local.environment}-storage-key"
  key_ring = google_kms_key_ring.main.id
  
  rotation_period = "7776000s" # 90 days
  
  lifecycle {
    prevent_destroy = true
  }
}

# Service Account for workload identity
resource "google_service_account" "workload_identity" {
  account_id   = "${local.project_name}-${local.environment}-wi"
  display_name = "Workload Identity for ${local.project_name}"
}

# IAM bindings for workload identity
resource "google_service_account_iam_binding" "workload_identity" {
  service_account_id = google_service_account.workload_identity.name
  role               = "roles/iam.workloadIdentityUser"
  
  members = [
    "serviceAccount:${var.project_id}.svc.id.goog[suitecrm-cobol/suitecrm]",
    "serviceAccount:${var.project_id}.svc.id.goog[suitecrm-cobol/api-gateway]",
  ]
}

# Grant necessary permissions to the service account
resource "google_project_iam_member" "workload_identity_storage" {
  project = var.project_id
  role    = "roles/storage.objectAdmin"
  member  = "serviceAccount:${google_service_account.workload_identity.email}"
}

resource "google_project_iam_member" "workload_identity_sql" {
  project = var.project_id
  role    = "roles/cloudsql.client"
  member  = "serviceAccount:${google_service_account.workload_identity.email}"
}

# Outputs
output "gke_cluster_name" {
  value = google_container_cluster.main.name
}

output "gke_cluster_endpoint" {
  value     = google_container_cluster.main.endpoint
  sensitive = true
}

output "mysql_connection_name" {
  value = google_sql_database_instance.mysql.connection_name
}

output "mysql_private_ip" {
  value = google_sql_database_instance.mysql.private_ip_address
}

output "redis_host" {
  value = google_redis_instance.main.host
}

output "redis_port" {
  value = google_redis_instance.main.port
}

output "uploads_bucket" {
  value = google_storage_bucket.uploads.name
}

output "backups_bucket" {
  value = google_storage_bucket.backups.name
}

output "workload_identity_email" {
  value = google_service_account.workload_identity.email
}