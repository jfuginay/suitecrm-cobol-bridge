terraform {
  required_version = ">= 1.0"
  
  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
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
  
  backend "azurerm" {
    resource_group_name  = "terraform-state-rg"
    storage_account_name = "suitecrmtfstate"
    container_name       = "tfstate"
    key                  = "azure/terraform.tfstate"
  }
}

provider "azurerm" {
  features {
    key_vault {
      purge_soft_delete_on_destroy = false
    }
  }
}

locals {
  project_name = "suitecrm-cobol"
  environment  = var.environment
  location     = var.location
  
  common_tags = {
    Project     = "SuiteCRM-COBOL-Bridge"
    Environment = var.environment
    Location    = var.location
    ManagedBy   = "Terraform"
  }
}

# Resource Group
resource "azurerm_resource_group" "main" {
  name     = "${local.project_name}-${local.environment}-rg"
  location = local.location
  tags     = local.common_tags
}

# Virtual Network
resource "azurerm_virtual_network" "main" {
  name                = "${local.project_name}-${local.environment}-vnet"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  address_space       = [var.vnet_cidr]
  
  tags = local.common_tags
}

# Subnets
resource "azurerm_subnet" "aks" {
  name                 = "aks-subnet"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = [var.aks_subnet_cidr]
}

resource "azurerm_subnet" "database" {
  name                 = "database-subnet"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = [var.database_subnet_cidr]
  
  delegation {
    name = "mysql-delegation"
    service_delegation {
      name = "Microsoft.DBforMySQL/flexibleServers"
      actions = [
        "Microsoft.Network/virtualNetworks/subnets/join/action",
      ]
    }
  }
}

resource "azurerm_subnet" "redis" {
  name                 = "redis-subnet"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = [var.redis_subnet_cidr]
}

# Network Security Groups
resource "azurerm_network_security_group" "aks" {
  name                = "${local.project_name}-${local.environment}-aks-nsg"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  
  security_rule {
    name                       = "allow-http"
    priority                   = 100
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "80"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }
  
  security_rule {
    name                       = "allow-https"
    priority                   = 101
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "443"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }
  
  tags = local.common_tags
}

# AKS Cluster
resource "azurerm_kubernetes_cluster" "main" {
  name                = "${local.project_name}-${local.environment}-aks"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  dns_prefix          = "${local.project_name}-${local.environment}"
  kubernetes_version  = var.kubernetes_version
  
  default_node_pool {
    name                = "default"
    node_count          = var.node_count
    vm_size             = var.node_size
    vnet_subnet_id      = azurerm_subnet.aks.id
    enable_auto_scaling = true
    min_count           = var.node_min_count
    max_count           = var.node_max_count
    os_disk_size_gb     = 100
    type                = "VirtualMachineScaleSets"
    
    node_labels = {
      role = "general"
    }
  }
  
  identity {
    type = "SystemAssigned"
  }
  
  network_profile {
    network_plugin    = "azure"
    network_policy    = "calico"
    load_balancer_sku = "standard"
  }
  
  addon_profile {
    oms_agent {
      enabled                    = true
      log_analytics_workspace_id = azurerm_log_analytics_workspace.main.id
    }
    
    azure_policy {
      enabled = true
    }
    
    ingress_application_gateway {
      enabled   = true
      subnet_id = azurerm_subnet.appgw.id
    }
  }
  
  tags = local.common_tags
}

# Additional Node Pool for COBOL workloads
resource "azurerm_kubernetes_cluster_node_pool" "cobol" {
  name                  = "cobol"
  kubernetes_cluster_id = azurerm_kubernetes_cluster.main.id
  vm_size               = var.cobol_node_size
  node_count            = var.cobol_node_count
  enable_auto_scaling   = true
  min_count             = var.cobol_node_min_count
  max_count             = var.cobol_node_max_count
  vnet_subnet_id        = azurerm_subnet.aks.id
  os_disk_size_gb       = 200
  
  node_labels = {
    role     = "compute"
    workload = "cobol"
  }
  
  node_taints = [
    "workload=cobol:NoSchedule"
  ]
  
  tags = local.common_tags
}

# MySQL Flexible Server
resource "azurerm_mysql_flexible_server" "main" {
  name                = "${local.project_name}-${local.environment}-mysql"
  resource_group_name = azurerm_resource_group.main.name
  location            = azurerm_resource_group.main.location
  
  administrator_login    = "suitecrm"
  administrator_password = var.mysql_admin_password
  
  sku_name   = var.mysql_sku
  version    = var.mysql_version
  storage_mb = var.mysql_storage_mb
  
  backup_retention_days        = var.environment == "prod" ? 30 : 7
  geo_redundant_backup_enabled = var.environment == "prod" ? true : false
  
  high_availability {
    mode                      = var.environment == "prod" ? "ZoneRedundant" : "Disabled"
    standby_availability_zone = var.environment == "prod" ? "2" : null
  }
  
  delegated_subnet_id = azurerm_subnet.database.id
  
  tags = local.common_tags
}

# MySQL Database
resource "azurerm_mysql_flexible_database" "main" {
  name                = "suitecrm"
  resource_group_name = azurerm_resource_group.main.name
  server_name         = azurerm_mysql_flexible_server.main.name
  charset             = "utf8mb4"
  collation           = "utf8mb4_unicode_ci"
}

# Redis Cache
resource "azurerm_redis_cache" "main" {
  name                = "${local.project_name}-${local.environment}-redis"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  capacity            = var.redis_capacity
  family              = var.redis_family
  sku_name            = var.redis_sku
  
  enable_non_ssl_port = false
  minimum_tls_version = "1.2"
  
  redis_configuration {
    enable_authentication = true
    maxmemory_policy      = "allkeys-lru"
    
    rdb_backup_enabled            = var.environment == "prod" ? true : false
    rdb_backup_frequency          = var.environment == "prod" ? 60 : null
    rdb_backup_max_snapshot_count = var.environment == "prod" ? 1 : null
  }
  
  patch_schedule {
    day_of_week    = "Sunday"
    start_hour_utc = 2
  }
  
  tags = local.common_tags
}

# Storage Account for uploads
resource "azurerm_storage_account" "uploads" {
  name                     = "${replace(local.project_name, "-", "")}${local.environment}uploads"
  resource_group_name      = azurerm_resource_group.main.name
  location                 = azurerm_resource_group.main.location
  account_tier             = "Standard"
  account_replication_type = var.environment == "prod" ? "GRS" : "LRS"
  
  blob_properties {
    versioning_enabled = true
    
    delete_retention_policy {
      days = 30
    }
  }
  
  tags = local.common_tags
}

# Storage Container
resource "azurerm_storage_container" "uploads" {
  name                  = "uploads"
  storage_account_name  = azurerm_storage_account.uploads.name
  container_access_type = "private"
}

# Key Vault for secrets
resource "azurerm_key_vault" "main" {
  name                = "${replace(local.project_name, "-", "")}${local.environment}kv"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  tenant_id           = data.azurerm_client_config.current.tenant_id
  sku_name            = "standard"
  
  soft_delete_retention_days = 90
  purge_protection_enabled   = var.environment == "prod" ? true : false
  
  access_policy {
    tenant_id = data.azurerm_client_config.current.tenant_id
    object_id = azurerm_kubernetes_cluster.main.identity[0].principal_id
    
    secret_permissions = [
      "Get",
      "List",
    ]
  }
  
  tags = local.common_tags
}

# Log Analytics Workspace
resource "azurerm_log_analytics_workspace" "main" {
  name                = "${local.project_name}-${local.environment}-logs"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  sku                 = "PerGB2018"
  retention_in_days   = var.log_retention_days
  
  tags = local.common_tags
}

# Application Insights
resource "azurerm_application_insights" "main" {
  name                = "${local.project_name}-${local.environment}-appinsights"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  workspace_id        = azurerm_log_analytics_workspace.main.id
  application_type    = "web"
  
  tags = local.common_tags
}

# Application Gateway Subnet
resource "azurerm_subnet" "appgw" {
  name                 = "appgw-subnet"
  resource_group_name  = azurerm_resource_group.main.name
  virtual_network_name = azurerm_virtual_network.main.name
  address_prefixes     = [var.appgw_subnet_cidr]
}

# Public IP for Application Gateway
resource "azurerm_public_ip" "appgw" {
  name                = "${local.project_name}-${local.environment}-appgw-pip"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  allocation_method   = "Static"
  sku                 = "Standard"
  
  tags = local.common_tags
}

# Data source for current client config
data "azurerm_client_config" "current" {}

# Outputs
output "aks_cluster_name" {
  value = azurerm_kubernetes_cluster.main.name
}

output "aks_cluster_endpoint" {
  value     = azurerm_kubernetes_cluster.main.kube_config.0.host
  sensitive = true
}

output "mysql_server_name" {
  value = azurerm_mysql_flexible_server.main.name
}

output "mysql_fqdn" {
  value = azurerm_mysql_flexible_server.main.fqdn
}

output "redis_hostname" {
  value = azurerm_redis_cache.main.hostname
}

output "redis_ssl_port" {
  value = azurerm_redis_cache.main.ssl_port
}

output "storage_account_name" {
  value = azurerm_storage_account.uploads.name
}

output "key_vault_uri" {
  value = azurerm_key_vault.main.vault_uri
}

output "app_insights_connection_string" {
  value     = azurerm_application_insights.main.connection_string
  sensitive = true
}