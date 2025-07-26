terraform {
  required_version = ">= 1.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
    google = {
      source  = "hashicorp/google"
      version = "~> 4.0"
    }
  }
  
  backend "s3" {
    bucket = "suitecrm-cobol-terraform-state"
    key    = "cloud-burst/terraform.tfstate"
    region = "us-east-1"
    encrypt = true
    dynamodb_table = "terraform-state-lock"
  }
}

# Provider configuration
provider "aws" {
  region = var.aws_region
}

provider "azurerm" {
  features {}
}

provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# Variables
variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"
}

variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

variable "azure_location" {
  description = "Azure location"
  type        = string
  default     = "East US"
}

variable "gcp_project_id" {
  description = "GCP project ID"
  type        = string
}

variable "gcp_region" {
  description = "GCP region"
  type        = string
  default     = "us-central1"
}

variable "mainframe_cidr" {
  description = "CIDR block for mainframe network"
  type        = string
  default     = "10.100.0.0/16"
}

# Common tags
locals {
  common_tags = {
    Application = "SuiteCRM-COBOL-Bridge"
    Component   = "CloudBurst"
    Environment = var.environment
    ManagedBy   = "Terraform"
  }
}

# AWS Resources
module "aws_infrastructure" {
  source = "./modules/aws"
  
  environment    = var.environment
  region         = var.aws_region
  vpc_cidr       = "10.0.0.0/16"
  public_subnets = ["10.0.1.0/24", "10.0.2.0/24"]
  private_subnets = ["10.0.10.0/24", "10.0.11.0/24"]
  
  mainframe_cidr = var.mainframe_cidr
  
  tags = local.common_tags
}

# Azure Resources
module "azure_infrastructure" {
  source = "./modules/azure"
  
  environment        = var.environment
  location           = var.azure_location
  resource_group_name = "rg-cobol-cloudburst-${var.environment}"
  vnet_address_space  = ["10.1.0.0/16"]
  public_subnets      = ["10.1.1.0/24", "10.1.2.0/24"]
  private_subnets     = ["10.1.10.0/24", "10.1.11.0/24"]
  
  mainframe_cidr = var.mainframe_cidr
  
  tags = local.common_tags
}

# GCP Resources
module "gcp_infrastructure" {
  source = "./modules/gcp"
  
  environment     = var.environment
  project_id      = var.gcp_project_id
  region          = var.gcp_region
  network_cidr    = "10.2.0.0/16"
  public_subnets  = ["10.2.1.0/24"]
  private_subnets = ["10.2.10.0/24"]
  
  mainframe_cidr = var.mainframe_cidr
  
  labels = local.common_tags
}

# Redis Cluster for Job Queue
resource "aws_elasticache_replication_group" "job_queue" {
  replication_group_id       = "cobol-cloudburst-${var.environment}"
  replication_group_description = "Redis cluster for CloudBurst job queue"
  
  engine               = "redis"
  engine_version       = "7.0"
  node_type            = "cache.t4g.medium"
  number_cache_clusters = 3
  port                 = 6379
  
  subnet_group_name = module.aws_infrastructure.elasticache_subnet_group_name
  security_group_ids = [module.aws_infrastructure.redis_security_group_id]
  
  at_rest_encryption_enabled = true
  transit_encryption_enabled = true
  
  automatic_failover_enabled = true
  multi_az_enabled           = true
  
  snapshot_retention_limit = 7
  snapshot_window          = "03:00-05:00"
  maintenance_window       = "sun:05:00-sun:07:00"
  
  parameter_group_name = aws_elasticache_parameter_group.redis.name
  
  tags = merge(local.common_tags, {
    Name = "cobol-cloudburst-redis-${var.environment}"
  })
}

resource "aws_elasticache_parameter_group" "redis" {
  name   = "cobol-cloudburst-redis-params"
  family = "redis7"
  
  parameter {
    name  = "maxmemory-policy"
    value = "allkeys-lru"
  }
  
  parameter {
    name  = "timeout"
    value = "300"
  }
}

# S3 Bucket for COBOL Program Storage
resource "aws_s3_bucket" "cobol_programs" {
  bucket = "cobol-cloudburst-programs-${var.environment}"
  
  tags = merge(local.common_tags, {
    Name = "COBOL Program Storage"
  })
}

resource "aws_s3_bucket_versioning" "cobol_programs" {
  bucket = aws_s3_bucket.cobol_programs.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "cobol_programs" {
  bucket = aws_s3_bucket.cobol_programs.id
  
  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "AES256"
    }
  }
}

# IAM Roles and Policies
resource "aws_iam_role" "cloudburst_scheduler" {
  name = "cloudburst-scheduler-${var.environment}"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      }
    ]
  })
  
  tags = local.common_tags
}

resource "aws_iam_role_policy" "cloudburst_scheduler" {
  name = "cloudburst-scheduler-policy"
  role = aws_iam_role.cloudburst_scheduler.id
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "ec2:RunInstances",
          "ec2:TerminateInstances",
          "ec2:DescribeInstances",
          "ec2:DescribeInstanceStatus",
          "ec2:CreateTags",
          "ec2:DescribeTags"
        ]
        Resource = "*"
      },
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:PutObject",
          "s3:ListBucket"
        ]
        Resource = [
          aws_s3_bucket.cobol_programs.arn,
          "${aws_s3_bucket.cobol_programs.arn}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "elasticache:DescribeCacheClusters",
          "elasticache:DescribeReplicationGroups"
        ]
        Resource = "*"
      },
      {
        Effect = "Allow"
        Action = [
          "cloudwatch:PutMetricData",
          "cloudwatch:GetMetricStatistics",
          "cloudwatch:ListMetrics"
        ]
        Resource = "*"
      }
    ]
  })
}

# Worker Instance Launch Template
resource "aws_launch_template" "cobol_worker" {
  name_prefix = "cobol-worker-${var.environment}-"
  
  image_id      = data.aws_ami.cobol_worker.id
  instance_type = "t3.large"
  
  vpc_security_group_ids = [module.aws_infrastructure.worker_security_group_id]
  
  iam_instance_profile {
    name = aws_iam_instance_profile.cobol_worker.name
  }
  
  user_data = base64encode(templatefile("${path.module}/scripts/worker-init.sh", {
    environment         = var.environment
    mainframe_host      = var.mainframe_host
    redis_endpoint      = aws_elasticache_replication_group.job_queue.configuration_endpoint_address
    s3_bucket           = aws_s3_bucket.cobol_programs.id
  }))
  
  block_device_mappings {
    device_name = "/dev/xvda"
    
    ebs {
      volume_size           = 100
      volume_type           = "gp3"
      encrypted             = true
      delete_on_termination = true
    }
  }
  
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }
  
  tag_specifications {
    resource_type = "instance"
    
    tags = merge(local.common_tags, {
      Name = "cobol-worker-${var.environment}"
      Type = "worker"
    })
  }
}

resource "aws_iam_instance_profile" "cobol_worker" {
  name = "cobol-worker-profile-${var.environment}"
  role = aws_iam_role.cobol_worker.name
}

resource "aws_iam_role" "cobol_worker" {
  name = "cobol-worker-${var.environment}"
  
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      }
    ]
  })
  
  tags = local.common_tags
}

resource "aws_iam_role_policy" "cobol_worker" {
  name = "cobol-worker-policy"
  role = aws_iam_role.cobol_worker.id
  
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:ListBucket"
        ]
        Resource = [
          aws_s3_bucket.cobol_programs.arn,
          "${aws_s3_bucket.cobol_programs.arn}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "cloudwatch:PutMetricData"
        ]
        Resource = "*"
      }
    ]
  })
}

# Data source for COBOL worker AMI
data "aws_ami" "cobol_worker" {
  most_recent = true
  owners      = ["self"]
  
  filter {
    name   = "name"
    values = ["cobol-worker-*"]
  }
  
  filter {
    name   = "state"
    values = ["available"]
  }
}

# Outputs
output "aws_vpc_id" {
  value = module.aws_infrastructure.vpc_id
}

output "azure_vnet_id" {
  value = module.azure_infrastructure.vnet_id
}

output "gcp_network_id" {
  value = module.gcp_infrastructure.network_id
}

output "redis_endpoint" {
  value = aws_elasticache_replication_group.job_queue.configuration_endpoint_address
}

output "s3_bucket" {
  value = aws_s3_bucket.cobol_programs.id
}

output "launch_template_id" {
  value = aws_launch_template.cobol_worker.id
}