terraform {
  required_version = ">= 1.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
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
  
  backend "s3" {
    bucket         = "suitecrm-cobol-terraform-state"
    key            = "aws/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "terraform-state-lock"
  }
}

provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Project     = "SuiteCRM-COBOL-Bridge"
      Environment = var.environment
      ManagedBy   = "Terraform"
    }
  }
}

locals {
  cluster_name = "suitecrm-cobol-${var.environment}"
  common_tags = {
    Project     = "SuiteCRM-COBOL-Bridge"
    Environment = var.environment
    Region      = var.aws_region
  }
}

# VPC and Networking
module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  version = "~> 5.0"
  
  name = "${local.cluster_name}-vpc"
  cidr = var.vpc_cidr
  
  azs             = var.availability_zones
  private_subnets = var.private_subnet_cidrs
  public_subnets  = var.public_subnet_cidrs
  
  enable_nat_gateway   = true
  single_nat_gateway   = var.environment == "dev" ? true : false
  enable_dns_hostnames = true
  enable_dns_support   = true
  
  enable_vpn_gateway = var.enable_vpn
  
  tags = merge(local.common_tags, {
    "kubernetes.io/cluster/${local.cluster_name}" = "shared"
  })
  
  public_subnet_tags = {
    "kubernetes.io/cluster/${local.cluster_name}" = "shared"
    "kubernetes.io/role/elb"                      = "1"
  }
  
  private_subnet_tags = {
    "kubernetes.io/cluster/${local.cluster_name}" = "shared"
    "kubernetes.io/role/internal-elb"             = "1"
  }
}

# EKS Cluster
module "eks" {
  source = "terraform-aws-modules/eks/aws"
  version = "~> 19.0"
  
  cluster_name    = local.cluster_name
  cluster_version = var.kubernetes_version
  
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets
  
  cluster_endpoint_private_access = true
  cluster_endpoint_public_access  = true
  cluster_endpoint_public_access_cidrs = var.allowed_cidr_blocks
  
  enable_irsa = true
  
  cluster_addons = {
    coredns = {
      most_recent = true
    }
    kube-proxy = {
      most_recent = true
    }
    vpc-cni = {
      most_recent = true
    }
    aws-ebs-csi-driver = {
      most_recent = true
    }
  }
  
  eks_managed_node_groups = {
    general = {
      name             = "${local.cluster_name}-general"
      instance_types   = var.node_instance_types
      min_size         = var.node_group_min_size
      max_size         = var.node_group_max_size
      desired_size     = var.node_group_desired_size
      
      disk_size = 100
      disk_type = "gp3"
      
      labels = {
        role = "general"
      }
      
      taints = []
      
      update_config = {
        max_unavailable_percentage = 50
      }
    }
    
    compute = {
      name             = "${local.cluster_name}-compute"
      instance_types   = var.compute_instance_types
      min_size         = var.compute_group_min_size
      max_size         = var.compute_group_max_size
      desired_size     = var.compute_group_desired_size
      
      disk_size = 200
      disk_type = "gp3"
      
      labels = {
        role = "compute"
        workload = "cobol"
      }
      
      taints = [
        {
          key    = "workload"
          value  = "cobol"
          effect = "NoSchedule"
        }
      ]
    }
  }
  
  tags = local.common_tags
}

# RDS MySQL Database
module "rds" {
  source = "terraform-aws-modules/rds/aws"
  version = "~> 6.0"
  
  identifier = "${local.cluster_name}-mysql"
  
  engine               = "mysql"
  engine_version       = var.mysql_version
  family               = var.mysql_family
  major_engine_version = var.mysql_major_version
  instance_class       = var.db_instance_class
  
  allocated_storage     = var.db_allocated_storage
  max_allocated_storage = var.db_max_allocated_storage
  storage_encrypted     = true
  storage_type          = "gp3"
  
  db_name  = "suitecrm"
  username = "suitecrm"
  port     = 3306
  
  multi_az               = var.environment == "prod" ? true : false
  db_subnet_group_name   = module.vpc.database_subnet_group_name
  vpc_security_group_ids = [module.rds_security_group.security_group_id]
  
  backup_retention_period = var.environment == "prod" ? 30 : 7
  backup_window          = "03:00-04:00"
  maintenance_window     = "sun:04:00-sun:05:00"
  
  enabled_cloudwatch_logs_exports = ["error", "general", "slowquery"]
  
  create_db_option_group    = true
  create_db_parameter_group = true
  
  parameters = [
    {
      name  = "character_set_server"
      value = "utf8mb4"
    },
    {
      name  = "collation_server"
      value = "utf8mb4_unicode_ci"
    },
    {
      name  = "max_connections"
      value = "500"
    },
    {
      name  = "innodb_buffer_pool_size"
      value = "{DBInstanceClassMemory*3/4}"
    }
  ]
  
  tags = local.common_tags
}

# ElastiCache Redis
module "elasticache" {
  source = "terraform-aws-modules/elasticache/aws"
  version = "~> 1.0"
  
  create_cluster_mode_enabled = false
  create_replication_group    = true
  
  replication_group_id = "${local.cluster_name}-redis"
  description          = "Redis cluster for SuiteCRM COBOL Bridge"
  
  engine_version       = var.redis_version
  node_type            = var.redis_node_type
  num_cache_clusters   = var.environment == "prod" ? 3 : 1
  
  port = 6379
  
  subnet_ids = module.vpc.private_subnets
  security_group_ids = [module.redis_security_group.security_group_id]
  
  at_rest_encryption_enabled = true
  transit_encryption_enabled = true
  auth_token_enabled         = true
  
  automatic_failover_enabled = var.environment == "prod" ? true : false
  multi_az_enabled          = var.environment == "prod" ? true : false
  
  apply_immediately          = var.environment == "dev" ? true : false
  auto_minor_version_upgrade = true
  
  snapshot_retention_limit = var.environment == "prod" ? 7 : 1
  snapshot_window         = "03:00-05:00"
  
  log_delivery_configuration = [
    {
      destination      = aws_cloudwatch_log_group.redis.name
      destination_type = "cloudwatch-logs"
      log_format       = "json"
      log_type         = "slow-log"
    }
  ]
  
  tags = local.common_tags
}

# S3 Buckets
resource "aws_s3_bucket" "uploads" {
  bucket = "${local.cluster_name}-uploads"
  
  tags = local.common_tags
}

resource "aws_s3_bucket_versioning" "uploads" {
  bucket = aws_s3_bucket.uploads.id
  
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "uploads" {
  bucket = aws_s3_bucket.uploads.id
  
  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "AES256"
    }
  }
}

resource "aws_s3_bucket" "backups" {
  bucket = "${local.cluster_name}-backups"
  
  tags = local.common_tags
}

resource "aws_s3_bucket_lifecycle_configuration" "backups" {
  bucket = aws_s3_bucket.backups.id
  
  rule {
    id     = "archive-old-backups"
    status = "Enabled"
    
    transition {
      days          = 30
      storage_class = "GLACIER"
    }
    
    expiration {
      days = 365
    }
  }
}

# Security Groups
module "rds_security_group" {
  source = "terraform-aws-modules/security-group/aws"
  version = "~> 5.0"
  
  name        = "${local.cluster_name}-rds-sg"
  description = "Security group for RDS MySQL"
  vpc_id      = module.vpc.vpc_id
  
  ingress_with_source_security_group_id = [
    {
      from_port                = 3306
      to_port                  = 3306
      protocol                 = "tcp"
      source_security_group_id = module.eks.node_security_group_id
    }
  ]
  
  egress_rules = ["all-all"]
  
  tags = local.common_tags
}

module "redis_security_group" {
  source = "terraform-aws-modules/security-group/aws"
  version = "~> 5.0"
  
  name        = "${local.cluster_name}-redis-sg"
  description = "Security group for ElastiCache Redis"
  vpc_id      = module.vpc.vpc_id
  
  ingress_with_source_security_group_id = [
    {
      from_port                = 6379
      to_port                  = 6379
      protocol                 = "tcp"
      source_security_group_id = module.eks.node_security_group_id
    }
  ]
  
  egress_rules = ["all-all"]
  
  tags = local.common_tags
}

# CloudWatch Log Groups
resource "aws_cloudwatch_log_group" "eks" {
  name              = "/aws/eks/${local.cluster_name}/cluster"
  retention_in_days = var.log_retention_days
  
  tags = local.common_tags
}

resource "aws_cloudwatch_log_group" "redis" {
  name              = "/aws/elasticache/${local.cluster_name}-redis"
  retention_in_days = var.log_retention_days
  
  tags = local.common_tags
}

# Application Load Balancer
module "alb" {
  source = "terraform-aws-modules/alb/aws"
  version = "~> 8.0"
  
  name = "${local.cluster_name}-alb"
  
  load_balancer_type = "application"
  
  vpc_id  = module.vpc.vpc_id
  subnets = module.vpc.public_subnets
  
  security_groups = [module.alb_security_group.security_group_id]
  
  enable_deletion_protection = var.environment == "prod" ? true : false
  enable_http2              = true
  enable_cross_zone_load_balancing = true
  
  access_logs = {
    enabled = true
    bucket  = aws_s3_bucket.alb_logs.id
    prefix  = "alb"
  }
  
  tags = local.common_tags
}

module "alb_security_group" {
  source = "terraform-aws-modules/security-group/aws"
  version = "~> 5.0"
  
  name        = "${local.cluster_name}-alb-sg"
  description = "Security group for Application Load Balancer"
  vpc_id      = module.vpc.vpc_id
  
  ingress_rules = ["http-80-tcp", "https-443-tcp"]
  ingress_cidr_blocks = var.allowed_cidr_blocks
  
  egress_rules = ["all-all"]
  
  tags = local.common_tags
}

resource "aws_s3_bucket" "alb_logs" {
  bucket = "${local.cluster_name}-alb-logs"
  
  tags = local.common_tags
}

# WAF
resource "aws_wafv2_web_acl" "main" {
  name  = "${local.cluster_name}-waf"
  scope = "REGIONAL"
  
  default_action {
    allow {}
  }
  
  rule {
    name     = "RateLimitRule"
    priority = 1
    
    action {
      block {}
    }
    
    statement {
      rate_based_statement {
        limit              = 2000
        aggregate_key_type = "IP"
      }
    }
    
    visibility_config {
      cloudwatch_metrics_enabled = true
      metric_name                = "RateLimitRule"
      sampled_requests_enabled   = true
    }
  }
  
  rule {
    name     = "AWSManagedRulesCommonRuleSet"
    priority = 2
    
    override_action {
      none {}
    }
    
    statement {
      managed_rule_group_statement {
        name        = "AWSManagedRulesCommonRuleSet"
        vendor_name = "AWS"
      }
    }
    
    visibility_config {
      cloudwatch_metrics_enabled = true
      metric_name                = "CommonRuleSetMetric"
      sampled_requests_enabled   = true
    }
  }
  
  tags = local.common_tags
  
  visibility_config {
    cloudwatch_metrics_enabled = true
    metric_name                = "${local.cluster_name}-waf"
    sampled_requests_enabled   = true
  }
}

# Outputs
output "cluster_endpoint" {
  description = "Endpoint for EKS control plane"
  value       = module.eks.cluster_endpoint
}

output "cluster_security_group_id" {
  description = "Security group ID attached to the EKS cluster"
  value       = module.eks.cluster_security_group_id
}

output "rds_endpoint" {
  description = "RDS instance endpoint"
  value       = module.rds.db_instance_endpoint
  sensitive   = true
}

output "redis_endpoint" {
  description = "Redis cluster endpoint"
  value       = module.elasticache.primary_endpoint_address
  sensitive   = true
}

output "alb_dns_name" {
  description = "DNS name of the load balancer"
  value       = module.alb.lb_dns_name
}