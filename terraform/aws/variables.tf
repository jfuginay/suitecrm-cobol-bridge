variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

variable "environment" {
  description = "Environment name"
  type        = string
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "Availability zones for the VPC"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

variable "private_subnet_cidrs" {
  description = "CIDR blocks for private subnets"
  type        = list(string)
  default     = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
}

variable "public_subnet_cidrs" {
  description = "CIDR blocks for public subnets"
  type        = list(string)
  default     = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
}

variable "enable_vpn" {
  description = "Enable VPN gateway"
  type        = bool
  default     = false
}

variable "kubernetes_version" {
  description = "Kubernetes version for EKS"
  type        = string
  default     = "1.28"
}

variable "allowed_cidr_blocks" {
  description = "CIDR blocks allowed to access the cluster"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

variable "node_instance_types" {
  description = "EC2 instance types for general node group"
  type        = list(string)
  default     = ["m6i.large", "m6i.xlarge"]
}

variable "node_group_min_size" {
  description = "Minimum size of node group"
  type        = number
  default     = 2
}

variable "node_group_max_size" {
  description = "Maximum size of node group"
  type        = number
  default     = 10
}

variable "node_group_desired_size" {
  description = "Desired size of node group"
  type        = number
  default     = 3
}

variable "compute_instance_types" {
  description = "EC2 instance types for compute node group"
  type        = list(string)
  default     = ["c6i.xlarge", "c6i.2xlarge"]
}

variable "compute_group_min_size" {
  description = "Minimum size of compute node group"
  type        = number
  default     = 1
}

variable "compute_group_max_size" {
  description = "Maximum size of compute node group"
  type        = number
  default     = 5
}

variable "compute_group_desired_size" {
  description = "Desired size of compute node group"
  type        = number
  default     = 2
}

variable "mysql_version" {
  description = "MySQL version"
  type        = string
  default     = "8.0.35"
}

variable "mysql_family" {
  description = "MySQL parameter group family"
  type        = string
  default     = "mysql8.0"
}

variable "mysql_major_version" {
  description = "MySQL major version"
  type        = string
  default     = "8.0"
}

variable "db_instance_class" {
  description = "RDS instance class"
  type        = string
  default     = "db.r6i.large"
}

variable "db_allocated_storage" {
  description = "Allocated storage for RDS in GB"
  type        = number
  default     = 100
}

variable "db_max_allocated_storage" {
  description = "Maximum allocated storage for RDS in GB"
  type        = number
  default     = 500
}

variable "redis_version" {
  description = "Redis version"
  type        = string
  default     = "7.0"
}

variable "redis_node_type" {
  description = "ElastiCache node type"
  type        = string
  default     = "cache.r7g.large"
}

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}