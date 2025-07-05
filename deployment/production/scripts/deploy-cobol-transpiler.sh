#!/bin/bash

# =============================================================================
# COBOL Transpiler Production Deployment Script
# LegacyBaaS Platform - Complete Production Deployment
# =============================================================================

set -euo pipefail

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
DEPLOYMENT_DIR="${PROJECT_ROOT}/deployment/production"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="/tmp/cobol-transpiler-deployment-${TIMESTAMP}.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "${LOG_FILE}"
}

warn() {
    echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')] WARNING:${NC} $1" | tee -a "${LOG_FILE}"
}

error() {
    echo -e "${RED}[$(date +'%Y-%m-%d %H:%M:%S')] ERROR:${NC} $1" | tee -a "${LOG_FILE}"
    exit 1
}

info() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')] INFO:${NC} $1" | tee -a "${LOG_FILE}"
}

# Environment configuration
ENVIRONMENT=${ENVIRONMENT:-production}
NAMESPACE=${NAMESPACE:-legacybaas-production}
DOCKER_REGISTRY=${DOCKER_REGISTRY:-legacybaas}
IMAGE_TAG=${IMAGE_TAG:-1.0.0}
KUBECONFIG=${KUBECONFIG:-~/.kube/config}

# Database configuration
DB_HOST=${DB_HOST:-postgres-cluster.legacybaas-production.svc.cluster.local}
DB_PORT=${DB_PORT:-5432}
DB_NAME=${DB_NAME:-legacybaas_cobol}
DB_USER=${DB_USER:-legacybaas_app}
DB_PASSWORD=${DB_PASSWORD:-}

# Redis configuration
REDIS_HOST=${REDIS_HOST:-redis-cluster.legacybaas-production.svc.cluster.local}
REDIS_PORT=${REDIS_PORT:-6379}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

check_prerequisites() {
    log "Checking deployment prerequisites..."
    
    # Check required tools
    local required_tools=("kubectl" "docker" "helm" "psql" "redis-cli")
    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            error "Required tool '$tool' is not installed"
        fi
    done
    
    # Check environment variables
    if [[ -z "$DB_PASSWORD" ]]; then
        error "DB_PASSWORD environment variable is required"
    fi
    
    # Check kubeconfig
    if [[ ! -f "$KUBECONFIG" ]]; then
        error "Kubeconfig file not found: $KUBECONFIG"
    fi
    
    # Test kubectl connectivity
    if ! kubectl cluster-info &>/dev/null; then
        error "Cannot connect to Kubernetes cluster"
    fi
    
    log "Prerequisites check completed successfully"
}

check_existing_deployment() {
    log "Checking for existing COBOL transpiler deployment..."
    
    if kubectl get namespace "$NAMESPACE" &>/dev/null; then
        warn "Namespace '$NAMESPACE' already exists"
        
        if kubectl get deployment cobol-transpiler -n "$NAMESPACE" &>/dev/null; then
            warn "COBOL transpiler deployment already exists"
            read -p "Do you want to update the existing deployment? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                error "Deployment cancelled by user"
            fi
        fi
    fi
}

# =============================================================================
# DATABASE SETUP FUNCTIONS
# =============================================================================

setup_database() {
    log "Setting up COBOL transpiler database..."
    
    # Test database connectivity
    if ! PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d postgres -c "SELECT 1;" &>/dev/null; then
        error "Cannot connect to database server"
    fi
    
    # Create database if it doesn't exist
    PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d postgres -c "
        SELECT 'CREATE DATABASE $DB_NAME' 
        WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '$DB_NAME')\\gexec
    " || error "Failed to create database"
    
    # Run migrations
    log "Running database migrations..."
    PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -f "${DEPLOYMENT_DIR}/cobol-transpiler/database-migrations.sql" || error "Failed to run database migrations"
    
    log "Database setup completed successfully"
}

validate_database() {
    log "Validating database setup..."
    
    # Check if all required tables exist
    local tables=("transpilation_projects" "transpilation_usage" "cobol_templates" "banking_configurations" "transpilation_jobs" "transpilation_audit_logs" "transpilation_performance_metrics" "transpilation_quotas" "blockchain_deployments")
    
    for table in "${tables[@]}"; do
        if ! PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c "SELECT 1 FROM $table LIMIT 1;" &>/dev/null; then
            error "Table '$table' not found or not accessible"
        fi
    done
    
    # Check if default data exists
    local template_count=$(PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -t -c "SELECT COUNT(*) FROM cobol_templates WHERE is_public = true;")
    if [[ $template_count -lt 4 ]]; then
        error "Default templates not found in database"
    fi
    
    log "Database validation completed successfully"
}

# =============================================================================
# DOCKER IMAGE FUNCTIONS
# =============================================================================

build_docker_image() {
    log "Building COBOL transpiler Docker image..."
    
    cd "$PROJECT_ROOT"
    
    # Create optimized Dockerfile for production
    cat > Dockerfile.cobol-transpiler << 'EOF'
# Multi-stage build for COBOL Transpiler
FROM node:18-alpine AS builder

WORKDIR /app

# Copy package files
COPY package*.json ./
COPY partner-portal/package*.json ./partner-portal/

# Install dependencies
RUN npm ci --only=production && \
    cd partner-portal && \
    npm ci --only=production

# Copy source code
COPY src/ ./src/
COPY partner-portal/app/ ./partner-portal/app/
COPY partner-portal/components/ ./partner-portal/components/
COPY partner-portal/lib/ ./partner-portal/lib/

# Build the application
RUN npm run build

# Production stage
FROM node:18-alpine AS production

# Install system dependencies
RUN apk add --no-cache \
    dumb-init \
    curl \
    postgresql-client \
    redis

# Create app user
RUN addgroup -g 1000 app && \
    adduser -D -s /bin/sh -u 1000 -G app app

WORKDIR /app

# Copy built application
COPY --from=builder --chown=app:app /app/node_modules ./node_modules
COPY --from=builder --chown=app:app /app/src ./src
COPY --from=builder --chown=app:app /app/package*.json ./

# Copy configuration files
COPY --chown=app:app src/adapters/configs/ ./src/adapters/configs/
COPY --chown=app:app src/adapters/templates/ ./src/adapters/templates/

# Create necessary directories
RUN mkdir -p /app/logs /app/temp /app/uploads && \
    chown -R app:app /app

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health/liveness || exit 1

USER app

EXPOSE 3000 8080 9090

ENTRYPOINT ["dumb-init", "--"]
CMD ["node", "src/index.js"]
EOF

    # Build the image
    docker build -f Dockerfile.cobol-transpiler -t "${DOCKER_REGISTRY}/cobol-transpiler:${IMAGE_TAG}" . || error "Failed to build Docker image"
    
    # Tag as latest
    docker tag "${DOCKER_REGISTRY}/cobol-transpiler:${IMAGE_TAG}" "${DOCKER_REGISTRY}/cobol-transpiler:latest"
    
    log "Docker image built successfully"
}

push_docker_image() {
    log "Pushing Docker image to registry..."
    
    docker push "${DOCKER_REGISTRY}/cobol-transpiler:${IMAGE_TAG}" || error "Failed to push Docker image with tag"
    docker push "${DOCKER_REGISTRY}/cobol-transpiler:latest" || error "Failed to push Docker image latest"
    
    log "Docker image pushed successfully"
}

# =============================================================================
# KUBERNETES DEPLOYMENT FUNCTIONS
# =============================================================================

deploy_to_kubernetes() {
    log "Deploying COBOL transpiler to Kubernetes..."
    
    # Create namespace if it doesn't exist
    kubectl create namespace "$NAMESPACE" --dry-run=client -o yaml | kubectl apply -f -
    
    # Apply ConfigMaps and Secrets first
    apply_configurations
    
    # Apply the main deployment
    envsubst < "${DEPLOYMENT_DIR}/cobol-transpiler/kubernetes-deployment.yaml" | kubectl apply -f - || error "Failed to apply Kubernetes deployment"
    
    log "Kubernetes deployment applied successfully"
}

apply_configurations() {
    log "Applying Kubernetes configurations..."
    
    # Create ConfigMap for environment variables
    kubectl create configmap cobol-transpiler-config \
        --namespace="$NAMESPACE" \
        --from-literal=NODE_ENV="$ENVIRONMENT" \
        --from-literal=DATABASE_HOST="$DB_HOST" \
        --from-literal=DATABASE_PORT="$DB_PORT" \
        --from-literal=DATABASE_NAME="$DB_NAME" \
        --from-literal=REDIS_HOST="$REDIS_HOST" \
        --from-literal=REDIS_PORT="$REDIS_PORT" \
        --dry-run=client -o yaml | kubectl apply -f -
    
    # Create Secret for sensitive data
    kubectl create secret generic cobol-transpiler-secrets \
        --namespace="$NAMESPACE" \
        --from-literal=DATABASE_PASSWORD="$DB_PASSWORD" \
        --dry-run=client -o yaml | kubectl apply -f -
    
    log "Configurations applied successfully"
}

wait_for_deployment() {
    log "Waiting for deployment to be ready..."
    
    # Wait for deployment to be available
    kubectl wait --for=condition=available deployment/cobol-transpiler \
        --namespace="$NAMESPACE" \
        --timeout=600s || error "Deployment failed to become available"
    
    # Wait for all pods to be ready
    kubectl wait --for=condition=ready pod \
        --selector=app=cobol-transpiler \
        --namespace="$NAMESPACE" \
        --timeout=300s || error "Pods failed to become ready"
    
    log "Deployment is ready"
}

# =============================================================================
# TESTING FUNCTIONS
# =============================================================================

run_health_checks() {
    log "Running health checks..."
    
    # Get service endpoint
    local service_ip=$(kubectl get service cobol-transpiler-service \
        --namespace="$NAMESPACE" \
        --output=jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")
    
    if [[ -z "$service_ip" ]]; then
        # Try hostname for AWS ELB
        service_ip=$(kubectl get service cobol-transpiler-service \
            --namespace="$NAMESPACE" \
            --output=jsonpath='{.status.loadBalancer.ingress[0].hostname}' 2>/dev/null || echo "")
    fi
    
    if [[ -z "$service_ip" ]]; then
        warn "LoadBalancer IP not available, using port-forward for health checks"
        kubectl port-forward service/cobol-transpiler-service 8080:80 --namespace="$NAMESPACE" &
        local port_forward_pid=$!
        sleep 5
        service_ip="localhost:8080"
    fi
    
    # Health check endpoints
    local endpoints=("/health/liveness" "/health/readiness" "/health/startup")
    
    for endpoint in "${endpoints[@]}"; do
        info "Testing endpoint: $endpoint"
        if curl -f "http://${service_ip}${endpoint}" &>/dev/null; then
            log "✓ Health check passed: $endpoint"
        else
            error "✗ Health check failed: $endpoint"
        fi
    done
    
    # Clean up port-forward if used
    if [[ -n "${port_forward_pid:-}" ]]; then
        kill $port_forward_pid 2>/dev/null || true
    fi
    
    log "Health checks completed successfully"
}

run_integration_tests() {
    log "Running integration tests..."
    
    # Run basic API tests
    cd "$PROJECT_ROOT"
    
    # Install test dependencies if needed
    if [[ ! -d "node_modules" ]]; then
        npm install
    fi
    
    # Run COBOL transpiler specific tests
    npm test -- --testPathPattern="cobol.*test" --testTimeout=60000 || error "Integration tests failed"
    
    log "Integration tests completed successfully"
}

# =============================================================================
# MONITORING SETUP FUNCTIONS
# =============================================================================

setup_monitoring() {
    log "Setting up monitoring for COBOL transpiler..."
    
    # Apply ServiceMonitor for Prometheus
    cat << EOF | kubectl apply -f -
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: cobol-transpiler-monitor
  namespace: $NAMESPACE
  labels:
    app: cobol-transpiler
spec:
  selector:
    matchLabels:
      app: cobol-transpiler
  endpoints:
  - port: metrics
    path: /metrics
    interval: 30s
EOF

    # Create Grafana dashboard ConfigMap
    kubectl create configmap cobol-transpiler-grafana-dashboard \
        --namespace="$NAMESPACE" \
        --from-file="${DEPLOYMENT_DIR}/monitoring/grafana-dashboard.json" \
        --dry-run=client -o yaml | kubectl apply -f - || warn "Grafana dashboard creation failed"
    
    log "Monitoring setup completed"
}

setup_alerts() {
    log "Setting up alerts for COBOL transpiler..."
    
    # Apply PrometheusRule for alerting
    cat << EOF | kubectl apply -f -
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: cobol-transpiler-alerts
  namespace: $NAMESPACE
  labels:
    app: cobol-transpiler
spec:
  groups:
  - name: cobol-transpiler
    rules:
    - alert: CobolTranspilerDown
      expr: up{job="cobol-transpiler"} == 0
      for: 1m
      labels:
        severity: critical
      annotations:
        summary: "COBOL Transpiler service is down"
        description: "COBOL Transpiler has been down for more than 1 minute"
    
    - alert: CobolTranspilerHighErrorRate
      expr: rate(cobol_transpiler_errors_total[5m]) > 0.1
      for: 2m
      labels:
        severity: warning
      annotations:
        summary: "High error rate in COBOL Transpiler"
        description: "Error rate is {{ \$value }} errors per second"
    
    - alert: CobolTranspilerHighLatency
      expr: histogram_quantile(0.95, rate(cobol_transpiler_request_duration_seconds_bucket[5m])) > 30
      for: 5m
      labels:
        severity: warning
      annotations:
        summary: "High latency in COBOL Transpiler"
        description: "95th percentile latency is {{ \$value }} seconds"
EOF

    log "Alerts setup completed"
}

# =============================================================================
# BACKUP AND ROLLBACK FUNCTIONS
# =============================================================================

create_backup() {
    log "Creating backup before deployment..."
    
    local backup_dir="/tmp/cobol-transpiler-backup-${TIMESTAMP}"
    mkdir -p "$backup_dir"
    
    # Backup existing deployment if it exists
    if kubectl get deployment cobol-transpiler -n "$NAMESPACE" &>/dev/null; then
        kubectl get deployment cobol-transpiler -n "$NAMESPACE" -o yaml > "${backup_dir}/deployment.yaml"
        kubectl get configmap cobol-transpiler-config -n "$NAMESPACE" -o yaml > "${backup_dir}/configmap.yaml" 2>/dev/null || true
        kubectl get secret cobol-transpiler-secrets -n "$NAMESPACE" -o yaml > "${backup_dir}/secret.yaml" 2>/dev/null || true
    fi
    
    # Backup database
    PGPASSWORD="$DB_PASSWORD" pg_dump -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" > "${backup_dir}/database_backup.sql" || warn "Database backup failed"
    
    echo "$backup_dir" > /tmp/cobol-transpiler-last-backup
    log "Backup created at: $backup_dir"
}

rollback_deployment() {
    log "Rolling back deployment..."
    
    local backup_dir
    if [[ -f /tmp/cobol-transpiler-last-backup ]]; then
        backup_dir=$(cat /tmp/cobol-transpiler-last-backup)
    else
        error "No backup found for rollback"
    fi
    
    if [[ -d "$backup_dir" ]]; then
        # Restore Kubernetes resources
        kubectl apply -f "${backup_dir}/deployment.yaml" 2>/dev/null || warn "Failed to restore deployment"
        kubectl apply -f "${backup_dir}/configmap.yaml" 2>/dev/null || warn "Failed to restore configmap"
        kubectl apply -f "${backup_dir}/secret.yaml" 2>/dev/null || warn "Failed to restore secret"
        
        # Wait for rollback to complete
        kubectl rollout status deployment/cobol-transpiler -n "$NAMESPACE" --timeout=300s || error "Rollback failed"
        
        log "Rollback completed successfully"
    else
        error "Backup directory not found: $backup_dir"
    fi
}

# =============================================================================
# CLEANUP FUNCTION
# =============================================================================

cleanup() {
    log "Cleaning up temporary files..."
    
    # Remove temporary Dockerfile
    rm -f "${PROJECT_ROOT}/Dockerfile.cobol-transpiler"
    
    # Clean up any temporary files
    find /tmp -name "cobol-transpiler-*" -type f -mtime +7 -delete 2>/dev/null || true
    
    log "Cleanup completed"
}

# =============================================================================
# MAIN DEPLOYMENT FUNCTION
# =============================================================================

main() {
    log "Starting COBOL Transpiler production deployment"
    log "Environment: $ENVIRONMENT"
    log "Namespace: $NAMESPACE"
    log "Image: ${DOCKER_REGISTRY}/cobol-transpiler:${IMAGE_TAG}"
    log "Log file: $LOG_FILE"
    
    # Trap for cleanup on exit
    trap cleanup EXIT
    
    # Check prerequisites
    check_prerequisites
    
    # Check existing deployment
    check_existing_deployment
    
    # Create backup
    create_backup
    
    # Setup database
    setup_database
    validate_database
    
    # Build and push Docker image
    build_docker_image
    push_docker_image
    
    # Deploy to Kubernetes
    deploy_to_kubernetes
    wait_for_deployment
    
    # Setup monitoring and alerts
    setup_monitoring
    setup_alerts
    
    # Run tests
    run_health_checks
    run_integration_tests
    
    log "🎉 COBOL Transpiler deployment completed successfully!"
    log "📊 Deployment summary:"
    log "   - Namespace: $NAMESPACE"
    log "   - Image: ${DOCKER_REGISTRY}/cobol-transpiler:${IMAGE_TAG}"
    log "   - Replicas: $(kubectl get deployment cobol-transpiler -n "$NAMESPACE" -o jsonpath='{.status.readyReplicas}')"
    log "   - Service: $(kubectl get service cobol-transpiler-service -n "$NAMESPACE" -o jsonpath='{.status.loadBalancer.ingress[0].ip}')"
    log "   - Log file: $LOG_FILE"
    
    # Display next steps
    log "📋 Next steps:"
    log "   1. Monitor deployment: kubectl get pods -n $NAMESPACE -w"
    log "   2. View logs: kubectl logs -f deployment/cobol-transpiler -n $NAMESPACE"
    log "   3. Access service: kubectl port-forward service/cobol-transpiler-service 3000:80 -n $NAMESPACE"
    log "   4. Run tests: npm test -- --testPathPattern='cobol.*test'"
}

# =============================================================================
# SCRIPT ENTRY POINT
# =============================================================================

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --environment)
            ENVIRONMENT="$2"
            shift 2
            ;;
        --namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        --image-tag)
            IMAGE_TAG="$2"
            shift 2
            ;;
        --rollback)
            rollback_deployment
            exit 0
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --environment ENV    Deployment environment (default: production)"
            echo "  --namespace NS       Kubernetes namespace (default: legacybaas-production)"
            echo "  --image-tag TAG      Docker image tag (default: 1.0.0)"
            echo "  --rollback           Rollback to previous deployment"
            echo "  --help               Show this help message"
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            ;;
    esac
done

# Run main deployment
main