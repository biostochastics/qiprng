# Mintlify Documentation Deployment Guide

This guide explains how to deploy the qiprng documentation to qiprng.biostochastics.com using Mintlify.

## Prerequisites

1. **Mintlify Account**: Sign up at [mintlify.com](https://mintlify.com)
2. **GitHub Repository**: Access to biostochastics/qiprng
3. **Domain Control**: Access to DNS settings for biostochastics.com
4. **API Keys**: Mintlify API key for deployment

## Initial Setup

### 1. Mintlify Project Creation

```bash
# Install Mintlify CLI globally
npm install -g mintlify

# Initialize Mintlify in the project
cd /Users/biostochastics/Development/GitHub/qiprng/mintlify
mintlify init

# Validate configuration
mintlify validate
```

### 2. Configure GitHub Secrets

Add these secrets to your GitHub repository:

1. Go to Settings → Secrets and variables → Actions
2. Add the following secrets:

```
MINTLIFY_API_KEY=<your-mintlify-api-key>
```

To get your Mintlify API key:

- Log in to Mintlify dashboard
- Go to Settings → API Keys
- Generate a new deployment key

### 3. DNS Configuration

Configure DNS for qiprng.biostochastics.com:

#### Option A: CNAME Record (Recommended)

```
Type: CNAME
Name: qiprng
Value: cname.vercel-dns.com
TTL: 3600
```

#### Option B: A Records

```
Type: A
Name: qiprng
Value: 76.76.21.21
TTL: 3600
```

### 4. Mintlify Dashboard Configuration

1. Log in to [Mintlify Dashboard](https://dashboard.mintlify.com)
2. Create new project or select existing
3. Configure custom domain:
   - Go to Settings → Domain
   - Add custom domain: `qiprng.biostochastics.com`
   - Wait for SSL certificate provisioning (5-10 minutes)

## Deployment Process

### Automatic Deployment (GitHub Actions)

The documentation automatically deploys when:

- Changes are pushed to the `main` branch
- Any of these paths are modified:
  - `R/**`
  - `man/**`
  - `src/**`
  - `mintlify/**`
  - `README.md`
  - `CHANGELOG.md`
  - `DESCRIPTION`

### Manual Deployment

#### Local Deployment

```bash
cd mintlify

# Build documentation locally
mintlify build

# Test locally
mintlify dev

# Deploy to production
MINTLIFY_API_KEY=<your-key> mintlify deploy --prod
```

#### Trigger GitHub Action

```bash
# Trigger workflow manually
gh workflow run mintlify-deploy.yml \
  --ref main \
  -f full_rebuild=true
```

## Documentation Generation Pipeline

### Stage 1: R Documentation

```bash
# Generate roxygen2 documentation
Rscript -e "roxygen2::roxygenize()"

# Convert to MDX
Rscript scripts/roxygen2_to_mintlify.R man mintlify/api-reference/r-functions
```

### Stage 2: C++ Documentation

```bash
# Extract and convert C++ docs
python scripts/cpp_to_mintlify.py \
  --src src \
  --output mintlify/api-reference/cpp-classes
```

### Stage 3: Static Content

```bash
# Convert README to introduction
python scripts/convert_static.py

# Update version in mint.json
VERSION=$(grep "^Version:" DESCRIPTION | cut -d' ' -f2)
jq ".version = \"$VERSION\"" mintlify/mint.json > tmp.json
mv tmp.json mintlify/mint.json
```

## Monitoring and Validation

### Check Deployment Status

1. **GitHub Actions**:
   - Go to Actions tab in GitHub
   - Check "Deploy Mintlify Documentation" workflow
   - Review deployment logs

2. **Mintlify Dashboard**:
   - Log in to dashboard.mintlify.com
   - Check deployment history
   - View build logs

3. **DNS Propagation**:

   ```bash
   # Check DNS resolution
   dig qiprng.biostochastics.com

   # Check CNAME
   nslookup qiprng.biostochastics.com
   ```

### Validate Documentation

```bash
# Local validation
cd mintlify
mintlify validate

# Check for broken links
mintlify check-links

# Test search functionality
mintlify test-search
```

## Troubleshooting

### Common Issues

#### DNS Not Resolving

- Wait 24-48 hours for full propagation
- Clear DNS cache: `sudo dscacheutil -flushcache` (macOS)
- Try different DNS servers (8.8.8.8, 1.1.1.1)

#### SSL Certificate Issues

- Ensure DNS is properly configured
- Wait for automatic SSL provisioning (up to 1 hour)
- Contact Mintlify support if issues persist

#### Build Failures

```bash
# Check configuration
mintlify validate

# Review error logs
cat .mintlify/logs/build.log

# Test locally first
mintlify dev
```

#### Content Not Updating

- Check GitHub Actions logs
- Verify API key is correct
- Clear browser cache
- Use incognito/private browsing

## Maintenance

### Regular Tasks

1. **Weekly**:
   - Check for broken links
   - Review deployment logs
   - Update dependencies

2. **Monthly**:
   - Review analytics
   - Update search indexes
   - Check SSL certificate expiry

3. **Per Release**:
   - Update version numbers
   - Regenerate API documentation
   - Update changelog

### Backup Strategy

```bash
# Backup documentation
tar -czf mintlify-backup-$(date +%Y%m%d).tar.gz mintlify/

# Store in cloud
aws s3 cp mintlify-backup-*.tar.gz s3://backups/qiprng-docs/
```

## Advanced Configuration

### Search Optimization

Edit `mintlify/mint.json`:

```json
{
  "search": {
    "prompt": "Search qiprng documentation...",
    "hotkeys": ["cmd+k", "ctrl+k"],
    "index": {
      "include": ["**/*.mdx"],
      "exclude": ["**/node_modules/**"]
    }
  }
}
```

### Analytics Integration

```json
{
  "analytics": {
    "gtm": {
      "tagId": "GTM-XXXXXXX"
    },
    "ga4": {
      "measurementId": "G-XXXXXXXXXX"
    },
    "plausible": {
      "domain": "qiprng.biostochastics.com"
    }
  }
}
```

### Custom Styling

Create `mintlify/custom.css`:

```css
/* Custom branding */
:root {
  --primary-color: #0D9373;
  --font-family: 'Inter', sans-serif;
}

/* Code block styling */
.code-block {
  background: #1e1e1e;
  border-radius: 8px;
}
```

## Support

- **Mintlify Support**: [support@mintlify.com](mailto:support@mintlify.com)
- **GitHub Issues**: <https://github.com/biostochastics/qiprng/issues>
- **Documentation**: <https://mintlify.com/docs>

## Checklist for First Deployment

- [ ] Mintlify account created
- [ ] API key generated and added to GitHub secrets
- [ ] DNS CNAME record configured
- [ ] SSL certificate provisioned
- [ ] GitHub Actions workflow enabled
- [ ] Initial documentation generated
- [ ] Local testing completed
- [ ] Production deployment successful
- [ ] Custom domain accessible
- [ ] Search functionality working
- [ ] Analytics configured (optional)

## Version History

- v1.0.0: Initial Mintlify setup
- v1.1.0: Added automated roxygen2 conversion
- v1.2.0: C++ documentation parser integration
- v1.3.0: GitHub Actions automation
