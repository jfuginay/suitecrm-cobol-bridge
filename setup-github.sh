#!/bin/bash

echo "🚀 Setting up GitHub repository for SuiteCRM COBOL Bridge"
echo "========================================================"

# Initialize git if not already initialized
if [ ! -d .git ]; then
    echo "📁 Initializing git repository..."
    git init
fi

# Create .gitignore
echo "📝 Creating .gitignore..."
cat > .gitignore << 'EOF'
# Dependencies
node_modules/
vendor/
.pnpm-store/

# Build outputs
build/
dist/
compiled/
*.exe
*.o

# Logs
logs/
*.log
npm-debug.log*
yarn-debug.log*
yarn-error.log*

# Runtime data
pids/
*.pid
*.seed
*.pid.lock

# Environment files
.env
.env.local
.env.*.local

# IDE files
.vscode/
.idea/
*.swp
*.swo
.DS_Store

# Docker volumes
mysql-data/
redis-data/
prometheus-data/
grafana-data/
suitecrm-data/

# Temporary files
*.tmp
*.temp
.cache/

# Debug traces
debugger/traces/
monitor.log
debug-trace.log

# Generated files
api-gateway/swagger-ui/
coverage/
.nyc_output/

# OS files
Thumbs.db
EOF

# Create LICENSE file
echo "📄 Creating LICENSE..."
cat > LICENSE << 'EOF'
MIT License

Copyright (c) 2025 SuiteCRM COBOL Bridge Team

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF

# Use the GitHub-ready README
echo "📖 Setting up README..."
cp README-GITHUB.md README.md

# Generate screenshot placeholders
echo "📸 Generating screenshot placeholders..."
if command -v node &> /dev/null; then
    node create-screenshots.js
else
    echo "⚠️  Node.js not found. Skipping screenshot generation."
fi

# Add all files
echo "📦 Adding files to git..."
git add .

# Create initial commit
echo "💾 Creating initial commit..."
git commit -m "Initial commit: SuiteCRM COBOL Bridge - Complete modernization platform with 6 revolutionary features"

echo ""
echo "✅ Repository is ready for GitHub!"
echo "================================"
echo ""
echo "Next steps:"
echo "1. Create a new repository on GitHub"
echo "2. Add the remote origin:"
echo "   git remote add origin https://github.com/YOUR_USERNAME/suitecrm-cobol-bridge.git"
echo ""
echo "3. Push to GitHub:"
echo "   git branch -M main"
echo "   git push -u origin main"
echo ""
echo "4. Update README-GITHUB.md with:"
echo "   - Your course name and university"
echo "   - Your instructor's name"
echo "   - The actual demo video link"
echo "   - Your GitHub username in the clone URL"
echo ""
echo "5. Take actual screenshots of the running application and replace the placeholders in docs/screenshots/"
echo ""
echo "📚 Share the GitHub link with your instructor!"