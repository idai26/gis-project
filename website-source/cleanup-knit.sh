#!/bin/bash
# Clean up intermediate .knit.md files that can cause rendering issues
find . -maxdepth 1 -name "*.knit.md" -type f -delete
echo "Cleaned up .knit.md files"
