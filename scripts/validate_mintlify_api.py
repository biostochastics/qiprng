#!/usr/bin/env python3
"""
Validate Mintlify documentation against API requirements
Ensures compliance with 2024 Mintlify standards
"""

import json
import os
import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple

class MintlifyValidator:
    """Validate Mintlify documentation structure and content"""

    def __init__(self, mintlify_dir: str):
        self.mintlify_dir = Path(mintlify_dir)
        self.errors = []
        self.warnings = []
        self.info = []

    def validate_all(self) -> bool:
        """Run all validation checks"""
        print("🔍 Validating Mintlify Documentation API Compliance")
        print("=" * 50)

        # Check configuration
        self.validate_config()

        # Check MDX syntax
        self.validate_mdx_files()

        # Check component usage
        self.validate_components()

        # Check navigation structure
        self.validate_navigation()

        # Report results
        self.report_results()

        return len(self.errors) == 0

    def validate_config(self):
        """Validate docs.json configuration"""
        config_path = self.mintlify_dir / "docs.json"

        if not config_path.exists():
            # Check for legacy mint.json
            legacy_path = self.mintlify_dir / "mint.json"
            if legacy_path.exists():
                self.warnings.append("⚠️  Using legacy mint.json - should migrate to docs.json")
                config_path = legacy_path
            else:
                self.errors.append("❌ docs.json not found")
                return

        try:
            with open(config_path, 'r') as f:
                config = json.load(f)
        except json.JSONDecodeError as e:
            self.errors.append(f"❌ Invalid JSON in mint.json: {e}")
            return

        # Check required fields (2024 API)
        required_fields = ['name', 'colors', 'navigation']
        for field in required_fields:
            if field not in config:
                self.errors.append(f"❌ Missing required field: {field}")
            elif field == 'colors' and 'primary' not in config['colors']:
                self.errors.append("❌ Missing required colors.primary field")

        # Check recommended fields
        recommended_fields = ['theme', 'logo', 'favicon', 'topbarLinks']
        for field in recommended_fields:
            if field not in config:
                self.warnings.append(f"⚠️  Missing recommended field: {field}")

        # Validate theme field
        if 'theme' in config:
            valid_themes = ['default', 'quill', 'prism']
            if config['theme'] not in valid_themes:
                self.warnings.append(f"⚠️  Invalid theme: {config['theme']}. Valid options: {valid_themes}")

        self.info.append(f"✓ Configuration file validated: {config.get('name', 'unnamed')}")

    def validate_mdx_files(self):
        """Validate MDX file syntax"""
        mdx_files = list(self.mintlify_dir.rglob("*.mdx"))

        if not mdx_files:
            self.warnings.append("⚠️  No MDX files found")
            return

        for mdx_file in mdx_files:
            with open(mdx_file, 'r') as f:
                content = f.read()

            # Check frontmatter
            if not content.startswith('---'):
                self.errors.append(f"❌ Missing frontmatter in {mdx_file.name}")
                continue

            # Extract frontmatter
            frontmatter_match = re.match(r'^---\n(.*?)\n---', content, re.DOTALL)
            if frontmatter_match:
                frontmatter = frontmatter_match.group(1)
                if 'title' not in frontmatter:
                    self.warnings.append(f"⚠️  Missing title in {mdx_file.name}")
                if 'description' not in frontmatter:
                    self.warnings.append(f"⚠️  Missing description in {mdx_file.name}")

        self.info.append(f"✓ Validated {len(mdx_files)} MDX files")

    def validate_components(self):
        """Validate Mintlify component usage"""
        mdx_files = list(self.mintlify_dir.rglob("*.mdx"))

        component_patterns = {
            'ParamField': r'<ParamField\s+(?:body|query|path|header)="[^"]+"\s+type="[^"]+"\s*(?:required)?>',
            'CodeGroup': r'<CodeGroup>.*?</CodeGroup>',
            'Card': r'<Card\s+.*?>',
            'CardGroup': r'<CardGroup\s+.*?>',
            'Accordion': r'<Accordion\s+.*?>',
            'Note': r'<Note>',
            'Warning': r'<Warning>',
            'Info': r'<Info>',
            'Tip': r'<Tip>'
        }

        for mdx_file in mdx_files:
            with open(mdx_file, 'r') as f:
                content = f.read()

            # Check ParamField syntax specifically
            param_fields = re.findall(r'<ParamField[^>]*>', content)
            for param in param_fields:
                # Check for correct attributes
                if 'query=' in param and '/api-reference/r-functions/' in str(mdx_file):
                    # R functions shouldn't use 'query' attribute
                    self.warnings.append(f"⚠️  Incorrect ParamField in {mdx_file.name}: R functions should use 'body' not 'query'")

                # Check for deprecated syntax
                if 'query="params"' in param:
                    self.errors.append(f"❌ Deprecated ParamField syntax in {mdx_file.name}: Don't wrap params in object")

                # Check for docs.json specific syntax
                if 'query=' in param and 'type="object"' in param:
                    self.warnings.append(f"⚠️  Legacy ParamField syntax in {mdx_file.name}")

        self.info.append("✓ Component syntax validated")

    def validate_navigation(self):
        """Validate navigation structure"""
        config_path = self.mintlify_dir / "docs.json"

        if not config_path.exists():
            config_path = self.mintlify_dir / "mint.json"
            if not config_path.exists():
                return

        with open(config_path, 'r') as f:
            config = json.load(f)

        if 'navigation' not in config:
            return

        # Check all navigation pages exist
        missing_pages = []
        for group in config['navigation']:
            if 'pages' in group:
                for page in group['pages']:
                    # Handle nested pages
                    if isinstance(page, dict) and 'pages' in page:
                        for subpage in page['pages']:
                            page_path = self.mintlify_dir / f"{subpage}.mdx"
                            if not page_path.exists():
                                missing_pages.append(subpage)
                    else:
                        page_path = self.mintlify_dir / f"{page}.mdx"
                        if not page_path.exists():
                            missing_pages.append(page)

        if missing_pages:
            for page in missing_pages[:5]:  # Limit output
                self.warnings.append(f"⚠️  Navigation references missing page: {page}")
            if len(missing_pages) > 5:
                self.warnings.append(f"⚠️  ... and {len(missing_pages) - 5} more missing pages")
        else:
            self.info.append("✓ All navigation pages exist")

    def report_results(self):
        """Report validation results"""
        print("\n📊 Validation Results")
        print("-" * 50)

        # Errors
        if self.errors:
            print(f"\n🔴 Errors ({len(self.errors)}):")
            for error in self.errors:
                print(f"  {error}")

        # Warnings
        if self.warnings:
            print(f"\n🟡 Warnings ({len(self.warnings)}):")
            for warning in self.warnings[:10]:  # Limit output
                print(f"  {warning}")
            if len(self.warnings) > 10:
                print(f"  ... and {len(self.warnings) - 10} more warnings")

        # Info
        if self.info:
            print(f"\n🟢 Success ({len(self.info)}):")
            for info in self.info:
                print(f"  {info}")

        # Summary
        print("\n" + "=" * 50)
        if self.errors:
            print("❌ FAILED: Fix errors before deployment")
        elif self.warnings:
            print("⚠️  PASSED WITH WARNINGS: Consider addressing warnings")
        else:
            print("✅ PASSED: Documentation is API compliant")

def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(description='Validate Mintlify documentation')
    parser.add_argument('--dir', default='mintlify',
                       help='Mintlify directory to validate')

    args = parser.parse_args()

    validator = MintlifyValidator(args.dir)
    success = validator.validate_all()

    sys.exit(0 if success else 1)

if __name__ == '__main__':
    main()
