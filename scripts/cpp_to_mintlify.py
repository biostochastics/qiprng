#!/usr/bin/env python3
"""
Convert C++ inline documentation to Mintlify MDX format
Parses C++ header files and extracts documentation comments
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple

class CppDocParser:
    """Parse C++ documentation comments and generate MDX"""

    def __init__(self, src_dir: str, output_dir: str):
        self.src_dir = Path(src_dir)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def parse_file(self, filepath: Path) -> Dict:
        """Parse a single C++ file for documentation"""
        with open(filepath, 'r') as f:
            content = f.read()

        # Extract file-level documentation
        file_doc = self._extract_file_doc(content)

        # Extract class documentation
        classes = self._extract_classes(content)

        # Extract method documentation
        methods = self._extract_methods(content)

        return {
            'filename': filepath.name,
            'file_doc': file_doc,
            'classes': classes,
            'methods': methods
        }

    def _extract_file_doc(self, content: str) -> str:
        """Extract file-level documentation comment"""
        pattern = r'^//\s*File:\s*(.+?)$.*?^//\s*-+$'
        match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
        if match:
            # Extract all comment lines until the separator
            lines = content[:match.end()].split('\n')
            doc_lines = [line.lstrip('// ').rstrip() for line in lines if line.startswith('//')]
            return '\n'.join(doc_lines[:-1])  # Exclude the separator line
        return ""

    def _extract_classes(self, content: str) -> List[Dict]:
        """Extract class documentation and signatures"""
        classes = []

        # Pattern for class with documentation
        pattern = r'/\*\*(.*?)\*/\s*class\s+(\w+)(?:\s*:\s*public\s+(\w+))?'

        for match in re.finditer(pattern, content, re.DOTALL):
            doc_comment = match.group(1)
            class_name = match.group(2)
            base_class = match.group(3) if match.group(3) else None

            # Parse the documentation comment
            doc = self._parse_doc_comment(doc_comment)

            classes.append({
                'name': class_name,
                'base': base_class,
                'brief': doc.get('brief', ''),
                'details': doc.get('details', ''),
                'thread_safety': doc.get('thread_safety', ''),
                'performance': doc.get('performance', '')
            })

        return classes

    def _extract_methods(self, content: str) -> List[Dict]:
        """Extract method documentation"""
        methods = []

        # Pattern for documented methods
        pattern = r'/\*\*(.*?)\*/\s*(?:(?:virtual|static|inline)\s+)?(\w+(?:\s*<[^>]+>)?)\s+(\w+)\s*\([^)]*\)'

        for match in re.finditer(pattern, content, re.DOTALL):
            doc_comment = match.group(1)
            return_type = match.group(2)
            method_name = match.group(3)

            # Skip constructors/destructors
            if method_name.startswith('~'):
                continue

            doc = self._parse_doc_comment(doc_comment)

            methods.append({
                'name': method_name,
                'return_type': return_type,
                'brief': doc.get('brief', ''),
                'params': doc.get('params', []),
                'returns': doc.get('returns', ''),
                'complexity': doc.get('complexity', ''),
                'throws': doc.get('throws', '')
            })

        return methods

    def _parse_doc_comment(self, comment: str) -> Dict:
        """Parse a documentation comment into structured data"""
        doc = {
            'brief': '',
            'details': '',
            'params': [],
            'returns': '',
            'complexity': '',
            'thread_safety': '',
            'performance': '',
            'throws': ''
        }

        # Clean up the comment
        lines = [line.strip(' *').strip() for line in comment.split('\n')]

        current_section = 'brief'
        current_content = []

        for line in lines:
            if line.startswith('@brief'):
                current_section = 'brief'
                current_content = [line[6:].strip()]
            elif line.startswith('@details'):
                doc['brief'] = ' '.join(current_content)
                current_section = 'details'
                current_content = []
            elif line.startswith('@param'):
                if current_section == 'details':
                    doc['details'] = '\n'.join(current_content)
                parts = line[6:].strip().split(None, 1)
                if len(parts) == 2:
                    doc['params'].append({'name': parts[0], 'desc': parts[1]})
            elif line.startswith('@return'):
                doc['returns'] = line[7:].strip()
            elif line.startswith('@complexity') or line.startswith('Complexity:'):
                doc['complexity'] = line.split(':', 1)[1].strip() if ':' in line else line[11:].strip()
            elif line.startswith('@throws'):
                doc['throws'] = line[7:].strip()
            elif 'Thread Safety:' in line or 'Thread-safe' in line:
                doc['thread_safety'] = line.split(':', 1)[1].strip() if ':' in line else line
            elif 'Performance:' in line:
                doc['performance'] = line.split(':', 1)[1].strip()
            elif line and not line.startswith('@'):
                current_content.append(line)

        # Handle remaining content
        if current_section == 'brief' and current_content:
            doc['brief'] = ' '.join(current_content)
        elif current_section == 'details' and current_content:
            doc['details'] = '\n'.join(current_content)

        return doc

    def generate_mdx(self, parsed_data: Dict) -> str:
        """Generate MDX content from parsed data"""
        mdx = []

        # Frontmatter
        mdx.append('---')
        mdx.append(f"title: '{parsed_data['filename'].replace('.hpp', '').replace('_', ' ').title()}'")
        mdx.append(f"description: 'C++ implementation details for {parsed_data['filename']}'")
        mdx.append('---')
        mdx.append('')

        # File documentation
        if parsed_data['file_doc']:
            mdx.append('## Overview')
            mdx.append('')
            mdx.append(parsed_data['file_doc'])
            mdx.append('')

        # Classes
        if parsed_data['classes']:
            mdx.append('## Classes')
            mdx.append('')

            for cls in parsed_data['classes']:
                mdx.append(f"### {cls['name']}")
                mdx.append('')

                if cls['base']:
                    mdx.append(f"**Inherits from:** `{cls['base']}`")
                    mdx.append('')

                if cls['brief']:
                    mdx.append(cls['brief'])
                    mdx.append('')

                if cls['details']:
                    mdx.append('<Accordion title="Details">')
                    mdx.append('')
                    mdx.append(cls['details'])
                    mdx.append('')
                    mdx.append('</Accordion>')
                    mdx.append('')

                if cls['thread_safety']:
                    mdx.append('<Note>')
                    mdx.append(f"**Thread Safety:** {cls['thread_safety']}")
                    mdx.append('</Note>')
                    mdx.append('')

                if cls['performance']:
                    mdx.append(f"**Performance:** {cls['performance']}")
                    mdx.append('')

        # Methods
        if parsed_data['methods']:
            mdx.append('## Methods')
            mdx.append('')

            for method in parsed_data['methods']:
                mdx.append(f"### `{method['name']}()`")
                mdx.append('')

                # Signature
                mdx.append('```cpp')
                mdx.append(f"{method['return_type']} {method['name']}(...)")
                mdx.append('```')
                mdx.append('')

                if method['brief']:
                    mdx.append(method['brief'])
                    mdx.append('')

                # Parameters
                if method['params']:
                    mdx.append('#### Parameters')
                    mdx.append('')
                    for param in method['params']:
                        mdx.append(f"- **{param['name']}**: {param['desc']}")
                    mdx.append('')

                # Returns
                if method['returns']:
                    mdx.append('#### Returns')
                    mdx.append('')
                    mdx.append(method['returns'])
                    mdx.append('')

                # Complexity
                if method['complexity']:
                    mdx.append(f"**Complexity:** {method['complexity']}")
                    mdx.append('')

                # Exceptions
                if method['throws']:
                    mdx.append('<Warning>')
                    mdx.append(f"**Throws:** {method['throws']}")
                    mdx.append('</Warning>')
                    mdx.append('')

        return '\n'.join(mdx)

    def process_all(self):
        """Process all C++ files in the source directory"""
        # Find all header files
        header_files = list(self.src_dir.glob('*.hpp')) + list(self.src_dir.glob('*.h'))

        print(f"Found {len(header_files)} header files to process")

        for header_file in header_files:
            print(f"Processing: {header_file.name}")

            try:
                # Parse the file
                parsed = self.parse_file(header_file)

                # Generate MDX
                mdx_content = self.generate_mdx(parsed)

                # Write output
                output_file = self.output_dir / f"{header_file.stem}.mdx"
                output_file.write_text(mdx_content)

                print(f"  ✓ Created: {output_file}")

            except Exception as e:
                print(f"  ✗ Error processing {header_file.name}: {e}")

def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(description='Convert C++ docs to Mintlify MDX')
    parser.add_argument('--src', default='src', help='Source directory with C++ files')
    parser.add_argument('--output', default='mintlify/api-reference/cpp-classes',
                       help='Output directory for MDX files')

    args = parser.parse_args()

    # Create parser and process files
    doc_parser = CppDocParser(args.src, args.output)
    doc_parser.process_all()

    print("\nC++ documentation conversion complete!")

if __name__ == '__main__':
    main()
