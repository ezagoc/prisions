import os
from pathlib import Path
from pyunpack import Archive

# Set the directory (current directory)
directory = Path('../../data/01-judicial/00-sentencing/raw/')

# Loop through archive files
for file in directory.iterdir():
    if file.suffix in [".zip", ".rar"] or file.name.endswith(".tar.gz"):
        output_folder = directory / file.stem.replace(".tar", "")  # clean up name
        output_folder.mkdir(exist_ok=True)
        print(f"Extracting {file} to {output_folder}...")
        try:
            Archive(str(file)).extractall(str(output_folder))
        except Exception as e:
            print(f"Failed to extract {file.name}: {e}")
