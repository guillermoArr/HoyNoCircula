#!/usr/bin/env python3
import os
import gzip
import shutil
from pathlib import Path

def unzip_gz_files(source_dir, target_dir):
    """
    Unzips all .gz files from the source directory into the target directory.
    
    Args:
        source_dir (str): Path to the source directory containing .gz files
        target_dir (str): Path to the target directory where unzipped files will be saved
    """
    
    # Get all .gz files in the source directory
    source_path = Path(source_dir)
    gz_files = list(source_path.glob('*.gz'))
    
    if not gz_files:
        print(f"No .gz files found in {source_dir}")
        return
    
    print(f"Found {len(gz_files)} .gz files to extract")
    
    # Extract each .gz file
    for gz_file in gz_files:
        # Get the filename without the .gz extension
        output_filename = os.path.basename(gz_file).rsplit('.gz', 1)[0]
        output_path = os.path.join(target_dir, output_filename)
        
        print(f"Extracting {gz_file} to {output_path}")
        
        try:
            with gzip.open(gz_file, 'rb') as f_in:
                with open(output_path, 'wb') as f_out:
                    shutil.copyfileobj(f_in, f_out)
            print(f"Successfully extracted {output_filename}")
        except Exception as e:
            print(f"Error extracting {gz_file}: {e}")

def main():
    # Get the Downloads and Documents folders paths for Windows
    # Using the USERPROFILE environment variable to get the user's home directory
    user_home = os.environ['USERPROFILE']
    downloads_dir = os.path.join(user_home, 'Downloads')
    documents_dir = os.path.join(user_home, 'OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO\\Documentos\\ITAM\\10 sem\\PolPub\\HoyNoCircula\\Datos\\raw')
    
    print(f"Source directory (Downloads): {downloads_dir}")
    print(f"Target directory (Documents): {documents_dir}")
    
    # Extract files
    unzip_gz_files(downloads_dir, documents_dir)
    print("Extraction process completed")
    
    # Keep the console window open to see the results
    input("Press Enter to exit...")

if __name__ == "__main__":
    main()