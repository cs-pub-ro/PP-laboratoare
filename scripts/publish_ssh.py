#!/usr/bin/env python3
import pypandoc
import argparse
import subprocess
import tempfile
import os

REMOTE_BASE = '/home/pp/pp-pages/26/laboratoare'
REMOTE_HOST = 'pp@ocw.cs.pub.ro'
SSH_OPTS = [
    '-o', 'PubkeyAcceptedKeyTypes=+ssh-rsa',
    '-o', 'HostKeyAlgorithms=+ssh-rsa',
    '-o', 'StrictHostKeyChecking=no',
]

parser = argparse.ArgumentParser(description='ocw pp publishing via ssh')
parser.add_argument(
    '-f',
    '--files',
    nargs='+',
    help='markdown files to publish, all files should be named README.md',
    required=True)


def upload_page(page, content):
    remote_dir = f'{REMOTE_BASE}/{os.path.dirname(page)}'
    remote_path = f'{REMOTE_BASE}/{page}.txt'

    # Ensure remote directory exists
    subprocess.run(
        ['ssh'] + SSH_OPTS + [REMOTE_HOST, f'mkdir -p {remote_dir}'],
        check=True)

    # Write content to a temp file and scp it
    with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
        f.write(content)
        tmp_path = f.name

    try:
        subprocess.run(
            ['scp'] + SSH_OPTS + [tmp_path, f'{REMOTE_HOST}:{remote_path}'],
            check=True)
    finally:
        os.unlink(tmp_path)


def main():
    args = vars(parser.parse_args())
    files = args['files']
    for file in files:
        output = pypandoc.convert_file(file, 'dokuwiki', format='gfm')
        page = file.replace('/README.md', '')
        upload_page(page, output)
        print(f'Published {file} -> {page}')


if __name__ == '__main__':
    main()
