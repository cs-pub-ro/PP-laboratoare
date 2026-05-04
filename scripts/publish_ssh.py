#!/usr/bin/env python3
import pypandoc
import argparse
import subprocess
import tempfile
import os

REMOTE_BASE = '/home/pp/pp-pages/26/laboratoare'
REMOTE_MEDIA_BASE = '/home/pp/pp-media/26/laboratoare'
REMOTE_HOST = 'pp@ocw.cs.pub.ro'
SSH_OPTS = [
    '-o', 'PubkeyAcceptedKeyTypes=+ssh-rsa',
    '-o', 'HostKeyAlgorithms=+ssh-rsa',
    '-o', 'StrictHostKeyChecking=no',
]
TRANSLATE_PATH_FILTER = './scripts/translate_img_paths.lua'

parser = argparse.ArgumentParser(description='ocw pp publishing via ssh')
parser.add_argument(
    '-f',
    '--files',
    nargs='+',
    help='markdown files to publish, all files should be named README.md',
    required=True)
parser.add_argument(
    '--images',
    nargs='*',
    default=[],
    help='image files to publish, all paths should match "*/ocw-img/*"'
)


def upload_file(local_path, remote_path):
    remote_dir = os.path.dirname(remote_path)
    subprocess.run(
        ['ssh'] + SSH_OPTS + [REMOTE_HOST, f'mkdir -p {remote_dir}'])
    scp_cmd = ['scp'] + SSH_OPTS + [local_path, f'{REMOTE_HOST}:{remote_path}']
    print(scp_cmd)
    subprocess.run(scp_cmd, check=True)


def upload_page(page, content):
    remote_path = f'{REMOTE_BASE}/{page}.txt'
    with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
        f.write(content)
        tmp_path = f.name
    try:
        upload_file(tmp_path, remote_path)
    except Exception as e:
        print(f'WARNING: subprocess failed ({e})')
    finally:
        os.unlink(tmp_path)


def translate_image_path(local_path):
    # e.g. racket/legare/img-ocw/context.png -> racket/img/legare/context.png
    parts = local_path.split('/')
    try:
        idx = parts.index('img-ocw')
    except ValueError:
        raise ValueError(f'image path missing img-ocw segment: {local_path}')
    if idx < 1:
        raise ValueError(f'image path has no lab parent: {local_path}')
    lab_name = parts[idx - 1]
    prefix = parts[:idx - 1]
    suffix = parts[idx + 1:]
    return '/'.join(prefix + ['img', lab_name] + suffix)


def upload_image(img_path):
    relative = translate_image_path(img_path)
    remote_path = f'{REMOTE_MEDIA_BASE}/{relative}'
    try:
        upload_file(img_path, remote_path)
    except Exception as e:
        print(f'WARNING: subprocess failed ({e})')


def main():
    args = vars(parser.parse_args())
    files = args['files']
    for file in files:
        page = file.replace('/README.md', '')
        page_name = os.path.basename(page)
        output = pypandoc.convert_file(
                file,
                'dokuwiki', format='gfm',
                extra_args=
                    [f'--lua-filter={TRANSLATE_PATH_FILTER}',
                     f'--metadata=lab_name:{page_name}'])
        upload_page(page, output)
        print(f'Published {file} -> {page}')
    images = args['images']
    for img_path in images:
        upload_image(img_path)
        print(f'Published {img_path} -> {translate_image_path(img_path)}')


if __name__ == '__main__':
    main()
