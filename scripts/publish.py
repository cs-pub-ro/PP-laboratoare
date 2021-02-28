#!/usr/bin/env python3
import dokuwiki
import pypandoc
import argparse

NAMESPACE = 'pp/21/laboratoare/'

parser = argparse.ArgumentParser(description='ocw pp publishing')
parser.add_argument('-u', '--username', help='user', required=True)
parser.add_argument('-p', '--password', help='password', required=True)
parser.add_argument(
    '-f',
    '--files',
    nargs='+',
    help='markdown files to publish, all files should be named README.md',
    required=True)


def create_connection(username, password):
    return dokuwiki.DokuWiki('https://ocw.cs.pub.ro/courses',
                             username,
                             password,
                             cookieAuth=True)


def update_pages(conn, page, content):
    try:
        conn.pages.set(NAMESPACE + page, content)
    except (dokuwiki.DokuWikiError, Exception) as err:
        # ocw doesn't send always valid reply
        if not str(err).startswith(
                'XML or text declaration not at start of entity'):
            print('unable to process: %s' % err)


def main():
    args = vars(parser.parse_args())
    conn = create_connection(args['username'], args['password'])
    files = args['files']
    for file in files:
        output = pypandoc.convert_file(file, 'dokuwiki', format='gfm')
        update_pages(conn, file.replace('/README.md', ''), output)


if __name__ == '__main__':
    main()
