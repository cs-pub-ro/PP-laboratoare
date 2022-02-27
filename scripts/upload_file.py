#!/usr/bin/env python3
import dokuwiki
import argparse


parser = argparse.ArgumentParser(description='ocw pp files publishing')
parser.add_argument('-u', '--username', help='user', required=True)
parser.add_argument('-p', '--password', help='password', required=True)
parser.add_argument('-f', '--file', help='file', required=True)
parser.add_argument('-n', '--namespace', help='namespace', required=True)

def create_connection(username, password):
    return dokuwiki.DokuWiki('https://ocw.cs.pub.ro/courses',
                             username,
                             password,
                             cookieAuth=True)

def upload_file(conn, file, target):
    try:
        conn.medias.add(target, file, overwrite=True)
    except (dokuwiki.DokuWikiError, Exception) as err:
        # ocw doesn't send always valid reply
        if not str(err).startswith(
                'XML or text declaration not at start of entity'):
            print('unable to process: %s' % err)

def main():
    args = vars(parser.parse_args())
    conn = create_connection(args['username'], args['password'])
    file = args['file']
    namespace = args['namespace']
    print(namespace)
    upload_file(conn, file, namespace)



if __name__ == '__main__':
    main()
