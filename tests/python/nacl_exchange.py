#!/usr/bin/env python

"""Decrypt and then re-encrypt a message using NaCl."""

import attr
import argparse
import sys

from nacl.secret import SecretBox
from wormhole import util


def main():
    parser = argparse.ArgumentParser(prog='nacl')
    parser.add_argument(
        '--key', dest='key', type=unicode,
        help='NaCl key for secret box message, hex-encoded')
    parser.add_argument(
        '--nonce', dest='nonce', type=unicode,
        help='NaCl nonce for secret box message, hex-encoded')
    params = parser.parse_args(sys.argv[1:])
    run_exchange(
        Transport(input_stream=sys.stdin, output_stream=sys.stdout),
        util.hexstr_to_bytes(params.key), util.hexstr_to_bytes(params.nonce))


def run_exchange(transport, key, nonce):
    box = SecretBox(key)
    line = transport.receive_line()
    decrypted = box.decrypt(util.hexstr_to_bytes(line))
    transport.send_line(decrypted)
    encrypted = util.bytes_to_hexstr(box.encrypt(decrypted, nonce))
    transport.send_line(encrypted)


@attr.s
class Transport(object):
    # XXX: Duplicated with version_exchange.py
    input_stream = attr.ib()
    output_stream = attr.ib()

    def send_line(self, line):
        self.output_stream.write(line.rstrip().encode('utf8'))
        self.output_stream.write('\n')
        self.output_stream.flush()

    def receive_line(self):
        return self.input_stream.readline().strip().decode('utf8')


if __name__ == '__main__':
    main()
