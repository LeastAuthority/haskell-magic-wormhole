#!/usr/bin/env python
"""Derive a one-off key for NaCl given a SPAKE2 key."""

import argparse
import sys

from wormhole._key import derive_phase_key
from wormhole import util


def main():
    parser = argparse.ArgumentParser(prog='version_exchange')
    parser.add_argument(
        '--spake-key', dest='spake_key', type=unicode,
        help='SPAKE2 key to generate the one-off key, hex-encoded')
    parser.add_argument(
        '--side', dest='side', type=unicode,
        help='Identifier for this side of the exchange')
    parser.add_argument(
        '--phase', dest='phase', type=unicode,
        help='The phase of the message we need a one-off key for')
    params = parser.parse_args(sys.argv[1:])
    key = derive_phase_key(
        util.hexstr_to_bytes(params.spake_key), params.side, params.phase)
    print util.bytes_to_hexstr(key)


if __name__ == '__main__':
    main()
