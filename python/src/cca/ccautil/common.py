#!/usr/bin/env python3

import os
import logging

LOGGING_FORMAT = '[%(asctime)s][%(levelname)s][%(module)s][%(funcName)s] %(message)s'

DEFAULT_LOGGING_LEVEL = 25


def setup_logger(logger, log_level=DEFAULT_LOGGING_LEVEL, log_file=None):
    if log_file:
        fh = logging.FileHandler(log_file, mode='w', encoding='utf-8')
        fh.setLevel(log_level)
        fmt = logging.Formatter(LOGGING_FORMAT)
        fh.setFormatter(fmt)
        logging.basicConfig(level=log_level, handlers=[fh])
        logger.addHandler(fh)
    else:
        logging.basicConfig(format=LOGGING_FORMAT, level=log_level)


def normpath(path):
    p = os.path.normpath(path)
    if ' ' in p:
        p = "'"+p+"'"
    return p
