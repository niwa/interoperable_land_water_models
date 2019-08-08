#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Python version of bmi-runner to test BMI DLLs on Windows and Linux

Python 3 is required (i.e. this will not work with Python 2)

Example:
$ python bmi-runner.py ../path/to/MyBMI.dll config.yaml

This work is part of a National Science Challenge
Our Land and Water - Interoperable Modelling

Copyright 2019 Mike Toews, GNS Science <m.toews@gns.cri.nz>
"""

__author__ = 'Mike Toews'

import logging
import sys
from ctypes import CFUNCTYPE, PyDLL, POINTER, byref, create_string_buffer, \
    c_char_p, c_double, c_int
from enum import IntEnum

c_double_p = POINTER(c_double)
c_int_p = POINTER(c_int)

logging.basicConfig()


class OpenEarthBMI(object):
    """OpenEarth BMI

    https://github.com/openearth/bmi

    See "bmi.h" for details of C API
    """
    MAXDIMS = 6
    dll = None
    logger = None

    def __init__(self, engine: str, logger) -> None:
        self.logger = logger

        # Assume C++ DLL with Python C API (i.e. Python.h)
        self.dll = dll = PyDLL(engine)

        class Level(IntEnum):
            LEVEL_ALL = 0
            LEVEL_DEBUG = 1
            LEVEL_INFO = 2
            LEVEL_WARNING = 3
            LEVEL_ERROR = 4
            LEVEL_FATAL = 5
            LEVEL_NONE = 6

        # typedef void (CALLCONV *Logger)(Level level, const char *msg)
        LOGGER = CFUNCTYPE(None, c_int, c_char_p)

        def logger_message(level, msg):
            """Logger message function for ctypes callback"""
            level = Level(level)
            msg = msg.decode()
            if level in (Level.LEVEL_ALL, Level.LEVEL_DEBUG):
                self.logger.debug(msg)
            elif level == Level.LEVEL_INFO:
                self.logger.info(msg)
            elif level == Level.LEVEL_WARNING:
                self.logger.warning(msg)
            elif level == Level.LEVEL_ERROR:
                self.logger.error(msg)
            elif level == Level.LEVEL_FATAL:
                self.logger.fatal(msg)
            elif level == Level.LEVEL_NONE:
                pass
            else:
                raise ValueError(level)

        # Important: keep this variable alive to prevent DLL crash
        self._callback_function = LOGGER(logger_message)

        # BMI_API void set_logger(Logger callback)
        dll.set_logger.argtypes = [LOGGER]
        dll.set_logger.restype = None

    def is_implemented(self, name: str) -> bool:
        """Returns True if DLL function is implemented"""
        return hasattr(self.dll, name)

    def initialize(self, config: str) -> int:
        """BMI_API int initialize(const char *config_file)
        """
        if not hasattr(self.dll, 'initialize'):
            raise NotImplementedError('initialize')
        self.dll.initialize.argtypes = [c_char_p]
        self.dll.initialize.restype = c_int
        return self.dll.initialize(c_char_p(config.encode()))

    def update(self, dt: float) -> int:
        """BMI_API int update(double dt)
        """
        if not hasattr(self.dll, 'update'):
            raise NotImplementedError('update')
        self.dll.update.argtypes = [c_double]
        self.dll.update.restype = c_int
        return self.dll.update(dt)

    def finalize(self) -> int:
        """BMI_API int finalize()
        """
        if not hasattr(self.dll, 'finalize'):
            raise NotImplementedError('finalize')
        self.dll.finalize.argtypes = []
        self.dll.finalize.restype = c_int
        return self.dll.finalize()

    def get_start_time(self) -> float:
        """BMI_API void get_start_time(double *t)
        """
        if not hasattr(self.dll, 'get_start_time'):
            raise NotImplementedError('get_start_time')
        self.dll.get_start_time.argtypes = [c_double_p]
        self.dll.get_start_time.restype = None
        t = c_double()
        self.dll.get_start_time(byref(t))
        return t.value

    def get_end_time(self) -> float:
        """BMI_API void get_end_time(double *t)
        """
        if not hasattr(self.dll, 'get_end_time'):
            raise NotImplementedError('get_end_time')
        self.dll.get_end_time.argtypes = [c_double_p]
        self.dll.get_end_time.restype = None
        t = c_double()
        self.dll.get_end_time(byref(t))
        return t.value

    def get_current_time(self) -> float:
        """BMI_API void get_current_time(double *t)
        """
        if not hasattr(self.dll, 'get_current_time'):
            raise NotImplementedError('get_current_time')
        self.dll.get_current_time.argtypes = [c_double_p]
        self.dll.get_current_time.restype = None
        t = c_double()
        self.dll.get_current_time(byref(t))
        return t.value

    def get_time_step(self) -> float:
        """BMI_API void get_time_step(double *dt)
        """
        if not hasattr(self.dll, 'get_time_step'):
            raise NotImplementedError('get_time_step')
        self.dll.get_time_step.argtypes = [c_double_p]
        self.dll.get_time_step.restype = None
        dt = c_double()
        self.dll.get_time_step(byref(dt))
        return dt.value

    def get_var_shape(self, name: str, rank: int) -> tuple:
        """BMI_API void get_var_shape(const char *name, int shape[MAXDIMS])
        """
        if not hasattr(self.dll, 'get_var_shape'):
            raise NotImplementedError('get_var_shape')
        c_int_MAXDIMS = c_int * self.MAXDIMS
        self.dll.get_var_shape.argtypes = [c_char_p, c_int_MAXDIMS]
        self.dll.get_var_shape.restype = None
        shape = c_int_MAXDIMS()
        self.dll.get_var_shape(c_char_p(name.encode()), shape)
        return tuple(shape[i] for i in range(rank))

    def get_var_rank(self, name: str) -> int:
        """BMI_API void get_var_rank(const char *name, int *rank)
        """
        if not hasattr(self.dll, 'get_var_rank'):
            raise NotImplementedError('get_var_rank')
        self.dll.get_var_rank.argtypes = [c_char_p, c_int_p]
        self.dll.get_var_rank.restype = None
        rank = c_int()
        self.dll.get_var_rank(c_char_p(name.encode()), byref(rank))
        return rank.value

    def get_var_type(self, name: str) -> str:
        """BMI_API void get_var_type(const char *name, char *type)
        """
        if not hasattr(self.dll, 'get_var_type'):
            raise NotImplementedError('get_var_type')
        self.dll.get_var_type.argtypes = [c_char_p, c_char_p]
        self.dll.get_var_type.restype = None
        type_buffer = create_string_buffer(b'', 10)
        self.dll.get_var_type(c_char_p(name.encode()), type_buffer)
        return type_buffer.value.decode('utf8')

    def get_var_count(self) -> int:
        """BMI_API void get_var_count(int *count)
        """
        if not hasattr(self.dll, 'get_var_count'):
            raise NotImplementedError('get_var_count')
        self.dll.get_var_count.argtypes = [c_int_p]
        self.dll.get_var_count.restype = None
        count = c_int()
        self.dll.get_var_count(byref(count))
        return count.value

    def get_var_name(self, index: int) -> str:
        """BMI_API void get_var_name(int index, char *name)
        """
        if not hasattr(self.dll, 'get_var_name'):
            raise NotImplementedError('get_var_name')
        self.dll.get_var_name.argtypes = [c_int, c_char_p]
        self.dll.get_var_name.restype = None
        name_buffer = create_string_buffer(b'', 200)
        self.dll.get_var_name(index, name_buffer)
        return name_buffer.value.decode('utf8')


def bmi_runner(engine: str, config: str) -> int:
    logger = logging.getLogger('bmi-runner')
    logger.level = logging.DEBUG

    bmi = OpenEarthBMI(engine, logger)

    retcode = 0
    res = bmi.initialize(config)
    if res != 0:
        logger.error('initialize return code: %s', res)
        retcode += abs(res)

    if bmi.is_implemented('get_start_time'):
        t = bmi.get_start_time()
        logger.debug('start_time is %s', t)

    if bmi.is_implemented('get_end_time'):
        t = bmi.get_end_time()
        logger.debug('end_time is %s', t)

    if bmi.is_implemented('get_current_time'):
        t = bmi.get_current_time()
        logger.debug('current_time is %s', t)

    if bmi.is_implemented('get_time_step'):
        t = bmi.get_time_step()
        logger.debug('time_step is %s', t)

    var_count = None
    if bmi.is_implemented('get_var_count'):
        var_count = bmi.get_var_count()

    var_names = []
    if var_count and bmi.is_implemented('get_var_name'):
        for index in range(var_count):
            name = bmi.get_var_name(index)
            var_names.append(name)
            logger.debug('get_var_name %s is %r', index, name)

    var_ranks = {}
    if var_names and bmi.is_implemented('get_var_rank'):
        for name in var_names:
            rank = bmi.get_var_rank(name)
            var_ranks[name] = rank
            logger.debug('get_var_rank for %r is %d', name, rank)

    if var_ranks and bmi.is_implemented('get_var_shape'):
        for name in var_ranks.keys():
            shape = bmi.get_var_shape(name, var_ranks[name])
            logger.debug('get_var_shape for %r is %s', name, shape)

    if var_names and bmi.is_implemented('get_var_type'):
        for name in var_names:
            type = bmi.get_var_type(name)
            logger.debug('get_var_type for %r is %r', name, type)

    if bmi.is_implemented('update'):
        res = bmi.update(1.2)
        if res != 0:
            logger.error('update return code: %s', res)
            retcode += abs(res)

    res = bmi.finalize()
    if res != 0:
        logger.error('finalize return code: %s', res)
        retcode += abs(res)

    if retcode == 0:
        logger.debug('Completed successfully!')
    else:
        logger.debug('Completed with issues.')
    return retcode


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('engine')
    parser.add_argument('config')
    args = vars(parser.parse_args())
    sys.exit(bmi_runner(**args))
