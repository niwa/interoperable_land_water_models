#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Python version of bmi-runner to test BMI DLLs on Windows and Linux

Python 3 is required (i.e. this will not work with Python 2)

Example from command prompt:
$ python bmi-runner.py ../path/to/MyBMI.dll config.yaml

Example as Python module:
>>> import importlib
>>> brmod = importlib.import_module("Test.bmi-runner")
>>> engine = 'SteadyState/GroundwaterBMI/x64/Release/GroundwaterBMI.dll'
>>> bmi = brmod.OpenEarthBMI(engine)
>>> bmi.initialize('F:/ES/aparima/Aparima-git/misc/aparima-po.yaml')
>>> for i in range(bmi.get_var_count()):
...     print('{}: {}'.format(i, bmi.get_var_name(i)))

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

    def __init__(self, engine: str, logger=None) -> None:

        # Provide a default logger, if None provided
        if logger is None:
            logger = logging.getLogger('OpenEarthBMI')

        self.logger = logger

        # Assume C++ DLL with Python C API (i.e. Python.h)
        self.dll = dll = PyDLL(engine)

        # cache properties here
        self._implemented = {}  # {name: bool or None}

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
        try:
            res = self._implemented[name]
        except KeyError:
            res = hasattr(self.dll, name)
            if res:
                self._implemented[name] = None
            else:
                self._implemented[name] = False
        return res is None or res

    def not_defined(self, name: str) -> bool:
        """Returns True if DLL function is not defined, False if it is,
        and raises NotImplementedError if not implemented"""
        try:
            res = self._implemented[name]
        except KeyError:
            res = hasattr(self.dll, name)
            if res:
                self._implemented[name] = None
                res = None
            else:
                self._implemented[name] = False
        if res is False:
            raise NotImplementedError(name)
        return res is None

    def initialize(self, config: str) -> int:
        """BMI_API int initialize(const char *config_file)
        """
        if self.not_defined('initialize'):
            self.dll.initialize.argtypes = [c_char_p]
            self.dll.initialize.restype = c_int
            self._implemented['initialize'] = True
        return self.dll.initialize(c_char_p(config.encode()))

    def update(self, dt: float) -> int:
        """BMI_API int update(double dt)
        """
        if self.not_defined('update'):
            self.dll.update.argtypes = [c_double]
            self.dll.update.restype = c_int
            self._implemented['update'] = True
        return self.dll.update(dt)

    def finalize(self) -> int:
        """BMI_API int finalize()
        """
        if self.not_defined('finalize'):
            self.dll.finalize.argtypes = []
            self.dll.finalize.restype = c_int
            self._implemented['finalize'] = True
        return self.dll.finalize()

    def get_start_time(self) -> float:
        """BMI_API void get_start_time(double *t)
        """
        if self.not_defined('get_start_time'):
            self.dll.get_start_time.argtypes = [c_double_p]
            self.dll.get_start_time.restype = None
            self._implemented['get_start_time'] = True
        t = c_double()
        self.dll.get_start_time(byref(t))
        return t.value

    def get_end_time(self) -> float:
        """BMI_API void get_end_time(double *t)
        """
        if self.not_defined('get_end_time'):
            self.dll.get_end_time.argtypes = [c_double_p]
            self.dll.get_end_time.restype = None
            self._implemented['get_end_time'] = True
        t = c_double()
        self.dll.get_end_time(byref(t))
        return t.value

    def get_current_time(self) -> float:
        """BMI_API void get_current_time(double *t)
        """
        if self.not_defined('get_current_time'):
            self.dll.get_current_time.argtypes = [c_double_p]
            self.dll.get_current_time.restype = None
            self._implemented['get_current_time'] = True
        t = c_double()
        self.dll.get_current_time(byref(t))
        return t.value

    def get_time_step(self) -> float:
        """BMI_API void get_time_step(double *dt)
        """
        if self.not_defined('get_time_step'):
            self.dll.get_time_step.argtypes = [c_double_p]
            self.dll.get_time_step.restype = None
            self._implemented['get_time_step'] = True
        dt = c_double()
        self.dll.get_time_step(byref(dt))
        return dt.value

    def get_var_shape(self, name: str, rank: int) -> tuple:
        """BMI_API void get_var_shape(const char *name, int shape[MAXDIMS])
        """
        c_int_MAXDIMS = c_int * self.MAXDIMS
        if self.not_defined('get_var_shape'):
            self.dll.get_var_shape.argtypes = [c_char_p, c_int_MAXDIMS]
            self.dll.get_var_shape.restype = None
            self._implemented['get_var_shape'] = True
        shape = c_int_MAXDIMS()
        self.dll.get_var_shape(c_char_p(name.encode()), shape)
        return tuple(shape[i] for i in range(rank))

    def get_var_rank(self, name: str) -> int:
        """BMI_API void get_var_rank(const char *name, int *rank)
        """
        if self.not_defined('get_var_rank'):
            self.dll.get_var_rank.argtypes = [c_char_p, c_int_p]
            self.dll.get_var_rank.restype = None
            self._implemented['get_var_rank'] = True
        rank = c_int()
        self.dll.get_var_rank(c_char_p(name.encode()), byref(rank))
        return rank.value

    def get_var_type(self, name: str) -> str:
        """BMI_API void get_var_type(const char *name, char *type)
        """
        if self.not_defined('get_var_type'):
            self.dll.get_var_type.argtypes = [c_char_p, c_char_p]
            self.dll.get_var_type.restype = None
            self._implemented['get_var_type'] = True
        type_buffer = create_string_buffer(b'', 10)
        self.dll.get_var_type(c_char_p(name.encode()), type_buffer)
        return type_buffer.value.decode('utf8')

    def get_var_count(self) -> int:
        """BMI_API void get_var_count(int *count)
        """
        if self.not_defined('get_var_count'):
            self.dll.get_var_count.argtypes = [c_int_p]
            self.dll.get_var_count.restype = None
            self._implemented['get_var_count'] = True
        count = c_int()
        self.dll.get_var_count(byref(count))
        return count.value

    def get_var_name(self, index: int) -> str:
        """BMI_API void get_var_name(int index, char *name)
        """
        if self.not_defined('get_var_name'):
            self.dll.get_var_name.argtypes = [c_int, c_char_p]
            self.dll.get_var_name.restype = None
            self._implemented['get_var_name'] = True
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

    if bmi.is_implemented('get_var_count'):
        var_count = bmi.get_var_count()
        if bmi.is_implemented('get_var_name'):
            for index in range(var_count):
                name = bmi.get_var_name(index)
                logger.debug('get_var_name %s is %r', index, name)
                if bmi.is_implemented('get_var_rank'):
                    rank = bmi.get_var_rank(name)
                    logger.debug('get_var_rank is %d', rank)
                    if bmi.is_implemented('get_var_shape'):
                        shape = bmi.get_var_shape(name, rank)
                        logger.debug('get_var_shape is %s', shape)
                if bmi.is_implemented('get_var_type'):
                    type = bmi.get_var_type(name)
                    logger.debug('get_var_type is %r', type)

    if bmi.is_implemented('update'):
        res = bmi.update(-1.0)
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
