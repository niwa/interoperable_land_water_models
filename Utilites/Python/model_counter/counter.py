'''
This module maintains a single counter which can be incremented, decremented,
and shared across programs and modules

The increment() and decrement() functions adjust a count variable 
internal to the module. The resulting count is accessible across 
multiple modules within a running Python interpreter. 

It was created to keep track of the number of PYBIND11-wrapped Python 
programs were imported as DLLs at runtime within a DIMR session.
'''

def increment():
  """
  Advances the counter by 1. 
  If the counter does not exist, it initialized with a value of 1.
  """
  global _count
  try:
    swap = _count
    swap += 1
    _count = swap
  except NameError:
    _count = 1

def decrement():
  """
  Decrements the counter by 1. 
  If the counter does not exist, it is initialized with a value of 0.
  """
  global _count
  try:
    swap = _count
    swap -= 1
    _count = swap
  except NameError:
    _count = 0

def get_count():
  """
  Returns the value of the global variable 'count'. 
  Returns 0 if 'count' does not exist.
  """
  try:
    return _count
  except NameError:
    return 0
