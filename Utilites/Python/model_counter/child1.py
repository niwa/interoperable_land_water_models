import counter

counter.increment()

def get_count():
  try:
    return counter.get_count()
  except NameError:
    return 0
