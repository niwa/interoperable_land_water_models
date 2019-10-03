import counter
import child1
import child2

def main():
  print("Parent: Count is {}".format(counter.get_count()))
  counter.increment()

  print("Child 1: Count is {}".format(child1.get_count()))

  print("Child 2: Count is {}".format(child2.get_count()))

  
if __name__ == "__main__":
  main()