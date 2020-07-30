NAME = GroundwaterBMI

LDFLAGS = -shared
CXXFLAGS = -g -rdynamic -Wall -fPIC $(shell python3-config --includes)

NAMESRC = $(NAME).cpp
NAMEOBJ = $(NAME).o
NAMEDLL = $(NAME).so

all : $(NAMEDLL)

$(NAMEDLL) : $(NAMEOBJ)
	$(CXX) $(LDFLAGS) -o $@ $<

clean :
	$(RM) $(NAMEOBJ) $(NAMEDLL)
