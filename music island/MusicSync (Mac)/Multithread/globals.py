from Queue import Queue
from threading import Lock

lock = Lock()
q = Queue()
program_quit = False
cloud = ''

def init():
    global q
    global program_quit
    global cloud