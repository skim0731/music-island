import time
from threading import Thread

from watchdog.observers import Observer

from Multithread import globals as g
from LibraryStructures.MusicLibrary import MusicLibrary
from Multithread.EventHandler import EventHandler
import json

def sync_worker():
    while True:
        # If queue is not empty, do sync.
        while g.q.not_empty:
            new_lib = g.q.get()
            new_lib.do_sync()
            # Check the global variable.
            g.lock.acquire()
            if g.program_quit:
                g.lock.release()
                break
            g.lock.release()
        # Check the global variable.
        g.lock.acquire()
        if g.program_quit:
            g.lock.release()
            break
        g.lock.release()

        time.sleep(1)

if __name__ == '__main__':
    # initialize global variables
    g.init()

    # Ask Dropbox or Google Drive.
    while True:
        print '1. Dropbox'
        print '2. Google Drive'

        cloud = raw_input()
        if int(cloud) == 1:
            print 'using Dropbox'
            g.cloud = 'Dropbox'
            break
        elif int(cloud) == 2:
            print 'using Google Drive'
            g.cloud = 'Google Drive'
            break
        else:
            print 'Invalid input. Try again.'

    # Create Sync Thread.
    sync_thread = Thread(target=sync_worker, args=())

    # Do initial sync.
    g.q.put(MusicLibrary())
    # Start Sync Thread.
    sync_thread.start()

    # Create EventHandler and Observer to watch file change.
    event_handler = EventHandler()
    observer = Observer()
    # Tell observer a file path.
    with open('./Configurations/config.json') as config_file:
        config = json.load(config_file)

    itunes_dir = config['itunes_dir']
    observer.schedule(event_handler, path=itunes_dir, recursive=False)
    # Start Observer (multi-thread)
    observer.start()

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
        # Tell sync_thread to quit.
        g.lock.acquire()
        g.program_quit = True
        g.lock.release()

    # Wait until threads end.
    observer.join()
    sync_thread.join()
