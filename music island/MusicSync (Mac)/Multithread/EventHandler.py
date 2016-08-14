from watchdog.events import FileSystemEventHandler

import json
import globals as g
from LibraryStructures.MusicLibrary import MusicLibrary


class EventHandler(FileSystemEventHandler):
    with open('./Configurations/config.json') as config_file:
        config = json.load(config_file)

    itunes_dir = config['itunes_dir']

    def on_modified(self, event):
        # iTunes Media Library.xml is modified, do sync.
        if event.src_path == self.itunes_dir:
            g.q.put(MusicLibrary())