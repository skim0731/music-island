# coding=<utf-8>
import urllib
import json
import xml.etree.cElementTree as ET

from Dropbox import DropboxAccount
from GoogleDrive import GoogleDriveAccount
from Multithread import globals as g

class MusicLibrary:

    with open('./Configurations/config.json') as config_file:
        config = json.load(config_file)

    dropbox_path = config['dropbox_path']
    itunes_path = config['itunes_path']

    def __init__(self):
        self.cloud_type = g.cloud

        # init cloud account.
        if self.cloud_type == 'Dropbox':
            DropboxAccount()
        elif self.cloud_type == 'Google Drive':
            GoogleDriveAccount()

        self.tracks = list()
        tree = ET.parse(self.itunes_path)
        library = tree.getroot()
        library = library.find('dict')
        library = library.find('dict') # Now 'library' is a list of tracks

        for music in library:
            # Now 'music' contains the list of information about a music.
            if music.tag == 'dict':
                name_ready = False
                location_ready = False
                file_size_ready = False
                artist_ready = False
                trackID_ready = False
                get_name = False
                get_location = False
                get_file_size = False
                get_artist = False
                get_trackID = False
                # Parse through music_data, and get values for keys 'Name', 'Size' and 'Location'.
                for music_data in music:
                    text = music_data.text
                    if text == 'Name':
                        get_name = True
                    elif text == 'Artist':
                        get_artist = True
                    elif text == 'Location':
                        get_location = True
                    elif text == 'Size':
                        get_file_size = True
                    elif text == 'Track ID':
                        get_trackID = True
                    elif get_name:
                        track_name = text
                        get_name = False
                        name_ready = True
                    elif get_artist:
                        track_artist = text
                        get_artist = False
                        artist_ready = True
                    elif get_location:
                        byte_string = urllib.unquote(text[7:])
                        track_location = byte_string.decode('utf-8')
                        get_location = False
                        location_ready = True
                    elif get_file_size:
                        track_size = text
                        get_file_size = False
                        file_size_ready = True
                    elif get_trackID:
                        trackID = text
                        get_trackID = False
                        trackID_ready = True

                    # Add data to the list as a tuple of (name, artist, size, location).
                    if name_ready and artist_ready and location_ready and file_size_ready and trackID_ready:
                        # Remove any quotation marks! They cause errors.
                        track_name.replace('\'', '')
                        track_name.replace('\"', '')
                        track_artist.replace('\'', '')
                        track_artist.replace('\"', '')

                        self.tracks.append((track_name, track_artist, track_size, track_location, trackID))
                        name_ready = False
                        location_ready = False
                        file_size_ready = False
                        artist_ready = False
                        trackID_ready = False

    def upload_all_tracks_Dropbox(self):
        '''
        Upload all tracks using Dropbox API.
        This method avoids uploading the same file twice.
        :return:
        '''
        dropbox_file_list = DropboxAccount.instance.get_file_list()

        i = 1
        for track in self.tracks:
            file_path = track[3]
            file_name = self.get_filename(track)
            # check if the file is already in the cloud.
            if file_name in dropbox_file_list:
                print file_name + ' already exists.'
            else:
                DropboxAccount.instance.upload_file(file_name, file_path)
                print i, "uploaded " + file_name
                i += 1
        print 'Upload Done'

    def get_filename(self, track):
        # filename is in the form of : 'trackID artist - name.file_type'
        return track[4] + ' ' + track[1] + ' - ' + track[0] + self.get_file_type(track[3])

    def get_file_type(self, file_path):
        '''
        Get a file type (mp3, m4a, etc.) from a file_path.
        :param file_path: a path to a file
        :return: file type
        '''
        index = file_path.rfind('.')
        return file_path[index:]
    def delete_unwanted_tracks_Dropbox(self):
        '''
        Delete files in Dropbox that are not in iTunes
        :return:
        '''
        # Get a list of files in the Dropbox
        cloud_list = DropboxAccount.instance.get_file_list()
        # Get a list of tracks in the iTunes library
        track_name_list = [self.get_filename(track) for track in self.tracks]
        # If there is a file in Dropbox that is not in iTunes, delete it.
        for cloud_file in cloud_list:
            if cloud_file not in track_name_list:
                DropboxAccount.instance.delete_file(cloud_file)
                print cloud_file + ' deleted'
        print 'Delete Done'

    def upload_all_tracks_GoogleDrive(self):
        '''
        Upload files in iTunes that are not in Google Drive.
        :return:
        '''
        # Get list of files
        drive_file_list = GoogleDriveAccount.instance.get_file_list()

        # Get names of files
        file_list = [item['name'] for item in drive_file_list]

        i = 1
        for track in self.tracks:
            file_path = track[3]
            file_name = self.get_filename(track)
            # check if the file is already in the cloud.
            if file_name in file_list:
                print file_name + ' already exists.'
            else:
                GoogleDriveAccount.instance.upload_file(file_name, file_path)
                print i, "uploaded " + file_name
                i += 1
        print 'Upload Done'

    def delete_unwanted_tracks_GoogleDrive(self):
        '''
        Delete files in Google Drive that are not in iTunes.
        :return:
        '''
        # Get a list of files in the GoogleDrive
        drive_file_list = GoogleDriveAccount.instance.get_file_list()
        # Get a list of tracks in the iTunes library
        track_name_list = [self.get_filename(track) for track in self.tracks]
        # If there is a file in Google Drive that is not in iTunes, delete it.
        for drive_file in drive_file_list:
            if drive_file['name'] not in track_name_list:
                GoogleDriveAccount.instance.delete_file(drive_file['id'])
                print drive_file['name'] + ' deleted'
        print 'Delete Done'

    def do_sync(self):
        '''
        Sync Dropbox to iTunes library.
        First, delete files in Dropbox that are not in iTunes.
        Second, upload files in iTunes to Dropbox
        (upload_all_tracks_API method ensures that we are not uploading the same file twice).
        :return:
        '''
        if self.cloud_type == 'Dropbox':
            self.delete_unwanted_tracks_Dropbox()
            self.upload_all_tracks_Dropbox()
        elif self.cloud_type == 'Google Drive':
            self.delete_unwanted_tracks_GoogleDrive()
            self.upload_all_tracks_GoogleDrive()

    def print_all_tracks(self):
        i = 1
        for track in self.tracks:
            print i, track
            i += 1

