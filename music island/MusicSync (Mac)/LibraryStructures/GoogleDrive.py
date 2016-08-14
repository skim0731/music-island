# coding=<utf-8>

import httplib2
import oauth2client
import apiclient
from oauth2client import client, tools
from apiclient import discovery

class GoogleDriveAccount():
    '''
    Singleton of Google Drive account.
    '''

    class __GoogleDriveAccount:
        try:
            import argparse
            flags = argparse.ArgumentParser(parents=[tools.argparser]).parse_args()
        except ImportError:
            flags = None
        # This program only access its application data.
        SCOPES = 'https://www.googleapis.com/auth/drive.appdata'
        CLIENT_SECRET_FILE = './Configurations/client_secret.json'
        APPLICATION_NAME = 'Music Island'
        CREDENTIAL_PATH = './Configurations/google_drive_credential.json'

        def get_credential(self):
            '''
            A user must login through a web browser and allow this app.
            :return: credential
            '''
            flow = client.flow_from_clientsecrets(filename=self.CLIENT_SECRET_FILE, scope=self.SCOPES)
            flow.user_agent = self.APPLICATION_NAME

            store = oauth2client.file.Storage(self.CREDENTIAL_PATH)

            credentials = tools.run_flow(flow, store, self.flags)

            return credentials
        def __init__(self):
            '''
            Get credential, and initialize 'drive_service' objects.
            List, Upload, Delete functions are in 'drive_service'.
            :return:
            '''
            self.credential = self.get_credential()
            self.http = self.credential.authorize(httplib2.Http())
            # API version 3. This is the most recent version.
            self.drive_service = discovery.build('drive', 'v3', self.http)

        def get_file_list(self):
            # list 1000 files in the appDataFolder
            results = self.drive_service.files().list(pageSize=1000,fields="nextPageToken, files(id, name)", spaces='appDataFolder').execute()
            items = results.get('files', [])
            if not items:
                print('No files found.')
                return list()
            else:
                return items

        def delete_file(self, file_ID):
            file_ID = file_ID.encode('ascii', 'ignore')
            self.drive_service.files().delete(fileId=file_ID).execute()

        def upload_file(self, file_name, file_path):
            f = open(file_path, 'rb')
            if f == None:
                print 'file open error'
            else:
                file_metadata = {
                    'name' : file_name,
                    'parents': [ 'appDataFolder']
                }
                media = apiclient.http.MediaFileUpload(file_path,
                                                        resumable = True)

                self.drive_service.files().create(body=file_metadata, media_body = media, fields='id').execute()
    instance = None

    def __init__(self):
        if not GoogleDriveAccount.instance:
            GoogleDriveAccount.instance = GoogleDriveAccount.__GoogleDriveAccount()