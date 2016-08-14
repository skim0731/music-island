# coding=<utf-8>
import dropbox
from dropbox import DropboxOAuth2FlowNoRedirect
import json

class DropboxAccount:
    '''
    Singleton Class of DropboxAccount
    '''
    class __DropboxAccount:
        # Get access token from config.json. (Saved for later)
        with open('./Configurations/config.json') as config_file:
            config = json.load(config_file)

        APP_KEY = config['APP_KEY']
        APP_SECRET = config['APP_SECRET']

        def __init__(self):
            '''
            Redirect to Dropbox page to get a ACCESS_TOKEN
            source: http://dropbox-sdk-python.readthedocs.org/en/master/moduledoc.html#module-dropbox.oauth
            :return:
            '''
            auth_flow = DropboxOAuth2FlowNoRedirect(self.APP_KEY, self.APP_SECRET)

            authorize_url = auth_flow.start()
            print "1. Go to: " + authorize_url
            print "2. Click \"Allow\" (you might have to log in first)."
            print "3. Copy the authorization code."
            auth_code = raw_input("Enter the authorization code here: ").strip()

            try:
                ACCESS_TOKEN, USER_ID = auth_flow.finish(auth_code)
            except Exception, e:
                print('Error: %s' % (e,))
                return

            self.dbx = dropbox.Dropbox(ACCESS_TOKEN)

        def get_file_list(self):
            '''
            :return: a list of file_name's in the Dropbox folder
            '''
            dropbox_file_list = [entry.name for entry in self.dbx.files_list_folder('').entries]
            return dropbox_file_list

        def print_files(self):
            '''
            print all files in the Dropbox folder
            :return:
            '''
            for entry in self.dbx.files_list_folder('').entries:
                print entry.name

        def upload_file(self, file_name, file_path):
            '''
            Upload a file to Dropbox
            :param file_name: file name to be used in Dropbox
            :param file_path: path of a file in PC
            :return:
            '''
            f = open(file_path, 'rb')
            if f == None:
                print 'file open error'
            else:
                # upload a file 'f' with 'file_name' to the root directory of the Dropbox's app folder
                self.dbx.files_upload(f, '/' + file_name)

        def delete_file(self, file_name):
            '''
            Delete a file in Dropbox
            :param file_name: name of file in Dropbox
            :return:
            '''
            search_list = self.dbx.files_search('', file_name)

            match_names = [entry.metadata.name for entry in search_list.matches]
            if file_name in match_names:
                self.dbx.files_delete('/'+file_name)
            else:
                print 'no such file'

    instance = None

    def __init__(self):
        if not DropboxAccount.instance:
            DropboxAccount.instance = DropboxAccount.__DropboxAccount()
