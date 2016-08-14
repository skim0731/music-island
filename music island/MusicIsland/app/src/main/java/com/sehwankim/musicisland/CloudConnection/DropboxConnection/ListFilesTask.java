package com.sehwankim.musicisland.CloudConnection.DropboxConnection;

import android.os.AsyncTask;

import com.dropbox.core.DbxException;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.ListFolderResult;

/**
 * Modified by kimsehwan on 2016. 4. 11..
 * original source code: https://github.com/dropbox/dropbox-sdk-java/blob/master/examples/android/src/main/java/com/dropbox/core/examples/android/ListFolderTask.java
 *
 * Async task to list items in a folder.
 *
 * In Android, you cannot make any network request on main thread.
 * Thus, manage each Dropbox API request as a class that extends AsyncTask,
 * so it can run in background.
 */
public class ListFilesTask extends AsyncTask<String, Void, ListFolderResult> {
    private final DbxClientV2 mDbxClient;
    private final Callback mCallback;
    private Exception mException;

    /**
     * Callback functions that define what to do when:
     * data is successfully loaded, or
     * on error.
     */
    public interface Callback {
        void onDataLoaded(ListFolderResult result);
        void onError(Exception e);
    }

    /**
     * Constructor that accepts DropboxClient and Callback functions.
     * @param dbxClient used to make Dropbox API calls.
     * @param callback used on post execute.
     */
    public ListFilesTask(DbxClientV2 dbxClient, Callback callback) {
        mDbxClient = dbxClient;
        mCallback = callback;
    }

    /**
     * If there was an error, run onError().
     * Otherwise, run onDataLoaded().
     * @param result
     */
    @Override
    protected void onPostExecute(ListFolderResult result) {
        super.onPostExecute(result);

        if (mException != null) {
            mCallback.onError(mException);
        } else {
            mCallback.onDataLoaded(result);
        }
    }

    /**
     * This is what the background thread does.
     * Simply make an API call that gets a list of files in Dropbox.
     * @param params
     * @return
     */
    @Override
    protected ListFolderResult doInBackground(String... params) {
        try {
            // Get a list of files on the root directory.
            return mDbxClient.files().listFolder(params[0]);
        } catch (DbxException e) {
            mException = e;
        }

        return null;
    }
}
