package com.sehwankim.musicisland.CloudConnection.DropboxConnection;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Environment;
import android.util.Log;

import com.dropbox.core.DbxException;
import com.dropbox.core.v2.DbxClientV2;
import com.dropbox.core.v2.files.Metadata;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Modified by kimsehwan on 2016. 4. 11..
 *
 * Async task to download a file from Dropbox and put it in the Downloads folder.
 * Original source: https://github.com/dropbox/dropbox-sdk-java/blob/master/examples/android/src/main/java/com/dropbox/core/examples/android/DownloadFileTask.java
 */
public class DownloadFileTask extends AsyncTask <List<Metadata>, Void, List<File>> {

    private final Context mContext;
    private final DbxClientV2 mDbxClient;
    private final Callback mCallback;
    private Exception mException;

    /**
     * Callback functions to be called after the API call.
     */
    public interface Callback {
        void onDownloadComplete(List<File> result);
        void onError(Exception e);
    }

    /**
     * Constructor.
     * @param context You need a Context to make a system calls (like file read/write.)
     * @param dbxClient used to make Dropbox API calls.
     * @param callback used after Dropbox API calls are made.
     */
    public DownloadFileTask(Context context, DbxClientV2 dbxClient, Callback callback) {
        mContext = context;
        mDbxClient = dbxClient;
        mCallback = callback;
    }

    /**
     * If there was an error, run onError().
     * Otherwise, run onDataLoaded().
     * @param result
     */
    @Override
    protected void onPostExecute(List<File> result) {
        super.onPostExecute(result);
        if (mException != null) {
            mCallback.onError(mException);
        } else {
            mCallback.onDownloadComplete(result);
        }
    }

    /**
     * Download a list of files from Dropbox
     * @param params a list of Metadata of files that needs to be downloaded
     * @return
     */
    @Override
    protected List<File> doInBackground(List<Metadata>... params) {
        if (params[0] == null)
            return null;

        List<File> downloadedFiles = new ArrayList<>();

        for (Metadata metadata : params[0]){
            if (metadata == null){
                Log.e("DownloadFileTask", "metadata is null");
                return null;
            }

            Log.e("Download", "Start");
            try {
                // Set download path. (System's Music folder)
                File path = Environment.getExternalStoragePublicDirectory(
                        Environment.DIRECTORY_MUSIC);
                // File that we are going to write.
                File file = new File(path, metadata.getName());
                Log.d("", file.getPath());


                // Make sure the Downloads directory exists.
                if (!path.exists()) {
                    if (!path.mkdirs()) {
                        mException = new RuntimeException("Unable to create directory: " + path);
                    }
                } else if (!path.isDirectory()) {
                    mException = new IllegalStateException("Download path is not a directory: " + path);
                    return null;
                }


                // Download the file.
                Log.d("", "before outputStream");
                try (OutputStream outputStream = new FileOutputStream(file)) {
                    Log.d("", metadata.getPathLower());

                    // API call to download file to 'file' object.
                    mDbxClient.files().download(metadata.getPathLower())
                            .download(outputStream);

                    Log.d("", "after download");
                }

                // Tell android about the file
                Intent intent = new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE);
                intent.setData(Uri.fromFile(file));
                mContext.sendBroadcast(intent);

                Log.d("", "after intent call");

                downloadedFiles.add(file);
            } catch (DbxException | IOException e) {
                mException = e;
            }
        }

        return downloadedFiles;
    }
}
