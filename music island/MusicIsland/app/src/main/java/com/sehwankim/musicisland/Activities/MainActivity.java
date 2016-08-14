package com.sehwankim.musicisland.Activities;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import com.dropbox.core.v2.files.ListFolderResult;
import com.dropbox.core.v2.files.Metadata;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.drive.DriveApi;
import com.google.android.gms.drive.MetadataBuffer;
import com.sehwankim.musicisland.CloudConnection.DropboxConnection.DownloadFileTask;
import com.sehwankim.musicisland.CloudConnection.DropboxConnection.DropboxAccount;
import com.sehwankim.musicisland.CloudConnection.DropboxConnection.ListFilesTask;
import com.sehwankim.musicisland.CloudConnection.GoogleDriveConnection.GoogleDriveAccount;
import com.sehwankim.musicisland.ListViewAdapter.FileListAdapter;
import com.sehwankim.musicisland.MusicLibrary.AndroidMusicLibrary;
import com.sehwankim.musicisland.R;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * MainActivity. This activity is the first activity that is shown when the app starts.
 * Shows a list of files that need to be removed in red and a list of files that need to be downloaded in green.
 * When a 'sync button' is pressed, the app sync its phone's music folder to Dropbox folder.
 */
public class MainActivity extends AppCompatActivity implements SwipeRefreshLayout.OnRefreshListener {
    private ListView fileListView;
    private AndroidMusicLibrary musicLibrary = new AndroidMusicLibrary();
    private List<Metadata> toDownloadList_dropbox = new ArrayList<>();
    private List<com.google.android.gms.drive.Metadata> toDownloadList_googledrive = new ArrayList<>();
    private SwipeRefreshLayout filelistRefreshLayout;
    private List<String> toDeleteList = new ArrayList<>();
    private String cloudType;

    /**
     * This function is called when
     * @param savedInstanceState
     */
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Get Cloud Type.
        SharedPreferences prefs = getSharedPreferences("com.sehwankim.musicisland", Context.MODE_PRIVATE);
        cloudType = prefs.getString("cloud_type", "");

        fileListView = (ListView) findViewById(R.id.main_listView);

        // Setup SwipeRefreshLayout.
        filelistRefreshLayout = (SwipeRefreshLayout) findViewById(R.id.filelist_refresh_layout);
        filelistRefreshLayout.setOnRefreshListener(this);

        // Turn on the refreshing animation, so a user knows that data is loading.
        filelistRefreshLayout.setRefreshing(true);
        listFile();

        // Setup Sync Button.
        Button syncButton = (Button) findViewById(R.id.sync_button);
        syncButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                doSync();
            }
        });
    }

    /**
     * Refresh method for list view.
     */
    @Override
    public void onRefresh() {
        filelistRefreshLayout.setRefreshing(true);
        // update both phone library and Dropbox library info.
        musicLibrary = new AndroidMusicLibrary();
        listFile();
    }

    /**
     * Sync phone's music folder to Dropbox folder.
     * First, delete unwanted files.
     * Second, download new files from Dropbox.
     * Lastly, refresh the page.
     */
    private void doSync () {
        if (!toDeleteList.isEmpty()) {
            deleteUnwantedFiles();
        }
        if (cloudType.equals("Dropbox")){
            if (!toDownloadList_dropbox.isEmpty()) {
                downloadFile_dropbox(toDownloadList_dropbox);
            }
        }
        if (cloudType.equals("Google Drive")) {
            if (!toDownloadList_googledrive.isEmpty()) {
                downloadFile_googledrive(toDownloadList_googledrive);
            }
        }
    }

    /**
     * This function is called when the activity resumes.
     * When it resumes, simply refresh the data.
     */
    @Override
    protected void onResume() {
        super.onResume();
        // When the activity resumes, refresh the data and display.
        onRefresh();
    }

    /**
     * Asynchronously get a list of files in Dropbox.
     * Create ListFilesTask and execute.
     */
    private void listFile() {
        if(cloudType.equals("Dropbox")){
            new ListFilesTask(DropboxAccount.getInstance().getClient(), new ListFilesTask.Callback()
            {
                @Override
                public void onDataLoaded(ListFolderResult result) {
                    List<Metadata> dropboxFileList = result.getEntries();
                    // Determine files to download and files to delete.
                    toDownloadList_dropbox = musicLibrary.getToDownloadList_Dropbox(dropboxFileList);
                    toDeleteList = musicLibrary.getToDeleteList_Dropbox(dropboxFileList);
                    // Get file names from toDownload list.
                    List<String> toDownloadList = new ArrayList<>();
                    for (Metadata item : toDownloadList_dropbox) {
                        toDownloadList.add(item.getName());
                    }
                    // Display on listView
                    fileListView.setAdapter(new FileListAdapter(MainActivity.this, toDeleteList, toDownloadList));
                    // if refreshing animation is on, turn it off.
                    if(filelistRefreshLayout.isRefreshing()){
                        filelistRefreshLayout.setRefreshing(false);
                    }
                }

                @Override
                public void onError(Exception e) {
                    // Display a toast message.
                    Toast.makeText(MainActivity.this, "Error Occured.", Toast.LENGTH_LONG).show();
                    // if refreshing animation is on, turn it off.
                    if(filelistRefreshLayout.isRefreshing()){
                        filelistRefreshLayout.setRefreshing(false);
                    }
                }
            }).execute("");
        }
        else if (cloudType.equals("Google Drive")) {
            GoogleDriveAccount.getInstance().listFiles(new ResultCallback() {
                @Override
                public void onResult(Result result) {
                    MetadataBuffer driveFileList = ((DriveApi.MetadataBufferResult)result).getMetadataBuffer();
                    // Determine files to delete and files to download.
                    toDownloadList_googledrive = musicLibrary.getToDownloadList_GoogleDrive(driveFileList);
                    toDeleteList = musicLibrary.getToDeleteList_GoogleDrive(driveFileList);
                    // Get file names from toDownload list.
                    List<String> toDownloadList = new ArrayList<>();
                    for (com.google.android.gms.drive.Metadata item : toDownloadList_googledrive) {
                        toDownloadList.add(item.getTitle());
                    }
                    // Display on listView
                    fileListView.setAdapter(new FileListAdapter(MainActivity.this, toDeleteList, toDownloadList));
                    // if refreshing animation is on, turn it off.
                    if(filelistRefreshLayout.isRefreshing()){
                        filelistRefreshLayout.setRefreshing(false);
                    }
                }
            });
        }
    }

    /**
     * Asynchronously download files from Dropbox.
     * Create DownloadFileTask and execute.
     * @param fileList a list of metadata of files you want to download.
     */
    private void downloadFile_dropbox(List<Metadata> fileList) {
        new DownloadFileTask(MainActivity.this, DropboxAccount.getInstance().getClient(), new DownloadFileTask.Callback(){
            @Override
            public void onError(Exception e) {
                e.printStackTrace();
            }

            @Override
            public void onDownloadComplete(List<File> result) {
                Log.d("", "Downloaded");
                onRefresh();
            }
        }).execute(fileList);
    }

    private void downloadFile_googledrive(List<com.google.android.gms.drive.Metadata> fileList) {
        GoogleDriveAccount.getInstance().downloadFiles(fileList, getApplicationContext());
    }

    /**
     * Delete files in toDeleteList.
     * getToDeleteList is defined in AndroidMusicLibrary.
     */
    private void deleteUnwantedFiles() {
        // music folder
        File musicFolder = Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_MUSIC);

        // delete each file in toDeleteList.
        for (String fileName : toDeleteList) {
            File toDeleteFile = new File (musicFolder, fileName);
            toDeleteFile.delete();

            /* Tell android about the file
            * Without this line, Android doesn't know that the file is gone.
            * (So, what happens is that the file is deleted, but still visible in file manager.)
            * Thus, we should tell the system to update the file information.
            */
            Intent intent = new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE);
            intent.setData(Uri.fromFile(toDeleteFile));
            this.sendBroadcast(intent);
        }

        // refresh the file list
        onRefresh();
    }
}
