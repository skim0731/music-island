package com.sehwankim.musicisland.CloudConnection.GoogleDriveConnection;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import android.util.Log;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.drive.DriveApi;
import com.google.android.gms.drive.DriveContents;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.drive.DriveFolder;
import com.google.android.gms.drive.Metadata;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

/**
 * Created by kimsehwan on 2016. 4. 27..
 */
public class GoogleDriveAccount{
    private final String TAG = "GoogleDriveAccount";
    private static GoogleDriveAccount instance;
    GoogleApiClient client;

    /**
     * Constructor of class.
     * This function is not public.
     */
    protected GoogleDriveAccount (GoogleApiClient client) {
        this.client = client;
    }

    public static void init(GoogleApiClient client) {
        instance = new GoogleDriveAccount(client);
    }

    public static GoogleDriveAccount getInstance() {
        if (instance == null) {
            Log.e("GoogleDriveAccount", "Client not initialized.");
        }

        return instance;
    }

    public void listFiles(ResultCallback callback) {
        DriveFolder appFolder = Drive.DriveApi.getAppFolder(instance.client);
        appFolder.listChildren(instance.client).setResultCallback(callback);
    }

    private void downloadFile(final Metadata metadata, final Context mContext) {
        DriveFile file = Drive.DriveApi.getFile(GoogleDriveAccount.instance.client, metadata.getDriveId());
        file.open(GoogleDriveAccount.getInstance().client, DriveFile.MODE_READ_ONLY, null)
                .setResultCallback(new ResultCallback() {
                    @Override
                    public void onResult(Result result) {
                        DriveContents contents = ((DriveApi.DriveContentsResult)result).getDriveContents();
                        InputStream in = contents.getInputStream();

                        // Set download path. (System's Music folder)
                        File path = Environment.getExternalStoragePublicDirectory(
                                Environment.DIRECTORY_MUSIC);
                        // File that we are going to write.
                        File file = new File(path, metadata.getTitle());

                        // Copy the file.
                        try (OutputStream out = new FileOutputStream(file)) {
                            byte[] buf = new byte[1024];
                            int len;
                            while ((len = in.read(buf)) > 0) {
                                out.write(buf, 0, len);
                            }
                            in.close();
                            out.close();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }

                        Intent intent = new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE);
                        intent.setData(Uri.fromFile(file));
                        mContext.sendBroadcast(intent);
                    }
                });
    }

    public void downloadFiles(List<Metadata> metadataList, Context mContext) {
        for (Metadata metadata : metadataList) {
            downloadFile(metadata, mContext);
        }
    }

}
