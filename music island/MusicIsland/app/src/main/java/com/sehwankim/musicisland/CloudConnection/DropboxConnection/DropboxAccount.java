package com.sehwankim.musicisland.CloudConnection.DropboxConnection;


import android.util.Log;

import com.dropbox.core.DbxRequestConfig;
import com.dropbox.core.v2.DbxClientV2;

/**
 * Created by kimsehwan on 2016. 4. 11..
 *
 * Singleton of DropboxClient.
 */
public class DropboxAccount {
    private static DropboxAccount instance;
    DbxRequestConfig config;
    DbxClientV2 client;

    /**
     * Constructor of class.
     * This function is not public.
     */
    protected DropboxAccount(String accessToken){
        config = new DbxRequestConfig("Music Island", "en_US");
        client = new DbxClientV2(config, accessToken);
    }

    public static void init(String access_token) {
        instance = new DropboxAccount(access_token);
    }

    public static DropboxAccount getInstance() {
        if (instance == null) {
            Log.e("DropboxAccount", "Client not initialized.");
        }

        return instance;
    }

    public DbxClientV2 getClient() {
        return client;
    }

}
