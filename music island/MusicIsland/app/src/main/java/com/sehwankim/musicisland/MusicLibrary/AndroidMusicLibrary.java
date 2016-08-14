package com.sehwankim.musicisland.MusicLibrary;

import android.os.Environment;

import com.dropbox.core.v2.files.Metadata;
import com.google.android.gms.drive.MetadataBuffer;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by kimsehwan on 2016. 4. 12..
 */
public class AndroidMusicLibrary {
    List<String> musicList = new ArrayList<>();

    /**
     * Default constructor.
     * Get a list of files in Music folder, and construct a MusicLibrary
     */
    public AndroidMusicLibrary() {
        // System's music folder
        File musicFolder = Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_MUSIC);
        // Get a list of files.
        File[] listOfMusic = musicFolder.listFiles();
        // Only keep their names.
        for (File music : listOfMusic) {
            musicList.add(music.getName());
        }
    }

    public List<String> getMusicList() {
        return musicList;
    }

    /**
     * Get a list of music that is in the phone but not in Dropbox.
     * @param dropboxFileList
     * @return
     */
    public List<String> getToDeleteList_Dropbox(List<com.dropbox.core.v2.files.Metadata> dropboxFileList) {
        List<String> toDeleteList = new ArrayList<>();

        Boolean musicFound;
        // check if music files exist in Dropbox.
        for (String music : musicList) {
            musicFound = false;

            for (Metadata dropboxFile : dropboxFileList) {
                // if file is found, break.
                if (music.equals(dropboxFile.getName())) {
                    musicFound = true;
                    break;
                }
            }
            //if music was not found, add to the list.
            if (!musicFound) {
                toDeleteList.add(music);
            }
        }
        return toDeleteList;
    }

    /**
     * Get a list of music that is in Dropbox but not in phone.
     * @param dropboxFileList
     * @return
     */
    public List<Metadata> getToDownloadList_Dropbox(List<Metadata> dropboxFileList) {
        List<Metadata> toDownloadList = new ArrayList<>();

        Boolean musicFound;
        for (Metadata dropboxFile : dropboxFileList) {
            musicFound = false;
            for (String music : musicList) {
                if (dropboxFile.getName().equals(music)) {
                    musicFound = true;
                    break;
                }
            }

            if (!musicFound) {
                toDownloadList.add(dropboxFile);
            }
        }

        return toDownloadList;
    }

    /**
     * Get a list of music that is in Google Drive but not in phone.
     * @param driveFileList
     * @return
     */
    public List<String> getToDeleteList_GoogleDrive(MetadataBuffer driveFileList) {
        List<String> toDeleteList = new ArrayList<>();

        Boolean musicFound;
        // check if music files exist in Dropbox.
        for (String music : musicList) {
            musicFound = false;

            for (com.google.android.gms.drive.Metadata driveFile : driveFileList) {
                // if file is found, break.
                if (music.equals(driveFile.getTitle())) {
                    musicFound = true;
                    break;
                }
            }
            //if music was not found, add to the list.
            if (!musicFound) {
                toDeleteList.add(music);
            }
        }
        return toDeleteList;
    }

    /**
     * Get a list of music that is in Google Drive but not in phone.
     * @param driveFileList
     * @return
     */
    public List<com.google.android.gms.drive.Metadata> getToDownloadList_GoogleDrive(MetadataBuffer driveFileList) {
        List<com.google.android.gms.drive.Metadata> toDownloadList = new ArrayList<>();

        Boolean musicFound;
        for (com.google.android.gms.drive.Metadata driveFile : driveFileList) {
            musicFound = false;
            for (String music : musicList) {
                if (driveFile.getTitle().equals(music)) {
                    musicFound = true;
                    break;
                }
            }

            if (!musicFound) {
                toDownloadList.add(driveFile);
            }
        }

        return toDownloadList;
    }
}
