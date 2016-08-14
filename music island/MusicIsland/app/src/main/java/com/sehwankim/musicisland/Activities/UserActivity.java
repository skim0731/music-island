package com.sehwankim.musicisland.Activities;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.Button;

import com.dropbox.core.android.Auth;
import com.sehwankim.musicisland.CloudConnection.DropboxConnection.DropboxAccount;
import com.sehwankim.musicisland.R;

/**
 * This activity lets a user to login to his/her Dropbox account.
 * This is called when the app first starts.
 */
public class UserActivity extends AppCompatActivity{
    private final String TAG = "UserActivity";
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_user);


        Button dropboxLoginButton = (Button) findViewById(R.id.dropbox_login_button);
        dropboxLoginButton.setOnClickListener(new View.OnClickListener() {
            /**
             * Login to Dropbox using their authentication page. (WebView)
             * @param v
             */
            @Override
            public void onClick(View v) {
                Auth.startOAuth2Authentication(UserActivity.this, getString(R.string.app_key));
            }
        });

        Button googleLoginButton = (Button) findViewById(R.id.google_drive_login_button);
        googleLoginButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // proceed to DriveLoginActivity
                Intent intent = new Intent(UserActivity.this, DriveLoginActivity.class);
                startActivity(intent);
            }
        });
    }



    /**
     * This function is called after authentication is done.
     */
    @Override
    protected void onResume() {
        super.onResume();
        getAcessToken();
    }

    /**
     * If access token is available, proceed to MainActivity.
     * Otherwise, do nothing.
     */
    private void getAcessToken() {
        // Get Access Token
        String accessToken = Auth.getOAuth2Token();
        if (accessToken != null) {
            // initialize singleton DropboxAccount
            DropboxAccount.init(accessToken);
            // Store accessToken in SharedPreferences
            SharedPreferences prefs = getSharedPreferences("com.sehwankim.musicisland", Context.MODE_PRIVATE);
            prefs.edit().putString("access-token", accessToken).apply();
            // Set global variable for cloud type
            prefs.edit().putString("cloud_type", "Dropbox").apply();
            // Proceed to MainActivity
            Intent intent = new Intent(UserActivity.this, MainActivity.class);
            startActivity(intent);
        }
        else {
            Log.e("MainActivity", "null accessToken");
        }
    }
}
