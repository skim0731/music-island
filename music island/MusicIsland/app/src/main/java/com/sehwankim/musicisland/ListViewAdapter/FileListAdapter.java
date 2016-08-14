package com.sehwankim.musicisland.ListViewAdapter;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.sehwankim.musicisland.R;

import java.util.List;

/**
 * Created by kimsehwan on 2016. 4. 11..
 *
 * A ListAdapter for MainActivity's ListView.
 * This ListView displays a list of files that need to be removed in red
 * and a list of files that need to be downloaded in green.
 *
 */
public class FileListAdapter extends BaseAdapter {
    private List<String> toDeleteList;
    private List<String> toDownloadList;
    private LayoutInflater inflater;

    /**
     * Constructor
     * @param context this is used to inflate the view.
     * @param toDeleteList a list of files that need to be removed
     * @param toDownloadList a list of files that need to be downloaded
     */
    public FileListAdapter(Context context, List<String> toDeleteList, List<String> toDownloadList) {
        this.toDeleteList = toDeleteList;
        this.toDownloadList = toDownloadList;
        this.inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    /**
     * Get a total number of items to be displayed in the ListView.
     * @return
     */
    @Override
    public int getCount() {
        return toDeleteList.size() + toDownloadList.size();
    }

    /**
     * Get an item at 'position'.
     * We are displaying toDeleteList first and then display toDownloadList.
     * @param position
     * @return
     */
    @Override
    public Object getItem(int position) {
        // toDeleteList goes on top.
        if (position < toDeleteList.size()) {
            return toDeleteList.get(position);
        }
        // and then toDownloadList.
        else {
            position -= toDeleteList.size();
            return toDownloadList.get(position);
        }
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    /**
     * Get a view for each row of the ListView.
     * @param position position of this rowView
     * @param convertView not used
     * @param parent not used
     * @return
     */
    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        // Get View object for each row in ListView.
        View rowView = inflater.inflate(R.layout.file_list_container, null);
        // Name to display.
        String musicName = (String) getItem(position);
        TextView fileTextView = (TextView) rowView.findViewById(R.id.filelist_name);
        fileTextView.setText(musicName);
        // if this file is to be removed, set text color red.
        if(position < toDeleteList.size()) {
            fileTextView.setTextColor(Color.RED);
        }
        // else, this file is to be downloaded, so set text color green.
        else {
            fileTextView.setTextColor(Color.parseColor("#005c05"));
        }

        return rowView;
    }
}
