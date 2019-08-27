import os
import threading
import ast
import youtube_dl

def download_task(video, output_dir):

    ydl_opts = { 
        'outtmpl': f'{output_dir}/%(title)s.%(ext)s'
    }
    
    with youtube_dl.YoutubeDL(ydl_opts) as ydl:
        ydl.download([video])


if __name__ == "__main__":

    output_dir = "/Users/djm/Movies/cs_7642"

    with open('cs7642_dict.txt', 'r') as f:
        dict_string = f.read()
        playlist = ast.literal_eval(dict_string)
    
    threads = []
    for video in playlist['entries']:
        thread = threading.Thread(target=download_task, args=(video['webpage_url'], output_dir))
        threads.append(thread)

    #Actually start downloading
    for thread in threads:
        thread.start()

    #Wait for all the downloads to complete
    for thread in threads: 
        thread.join()
