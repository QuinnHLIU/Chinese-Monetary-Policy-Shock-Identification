# -*- coding: utf-8 -*-
"""
Created on Fri Aug 25 19:05:55 2022

@author: Haiqin (Quinn) Liu


A script to retrieve minute-level MP timing from PBOC website

input: event types

output: list of events with exact timing + textual description >> Announcement/*.xlsx

"""


import os
os.chdir(r"E:\Dropbox\UMP_Paper\Git/Announcement")




import requests  # 请求数据
import time      # 时间模块
import json      # json 模块，储存数据
import csv       # 保存数据

# 请求数据
def get_response(page, event):
    
    # conventional events 
    if event == "MPR":
        url = 'http://www.pbc.gov.cn/zhengcehuobisi/125207/125227/index.html'
    
    elif event == "LR":
        url = 'http://www.pbc.gov.cn/zhengcehuobisi/125207/125213/125440/125838/125888/index.html'
        
    elif event == "RRR":
        url = 'http://www.pbc.gov.cn/zhengcehuobisi/125207/125213/125434/125798/index.html'
        
        
    # innovative events 
    elif event == "MLF":
        url = 'http://www.pbc.gov.cn/zhengcehuobisi/125207/125213/125431/index.html'
        
    elif event == "LPR": # 9：15 AM after 2022; 9:30 AM before that
        url = 'http://www.pbc.gov.cn/zhengcehuobisi/125207/125213/125440/3876551/index.html'
   
   
   
    headers = {
        'accept': '*/*',
        'accept-encoding': 'gzip, deflate, br',
        'accept-language': 'zh-CN,zh;q=0.9,en;q=0.8',
        'referer': url,
        'sec-ch-ua': '" Not A;Brand";v="99", "Chromium";v="100", "Google Chrome";v="100"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': 'Windows',
        'sec-fetch-dest': 'script',
        'sec-fetch-mode': 'no-cors',
        'sec-fetch-site': 'same-site',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 '
                      '(KHTML, like Gecko) Chrome/100.0.4896.60 Safari/537.36'
        # may require private cookie
    }
  
    params = {
        # 'callback': 'jQuery17201888299578386794_' + str(round(time.time() * 1000)),
        # 'jsonp': 'jsonp',
        'next': page,  
        'type': 1,
        'oid': 715024588,  
        'mode': 3,  
        # 'plat': 1,
        # '_': str(round(time.time() * 1000))  # time stamp 
    }
    # 
    response = requests.get(url, headers=headers, params=params)
    return response



def retrieveTiming(response):
    """
    
    Parameters
    ----------
    response : request response object
        DESCRIPTION.

    Returns : events
    -------
    None.

    """
    response.encoding = 'utf-8'                  # 修改编码格式
    data_json = json.loads(response.text)        # 通过 json 解析数据
    event_list = data_json['data']['replies']  
    
    events = []                      
    for i in range(len(event_list)):  # iterate over each announcemnets 
        comment = {
            'id': event_list[i]['rpid'],   
            'time': time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(event_list[i]['ctime'])),  
            # release time; converted from time stamp 
            'title': event_list[i]['content']['title']
            'content': event_list[i]['content']['body']  # announcement content
            # see other entries in json file; e.g. attachment 
        }
        events.append(comment)  # 每页的评论数据
    return events
    


def save_data(comments, save_path):
    """

    Parameters
    ----------
    comments : TYPE
        DESCRIPTION.
    save_path : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """
    with open(save_path, 'a', newline='', encoding='utf-8') as fp:
        # 设置表头，即列名
        csv_header = ['id', 'time', 'parent', 'like', 'user_id', 'user_name', 'content']  
        csv_writer = csv.DictWriter(fp, csv_header)
        # 如果文件不存在，则写入表头；如果文件已经存在，则直接追加数据不再次写入表头
        if fp.tell() == 0:
            csv_writer.writeheader()  
        csv_writer.writerows(comments)  # 写入数据



def crawler(page, save_path):
    time.sleep(2)  # 暂停 2 秒，避免请求过于频繁
    response = get_response(page)    # 请求数据
    comments = parse_data(response)  # 解析数据
    save_data(comments, save_path)   # 储存数据
    print(f'成功爬取第{page+1}页')
    


if __name__ == '__main__':
    
    event = input("Enter event type:")
    save_file = event+'.csv'  # 保存路径
    total_counts = data_json['data']['cursor']['all_count']
   
    for p in range(total_counts//20 + 1):  #20 entries on each page 
        crawler(p, save_file)
