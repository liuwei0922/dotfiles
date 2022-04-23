#!/usr/bin/python3
import requests
import json

url = "http://10.2.5.251:801/eportal/"

param={'c':'Portal',
        'a':'login',
        'login_method':1,
        'user_account':'20195609@unicom',
        'user_password':193114,
        }

gt=requests.get(url,params=param)

js=json.loads(gt.text.replace(")","").replace("(",""))

if js.get('result') == '1':
    print('连接成功')
else :
    print('已连接')
