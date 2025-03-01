#!/usr/bin/env python3
import json
import sys
import os
import requests
import base64
import concurrent.futures
from configparser import ConfigParser

# スクリプトのあるディレクトリ
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
CONFIG_PATH = os.path.join(SCRIPT_DIR, 'config.ini')

def get_github_token():
    """GitHubのアクセストークンを取得"""
    # 1. 環境変数からトークンを取得
    token = os.environ.get('GITHUB_TOKEN')
    if token:
        return token
    
    # 2. 設定ファイルからトークンを取得
    if os.path.exists(CONFIG_PATH):
        config = ConfigParser()
        config.read(CONFIG_PATH)
        if 'github' in config and 'token' in config['github']:
            return config['github']['token']
    
    # 3. トークンが見つからない場合はエラー
    sys.stderr.write("GitHub token not found. Please set GITHUB_TOKEN environment variable or add token to config.ini\n")
    sys.exit(1)

def get_github_username(token):
    """GitHub APIからユーザー名を取得"""
    headers = {
        'Authorization': f'token {token}',
        'Accept': 'application/vnd.github.v3+json'
    }
    response = requests.get('https://api.github.com/user', headers=headers)
    if response.status_code == 200:
        return response.json()['login']
    else:
        return None

def search_github_code(keyword, username, headers):
    """GitHubでコードを検索"""
    results = []
    code_url = f'https://api.github.com/search/code?q={keyword}+user:{username}'
    
    try:
        code_response = requests.get(code_url, headers=headers)
        if code_response.status_code == 200:
            code_data = code_response.json()
            
            # 並列でコードコンテンツを取得
            with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
                future_to_item = {
                    executor.submit(get_code_content, item, headers): item 
                    for item in code_data.get('items', [])
                }
                
                for future in concurrent.futures.as_completed(future_to_item):
                    item = future_to_item[future]
                    try:
                        snippet = future.result()
                        repo_name = item['repository']['name']
                        path = item['path']
                        html_url = item['html_url']
                        
                        results.append({
                            'type': 'code',
                            'repo': repo_name,
                            'path': path,
                            'html_url': html_url,
                            'snippet': snippet,
                            'title': f"{repo_name}: {path}"
                        })
                    except Exception as e:
                        pass
    except Exception as e:
        pass
        
    return results

def get_code_content(item, headers):
    """コードコンテンツを取得"""
    content_url = item['url']
    snippet = ""
    
    try:
        content_response = requests.get(content_url, headers=headers)
        if content_response.status_code == 200:
            content_data = content_response.json()
            if content_data.get('encoding') == 'base64' and content_data.get('content'):
                decoded_content = base64.b64decode(content_data['content']).decode('utf-8')
                lines = decoded_content.split('\n')
                snippet = '\n'.join(lines[:5]) + ('...' if len(lines) > 5 else '')
    except Exception:
        pass
        
    return snippet

def search_github_issues(keyword, username, headers, is_pr=False):
    """GitHubでイシューまたはPRを検索"""
    results = []
    type_query = "type:pr" if is_pr else "type:issue"
    type_name = "pr" if is_pr else "issue"
    
    url = f'https://api.github.com/search/issues?q={keyword}+{type_query}+author:{username}'
    
    try:
        response = requests.get(url, headers=headers)
        if response.status_code == 200:
            data = response.json()
            for item in data.get('items', []):
                # PRの場合はpull_requestが含まれる、イシューの場合は含まれない
                if ('pull_request' in item) == is_pr:
                    repo_url = item['repository_url']
                    repo_name = repo_url.split('/')[-1]
                    number = item['number']
                    title = item['title']
                    html_url = item['html_url']
                    
                    results.append({
                        'type': type_name,
                        'repo': repo_name,
                        'number': number,
                        'title': title,
                        'html_url': html_url
                    })
    except Exception as e:
        pass
        
    return results

def search_github(keyword):
    """GitHubでコード、イシュー、PRを並列検索"""
    token = get_github_token()
    username = get_github_username(token)
    if not username:
        return []
    
    headers = {
        'Authorization': f'token {token}',
        'Accept': 'application/vnd.github.v3+json'
    }
    
    # 並列で3種類の検索を実行
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
        code_future = executor.submit(search_github_code, keyword, username, headers)
        issue_future = executor.submit(search_github_issues, keyword, username, headers, False)
        pr_future = executor.submit(search_github_issues, keyword, username, headers, True)
        
        # 全ての結果を待機
        code_results = code_future.result()
        issue_results = issue_future.result()
        pr_results = pr_future.result()
    
    # 結果を統合
    return code_results + issue_results + pr_results

if __name__ == "__main__":
    if len(sys.argv) > 1:
        keyword = sys.argv[1]
        results = search_github(keyword)
        print(json.dumps(results, ensure_ascii=False))
    else:
        sys.stderr.write("Usage: python github_search.py <search_keyword>\n")
