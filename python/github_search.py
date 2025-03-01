#!/usr/bin/env python3
import json
import sys
import os
import requests
import base64
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
        sys.stderr.write(f"Failed to get GitHub username: {response.status_code}\n")
        sys.stderr.write(response.text + "\n")
        return None

def search_github(keyword):
    """GitHubでコード、イシュー、PRを検索"""
    token = get_github_token()
    username = get_github_username(token)
    if not username:
        return []
    
    headers = {
        'Authorization': f'token {token}',
        'Accept': 'application/vnd.github.v3+json'
    }
    
    results = []
    
    # コード検索
    code_url = f'https://api.github.com/search/code?q={keyword}+user:{username}'
    code_response = requests.get(code_url, headers=headers)
    if code_response.status_code == 200:
        code_data = code_response.json()
        for item in code_data.get('items', []):
            repo_name = item['repository']['name']
            path = item['path']
            html_url = item['html_url']
            # コードのコンテンツを取得（オプション）
            content_url = item['url']
            content_response = requests.get(content_url, headers=headers)
            snippet = ""
            if content_response.status_code == 200:
                content_data = content_response.json()
                if content_data.get('encoding') == 'base64' and content_data.get('content'):
                    try:
                        decoded_content = base64.b64decode(content_data['content']).decode('utf-8')
                        lines = decoded_content.split('\n')
                        # マッチを含む行の前後数行を抽出（簡略化のため全て取得）
                        snippet = '\n'.join(lines[:5]) + ('...' if len(lines) > 5 else '')
                    except Exception as e:
                        sys.stderr.write(f"Error decoding content: {e}\n")
            
            results.append({
                'type': 'code',
                'repo': repo_name,
                'path': path,
                'html_url': html_url,
                'snippet': snippet,
                'title': f"{repo_name}: {path}"
            })
    else:
        sys.stderr.write(f"Code search failed: {code_response.status_code}\n")
        sys.stderr.write(code_response.text + "\n")
    
    # イシュー検索
    issue_url = f'https://api.github.com/search/issues?q={keyword}+type:issue+author:{username}'
    issue_response = requests.get(issue_url, headers=headers)
    if issue_response.status_code == 200:
        issue_data = issue_response.json()
        for item in issue_data.get('items', []):
            if 'pull_request' not in item:  # PRでないものだけ
                repo_url = item['repository_url']
                repo_name = repo_url.split('/')[-1]
                number = item['number']
                title = item['title']
                html_url = item['html_url']
                
                results.append({
                    'type': 'issue',
                    'repo': repo_name,
                    'number': number,
                    'title': title,
                    'html_url': html_url
                })
    else:
        sys.stderr.write(f"Issue search failed: {issue_response.status_code}\n")
        sys.stderr.write(issue_response.text + "\n")
    
    # PR検索
    pr_url = f'https://api.github.com/search/issues?q={keyword}+type:pr+author:{username}'
    pr_response = requests.get(pr_url, headers=headers)
    if pr_response.status_code == 200:
        pr_data = pr_response.json()
        for item in pr_data.get('items', []):
            if 'pull_request' in item:  # PRのみ
                repo_url = item['repository_url']
                repo_name = repo_url.split('/')[-1]
                number = item['number']
                title = item['title']
                html_url = item['html_url']
                
                results.append({
                    'type': 'pr',
                    'repo': repo_name,
                    'number': number,
                    'title': title,
                    'html_url': html_url
                })
    else:
        sys.stderr.write(f"PR search failed: {pr_response.status_code}\n")
        sys.stderr.write(pr_response.text + "\n")
    
    return results

if __name__ == "__main__":
    if len(sys.argv) > 1:
        keyword = sys.argv[1]
        results = search_github(keyword)
        print(json.dumps(results, ensure_ascii=False))
    else:
        sys.stderr.write("Usage: python github_search.py <search_keyword>\n")
