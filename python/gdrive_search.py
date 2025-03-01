#!/usr/bin/env python3
import json
import sys
import os
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
import pickle

# スクリプトのあるディレクトリ
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
TOKEN_PATH = os.path.join(SCRIPT_DIR, 'token.pickle')
CREDENTIALS_PATH = os.path.join(SCRIPT_DIR, 'credentials.json')

def get_credentials():
    """Google APIの認証情報を取得"""
    SCOPES = ['https://www.googleapis.com/auth/drive.readonly']
    creds = None

    if os.path.exists(TOKEN_PATH):
        with open(TOKEN_PATH, 'rb') as token:
            creds = pickle.load(token)
    
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            if not os.path.exists(CREDENTIALS_PATH):
                sys.stderr.write(f"Error: credentials.json not found at {CREDENTIALS_PATH}\n")
                sys.exit(1)
                
            flow = InstalledAppFlow.from_client_secrets_file(CREDENTIALS_PATH, SCOPES)
            creds = flow.run_local_server(port=0)
        
        with open(TOKEN_PATH, 'wb') as token:
            pickle.dump(creds, token)
    
    return creds

def get_docs_with_keyword(keyword):
    """キーワードでGoogle Driveのドキュメントを検索"""
    try:
        creds = get_credentials()
        drive_service = build('drive', 'v3', credentials=creds)

        query = f"(name contains '{keyword}' or fullText contains '{keyword}') and trashed=false"

        response = drive_service.files().list(
            q=query,
            spaces='drive',
            fields='files(id, name, webViewLink)'
        ).execute()
        return response.get('files', [])
    except HttpError as error:
        sys.stderr.write(f"Google Drive API error: {error}\n")
        return []
    except Exception as e:
        sys.stderr.write(f"Unexpected error: {e}\n")
        return []

if __name__ == "__main__":
    if len(sys.argv) > 1:
        keyword = sys.argv[1]
        results = get_docs_with_keyword(keyword)
        print(json.dumps(results, ensure_ascii=False))
    else:
        sys.stderr.write("Usage: python gdrive_search.py <search_keyword>\n")
