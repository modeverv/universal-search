# Universal Search for Emacs

Unified search interface for local files, Google Drive documents, and GitHub repositories.

## Installation

### 1. Clone this repository:
```bash
git clone https://github.com/yourusername/universal-search ~/.emacs.d/universal-search
```

### 2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "~/.emacs.d/universal-search")
(require 'universal-search)
```

### 3. Install Python dependencies:
```bash
cd ~/.emacs.d/universal-search/python
pip install -r requirements.txt
```
### 4. Set up API credentials:
#### Google Drive API
- Visit Google Cloud Console: https://console.cloud.google.com/
- Create a new project (or select an existing one)
- Enable the Drive API
- Create OAuth credentials for a Desktop application
- Download the credentials and save as ~/.emacs.d/universal-search/python/credentials.json

#### GitHub API

For GitHub search, you need to set up a personal access token and save it in the configuration.

## Usage

M-x universal-search - Start searching across all sources
In the Helm interface:

RET (Return) to open the selected item
C-z to copy the link/path to clipboard
C-c C-o to view all available actions


## Customization
You can customize this package through M-x customize-group RET universal-search.

