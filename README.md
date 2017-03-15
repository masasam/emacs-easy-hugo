# easy-hugo

Package for writing blogs made with hugo just by emacs  

## Screenshot

M-x easy-hugo-publish  

![emacs-hugo1](image/image1.png)

## Requirements

- Emacs 24.4 or higher
- hugo 0.19 or higher

## Sample Configuration

    (setq easy-hugo-base-dir "~/src/github.com/masasam/blog/")
	(setq easy-hugo-domain "blogdomain")
	(setq easy-hugo-root "/home/blog/")

## Preparation for useing this package

Write at ~/.ssh/config  

	 Host blogdomain
                        HostName "Your server's IP"
                        User "Your server's ssh login username"

Because blog is generated under /home/blog/ on the server  
Set it to reference it with nginx.  
Ssl uses certbot  

sample nginx.conf  

	server {
		listen 80;
		server_name yourdomain;
		return  301 https://yourdomain$request_uri;
		location / {
			root /home/blog;
		index	index.html index.htm;
		}
	}
	server {
		listen 443;
		server_name yourdomain;
		ssl on;
		ssl_certificate      /etc/letsencrypt/live/yourdomain/fullchain.pem;
		ssl_certificate_key  /etc/letsencrypt/live/yourdomain/privkey.pem;

	location / {
		root    /home/blog;
	index   index.html index.htm;
		}
	}

Confirm that PATH passes from emacs to go  
For example  

	(setenv "GOPATH" "~/go")
	(add-to-list 'exec-path (expand-file-name "~/go/bin"))
