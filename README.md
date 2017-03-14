# hugo

Edit hugo with emacs  

## Image

M-x hugo-publish  

![emacs-hugo1](image/image1.png)

## Requirements

- Emacs 24 or higher
- hugo 0.16 or higher

## Sample Configuration

    (setq hugo-base-dir "~/src/github.com/masasam/blog/")
	(setq hugo-domain "blogdomain")
	(setq hugo-root "/home/blog/")

If publishing with GitHub Pages, set the theme as follows  

	(setq hugo-theme-of-github-page 'themename)

~/.ssh/config  

	 Host blogdomain
                        HostName "your server's ip address"
                        User "your ssh login user"

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
