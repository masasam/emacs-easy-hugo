# easy-hugo

Emacs package for writing blogs made with hugo  

## Screenshot

    M-x easy-hugo-newpost

You can post a new article  

![easy-hugo1](image/easy-hugo1.png)

Hit a blog file name  

![easy-hugo2](image/easy-hugo2.png)

A markdown file is automatically generated  

![easy-hugo3](image/easy-hugo3.png)

You can write a blog with markdown  

![easy-hugo4](image/easy-hugo4.png)

    M-x easy-hugo-preview  

![easy-hugo5](image/easy-hugo5.png)

The browser opens automatically and you can preview the blog on your PC  
Since the process of hugo running in the PC disappears in 300 seconds,  
you do not have to worry  
Even if you run the easy-hugo-preview command many times,  
only one hugo process will run so do not mind it  

![easy-hugo6](image/easy-hugo6.png)

Let's add another article  

![easy-hugo7](image/easy-hugo7.png)

Woo  

![easy-hugo8](image/easy-hugo8.png)

    M-x easy-hugo-publish  

You can publish your blog to the server  

![easy-hugo9](image/easy-hugo9.png)

    M-x easy-hugo-article

![easy-hugo10](image/easy-hugo10.png)

Display the list of articles you wrote  

![easy-hugo11](image/easy-hugo11.png)

Well then it will be completed in one

![easy-hugo12](image/easy-hugo12.png)

pen  

![easy-hugo13](image/easy-hugo13.png)

Blogs with the same file name can not be made  

![easy-hugo14](image/easy-hugo14.png)

Rename PPA blog  

![easy-hugo15](image/easy-hugo15png)

## Requirements

- Emacs 24.4 or higher
- hugo 0.19 or higher

## Sample Configuration

	(setq easy-hugo-basedir "~/hugo/")
	(setq easy-hugo-url "https://yourblogdomain")
	(setq easy-hugo-sshdomain "blogdomain")
	(setq easy-hugo-root "/home/blog/")
	(setq easy-hugo-previewtime "300")

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
