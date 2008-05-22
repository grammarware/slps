Hostname = shell.sourceforge.net
Username = rlaemmel
ProjectGroupDirectory = /home/groups/s/sl/slps
ProjectWebDirectory = ${ProjectGroupDirectory}/htdocs
ProjectWebCGIScriptDirectory = ${ProjectGroupDirectory}/cgi-bin

all:

upload:
	scp index.html ${Username}@${Hostname}:${ProjectWebDirectory}

clean:
	rm -f *~
