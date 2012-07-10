Install steps
-------------

1. User directory is: /Users/&lt;user&gt;.   &lt;top&gt; is a directory where you like to stash such things.

2. `cd ~/<top>`<br />
`git clone git://github.com/tinyprod/nesc.git`  (nesc/macports has the portfiles)

3. Edit `/opt/local/etc/macports/sources.conf` to include a line: file:///Users/&lt;user&gt;/&lt;top&gt;/nesc/macports

4. `sudo port install nesc`


**Changelog:**

* Jul  7, 2012: add nesc 1.3.4 to nesc repository
