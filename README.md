# confluence-reader

You can't open Confluence pages using
[EWW](https://www.gnu.org/software/emacs/manual/html_mono/eww.html) because it requires Javascript.
And sure, Confluence gives you the ability to create super-duper dynamic pages. But, and thank
heavens and all deities for that, most of the times the pages people create are just text and images.  

## Configuration 

    (use-package confluence-reader :load-path "/path/to/this/repo"
      :custom
      (confluence-host "thecompany.atlassian.net")
      (confluence-buffer-name-style 'page-title)
      :commands
      (confluence-search confluence-page-by-id confluence-page-from-url))
      
You need to create a token in JIRA and add it to any `auth-source` enabled location, for example in
your `.authinfo` or `.authinfo.gpg` file:

    machine thecompany.atlassian.net login youruser@thecompany.com password your-token-here
    
### Customization options

* `confluence-host`: The host of the Confluence server. Usually has the form
\"orgname.atlassian.net\". This name is also used to lookup credentials using `authsource'."

* `confluence-browser-url`: URL template to open a page in an external browser. You probably won't
need to change this.

* `confluence-buffer-name-style`: The page buffers are named "*Confluence ?*" where ? is affected by
  this setting. The default `page-id` makes buffer names shorters. With `page-title`, they are
  easier to find but can be very long.
  
### Usage

This package has three entry points:

* `confluence-search`: enter some text, get results, hit `RET` on any of them to display the page.
  By default it uses standard "text contains" search, invoke with prefix arg to type your own
  [CQL](https://developer.atlassian.com/cloud/confluence/advanced-searching-using-cql/) with
  advanced operators.

* `confluence-page-by-id`: if you have a page id (you can see them in the URL when on the browser)
  you can use this to jump directly to that page. It isn't very practical :) but it is used
  internally by the other commands.

* `confluence-page-from-url`: just paste the URL from your browser in the minibuffer and this will
  get the page id from it (using the very scientific method of splitting by "/" chars)

The page view uses `confluence-page-mode`, where you can see the page exported in glorious plain
HTML. Some of the main bindings in this mode are:  

* `q`, the usual "quit window" command
* `RET` over a link to open it. More about this below.
* `b` calls `bookmark-set`, to bookmark the current page in the standard Emacs bookmarking facility.
* `TAB` to move forward through the page's links (add shift to go backwards)
* `o` to open the page using `browse-url-secondary-browser-function` (in my case, Firefox)

When rendering a page, this package tries to identify links to other Confluence pages to handle
those internally. The internal links have a "➡️" character, while the ones handled by the usual
`browse-url` machinery have "🔗" next to them. Table of content/anchor links aren't supported, so
they get disabled (but still have an underline). You can also access all page headings using imenu.

