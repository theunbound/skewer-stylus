skewer-stylus.el
================

Emacs minor mode allowing [Stylus](https://stylus-lang.com)
manipulation through
[skewer-mode](https://github.com/skeeto/skewer-mode).

Intended to be used in place of `skewer-css-mode`

Stylus code is passed through the `stylus` command, which must be
present on `exec-path`, and then to the `skewer` machinery through
`skewer-css`. 

Inspired by [skewer-less](https://github.com/purcell/skewer-less) and
[bling](https://bling.github.io/blog/2014/01/21/asynchronous-eval-with-stylus-and-skewer/).

Installation
------------

Add the directory containing `skewer-stylus.el` to your `load-path`,
and then `(require 'skewer-stylus-mode)`.

Usage
-----

Enable in the current buffer by saying

<kbd>M-x</kbd> `skewer-stylus-mode`

The following keyboard commands are defined by default:
- <kbd>C-x C-e</kbd>: `(skewer-stylus-send-declaration)` Isolate the
  declaration that the point is located within and all selectors that
  apply to it, evaluate that, and send it though skewer. 
- <kbd>C-M-x</kbd>: `(skewer-stylus-send-rule)` Isolate the rule that
  the point is currently located within and all selectors that apply
  to it, evaluate that, and send them though skewer. 
- <kbd>C-x C-k</kbd>: `(skewer-stylus-eval-block)` Evaluate the entire
  current stylus code block (by default delimited by html `<style>`
  tags), or the entire buffer, and send that through skewer. 

The delimiters for Stylus code blocks can be `customize`-d with the
variables `skewer-stylus-block-start-regexps` and
`skewer-stylus-block-end-regexps`.
