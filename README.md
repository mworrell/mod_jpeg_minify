mod_jpeg_minify
===============

Zotonic module to re-compress uploaded JPEGs to save file storage space.
The compression depends on the number of pixels (width * height).

 * Images smaller than 1000 pixels use quality 99.
 * Images greater than 250000 pixels use quality 50.
 * Any image in between uses a quality on a lineair scale between 50 and 99.

*Only works with Zotonic 0.10dev*
