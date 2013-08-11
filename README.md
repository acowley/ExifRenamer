Command line utility for renaming image files based on
[EXIF](http://en.wikipedia.org/wiki/Exchangeable_image_file_format)
tags.

Usage:

    ExifRenamer [-r] [-e extension]* [-p prefix] [dir]
    -r           : recurse into subdirectories
    -e extension : include files with the given extension (default [jpg,JPG])
    -p prefix    : prefix output file names with "prefix"
    dir          : working directory (defaults to current directory)

Suppose you took a vacation in Portugal in May of 2012. You come home,
and you have a whole mess of images with names like
`IMG_0403.JPG`. You can use this program as such,

    > ExifRenamer -p Portugal_

It will find all files with the extension `.jpg` or `.JPG`, and give
them names like `Portugal_2012-05-28_14-57-32.jpg`. The date -- in
`Year-Month-Day` format -- and time -- in `Hour-Minute-Second` format
-- are obtained from the `DateTimeOriginal` EXIF tag in the image
file, and should correspond to the time when you originally took the
picture. The program keeps track of all the file names it generates,
and will give images taken within a second of each other unique names.

Before any image files are renamed, the program will give an example
of the renaming it is about to perform as well as a count of how many
files will be renamed. You will be asked to confirm that you want to
proceed with the renaming.
