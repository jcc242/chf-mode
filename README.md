# chf-mode

This repository contains a Chombo Fortran mode for Emacs derived from Emacs' fortran-mode.

Chombo Fortran is a dialect of Fortran with a few extra keywords and control structures. 
These extra keywords and control structures are read by Chombo's Fortran preprocessor and converted into standard Fortran.

The indentation function is copied from fortran-mode, then modified with Chombo Fortran's new control structures.
As the bulk of the code in this mode is that indentation, I feel that the copyright should stay with the original fortran-mode authors and the license remain the same.
See the license at the top of chf.el for more information.

## Usage

Clone the repository to somewhere on your local machine.
Then, load the path and require the mode:

    ;; Load chf-mode
    (add-to-list 'load-path "/PATH/TO/CLONED/REPOSITORY/")
    (require 'chf)

Then, make files ending in .ChF open in chf-mode:

    ;; setup files ending in “.ChF” to open in chf-mode
    (add-to-list 'auto-mode-alist '("\\.ChF\\'" . chf-mode))

## License

Copyright stays with the creators of fortran-mode, and the license remains the same.
See the license at the top of chf.el for more information.

Copyright (C) 1986, 1993-1995, 1997-2016 Free Software Foundation, Inc.

Licensed under the [GNU General Public License, version 3](http://www.gnu.org/licenses/gpl-3.0.html), or (at your option) any later version.
