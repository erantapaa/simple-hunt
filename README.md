The application will process the following Hackage files:

- http://hackage.haskell.org/packages/index.tar.gz
- http://hackage.haskell.org/packages/hoogle.tar.gz

and create the Hunt commands to populate the Hayoo database.

Usage:

The application expects the files `index.tar.gz` and `hoogle.tar.gz`
to be present in the working directory.

Just run the `simple-hunt` application with no arguments.

Output will be placed in the `json` directory.

