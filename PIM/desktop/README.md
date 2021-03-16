# hoplon-skeleton

A [Hoplon][3] based web app to...

By default it includes a basic Codemirror-based editor and sidebar layout you're expected
to rip out.  It currently uses MUICSS for layout but that's strictly optional.

## Dependencies

- java 1.8+
- [boot][1]

## Usage
### Development
1. Start the `dev` task. In a terminal run:
    ```bash
    $ boot dev
    ```
    This will give you a Hoplon development setup with:
    - auto compilation on file changes
    - audible warning for compilation success or failures
    - auto reload the html page on changes
    - Clojurescript REPL

2. Go to [http://localhost:8000][2] in your browser. You should see a basic app appear.

3. If you edit and save a file, the task will recompile the code and reload the
   browser to show the updated version.

### Production
1. Run the `prod` task. In a terminal run:
    ```bash
    $ boot prod
    ```
2. The compiled files will be on the `target/` directory. This will use
   advanced compilation and prerender the html.

## License

Copyright © 2019, Rally Health

[1]: http://boot-clj.com
[2]: http://localhost:8000
[3]: http://hoplon.io
