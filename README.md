# Application template

Starting point for an application project. It is not a framework but a source template to be
adjusted to the requirements at hand.
It features:

- install signal handlers for process termination or user defined signal implementation
- termination signals are transformed to asynchronous exceptions to terminate user
  threads. An example of masking this exception for an uninterruptable operation is given
- log to file or console with different log levels
- configuration (ini format) and command line parsing
- start asynchronous event handlers which implement the application
- (side) effects like logging or state manipulation are factored out by class constraints, the
  application functions run in monads without general IO privileges

## Code design

The code is organized in three layers:

- 1. Startup layer
    - build application's environment `Env` from effect implementations
    - run application in `type App = ReaderT Env IO`
    - located in `src/App/Impl.hs` and `test/Tests/AppSpec.hs`
- 2. Effect layer
    - type classes for effects like logging, state and threading used in the application
      located in `src/Effect`
    - implemenation of theses effects located in `src/Effect/XXX/Impl.hs`
- 3. Application layer
    - only pure code or monadic code with access to effects only by type class constraints
    - located in `src`

## Haskell techniques

- make use of class constraints for IO effects, do not use `MonadIO m` in layer 3, e.g.

    `poll :: (Log.LogM m, State.StateM m, Thread.ThreadM m) => TL.Text -> m ()`

- to provide an implemention for effect `X`, make it an instance of `Effect.X.Impl.HasX`, e.g.
  define `type App = ReaderT Env IO` and `instance Log.Impl.HasLog App`, where `Env` contains
  the logging implementing function. By splitting the effect `Log` in the type classes `LogM`
  (part of `Effect.Log`) and `HasLog` (part of `Effect.Log.Impl`) the effect implementation
  does not need to have a dependency to `App` (which may contain also other effect implementations).

- transform `IO a -> m a` with `liftIO`, e.g.

    ```
    f :: ReaderT env IO ()
    f = liftIO $ putStrLn "hello"
    ```

- transform `m a -> IO a` with `runReaderT`, e.g.

    ```
    handler :: ReaderT env IO a
    runReaderT handler env :: IO a
    ```

- use `control` from `MonadBaseControl` to provide handler callbacks that need to run in IO, e.g.

    ```
    f, handler :: ReaderT env IO ()
    f = control $ \runInIO -> forkIO (runInIO handler)
    ```

Why all the hassle? We like to adhere to the principle of least priviledges (PolP), which means
not to give a functions more privileges than it absolutely needs. Programming in `IO` or
`MonadIO` gives a function the privilege to do its work _and_ to launch missiles additionally. When
we reason about code, especially when we outsource parts of our implementation, it is important
to guarantee that not only the code fullfills the requirements (what we _can_ test), but also
that is does nothing more (what we can _not_ test). When for example we outsource the code to decrypt
content, we want to guarantee that the code does not make persistent copies by writing to an `IORef`
variable which another thread reads out and sends it to the russians.

By moving effects to type class constraints and coding in layer 3 only under these constraints, we
only have to check the effect implementations to be benign.

Additionally our code is easier to test by implementing mocked effects.

## Generate html docs

- generate docs with `haddock.sh` (see output for path to index.html)
- generate `README.html` with `readme.sh` (needs the `pandoc` tool to be installed)

## Generate module dependency graphs

- generate `src.png` and `test.png` with `dep.sh` (needs the `graphmod` tool)

## Generate code coverage

- run `coverage.sh`

## Module dependency graphs

- Source dependencies ![](src.png)
- Test code dependencies (prunded edges) ![](test.png)
