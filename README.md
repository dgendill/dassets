
Projects that are deployed across different channels often require one-off assets when they are deployed.  For example, the same game could be deployed on a website, on anroid, or as a facebook app.  As the number of channels increases, it becomes difficult to remember which project assets are meant to be copied to which channel.

# Project Goals

1. Define a configuration file schema for listing project assets and grouping them by name.

2. Provide a command line interface that will answer these questions.

  - What assets are required by a certain channel?
    assets --in [channel name]

  - Do all of the assets exist?  Where some files renamed or deleted?
    assets --in [channel name] --deleted

  - What assets are shared between channels?
    assets --shared [channel name] [channel name] ...

  - Are any shared assets missing?
    assets --shared [channel name] [channel name] --deleted

3. Enable installation via an global npm package.

# Install

npm install -g assets
