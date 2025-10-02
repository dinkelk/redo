# docker

The Dockerfile in this directory contains an environment with `redo`, `redo-ifchange`, etc. already installed at `/root/redo/bin`, if you want to quickly try it out. The container also includes the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), so you can make modifications and recompile `redo` easily.

To create the container, first install [Docker Desktop](https://www.docker.com/products/docker-desktop/), then run:

   ```
   $ ./env.sh start
   ```

Once the container is built, you can log into the container by running.

   ```
   $ ./env.sh login
   ```

`redo` will already be included in the build path.

The container can be started or stopped via:

  ```
  $ ./env.sh start
  $ ./env.sh stop
  ```

and the image can be recreated from scratch by running:
  
  ```
  $ ./env.sh build
  ```

Enjoy.
