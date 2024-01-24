# docker

The Dockerfile in this directory contains an environment with `redo` already installed, if you want to quickly try it out. The container also includes the Haskell Stack, so you can make modifications and recompile `redo` easily.

To create the container, first install [Docker Desktop](https://www.docker.com/products/docker-desktop/) and run:

   ```
   $ ./create_container.sh
   ```

Once the container is built, you can log into the container by running.

   ```
   $ ./login_container.sh
   ```

`redo` will already be included in the build path.

The container can be started or stopped via:

  ```
  $ ./start_container.sh
  $ ./stop_container.sh
  ```

and recreated from scratch by running:
  
  ```
  $ ./build_image.sh
  ```

Enjoy.
