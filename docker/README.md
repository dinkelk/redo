# docker

The Dockerfile in this directory contains an environment with `redo`, `redo-ifchange`, etc. already installed at `/root/redo/bin`, if you want to quickly try it out. The container also includes the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), so you can make modifications and recompile `redo` easily.

To create the container, first install [Docker Desktop](https://www.docker.com/products/docker-desktop/), then run:

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

and the image can be recreated from scratch by running:
  
  ```
  $ ./build_image.sh
  ```

Enjoy.
