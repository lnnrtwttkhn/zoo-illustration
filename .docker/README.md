# Docker

## Login (once)

```bash
docker login registry.git.mpib-berlin.mpg.de
```

## Edit Dockerfile

Edit the [Dockerfile](Dockerfile)

## Build and push container

```bash
docker build -t registry.git.mpib-berlin.mpg.de/wittkuhn/zoo-illustration/renv:latest .
docker push registry.git.mpib-berlin.mpg.de/wittkuhn/zoo-illustration/renv:latest
```
