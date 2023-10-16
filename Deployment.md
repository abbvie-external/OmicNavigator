# Deployment instructions

You may follow these instructions to deploy OmicNavigator with Docker. All you need to start is the [DockerDeploy](https://github.com/abbvie-external/OmicNavigator/blob/main/DockerDeploy) dockerfile within this repository.

## Create image and start the container

The `DockerDeploy` file contains instructions for creating an OmicNavigator (latest release) docker image from the opencpu/base:v2.2.11-2 image available from [dockerhub](https://hub.docker.com/r/opencpu/base/tags). It also does a few other things, including  adding our [OmicNavigator demo study](https://github.com/abbvie-external/OmicNavigatorExample) called `RNAseq123`. Execute the following code to create the image and start a container:

```sh
## create the image
docker build -t ondeploy -f DockerDeploy . 
## start the container
docker run --name onappdeploy -t -d -p 8004:8004 ondeploy
```

You can now open the app in your browser at http://localhost:8004/ocpu/library/OmicNavigator/
and select `RNAseq123` from the study drop-down menu.

## Add your custom package to the container

Your custom OmicNavigator studies (created by a call to `OmicNavigator::exportStudy(study=yourstudy, type='tarball')`) may be installed  via the following approach:

First, copy your study tarball into the container and start a shell:

```sh
## copy to container 
docker cp YourOmicNavigatorStudy.tar.gz onappdeploy:/

## shell in
docker exec -it onappdeploy /bin/sh
```

After shelling in, you can run `R CMD INSTALL` on your ONstudy tarball, or you can start an interactive R session and install your OmicNavigator study bundle. For example, after starting an R session you can execute the following using the 'pak' package:

```R
pak::pkg_install('./YourOmicNavigatorStudy.tar.gz', dependencies=T)

```

You can navigate to http://localhost:8004/ocpu/library/OmicNavigator/ and select your newly installed study from the dropdown menu to verify installation.

## Expose your container

Procedures for the exposure of an OmicNavigator container to network traffic depend on the hosting environment and/or cloud provider. The usual server administration and operation considerations apply, such as security, port configuration, resource allocation, etc. 
