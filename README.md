# Executable Environment for OSF Project [eg6w5](https://osf.io/eg6w5/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Social Decoys: Leveraging Choice Architecture to Alter Social Preferences

**Project Description:**
> Many of society's most significant social decisions are made over sets of individuals: for example, evaluating a collection of job candidates when making a hiring decision. Rational theories of choice dictate that decision makers' preferences between any two options should remain the same irrespective of the number or quality of other options. Yet people's preferences for each option in a choice set shift in predictable ways as function of the available alternatives. These violations are well documented in consumer behavior contexts: for example, the decoy effect, in which introducing a third inferior product changes consumers' preferences for two original products. The current experiments test the efficacy of social decoys and harness insights from computational models of decision-making to examine whether choice set construction can be used to change preferences in a hiring context. Across seven experiments (N = 6312) we find that participants have systematically different preferences for the exact same candidate as a function of the other candidates in the choice set (Experiments 1a-1d, 2) and the salience of the candidate attributes under consideration (Experiments 2, 3a, 3b). Specifically, compromise and (often) asymmetric-dominance decoys increased relative preference for their yoked candidates when candidates were counter-stereotypical (e.g., high warmth/low competence male candidate). More importantly, we demonstrate for the first time that we can mimic the effect of a decoy in the absence of a third candidate by manipulating participants' exposure to candidates' attributes: balanced exposure to candidates' warmth and competence information significantly reduced bias between the two candidates.

**Original OSF Page:** [https://osf.io/eg6w5/](https://osf.io/eg6w5/)

---

**Important Note:** The contents of the `eg6w5_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_eg6w5-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_eg6w5-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `eg6w5_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-eg6w5-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-eg6w5-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_eg6w5](https://github.com/code-inspect-binder/osf_eg6w5)

