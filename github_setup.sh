#!/usr/bin/env bash
###############################################################################
#  github_setup.sh
#
#  This script documents EXACTLY how this folder (R_Replication) was turned
#  into a git repository and pushed to GitHub:
#      https://github.com/polojoecarr/fontagne-2022-replication
#
#  It is written so you can (a) understand each step, and (b) reproduce the
#  whole thing on another machine or for another project. It is safe to read
#  top-to-bottom; if you actually run it, run it from INSIDE the R_Replication
#  folder.
#
#  KEY DESIGN CHOICE -------------------------------------------------------
#  The git repository root is the R_Replication folder itself, NOT the parent
#  "Fontange 2022" folder. That parent folder holds tens of GB of source data
#  (BACI, Gravity, the 5,052 Sigma_HS6 files, PDFs). Because git only tracks
#  files at or below the repo root, scoping the repo to R_Replication means
#  that giant data is automatically and safely excluded -- it is never even
#  visible to git. This is the simplest way to avoid accidentally committing
#  multi-GB files (which GitHub would reject anyway; its per-file limit is
#  100 MB).
###############################################################################

set -e  # stop on the first error

# ----------------------------------------------------------------------------
# STEP 0.  Move into the folder that will BECOME the repository.
# ----------------------------------------------------------------------------
cd "C:/Claude Code Project Folder/Fontange 2022/R_Replication"

# ----------------------------------------------------------------------------
# STEP 1.  Create a brand-new, empty git repository, with the default branch
#          named "main" (the modern GitHub default).
#          `-b main` sets the initial branch name in one go.
# ----------------------------------------------------------------------------
git init -b main

# ----------------------------------------------------------------------------
# STEP 2.  Tell git who the author is. We set this LOCALLY (no --global), so it
#          only applies to THIS repository and does not touch your global git
#          settings. Change the name/email freely.
# ----------------------------------------------------------------------------
git config user.email "polojoecarr@gmail.com"
git config user.name  "Joseph Carr"

# ----------------------------------------------------------------------------
# STEP 3.  A .gitignore was created (see the .gitignore file in this folder).
#          It ignores R/IDE junk (.Rhistory, .RData, *.Rproj, ...) and, as a
#          safety net, common large-data patterns (*.dta, BACI_*/, Gravity_*/,
#          Replic_FGO/, *.xlsx) so no big dataset can ever be committed even if
#          one were copied into this folder by mistake.
# ----------------------------------------------------------------------------
# (nothing to run here -- the file already exists)

# ----------------------------------------------------------------------------
# STEP 4.  Stage every (non-ignored) file, then check what will be committed.
#          `git status --short` is worth a glance to confirm ONLY the R scripts,
#          small output CSVs, the figure and the READMEs are listed -- no data.
# ----------------------------------------------------------------------------
git add .
git status --short

# ----------------------------------------------------------------------------
# STEP 5.  Make the first commit. (The real commit message was multi-line and
#          listed the three R scripts; a short version is shown here.)
# ----------------------------------------------------------------------------
git commit -m "Replication of Fontagne, Guimbard & Orefice (2022) trade elasticities"

# ----------------------------------------------------------------------------
# STEP 6.  Point the local repo at the GitHub repository you created (empty,
#          no README/.gitignore/license). "origin" is the conventional name
#          for the main remote.
# ----------------------------------------------------------------------------
git remote add origin https://github.com/polojoecarr/fontagne-2022-replication.git

# ----------------------------------------------------------------------------
# STEP 7.  Push the "main" branch and set it to track "origin/main" (-u), so
#          future syncs are just `git push` / `git pull`.
#
#          AUTHENTICATION: the FIRST push opens a browser window (Git
#          Credential Manager) asking you to log in to GitHub and authorize.
#          This step is interactive and must be done by you at the keyboard --
#          it cannot be automated, which is why this was the one command you
#          ran yourself.
# ----------------------------------------------------------------------------
git push -u origin main

###############################################################################
#  AFTERWARDS -----------------------------------------------------------------
#
#  * Download on another machine:
#        git clone https://github.com/polojoecarr/fontagne-2022-replication.git
#
#  * Make more changes later, then publish them:
#        git add -A
#        git commit -m "describe what changed"
#        git push
#
#  * Remember the DATA is not in the repo (by design). On a new machine,
#    download BACI / MAcMap / Gravity from CEPII and point each script's
#    `project_root` variable at wherever you put them.
###############################################################################
