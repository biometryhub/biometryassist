# Automated Release System

This repository uses an automated release system via GitHub Actions that triggers whenever code is pushed to the `main` branch.

## How it works

The automated release workflow (`.github/workflows/release.yaml`) performs the following steps:

### 1. Version Check
- Extracts the package version from the `DESCRIPTION` file
- Compares it with the latest GitHub release tag
- Determines whether to create a new release or update an existing one

### 2. Package Building
If a release action is needed, the workflow builds the package for multiple platforms:
- **Source package**: `.tar.gz` file that can be built from source
- **Windows binary**: `.zip` file for Windows users
- **Linux binary**: `_R_x86_64-pc-linux-gnu.tar.gz` file for Linux users  
- **macOS binary**: `.tgz` file for macOS users

### 3. Release Management
- **New version**: If the version in `DESCRIPTION` differs from the latest release, creates a new GitHub release
- **Same version**: If the version is the same, updates the existing release with new binaries
- Extracts changelog content from `NEWS.md` for the release notes
- Uploads all package files as release assets

## Triggering a Release

### For a New Version
1. Update the `Version:` field in `DESCRIPTION` 
2. Add corresponding changelog entry in `NEWS.md` following the existing format:
   ```markdown
   # biometryassist x.y.z
   
   ## Bug Fixes / Major changes / Minor changes
   
   - Description of changes
   ```
3. Merge changes to the `main` branch
4. The workflow will automatically create a new release with the version tag `vx.y.z`

### For Updating Existing Release (same version)
1. Make changes to code without updating the version in `DESCRIPTION`
2. Merge changes to the `main` branch  
3. The workflow will rebuild the packages and update the existing release with new binaries

## Release Format

Each release includes:
- Release title: `vx.y.z` (e.g., `v1.3.1`)
- Release notes with changelog extracted from `NEWS.md`
- Package files:
  - `biometryassist_x.y.z.tar.gz` (source)
  - `biometryassist_x.y.z.zip` (Windows binary)
  - `biometryassist_x.y.z_R_x86_64-pc-linux-gnu.tar.gz` (Linux binary)
  - `biometryassist_x.y.z.tgz` (macOS binary)

## Monitoring

You can monitor the release process by:
1. Viewing the Actions tab in the GitHub repository
2. Looking for the "Automated Release" workflow runs
3. Checking the releases page for new releases

## Troubleshooting

If the release workflow fails:
1. Check the Actions tab for error details
2. Common issues:
   - Invalid version format in `DESCRIPTION` (must be `x.y.z`)
   - Missing changelog entry in `NEWS.md`
   - Package build failures due to dependencies or code issues
   - GitHub API rate limits or permissions issues

## Manual Override

If needed, releases can still be created manually through the GitHub web interface, but the automated system should handle most cases.
