# Secrets

This directory holds encrypted key material used by CI, kept separate from the workflow
definitions in `.github/workflows/`.

## Contents

- `artma-bot-private-key.asc.gpg`: ArtmaBot's GPG signing key, symmetrically encrypted.
- `artma-bot-ssh-key.gpg`: ArtmaBot's SSH deploy key, symmetrically encrypted.

Both files are decrypted at CI runtime by
`.github/actions/composite/setupGitForArtmaBot/action.yaml`, using the `GPG_PASSPHRASE`
GitHub Actions secret.

## Rotation

1. Generate the new key material locally.
2. Encrypt it with `scripts/encrypt.sh <file>`, using a new passphrase.
3. Replace the corresponding `.gpg` file in this directory.
4. Update the `GPG_PASSPHRASE` secret in the repository settings.
5. Confirm the release workflow still decrypts and imports the key correctly.

Note: the passphrase itself is never committed; it lives only in the GitHub Actions
secret store.
