Requirements
============

  * A `Hackage`_ account
  * Membership in the `Package uploaders`_ group
  * Membership in the `magic-wormhole maintainers`_ group
  * Cabal, probably at least 2.2
  * hpack, probably at least 0.28.2

Steps
=====

  #. Make a release branch like ``release-$NEW_VERSION``
  #. Bump version in package.yaml according to `PVP`_
  #. Update any other `package.yaml metadata`_ that needs it
  #. Run hpack
  #. Commit changes
  #. Generate sdist (ignore the warning about running "configure")::
       cabal sdist

  #. Generate documentation::
       cabal new-haddock --builddir="dist" --haddock-for-hackage --haddock-option=--hyperlinked-source

  #. Perform candidate uploads::
       cabal upload --username <Hackage username> --password <Hackage password> dist/magic-wormhole-$NEW_VERSION.tar.gz
       cabal upload --documentation --username <Hackage username> --password <Hackage password> dist/magic-wormhole-$NEW_VERSION-docs.tar.gz

  #. Check things out on Hackage: https://hackage.haskell.org/package/magic-wormhole-$NEW_VERSION/candidate

  #. Perform real uploads (irreversible!) if things look good::
       cabal upload --publish --username <Hackage username> --password <Hackage password> dist/magic-wormhole-$NEW_VERSION.tar.gz
       cabal upload --publish --documentation --username <Hackage username> --password <Hackage password> dist/magic-wormhole-$NEW_VERSION-docs.tar.gz

  #. Tag the release on the release branch::
       git tag v$NEW_VERSION
       git push --tags

  #. Create a PR for the release branch and get it merged in to master.

.. _Hackage: https://hackage.haskell.org/
.. _PVP: https://pvp.haskell.org/
.. _Package uploaders: https://hackage.haskell.org/packages/uploaders/
.. _magic-wormhole maintainers: https://hackage.haskell.org/package/magic-wormhole/maintainers/
.. _package.yaml metadata: https://github.com/sol/hpack#quick-reference
