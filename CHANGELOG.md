# Changelog

 ## 0.4.0
- **[BREAKING]** Include line breaks every 60 characters in base64 encoded strings to mirror what the actual GitHub API does
- Add support for Git blobs endpoints (https://docs.github.com/en/rest/git/blobs?apiVersion=2022-11-28#get-a-blob)

## 0.3.0
- Correctly handle binary files in create-blob! and get-blob operations
- Fix reflective accesses in clj-github-mock.impl.jgit
- Remove base64-clj dependency 
- Bump dependencies
  - org.eclipse.jgit/org.eclipse.jgit from 5.11.0 to 6.10.0
  - metosin/reitit-ring from 0.5.13 to 0.7.2
  - datascript from 1.1.0 to 1.7.3

## 0.2.0
- Bump some libs

## 0.1.0
- Initial version implementing most of github database api (https://docs.github.com/en/rest/reference/git)
