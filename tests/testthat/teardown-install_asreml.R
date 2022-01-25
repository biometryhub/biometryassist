# Delete file if it exists
if(length(list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", recursive = TRUE))>0) {
    file.remove(list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", recursive = TRUE))
}
