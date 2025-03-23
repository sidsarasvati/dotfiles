##############################################################################
#                                                                            #
#                         Utility Shell Functions                            #
#                                                                            #
#            A collection of useful functions to improve workflow            #
#                      and handle common operations                          #
#                                                                            #
##############################################################################

# === File and Archive Handling ===

#
# extract - Universal archive extraction
#
# Extracts many different archive types with a single command, 
# automatically detecting the archive type.
#
# Usage: extract <archive_file>
#
function extract() {
  if [ -f "$1" ] ; then
    case "$1" in
      *.tar.bz2)   tar xjf "$1"     ;; # Tar with bzip2 compression
      *.tar.gz)    tar xzf "$1"     ;; # Tar with gzip compression
      *.bz2)       bunzip2 "$1"     ;; # Bzip2 compression
      *.rar)       unrar e "$1"     ;; # RAR archive
      *.gz)        gunzip "$1"      ;; # Gzip compression
      *.tar)       tar xf "$1"      ;; # Tar archive
      *.tbz2)      tar xjf "$1"     ;; # Alternate extension for tar+bzip2
      *.tgz)       tar xzf "$1"     ;; # Alternate extension for tar+gzip
      *.zip)       unzip "$1"       ;; # Zip archive
      *.Z)         uncompress "$1"  ;; # Z compression
      *.7z)        7z x "$1"        ;; # 7zip archive
      *)           echo "'$1' cannot be extracted via extract()" ;; # Unknown format
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

#
# mkd - Create and enter directory
#
# A combination of mkdir and cd - creates a directory and 
# immediately changes into it.
#
# Usage: mkd <directory_name>
#
function mkd() {
  mkdir -p "$@" && cd "$_";
}

#
# cdf - Jump to Finder location
#
# macOS function that changes the terminal's current directory
# to match the frontmost Finder window's location.
#
# Usage: cdf
#
function cdf() {
  # Uses AppleScript to get the Finder's current location
  cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')";
}

#
# targz - Create .tar.gz archive
#
# Creates a compressed tar archive from a directory, 
# excluding .DS_Store files.
#
# Usage: targz <directory>
#
function targz() {
  local tmpFile="${1%/}.tar";
  # Create tar archive (excluding .DS_Store files)
  tar -cvf "${tmpFile}" --exclude=".DS_Store" "${1}" || return 1;
  # Compress with gzip
  gzip "${tmpFile}" || return 1;
  # Remove intermediate tar file
  rm "${tmpFile}" || return 1;
}

#
# fs - File/directory size
#
# Shows the size of a file or the total size of a directory,
# with human-readable output.
#
# Usage: fs [file_or_directory]
#
function fs() {
  # Determine the right du argument for the system
  if du -b /dev/null > /dev/null 2>&1; then
    local arg=-sbh;  # Linux style
  else
    local arg=-sh;   # BSD/macOS style
  fi
  
  # Show sizes for specified files or all files in current directory
  if [[ -n "$@" ]]; then
    du $arg -- "$@";
  else
    du $arg .[^.]* ./*;
  fi;
}

# === Development Tools ===

#
# diff - Enhanced diff using git's coloring
#
# Improves the standard diff by using git's colored diff when available.
# Works on any files, not just those in a git repository.
#
# Usage: diff <file1> <file2>
#
if hash git &>/dev/null; then
  function diff() {
    # Use git's colored diff for improved readability
    git diff --no-index --color-words "$@";
  }
fi;

#
# server - Quick HTTP server
#
# Starts a simple HTTP server in the current directory,
# making it accessible via web browser.
#
# Usage: server [port]
#
function server() {
  local port="${1:-8000}";  # Default port 8000 if not specified
  echo "Starting server at http://localhost:${port}/"
  python -m http.server "$port";
}

# === Security & Network Tools ===

#
# getcertnames - SSL certificate inspection
#
# Retrieves and displays the Common Name and Subject Alternative Names (SANs)
# from an SSL certificate on a website.
#
# Usage: getcertnames <domain>
#
function getcertnames() {
  if [ -z "${1}" ]; then
    echo "ERROR: No domain specified.";
    echo "Usage: getcertnames <domain>";
    return 1;
  fi;

  local domain="${1}";
  echo "Testing ${domain}…";
  echo ""; # newline

  # Connect to the domain and retrieve the certificate
  local tmp=$(echo -e "GET / HTTP/1.0\nEOT" \
    | openssl s_client -connect "${domain}:443" -servername "${domain}" 2>&1);

  if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
    # Extract relevant certificate information
    local certText=$(echo "${tmp}" \
      | openssl x509 -text -certopt "no_aux, no_header, no_issuer, no_pubkey, \
      no_serial, no_sigdump, no_signame, no_validity, no_version");
    
    # Display Common Name (CN)
    echo "Common Name:";
    echo ""; # newline
    echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//" | sed -e "s/\/emailAddress=.*//";
    echo ""; # newline
    
    # Display Subject Alternative Names (SANs)
    echo "Subject Alternative Name(s):";
    echo ""; # newline
    echo "${certText}" | grep -A 1 "Subject Alternative Name:" \
      | sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\n" | tail -n +2;
    return 0;
  else
    echo "ERROR: Certificate not found.";
    return 1;
  fi;
}