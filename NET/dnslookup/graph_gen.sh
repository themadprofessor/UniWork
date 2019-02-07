#!/usr/bin/env bash
domains="www.google.com www.reddit.com www.disroot.org stackoverflow.com moodle.gla.ac.uk www.tldp.com www.gnu.org unix.stackexchange.com 4chan.org github.com gitlab.com facebook.com en.wikibooks.org mariadb.com js.masterpassword.app"

echo "graph routertopology {" && cmake-build-debug/dnslookup $domains | awk '{print $3}' | xargs -d '\n' -n 1 traceroute -4 -q 1 -n | sed '/\*/d' | sed '/to/d' | awk 'NR>1{print "\"" f "\" -- \"" $2 "\""} {f=$2}' | sort | uniq && echo "}"
