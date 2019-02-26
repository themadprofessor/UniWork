#!/usr/bin/env bash
domains="www.google.com www.reddit.com www.disroot.org stackoverflow.com moodle.gla.ac.uk www.tldp.com www.gnu.org unix.stackexchange.com 4chan.org github.com gitlab.com facebook.com en.wikibooks.org mariadb.com js.masterpassword.app iran.gov.ir www.president.ir www.gov.cn www.korea-dpr.com www.fcc.gov www.ipv6tf.org"

parse_trace () {
	traceroute -q 1 -n $1 | sed '/\*/d' | sed '/to/d' | awk 'NR>1{print "\"" f "\" -- \"" $2 "\""} {f=$2}'
}
export -f parse_trace

{ echo "graph routertopology {" && ./dnslookup $domains | awk '{print $3}' | sed '/:/d' | xargs -d '\n' -I "%" -n 1 bash -c 'parse_trace "%"' | sort | uniq && echo "}"; } > router-4.dot
{ echo "graph routertopology {" && ./dnslookup $domains | awk '{print $3}' | sed '/\./d' | xargs -d '\n' -I "%" -n 1 bash -c 'parse_trace "%"' | sort | uniq && echo "}"; } > router-6.dot
dot -Tpdf router-4.dot -o router-topology-v4.pdf
dot -Tpdf router-6.dot -o router-topology-v6.pdf
