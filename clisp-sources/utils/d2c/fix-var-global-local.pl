#!/usr/bin/perl -w

use strict;

while (<>) {
  /\s*\#\s*(define|undef)\s+(var|local|global)\b/ and next;
  s/\bvar\s+local\b/static/g;
  s/\blocal\s+var\b/static/g;
  s/\binline\s+local\b/inline static/g;
  s/\bvar\s+(\w+)\b/$1/g;
  s/^(\s*)(global|var) /$1/;
  s/^(\s*)local\b/$1static/;
  s/({\s*)var /$1/;
  s/nonreturning_function\s*\(global,/nonreturning_function(,/g;
  s/nonreturning_function\s*\(local,/nonreturning_function(static,/g;
  print;
}

__END__

