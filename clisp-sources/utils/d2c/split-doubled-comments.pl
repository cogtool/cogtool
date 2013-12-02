#!/usr/bin/perl -w

# Split two consecutive /**/-style comments onto separate lines.

use strict;

while (<>) {
  s!\*//\*!*/\n/*!g;
  print;
}

__END__

