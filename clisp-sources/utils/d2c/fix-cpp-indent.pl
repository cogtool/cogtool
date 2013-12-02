#!/usr/bin/perl -w

use strict;

while (<>) {
  if (/^(\s*)\#(.*)$/) {
    print "#$1$2\n";
  } else {
    print;
  }
}

__END__
