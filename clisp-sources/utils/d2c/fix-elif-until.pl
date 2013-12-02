#!/usr/bin/perl -w

use strict;

$/ = undef;

my $text = <>;
$text =~ s/^\s*\#\s*define\s+(until|elif|loop)\b.*$//gm;
$text =~ s/(\s+)elif\b/$1else if/g;
$text =~ s/\bloop\b/while (1)/g;
$text =~ s/^(\#\s*)else if/$1elif/gm;

OUTER:
while ($text =~ m/until\s*/g) {
  my $pos    = pos $text;
  my $end    = length $text;
  my $parens = 0;

  for (my $i = $pos; $i < $end; $i++) {
    my $c = substr($text, $i, 1);
    if ($c eq '(') { $parens++; }
    if ($c eq ')') { $parens--; }
    if ($parens == 0) {
      substr($text, $i, 0, ")");
      substr($text, $pos, 0, "(!");
      $text =~ s/until(\s*)/while$1/;
      pos $text = $i + 3;
      next OUTER;
    }
  }
}

print $text;

__END__

