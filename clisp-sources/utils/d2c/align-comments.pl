#!/usr/bin/perl -w

# Tidy up lines with consecutive end-of-line comments.

use strict;

my @line_buffer = ();

while (<>) {
  process_line($_);
}
dump_line_buffer();


sub process_line {
  my ($line) = @_;

  if ($line =~ m!^(.*?\*/)(/\*.*$)!) {
    process_line("$1\n");
    process_line(" $2\n");
    return;
  }

  if ($line =~ m!^.*\S.*/\*.*\*/!) {
    push @line_buffer, $line;
  } else {
    dump_line_buffer();
    print $line;
  }
}


sub dump_line_buffer {
  if (@line_buffer) {
    if ($#line_buffer == 0) {
      print $line_buffer[0];
    } else {
      my @parts_buffer = ();
      my @max = (0, 0);
      foreach my $line (@line_buffer) {
        my @parts = $line =~ m!^(.*?)\s*/\*\s*(.*?)\s*\*/\s*(.*?)\s*$!;
        $parts[1] = "/* $parts[1] */";
        push @parts_buffer, \@parts;
        my @l = map { length } @parts;
        for (my $i = 0; $i <= $#max; $i++) {
          $max[$i] < $l[$i] and $max[$i] = $l[$i];
        }
      }
      foreach my $line (@parts_buffer) {
        if ($line->[2]) {
          printf("%-*s %-*s %s\n", $max[0], $line->[0], $max[1], $line->[1], $line->[2]);
        } else {
          printf("%-*s %s\n", $max[0], $line->[0], $line->[1]);
        }
      }
    }
    @line_buffer = ();
  }
}

__END__
