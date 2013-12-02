#!/usr/bin/perl -w

# After comment5 replaces #-style comments with normal C comments,
# this script finds consecutive lines of comments (each with their own
# set of /**/) and turns each chunk of comments into a single block
# comment.

use strict;

my @line_buffer = ();
my $indent = undef;
my $header_comment = 1;
my $add_to_header_comment = undef;

while (<>) {
  m!^#line 1 ".*?\.d"$! and next;
  if (m!^(\s*)/\*.*\*/\s*$!) {
    my $i = length($1);
    if (not defined $indent) {
      $indent = $i;
    }

    if ($i != $indent) {
      dump_line_buffer();
      $indent = $i;
    }

    push @line_buffer, $_;

  } else {
    dump_line_buffer();
    print;
  }
}
dump_line_buffer();


sub dump_line_buffer {
  if (@line_buffer) {
    if ($header_comment) {
      $add_to_header_comment and push @line_buffer, $add_to_header_comment;
      $header_comment = 0;
      tidy_comment(@line_buffer);
    } elsif ($#line_buffer == 0) {
      print $line_buffer[0];
    } else {
      compact_comment(@line_buffer);
    }
    @line_buffer = ();
    $indent = undef;
  }
}


sub compact_comment {
  my (@line_buffer) = @_;
  print " " x $indent;
  print "/*";
  for (my $i = 0; $i <= $#line_buffer; $i++) {
    my $line = $line_buffer[$i];
    my ($guts) = $line =~ m!/\*(.*?)\s*\*/!;
    unless ($i == 0) { print " " x $indent; }
    print "$guts";
    if ($i < $#line_buffer) { print "\n"; }
  }
  print " */\n";
}

sub tidy_comment {
  my (@line_buffer) = @_;
  print " " x $indent;
  print "/*\n";
  for (my $i = 0; $i <= $#line_buffer; $i++) {
    my $line = $line_buffer[$i];
    my ($guts) = $line =~ m!/\*(.*?)\s*\*/!;
    print " " x $indent;
    print " * $guts\n";
  }
  print " " x $indent;
  print " */\n";
}

__END__
