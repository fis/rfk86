#! /usr/bin/env perl

# messages.pl -- convert rfk messages to rfk86 data

use strict;
use warnings;

my $verbose = shift @ARGV;

# define the rfk86 alphabet (to save space in font)

my $alpha = " ABCDEFGHIJKLMNOPQRSTUVWXYZ.,'\"!";
$alpha .=   "-abcdefghijklmnopqrstuvwxyz:;/\$?";
$alpha .=   "0123456789()";

my @alpha = split //, $alpha;

my %alpha;
$alpha{$alpha[$_]} = $_ foreach (0 .. $#alpha);

# slurp in all the messages

open IN, '<', 'messages.txt' or die "can't read messages.txt: $!";
my @messages = <IN>;
close IN;
s/\n$// foreach @messages;

# write out index and data in new alphabet

open OUT, '>:raw', 'messages.bin' or die "can't write messages.dat: $!";

my $bytes = 0;

print OUT pack('C*', map { length($_) } @messages);

foreach my $msg (@messages)
{
    print OUT pack('C*', map { $alpha{$_} } split //, $msg);
    $bytes += length($msg);
}

close OUT;

# write the output z80asm-compatible inc files

open OUT, '>', 'messages.inc' or die "can't write messages.inc: $!";

print OUT "messages_count: equ ", scalar @messages, "\n";
print OUT "messages_bytes: equ $bytes\n";

close OUT;
