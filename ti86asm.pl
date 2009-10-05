#! /usr/bin/env perl

# ti86asm.pl: assemble a 86p program

# Copyright (c) 2009, Heikki Kallasjoki.
# All rights reserved.
# License terms: see README.txt, "Legal Disclaimer".

use strict;
use warnings;

# Parse arguments.

my ($inf, $progf, $progn, $outf, $symf) = @ARGV;
$inf or die "usage: ti86asm.pl input [progfile [progname [binout [symbols]]]]";
$progf ||= $inf;
$progf =~ s/(\.\w+)?$/.86p/;
$progn ||= $inf;
$progn =~ s/^(\w{1,8}).*/$1/;
$outf ||= $inf;
$outf =~ s/(\.\w+)?$/.bin/;
$symf ||= $inf;
$symf =~ s/(\.\w+)?$/.sym/;

# Assemble the program.

system("z80asm -o $outf -I ../inc $inf");
exit 0 if $?;

# Produce the symbol file for the program.

my $loadbytes;

open SYMIN, '-|', "z80asm -L -I ../inc -o /dev/null $inf 2>&1"
    or die "can't run z80asm for symbols: $!";
open SYMOUT, '>', $symf
    or die "can't open $symf: $!";

while (<SYMIN>) {
    chomp;
    next unless m/^(\w+):\s+equ\s+\$([a-z0-f]+)$/;
    print SYMOUT "$1 = $2\n";
    $loadbytes = hex($2) if $1 eq 'LOAD_SIZE';
}

close SYMIN;
close SYMOUT;

# Convert the .bin to .86p.

my $bytes = -s $outf;
$loadbytes ||= $bytes;

open PROG, '>:raw', $progf
    or die "can't write to $progf: $!";
open BYTES, '<:raw', $outf
    or die "can't read from $outf: $!";

my $checksum = 0;
my $buf;

# File header.
print PROG pack('a[8] C[3] A[42]', "**TI86**", 0x1a, 0x0a, 0x00, "$inf [compiled]");
print PROG pack('v', 16+4+$bytes);
# Variable header for a compiled assembly program.
$buf = pack('vv CC A[8] v', 12, 4+$bytes, 0x12, length($progn), $progn, 4+$bytes);
$checksum += $_ foreach unpack('C*', $buf);
print PROG $buf;
# Actual variable contents.
$buf = pack('v CC', 2+$loadbytes, 0x8e, 0x28);
$checksum += $_ foreach unpack('C*', $buf);
print PROG $buf;
# Data.
while (read(BYTES, $buf, 1024) > 0) {
    $checksum += $_ foreach unpack('C*', $buf);
    print PROG $buf;
}
# Checksum.
print PROG pack('v', $checksum&0xffff);

close PROG;
close BYTES;
