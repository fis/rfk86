#! /usr/bin/env perl

# image.pl: 4-level grayscale -> two .bin files (rfk86)

# TI-86 greyscale is the flickering thing; we assume frame 1 will be
# shown 2/3 of the time, frame 2 1/3 of the time; so 00 is white, 01
# is light gray, 10 is dark gray and 11 is black.

use strict;
use warnings;

use GD;

my $base = shift @ARGV or die "usage: $0 base; in base.png, out base-1.bin, base-2.bin";

my $png = GD::Image->newFromPng($base.'.png') or die "can't read: ${base}.png";
my ($w, $h) = $png->getBounds;

$w % 8 == 0 or die "$base: bad width: $w not a multiple of 8";

my @frame1;
my @frame2;

for (my $y = 0; $y < $h; $y++)
{
    for (my $x = 0; $x < $w; $x += 8)
    {
        # build one byte from 8 image pixels

        my ($byte1, $byte2) = (0, 0);

        foreach my $cx (0 .. 7)
        {
            my ($r, $g, $b) = $png->rgb($png->getPixel($x+$cx, $y));
            my $sum = $r+$g+$b;
            my $v = 3;
            $v = 2 if $sum > 128;
            $v = 1 if $sum > 384;
            $v = 0 if $sum > 640;
            $byte1 <<= 1;
            $byte2 <<= 1;
            $byte1 |= 1 if $v & 2;
            $byte2 |= 1 if $v & 1;
        }

        push @frame1, $byte1;
        push @frame2, $byte2;
    }
}

my @frames = (\@frame1, \@frame2);

foreach my $frame (1, 2)
{
    my $f = $base.'-'.$frame.'.bin';
    open BIN, '>:raw', $f or die "can't write: $f: $!";
    print BIN pack('C*', @{$frames[$frame-1]});
    close BIN;
}
