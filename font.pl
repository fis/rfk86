#! /usr/bin/env perl

# font.pl: rfk86 tiny unreadable font, png -> binary

# Copyright (c) 2009, Heikki Kallasjoki.
# All rights reserved.
# License terms: see README.txt, "Legal Disclaimer".

# TI-86 screen is a monochrome thing; 128x64 pixels, with eight
# horizontal neighbor pixels packed into a single byte.  Our font
# characters are 4 pixels wide, so one nybble contains all the pixels
# of a single font row.  Since we always want to draw all the six
# rows, the font is stored in a "planar" format, consisting of six
# 38-byte blocks, each containing the corresponding row of 76
# characters.

use strict;
use warnings;

use GD;

my $pngfile = 'font.png';
my $binfile = 'font.bin';

my $chrcount = 78;

my $png = GD::Image->newFromPng($pngfile) or die "can't read: $pngfile";
my ($w, $h) = $png->getBounds;

$w % 8 == 0 or die "$pngfile: bad width: $w not a multiple of 8";
$h % 6 == 0 or die "$pngfile: bad height: $h not a multiple of 6";

my @planes = map { [] } (1 .. 6);

 LOOP: for (my $y = 0; $y < $h; $y += 6)
{
    for (my $x = 0; $x < $w; $x += 8)
    {
        # build one byte for each of the 6 planes

        foreach my $cy (0 .. $#planes)
        {
            my $byte = 0;

            foreach my $cx (0 .. 7)
            {
                my ($r, $g, $b) = $png->rgb($png->getPixel($x+$cx, $y+$cy));
                $byte <<= 1;
                $byte |= 1 if ($r+$g+$b) < 382;
            }

            push @{$planes[$cy]}, $byte;
        }

        $chrcount -= 2;
        last LOOP unless $chrcount;
    }
}

open BIN, '>:raw', $binfile or die "can't write: $binfile: $!";
print BIN pack('C*', @$_) foreach @planes;
close BIN;
