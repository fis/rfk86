#! /usr/bin/env perl

# messages.pl -- convert rfk messages to rfk86 data

# Copyright (c) 2009, Heikki Kallasjoki.
# All rights reserved.
# License terms: see README.txt, "Legal Disclaimer".

# RFK86 data compression details
# ------------------------------
#
# We use here a simplified variant of DEFLATE compression.
#
# As we don't want to have to uncompress everything just to pick up 20
# NKO descriptions, each message is compressed as a single "block",
# with the LZ77 backreferences never going past the beginning of the
# block (so the output buffer can be used to resolve them).
# Additionally, all messages share a common Huffman code for the
# literal/length alphabet, as well as for the distance alphabet.
#
# The message alphabet has 76 different literals: 0 .. 75.  In
# addition, there's a special "end of message" token (76), and 22 (77
# .. 98) tokens (with some extra bits) denoting the backreference
# lengths: (this is from DEFLATE except with a switch)
#
#      Extra               Extra               Extra
# Code Bits Length(s) Code Bits Lengths   Code Bits Length(s)
# ---- ---- ------     ---- ---- -------   ---- ---- -------
#   77   0     3        87   1   15,16      97   4   67-82
#   78   0     4        88   1   17,18      98   4   83-98
#   79   0     5        89   2   19-22
#   80   0     6        90   2   23-26
#   81   0     7        91   2   27-30
#   82   0     8        92   2   31-34
#   83   0     9        93   3   35-42
#   84   0    10        94   3   43-50
#   85   1  11,12       95   3   51-58
#   86   1  13,14       96   3   59-66
#
# The distance alphabet has 13 symbols (0 .. 12) like this:
# (also from DEFLATE except truncated)
#
#      Extra           Extra
# Code Bits Dist  Code Bits   Dist
# ---- ---- ----  ---- ----  ------
#   0   0    1     10   4     33-48
#   1   0    2     11   4     49-64
#   2   0    3     12   5     65-96
#   3   0    4
#   4   1   5,6
#   5   1   7,8
#   6   2   9-12
#   7   2  13-16
#   8   3  17-24
#   9   3  25-32
#
# Raw Huffman tree storage: inner nodes are four bytes (pointer to
# left and right child), while child nodes are two (zero byte followed
# by the symbol).  Pointers to child nodes point at the zero byte,
# while pointers to inner nodes point at the second byte (high byte of
# left pointer); this is always nonzero, and it's simple to inc/dec
# once to access either pointer.
#
# Extra note: the Huffman tree for the distance alphabet takes up more
# space than the coding saves.  So we use a simplified distance code,
# where a zero bit means Dist=1 (i.e. RLE) and a one bit is followed
# by the four-bit binary encoding of the "Code" field of the above
# table.
#
# Binary table for the length codes:
#
# code-(N+1)     len-3
# ----------   -------
#      00000   0000000
#        ...       ...
#      00111   0000111
#      01000   000100x
#      01001   000101x
#      01010   000110x
#      01011   000111x
#      01100   00100xx
#      01101   00101xx
#      01110   00110xx
#      01111   00111xx
#      10000   0100xxx
#      10001   0101xxx
#      10010   0110xxx
#      10011   0111xxx
#      10100   100xxxx
#      10101   101xxxx
# So: with extra bits, '1' + two lowest bits + extras
#     number of extra bits = (C >> 2) - 1
#
# Binary table for the distance codes:
#
# code    dist-1
# ----   -------
# 0000   0000000 (not actually used)
#  ...       ...
# 0011   0000011
# 0100   000010x
# 0101   000011x
# 0110   00010xx
# 0111   00011xx
# 1000   0010xxx
# 1001   0011xxx
# 1010   010xxxx
# 1011   011xxxx
# 1100   10xxxxx
# So: with extra bits, '1' + lowest bit + extras
#     number of extra bits = (C >> 1) - 1

use strict;
use warnings;

# define the rfk86 alphabet (to save space in font)

my $alpha = " ABCDEFGHIJKLMNOPQRSTUVWXYZ.,'\"!";
$alpha .=   "-abcdefghijklmnopqrstuvwxyz:;/\$?";
$alpha .=   "0123456789()";

my @alpha = split //, $alpha;

my %alpha;
$alpha{$alpha[$_]} = $_ foreach (0 .. $#alpha);

my $N = scalar @alpha;

# alphabet-dependant length/distance codes for the compression

my @lcodes = (
    [3, 10, $N+1, 0],
    [11, 18, $N+9, 1],
    [19, 34, $N+13, 2],
    [35, 66, $N+17, 3],
    [67, 98, $N+21, 4]
    );
my @dcodes = (
    [1, 4, 0, 0],
    [5, 8, 4, 1],
    [9, 16, 6, 2],
    [17, 32, 8, 3],
    [33, 64, 10, 4],
    [65, 96, 12, 5]
    );

sub xcode
{
    my ($table, $n) = @_;
    foreach my $row (@$table)
    {
        next unless $n >= $row->[0] and $n <= $row->[1];
        my $x = '';
        $n -= $row->[0];
        $x = ($n&1).$x, $n >>= 1 foreach (1 .. $row->[3]);
        return ($row->[2] + $n, $x);
    }
    die "aieeee xcode broke";
}

# add-on feature: convert argument strings

while (my $msg = shift @ARGV)
{
    chomp $msg;
    print "\t;; $msg\n";
    print "\tdb ";
    print join(', ', map { $alpha{$_} } split //, $msg);
    print "\n";
}

# slurp in all the messages

open IN, '<', 'messages.txt' or die "can't read messages.txt: $!";
my @texts = <IN>;
close IN;
s/\n$// foreach @texts;

# try to sensiblize the line-wrapping, if possible

$_ = try_wrap($_) foreach @texts;

my $bytes = 0;
$bytes += length($_) foreach @texts;
print "Uncompressed message data: $bytes bytes\n";

# compress the messages

my @messages;
push @messages, compress($_) foreach @texts;

my $t = 0;
foreach my $code (@messages)
{
    $t += (ref($_) ? 2 : 1) foreach @$code;
}
print "LZ77 with trivial encoding: $t bytes\n";

my @lfreqs = map { 0 } (0 .. ($N + 23));
my @dfreqs = map { 0 } (0 .. 12);

foreach my $code (@messages)
{
    foreach my $item (@$code)
    {
        if (ref($item))
        {
            my ($len, $dist) = @$item;
            my ($lencode, $lenx) = xcode(\@lcodes, $len);
            my ($distcode, $distx) = xcode(\@dcodes, $dist);
            @$item = ($lencode, $lenx, $distcode, $distx);
            $lfreqs[$lencode]++;
            $dfreqs[$distcode]++;
        }
        else
        {
            $lfreqs[$item]++;
        }
    }
}

# when debugging:
#printf "l[%02d] = %d\n", $_, $lfreqs[$_] foreach (0 .. $#lfreqs);
#printf "d[%02d] = %d\n", $_, $dfreqs[$_] foreach (0 .. $#dfreqs);

my ($ltree, $lsize) = huffman(\@lfreqs);
my ($dtree, $dsize) = huffman(\@dfreqs);

my $lcode = huffman_code($ltree);
my $dcode = huffman_code($dtree);

my @mdata;
my @mindex;

my $msize = 0;
my $msize_dhuff = 0;

foreach my $code (@messages)
{
    my $bits = '';
    my $bdiff = 0;

    foreach my $item (@$code)
    {
        if (ref($item))
        {
            my ($lsym, $lx, $dsym, $dx) = @$item;
            my $lc = $lcode->[$lsym] or die "no code for len $lsym";
            my $dc = $dcode->[$dsym] or die "no code for dist $dsym";
            my $fixdc = ($dsym == 0 ? '0' : '1'.unpack('B4', chr($dsym<<4)));
            $bits .= $lc.$lx.$fixdc.$dx;
            $bdiff += length($dc) - length($fixdc);
        }
        else
        {
            my $c = $lcode->[$item] or die "no code for $item";
            $bits .= $c;
        }
    }

    my $len = int((length($bits) + 7) / 8);
    push @mdata, $bits;
    push @mindex, $len;
    $msize += $len;
    $msize_dhuff += int((length($bits) + $bdiff + 7)/8);
}

print "LZ77 with Huffman (w/o trees): $msize bytes\n";
print "LZ77 with Huffman (raw trees): ", ($msize_dhuff+$lsize+$dsize), " bytes\n";
print "LZ77 with Huffman (no dist tree): ", ($msize+$lsize), " bytes\n";

# write out index and data in new alphabet

open OUT, '>:raw', 'messages.bin' or die "can't write messages.dat: $!";

print OUT pack('C*', @mindex);
print OUT pack('b*', $_) foreach @mdata;

close OUT;

# write the output z80asm-compatible inc files

open OUT, '>', 'messages.inc' or die "can't write messages.inc: $!";

print OUT "messages_count: equ ", scalar @messages, "\n";
print OUT "messages_bytes: equ $bytes\n";

close OUT;

open my $treeout, '>', 'huffman.inc' or die "can't write huffman.inc: $!";
huffman_print($ltree, 'hufftree', $treeout);
close $treeout;

# compression helper: text -> array of length, distance pairs

sub compress
{
    my $msg = shift;
    my @chars = split //, $msg;

    my @code;

    my $i = 0;
    while ($i <= $#chars)
    {
        # look for the longest match starting in [0 .. $i-1] range

        my ($match_distance, $match_length);

        for (my $j = 0; $j < $i; $j++)
        {
            next unless $chars[$j] eq $chars[$i];
            # found some sort of match; check length
            my $len = 1;
            $len++ while $i+$len <= $#chars and $chars[$j+$len] eq $chars[$i+$len];
            next if $len < 3 or (defined $match_length and $match_length > $len);
            # yay, it's better
            $match_distance = $i-$j;
            $match_length = $len;
        }

        # output either a literal or a backref

        if (defined $match_distance)
        {
            push @code, [$match_length, $match_distance];
            $i += $match_length;
        }
        else
        {
            push @code, $alpha{$chars[$i]};
            $i++;
        }
    }

    push @code, $N;

    return \@code;
}

# Huffman tree builder / code extractor / assembler printer

sub huffman
{
    my $freqs = shift;

    my @leaves = ();
    my @nodes = ();

    foreach my $sym (0 .. $#$freqs)
    {
        next unless $freqs->[$sym];
        push @leaves, [$freqs->[$sym], $sym];
    }
    @leaves = sort { $a->[0] <=> $b->[0] } @leaves;

    my $size = 2*(scalar @leaves);

    my $get = sub {
        return shift @nodes unless @leaves;
        return shift @leaves unless @nodes;
        return shift @leaves unless $nodes[0]->[0] < $leaves[0]->[0];
        return shift @nodes;
    };

    while ((@leaves + @nodes) > 1)
    {
        my $left = $get->();
        my $right = $get->();

        push @nodes, [$left->[0] + $right->[0], $left, $right];
        $size += 4;
    }

    return ($get->(), $size);
}

sub huffman_code
{
    my $tree = shift;
    my @code;

    my $walk;
    $walk = sub {
        my ($t, $prefix) = @_;
        if (scalar @$t == 2)
        {
            # leaf node
            $code[$t->[1]] = $prefix;
            return;
        }
        # inner node
        $walk->($t->[1], $prefix.'0');
        $walk->($t->[2], $prefix.'1');
    };

    $walk->($tree, '');
    return \@code;
}

sub huffman_print
{
    my ($tree, $label, $file) = @_;

    print $file "$label: equ \$+1\n";

    my $walk;
    $walk = sub {
        my ($t, $prefix) = @_;
        my $n = $label.'_node_'.$prefix;
        if (scalar @$t == 2)
        {
            print $file ".$n: equ \$-1 ; leaf\n";
            print $file "\tdb 0, $t->[1]\n";
            return;
        }
        print $file ".$n: ; inner\n";
        print $file "\tdw .${n}0+1, .${n}1+1\n";
        $walk->($t->[1], $prefix.'0');
        $walk->($t->[2], $prefix.'1');
    };

    $walk->($tree, '');
}

# line wrapping helper

sub try_wrap
{
    my $msg = shift;

    my $space_left = 96-length($msg);

    return $msg if length($msg) <= 32;

    # try to better line-1 wrapping

    if (substr($msg, 32, 1) eq ' ')
    {
        # very good place to break, just remove this space
        substr($msg, 32, 1, '');
        $space_left++;
    }
    else
    {
        my $break = rindex($msg, ' ', 31);
        die "aieee, wrapping problem: $msg" if $break < 0;
        my $to_move = 31-$break;
        return $msg if $to_move > $space_left;
        substr($msg, $break, 0, ' 'x$to_move);
        $space_left -= $to_move;
    }

    return $msg if length($msg) <= 64;

    # try to better line-2 wrapping

    if (substr($msg, 64, 1) eq ' ')
    {
        # again a fortuitous happenstance
        substr($msg, 64, 1, '');
    }
    else
    {
        my $break = rindex($msg, ' ', 63);
        die "aieee, another wrapping problem: $msg" if $break < 0;
        my $to_move = 63-$break;
        return $msg if $to_move > $space_left;
        substr($msg, $break, 0, ' 'x$to_move);
    }

    return $msg;
}
