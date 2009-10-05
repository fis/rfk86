#! /usr/bin/env perl

# messages.pl -- convert rfk messages to rfk86 data

use strict;
use warnings;

# define the rfk86 alphabet (to save space in font)

my $alpha = " ABCDEFGHIJKLMNOPQRSTUVWXYZ.,'\"!";
$alpha .=   "-abcdefghijklmnopqrstuvwxyz:;/\$?";
$alpha .=   "0123456789()";

my @alpha = split //, $alpha;

my %alpha;
$alpha{$alpha[$_]} = $_ foreach (0 .. $#alpha);

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
my @messages = <IN>;
close IN;
s/\n$// foreach @messages;

# try to sensiblize the line-wrapping, if possible

$_ = try_wrap($_) foreach @messages;

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
