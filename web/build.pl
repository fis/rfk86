#! /usr/bin/env perl

use strict;
use warnings;
use utf8;

use XML::Twig;

# Slurp in the full site description and split to main elements.

my $site = XML::Twig->new();
$site->parsefile('site.xml');

my %pages;
my @pages;
my @navbar;

foreach my $page ($site->get_xpath('/rfk/page'))
{
    push @pages, $page;
    $pages{$page->att('id')} = $page;
}

foreach my $navitem ($site->get_xpath('/rfk/navbar/navitem'))
{
    push @navbar, { map { $_ => $navitem->att($_) } $navitem->att_names };
}

my $header = trim($site->root->child(0, "header")->text);
my $footer = trim($site->root->child(0, "footer")->text);

$header =~ s/^\s*(.*?)\s*$/$1/s;
$footer =~ s/^\s*(.*?)\s*$/$1/s;

# Also determine the latest version for substs.

my $latest = `git describe --tags --always --abbrev=40`;

if ($latest =~ /^(v[\d.]+)-/)
{
    $latest = "$1.".`git rev-parse $1`;
    chomp $latest;
}

# Compute the page element widths.

my $total_width = 80;

my $navbar_width = 1;

foreach my $item (@navbar)
{
    next if $item->{'type'} eq 'sep';
    my $w = length($item->{'type'} eq 'page' ? $pages{$item->{'id'}}->att('name') : $item->{'title'});
    $navbar_width = $w if $w > $navbar_width;
}

my $body_width = $total_width - $navbar_width - 3;

# Unicode line-drawing page layout formatters.

foreach my $page (@pages)
{
    my $file = $page->att('id').'.html';
    open PAGE, '>:utf8', $file or die "can't write: $file: $!";
    my $oldout = select(PAGE);

    my $h = $header;
    $h =~ s/%TITLE%/$page->att('title')/e;
    my $pf = $page->child(0, 'footer');

    print $h;
    format_page($page);
    print trim($pf->text) if $pf;
    print $footer;

    select($oldout);
    close PAGE;
}

sub format_line
{
    my ($template, $data, $width, $data_width) = @_;
    $data_width ||= length($data);
    $data .= ' 'x($width-$data_width);
    $template =~ s/DATA/$data/;
    return $template;
}

sub format_page
{
    my ($page) = @_;

    # Generate the different types of rows.

    my $row_top    = '╔'.('═' x $body_width).'╗';
    my $row_body   = '║<span class="b">DATA</span>║';
    my $row_sep    = '╟'.('─' x $body_width).'╢';
    my $row_bottom = '╚'.('═' x $body_width).'╝';

    my $navbar_top    = ('─' x $navbar_width).'╮';
    my $navbar_body   = '<span class="n">DATA</span>│';
    my $navbar_sep    = ('┄' x $navbar_width).'┤';
    my $navbar_bottom = ('─' x $navbar_width).'╯';

    my %navbar_map = ('║' => '╟', '╢' => '╫');

    # Generate the navbar (except the parts in body).

    my @navbar_rows;

    push @navbar_rows, [1, $navbar_top];

    foreach my $item (@navbar)
    {
        my $text;
        my $width;

        if ($item->{'type'} eq 'page' and $item->{'id'} eq $page->att('id'))
        {
            my $name = $page->att('name');
            $text = sprintf '<span class="sel">%s</span>', $name;
            $width = length($name);
        }
        elsif ($item->{'type'} eq 'page' or $item->{'type'} eq 'link')
        {
            my $url = $item->{'url'};
            my $title = $item->{'title'};

            if ($item->{'type'} eq 'page')
            {
                my $id = $item->{'id'};
                $url = "$id.html";
                $title = $pages{$id}->att('name');
            }

            $text = sprintf '<a href="%s">%s</a>', $url, $title;
            $width = length($title);
        }
        elsif ($item->{'type'} eq 'sep')
        {
            push @navbar_rows, [1, $navbar_sep];
            next;
        }
        die unless $text;

        push @navbar_rows, [0, format_line($navbar_body, $text, $navbar_width, $width)];
    }

    push @navbar_rows, [1, $navbar_bottom];

    # Generate the main body rows.

    my @rows;

    push @rows, $row_top;
    push @rows, format_line($row_body, ' '.$page->att('title'), $body_width);
    push @rows, $row_sep;

    foreach my $item ($page->children)
    {
        my @lines = ();

        my $gi = $item->gi;
        my $text = trim($item->text);
        $text =~ s/[ \n]+/ /g;

        next if $gi eq 'footer';

        if ($gi eq 'p')
        {
            my @words = elt2words($item);
            foreach my $line (wrapwords(\@words, $body_width - 2))
            {
                my $t = join(' ', map { word2text($_) } @$line);

                my $w = -1;
                $w += 1 + (ref($_) ? length($_->[0]) : length($_)) foreach @$line;

                push @lines, [' '.$t, 1+$w];
            }
            push @lines, '';
        }
        elsif ($gi eq 'h')
        {
            push @lines, [sprintf(' <span class="h">%s</span>', $text), length($text)+1];
            push @lines, '';
        }
        elsif ($gi eq 'sep')
        {
            push @rows, $row_sep;
            next;
        }

        foreach my $line (@lines)
        {
            my ($t, $w) = ($line, length($line));
            if (ref($line))
            {
                ($t, $w) = @$line;
            }
            push @rows, format_line($row_body, $t, $body_width, $w);
        }
    }

    # Generate the main body with navbar.

    my @result;

    foreach (my $y = 0; $y <= $#rows or $y-2 <= $#navbar_rows; $y++)
    {
        my $row = '';
        $row = $rows[$y] if $y <= $#rows;

        unless ($row)
        {
            $row = format_line($row_body, '', $body_width);
        }

        if ($y >= 2 && $y-2 <= $#navbar_rows)
        {
            my $nr = $navbar_rows[$y-2];

            substr($row, -1, 1, $navbar_map{substr($row, -1)}) if $nr->[0];
            $row .= $nr->[1];
        }

        push @result, $row;
    }

    push @result, $row_bottom;

    print join("\n", @result), "\n";
}

# Random helper functions.

sub trim
{
    my $s = shift;
    $s =~ s/^[ \n]*(.*?)[ \n]*$/$1/s;
    return $s;
}

sub elt2words
{
    my $elt = shift;
    my @words;

    foreach my $child ($elt->children)
    {
        my $gi = $child->gi;

        my $text = trim($child->text);

        if ($gi =~ /#P?CDATA/)
        {
            push @words, split /[ \n]+/, $text;
        }
        elsif ($gi eq 'br')
        {
            push @words, ['', 'br', {}];
        }
        else
        {
            my %att = map { $_ => $child->att($_) } $child->att_names;
            push @words, map { [$_, $gi, \%att] } split /[ \n]+/, $text;
        }
    }

    foreach (@words)
    {
        if (ref)
        {
            $_->[0] =~ s/%LATEST%/$latest/o;
        }
        else
        {
            s/%LATEST%/$latest/o;
        }
    }

    return @words;
}

sub wrapwords
{
    my ($words, $width) = @_;

    my @rows;
    my @row;
    my $len = 0;

    foreach my $word (@$words)
    {
        if (ref($word) and $word->[1] eq 'br')
        {
            push @rows, [@row] if scalar @row;
            @row = ();
            $len = 0;
            next;
        }

        my $wlen = (ref($word) ? length($word->[0]) : length($word));

        if ($len + ($len ? 1 : 0) + $wlen > $width)
        {
            # too wide, must break something
            if ($len == 0)
            {
                # single extra-wide word, break into two
                my $w1 = $word;
                my $w2 = $word;
                if (ref($word))
                {
                    $w1->[0] = substr($w1->[0], 0, $width);
                    $w2->[0] = substr($w2->[0], $width);
                }
                else
                {
                    $w1 = substr($w1, 0, $width);
                    $w2 = substr($w2, $width);
                }
                push @rows, [$w1];
                $word = $w2;
                redo;
            }
            # can do a line-break
            push @rows, [@row];
            @row = ();
            $len = 0;
            redo;
        }

        push @row, $word;
        $len++ if $len;
        $len += $wlen;
    }

    push @rows, \@row if scalar @row;

    return @rows;
}

sub word2text
{
    my $word = shift;

    my $text = (ref($word) ? $word->[0] : $word);
    return $text unless ref($word);

    my $attr = '';
    $attr = ' '.join(' ', map { sprintf('%s="%s"', $_, $word->[2]->{$_}) } keys %{$word->[2]})
        if keys %{$word->[2]};

    return sprintf('<%s%s>%s</%s>', $word->[1], $attr, $text, $word->[1]);
}
