#!/usr/bin/env perl

use warnings;
use strict;

use Getopt::Long;
use Pod::Usage;

use File::Copy;

my $man = 0;
my $help = 0;
my $directory = '.';
my $verbose;

GetOptions('help|?'      => \$help,
           man           => \$man,
           'directory=s' => \$directory,
           verbose       => \$verbose )
  or pod2usage(2);

pod2usage(1) if $help;
pod2usage( -exitstatus => 0,
           -verbose    => 2 ) if $man;

my @files = glob( "$directory/*" );

FILE: foreach my $filename ( @files ) {
    ( my $newfilename = $filename ) =~ s/\s+/_/g;
    print "renaming $filename to $newfilename\n" if $verbose;
    if ( copy( $filename, $newfilename ) ) {
        unlink( $filename );
    } else {
        warn "unable to rename $filename to $newfilename: $!";
        next FILE;
    }
}

__END__

=head1 NAME

removespaces.pl - renames files to remove spaces.

=head1 SYNOPSIS

removespaces.pl --verbose --directory /tmp/foo

 Options:
  --directory       the directory to deal with. defaults to "."
   -help            brief help message
   -man             full documentation
  --verbose         be verbose

=head1 OPTIONS

=over 8

=item B<--directory>

The directory that you want to fix.

defaults to ".". 

=item B<--verbose>

prints out the list of files that are being changed

=item B<-help>

Print a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=back

=head1 DESCRIPTION

renames all of the files in a directory to names that don't contain spaces.

=cut

