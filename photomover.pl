#!/usr/bin/perl -w

use warnings;
use strict;

=head1 NAME

photomover.pl - take a directory full of photos from my camera 
                and move them to directories named after dates
                in my photo archive


=head1 SYNOPSIS

photomover.pl [options]

photomover.pl --from=/home/amoore/newphoto --to=/home/www/andrewmoore.com/photo

=head1 OPTIONS

=over 8

=item B<--help>

Print a brief help message and exits.

=item B<--from>

source directory that contains a bunch of phots

=item B<--to>

destination directory that you want them moved to.

=back

=head1 DESCRIPTION

  Andrew Moore <amoore@mooresystems.com> 7/6/2006

=cut

use Carp;
use Getopt::Long;
use Pod::Usage;

use Image::EXIF;
use File::Copy;


my $from = q(/home/amoore/newphoto);
my $to = q(/home/www/andrewmoore.com/photo);
my $help;
my $result = GetOptions ( 'help|?'     => \$help,
                          "from=s"     => \$from,
                          "to=s"       => \$to   );
# unable to parse the options.
if ( ! $result ) {
    pod2usage(2);
}
# --help flag.
if ( $help ) {
    pod2usage(1);
}

$to = checkdest( $to );

my $exif = new Image::EXIF;

opendir( DIR, $from ) || die "can't opendir $from: $!";
while ( my $image = readdir(DIR) ) {
  my $fullpath = $from . "/" . $image;
  next unless ( -f $fullpath );
  next if ( $image =~ /^\./ );
  $exif->file_name($fullpath);

  # my $imageinfo = $exif->get_all_info(); # hash reference
  # print Data::Dumper::Dumper( $imageinfo );

  my $datepath = finddate( $exif );
  carp "date: $datepath";

  my $destdir = preparedest( $datepath );
  copy( $fullpath, $destdir  ) or croak "$!";
}

closedir DIR;

sub checkdest {
  my $dest = shift;

  croak unless ( -d $dest );
  croak unless ( -w $dest );

  return $dest;

}

sub finddate {
  my $exif = shift;

  my $allinfo = $exif->get_all_info();

  my $date = $allinfo->{'image'}{'Image Created'}
    or carp "Unable to find date from " . Data::Dumper::Dumper( $allinfo );
  carp "checking date: $date";
  my $datepath;
  if ( $date =~ /^(\d\d\d\d):(\d\d):(\d\d)\s/ ) {
    $datepath = join( "/", $1, $2, $3 );
    return $datepath;
  } else {
    carp "unable to find date";
    return undef;
  }
}



sub preparedest {
  my $datepath = shift;

  my $fulldir = $to;
  foreach my $dir ( split( "/", $datepath ) ) {
    $fulldir .= "/" . $dir;
    unless ( -d $fulldir ) {
      mkdir $fulldir or croak "unable to mkdir $fulldir";
    }
  }

  my $destpath = $to . "/" . $datepath;
  croak "$destpath is still not a directory" unless ( -d $destpath );
  return $destpath;

}

