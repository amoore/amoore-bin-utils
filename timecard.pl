#!/usr/bin/perl -w

use strict;
use Carp;
use Data::Dumper;

use Date::Calc qw( Add_Delta_Days Monday_of_Week Week_of_Year Today );
use Getopt::Long;


my $opt = { plannerdir => q(/home/amoore/Plans) };

my $startday = getStartOfPayPeriod();
# carp "startday is " . Data::Dumper::Dumper( $startday );

for ( 0..21 ) {
  my $dayfile = join(".", map { sprintf( "%.2d", $_ ) }
		     Add_Delta_Days( $startday->{'year'}, $startday->{'month'}, $startday->{'day'}, $_ ) );
  my $tasks = parseDayFile( $dayfile );
  if ( $tasks ) {
      print " * $dayfile\n";
      printCleanTasks( $tasks );
      # print @$tasks if ( $tasks );
      print "\n";
    }

}


sub getStartOfPayPeriod {
  # returns a hashref of year, month, day of beginning of payperiod.
  # payperiods are two weeks long, so find the Monday that started last week

  my ($year,$month,$day) = Today();
  my $week = Week_of_Year( $year, $month, $day );
  my ($fyear,$fmonth,$fday) = Add_Delta_Days( Monday_of_Week( $week, $year ), -7 );

  my $monday = { year  => $fyear,
		 month => $fmonth,
		 day   => $fday };

  return $monday;
}

sub parseDayFile {
  my $dayfile = shift;

  $dayfile = $opt->{'plannerdir'} . "/" . $dayfile . ".muse";

  # carp "checking out $dayfile";

  return unless ( -f $dayfile );

  open( FH, $dayfile ) or croak "Unable to open $dayfile";
  my $intasks = 0;
  my $tasks = ();
  while ( <FH> ) {
    if( /^\* Schedule$/ ) {
      $intasks = 0;
    } elsif ( /^$/ ) {
      next;
    } elsif ( $intasks ) {
      push @$tasks, $_;
    } elsif( /^\* Tasks$/ ) {
      $intasks++;
    }
  }
  return $tasks;

}


sub printCleanTasks {
  my $tasks = shift;

  foreach my $t ( @$tasks ) {
    $t =~ s/^\#[ABC]\s+[X_]\s//;
    $t =~ s/\(\[\[TaskPool\]\]\)$//;
    print $t;
  }

}
