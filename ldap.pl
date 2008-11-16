#!/usr/bin/perl

use strict;
use Net::LDAP;

my $ldap_server = "mail.example.com";
my $BASEDN = "dc=example,dc=com";
my $user = 'amoore@example.com';
my $passwd = 'secret!';

my @fields = qw(cn mail sn givenname);

my $ldap = Net::LDAP->new($ldap_server) or die "$@";

#$ldap->bind;

my $mesg = $ldap->bind ( dn =>
$user,

password =>$passwd);

if ( $mesg->code()) {
    die ("error:", $mesg->code(),"\n");
  }



$ldap->debug( 0 );

my @result;

foreach my $ask_for (@ARGV) {
    my $query = join '', map { "($_=$ask_for*) " } @fields;
    my $mesg = $ldap->search(base => $BASEDN, filter => "(|$query)");
    $mesg->code && die $mesg->error;
    foreach my $entry ($mesg->all_entries) { 
	next unless ( $entry->get_value( 'mail' ) );  # can't mail them without an address
	my $mail = $entry->get_value('mail');
	my $name = $entry->get_value('givenName') . " " . $entry->get_value('sn');
	my $telephone = $entry->get_value('telephoneNumber');
	push(@result, "$mail\t$name\t$telephone");
    }
}
print "LDAP query: found ", scalar(@result), "\n";
print join "\n", @result;

exit 1 if ! @result;

$ldap->unbind;

__END__

