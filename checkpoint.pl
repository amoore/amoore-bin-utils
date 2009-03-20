#!/usr/bin/env perl

use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;
use Carp;
use Data::Dumper;
use WWW::Bugzilla;
use Git::Wrapper;
use File::Copy;
use File::Spec;
use Time::localtime;
use Date::Calc qw( Today );

our %args = process_command_line_arguments();
format_patches()  if $args{'format-patches'};
archive_patches() if $args{'archive-patches'};

our $bz = connect_to_bugzilla();
add_all_patches();
update_bug();

sub connect_to_bugzilla {
    my $bz = WWW::Bugzilla->new(
        server     => $args{'host'},
        email      => $args{'username'},
        password   => $args{'password'},
        bug_number => $args{'bugnumber'},
    ) or croak "unable to connect_to_bugzilla: $!";
    croak "unable to connect_to_bugzilla: $!" if $bz->check_error();
    carp 'connected' if $args{'verbose'};
    return $bz;
}

sub process_command_line_arguments {
    GetOptions(
        \%args,
        'help|?',
        'man',
        'verbose',
        'host|server=s',       # bugzilla server.
        'username|email=s',    # bugzilla login.
        'password=s',          # bugzilla password
        'bugnumber=i',         # optional bugnumber. Tries to get it from branch name
        'component=i',         # 'koha'. Should I remove this?
        'hours=i',             # number of hours worked
        'format-patches',      # create patches
        'archive-patches',     # move patches to the patch archive
        'local-patches',       # attach *.patch from cwd to bug.
        'patch=s@',            # list of patches to attach, instead of --local-patches
        'dry-run',             # don't actually do anything.
    ) or pod2usage(2);

    pod2usage(1) if $args{'help'};
    pod2usage( -verbose => 2 ) if $args{'man'};

    if ( $args{'local-patches'} ) {
        @{ $args{'patch'} } = get_patches_from_current_directory();
    }

    if ( !$args{'bugnumber'} ) {
        $args{'bugnumber'} = get_bugnumber_from_branch_name();
        carp "determined bugnumber from branch name: $args{'bugnumber'}" if $args{'verbose'};
    }

    $args{'dry-run'} = 1;

    carp( Data::Dumper->Dump( [ \%args ], ['args'] ) ) if $args{'verbose'};
    return %args;
}

sub get_patches_from_current_directory {
    my @files = glob('*.patch');
    return @files;
}

sub get_current_branch_name {
    my $git    = Git::Wrapper->new('.');
    my $method = 'name-rev';               # perl doesn't like dashes in method names
    carp 'working on: ' . join( ', ', $git->$method('HEAD') );
    my @output = $git->$method('HEAD');
    my ( $head, $branch_name ) = split( /\s+/smx, $output[0], 2 );
    carp "branch name is: $branch_name";
    return $branch_name;
}

sub get_bugnumber_from_branch_name {
    my $branch_name = get_current_branch_name()
      or return;
    if ( $branch_name =~ /(^\d+)/smx ) {
        return $1;
    }
    return;
}

sub format_patches {
    my $git    = Git::Wrapper->new('.');
    my $method = 'format-patch';           # perl doesn't like dashes in method names.
    $git->$method( { numbered => 1, 'cover-letter' => 1, 'output-directory' => patch_archive_directory() }, 'origin', '-M', '-B' );
    return;
}

sub patch_base_directory {
    my $patch_base_directory = File::Spec->catfile( $ENV{'HOME'}, 'patches', get_current_branch_name() );
    unless ( -d $patch_base_directory ) {
        mkdir $patch_base_directory
          or carp "unable to create $patch_base_directory: $!";
    }
    carp "unable to use $patch_base_directory" unless -d $patch_base_directory;
    return $patch_base_directory;
}

sub patch_archive_directory {
    my $datestring = join( '', localtime->year() + 1900, localtime->mon() + 1, localtime->mday() ) . '-' . join( '', localtime->hour(), localtime->min(), localtime->sec() );
    my $patch_archive_directory = File::Spec->catfile( patch_base_directory(), $datestring );
    unless ( -d $patch_archive_directory ) {
        mkdir $patch_archive_directory
          or carp "unable to create $patch_archive_directory: $!";
    }
    carp "unable to use $patch_archive_directory" unless -d $patch_archive_directory;
    carp "patch_archive_directory: $patch_archive_directory" if $args{'verbose'};
    return $patch_archive_directory;
}

sub archive_patches {
    my @patches                 = get_patches_from_current_directory();
    my $patch_count             = scalar @patches;
    my $patch_archive_directory = patch_archive_directory();
    carp "copying $patch_count patches to $patch_archive_directory" if $args{'verbose'};
    foreach my $file (@patches) {
        copy( $file, File::Spec->catfile( $patch_archive_directory, $file ) )
          or carp "unable to copy $file to $patch_archive_directory: $!";
    }
    return @patches;
}

sub add_all_patches {
  PATCH: foreach my $filename ( @{ $args{'patch'} } ) {
        next PATCh if ( $filename eq '0000-cover-letter.patch' );
        add_one_patch($filename);
    }
  return;
}

sub add_one_patch {
    my $filename = shift;

    carp "attaching $filename..." if $args{'verbose'};
    my $return;
    unless ( $args{'dry-run'} ) {
        $return = $bz->add_attachment(
            server      => $args{'host'},
            email       => $args{'username'},
            password    => $args{'password'},
            filepath    => $filename,
            description => get_description_from_patch($filename),
            is_patch    => 1,
            comment     => get_comment_from_patch($filename),
        ) or carp "unable to add_one_patch: $!";
    }
    carp 'attached.' if $args{'verbose'};
    return $return;
}

sub get_description_from_patch {
    my $filename = shift;

    carp 'get_description_from_patch is not yet implemented';
    return $filename;
}

sub get_comment_from_patch {
    my $filename = shift;

    carp 'get_comment_from_patch is not yet implemented';
    return 'comment';
}

sub update_bug { 

    $bz->work_time( $args{'hours'} ) if $args{'hours'};
    add_comment();
    $bz->change_status('assigned');
    $bz->commit unless $args{'dry-run'};
}

sub add_comment {
    my $comment = '';
    $comment .= "hours worked: $args{'hours'}\n\n" if $args{'hours'};
    my $directory = patch_archive_directory();
    if ( -d $directory ) {
        $comment .= "current patch archive: $directory\n\n";
    }

    return $bz->additional_comments($comment);
}

__END__

=head1 NAME

sample - Using GetOpt::Long and Pod::Usage

=head1 SYNOPSIS

sample [options] [file ...]

 Options:
   -help            brief help message
   -man             full documentation

=head1 OPTIONS

=over 8

=item B<-help>

Print a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=back

=head1 DESCRIPTION

B<This program> will read the given input file(s) and do something
useful with the contents thereof.

=cut
