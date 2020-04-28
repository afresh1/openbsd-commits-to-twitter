#!/usr/bin/perl
########################################################################
# Copyright (c) 2012 Andrew Fresh <andrew@afresh1.com>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
########################################################################
use strict;
use warnings;
use 5.012;
use Encode qw(encode decode);

use DB_File;
use File::Basename;
use Net::Twitter;
use Net::FTP;
use OpenBSD::PackageName;
use POSIX       qw( strftime );
use Time::Local qw( timegm );

my $seen_file = $ENV{HOME} . '/.tweeted_changes';
my $auth_file = $ENV{HOME} . '/.auth_tokens';
#my $mirror    = 'openbsd.cs.toronto.edu';
my $mirror    = 'ftp5.usa.openbsd.org';
#my $mirror    = 'mirrors.sonic.net';

sub set_seen_file { $seen_file = $_[0] } # For debugging

my $default_maxlen = 280;

my %accounts = (
    cvs      => 'openbsd_cvs',
    src      => 'openbsd_src',
    ports    => 'openbsd_ports',
    xenocara => 'openbsd_xenocar',
    www      => 'openbsd_www',
    stable   => 'openbsd_stable',

    sets    => 'openbsd_sets',
);

do_it() unless caller;
sub do_it {
    my ($changelog) = @ARGV;
    die "Usage: $0 <path/to/ChangeLog>\n" unless $changelog;

    # Login to twitter
    foreach my $key ( sort keys %accounts ) {
        my $account = $accounts{$key};
        get_twitter_account($account);
    }

    my @commits = parse_changelog($changelog);
    my @sets = do {
        local $@;
        my @s = eval { local $SIG{__DIE__}; parse_sets($mirror) };
        warn $@ if $@;
        @s;
    };
    foreach my $details (@commits, @sets) {
        check_message( $details );
    }
    sleep 10;
    retweet();
}

sub check_message {
    my ($details) = @_;

    return unless $details;
    return unless $details->{id};

    my $seen = seen();

    my ( $message, $params ) = make_tweet($details);

    if (!$seen->{ $details->{id} }) {
        if ( tweet( $message, $params ) ) {
            $seen->{$_} = time
                for $details->{id}, @{ $details->{ids} || [] };
        }
    }

    if ($details->{Tag} && !$seen->{ 'stable_' . $details->{id} }) {
        $params->{who} = account_for( 'stable' );
        if ( tweet( $message, $params ) ) {
            $seen->{ 'stable_' . $_ } = time
                for $details->{id}, @{ $details->{ids} || [] };
        }
    }

    sync_seen();
}

sub account_for {
    my ($module) = @_;
    return $accounts{$module} || 'openbsd_cvs';
}

sub change_for {
    my ($commit) = @_;
    my %changes;
    my @dirs;

    my $has_regress     = 0;
    my $has_non_regress = 0;
    foreach my $key ( keys %{$commit} ) {
        if ( $key =~ /^(\w+)\s+files$/ ) {
            $changes{ lc $1 }++;
            foreach ( keys %{ $commit->{$key} } ) {
                my $dir = $_;
                my @files = @{ $commit->{$key}->{$dir} || [] };
                @files = '' unless @files;

                if   ( $dir =~ s{^regress/}{} ) { $has_regress++ }
                else                            { $has_non_regress++ }

                push @dirs, map {"$dir/$_"} @files;
            }
        }
    }

    my @changes = keys %changes;
    my $changed = @changes == 1 ? $changes[0] : 'changed';

    unless (@dirs) {
        if (@changes) {
            return "$changed something";
        }
        return "did something the parser didn't understand";
    }

    # Put them shortest first
    @dirs = sort { length $a <=> length $b } @dirs;
    my $num_changed = @dirs;

    my $match = shift @dirs;
    $match //= '';

    my $last = '/';
    foreach my $dir (@dirs) {
        $last = chop $match while $dir !~ /^\Q$match/;
    }
    $match .= '*' if $match and $last ne '/' and $match !~ m{/$};

    $match =~ s{^[\.\/]+}{};    # No need for leading ./
    $match =~ s{/+$}{};         # one less char most likely

    my $message = $changed;
    if ( !$match ) {
        if ($has_non_regress) {
            if    ( $num_changed > 5 ) { $message .= ' many things' }
            elsif ( $num_changed > 2 ) { $message .= ' a few things' }
            elsif ( $num_changed > 1 ) { $message .= ' a couple things' }
            else                       { $message .= ' something' }
        }
        $message .= ' including' if $has_regress and $has_non_regress;
        $message .= ' regression tests' if $has_regress;
    }
    elsif ($has_regress) {
        if ($has_non_regress) {
            $message .= " $match and regression tests";
        }
        else {
            $message .= " regress/$match";
        }
    }
    else {
        $message .= " $match";
    }

    return $message;
}

sub make_tweet {
    my ($commit) = @_;
    if ( my $type = $commit->{type} ) {
        return make_tweet_for_stable_package(@_)
            if $type eq 'packages-stable';
        return make_tweet_for_sets(@_);
    }

    my %params = ( who => account_for( $commit->{'Module name'} ), );

    my $by = $commit->{'Changes by'};
    $by =~ s/\@.*$/\@/;

    my $change = change_for($commit);

    my $message = "$by $change: " . $commit->{'Log message'};
    $message = $commit->{Tag} . ' ' . $message if $commit->{Tag};
    $message =~ s/\s*\d+\s*conflicts created by this import.*//s;
    $message =~ s/\s+/ /gms;

    return shorten($message), \%params;
}

sub make_tweet_for_sets {
    my ($set) = @_;
    my %params = ( who => 'openbsd_sets' );

    my $message = "New OpenBSD $set->{release} $set->{type} for $set->{arch}";
    $message .= " including X" if $set->{xepoch};

    if ( $set->{type} eq 'syspatch' ) {
        $params{who} = 'openbsd_stable';
        my $file = $set->{file};
        $file =~ s/^syspatch\d+-//;
        $message .= ": $file";
    }

    return shorten($message), \%params;
}

sub make_tweet_for_stable_package {
    my ($set) = @_;
    my %params = ( who => 'openbsd_stable' );

    my $message = '';
    my $package = 'package';

    if ( my $packages = $set->{packages} ) {
        if ( $set->{name} ) {
            my $s = @{ $packages } == 1 ? '' : 's';
            $message .= ": $set->{file} with subpackage$s ";
        }
        else {
            $package = 'packages';
            $message .= " version $set->{version} of ";
        }
        $message .= join ", ", @{$packages};
    }
    else {
        $message .= ": $set->{file}";
    }

    if ( my $flavors = $set->{flavors} ) {
        if ( $flavors->[0] eq '' ) {
            shift @{ $flavors };
            my $s = @{ $flavors } == 1 ? '' : 's';
            $message .= " with additional flavor$s: ";
        }
        else {
            my $s = @{ $flavors } == 1 ? '' : 's';
            $message .= " with flavor$s: ";
        }
        $message .= join ", ", @{ $flavors };
    }

    my $arch = join ", ", $set->{arch}, @{ $set->{archs} || [] };

    $message
        = "New OpenBSD $set->{release} $package for $arch$message";

    return shorten($message), \%params;
}

sub shorten {
    my ($message, $maxlen) = @_;
    $maxlen ||= $default_maxlen;
    if ( length $message > $maxlen ) {
        my $keep = $maxlen - 3;
        $message =~ s/^(.{$keep}).*/$1/ms;
        $message =~ s/\s+$//ms;
        $message .= '...';
    }
    return $message;
}

sub tweet {
    my ( $message, $params ) = @_;

    say encode('UTF-8', "Tweeting [$message]");
    my $encoded = encode('UTF-8', $message);
    local $@;
    eval { local $SIG{__DIE__};
        get_twitter_account( $params->{who} )->update($encoded) };
    if ($@) {

        # If we have what Twitter thinks is a URL, they are going to
        # "shorten" it.  That might make it longer, too long.
        # so, our best bet is to just keep chomping letters.
        if ($@ =~ /tweet is too long|is over 280 characters|needs to be a bit shorter/) {
            $message =~ s/\.+$//; # strip the ellipse
            return tweet( shorten($message, length($message) - 1), $params );
        }
        elsif ($@ =~ /Status is a duplicate/) {
            #warn "$@\n"; # could be useful, but mostly just annoys me with email
            return 1;
        }

        warn $@;
        return 0;
    }
    return 1;
}

sub retweet {

    my $opts = { count => 100, trim_user => 1 };
    my $since_id = seen()->{openbsd_cvs_last_retweet} || 0;
    $opts->{since_id} = $since_id if $since_id;

    my $nt     = get_twitter_account('openbsd_cvs');
    my $tokens = get_access_tokens('openbsd_cvs');
    my $tweets = $nt->home_timeline($opts);

    foreach my $tweet ( reverse @{$tweets} ) {
        next if $tweet->{user}->{id_str} == $tokens->{user_id};
        next if $tweet->{retweeted};
        print "Retweet $tweet->{id_str}\n";
        $nt->retweet( $tweet->{id_str} );
        seen()->{openbsd_cvs_last_retweet} = $tweet->{id_str};
    }
    sync_seen();
}

sub parse_changelog {
    my ($file) = @_;
    return {} unless -f $file;

    my @commits;
    my %commit;

    my $finish_commit = sub {
        if ( my $changes = $commit{'Changes by'} ) {
            my ( $who, $when ) = split /\s+/, $changes, 2;
            $commit{'Changes by'} = $who;
            $commit{'Changes on'} = $when;
        }

        $commit{'Log message'} //= '';
        $commit{'Log message'} =~ s/^\s+//gm;
        $commit{'Log message'} =~ s/\s+$//gm;

        $commit{id} = join '|', grep {defined}
            @commit{ 'Module name', 'Changes by', 'Changes on' };

        push @commits, {%commit};
        %commit = ();
    };

    open my $fh, '<', $file or die $!;
    my $key = '';
    my $dir = '';
    while (1) {
        $_ = decode('UTF-8', readline $fh) || last;
        chomp;

        if (/^\s*(CVSROOT|Module name|Changes by):\s+(.*)$/) {
            $commit{$1} = $2;
            next;
        }
        next unless $commit{CVSROOT};    # first thing should be CVSROOT

        if (/^(Update of)\s+(.*)\/([^\/]+)$/) {
            $commit{'Updated files'}{$2} = [$3];
            next;
        }

        if (/^(\w+ files):/) {
            $key = $1;
            next;
        }

        if ($key) {
            s/^\s+//;
            unless ($_) { $key = ''; $dir = ''; next; }

            my @files;
            if (/^\s*([^:]*?)\s*:\s*(.*)$/) {
                $dir = $1;
                @files = $2;
            }
            else { @files = $_ }
            @files = map {split} @files;
            next unless $dir;

            if (@files && $files[0] eq 'Tag:') {
                my $k = shift @files;
                my $v = shift @files;

                $k =~ s/:$//;
                $commit{$k} = $v;
            }

            push @{ $commit{$key}{$dir} }, @files;
            next;
        }

        if (/^Log [Mm]essage:/) {
            my $cvsroot = parse_log_message( \%commit, $fh );
            $finish_commit->();
            $commit{CVSROOT} = $cvsroot;
        }
    }
    close $fh;

    $finish_commit->();
    return @commits;
}

sub parse_log_message {
    my ( $commit, $fh ) = @_;

    my $importing = 0;

    while (<$fh>) {
        if ( /^CVSROOT:\s+(.*)$/ ) {
            return $1; # we've found the end of this message
        }
        elsif ( my ( $k, $v ) = /^\s*(Vendor Tag|Release Tags):\s+(.*)$/ ) {
            $commit->{$k} = $v;
            $commit->{'Log message'} =~ s/\s*Status:\s*$//ms;
            $importing = 1;
        }
        elsif ( $importing && m{^\s*[UCN]\s+[^/]*/(.*)/([^/]+)\b$} ) {
            push @{ $commit->{'Imported files'}{$1} }, $2;
        }
        else {
            $commit->{'Log message'} .= $_;
        }
    }
    return;
}

sub parse_sets {
    my ($host) = @_;

    # First gather up all the files on the ftp server
    # do this and save the output so we're not holding open their connection.

    my $ftp = Net::FTP->new( $host, Debug => 0 )
        or die "Cannot connect to $host: $@";

    $ftp->login( "anonymous", 'openbsd_sets@twitter' )
        or die "Cannot login ", $ftp->message;

    my @in_version = map { $ftp->dir("/pub/OpenBSD/*/$_") }
        ( '*/*base*.tgz', '*/xshare*.tgz', '*/man*.tgz',
          '*/miniroot*.fs', 'packages/*/index.txt' );
    my @syspatch   = $ftp->dir('/pub/OpenBSD/syspatch/*/*/*.tgz');
    my @packages_stable
         = map { $ftp->dir($_) }
           map { s{/SHA256\.sig$}{/*.tgz}r }
           $ftp->ls('/pub/OpenBSD/*/packages-stable/*/SHA256.sig');

    $ftp->quit;

    # Now we return those lines, converted into hashrefs
    # we can use to make a tweet.
    return parse_ftp_dir( @in_version, @syspatch ),
        collapse_stable_packages( parse_ftp_dir(@packages_stable) );
}

sub parse_ftp_dir {
    my @non_sets;
    my %sets;

    # Now we parse each line into a hashref that has a "type"
    foreach my $set ( map { parse_ftp_dir_line($_) } @_ ) {

        # If it's a "sets" type, then we actually get a few different files
        # from that set and are going to validate that the set is fully
        # populated.
        # We should probably do that for packages as well,
        # but that's less of a problem and we'd have to work hard to
        # figure out what files should be at the start and end of a sync.
        if ($set->{type} eq 'sets') {
            my $merge = $sets{"$set->{release}/$set->{arch}"} ||= $set;
            $merge->{ delete $set->{file} } = delete $set->{epoch};
        }
        else {
            push @non_sets, $set;
        }
    }

    # Now we return any sets we can find an "id" for
    return grep { $_->{id} }
        map { $_->{id} = id_for_set($_); $_ } ( @non_sets, values %sets );
}

sub parse_ftp_dir_line {
    my ( $perm, $links, $u, $g, $size, $mon, $day, $yort, $path )
        = split " ", $_[0];

    # Complain if the server sent us back something we didn't understand
    # that's usually that we got back the glob due to a timeout.
    unless ($path) {
        warn "No files in [$_[0]]\n";
        return;
    }

    my ( $file, $arch, $release, $type ) = reverse split qr{/}, $path;

    # We don't care about the "tools",
    # but it's hard to build a glob that doesn't grab it.
    return if $arch eq 'tools';

    if ( $release eq 'packages' or $release eq 'packages-stable' ) {
        # packages have the release and type reversed
        ( $release, $type ) = ( $type, $release );
    }
    elsif ( $type eq 'OpenBSD' ) {

        # If this is in the "OpenBSD" dir, then they're install sets
        # but, they're verisioned and that makes working with them hard.
        # So, replace the version with X.
        $type = 'sets';
        $file =~ s/\d/X/g;
    }

    # We don't have room for file extensions in the tweet!
    $file =~ s/\.\w+$//;

    $release = 'snapshot' if $release eq 'snapshots';

    # Now we return the parsed out information in a useful hashref
    return {
        release => $release,
        arch    => $arch,
        type    => $type,
        file    => $file,
        epoch   => scalar to_epoch( $mon, $day, $yort ),
    };
}

sub id_for_set {
    my ($set) = @_;

    # Here we're going to calculate a unique-id for this set
    # We want it to be fairly unique, but not too touchy.
    my $type   = $set->{type};

    # For example, we want to know if a snapshot changes multile times a day
    # but not for releases.
    my $ts_fmt = $set->{release} eq 'snapshot' ? '%FT%H%M' : '%F';

    # We do know some things that are going to uniquely identify this item
    my @keys = qw< type release arch >;

    # To detect when a complete set is available, we make some
    # guesses.  If there is an installXX, or xbaseXX, those should
    # be newer than the baseXX because that means a full set has
    # been built.  Some of the slower architectures seem to get
    # base builds more frequently than X builds, but lowering the
    # noise level is more important than accuracy.  Most folks
    # interested in this probably care only about amd64 anyway.
    if ( $type eq 'sets' ) {
        my $epoch    = $set->{baseXX}     || 0;
        my $complete = $set->{minirootXX} || $set->{manXX} || 0;

        unless ( $epoch and $complete >= $epoch ) {
            warn "Incomplete set for $set->{arch}/$set->{release} ($epoch > $complete)\n";
            $set->{remove} = 1;
            return;
        }

        if ( my $xepoch = $set->{xbaseXX} ) {
            my $xcomplete = $set->{xshareXX} || 0;
            if ( $xepoch > $epoch and $xepoch > $xcomplete ) {
                $set->{xepoch} = $xepoch;
            }
        }

        $set->{epoch} = $epoch;
    }
    elsif ( $type eq 'syspatch' or $type eq 'packages-stable' ) {

        # For stable packages and sysparches we don't care about timestamps
        # as they all seem to change whenever a new one is released.
        $ts_fmt = '';

        # Instead, we end u unique by filename
        push @keys, 'file';
    }

    # Now we actually build the ID,
    my $id = join '-', map { $set->{$_} } @keys;
    $id .= strftime( "-$ts_fmt", gmtime $set->{epoch} ) if $ts_fmt;

    return $id;
}

sub collapse_stable_packages {

    # Sort by length of the file name,
    # this makes sure we find the "main" package before any flavors.
    my @all = map { $_->[0] }
        sort { $a->[1] <=> $b->[1] }
        map  { [ $_, length $_->{file} ] } @_;

    my $seen = seen();

    my %by_package;
    for (@all) {
        if ( $seen->{ $_->{id} } ) {
            $_->{remove} = 1;
            next;
        }

        my ( $name, $version, @flavors )
            = OpenBSD::PackageName::splitname( $_->{file} );

        # Need to group by release, arch, and version
        my $group = "$_->{release}/$_->{arch}-$version";

        # Find the main entry for this particular rel and name
        my $entry = $by_package{$group}{'---'}{$name} ||= $_;

        if ( $_ eq $entry ) {
            # We might need the version for display later
            $entry->{version} = $version;
        }
        else {
            # Remove things that are collapsing by flavor
            $_->{remove} = $entry->{id};
            push @{ $entry->{ids} }, $_->{id};
        }

        # Add this package's flavors to the list that are included
        push @{ $entry->{flavors} }, join '-', @flavors;

        # Now prepare to collapse by a subset of their parts.
        my ( $base, @packages ) = split /-/, $name;
        $by_package{$group}{$base}{$name} = $entry;
        for (@packages) {
            $base .= '-' . $_;
            $by_package{$group}{$base}{$name} = $entry;
        }
    }

    my %by_flavor;
    foreach my $group ( keys %by_package ) {

        # We no longer need to lookup by name, so remove the table
        delete $by_package{$group}{'---'};

        # Try to collapse packages with a matching base into a single entry
        foreach my $base ( sort keys %{ $by_package{$group} } ) {

            # Now try to collapse any packages that share flavors
            my %flavors;
            foreach my $name ( keys %{ $by_package{$group}{$base} || {} } ) {
                my $entry  = $by_package{$group}{$base}{$name};
                next if $entry->{remove}; # already collapsed somewhere

                my $flavor = join '/', @{ $entry->{flavors} || [''] };
                $flavors{$flavor}{$name} = $entry;
            }

            # Only collapse packages that share flavors
            foreach my $flavor ( keys %flavors ) {
                my @packages = sort keys %{ $flavors{$flavor} };

                # pre-sorted, so we just pick the first package as main
                my $package = shift @packages;
                my $entry   = $flavors{$flavor}{$package};

                # No need to track if the only flavor was no flavor
                delete $entry->{flavors} unless $flavor;

                # We can group release, base, and version by matching flavors and subpackages
                my $flavor_group = "$entry->{release}/$package-$entry->{version}";
                my $flavor_base  = "$flavor-@packages";

                $by_flavor{$flavor_group}{$flavor_base}{ $entry->{arch} } = $entry;

                # No need to collapse a flavor with only a single package
                next unless @packages;

                # remove packages we're collapsing into this one
                for (@packages) {
                    $flavors{$flavor}{$_}{remove} = $entry->{id};
                    push @{ $entry->{ids} }, $flavors{$flavor}{$_}{id};
                }

                # If the base matches, we can trim it off
                # otherwise, it goes back in the list of multi-packages
                if ( $package eq $base ) {
                    $entry->{name} = $package;
                    s/^\Q$package-// for @packages;
                }
                else {
                    unshift @packages, $package;
                }

                $entry->{packages} = \@packages;
            }
        }
    }

    foreach my $group ( keys %by_flavor ) {

        # Try to collapse architectures with a matching base into a single entry
        foreach my $base ( keys %{ $by_flavor{$group} } ) {
            my @archs = sort keys %{ $by_flavor{$group}{$base} || {} };

            # Pick the first one to collase into.
            # Since we sorted, and we'll put it first,
            # that keeps the list ordered.
            my $arch  = shift @archs;
            my $entry = $by_flavor{$group}{$base}{$arch};

            # No need to collapse a flavor with only a single arch
            next unless @archs;

            # remove entries we're collapsing into this one
            for (@archs) {
                my $removed = $by_flavor{$group}{$base}{$_};
                $removed->{remove} = $entry->{id};
                push @{ $entry->{ids} }, $removed->{id};
            }

            $entry->{archs} = \@archs;
        }
    }

    return grep { not $_->{remove} } @all;
}

sub to_epoch {
    my ($mon, $day, $yort) = @_;

    state $months = {
        Jan => 0, Feb => 1, Mar =>  2, Apr =>  3,
        May => 4, Jun => 5, Jul =>  6, Aug =>  7,
        Sep => 8, Oct => 9, Nov => 10, Dec => 11,
    };

    my $time  = '00:00';
    my $month = $months->{$mon};
    my $year  = $yort;

    if ($yort =~ /:/) {
        my ( $this_month, $this_day );
        ($this_day, $this_month, $year) = (gmtime)[3,4,5];
        $year += 1900;

        $year--
            if $this_month < $month
            or $this_month == $month and $this_day < $day;

        $time = $yort;
    }

    my ($hour, $min) = split /:/, $time;

    return timegm( 0, $min, $hour, $day, $month, $year );
}


{
    my $X;
    my %seen;

    sub seen {
        return \%seen if %seen;

        $X = tie %seen, 'DB_File', $seen_file or die;

        return \%seen;
    }

    sub sync_seen {
        $X->sync;
    }

}

{
    my %tokens;

    sub get_access_tokens {
        my ( $account, $nt ) = @_;

        return $tokens{$account} if exists $tokens{$account};

        open my $fh, '<', $auth_file or die $!;
        while (<$fh>) {
            chomp;
            my ($account_from_file, $access_token, $access_token_secret,
                $user_id,           $screen_name
            ) = split /\s+/;

            if ( $account_from_file eq 'consumer' ) {
                $tokens{$account_from_file} = {
                    consumer_key    => $access_token,
                    consumer_secret => $access_token_secret,
                };
            }
            else {
                $tokens{$account_from_file} = {
                    access_token        => $access_token,
                    access_token_secret => $access_token_secret,
                    user_id             => $user_id,
                    screen_name         => $screen_name,
                };
            }
        }
        close $fh;
        return $tokens{$account} if exists $tokens{$account};

        return unless $nt;

        my $auth_url = $nt->get_authorization_url;
        print
            " Authorize $account for this application at:\n  $auth_url\nThen, enter the PIN# provided to continue ";

        my $pin = <STDIN>;    # wait for input
        chomp $pin;

        # request_access_token stores the tokens in $nt AND returns them
        my ( $access_token, $access_token_secret, $user_id, $screen_name )
            = $nt->request_access_token( verifier => $pin );

        # save the access tokens
        $tokens{$account} = {
            access_token        => $access_token,
            access_token_secret => $access_token_secret,
            user_id             => $user_id,
            screen_name         => $screen_name,
        };

        save_access_tokens();

        return $tokens{$account};
    }

    sub save_access_tokens {
        die "Saving is disabled, make sure you really want to";
        open my $fh, '>', $auth_file or die $!;
        foreach my $key ( sort keys %tokens ) {
            my @keys
                = $key eq 'consumer'
                ? qw( consumer_key consumer_secret )
                : qw( access_token access_token_secret user_id screen_name );
            say $fh join "\t", $key, @{ $tokens{$key} }{@keys};
        }
        close $fh;
    }
}

sub get_twitter_account {
    my ($account) = @_;

    my $consumer_tokens = get_access_tokens('consumer');

    my $nt = Net::Twitter->new(
        traits => [qw/API::RESTv1_1 OAuth/],
        ssl    => 1,
        %{$consumer_tokens}
    );

    my $tokens = get_access_tokens( $account, $nt );

    $nt->access_token( $tokens->{access_token} );
    $nt->access_token_secret( $tokens->{access_token_secret} );

    #my $status = $nt->user_timeline( { count => 1 } );
    #print Dumper $status;
    #print Dumper $nt;

    return $nt;
}

1;
