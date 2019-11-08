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
use POSIX       qw( strftime );
use Time::Local qw( timegm );

my $seen_file = $ENV{HOME} . '/.tweeted_changes';
my $auth_file = $ENV{HOME} . '/.auth_tokens';
#my $mirror    = 'openbsd.cs.toronto.edu';
my $mirror    = 'ftp5.usa.openbsd.org';
#my $mirror    = 'mirrors.sonic.net';

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
            $seen->{ $details->{id} } = time;
        }
    }

    if ($details->{Tag} && !$seen->{ 'stable_' . $details->{id} }) {
        $params->{who} = account_for( 'stable' );
        if ( tweet( $message, $params ) ) {
            $seen->{ 'stable_' . $details->{id} } = time;
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
    return make_tweet_for_sets($commit) if $commit->{type};

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

    my $type = $set->{type} eq 'packages-stable' ? 'stable package' : $set->{type};

    my $message = "New OpenBSD $set->{release} $set->{type} for $set->{arch}";

    if ($set->{type} eq 'syspatch' or $set->{type} eq 'packages-stable') {
        $params{who} = 'openbsd_stable';
        my $file = $set->{file};
        $file =~ s/^syspatch\d+-//;
        $message .= ": $file";
    }

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
        if ($@ =~ /tweet is too long|is over 140 characters|needs to be a bit shorter/) {
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

    my $ftp = Net::FTP->new( $host, Debug => 0 )
        or die "Cannot connect to $host: $@";

    $ftp->login( "anonymous", 'openbsd_sets@twitter' )
        or die "Cannot login ", $ftp->message;

    my %sets;
    my @paths = (
        '*/*base*.tgz',
        '*/install*.*',
        'packages/*/index.txt',
        'packages-stable/*/*.tgz'
    );

    my @in_version = map { $ftp->dir("/pub/OpenBSD/*/$_") } @paths;
    my @syspatch   = $ftp->dir('/pub/OpenBSD/syspatch/*/*/*.tgz');

    $ftp->quit;

    for (@in_version, @syspatch) {
        my ( $perm, $links, $u, $g, $size, $mon, $day, $yort, $path ) = split;

        unless ($path) {
            warn "No files in [$_]\n";
            next;
        }

        my ( $file, $arch, $release, $type ) = reverse split qr{/}, $path;

        next if $arch eq 'tools';

        if ( $release eq 'packages' or $release eq 'packages-stable' ) {
            ( $release, $type ) = ( $type, $release );
        }
        elsif ( $type eq 'OpenBSD' ) {
            $type = 'sets';
            $file =~ s/\d/X/g;
        }

        $file =~ s/\.\w+$//;

        $release = 'snapshot' if $release eq 'snapshots';
        $sets{$release}{$arch}{$type}{$file} = to_epoch( $mon, $day, $yort );
    }

    my @sets;
    foreach my $release ( sort keys %sets ) {
        my $ts_fmt = $release eq 'snapshot' ? '%FT%H%M' : '%F';

        foreach my $arch ( sort keys %{ $sets{$release} } ) {
            my %update = %{ $sets{$release}{$arch} };
            my $fmt = "$release-$arch";

            foreach my $type ( sort keys %update ) {
                next if $type eq 'sets'; # special handling later
                foreach my $file ( sort keys %{ $update{$type} } ) {
                    if ( my $epoch = $update{$type}{$file} ) {
                        my $id = "$type-$fmt";
                        $id .= "-$file" unless $type eq 'packages';
                        $id .= strftime( "-$ts_fmt", gmtime $epoch )
			    unless $type eq 'syspatch' or $type eq 'packages-stable';
                        push @sets, {
                            id      => $id,
                            epoch   => $epoch,
                            type    => $type,
                            release => $release,
                            arch    => $arch,
                            file    => $file,
                        };
                    }
                }
            }

            if ( $update{sets} and my $epoch = $update{sets}{baseXX} ) {
                my %set = %{ $update{sets} };

                # To detect when a complete set is available, we make some
                # guesses.  If there is an installXX, or xbaseXX, those should
                # be newer than the baseXX because that means a full set has
                # been built.  Some of the slower architectures seem to get
                # base builds more frequently than X builds, but lowering the
                # noise level is more important than accuracy.  Most folks
                # interested in this probably care only about amd64 anyway.

                my $complete
                    = $set{installXX} || $set{xbaseXX} || $set{baseXX};
                next if $complete < $epoch;

                push @sets, {
                    id      => strftime( "sets-$fmt-$ts_fmt", gmtime $epoch ),
                    epoch   => $epoch,
                    type    => 'sets',
                    release => $release,
                    arch    => $arch,
                };
            }
        }
    }

    return @sets;
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
