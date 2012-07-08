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
use 5.010;

use DB_File;
use File::Basename;
use File::ChangeNotify;
use File::Find;
use Net::Twitter;

my $seen_file = $ENV{HOME} . '/.tweeted_changes';
my $auth_file = $ENV{HOME} . '/.auth_tokens';

my %accounts = (
    cvs      => 'openbsd_cvs',
    src      => 'openbsd_src',
    ports    => 'openbsd_ports',
    xenocara => 'openbsd_xenocar',
    www      => 'openbsd_www',
);

# Login to twitter
foreach my $key ( sort keys %accounts ) {
    my $account = $accounts{$key};
    get_twitter_account($account);
}

my @dirs = (
    'Maildir/.lists.openbsd.source-changes/',
    'Maildir/.lists.openbsd.ports-changes/',
);

find( sub { check_message($_) }, @dirs );

my $watcher
    = File::ChangeNotify->instantiate_watcher( directories => \@dirs, );
while ( my @events = $watcher->wait_for_events() ) {
    foreach my $event (@events) {
        next unless $event->type eq 'create';
        check_message( $event->path );
    }
}

sub check_message {
    my ($file) = @_;
    state $seen = load_seen();

    my $commit = parse_commit($file);
    return unless $commit;
    return unless $commit->{id};

    return if $seen->{ $commit->{id} };

    my ( $message, $params ) = make_tweet($commit);
    tweet( $message, $params );

    if ( $params->{who} ne 'openbsd_cvs' ) {
        tweet( shorten( $commit->{'Module name'} . ': ' . $message ),
            { %{$params}, who => 'openbsd_cvs' } );
    }
    $seen->{ $commit->{id} } = time;
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

    my $match = shift @dirs;
    $match //= '';

    my $last = '/';
    foreach my $dir (@dirs) {
        $last = chop $match while $dir !~ /^\Q$match/;
    }
    $match .= '*' unless not $match or $last eq '/' or $match =~ s{/$}{};

    $match =~ s{^[\.\/]+}{};    # No need for leading ./
    $match =~ s{/+$}{};         # one less char most likely

    my $message = $changed;
    if ( !$match ) {
        $message .= ' many things'      if $has_non_regress;
        $message .= ' including'        if $has_regress and $has_non_regress;
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
    my %params = ( who => account_for( $commit->{'Module name'} ), );

    my $by = $commit->{'Changes by'};
    $by =~ s/\@.*$/\@/;

    my $change = change_for($commit);

    my $message = "$by $change: " . $commit->{'Log message'};
    $message =~ s/\s+/ /gms;

    return shorten($message), \%params;
}

sub shorten {
    my ($message) = @_;
    if ( length $message > 140 ) {
        $message =~ s/^(.{137}).*/$1/ms;
        $message =~ s/\s+$//ms;
        $message .= '...';
    }
    return $message;
}

sub tweet {
    my ( $message, $params ) = @_;

    say "Tweeting $message";
    eval { get_twitter_account( $params->{who} )->update($message) };
    if ($@) {
        warn $@;
        return 0;
    }
    return 1;
}

sub parse_commit {
    my ($file) = @_;
    return {} unless -f $file;

    my %commit;

    my $in = 'HEADER';
    open my $fh, '<', $file or die $!;
    my $key = '';
    my $dir = '';
    while (<$fh>) {
        chomp;

        if ( $in eq 'HEADER' ) {
            if (/^Message-ID:\s+(.+?)\s*$/i) { $commit{id} = $1 }
            unless ($_) { $in = 'BODY' }
            next;
        }

        if (/(CVSROOT|Module name|Changes by):\s+(.*)$/) {
            $commit{$1} = $2;
            next;
        }
        return unless $commit{CVSROOT};    # first thing should be CVSROOT

        if (/^(Update of)\s+(.*)\/([^\/]+)$/) {
            $commit{'Updated files'}{$2} = [$3];
            next;
        }

        if (/^(\w+ files):/) {
            $key = $1;
            next;
        }

        if ($key) {
            chomp;
            s/^\s+//;
            unless ($_) { $key = ''; next; }

            my (@files) = split /\s*:\s+/;
            $dir = shift @files if @files > 1;
            @files = map {split} @files;
            next unless $dir;

            push @{ $commit{$key}{$dir} }, @files;
        }

        if (/^Log [Mm]essage:/) {
            $commit{'Log message'} .= $_ while <$fh>;
        }
    }
    close $fh;

    if ( my $changes = $commit{'Changes by'} ) {
        my ( $who, $when ) = split /\s+/, $changes, 2;
        $commit{'Changes by'} = $who;
        $commit{'Changes on'} = $when;
    }

    $commit{'Log message'} =~ s/\s+$//ms;

    return \%commit;
}

{
    my $X;

    sub load_seen {
        $X = tie my %seen, 'DB_File', $seen_file or die;
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
        open my $fh;
        foreach my $key ( sort keys %tokens ) {
            my @keys
                = $key eq 'consumer'
                ? qw( consumer_key consumer_secret )
                : qw( access_token access_token_secret user_id screen_name );
            say join "\t", $key, @{ $tokens{$key} }{@keys};
        }
        close $fh;
    }
}

sub get_twitter_account {
    my ($account) = @_;

    my $consumer_tokens = get_access_tokens('consumer');

    my $nt = Net::Twitter->new(
        traits => [qw/API::REST OAuth/],
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
