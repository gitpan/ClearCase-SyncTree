package ClearCase::SyncTree;

$VERSION = '0.05';

require 5.004;	# but primarily used/tested with 5.005+

use strict;

use File::Basename;
use File::Compare;
use File::Copy;
use File::Find;
use File::Path;

use ClearCase::Argv 0.21;

use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i;

sub new {
    my $proto = shift;
    my($class, $self);
    if ($class = ref($proto)) {
	# Make a (deep) clone of the original
	require Data::Dumper;
	eval Data::Dumper->new([$proto], ['self'])->Deepcopy(1)->Dumpxs;
	return $self;
    }
    $class = $proto;
    $self = {};
    bless $self, $class;
    $self->comment('By:' . __PACKAGE__);
    # Default is to sync file modes unless on ^$%#* Windows.
    $self->protect(!MSWIN);
    # Set up a ClearCase::Argv instance with the appropriate attrs.
    $self->ct;
    # By default we'll call SyncTree->fail on any cleartool error.
    $self->err_handler($self, 'fail');
    return $self;
}

sub err_handler {
    my $self = shift;
    my $ct = $self->ct;
    if (@_ == 2) {
	my($obj, $method) = @_;
	$method = join('::', ref($obj), $method) unless $method =~ /::/;
	$ct->autofail([\&$method, $obj]);
    } else {
	$ct->autofail(@_);
    }
}

sub ct {
    my $self = shift;
    if (!defined(wantarray)) {
	my $ct = ClearCase::Argv->new({-autochomp=>1, -outpathnorm=>1});
	$ct->syxargs($ct->qxargs);
	$self->{ST_CT} = $ct;
    }
    return $self->{ST_CT};
}

# For internal use only.  Returns a clone of the ct object.
sub clone_ct {
    my $self = shift;
    my $ct = $self->ct;
    die __PACKAGE__ . ": Internal Error: incorrect use of ->clone_ct" if !$ct;
    # Since the ct instance may have a ref back to its enclosing
    # instance (i.e. the default error handler is st->ct->st->fail),
    # we have a hack here to keep from cloning that.
    my $r_handler = $ct->autofail;
    $ct->autofail(0);
    my $ctclone = $ct->clone(@_);
    $ct->autofail($r_handler);
    $ctclone->autofail($r_handler);
    return $ctclone;
}

sub protect {
    my $self = shift;
    $self->{ST_PROTECT} = shift if @_;
    return $self->{ST_PROTECT};
}

sub comment {
    my $self = shift;
    my $cmnt = shift;
    if (ref $cmnt) {
	$self->{ST_CMNT} = $cmnt;
    } elsif ($cmnt) {
	$self->{ST_CMNT} = ['-c', $cmnt];
    }
    return $self->{ST_CMNT};
}

sub srcbase {
    my $self = shift;
    $self->{ST_SRCBASE} = shift if @_;
    return $self->{ST_SRCBASE};
}

sub dstbase {
    my $self = shift;
    if (@_) {
	my $dbase = shift;
	$self->{ST_DSTBASE} = $dbase;
	my $dvob = ClearCase::Argv->desc(['-s'], "vob:$dbase")->qx;
	chomp $dvob;
	$self->dstvob($dvob);
    }
    return $self->{ST_DSTBASE};
}

# We may have created a view-private parent tree above, so
# have to work our way upwards till we get to a versioned dir.
sub _mkbase {
    my $self = shift;
    if (! $self->{ST_MKBASE}) {
	my $mbase = $self->dstbase;
	my $dvob = $self->dstvob;
	my $ct = $self->clone_ct({-stdout=>0, -stderr=>0});
	while (1) {
	    last if length($mbase) <= length($dvob);
	    last if -d $mbase &&
			! $ct->argv('desc', ['-s'], "$mbase@@")->system;
	    $mbase = dirname($mbase);
	}
	$self->{ST_MKBASE} = $mbase;
    }
    return $self->{ST_MKBASE};
}

sub dstvob {
    my $self = shift;
    $self->{ST_DSTVOB} = shift if @_;
    return $self->{ST_DSTVOB};
}

sub lbtype {
    my $self = shift;
    $self->{ST_LBTYPE} = shift if @_;
    return $self->{ST_LBTYPE};
}

sub no_cr {
    my $self = shift;
    $self->{ST_NO_CR} = 1 if $_[0] || !defined(wantarray);
    return $self->{ST_NO_CR};
}

sub no_cmp {
    my $self = shift;
    $self->{ST_NO_CMP} = 1 if $_[0] || !defined(wantarray);
    return $self->{ST_NO_CMP};
}

sub srclist {
    my $self = shift;
    my $type = ref($_[0]) ? ${shift @_} : 'NORMAL';
    my $sbase = $self->srcbase;
    die "Error: must specify src base before src list" if !$sbase;
    for (@_) {
	next if $_ eq $sbase;
	if (m%^(?:[a-zA-Z]:)?$sbase[/\\]*(.+)%) {
	    $self->{ST_SRCMAP}->{$1}->{type} = $type;
	} elsif (-e "$sbase/$_") {
	    $self->{ST_SRCMAP}->{$_}->{type} = $type;
	} else {
	    warn "Warning: $_: no such file or directory\n";
	}
    }
}

sub srcmap {
    my $self = shift;
    my $type = ref($_[0]) ? ${shift @_} : 'NORMAL';
    my %sdmap = @_;
    my $sbase = $self->srcbase;
    my $dbase = $self->dstbase;
    die "Error: must specify src base before src map" if !$sbase;
    die "Error: must specify dst base before src map" if !$dbase;
    for (keys %sdmap) {
	if (m%^(?:[a-zA-Z]:)?$sbase[/\\]*(.+)$%) {
	    my $key = $1;
	    $self->{ST_SRCMAP}->{$key}->{type} = $type;
	    my($dst) = ($sdmap{$_} =~ m%^(?:[a-zA-Z]:)?$dbase[/\\]*(.+)$%);
	    $self->{ST_SRCMAP}->{$key}->{dst} = $dst;
	} elsif (-e $_) {
	    $self->{ST_SRCMAP}->{$_}->{type} = $type;
	    if ($sdmap{$_} =~ m%^(?:[a-zA-Z]:)?$dbase[/\\]*(.+)$%) {
		$self->{ST_SRCMAP}->{$_}->{dst} = $1;
	    } else {
		$self->{ST_SRCMAP}->{$_}->{dst} = $sdmap{$_};
	    }
	} elsif (-e "$sbase/$_") {
	    $self->{ST_SRCMAP}->{$_}->{type} = $type;
	    $self->{ST_SRCMAP}->{$_}->{dst} = $sdmap{$_};
	} else {
	    warn "Warning: $_: no such file or directory\n";
	}
    }
}

sub analyze {
    my $self = shift;
    my $type = ref($_[0]) ? ${shift @_} : 'NORMAL';
    my $sbase = $self->srcbase;
    my $dbase = $self->dstbase;
    die "Error: must specify dest base before analyzing" if !$dbase;
    die "Error: must specify dest vob before analyzing" if !$self->dstvob;
    $self->_mkbase;
    my $ct = $self->clone_ct({-autochomp=>0});
    if (-e $dbase) {
	my @vp = grep m%^$dbase%,
			$ct->argv('lsp', [qw(-oth -s -inv), $dbase])->qx;
	die "Error: view-private files exist under $dbase:\n @vp\n" if @vp;
    }
    delete $self->{ST_ADD};
    delete $self->{ST_MOD};
    for (sort keys %{$self->{ST_SRCMAP}}) {
	next if $self->{ST_SRCMAP}->{$_}->{type} &&
		$self->{ST_SRCMAP}->{$_}->{type} !~ /$type/;
	my $src = join('/', $sbase, $_);
	$src = $_ if ! -e $src;
	my $dst = join('/', $dbase, $self->{ST_SRCMAP}->{$_}->{dst} || $_);
	if (! -e $dst) {
	    $self->{ST_ADD}->{$_}->{src} = $src;
	    $self->{ST_ADD}->{$_}->{dst} = $dst;
	} else {
	    my $update = $self->{ST_NO_CMP} || compare($src, $dst);
	    if ($update) {
		warn "Warning: error comparing $src vs $dst" if $update < 0;
		$self->{ST_MOD}->{$_}->{src} = $src;
		$self->{ST_MOD}->{$_}->{dst} = $dst;
	    }
	}
    }
}

sub preview {
    my $self = shift;
    my $fmt = "   %s -> %s\n";
    print "Adding:\n" if $self->{ST_ADD};
    for (keys %{$self->{ST_ADD}}) {
	printf $fmt, $self->{ST_ADD}->{$_}->{src}, $self->{ST_ADD}->{$_}->{dst};
    }
    print "Modifying:\n" if $self->{ST_MOD};
    for (keys %{$self->{ST_MOD}}) {
	printf $fmt, $self->{ST_MOD}->{$_}->{src}, $self->{ST_MOD}->{$_}->{dst};
    }
}

sub add {
    my $self = shift;
    my $sbase = $self->srcbase;
    my $mbase = $self->_mkbase;
    my $ct = $self->clone_ct;
    return if ! $self->{ST_ADD};
    for (sort keys %{$self->{ST_ADD}}) {
	my $src = $self->{ST_ADD}->{$_}->{src};
	my $dst = $self->{ST_ADD}->{$_}->{dst};
	if (-d $src) {
	    -e $dst || mkpath($dst, 0, 0777) || die "Error: $dst: $!";
	} elsif (-e $src) {
	    my $dad = dirname($dst);
	    -d $dad || mkpath($dad, 0, 0777) || die "Error: $dad: $!";
	    copy($src, $dst) || die "Error: $_: $!\n";
	    utime(time(), (stat $src)[9], $dst) ||
		    warn "Warning: $dst: touch failed";
	    $self->{ST_CI_FROM}->{$_} = $self->{ST_ADD}->{$_};
	} else {
	    warn "Error: $src: no such file or directory\n";
	    $ct->fail;
	}
    }
    my @candidates = sort grep m%^$mbase%,
			$ct->argv('lsp', [qw(-oth -s -inv), $mbase])->qx;
    return if !@candidates;
    # We'll be separating the elements-to-be into files and directories.
    my(@files, %dirs);
    # If the parent directories of any of the candidates are
    # already versioned, we'll need to check them out unless
    # it's already been done.
    for (@candidates) {
	my $dad = dirname($_);
	next if ! $dad || $dirs{$dad};
	my $lsd = $ct->argv('ls', ['-d'], $dad)->qx;
	# No /Rule:/ means it's a view-private dir to be handled below
	next unless $lsd =~ /\sRule:\s/;
	# If already checked out, nothing to do.
	next if $lsd =~ /CHECKEDOUT$/;
	# Now we know it's an element and needs to be checked out.
	$dirs{$dad}++;
    }
    $ct->argv('co', $self->comment, keys %dirs)->system if keys %dirs;
    # Process candidate directories here, then do files below.
    for my $cand (@candidates) {
	if (! -d $cand) {
	    push(@files, $cand);
	    next;
	}
	# Now we know we're dealing with directories.  These cannot
	# exist at mkelem time so we move them aside, make
	# a versioned dir, then move all the files from the original
	# back into the new dir (still as view-private files).
	my $tmpdir = "$cand.$$.keep.d";
	if (!rename($cand, $tmpdir)) {
	    warn "Error: $cand: $!\n";
	    $ct->fail;
	    next;
	}
	if ($ct->autofail(0)->argv('mkdir', $self->comment, $cand)->system) {
	    rename($tmpdir, $cand);
	    $ct->fail;
	    next;
	}
	if (!opendir(DIR, $tmpdir)) {
	    warn "Error: $tmpdir: $!";
	    $ct->fail;
	    next;
	}
	while (defined(my $i = readdir(DIR))) {
	    next if $i eq '.' || $i eq '..';
	    rename("$tmpdir/$i", "$cand/$i") || die "Error: $cand/$i: $!";
	}
	closedir DIR;
	rmdir $tmpdir || warn "Error: $tmpdir: $!";
    }
    $ct->argv('mkelem', $self->comment, @files)->system if @files;
}

sub modify {
    my $self = shift;
    return if !keys %{$self->{ST_MOD}};
    my $dbase = $self->dstbase;
    my $co = $self->clone_ct('co', $self->comment);
    $co->args(map {$self->{ST_MOD}->{$_}->{dst}} keys %{$self->{ST_MOD}});
    $co->system;
    for (keys %{$self->{ST_MOD}}) {
	$self->{ST_CI_FROM}->{$_} = $self->{ST_MOD}->{$_} if !$self->no_cr;
	my $src = $self->{ST_MOD}->{$_}->{src};
	my $dst = $self->{ST_MOD}->{$_}->{dst};
	if (!copy($src, $dst)) {
	    warn "Error: $dst: $!\n";
	    $co->fail;
	    next;
	}
	utime(time(), (stat $src)[9], $dst) ||
				    warn "Warning: $dst: touch failed";
    }
}

sub subtract {
    my $self = shift;
    my $dbase = $self->dstbase;
    my $ct = $self->clone_ct;
    my %checkedout = map {$_ => 1} grep m%^$dbase%,
		    $ct->argv('lsco', [qw(-s -cvi -a)], $dbase)->qx;
    my(%dirs, %files, @exfiles, @exdirs);
    my $wanted = sub {
	my $path = $File::Find::name;
	return if $path eq $dbase;
	if ($path =~ /lost\+found/) {
	    $File::Find::prune = 1;
	    return;
	}
	# Get a relative path from the absolute path.
	(my $relpath = $path) =~ s%^$dbase\W?%%;
	if (-d $path) {
	    $dirs{$path} = 1;
	} elsif (-f $path) {
	    $files{$relpath} = $path;
	}
    };
    find($wanted, $dbase);

    my %dst2src;
    for (keys %{$self->{ST_SRCMAP}}) {
	my $dst = $self->{ST_SRCMAP}->{$_}->{dst};
	$dst2src{$dst} = $_ if $dst;
    }
    for (keys %files) {
	next if $self->{ST_SRCMAP}->{$_} && !$self->{ST_SRCMAP}->{$_}->{dst};
	push(@exfiles, $files{$_}) if !$dst2src{$_};
    }
    my $r_cmnt = $self->comment;
    for my $dad (map {dirname($_)} @exfiles) {
	$ct->argv('co', $r_cmnt, $dad)->system if !$checkedout{$dad}++;
    }
    $ct->argv('rm', $r_cmnt, @exfiles)->system if @exfiles;
    while (1) {
	for (sort {$b cmp $a} keys %dirs) {
	    if (opendir(DIR, $_)) {
		my @entries = readdir DIR;
		closedir(DIR);
		next if @entries > 2;
		push(@exdirs, $_);
		delete $dirs{$_};
	    }
	}
	last if !@exdirs;
	for my $dad (map {dirname($_)} @exdirs) {
	    $ct->argv('co', $r_cmnt, $dad)->system if !$checkedout{$dad}++;
	}
	if (my @co = $ct->argv('lsco', [qw(-s -cvi -d)], @exdirs)->qx) {
	    $ct->argv('ci', $r_cmnt, @co)->system;
	}
	$ct->argv('rm', $r_cmnt, @exdirs)->system;
	@exdirs = ();
    }
}

sub label {
    my $self = shift;
    my $lbtype = $self->lbtype;
    return unless $lbtype;
    my $dbase = $self->dstbase;
    my $ct = $self->clone_ct;
    my $ctq = $ct->clone({-stdout=>0});
    my $ctbool = $ctq->clone({-autofail=>0, -stderr=>0});
    my $locked;
    if ($ctbool->lstype(['-s'], "lbtype:$lbtype\@$dbase")->system) {
	$ct->mklbtype(['-nc'], "lbtype:$lbtype\@$dbase")->system;
    } else {
	$locked = $self->clone_ct->lslock(['-s'], "lbtype:$lbtype\@$dbase")->qx;
	$ct->unlock("lbtype:$lbtype\@$dbase")->system if $locked;
    }
    $ctq->mklabel([qw(-rep -rec -nc), $lbtype], $dbase)->system;
    # Last, label the ancestors of the destination back to the vob tag.
    my $dvob = $self->dstvob;
    my($dad, @ancestors);
    for ($dad = dirname($dbase);
		    length($dad) >= length($dvob); $dad = dirname($dad)) {
	push(@ancestors, $dad);
    }
    $ctq->mklabel([qw(-rep -nc), $lbtype], @ancestors)->system if @ancestors;
    $self->clone_ct->lock("lbtype:$lbtype\@$dbase")->system if $locked;
}

sub checkin {
    my $self = shift;
    my $mbase = $self->_mkbase;
    my $dad = dirname($mbase);
    my $ct = $self->clone_ct;
    # Do one-by-one ci's with -from (to preserve CR's) unless
    # otherwise requested.
    if (! $self->no_cr) {
	$ct->argv('ci', [qw(-nc -ide -pti -rm -from)]);
	for (keys %{$self->{ST_CI_FROM}}) {
	    my $src = $self->{ST_CI_FROM}->{$_}->{src};
	    my $dst = $self->{ST_CI_FROM}->{$_}->{dst};
	    $ct->args($src, $dst)->system;
	}
    }
    # Check in anything not handled above.
    my %allco = map {$_ => 1} $ct->argv('lsco', [qw(-s -cvi -a)], $mbase)->qx;
    my @co = grep m%^$mbase%, keys %allco;
    unshift(@co, $dad) if $allco{$dad} || $allco{"$dad/."};
    $ct->argv('ci', [qw(-nc -ide -pti)], sort @co)->system if @co;
    # Fix the protections of the target files if requested.
    if ($self->protect) {
	my %perms;
	for (keys %{$self->{ST_ADD}}) {
	    my $src = $self->{ST_ADD}->{$_}->{src};
	    my $dst = $self->{ST_ADD}->{$_}->{dst};
	    my $src_mode = (stat $src)[2];
	    my $dst_mode = (stat $dst)[2];
	    # 07551 represents the only bits that matter to clearcase
	    if (($src_mode & 07551) ne ($dst_mode & 07551) &&
		    $src !~ m%\.(?:p|html?|gif|mak|rc|ini|java|
				c|cpp|cxx|h|bmp|ico)$|akefile%x) {
		my $sym = sprintf("%o", ($src_mode & 07775) | 0444);
		push(@${$perms{$sym}}, $dst);
	    }
	}
	for (keys %{$self->{ST_MOD}}) {
	    my $src = $self->{ST_MOD}->{$_}->{src};
	    my $dst = $self->{ST_MOD}->{$_}->{dst};
	    my $src_mode = (stat $src)[2];
	    my $dst_mode = (stat $dst)[2];
	    # 07551 represents the only bits that matter to clearcase
	    if (($src_mode & 07551) ne ($dst_mode & 07551) &&
		    $src !~ m%\.(?:p|html?|gif|mak|rc|ini|java|
				c|cpp|cxx|h|bmp|ico)$|akefile%x) {
		my $sym = sprintf("%o", ($src_mode & 07775) | 0444);
		push(@${$perms{$sym}}, $dst);
	    }
	}
	for (keys %perms) {
	    $ct->argv('protect', ['-chmod', $_], @${$perms{$_}})->system;
	}
    }
}

sub cleanup {
    my $self = shift;
    my $mbase = $self->_mkbase;
    my $dad = dirname($mbase);
    my $ct = $self->clone_ct({-autofail=>0});
    my @vp = grep m%^$mbase%,
			$ct->argv('lsp', [qw(-oth -s -inv), $mbase])->qx;
    for (sort {$b cmp $a} @vp) {
	if (-d $_) {
	    rmdir $_ || warn "Error: unable to remove $_\n";
	} else {
	    unlink $_ || warn "Error: unable to remove $_\n";
	}
    }
    my %allco = map {$_ => 1} $ct->argv('lsco', [qw(-s -cvi -a)], $mbase)->qx;
    my @co = grep m%^$mbase%, keys %allco;
    unshift(@co, $dad) if $allco{$dad} || $allco{"$dad/."};
    $ct->argv('unco', [qw(-rm)], sort {$b cmp $a} @co)->system if @co;
}

# Undo current work and exit. May be called from an exception handler.
sub fail {
    my $self = shift;
    my $rc = shift;
    $self->ct->autofail(0);	# avoid exception-handler loop
    $self->cleanup;
    exit defined($rc) ? $rc : 2;
}

1;

__END__

=head1 NAME

ClearCase::SyncTree - Import a tree of files to a tree of ClearCase elements

=head1 SYNOPSIS

    # Create a 'synctree' object.
    my $sync = ClearCase::SyncTree->new;
    # Tell it where the files are coming from ...
    $sync->srcbase($sbase);
    # Tell it where they're going to ...
    $sync->dstbase($dbase);
    # Supply the list of files to work on (relative or absolute paths).
    $sync->srclist(keys %files);
    # Compare src and dest lists and figure out what to do.
    $sync->analyze;
    # Create new elements in the target area.
    $sync->add;
    # Update existing files which differ between src and dest.
    $sync->modify;
    # Remove any files from dest that aren't in src.
    $sync->subtract;
    # Check in the changes.
    $sync->checkin;

See the enclosed I<synctree> script for full example usage.

=head1 DESCRIPTION

No detailed documentation yet, sorry. Next version.

=head1 AUTHOR

Based on code originally written by Paul D. Smith
<pausmith@nortelnetworks.com>.  Paul's version was based on the Bourne
shell script 'citree' delivered as sample code with ClearCase.

Rewritten for Unix/Win32 portability by David Boyce <dsb@world.std.com>
in 8/1999, then reorganized into a module in 1/2000.

=head1 COPYRIGHT

Copyright 1997,1998 Paul D. Smith and Bay Networks, Inc.

Copyright 1999,2000 David Boyce (dsb@world.std.com).

This script is distributed under the terms of the GNU General Public License.
You can get a copy via ftp://ftp.gnu.org/pub/gnu/ or its many mirrors.
This script comes with NO WARRANTY whatsoever, not even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 STATUS

This is currently ALPHA code and, as such, I reserve the right to
change the API incompatibly. At some point I'll bump the version
suitably and remove this warning, which will constitute an (almost)
ironclad promise to leave the API alone.

=head1 SEE ALSO

perl(1), ClearCase::Argv(3), Getopt::Long(3), IPC::ChildSafe(3)
