package ClearCase::SyncTree;

$VERSION = '0.21';

require 5.004;

use strict;

use File::Basename;
use File::Compare;
use File::Copy;
use File::Find;
use File::Path;

use ClearCase::Argv 0.21;

use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i;

my $lext = '.=lnk=';	# special extension for pseudo-symlinks

sub new {
    my $proto = shift;
    my($class, $self);
    if ($class = ref($proto)) {
	# Make a (deep) clone of the original
	require Data::Dumper;
	# Older versions may not have the XS version installed ...
	if (defined $Data::Dumper::Dumpxs) {
	    eval Data::Dumper->Deepcopy(1)->new([$proto], ['self'])->Dumpxs;
	} else {
	    eval Data::Dumper->Deepcopy(1)->new([$proto], ['self'])->Dump;
	}
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
    # Set default file comparator.
    $self->cmp_func(\&File::Compare::compare);
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

# For internal use only.  Returns the ClearCase::Argv object.
sub ct {
    my $self = shift;
    if (!defined(wantarray)) {
	my $ct = ClearCase::Argv->new({-autochomp=>1, -outpathnorm=>1});
	$ct->syxargs($ct->qxargs);
	$self->{ST_CT} = $ct;
    }
    return $self->{ST_CT};
}

# For internal use only.  Returns a clone of the ClearCase::Argv object.
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

sub reuse {
    my $self = shift;
    $self->{ST_RECOVER} = shift if @_;
    return $self->{ST_RECOVER};
}

sub snapdest {
    my $self = shift;
    $self->{ST_SNAPDEST} = shift if @_;
    return $self->{ST_SNAPDEST};
}

sub ctime {
    my $self = shift;
    $self->{ST_CTIME} = shift if @_;
    return $self->{ST_CTIME};
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
	if (!$self->dstvob) {
	    my $dvob = ClearCase::Argv->desc(['-s'], "vob:$dbase")->qx;
	    chomp $dvob;
	    $self->dstvob($dvob);
	}
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
	$ct->autofail(0);
	while (1) {
	    last if length($mbase) <= length($dvob);
	    last if -d $mbase &&
			! $ct->argv('desc', ['-s'], "$mbase@@")->system;
	    $mbase = dirname($mbase);
	}
	$mbase =~ s%^[a-z]:%%i if MSWIN;
	$self->{ST_MKBASE} = $mbase;
    }
    return $self->{ST_MKBASE};
}

sub dstvob {
    my $self = shift;
    if (@_) {
	$self->{ST_DSTVOB} = shift;
	$self->{ST_DSTVOB} =~ s%\\%/%g;
    }
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
    return $self->{ST_NO_CMP} || 0;
}

sub cmp_func {
    my $self = shift;
    $self->{ST_CMP_FUNC} = shift if @_;
    return $self->{ST_CMP_FUNC};
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

sub eltypemap {
    my $self = shift;
    %{$self->{ST_ELTYPEMAP}} = @_ if @_;
    return $self->{ST_ELTYPEMAP} ? %{$self->{ST_ELTYPEMAP}} : ();
}

sub analyze {
    my $self = shift;
    my $type = ref($_[0]) ? ${shift @_} : 'NORMAL';
    my $sbase = $self->srcbase;
    my $dbase = $self->dstbase;
    die "Error: must specify dest base before analyzing" if !$dbase;
    die "Error: must specify dest vob before analyzing" if !$self->dstvob;
    $self->snapdest(1) if ! -e "$dbase@@/main" && ! -e "$dbase/@@/main";
    $self->_mkbase;
    my $ct = $self->clone_ct({-autochomp=>0});
    if (-e $dbase) {
	my @vp = grep m%^$dbase[/\\]%,
			$ct->argv('lsp', [qw(-oth -s -inv), $dbase])->qx;
	die "Error: view-private files exist under $dbase:\n @vp\n" if @vp;
    }
    delete $self->{ST_ADD};
    delete $self->{ST_MOD};
    my $compare = $self->cmp_func;
    for (sort keys %{$self->{ST_SRCMAP}}) {
	next if $self->{ST_SRCMAP}->{$_}->{type} &&
		$self->{ST_SRCMAP}->{$_}->{type} !~ /$type/;
	my $src = join('/', $sbase, $_);
	$src = $_ if ! -e $src;
	my $dst = join('/', $dbase, $self->{ST_SRCMAP}->{$_}->{dst} || $_);
	if (! -e $dst && ! -l $dst) {
	    $self->{ST_ADD}->{$_}->{src} = $src;
	    $self->{ST_ADD}->{$_}->{dst} = $dst;
	} elsif (! -d $src) {
	    my $update = 0;
	    if (-l $src && -l $dst) {
		my $stxt = readlink $src;
		my $dtxt = readlink $dst;
		$update = $self->no_cmp || ($stxt ne $dtxt);
	    } elsif (! -l $src && ! -l $dst) {
		$update = $self->no_cmp || &$compare($src, $dst);
		die "Error: failed comparing $src vs $dst: $!" if $update < 0;
	    } else {
		$update = 1;
	    }
	    if ($update) {
		$self->{ST_MOD}->{$_}->{src} = $src;
		$self->{ST_MOD}->{$_}->{dst} = $dst;
	    }
	}
    }
}

sub preview {
    my $self = shift;
    my $subtracting = shift;
    my $fmt = "   %s =>\n\t%s\n";
    print "Adding:\n" if $self->{ST_ADD};
    for (keys %{$self->{ST_ADD}}) {
	printf $fmt, $self->{ST_ADD}->{$_}->{src}, $self->{ST_ADD}->{$_}->{dst};
    }
    print "Modifying:\n" if $self->{ST_MOD};
    for (keys %{$self->{ST_MOD}}) {
	printf $fmt, $self->{ST_MOD}->{$_}->{src}, $self->{ST_MOD}->{$_}->{dst};
    }
    if ($subtracting) {
	##print "Subtracting:\n" if $self->{ST_MOD};
	## This part is difficult and I haven't gotten to it yet ...
    }
    return keys(%{$self->{ST_ADD}}) + keys(%{$self->{ST_MOD}});
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
	if (-d $src && ! -l $src) {
	    -e $dst || mkpath($dst, 0, 0777) || die "Error: $dst: $!";
	} elsif (-e $src) {
	    my $dad = dirname($dst);
	    -d $dad || mkpath($dad, 0, 0777) || die "Error: $dad: $!";
	    if (-l $src) {
		open(SLINK, ">$dst$lext") || die "Error: $dst$lext: $!";
		print SLINK readlink($src), "\n";;
		close(SLINK);
	    } else {
		copy($src, $dst) || die "Error: $_: $!\n";
		utime(time(), (stat $src)[9], $dst) ||
			warn "Warning: $dst: touch failed";
		$self->{ST_CI_FROM}->{$_} = $self->{ST_ADD}->{$_};
	    }
	} else {
	    warn "Error: $src: no such file or directory\n";
	    $ct->fail;
	}
    }
    my @candidates = $ct->argv('lsp', [qw(-oth -s -inv), $mbase])->qx;
    for (@candidates) { s%\\%/%g }
    @candidates = sort grep m%^$mbase%, @candidates;
    return if !@candidates;
    # We'll be separating the elements-to-be into files and directories.
    my(%files, @symlinks, %dirs);
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
    my $mkdir = $self->clone_ct->argv({-autofail=>0}, 'mkdir', $self->comment);
    for my $cand (@candidates) {
	if (! -d $cand) {
	    if ($cand =~ /$lext$/) {
		push(@symlinks, $cand);
	    } else {
		$files{$cand} = 1;
	    }
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
	if ($mkdir->args($cand)->system) {
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

    # Optionally, reconstitute an old element of the same name if present.
    if ($self->reuse) {
	my $snapview = $self->snapdest;
	my $vt = ClearCase::Argv->lsvtree([qw(-a -s -nco)]);
	my $ds = ClearCase::Argv->desc([qw(-s)]);
	$ds->stderr(1);
	my $ln = ClearCase::Argv->ln;
	for my $elem (keys %files) {
	    my($name, $dir) = fileparse($elem);
	    chomp(my @vtree = reverse $vt->args($dir)->qx);
	    for (@vtree) {
		if ($snapview ? $ds->args("$_/$name@@/main")->qx !~ /Error:/ :
							-e "$_/$name@@/main") {
		    delete $files{$elem};
		    unlink($elem);
		    $ln->args("$_/$name", $elem)->system;
		    last;
		}
	    }
	}
	# If any elements were "reconstituted" they may be candidates for
	# modification. Therefore, re-analyze the situation (the 'add'
	# list should be null this time through but we don't care about
	# that by this point).
	$self->analyze;
    }

    # Now do the files in one fell swoop.
    $ct->argv('mkelem', $self->comment, sort keys %files)->system if %files;

    # Deal with symlinks.
    for my $symlink (@symlinks) {
	(my $lnk = $symlink) =~ s/$lext$//;
	if (!open(SLINK, $symlink)) {
	    warn "$symlink: $!";
	    next;
	}
	chomp(my $txt = <SLINK>);
	close SLINK;
	unlink $symlink;
	$ct->argv('ln', ['-s'], $txt, $lnk)->system;
    }
}

sub modify {
    my $self = shift;
    return if !keys %{$self->{ST_MOD}};
    my(@files, @symlinks);
    for (sort keys %{$self->{ST_MOD}}) {
	if (-l $self->{ST_MOD}->{$_}->{src}) {
	    push(@symlinks, $_)
	} else {
	    push(@files, $_)
	}
    }
    my $co = $self->clone_ct('co', $self->comment);
    if (@files) {
	$co->args(map {$self->{ST_MOD}->{$_}->{dst}} @files)->system;
	for (@files) {
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
    if (@symlinks) {
	my $dbase = $self->dstbase;
	my %checkedout = map {$_ => 1}
		    $self->ct->argv('lsco', [qw(-s -cvi -a)], $dbase)->qx;
	my $ln = $co->clone->prog('ln');
	$ln->opts('-s', $ln->opts);
	my $rm = $co->clone->prog('rmname');
	for (@symlinks) {
	    my $txt = readlink $self->{ST_MOD}->{$_}->{src};
	    my $lnk = $self->{ST_MOD}->{$_}->{dst};
	    my $dad = dirname($lnk);
	    if (!$checkedout{$dad}) {
		$checkedout{$dad} = 1 if ! $co->args($dad)->system;
	    }
	    $rm->args($lnk)->system;
	    $ln->args($txt, $lnk)->system;
	}
    }
}

sub subtract {
    my $self = shift;
    my $dbase = $self->dstbase;
    my $ct = $self->clone_ct;
    my %checkedout = map {$_ => 1}
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
    my $lbtype = shift || $self->lbtype;
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
    my @ptime = qw(-pti) unless $self->ctime;
    my $ct = $self->clone_ct;
    # If special eltypes are registered, chtype them here.
    if (my %emap = $self->eltypemap) {
	for my $re (keys %emap) {
	    my @chtypes = grep {/$re/} map {$self->{ST_ADD}->{$_}->{dst}}
				       keys %{$self->{ST_ADD}};
	    next unless @chtypes;
	    $ct->argv('chtype', [qw(-nc -f), $emap{$re}], @chtypes)->system;
	}
    }
    # Do one-by-one ci's with -from (to preserve CR's) unless
    # otherwise requested.
    if (! $self->no_cr) {
	$ct->argv('ci', [@ptime, qw(-nc -ide -rm -from)]);
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
    $ct->argv('ci', [qw(-nc -ide), @ptime], sort @co)->system if @co;
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

ClearCase::SyncTree - Synchronize a tree of files with a tree of elements

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

This module provides an infrastructure for programs which want to
I<synchronize> a set of files, typically a subtree, with a similar
destination subtree in VOB space.  The enclosed I<synctree> script is
an example of such a program.

The source area may be in a VOB or may be a regular filesystem; the
destination area must be in a VOB. Methods are supplied for adding,
subtracting, and modifying destination files so as to make that area
look identical to the source.

Symbolic links are supported, even on Windows (of course in this case
the source filesystem must support them, which is only likely in the
event of an MVFS->MVFS transfer). Note that the text of the link is
transported verbatim from source area to dest area; thus relative
symlinks may no longer resolve in the destination.

=head2 CONSTRUCTOR

Use C<ClearCase::SyncTree-E<gt>new> to construct a SyncTree object, which
can then be filled in and used via the instance methods below.

=head2 INSTANCE METHODS

Following is a brief description of each supported method. Examples
are given for all methods that take parameters; if no example is
given usage may be assumed to look like:

    $obj->method;

=over 4

=item * -E<gt>srcbase

Provides the base by which to 'relativize' the incoming pathnames.
E.g.  with a B<srcbase> of I</tmp/x> the incoming file I</tmp/x/y/z>
will become I<y/z> and will be deposited under the B<dstbase> (see) by
that path. Example:

    $obj->srcbase('/var/tmp/newstuff');

=item * -E<gt>dstbase

Provides the root of the tree into which to place the relative paths
derived from B<srcbase> as described above. Example:

    $obj->dstbase('/vobs/tps/newstuff');

=item * -E<gt>srclist/-E<gt>srcmap

There are two ways to specify the list of incoming files. They may be
provided as a simple list via B<srclist>, in which case they'll be
relativized as described above and deposited in B<dstbase>, or they can
be specified via B<srcmap> which allows the destination file to have a
different name from the source.

I<srclist> takes a list of input filenames. These may be absolute or relative;
they will be canonicalized and then relativized internally.

I<srcmap> is similar but takes a hash mapping input filenames to
their destination counterparts.

Examples:

    $obj->srclist(@ARGV);	# check in the named files

    my %filemap = (x/y/z.c => 'x/y/z.cxx', a/b => 'foo');
    $obj->srcmap(%filemap);	# check in the named files

=item * -E<gt>analyze

After the object knows its I<srcbase>, I<dstbase>, and input file
lists, this method compares the source and target trees and categorizes
the required actions into I<additions> (new files in the destination
area), I<modifications> (those which exist but need to be updated) and
I<subtractions> (those which no longer exist in the source area).
After analysis is complete, these actions may be taken via the I<add>,
I<modify>, and I<subtract> methods as desired.

=item * -E<gt>add

Takes the list of I<additions> as determined by the B<analyze> method
and creates them as new elements.

=item * -E<gt>modify

Takes the list of I<modifications> as determined by the B<analyze>
method and updates them in the destination tree.

=item * -E<gt>subtract

Takes the list of I<subtractions> as determined by the B<analyze>
method and rmname's them in the destination tree.

=item * -E<gt>label

Labels the new work. The label type can be specified as a parameter;
otherwise it will be taken from the attribute previously set by the
I<lbtype> method.

Labeling consists of a I<mklabel -recurse> from I<dstbase> down,
followed by labeling of parent directories from I<dstbase> B<up> to the
vob root. Example:

    $obj->label('FOO');

=item * -E<gt>checkin

Checks in all checkouts under the I<dstbase> area.

=item * -E<gt>cleanup

Undoes all checkouts under the I<dstbase> area.

=item * -E<gt>fail

Calls the I<cleanup> method, then exits with a failure status. This is
the default exception handler; a different handler can be registered
via the I<err_handler> method (see).

=item * -E<gt>err_handler

Registers an exception handler to be called upon failure of any
cleartool command. Call with 0 to have no handler Pass it a code ref
to register a function, with an object and method I<name> to
register a method. Examples:

    $obj->err_handler(0);		# ignore cleartool errors
    $obj->err_handler(\&func);		# register func() for errors
    $obj->err_handler($self, 'method');	# register $obj->method

=item * -E<gt>protect

Sets an attribute which causes the I<checkin> method to align file
permissions after checking in. By default this attribute is set on
UNIX, unset on Windows. Example:

    $obj->protect(0);

=item * -E<gt>reuse

Attempt "element reuse". Before creating a new file with I<mkelem>,
look through its directory's version tree to see if another of the same
name exists in any other version. If so, assume the new file intended
to be the same element and link the old and new names.

    $obj->reuse(1);

=item * -E<gt>ctime

Sets a boolean indicating whether to throw away the timestamp of the
source file and give modified files their checkin date instead. This
flag is I<false> by default (i.e. checkins have I<-ptime> behavior).

=item * -E<gt>no_cr

By default, checkins initiated by the I<checkin> method are done one at
a time using the I<-from> flag. This will preserve config records in
the case where the input file is a derived object.  Setting the
I<no_cr> attribute causes checkins to be done in one big C<"cleartool
ci"> operation, which is faster but loses CR's.

=item * -E<gt>no_cmp

This attribute causes all files which exist in both src and dest areas
to be considered modified by the I<analyze> method.

=item * -E<gt>cmp_func

Sets or returns the coderef that's used to compare the source and
destination files. The default is I<File::Compare::compare()> but can be
modifed by passing in a ref to your preferred function, like so:

    $obj->cmp_func(\&my_compare_function);

A replacement function should set C<$!> on failure.

=item * -E<gt>comment

Provides a comment to be used by the I<checkin> method. The default
comment is C<"By:$0">. Example:

    $obj->comment("your comment here");

=item * -E<gt>eltypemap

In case the eltype of a particular file or set of files needs to be
overridden at creation time. Example:

    $obj->eltypemap('\.(ht|x)ml$' => 'compressed_file');

=back

=head1 BUGS

=over 4

=item *

Subtraction of symlinks is currently unimplemented (it's just a little
corner case I haven't gotten to).

=item *

If a file is removed via the -E<gt>subtract method and later added back
via -E<gt>add, the result will be a new element (aka I<evil twin>).
SyncTree does not have the smarts to find the old element of the same
name and relink to it. This could be handled but I haven't yet needed
it.

=item *

I have not tested SyncTree in snapshot views and would not expect it to
work out of the box, though the fixes would be largely mechanical.  At
the least, though, expect it to be slower in a snapshot view.

=back

=head1 AUTHOR

Based on code originally written by Paul D. Smith
<pausmith@nortelnetworks.com>.  Paul's version was based on the Bourne
shell script 'citree' delivered as sample code with ClearCase.

Rewritten for Unix/Win32 portability by David Boyce <dsb@world.std.com>
in 8/1999, then reorganized into a module in 1/2000. This module no
longer bears the slightest resemblance to citree.

=head1 COPYRIGHT

Copyright 1997,1998 Paul D. Smith and Bay Networks, Inc.

Copyright 1999,2000 David Boyce (dsb@world.std.com).

This script is distributed under the terms of the GNU General Public License.
You can get a copy via ftp://ftp.gnu.org/pub/gnu/ or its many mirrors.
This script comes with NO WARRANTY whatsoever, not even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 STATUS

This is currently ALPHA code and thus I reserve the right to change the
API incompatibly. At some point I'll bump the version suitably and
remove this warning, which will constitute an (almost) ironclad promise
to leave the interface alone.

=head1 PORTING

This module is known to work on Solaris 2.6-7 and Windows NT 4.0SP3-5,
and with perl 5.004_04 and 5.6.  As these platforms cover a fairly wide
range there should be no I<major> portability issues, but please send
bug reports or patches to the address above.

=head1 SEE ALSO

perl(1), ClearCase::Argv(3), Getopt::Long(3), IPC::ChildSafe(3)
