use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'ClearCase::SyncTree',
    'VERSION_FROM'	=> 'SyncTree.pm',
    'EXE_FILES'		=> [ 'synctree' ],
    'PREREQ_PM'		=> {
	Argv		=> 1.00,
	ClearCase::Argv	=> 1.00,
	File::Spec	=> 0.82,
    },
    'dist'		=> {
	COMPRESS	=>   'gzip --force --best',
	PREOP		=>   'pod2html synctree > SYNCTREE.html',
	TAR		=>   'gtar',
	TARFLAGS	=>   '--mode=u+w -cvf',
    },
);
