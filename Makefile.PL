use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'		=> 'ClearCase::SyncTree',
    'VERSION_FROM'	=> 'SyncTree.pm',
    'EXE_FILES'		=> [ 'synctree' ],
    'PREREQ_PM'		=> {
	Argv		=> 0.53,
	ClearCase::Argv	=> 0.23,
    },
    'dist'		=> {
	COMPRESS	=>   'gzip --force --best',
	TAR		=>   'gtar',
	TARFLAGS	=>   '--mode=u+w -cvf',
    },
);
