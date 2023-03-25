#!/usr/bin/env perl
# Internal functions for Ultimate DOS Boot CD (UDBCD)
# (c) e1z0 2023, e1z0@vintage2000.org
package udbcd;
use File::Basename;
use File::Copy;
my $settingsfile = "settings.ini";
my $menufile     = "default.pcm";
my $conf;
my $appsconf;
my $debug = 0;

sub currDir {
    $scriptdir = dirname($0);
}

sub iniRead {
    my $ini = $_[0];
    my $conf;
    my $section;
    open( INI, "$ini" ) || die "Can't open $ini: $!\n";
    while (<INI>) {
        chomp;
        if (/^\s*\[\s*(.+?)\s*\]\s*$/) {
            $section = $1;
        }

        if (/^\s*([^=]+?)\s*=\s*(.*?)\s*$/) {
            $conf->{$section}->{$1} = $2;
        }
    }
    close(INI);
    return $conf;
}

sub backupFile {
    my ($file)    = @_;
    my @now       = localtime();
    my $timeStamp = sprintf(
        "%04d%02d%02d%02d%02d%02d",
        $now[5] + 1900,
        $now[4] + 1,
        $now[3], $now[2], $now[1], $now[0]
    );
    my $dir    = currDir();
    my $bakdir = "$dir/../bak";
    if ( !-d $bakdir ) {
        print "Backup directory does not exist, creating...\n";
        mkdir($bakdir);
    }
    my $to = $bakdir . "/" . basename($file) . "-" . $timeStamp;
    print "Backing up file $file to: $to\n";
    copy( $file, $to );
}

sub iniWrite {
    my $ini      = $_[0];
    my $conf     = $_[1];
    my $contents = '';
    foreach my $section (
        sort { ( ( $b eq '_' ) <=> ( $a eq '_' ) ) || ( $a cmp $b ) }
        keys %$conf
      )
    {
        my $block = $conf->{$section};
        $contents .= "\n" if length $contents;
        $contents .= "[$section]\n" unless $section eq '_';
        foreach my $property ( sort keys %$block ) {
            $contents .= "$property=$block->{$property}\n";
        }
    }
    open( CONF, "> $ini" ) or print("not open the file");
    print CONF $contents;
    close CONF;
}

sub readSettings {
    my ($settingsfile) = @_;
    my $conf;
    if ( !-e $settingsfile ) {
        if ( -e "../" . $settingsfile ) {
            $settingsfile = "../" . $settingsfile;
        }
    }
    if ( !-e $settingsfile ) {
        print "Settings file: $settingsfile couldn't be found, exiting...\n";
        exit 1;
    }
    print "Settings file found at: $settingsfile\n";
    if ( $conf = iniRead($settingsfile) ) {
        print "Settings have been loaded!\n";
    }
    else {
        print "Unable to load settings from $settingsfile\n";
        exit 1;
    }
    return $conf;
}

sub parseCfgDir {
    my ($dir) = @_;
    my @cfgs = ();
    opendir( DH, $dir );
    my @files = readdir(DH);
    closedir(DH);
    @files = sort { $a cmp $b }
      @files;    # a little bit sorting is required to fix this mess

    foreach my $file (@files) {

        # skip . and ..
        next if ( $file =~ /^\.$/ );
        next if ( $file =~ /^\.\.$/ );

        if ( $file =~ /\.ini$/ ) {

            #print $file . "\n";
            push( @cfgs, iniRead( $dir . "/" . $file ) );
        }

    }
    return @cfgs;
}

sub writeToFile {
    my ( $file, $text ) = @_;
    open( my $fh, '>>', $file ) or die "Could not open file $file";
    print $fh $text;
    close $fh;
}

# function cuts the filename to 8.3 (dos format)
sub cutfname {
    my ($s) = @_;
    $s = lc($s);
    $s =~ s/\s*$//;
    $s =~ tr/ //ds;
    if ( $s !~ /\./ ) {
        $s =~ /(.{1,8})(.{0,3})/;
        return ( $2 ne "" ) ? "$1.$2" : $1;
    }
    $s =~ /(.*)\.(.*)/;
    my $base = $1;
    my $ext  = $2;
    if ( $base eq "" ) { return conv("_$ext"); }
    $base =~ s/(.{8}).*/$1/;
    $base =~ s/\./_/g;
    $ext  =~ s/(.{3}).*/$1/;
    return ( $ext ne "" ) ? "$base.$ext" : $base;
}

# read all file to array
sub readFile {
    my ($file) = @_;
    my @text = ();
    open( FH, '<', $file ) or die "Could not open file $file";
    while (<FH>) {
        push( @text, $_ );
    }
    close(FH);
    return @text;
}

# only read first line
sub readDiz {
    my ( $diz, @info ) = @_;
    if ( !-e $diz ) {
        return @info;
    }
    open( FH, $diz );
    my $text = <FH>;
    chomp($text);
    push @info, $text;
    close(FH);
    return @info;
}

sub selectOption {
    my ( $queryString, @options ) = @_;

    # First item is the query string, so shift it
    # from the inputs and save it
    my $queryString = shift @_;

    # Loop control variable;
    my $lcv;

    # User selection of choices
    my $selection;

    # Flag to indicate you have the correct input
    my $notComplete = 1;

    # Clear some space on the screen
    print "\n" x 10;

    # Loop until you have an answer
    while ($notComplete) {

        print "-" x 40 . "\n";
        for ( $lcv = 1 ; $lcv <= scalar(@options) ; $lcv++ ) {
            printf " %4d)  %s\n", $lcv, $_[ $lcv - 1 ];
        }
        print "\n";

        # Query for a response
        print "$queryString\n";

        # Get response
        $selection = <STDIN>;

        # Remove the carriage return
        chomp($selection);

        # Check to make sure it is string of digits
        # and it is within the range of the numbers
        # If it is, clear the not complete flag
        if (   ( $selection =~ m/^\d*/ )
            && ( 0 < $selection )
            && ( scalar(@options) >= $selection ) )
        {
            $notComplete = 0;
        }

        # Else there is a error so try again
        else {
            print "\n" x 10;
            print "\nIncorrect Input.  Try again\n";
        }
    }

    # Return the index of the selected array item
    return ( $selection - 1 );
}

sub genMenu {
    my ( $apps, $menu ) = @_;

# first we need to create categories then append files with contents and build main menu

    if ( !-e $apps . "/apps.ini" ) {
        print
"Applications definition file: $apps/apps.ini not found, create it!\n";
        exit 1;
    }
    $appsconf = iniRead( $apps . "/apps.ini" );
    my $categories;
    my $main_menu;

    # group to categories or main menu
    while ( my ( $key, $value ) = each(%$appsconf) ) {
        if ( defined( %$appsconf{$key}->{"category"} )
            && %$appsconf{$key}->{"category"} ne "" )
        {
            my $cat = %$appsconf{$key}->{"category"};
            push @{ $categories->{$cat} }, %$appsconf{$key};
        }
        else {
            my $title = %$appsconf{$key}->{"title"};
            push @{ $main_menu->{$title} }, %$appsconf{$key};
        }
    }

    # check if menu file exist (remove then)
    my $mainmenufile = $menu . "/" . $menufile;
    if ( -e $mainmenufile ) {
        print "Main menu file exists: $mainmenufile, removing...\n";
        unlink($mainmenufile);
    }

    # Write menu header
    print "Generating menu header...\n";
    writeToFile( $mainmenufile, "\n.t=Ultimate DOS Boot CD\n" );
    writeToFile( $mainmenufile, ".c=27,113\n\n" );

    while ( my ( $key, $value ) = each(%$categories) ) {
        my $submenu = cutfname( $key . ".pcm" );

        print "Generating menu categories...\n";

        # Generate main menu file (categories)
        writeToFile( $mainmenufile, "$key >>\n" );
        writeToFile( $mainmenufile, "        <$submenu\n\n" );
        if ( -e $menu . "/" . $submenu ) {
            print "Found submenu: $menu/$submenu, removing...\n";
            unlink( $menu . "/" . $submenu );
        }

        print "Generating category $key header...\n";

        # Generate category header
        writeToFile( $menu . "/" . $submenu, "\n.t=$key\n" );
        writeToFile( $menu . "/" . $submenu, ".e=0\n" );
        writeToFile( $menu . "/" . $submenu, ".c=47,31\n\n" );

        # Generate back button in every category
        writeToFile( $menu . "/" . $submenu, "<-- BACK\n" );
        writeToFile( $menu . "/" . $submenu, "        <default.pcm\n\n" );

        print "Generating category $key files...\n";

        # Generate categories files
        foreach my $program (@$value) {
            if ( ref($program) eq "HASH" ) {

                #print "type of var: " . ref($program) . "\n";
                my $dospath = lc( $program->{path} . "/" . $program->{exe} );
                $dospath =~ s{/}{\\}g;
                writeToFile( $menu . "/" . $submenu, $program->{title} . "\n" );
                writeToFile( $menu . "/" . $submenu,
                    "        %cdrom%" . $dospath . "\n" );

                writeToFile( $menu . "/" . $submenu, "        mpause\n" );
                writeToFile( $menu . "/" . $submenu, "        /\n" );
            }
        }

    }

    print "Finishing generating main menu...\n";

    # Generate single items for the main menu
    while ( my ( $key, $value ) = each(%$main_menu) ) {
        foreach my $program (@$value) {
            if ( ref($program) eq "HASH" ) {
                my $dospath = lc( $program->{path} . "/" . $program->{exe} );
                $dospath =~ s{/}{\\}g;
                writeToFile( $mainmenufile, $program->{title} . "\n" );
                writeToFile( $mainmenufile,
                    "        %cdrom%" . $dospath . "\n" );

                writeToFile( $mainmenufile, "        mpause\n" );
                writeToFile( $mainmenufile, "        /\n" );
            }
        }
    }

    writeToFile( $mainmenufile, "<-- QUIT\n" );
    writeToFile( $mainmenufile, "        exit\n\n" );

    print "Converting menu files to the dos format (unix2dos)...\n";
    my $cmd = "perl -i -pe 's/\\012/\\015\\012/g' $menu/*.pcm";
    print "Executing: $cmd\n" if ( $debug == 1 );
    system($cmd);
    if ( $? != 0 ) {
        print "Command failed to execute: $!\n";
        return;
    }
    print "Menu generation complete!\n";

}

sub genTemplates {
    my ($apps) = @_;
    if ( -e $apps . "/apps.ini" ) {
        print "Application definitions found, loading...\n";
        $appsconf = iniRead( $apps . "/apps.ini" );
    }
    else {
        print "Application definitions not found, will create then now!\n";
    }

    search_all_folder( $apps, $apps, 0 );
    print Dumper \%files if $debug == 1;

    foreach my $key ( keys %files ) {
        my $item = $key;
        my $path = $files{$key}{"origpath"};
        my @info = ();
        my $exe  = "";
        my $nfo  = "";

        if ( defined( $files{$key}{info} )
            && scalar( @{ $files{$key}{info} } ) gt 0 )
        {
            foreach my $diz ( @{ $files{$key}{info} } ) {
                @info = readDiz( $diz, @info ) if ( $diz ne "" );
            }
            $nfo = join( ' ', @info );
        }
        if ( scalar( @{ $files{$key}{files} } ) gt 1 ) {
            my $answer = selectOption(
                "Select which executable is the main for application $item",
                @{ $files{$key}{files} } );
            $exe = $files{$key}{files}[$answer];
        }
        else {
            $exe = $files{$key}{files}[0];
        }

        if ( defined( $appsconf->{$item} ) ) {
            print "Application definition for $item already found\n";
            next if ( $overwrite ne "1" );
            print "Overwrite mode enabled, overriding the definition...\n";
        }

        # check if file exists at all, if not then undefine the template
        $appsconf->{$item}->{"title"} = $item
          if ( defined( $appsconf->{$item}->{"title"} )
            && $appsconf->{$item}->{"title"} eq ""
            && $item ne "" );
        $appsconf->{$item}->{"exe"}  = basename($exe);
        $appsconf->{$item}->{"path"} = dirname($exe);
        $appsconf->{$item}->{"info"} = $nfo if ( $nfo ne "" );
    }
    backupFile( $apps . "/apps.ini" );
    print "Writing application definitions to the file...\n";
    iniWrite( $apps . "/apps.ini", $appsconf );
}

# scan only one level
sub search_all_folder {
    my ( $folder, $original_path, $level ) = @_;
    return if ( $level gt 1 );
    $level++;
    if ( -d $folder ) {

        #chdir $folder;
        opendir my $dh, $folder or die "can't open the directory: $folder $!";
        while ( defined( my $file = readdir($dh) ) ) {
            chomp $file;
            next if $file eq '.' or $file eq '..';
            if (   $file =~ m/\.exe$/i
                or $file =~ m/\.com$/i
                or $file =~ m/\.diz$/i )
            {
                my $path     = "$folder/$file";
                my $realpath = $path;
                my $item     = basename($folder);
                $files{$item}->{"origpath"} = $folder;
                if ( $path =~ /\Q$original_path\E/ ) {
                    $path =~ s/\Q$original_path\E/\/apps/g;
                }
                if ( $file =~ m/\.exe$/i or $file =~ m/\.com$/i ) {
                    push @{ $files{$item}{"files"} }, $path;
                }
                elsif ( $file =~ m/\.diz$/i ) {
                    push @{ $files{$item}{"info"} }, $realpath;
                }
            }
            search_all_folder( "$folder/$file", $original_path, $level )
              ;    ## recursive call
        }
        closedir $dh or die "can't close directory: $!";
    }
}
1;
