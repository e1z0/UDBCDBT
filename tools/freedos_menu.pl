#!/usr/bin/env perl
# This script generates internal FreeDOS menus with additional options
# (c) 2023 e1z0, e1z0@vintage2000.org

use warnings;
use strict;
use Getopt::Long;
use File::Basename;
use Data::Dumper;
use lib dirname(__FILE__);
use udbcd;

my $apps;
my $menu;
my $scan      = '';
my $overwrite = '';
my %files;

my %opt = ( "help" => 0, );

GetOptions(
    \%opt,
    'apps=s'    => \$apps,
    'menu=s'    => \$menu,
    'scan'      => \$scan,
    'overwrite' => \$overwrite,
    'help|h',
);

sub usage {

    # Shown with --help option passed
    print "\n"
      . "   freedos_menu.pl Ultimate DOS Boot CD automatic menu generation script for FreeDOS\n"
      . "   Created by e1z0(e1z0\@vintage2000.org) - Licensed under BSD License\n"
      . "\n"
      . "   Options\n"
      . "      --apps            Path to apps folder where are the application definitions for DOS\n"
      . "      --menu            Set location of PCMS130 menu application folder for generation (usually ./cd/utils/pcms130)\n"
      . "      --scan            Scan for applications (exe) and build templates\n"
      . "      --overwrite       Overwrite templates (when scanning)\n"
      . "      --help            Shows this text\n"
      . "      Use: $0 --apps <apps_folder> --menu <menu_folder> To build menu from templates\n"
      . "      OR Use: $0 --apps <apps_folder> --scan (--overwrite) To build templates (ini files) for application definitions\n"
      . "\n";
    exit;
}

if ( defined($apps) && defined($menu) ) {
    print "Menu file generation mode\n";
    udbcd::genMenu( $apps, $menu );
}
elsif ( defined($apps) && $scan eq "1" ) {
    print "Scan and build templates mode\n";
    if ( $overwrite eq "1" ) {
        print "WARNING: OVERWRITE TEMPLATES MODE IS ON\n";
        print "PRESS ANY KEY TO PROCEED OR CTRL+C TO ABORT!!!\n";
        <STDIN>;
    }

    udbcd::genTemplates($apps);
    print "Proceeding...\n";
}
else {
    usage();
}
