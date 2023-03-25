#!/usr/bin/env perl
# Script for generating Grub4dos/Isolinux boot menu entries from folder with templates, see loader_templates.md for more information.
# This script also generates application templates for Ultimate DOS Boot CD see apptemplate.md
# (c) 2023 e1z0, e1z0@vintage2000.org

use warnings;
use strict;
use Getopt::Long;
use File::Basename;
use Data::Dumper;
use lib dirname(__FILE__);
use udbcd;

my $settingsfile = "settings.ini";
my $conf;
my $bootloader = "grub";
my $bloaderpath;
my $images;
my $apps;
my $grubfile     = "menu.lst";
my $isolunuxfile = "isolinux.cfg";
my $menu;

my %opt = ( "help" => 0, );

GetOptions(
    \%opt,
    'bootloader=s'  => \$bootloader,
    'bloaderpath=s' => \$bloaderpath,
    'images=s'      => \$images,
    'apps=s'        => \$apps,
    'menu=s'        => \$menu,
    'help',
);

sub usage {

    # Shown with --help option passed
    print "\n"
      . "   genmenus.pl Ultimate DOS Boot CD automatic menu generation script\n"
      . "   Created by e1z0(e1z0\@icloud.com) - Licensed under BSD License\n"
      . "\n"
      . "   Options\n"
      . "      --bootloader      Select boot loader (freedos, isolinux, grub)\n"
      . "      --bloaderpath     Selected boot loader path (null for freedos)\n"
      . "      --images          Select images (img, iso) folder\n"
      . "      --apps            Path to apps folder where are the application definitions for DOS\n"
      . "      --menu            Set location of PCMS130 menu application folder for generation\n"
      . "      --help            Shows this text\n" . "\n";
    exit;
}

# Grub config file generation happens here
sub grub {
    if ( !-e $bloaderpath . "/grub.exe" ) {
        print
"Bad boot loader path specified: $bloaderpath cannot find grub.exe in there!\n";
        exit 1;
    }
    if ( !-d $images ) {
        print "Images (img, iso) folder: $images does not exist!\n";
        exit 1;
    }
    print "Found all required data, proceeding...\n";

    if ( -e $bloaderpath . "/" . $grubfile ) {
        print "Grub config file found, removing...\n";
        unlink( $bloaderpath . "/" . $grubfile );
    }

    # check if pre header exists for grub
    if ( -e $bloaderpath . "/pregrub.cfg" ) {
        my @pre = readFile( $bloaderpath . "/pregrub.cfg" );
        foreach my $txt (@pre) {
            udbcd::writeToFile( $bloaderpath . "/" . $grubfile, $txt );
        }
    }

    # settings write
    udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
        "color = " . $conf->{"grub"}->{"color"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
        "timeout = " . $conf->{"grub"}->{"timeout"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
        "default = " . $conf->{"grub"}->{"default"} . "\n\n" );

    # populate with images boot loader menu entries
    my @imagecfgs = udbcd::parseCfgDir($images);

    #print Dumper(@imagecfgs);
    foreach my $cfg (@imagecfgs) {

        #print $cfg->{"general"}->{"type"}."\n";
        udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
            "title " . $cfg->{"general"}->{"title"} . "\n" );
        udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
            "find --set-root /udbcd\n" );
        if (   $cfg->{"general"}->{"custom"} ne ""
            && $cfg->{"general"}->{"type"} eq "custom" )
        {
            # custom bootloader entry
            udbcd::writeToFile(
                $bloaderpath . "/" . $grubfile,
                $cfg->{"general"}->{"custom"} . "\n\n"
            );
        }
        else {
            if ( $cfg->{"general"}->{"type"} eq "floppy" ) {
                if ( $cfg->{"general"}->{"method"} eq "memdisk" ) {
                    udbcd::writeToFile(
                        $bloaderpath . "/" . $grubfile,
                        "map --mem " . $cfg->{"general"}->{"file"} . " (fd0)\n"
                    );
                }
                else {
                    udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
                        "map " . $cfg->{"general"}->{"file"} . " (fd0)\n" );
                }
                udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
                    "map --hook\n" );
                udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
                    "chainloader (fd0)+1\n" );
                udbcd::writeToFile(
                    $bloaderpath . "/" . $grubfile,
                    "rootnoverify (fd0)\n\n"
                );
            }
            elsif ( $cfg->{"general"}->{"type"} eq "command" ) {
                udbcd::writeToFile(
                    $bloaderpath . "/" . $grubfile,
                    $cfg->{"general"}->{"file"} . "\n\n"
                );
            }
            elsif ( $cfg->{"general"}->{"type"} eq "linux" ) {
                udbcd::writeToFile(
                    $bloaderpath . "/" . $grubfile,
                    "kernel " . $cfg->{"general"}->{"file"}
                );
                if ( $cfg->{"general"}->{"custom"} ne "" ) {
                    udbcd::writeToFile(
                        $bloaderpath . "/" . $grubfile,
                        " " . $cfg->{"general"}->{"custom"}
                    );
                }
                udbcd::writeToFile( $bloaderpath . "/" . $grubfile, "\n" );
                if ( $cfg->{"general"}->{"method"} ne "" ) {
                    udbcd::writeToFile( $bloaderpath . "/" . $grubfile,
                        "initrd " . $cfg->{"general"}->{"method"} . "\n" );
                }
                udbcd::writeToFile( $bloaderpath . "/" . $grubfile, "\n" );
            }

            # other types can be defined here

        }
    }    # end of foreach

    # check if post header exists for grub
    if ( -e $bloaderpath . "/postgrub.cfg" ) {
        my @post = readFile( $bloaderpath . "/postgrub.cfg" );
        foreach my $txt (@post) {
            udbcd::writeToFile( $bloaderpath . "/" . $grubfile, $txt );
        }
    }
    print "Grub configuration finished!\n";
}

# Syslinux generation happens here
sub isolinux {
    if ( !-e $bloaderpath . "/isolinux.bin" ) {
        print
"Bad boot loader path specified: $bloaderpath cannot find isolinux.bin in there!\n";
        exit 1;
    }
    if ( !-d $images ) {
        print "Images (img, iso) folder: $images does not exist!\n";
        exit 1;
    }
    print "Found all required data, proceeding...\n";

    if ( -e $bloaderpath . "/" . $isolunuxfile ) {
        print "Isolinux config file found, removing...\n";
        unlink( $bloaderpath . "/" . $isolunuxfile );
    }

    # default entries
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "DEFAULT " . $conf->{"isolinux"}->{"default"} . "\n" );
    udbcd::writeToFile(
        $bloaderpath . "/" . $isolunuxfile,
        "  SAY Now booting in the Boot MENU...\n\n"
    );

    # check if pre header exists for isolinux
    if ( -e $bloaderpath . "/pre.cfg" ) {
        my @pre = readFile( $bloaderpath . "/pre.cfg" );
        foreach my $txt (@pre) {
            udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile, $txt );
        }
    }

    # populate with images boot loader menu entries
    my @imagecfgs = udbcd::parseCfgDir($images);

    #print Dumper(@imagecfgs);
    foreach my $cfg (@imagecfgs) {
        my $title = $cfg->{"general"}->{"file"};

        # if file contains spaces then use the first one word
        if ( $title =~ m/ / ) {
            ($title) = $cfg->{"general"}->{"file"} =~ /\A([^:\s]+)/;
        }

        # if there is a path to filename, extract just a filename
        $title = basename($title);
        udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
            "LABEL " . $title . "\n" );
        udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
            " MENU LABEL " . $cfg->{"general"}->{"title"} . "\n" );
        if (   $cfg->{"general"}->{"custom"} ne ""
            && $cfg->{"general"}->{"type"} eq "custom" )
        {
            # custom bootloader entry
            udbcd::writeToFile(
                $bloaderpath . "/" . $isolunuxfile,
                " " . $cfg->{"general"}->{"custom"} . "\n\n"
            );
        }
        else {
            if ( $cfg->{"general"}->{"type"} eq "floppy" ) {
                udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
                    " kernel memdisk\n" );
                udbcd::writeToFile(
                    $bloaderpath . "/" . $isolunuxfile,
                    " append initrd="
                      . $cfg->{"general"}->{"file"}
                      . " floppy\n\n"
                );
            }
            elsif ( $cfg->{"general"}->{"type"} eq "command" ) {
                udbcd::writeToFile(
                    $bloaderpath . "/" . $isolunuxfile,
                    " " . $cfg->{"general"}->{"file"} . "\n\n"
                );
            }
            elsif ( $cfg->{"general"}->{"type"} eq "linux" ) {
                udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
                    " kernel " . $cfg->{"general"}->{"file"} . "\n" );
                if ( $cfg->{"general"}->{"method"} ne "" ) {
                    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
                        " append initrd=" . $cfg->{"general"}->{"method"} );
                    if ( $cfg->{"general"}->{"custom"} ne "" ) {
                        udbcd::writeToFile(
                            $bloaderpath . "/" . $isolunuxfile,
                            " " . $cfg->{"general"}->{"custom"}
                        );
                    }

                }
                udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile, "\n" );
            }    # end linux
                 # other types can be defined here

        }
    }    # end of foreach

    # check if post header exists for grub
    if ( -e $bloaderpath . "/post.cfg" ) {
        my @post = readFile( $bloaderpath . "/post.cfg" );
        foreach my $txt (@post) {
            udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile, $txt );
        }
    }

    # enable GRUB4DOS bootstrap
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "LABEL GRUB4DOS\n" );
    udbcd::writeToFile(
        $bloaderpath . "/" . $isolunuxfile,
        " MENU LABEL Bootstrap Grub boot loader\n"
    );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        " COM32 /isolinux/chain.c32\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        " append ntldr=/grub/grldr\n\n" );

    # settings write
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU TITLE " . $conf->{"isolinux"}->{"title"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU CMDLINEROW " . $conf->{"isolinux"}->{"cmdlinerow"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR title " . $conf->{"isolinux"}->{"title_color"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR sel " . $conf->{"isolinux"}->{"sel_color"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR border " . $conf->{"isolinux"}->{"border_color"} . "\n" );
    udbcd::writeToFile(
        $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR pwdheader "
          . $conf->{"isolinux"}->{"pwdheader_color"} . "\n"
    );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR hotkey " . $conf->{"isolinux"}->{"hotkey_color"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR hotsel " . $conf->{"isolinux"}->{"hotsel_color"} . "\n" );
    udbcd::writeToFile(
        $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR timeout_msg "
          . $conf->{"isolinux"}->{"timeout_msg_color"} . "\n"
    );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU COLOR timeout " . $conf->{"isolinux"}->{"timeout_color"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU ENDROW " . $conf->{"isolinux"}->{"endrow"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU MARGIN " . $conf->{"isolinux"}->{"margin"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU PASSWORDMARGIN " . $conf->{"isolinux"}->{"passwdmargin"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU PASSWORDROW " . $conf->{"isolinux"}->{"passwdrow"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU ROWS " . $conf->{"isolinux"}->{"rows"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU TABMSGROW " . $conf->{"isolinux"}->{"tabmsgrow"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU TIMEOUTROW " . $conf->{"isolinux"}->{"timeoutrow"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "MENU WIDTH " . $conf->{"isolinux"}->{"width"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "PROMPT 1 " . $conf->{"isolinux"}->{"prompt"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "TIMEOUT " . $conf->{"isolinux"}->{"timeout"} . "\n" );
    udbcd::writeToFile( $bloaderpath . "/" . $isolunuxfile,
        "UI " . $conf->{"isolinux"}->{"ui_menu"} . "\n" );
    print "Isolinux configuration finished!\n";
}

# Init

if (   defined($bootloader)
    && defined($bloaderpath)
    && defined($images)
    && defined($apps)
    && defined($menu) )
{
    $conf = udbcd::readSettings($settingsfile);
    print "Proceeding...\n";
    if ( $bootloader eq "grub" ) {
        print "Grub boot loader selected\n";
        grub();
    }
    elsif ( $bootloader eq "isolinux" ) {
        print "Isolinux boot loader selected\n";
        isolinux();
    }
    elsif ( $bootloader eq "freedos" ) {
        print "FreeDOS selected, skipping bootloader generation...\n";
    }
    else {
        print "Error: No correct boot loader is selected, exiting...\n";
        exit 1;
    }
    udbcd::genMenu( $apps, $menu );
}
else {
    usage();
}
