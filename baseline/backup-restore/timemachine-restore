#!/bin/bash
#set -e
#
#---------------------------------------------------------
# Simple script to get files in an Apple Time Machine (TM) archive. Inspired by http://hints.macworld.com/article.php?story=20080623213342356
#
# Usage: /path/to/tmrestore <Path to .HFS hidden directory> <Path where to restore> <Path to directory to restore> <Name of the directory to restore>
#
# You need to have rsync installed : sudo aptitude install rsync
#
# You need to run this script with the full path, ie. /usr/local/bin/tmrestore {arguments...} not ./tmrestore
#
# You need to run this script as root, running it in a real shell (not sudo script). Try "sudo su -" then invoke the script.
#
# Use tab completion to fill the arguments ie. /media/Time\ Machine\ Mount\ Point and not "/media/Time Machine Mount Point" (with double quotes). This must be also for the last argument if spaces are present.
#
# Exemple invocation :
#
# /usr/local/bin/tmrestore /media/Time\ Machine\ Mount\ Point/.HFS+\ Private\ Directory\ Data^M/ /TM\ Restore/My\ Files /media/Time\ Machine\ Mount\ Point/Backups.backupdb/iMac/Latest/Macintosh\ HD/Users/john/Desktop Desktop
#
# where :
# - /media/Time\ Machine\ Mount\ Point/.HFS+\ Private\ Directory\ Data^M/ is the path to the Time Machine hidden directory
# - /TM\ Restore/My\ Files is the path to where you want to restore your files/dirs
# - /media/Time\ Machine\ Mount\ Point/Backups.backupdb/iMac/Latest/Macintosh\ HD/Users/john/Desktop is the path in the Time Machine archive of the file/directory you want to restore
# - Desktop is the name of the directory that will be created in the path to where you want to restore your files/dirs
#
# THIS SCRIPT COMES WITH NO WARRANTY! (and is GPL)
#
# Feel free to improve it (adding comments, for exemple ;)
#---------------------------------------------------------

DEBUG=1

if [ $DEBUG -eq 1 ]
then
  echo ""
  echo "invoking $0 with arguments :"
  # note : if you copy/paste this code, you must replace ^M with the carriage return caracter. This is done, in vim, by, in edition mode (i), typing CtrlV, then the Enter key :
  HFS=`echo $1 | sed s/"^M"//g`
  DIR=`echo $3 | sed s/"^M"//g`
  echo "\$HFS : $HFS"
  echo "\$REST : $2"
  echo "\$DIR : $DIR"
  echo "\$DIRNAME : $4"
fi

IFS="
"
usage()
{
  echo "Usage: `basename $0` <Path to .HFS hidden directory> <Path where to restore> <Path to directory to restore> <Name of the directory to restore>"
}

HFS="$1"
REST="$2"
DIR="$3"
DIRNAME="$4"
[ $# -ne 4 ] &! [ -d "$HFS" ] &[ "$REST" == "/" ] &[ "$REST" == "$HOME" ] &! [ -d "$DIR" ] && ! [ -f "$DIR" ] && exit 0

[ $DEBUG -eq 1 ] && echo ""
[ $DEBUG -eq 1 ] && echo "mkdir -p "$REST""
mkdir -p "$REST"

if [ -f "$DIR" ]
then
  INODE=`ls -l "$DIR" | cut -d " " -f2`
  SIZE=`ls -s "$DIR" | cut -d " " -f 1`
  if [ $INODE -gt 100 ] && [ $SIZE -eq 0 ]
   then
     [ $DEBUG -eq 1 ] && echo "$DIR is a special TM dir, self invoking (in 1):"
     $0 "$HFS" "$REST/$DIRNAME" "$HFS/dir_$INODE" "`basename -- $DIR`"
     [ $? -ne 0 ] && exit 1
   else
     [ $DEBUG -eq 1 ] && echo rsync -avP -- "$DIR" "$REST"
     rsync -avP -- "$DIR" "$REST"
   fi
else
  if [ -d "$DIR" ]
  then
    cd "$DIR"
    for i in `ls -a .`
    do
      FILE="`basename -- "$i"`"
      [ "$FILE" == "." ] && continue
      [ "$FILE" == ".." ] && continue
      [ "$FILE" == ".DS_Store" ] && continue
      # please add file or dir you don't want to restore :
      [ "$FILE" == "Flash Player" ] && continue
      [ "$FILE" == "iPhoto Library" ] && continue
      if [ -f "$FILE" ]
      then
        INODE=`ls -l "$FILE" | cut -d " " -f2`
        SIZE=`ls -s "$FILE" | cut -d " " -f 1`
        if [ $INODE -gt 100 ] && [ $SIZE -eq 0 ]
        then
          [ $DEBUG -eq 1 ] && echo "$FILE is a special TM dir, self invoking (in 2) :"
          $0 "$HFS" "$REST/$FILE" "$HFS/dir_$INODE" "$FILE"
          [ $? -ne 0 ] && exit 1
        else
          [ $DEBUG -eq 1 ] && echo rsync -avP -- "$FILE" "$REST"
          rsync -avP -- "$FILE" "$REST"
        fi
      else
        if [ -d "$FILE" ]
        then
        [ $DEBUG -eq 1 ] && echo "$FILE is a dir, self invoking (in 3):"
        $0 "$HFS" "$REST/$FILE" "$DIR/$FILE" "$FILE"
        [ $? -ne 0 ] && exit 1
      else
        echo "$FILE : Not a file, not a dir, maybe a link. Not treated (in 3)."
      fi
      fi
    done
  else
    echo "$DIR : Not a file, not a dir, maybe a link. Not treated (in 4)."
  fi
fi

exit 0
