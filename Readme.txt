AGD Musicizer - David Saphier 2017 v1.7 (single song)
-----------------------------------------------------
http://zxbasic.uk/files/agdmusiczerv1.rar

Steps : 

	- Save your game in AGD with no basic loader (works with the basic loader too but not as clean) as game.tap in AGDMusicizer folder
	- [optional] - add screen.scr to AGDMusicizer folder to add loading screen - delete if none is wanted
	- Drop a pt3 music file on top of the AGDMusicizer.exe
	- A new tap will be created, the name will be the same as what you saved your AGD game.
	- Done!

	Remember to turn off AY sound effects if this is for a single AY chip, leave on for turbosound...

An archive of pt3 files can be downloaded here : http://zxbasic.uk/files/pt3archive.zip
	
	or find more great music here : http://zxart.ee/eng/music/
	
CMD line Usage :

	AGDMusicizer.exe musicfile.pt3
	
You need to have a game.tap in the AGDMusicizer folder. This is your AGD game saved without a basic loader. 

You can drop pt3 files on to the AGDMusicizer.exe. Files are compressed with zx7 and stored in bank 4. I have
had a 14kb tune working without issues. It should auto detect 48k mode and skip the music stuff.

The exe does :
	- compresses the tune
	- compile the asm
	- Bakes tune name / author and filename in to code and loader 
	- creates a basic loader
	- Adds optional screen (screen.scr in same folder)
	- stitch the loader and game (dont forget your game.tap)
	
the finished tape :
	- checks for 128k mem
	- decompresses the playroutine and tune to bank 4 - @49152
	- returns
	- loads the AGD game
	- Disables AY SFX 
	- patches byte $FBFF with $6060 (Thanks George!)
	- starts the IM routine that then starts the game 
	
In your game you can call :

25502 to turn the music off
25511 to start again 

History 
-------

1.1 - Now you can drag a pt3 file from other folders, and divmmc fix. 
1.4 - Uses RETI and increases 23672 as per docs
	  Changed jump address to FBFF, this allows for 768 at the end of memory
	  as per the docs and also keeps out of the way of massive games (Thanks George!)
	  Uses process to replace building the tap adds author and track
1.5	- Now an exe which bakes in author/title/fname into binary!
	- Now Next compatabile, plays music on AY chip A (out 64433,255) and game on AY chip B (out 64433,254)!
1.6	- Fixes -	Path issues, tape header info, many more!
	  Added -	If screen.scr exists it will be added
				Names finished tape the name of game.tap 
				***removed as not realiable on differing version of AGD - Disables AY effects in normal mode 
1.7 - I had forgot to put the GOTO address in VAL so the last line of basic caused a crash if your game
      exited after you died, whoops! Thanks Mat Recardo for finding that nugget. 


Enjoy

These scripts uses the following tools : 

bin2tap Copyright (C) 2009 mike/zeroteam
zx7 by Einar Saukas (and his decompression zx7.asm)
VTII playroutine by Sergy Bulba
pasmo by Julian Albo

Source ASM is included and was cobbled together by myself using pasmo. 

AGD FB Group : https://www.facebook.com/groups/785775881484393/
Basic on ZX Group : http://www.facebook.com/groups/zxbasic
zxbasic.uk
david saphier @ gmail com 

20 / 07 / 17 2:23AM