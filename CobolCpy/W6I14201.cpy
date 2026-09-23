000100 01  MID-W6I14201.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I14201                                
000400     03 MID-IDILIST-IN       PIC X(5).                                    
000500*                                 INLÄGGNINGSLISTEIDENTITET               
000600     03 MID-IDILIST-UT       PIC X(5).                                    
000700*                                 INLÄGGNINGSLISTEIDENTITET               
000800     03 MID-IDILIRAD-IN      PIC X(5).                                    
000900*                                 INLÄGGNINGSLISTERADNUMMER               
001000     03 MID-IDILIRAD-UT      PIC X(5).                                    
001100*                                 INLÄGGNINGSLISTERADNUMMER               
001200     03 MID-IDILIRAD-ENTER   PIC 9(5).                                    
001300*                                 INLÄGGNINGSLISTERADNUMMER               
001400     03 MID-IDILIRAD-NEXT    PIC 9(5).                                    
001500*                                 INLÄGGNINGSLISTERADNUMMER               
001600     03 MID-INPUT.                                                        
001700        05 MID-FLKLAR        PIC X.                                       
001800*                                 AVSLUTNINGSMARKERING                    
001900        05 MID-IDANSTNR      PIC 9(5).                                    
002000*                                 ANSTÄLLNINGSNUMMER                      
002100        05 MID-FLKLAR-MAK    PIC X.                                       
002200*                                 AVSLUTNINGSMARKERING                    
002300        05 MID-KDCMDVAL-RAD  OCCURS 12 TIMES                              
002400                             PIC X(3).                                    
002500*                                 GENERELL KOMMANDOKOD                    
002600        05 MID-KVINLART-UPD  OCCURS 12 TIMES                              
002700                             PIC X(6).                                    
002800*                                 ANTAL I PARTIRAD                        
002900        05 MID-ADINLOMR-NXT-UPD                                           
003000                             OCCURS 12 TIMES                              
003100                             PIC X(4).                                    
003200*                                 INLEVERANSOMRÅDE NÄSTA                  
003300     03 MID-IDILIRAD-RAD     OCCURS 12 TIMES                              
003400                             PIC X(5).                                    
003500*                                 INLÄGGNINGSLISTERADNUMMER               
003600*** END COPY W6I14201    LENGTH=253                                       
