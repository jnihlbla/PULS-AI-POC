000100 01  0-W155L030.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W15503         
000300     03 0-KDCALL             PIC S9(3)           COMP-3                   
000400                             VALUE +0.                                    
000500*                                 ANROPSTYP                               
000600     03 0-NYUPPLAGG-AVS-ILLU PIC S9(3)           COMP-3                   
000700                             VALUE +1.                                    
000800     03 0-NYUPPLAGG-ILLU     PIC S9(3)           COMP-3                   
000900                             VALUE +2.                                    
001000     03 0-NYUPPLAGG-RAD-RUB  PIC S9(3)           COMP-3                   
001100                             VALUE +3.                                    
001200     03 0-NYUPPLAGG-RAD-TEXT-KOL                                          
001300                             PIC S9(3)           COMP-3                   
001400                             VALUE +4.                                    
001500     03 0-NYUPPLAGG-RAD-TEXT-RUB                                          
001600                             PIC S9(3)           COMP-3                   
001700                             VALUE +5.                                    
001800     03 0-NYUPPLAGG-RAD-ART  PIC S9(3)           COMP-3                   
001900                             VALUE +6.                                    
002000     03 0-NYUPPLAGG-BEN      PIC S9(3)           COMP-3                   
002100                             VALUE +7.                                    
002200     03 0-NYUPPLAGG-TEXT     PIC S9(3)           COMP-3                   
002300                             VALUE +8.                                    
002400     03 0-NYUPPLAGG-HAEN     PIC S9(3)           COMP-3                   
002500                             VALUE +9.                                    
002600     03 0-NYUPPLAGG-FOT      PIC S9(3)           COMP-3                   
002700                             VALUE +10.                                   
002800     03 0-NYUPPLAGG-NOT      PIC S9(3)           COMP-3                   
002900                             VALUE +11.                                   
003000     03 0-NYUPPLAGG-RAD      PIC S9(3)           COMP-3                   
003100                             VALUE +12.                                   
003200     03 0-NYUPPLAGG-NOLL-AVS PIC S9(3)           COMP-3                   
003300                             VALUE +13.                                   
003400     03 0-NYUPPLAGG-NOLL-RAD PIC S9(3)           COMP-3                   
003500                             VALUE +14.                                   
003600     03 0-UPPDATERING-WDN1   PIC S9(3)           COMP-3                   
003700                             VALUE +15.                                   
003800     03 0-KDSVAR             PIC X                                        
003900                             VALUE SPACE.                                 
004000*                                 SVARSKOD FRÅN SUBPROGRAM                
004100     03 0-KDSVAR-OK          PIC X                                        
004200                             VALUE ' '.                                   
004300*                                 SVARSKOD : OK                           
004400     03 0-KDSVAR-FEL         PIC X                                        
004500                             VALUE 'F'.                                   
004600*                                 SVARSKOD: FEL                           
004700*                                                                         
004800     03 0-NYCKLAR.                                                        
004900*                                 NYCKLAR                                 
005000        05 0-IDKATNR-KEY     PIC S9(5)           COMP-3                   
005100                             VALUE ZEROS.                                 
005200*                                 KATALOG-ID                              
005300        05 0-IDKATGRP-KEY    PIC S9(3)           COMP-3                   
005400                             VALUE ZEROS.                                 
005500*                                 KATALOG-GRUPP                           
005600        05 0-IDKATAVS-KEY    PIC S9(5)           COMP-3                   
005700                             VALUE ZEROS.                                 
005800*                                 KATALOG-AVSNITT                         
005900        05 0-IDKATRAD-KEY    PIC S9(5)           COMP-3                   
006000                             VALUE ZEROS.                                 
006100*                                 RADNUMMER                               
006200*** END COPY W155L030C0  LENGTH=46                                        
