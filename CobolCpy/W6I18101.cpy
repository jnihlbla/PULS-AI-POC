000100 01  MID-W6I18101.                                                        
000200*                                 MID-COPYTEXT FÖR W6018100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-KEY-IN      PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-KEY-UT      PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-BEFT             PIC 9(2).                                    
001200*                                 FÖRPACKNINGSTYP                         
001300     03 MID-KDFORP.                                                       
001400*                                 FÖRPACKNINGSKOD                         
001500        05 MID-KDFORPPL      PIC 9.                                       
001600*                                 FÖRPACKNINGSPLATS                       
001700        05 MID-KDFORPGP      PIC 9(2).                                    
001800*                                 FÖRPACKNINGSGRUPP                       
001900        05 MID-KDFORPUF      PIC 9.                                       
002000*                                 UPPRÄKNINGSFAKTOR                       
002100     03 MID-TEBEFT           PIC X(40).                                   
002200*                                 TEXT FÖRPACKNINGSINSTRUKTION            
002300     03 MID-TEBEFT-79        PIC X(79).                                   
002400     03 MID-TEBEFT02-79      PIC X(79).                                   
002500*** END OF VILMAII-COPY LENGTH= 226 BYTES                                 
