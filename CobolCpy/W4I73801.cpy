000100 01  MID-W4I73801.                                                        
000200*                                 MID-COPYTEXT FÖR W40738                 
000300     03 MID-IDILIST-IN       PIC X(5).                                    
000400*                                 INLÄGGNINGSLISTEIDENTITET               
000500     03 MID-IDILIST-UT       PIC X(5).                                    
000600*                                 INLÄGGNINGSLISTEIDENTITET               
000700     03 MID-INPUT.                                                        
000800*                                 INMATNINGSFÄLT                          
000900        05 MID-FLKLAR        PIC X.                                       
001000*                                 AVSLUTNINGSMARKERING                    
001100        05 MID-IDANSTNR      PIC X(5).                                    
001200*                                 ANSTÄLLNINGSNUMMER                      
001300        05 MID-FLMAK         PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500        05 MID-IDPRT         PIC X(3).                                    
001600*                                 LOGISK PRINTERIDENTITET                 
001700        05 MID-INPUTLINE     OCCURS 13 TIMES.                             
001800*                                 INMATNINGSFÄLT PÅ RADEN                 
001900           07 MID-KDCMDVAL   PIC X(3).                                    
002000*                                 GENERELL KOMMANDOKOD                    
002100           07 MID-KVANTAL    PIC X(6).                                    
002200*                                 ANTAL ALLMÄNT                           
002300     03 MID-KEYFIELD         OCCURS 13 TIMES.                             
002400*                                 NYCKELFÄLT PÅ RADEN                     
002500        05 MID-IDARTNR       PIC X(8).                                    
002600*                                 ARTIKELNUMMER                           
002700        05 MID-IDRADNR       PIC X(4).                                    
002800*                                 RADNUMMER                               
002900     03 MID-MODFAELT-IN      PIC X(200).                                  
003000*** END OF VILMAII-COPY LENGTH= 493 BYTES                                 
