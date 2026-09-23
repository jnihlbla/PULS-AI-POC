000100 01  REQU-WL0153I1.                                                       
000200*                                 REQUEST TO PGM WL0153                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDILIST-KEY     PIC 9(5).                                    
000600*                                 INLÄGGNINGSLISTEIDENTITET               
000700     03 REQU-INPUT.                                                       
000800*                                 INMATNINGSFÄLT                          
000900        05 REQU-FLKLAR       PIC X.                                       
001000*                                 AVSLUTNINGSMARKERING                    
001100        05 REQU-IDANSTNR     PIC 9(5).                                    
001200*                                 ANSTÄLLNINGSNUMMER                      
001300        05 REQU-FLMAK        PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500        05 REQU-FLSKRIV      PIC X.                                       
001600*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900     03 REQU-LINE            OCCURS 500 TIMES.                            
002000*                                 NYCKELFÄLT/INMATNINGSFÄLT PÅ RA         
002100*                                 DEN                                     
002200        05 REQU-FLCMD        PIC X.                                       
002300        05 REQU-KVANTAL      PIC 9(6).                                    
002400*                                 ANTAL                                   
002500        05 REQU-IDARTNR      PIC 9(8).                                    
002600*                                 ARTIKELNUMMER                           
002700        05 REQU-IDRADNR      PIC 9(4).                                    
002800*                                 RADNUMMER                               
002900*** END OF VILMAII-COPY LENGTH= 9520 BYTES                                
