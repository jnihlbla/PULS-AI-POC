000100 01  MID-W4I74801.                                                        
000200*                                 MID-COPYTEXT FÖR W40748                 
000300     03 MID-KDBEHX-IN        PIC X.                                       
000400*                                 BEHANDLINGSKOD-X                        
000500     03 MID-IDILIST-IN       PIC X(5).                                    
000600*                                 INLÄGGNINGSLISTEIDENTITET               
000700     03 MID-INPUT.                                                        
000800*                                 INMATNINGSFÄLT                          
000900        05 MID-IDPRT         PIC X(3).                                    
001000*                                 LOGISK PRINTERIDENTITET                 
001100        05 MID-IDANSTNR      PIC X(5).                                    
001200*                                 ANSTÄLLNINGSNUMMER                      
001300        05 MID-KDCMD         OCCURS 14 TIMES                              
001400                             PIC X.                                       
001500         88 MID-KDCMD-INGENTING                                           
001600                             VALUE ' '.                                   
001700         88 MID-KDCMD-DELETE VALUE 'D'                                    
001800                             'B'.                                         
001900         88 MID-KDCMD-REPLACE                                             
002000                             VALUE 'R'                                    
002100                             'Ä'.                                         
002200         88 MID-KDCMD-INSERT VALUE 'I'                                    
002300                             'N'.                                         
002400         88 MID-KDCMD-SELECT VALUE 'S'                                    
002500                             'V'.                                         
002600         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002700                             'P'.                                         
002800         88 MID-KDCMD-COPY   VALUE 'C'                                    
002900                             'K'.                                         
003000*                                 RAD-UPPDATERINGSKOMMANDO                
003100*                                  BLANK  = INGENTING                     
003200*                                  D , B  = DELETE                        
003300*                                  R , Ä  = REPLACE                       
003400*                                  I , N  = INSERT                        
003500*                                  S , V  = SELECT                        
003600*                                  P , P  = PRINT                         
003700*                                  C , K  = COPY                          
003800     03 MID-KEYFIELD         OCCURS 14 TIMES.                             
003900*                                 NYCKELFÄLT PÅ RADEN                     
004000        05 MID-IDILIST       PIC X(5).                                    
004100*                                 INLÄGGNINGSLISTEIDENTITET               
004200*** END OF VILMAII-COPY LENGTH= 98 BYTES                                  
