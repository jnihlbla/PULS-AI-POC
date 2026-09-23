000100 01  MID-W2I31701.                                                        
000200*                                 MID-COPYTEXT FÖR W2031700               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INPUT.                                                        
000800        05 MID-KDCMD         OCCURS 14 TIMES                              
000900                             PIC X.                                       
001000         88 MID-KDCMD-INGENTING                                           
001100                             VALUE ' '.                                   
001200         88 MID-KDCMD-DELETE VALUE 'D'                                    
001300                             'B'.                                         
001400         88 MID-KDCMD-REPLACE                                             
001500                             VALUE 'R'                                    
001600                             'Ä'.                                         
001700         88 MID-KDCMD-INSERT VALUE 'I'                                    
001800                             'N'                                          
001900                             'A'.                                         
002000         88 MID-KDCMD-SELECT VALUE 'S'                                    
002100                             'V'.                                         
002200         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002300                             'P'.                                         
002400         88 MID-KDCMD-COPY   VALUE 'C'                                    
002500                             'K'.                                         
002600*                                 RAD-UPPDATERINGSKOMMANDO                
002700*                                  BLANK  = INGENTING                     
002800*                                  D , B  = DELETE                        
002900*                                  R , Ä  = REPLACE                       
003000*                                  I,N,A  = INSERT                        
003100*                                  S , V  = SELECT                        
003200*                                  P , P  = PRINT                         
003300*                                  C , K  = COPY                          
003400*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
