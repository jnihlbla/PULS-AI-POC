000100 01  MOD-W4O75401.                                                        
000200*                                 MOD-COPYTEXT FÖR W40754                 
000300*                                 DISCREPANCY CODES REFERRAL MATR         
000400*                                 IX                                      
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
001000*                                 GRUPP MED TABELL RADER                  
001100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300        05 MOD-KDCMD         PIC X.                                       
001400*                                 RAD-UPPDATERINGSKOMMANDO                
001500*                                  BLANK  = INGENTING                     
001600*                                  D , B  = DELETE                        
001700*                                  R , Ä  = REPLACE                       
001800*                                  I,N,A  = INSERT                        
001900*                                  S , V  = SELECT                        
002000*                                  P , P  = PRINT                         
002100*                                  C , K  = COPY                          
002200        05 MOD-IDDC          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 MOD-KDANMORS-GRP  OCCURS 19 TIMES.                             
002500*                                 GRUPP MED KDANMORS                      
002600           07 MOD-KDANMORS   PIC X(2).                                    
002700*                                 ORSAK TILL LEVERANSANMÄRKNING           
002800        05 MOD-IDUSER        PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000        05 MOD-TIUPPDAT      PIC 9(6).                                    
003100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003200     03 MOD-INMATNINGSRAD.                                                
003300*                                 INMATNINGSRAD                           
003400        05 MOD-KDCMD-E-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KDCMD-E       PIC X.                                       
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I,N,A  = INSERT                        
004200*                                  S , V  = SELECT                        
004300*                                  P , P  = PRINT                         
004400*                                  C , K  = COPY                          
004500        05 MOD-IDDC-E-ATTR   PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDDC-E        PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900        05 MOD-KDANMORS-GRP-E                                             
005000                             OCCURS 19 TIMES.                             
005100*                                 GRUPP MED KDANMORS                      
005200           07 MOD-KDANMORS-E-ATTR                                         
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500           07 MOD-KDANMORS-E PIC X(2).                                    
005600*                                 ORSAK TILL LEVERANSANMÄRKNING           
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 866 BYTES                                 
