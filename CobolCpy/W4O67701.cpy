000100 01  MOD-W4O67701.                                                        
000200*                                 MODCOPYTEXT TILL W40677.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRPTNR-IN      PIC X(3).                                    
000800*                                 TRANSPORTIDENTITET                      
000900     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 MOD-IDLBBET-IN       PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 MOD-IDLBBET-UT       PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 MOD-FLFARLIG-IN      PIC X.                                       
001600*                                 FARLIGT GODS-FLAGGA                     
001700     03 MOD-FLFARLIG-UT      PIC X.                                       
001800*                                 FARLIGT GODS-FLAGGA                     
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-FLAVSLUTA-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-FLAVSLUTA        PIC X.                                       
002600*                                 ALLMÄN FLAGGA                           
002700     03 MOD-IDSHIPM          PIC 9(7).                                    
002800*                                 SKEPPNINGSNUMMER                        
002900     03 MOD-RADER            OCCURS 13 TIMES.                             
003000*                                                                         
003100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KDCMD         PIC X.                                       
003400*                                 RAD-UPPDATERINGSKOMMANDO                
003500*                                  BLANK  = INGENTING                     
003600*                                  D , B  = DELETE                        
003700*                                  R , Ä  = REPLACE                       
003800*                                  I , N  = INSERT                        
003900*                                  S , V  = SELECT                        
004000*                                  P , P  = PRINT                         
004100        05 MOD-IDDISTR       PIC Z(3)9.                                   
004200*                                 DISTRIKTNUMMER                          
004300        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004400*                                 KUNDNUMMER                              
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 314 BYTES                                 
