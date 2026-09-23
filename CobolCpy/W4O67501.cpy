000100 01  MOD-W4O67501.                                                        
000200*                                 MODCOPYTEXT TILL W40675.                
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
002700     03 MOD-FLSKRIV-NU       PIC X.                                       
002800*                                 J/Y = SKRIV BEGÄRD LISTA                
002900     03 MOD-IDSHIPM          PIC 9(7).                                    
003000*                                 SKEPPNINGSNUMMER                        
003100     03 MOD-RADER            OCCURS 13 TIMES.                             
003200*                                                                         
003300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KDCMD         PIC X.                                       
003600*                                 RAD-UPPDATERINGSKOMMANDO                
003700*                                  BLANK  = INGENTING                     
003800*                                  D , B  = DELETE                        
003900*                                  R , Ä  = REPLACE                       
004000*                                  I , N  = INSERT                        
004100*                                  S , V  = SELECT                        
004200*                                  P , P  = PRINT                         
004300        05 MOD-IDDISTR       PIC Z(3)9.                                   
004400*                                 DISTRIKTNUMMER                          
004500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004600*                                 KUNDNUMMER                              
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 315 BYTES                                 
