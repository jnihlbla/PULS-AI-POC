000100 01  MOD-W3O18501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3185              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-REC-IN      PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-SEND-IN     PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDC-REC-UT      PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-SEND-UT     PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-FAKTURA-RAD      OCCURS 15 TIMES.                             
001600*                                 ARTIKLAR I K-FAKTURA                    
001700*                                 CLEARING FLEN MAASTRICHT                
001800        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-KDCMD         PIC X.                                       
002100*                                 RAD-UPPDATERINGSKOMMANDO                
002200*                                  BLANK  = INGENTING                     
002300*                                  D , B  = DELETE                        
002400*                                  R , Ä  = REPLACE                       
002500*                                  I , N  = INSERT                        
002600        05 MOD-IDDC-REC      PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 MOD-IDDC-SEND     PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000        05 MOD-IDFAKT        PIC X(7).                                    
003100*                                 FAKTURANUMMER                           
003200        05 MOD-KDTRSTAT-ATTR PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-KDTRSTAT      PIC X.                                       
003500*                                 TRANSAKTIONSSTATUS                      
003600        05 MOD-DAANKDAG      PIC X(8).                                    
003700*                                 ANKOMSTDAG                              
003800     03 MOD-TEMFSINF         PIC X(55).                                   
003900*                                 INFORMATIONSMEDDELANDE                  
004000*** END OF VILMAII-COPY LENGTH= 482 BYTES                                 
