000100 01  MOD-W2O11901.                                                        
000200*                                 MOD-COPYTEXT FÖR W2011900               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-START-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-START-UT PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-KDMAIL-START-IN  PIC X(4).                                    
001200*                                 TYP AV MAIL UTSKICK                     
001300     03 MOD-KDMAIL-START-UT  PIC X(4).                                    
001400*                                 TYP AV MAIL UTSKICK                     
001500     03 MOD-IDDC-KLEV-START-IN                                            
001600                             PIC X(2).                                    
001700*                                 DC FÖR KONTAKT LEVERANTÖR               
001800     03 MOD-IDDC-KLEV-START-UT                                            
001900                             PIC X(2).                                    
002000*                                 DC FÖR KONTAKT LEVERANTÖR               
002100     03 MOD-INFO-RAD         OCCURS 10 TIMES.                             
002200*                                 RADINFORMATION                          
002300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-KDCMD         PIC X.                                       
002600*                                 RAD-UPPDATERINGSKOMMANDO                
002700*                                  BLANK  = INGENTING                     
002800*                                  D , B  = DELETE                        
002900*                                  R , Ä  = REPLACE                       
003000*                                  I,N,A  = INSERT                        
003100*                                  S , V  = SELECT                        
003200*                                  P , P  = PRINT                         
003300*                                  C , K  = COPY                          
003400        05 MOD-IDLEVNR-UT    PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600        05 MOD-IDATTENT-UT   PIC Z9.                                      
003700*                                 ATTENTION NUMMER                        
003800        05 MOD-BELEV-UT      PIC X(35).                                   
003900*                                 LEVERANTÖRSNAMN                         
004000        05 MOD-KDMAIL-UT     PIC X(4).                                    
004100*                                 TYP AV MAIL UTSKICK                     
004200        05 MOD-IDDC-KLEV-UT  PIC X(2).                                    
004300*                                 DC FÖR KONTAKT LEVERANTÖR               
004400     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDLEVNR-IN       PIC X(5).                                    
004700*                                 LEVERANTÖRNUMMER                        
004800     03 MOD-IDATTENT-IN-ATTR PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDATTENT-IN      PIC Z9.                                      
005100*                                 ATTENTION NUMMER                        
005200     03 MOD-KDMAIL-IN-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDMAIL-IN        PIC X(4).                                    
005500*                                 TYP AV MAIL UTSKICK                     
005600     03 MOD-IDDC-KLEV-IN-ATTR                                             
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-IDDC-KLEV-IN     PIC X(2).                                    
006000*                                 DC FÖR KONTAKT LEVERANTÖR               
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 652 BYTES                                 
