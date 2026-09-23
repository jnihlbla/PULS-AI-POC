000100 01  MID-W4I32601.                                                        
000200*                                 MIDCOPYTEXT FÖR BILD 4326               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INPUT.                                                        
000800*                                 MID-INDATA                              
000900        05 MID-KDCMD         OCCURS 39 TIMES                              
001000                             PIC X.                                       
001100*                                 RAD-UPPDATERINGSKOMMANDO                
001200*                                  BLANK  = INGENTING                     
001300*                                  D , B  = DELETE                        
001400*                                  R , Ä  = REPLACE                       
001500*                                  I , N  = INSERT                        
001600     03 MID-RAD-INFO         OCCURS 39 TIMES.                             
001700        05 MID-IDKUNDNR      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MID-IDPRT            PIC X(3).                                    
002200*                                 LOGISK PRINTERIDENTITET                 
002300*** END OF VILMAII-COPY LENGTH= 288 BYTES                                 
