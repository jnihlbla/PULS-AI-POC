000100 01  MID-W6I19701.                                                        
000200*                                 MIDCOPYTEXT TILL W60197.                
000300*                                                                         
000400*                                 FRÅN                                    
000500*                                 W60101, W60102, W60115,                 
000600*                                 W60132, W60133, W60136                  
000700     03 MID-IDPRTLST         PIC X(8).                                    
000800*                                 LOGISK PRINTER+LISTA IDENTITET          
000900*                                 LOGICAL PRINTER+LIST IDENTITY           
001000     03 MID-IDPGM            PIC X(8).                                    
001100*                                 PROGRAM IDENTITET                       
001200*                                 PROGRAM INTENTITY                       
001300     03 MID-KVPOST           PIC 9(7).                                    
001400*                                 RÄKNARE, ANTAL POSTER                   
001500*                                 RECORD COUNTER                          
001600     03 MID-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 MID-FLSVS            PIC X.                                       
002000*                                 ANGER GENERELLT OM NÅGOT AVSER          
002100*                                 SVS                                     
002200     03 MID-PARTI-POST       OCCURS 15 TIMES.                             
002300        05 MID-IDLEVNR       PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002600        05 MID-IDFS          PIC X(8).                                    
002700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002800*                                 ADVICE NOTE NUMBER ODETTE               
002900        05 MID-TIAVIDAT      PIC 9(6).                                    
003000*                                 AVISERINGSDATUM (YYMMDD)                
003100*                                 ADVICE NOTE DATE                        
003200        05 MID-IDRADNR-INL   PIC 9(5).                                    
003300*                                 RADNUMMER INLEVERANS                    
003400*                                 LINE NUMBER GOODS RECEIVING             
003500*** END OF VILMAII-COPY LENGTH= 386 BYTES                                 
