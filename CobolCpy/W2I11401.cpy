000100 01  MID-W2I11401.                                                        
000200*                                 MID-COPYTEXT FÖR W2011400               
000300     03 MID-IDLEVNR-START-IN PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-START-UT PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-INFO-IDLEVNR     OCCURS 10 TIMES                              
000800                             INDEXED MID-INFO-IND                         
000900                             PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-IDLEVNR          PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-IDLEVKND         PIC X(17).                                   
001400*                                 LEVERANTÖRENS KUNDIDENTITET             
001500     03 MID-KDEDI            PIC X.                                       
001600*                                 ÖVERFÖRINGSSTANDARD                     
001700     03 MID-FLAVIS           PIC X.                                       
001800*                                 LEVERANTÖRSAVISERING                    
001900     03 MID-FLODETTE         PIC X.                                       
002000*                                 FAKTURERING SKER VIA ODETTE             
002100     03 MID-KDCMD            PIC X.                                       
002200      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002300      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002400                             'B'.                                         
002500      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002600                             'Ä'.                                         
002700      88 MID-KDCMD-INSERT    VALUE 'I'                                    
002800                             'N'.                                         
002900      88 MID-KDCMD-SELECT    VALUE 'S'                                    
003000                             'V'.                                         
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*                                  BLANK  = INGENTING                     
003300*                                  D , B  = DELETE                        
003400*                                  R , Ä  = REPLACE                       
003500*                                  I , N  = INSERT                        
003600*                                  S , V  = SELECT                        
003700*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
