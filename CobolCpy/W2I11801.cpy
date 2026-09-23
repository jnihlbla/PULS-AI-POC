000100 01  MID-W2I11801.                                                        
000200*                                 MID-COPYTEXT FÖR W2011800               
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INFO-RAD         OCCURS 10 TIMES                              
000800                             INDEXED MID-INFO-IND.                        
000900*                                 RADINFORMATION                          
001000        05 MID-INFO-IDLEVNR  PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200        05 MID-INFO-IDDC     PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MID-IDLEVNR          PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 MID-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MID-IDLEVKND         PIC X(17).                                   
001900*                                 LEVERANTÖRENS KUNDIDENTITET             
002000     03 MID-KDEDI            PIC X.                                       
002100*                                 ÖVERFÖRINGSSTANDARD                     
002200     03 MID-FLAVIS           PIC X.                                       
002300*                                 LEVERANTÖRSAVISERING                    
002400     03 MID-FLODETTE         PIC X.                                       
002500*                                 FAKTURERING SKER VIA ODETTE             
002600     03 MID-KDCMD            PIC X.                                       
002700      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002800      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002900                             'B'.                                         
003000      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
003100                             'Ä'.                                         
003200      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003300                             'N'                                          
003400                             'A'.                                         
003500      88 MID-KDCMD-SELECT    VALUE 'S'                                    
003600                             'V'.                                         
003700      88 MID-KDCMD-PRINT     VALUE 'P'                                    
003800                             'P'.                                         
003900      88 MID-KDCMD-COPY      VALUE 'C'                                    
004000                             'K'.                                         
004100*                                 RAD-UPPDATERINGSKOMMANDO                
004200*                                  BLANK  = INGENTING                     
004300*                                  D , B  = DELETE                        
004400*                                  R , Ä  = REPLACE                       
004500*                                  I,N,A  = INSERT                        
004600*                                  S , V  = SELECT                        
004700*                                  P , P  = PRINT                         
004800*                                  C , K  = COPY                          
004900*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
