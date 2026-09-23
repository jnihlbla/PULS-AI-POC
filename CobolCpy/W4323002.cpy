000100 01  W4323002-CTX.                                                        
000200*                                 URVAL FR≈N KUNDREGISTRET                
000300*                                                                         
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 BEBET.                                                            
000900*                                 BETALNINGSANSVARIG NAMN                 
001000        05 BEBETRAD-1        PIC X(35).                                   
001100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001200        05 BEBETRAD-2        PIC X(35).                                   
001300*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001400     03 ADBET.                                                            
001500*                                 BETALNINGSANSVARIG ADRESS               
001600        05 ADBETRAD-1        PIC X(35).                                   
001700*                                 ADRESSRAD BETALNINGSANSVARIG            
001800        05 ADBETRAD-2        PIC X(35).                                   
001900*                                 ADRESSRAD BETALNINGSANSVARIG            
002000     03 BEGMT.                                                            
002100*                                 GODSMOTTAGARNAMN                        
002200        05 BEGMT-RAD1        PIC X(35).                                   
002300*                                 GODSMOTTAGARNAMN RAD 1                  
002400        05 BEGMT-RAD2        PIC X(35).                                   
002500*                                 GODSMOTTAGARNAMN RAD 2                  
002600     03 ADGMT.                                                            
002700*                                 GODSMOTTAGARADRESS                      
002800        05 ADGMT-GATA        PIC X(35).                                   
002900*                                 GODSMOTTAGARADRESS GATA                 
003000        05 ADGMT-PADR        PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS POSTADRESS           
003200        05 ADGMT-LAND        PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS LAND                 
003400     03 KDSPRAK              PIC S9              COMP-3.                  
003500*                                 SPR≈KKOD                                
003600     03 TIFAKT               PIC S9(7)           COMP-3.                  
003700*                                 FAKTURERINGSDATUM (≈≈MMDD)              
003800     03 TISTADAT             PIC S9(7)           COMP-3.                  
003900*                                 GENERELLT STARTDATUM                    
004000     03 TISTODAT             PIC S9(7)           COMP-3.                  
004100*                                 GENERELLT STOPPDATUM                    
004200*** END OF VILMAII-COPY LENGTH= 335 BYTES                                 
