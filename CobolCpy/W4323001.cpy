000100 01  W4323001-CTX.                                                        
000200*                                 URVAL FR≈N KUNDREGISTRET                
000300*                                                                         
000400     03 IDDISTR              PIC 9(5).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 TAB-1                PIC X.                                       
000700*                                 TAB-TECKEN                              
000800     03 IDKUNDNR             PIC 9(7).                                    
000900*                                 KUNDNUMMER                              
001000     03 TAB-2                PIC X.                                       
001100*                                 TAB-TECKEN                              
001200     03 BEGMT.                                                            
001300*                                 GODSMOTTAGARNAMN                        
001400        05 BEGMT-RAD1        PIC X(35).                                   
001500*                                 GODSMOTTAGARNAMN RAD 1                  
001600        05 BEGMT-RAD2        PIC X(35).                                   
001700*                                 GODSMOTTAGARNAMN RAD 2                  
001800     03 TAB-3                PIC X.                                       
001900*                                 TAB-TECKEN                              
002000     03 ADGMT-PADR           PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS POSTADRESS           
002200     03 TAB-4                PIC X.                                       
002300*                                 TAB-TECKEN                              
002400     03 ADGMT-LAND           PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS LAND                 
002600     03 TAB-5                PIC X.                                       
002700*                                 TAB-TECKEN                              
002800     03 TISTADAT             PIC 9(6).                                    
002900*                                 GENERELLT STARTDATUM                    
003000     03 TAB-6                PIC X.                                       
003100*                                 TAB-TECKEN                              
003200     03 TISTODAT             PIC 9(6).                                    
003300*                                 GENERELLT STOPPDATUM                    
003400     03 TAB-7                PIC X.                                       
003500*                                 TAB-TECKEN                              
003600     03 TIFAKT               PIC 9(6).                                    
003700*                                 FAKTURERINGSDATUM (≈≈MMDD)              
003800     03 TAB-8                PIC X.                                       
003900*                                 TAB-TECKEN                              
004000     03 COMMENT              PIC X(25).                                   
004100*** END OF VILMAII-COPY LENGTH= 203 BYTES                                 
