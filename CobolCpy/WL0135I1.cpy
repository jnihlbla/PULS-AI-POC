000100 01  REQU-WL0135I1.                                                       
000200*                                 REQUEST TO PGM WL0135                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 REQU-IDKVAINF-KEY    PIC 9(2).                                    
000800*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
000900     03 REQU-TIREGDAT-KEY    PIC 9(6).                                    
001000*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001100     03 REQU-IDDC2-KEY       PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 REQU-INPUT.                                                       
001400*                                 INDATA F÷R UPPDATERING                  
001500        05 REQU-IDKVAINF-UPPD                                             
001600                             PIC 9(2).                                    
001700*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001800        05 REQU-TISTADAT-IN  PIC 9(6).                                    
001900*                                 GENERELLT STARTDATUM                    
002000        05 REQU-TISTODAT-IN  PIC 9(6).                                    
002100*                                 GENERELLT STOPPDATUM                    
002200        05 REQU-KVANTAL-IN   PIC 9(6).                                    
002300*                                 ANTAL                                   
002400        05 REQU-KVAVV-KVAL-IN                                             
002500                             PIC 9(7).                                    
002600*                                 ANTALSAVVIKELSE KVALITET                
002700        05 REQU-KVART-SKROT-IN                                            
002800                             PIC 9(7).                                    
002900*                                 ANTAL SKROTADE ARTIKLAR                 
003000        05 REQU-KVART-RET-IN PIC 9(7).                                    
003100*                                 ANTAL ARTIKLAR I RETUR                  
003200        05 REQU-KVART-KJUST-IN                                            
003300                             PIC 9(7).                                    
003400*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
003500        05 REQU-BEINIT       PIC X(3).                                    
003600*                                 INITIALER F÷R EN PERSON                 
003700        05 REQU-IDNAMN-DC-QUAL                                            
003800                             PIC X(21).                                   
003900*                                 NAMN                                    
004000        05 REQU-TEKVAINF-DC-RAD1                                          
004100                             PIC X(79).                                   
004200*                                 KVALITETS INFORMATION                   
004300        05 REQU-TEKVAINF-DC-RAD2                                          
004400                             PIC X(79).                                   
004500*                                 KVALITETS INFORMATION                   
004600        05 REQU-TEKVAINF-DC-RAD3                                          
004700                             PIC X(79).                                   
004800*                                 KVALITETS INFORMATION                   
004900        05 REQU-TEKVAINF-DC-RAD4                                          
005000                             PIC X(79).                                   
005100*                                 KVALITETS INFORMATION                   
005200        05 REQU-FLTABORT     PIC X.                                       
005300*                                 BORTTAGSFLAGGA                          
005400*** END OF VILMAII-COPY LENGTH= 409 BYTES                                 
