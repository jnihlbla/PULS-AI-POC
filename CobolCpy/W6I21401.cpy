000100 01  MID-W6I21401.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANT÷RNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANT÷RNUMMER                        
001100     03 MID-IDKVAINF-IN      PIC X(2).                                    
001200*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001300     03 MID-IDKVAINF-UT      PIC X(2).                                    
001400*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001500     03 MID-TIREGDAT-IN      PIC X(6).                                    
001600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001700     03 MID-TIREGDAT-UT      PIC X(6).                                    
001800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-TIREGDAT-9KOMPL-ENTER                                         
002400                             PIC 9(7).                                    
002500*                                 DATUMETS 9-KOMPLEMENT                   
002600     03 MID-TIKLOCK-9KOMPL-ENTER                                          
002700                             PIC 9(9).                                    
002800*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002900     03 MID-TIREGDAT-9KOMPL-NEXT                                          
003000                             PIC 9(7).                                    
003100*                                 DATUMETS 9-KOMPLEMENT                   
003200     03 MID-TIKLOCK-9KOMPL-NEXT                                           
003300                             PIC 9(9).                                    
003400*                                 TID LAGRAT SOM 9-KOMPLEMENT             
003500     03 MID-IDKVAINF-UPPD    PIC X(2).                                    
003600*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
003700     03 MID-INPUT.                                                        
003800*                                 INDATA F÷R UPPDATERING                  
003900        05 MID-TISTADAT-UT   PIC 9(6).                                    
004000*                                 GENERELLT STARTDATUM                    
004100        05 MID-TISTADAT-IN   PIC 9(6).                                    
004200*                                 GENERELLT STARTDATUM                    
004300        05 MID-TISTODAT-UT   PIC 9(6).                                    
004400*                                 GENERELLT STOPPDATUM                    
004500        05 MID-TISTODAT-IN   PIC 9(6).                                    
004600*                                 GENERELLT STOPPDATUM                    
004700        05 MID-KVANTAL-UT    PIC 9(7).                                    
004800*                                 ANTAL                                   
004900        05 MID-KVANTAL-IN    PIC 9(7).                                    
005000*                                 ANTAL                                   
005100        05 MID-KVAVV-KVAL-UT PIC 9(7).                                    
005200*                                 ANTALSAVVIKELSE KVALITET                
005300        05 MID-KVAVV-KVAL-IN PIC 9(7).                                    
005400*                                 ANTALSAVVIKELSE KVALITET                
005500        05 MID-KVART-SKROT-UT                                             
005600                             PIC 9(7).                                    
005700*                                 ANTAL SKROTADE ARTIKLAR                 
005800        05 MID-KVART-SKROT-IN                                             
005900                             PIC 9(7).                                    
006000*                                 ANTAL SKROTADE ARTIKLAR                 
006100        05 MID-KVART-RET-UT  PIC 9(7).                                    
006200*                                 ANTAL ARTIKLAR I RETUR                  
006300        05 MID-KVART-RET-IN  PIC 9(7).                                    
006400*                                 ANTAL ARTIKLAR I RETUR                  
006500        05 MID-KVART-KJUST-UT                                             
006600                             PIC 9(7).                                    
006700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
006800        05 MID-KVART-KJUST-IN                                             
006900                             PIC 9(7).                                    
007000*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
007100        05 MID-BEINIT        PIC X(3).                                    
007200*                                 INITIALER F÷R EN PERSON                 
007300        05 MID-IDNAMN        PIC X(21).                                   
007400*                                 NAMN                                    
007500        05 MID-TEKVAINF-DC-RAD1                                           
007600                             PIC X(63).                                   
007700*                                 KVALITETS INFORMATION                   
007800        05 MID-TEKVAINF-DC-RAD2                                           
007900                             PIC X(79).                                   
008000*                                 KVALITETS INFORMATION                   
008100        05 MID-TEKVAINF-DC-RAD3                                           
008200                             PIC X(79).                                   
008300*                                 KVALITETS INFORMATION                   
008400        05 MID-TEKVAINF-DC-RAD4                                           
008500                             PIC X(63).                                   
008600*                                 KVALITETS INFORMATION                   
008700        05 MID-FLTABORT      PIC X.                                       
008800*                                 BORTTAGSFLAGGA                          
008900*** END OF VILMAII-COPY LENGTH= 485 BYTES                                 
