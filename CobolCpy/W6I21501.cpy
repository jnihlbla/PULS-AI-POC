000100 01  MID-W6I21501.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDKVAINF-IN      PIC X(2).                                    
000800*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000900     03 MID-IDKVAINF-UT      PIC X(2).                                    
001000*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001100     03 MID-TIREGDAT-IN      PIC X(6).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300     03 MID-TIREGDAT-UT      PIC X(6).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-SHOW-ALL-IN      PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 MID-SHOW-ALL-UT      PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
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
003500     03 MID-LEVEL2-IDDC-NEXT PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 MID-LEVEL3-IDDC-NEXT PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 MID-INPUT.                                                        
004000*                                 INDATA FÖR UPPDATERING                  
004100        05 MID-IDDC          PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300        05 MID-TISTADAT      PIC 9(6).                                    
004400*                                 GENERELLT STARTDATUM                    
004500        05 MID-TISTODAT      PIC 9(6).                                    
004600*                                 GENERELLT STOPPDATUM                    
004700        05 MID-KVANTAL       PIC 9(7).                                    
004800*                                 ANTAL                                   
004900        05 MID-KVAVV-KVAL    PIC 9(7).                                    
005000*                                 ANTALSAVVIKELSE KVALITET                
005100        05 MID-KVART-SKROT   PIC 9(7).                                    
005200*                                 ANTAL SKROTADE ARTIKLAR                 
005300        05 MID-KVART-RET     PIC 9(7).                                    
005400*                                 ANTAL ARTIKLAR I RETUR                  
005500        05 MID-KVART-KJUST   PIC 9(7).                                    
005600*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
005700        05 MID-BEINIT        PIC X(3).                                    
005800*                                 INITIALER FÖR EN PERSON                 
005900*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
