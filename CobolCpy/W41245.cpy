000100 01  W41245.                                                              
000200*                                 UPPF÷LJNING ORDER CONSOLIDATION         
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC 9(7).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 KDORDKL              PIC X.                                       
001000*                                 ORDERKLASS                              
001100     03 TIAAVV               PIC 9(4).                                    
001200*                                 ≈R - VECKA  (≈≈VV)                      
001300     03 TIREGDAT             PIC 9(6).                                    
001400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001500     03 TIREGTID             PIC 9(6).                                    
001600*                                 REGISTRERINGSTID                        
001700     03 KVORDTIL-UTSKR       PIC 9(3).                                    
001800*                                 TOTALA ANTALET UTSKRIVNA ORDER          
001900     03 KVORDTIL             PIC 9(3).                                    
002000*                                 ANTAL HOPSLAGNA ORDER                   
002100     03 KDTILTYP             PIC X(3).                                    
002200*                                 TYP/ORSAK AV TILLƒGG                    
002300     03 IDORDER              PIC S9(7)           COMP-3.                  
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500     03 TIREGDAT-STO         PIC S9(7)           COMP-3.                  
002600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002700     03 TIREGTID-STO         PIC S9(7)           COMP-3.                  
002800*                                 REGISTRERINGSTID                        
002900     03 IDSYSTEM             PIC X(4).                                    
003000*                                 VOLVO VCCS SYSTEMNUMMER                 
003100     03 DAUTSKR              PIC 9(8).                                    
003200*                                 UTSKRIFTDATUM  (≈≈≈≈MMDD)               
003300     03 TIUTSTID             PIC S9(7)           COMP-3.                  
003400*                                 UTSKRIFTSTID (TTMMSS)                   
003500     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003600*                                 FRAKTSƒTT DC TILL KUND                  
003700     03 KVRADER              PIC S9(5)           COMP-3.                  
003800*                                 ANTAL RADER                             
003900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
