000100 01  MOD-W2O43201.                                                        
000200*                                 COPYTEXT FÖR MOD W2O43201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR.                                                      
001000        05 MOD-IDARTNR-UT    PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MOD-DASH          PIC X.                                       
001300        05 MOD-REKSIFFR      PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-BEART-ENG        PIC X(25).                                   
002000*                                 ENGELSK ARTIKELBENÄMNING                
002100     03 MOD-KVPB-REF         PIC Z(5)9.9.                                 
002200*                                 PERIODBEHOV REFILLING                   
002300     03 MOD-KVPB-TREND-UT    PIC +(6)9.9.                                 
002400*                                 PERIODTRENDVÄRDE                        
002500     03 MOD-KDTECKEN-TREND-IN-ATTR                                        
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KDTECKEN-TREND-IN                                             
002900                             PIC X.                                       
003000*                                 PLUS ELLER MINUS (+ -)                  
003100     03 MOD-KVPB-TREND-IN-ATTR                                            
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KVPB-TREND-IN    PIC Z(5)9.9.                                 
003500*                                 PERIODTRENDVÄRDE                        
003600     03 MOD-TIREFMPB         PIC 9(6).                                    
003700*                                 DATUM MANUELL PROGNOS REFILLING         
003800     03 MOD-KVVECKOR-TREND-UT                                             
003900                             PIC Z(2).                                    
004000*                                 ANTAL VECKOR TRENDVÄRDE                 
004100     03 MOD-KVVECKOR-TREND-IN-ATTR                                        
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-KVVECKOR-TREND-IN                                             
004500                             PIC Z9.                                      
004600*                                 ANTAL VECKOR TRENDVÄRDE                 
004700     03 MOD-DAREFESC         PIC 9(6).                                    
004800*                                 DATUM ESCLÅSTPROGNOS REFILLING          
004900     03 MOD-TIDATUM-TREND    PIC 9(6).                                    
005000*                                 JUSTERAD TREND AAMMDD                   
005100     03 MOD-KVPB-JUST1-UT    PIC Z(7).Z.                                  
005200*                                 PERIODBEHOVSJUSTERING-1                 
005300     03 MOD-TIPBJUST-1-UT    PIC X(4).                                    
005400*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
005500     03 MOD-KVPB-JUST2-UT    PIC Z(7).Z.                                  
005600*                                 PERIODBEHOVSJUSTERING-2                 
005700     03 MOD-TIPBJUST-2-UT    PIC X(4).                                    
005800*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
005900     03 MOD-KVPB-JUST1-IN-ATTR                                            
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVPB-JUST1-IN    PIC Z(5)9.9.                                 
006300*                                 PERIODBEHOVSJUSTERING-1                 
006400     03 MOD-TIPBJUST-1-IN-ATTR                                            
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-TIPBJUST-1-IN    PIC 9(4).                                    
006800*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
006900     03 MOD-KVPB-JUST2-IN-ATTR                                            
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KVPB-JUST2-IN    PIC Z(5)9.9.                                 
007300*                                 PERIODBEHOVSJUSTERING-2                 
007400     03 MOD-TIPBJUST-2-IN-ATTR                                            
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-TIPBJUST-2-IN    PIC 9(4).                                    
007800*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END OF VILMAII-COPY LENGTH= 260 BYTES                                 
