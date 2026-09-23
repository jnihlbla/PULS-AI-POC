000100 01  MOD-W2O123001.                                                       
000200*                                 COPYTEXT FÖR MOD W2O12301               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-AREA.                                                         
001200        05 MOD-STRECK-1      PIC X.                                       
001300        05 MOD-REKSIFFR      PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500        05 MOD-BEART-SVE     PIC X(25).                                   
001600*                                 SVENSK ARTIKELBENÄMNING                 
001700        05 MOD-MOD-DATA.                                                  
001800*                                                                         
001900           07 MOD-KVPB-SEP   OCCURS 2 TIMES                               
002000                             PIC Z(7).Z.                                  
002100*                                 SEPARAT PERIODBEHOV                     
002200           07 MOD-KVPB-VESL  OCCURS 2 TIMES                               
002300                             PIC Z(7).Z.                                  
002400*                                 GÄLLANDE PB VID VECKOSLUT               
002500           07 MOD-TIPBDAT    OCCURS 2 TIMES                               
002600                             PIC 9(5).                                    
002700*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
002800           07 MOD-KVPB-SATS  OCCURS 2 TIMES                               
002900                             PIC Z(7).Z.                                  
003000*                                 SATS-PERIODBEHOV                        
003100           07 MOD-FLMPB      OCCURS 2 TIMES                               
003200                             PIC X.                                       
003300*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
003400           07 MOD-KVMAD-SEP  OCCURS 2 TIMES                               
003500                             PIC Z(7).Z.                                  
003600*                                 SEPARAT PROGNOSFEL                      
003700           07 MOD-KVMAD-TOT  PIC Z(5)9.9.                                 
003800*                                 TOTALT PROGNOSFEL                       
003900           07 MOD-KVMAD-TOT-IN-ATTR                                       
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200           07 MOD-KVMAD-TOT-IN                                            
004300                             PIC Z(5)9.9.                                 
004400*                                 TOTALT PROGNOSFEL                       
004500           07 MOD-KVUTJFEL   OCCURS 2 TIMES                               
004600                             PIC -(7).-.                                  
004700*                                 UTJÄMNAT FEL                            
004800           07 MOD-RVPROURS   OCCURS 2 TIMES                               
004900                             PIC Z(3).                                    
005000*                                 ANTAL PROGNOSFEL I FÖLJD                
005100           07 MOD-RVPROFEL   OCCURS 2 TIMES                               
005200                             PIC Z(3).                                    
005300*                                 ANTAL STORA PROGNOSFEL                  
005400           07 MOD-KVPB-HIST  PIC Z(7).Z.                                  
005500*                                 PB (PROGNOS) HISTORISKT CDC             
005600           07 MOD-PB-JUST1   OCCURS 2 TIMES.                              
005700*                                                                         
005800              09 MOD-KVPB-JUST-1                                          
005900                             PIC Z(7).Z.                                  
006000*                                 PERIODBEHOVSJUSTERING                   
006100              09 MOD-TIPBJUST-1                                           
006200                             PIC 9(4).                                    
006300*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
006400              09 MOD-IN-KVPB-JUST-1-ATTR                                  
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700              09 MOD-IN-KVPB-JUST-1                                       
006800                             PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000              09 MOD-IN-TIPBJUST-1-ATTR                                   
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300              09 MOD-IN-TIPBJUST-1                                        
007400                             PIC X(2).                                    
007500*                                 MFS BEHANDLING AV INPUTFÄLT             
007600           07 MOD-PB-JUST2   OCCURS 2 TIMES.                              
007700*                                                                         
007800              09 MOD-KVPB-JUST-2                                          
007900                             PIC Z(7).Z.                                  
008000*                                 PERIODBEHOVSJUSTERING                   
008100              09 MOD-TIPBJUST-2                                           
008200                             PIC 9(4).                                    
008300*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
008400              09 MOD-IN-KVPB-JUST-2-ATTR                                  
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700              09 MOD-IN-KVPB-JUST-2                                       
008800                             PIC X(2).                                    
008900*                                 MFS BEHANDLING AV INPUTFÄLT             
009000              09 MOD-IN-TIPBJUST-2-ATTR                                   
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300              09 MOD-IN-TIPBJUST-2                                        
009400                             PIC X(2).                                    
009500*                                 MFS BEHANDLING AV INPUTFÄLT             
009600     03 MOD-KVPB-TREND-UT    PIC +(6)9.9.                                 
009700*                                 PERIODTRENDVÄRDE                        
009800     03 MOD-KVPB-TREND-CH REDEFINES MOD-KVPB-TREND-UT                     
009900                             PIC X(9).                                    
010000     03 MOD-KDTECKEN-TREND-IN-ATTR                                        
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-KDTECKEN-TREND-IN                                             
010400                             PIC X.                                       
010500*                                 PLUS ELLER MINUS (+ -)                  
010600     03 MOD-KVPB-TREND-IN-ATTR                                            
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-KVPB-TREND-IN    PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100     03 MOD-KVVECKOR-TREND-UT                                             
011200                             PIC Z(2).                                    
011300*                                 ANTAL VECKOR TRENDVÄRDE                 
011400     03 MOD-KVVECKOR-TREND-IN-ATTR                                        
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-KVVECKOR-TREND-IN                                             
011800                             PIC X(2).                                    
011900*                                 ANTAL VECKOR TRENDVÄRDE                 
012000     03 MOD-TIDATUM-TREND    PIC X(6).                                    
012100*                                 JUSTERAD TREND AAMMDD                   
012200     03 MOD-TEMFSINF         PIC X(55).                                   
012300*                                 INFORMATIONSMEDDELANDE                  
012400*** END OF VILMAII-COPY LENGTH= 397 BYTES                                 
