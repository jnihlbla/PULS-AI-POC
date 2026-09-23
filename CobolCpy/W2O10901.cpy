000100 01  MOD-W2O10901.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-TIAAVV-IN        PIC X(4).                                    
001200*                                 ÅR - VECKA  (ÅÅVV)                      
001300     03 MOD-TIAAVV-UT        PIC X(4).                                    
001400*                                 ÅR - VECKA  (ÅÅVV)                      
001500     03 MOD-TID-IN           PIC X.                                       
001600*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
001700     03 MOD-TID-UT           PIC X.                                       
001800*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
001900     03 MOD-TIAAVV           PIC X(4).                                    
002000*                                 ÅR - VECKA  (ÅÅVV)                      
002100     03 MOD-TIVV             PIC X(2).                                    
002200*                                 VECKA  (VV)                             
002300     03 MOD-TID              PIC X.                                       
002400*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
002500     03 MOD-BILD.                                                         
002600        05 MOD-KVOI-PROG-VV-UT                                            
002700                             PIC -(6)9.                                   
002800*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
002900        05 MOD-KVOT-PROG-VV-UT                                            
003000                             PIC Z(6)9.                                   
003100*                                 ORDERTRÄFF, PROGNOSPÅVERKANDE           
003200        05 MOD-KVOI-PROG-DD-UT                                            
003300                             PIC -(6)9.                                   
003400*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
003500        05 MOD-KVOI-PROG-VV-IN-ATTR                                       
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-KVOI-PROG-VV-IN                                            
003900                             PIC X(7).                                    
004000*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
004100        05 MOD-KVOI-PROG-DD-IN-ATTR                                       
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KVOI-PROG-DD-IN                                            
004500                             PIC X(7).                                    
004600*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
004700        05 MOD-KVOI-DIV-VV-UT                                             
004800                             PIC -(6)9.                                   
004900*                                 ORDERINGÅNG DIVERSE OCH TPO             
005000        05 MOD-KVOT-DIV-VV-UT                                             
005100                             PIC Z(6)9.                                   
005200*                                 ORDERTRÄFF, DIVERSE                     
005300        05 MOD-KVOI-DIV-DD-UT                                             
005400                             PIC -(6)9.                                   
005500*                                 ORDERINGÅNG DIVERSE OCH TPO             
005600        05 MOD-KVOI-DIV-VV-IN-ATTR                                        
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KVOI-DIV-VV-IN                                             
006000                             PIC X(7).                                    
006100*                                 ORDERINGÅNG DIVERSE OCH TPO             
006200        05 MOD-KVOI-DIV-DD-IN-ATTR                                        
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KVOI-DIV-DD-IN                                             
006600                             PIC X(7).                                    
006700*                                 ORDERINGÅNG DIVERSE OCH TPO             
006800        05 MOD-KVOI-SATS-VV-UT                                            
006900                             PIC -(6)9.                                   
007000*                                 ORDERINGÅNG SATSFÖRBRUKNING             
007100        05 MOD-KVOT-SATS-VV-UT                                            
007200                             PIC Z(6)9.                                   
007300*                                 ORDERTRÄFF, SATS                        
007400        05 MOD-KVOI-SATS-DD-UT                                            
007500                             PIC -(6)9.                                   
007600*                                 ORDERINGÅNG SATSFÖRBRUKNING             
007700        05 MOD-KVOI-SATS-VV-IN-ATTR                                       
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-KVOI-SATS-VV-IN                                            
008100                             PIC X(7).                                    
008200*                                 ORDERINGÅNG SATSFÖRBRUKNING             
008300        05 MOD-KVOI-SATS-DD-IN-ATTR                                       
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KVOI-SATS-DD-IN                                            
008700                             PIC X(7).                                    
008800*                                 ORDERINGÅNG SATSFÖRBRUKNING             
008900        05 MOD-KVOI-SDC-VV-UT                                             
009000                             PIC -(6)9.                                   
009100*                                 ORDERINGÅNG LEV FRÅN SDC                
009200        05 MOD-KVOI-SDC-DD-UT                                             
009300                             PIC -(6)9.                                   
009400*                                 ORDERINGÅNG LEV FRÅN SDC                
009500        05 MOD-KVOI-SDC-VV-IN-ATTR                                        
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-KVOI-SDC-VV-IN                                             
009900                             PIC X(7).                                    
010000*                                 ORDERINGÅNG LEV FRÅN SDC                
010100        05 MOD-KVOI-SDC-DD-IN-ATTR                                        
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-KVOI-SDC-DD-IN                                             
010500                             PIC X(7).                                    
010600*                                 ORDERINGÅNG LEV FRÅN SDC                
010700        05 MOD-KVOI-NDC-VV-UT                                             
010800                             PIC -(6)9.                                   
010900*                                 ORDERINGÅNG LEV FRÅN NDC                
011000        05 MOD-KVOI-NDC-DD-UT                                             
011100                             PIC -(6)9.                                   
011200*                                 ORDERINGÅNG LEV FRÅN NDC                
011300        05 MOD-KVOI-NDC-VV-IN-ATTR                                        
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-KVOI-NDC-VV-IN                                             
011700                             PIC X(7).                                    
011800*                                 ORDERINGÅNG LEV FRÅN NDC                
011900        05 MOD-KVOI-NDC-DD-IN-ATTR                                        
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200        05 MOD-KVOI-NDC-DD-IN                                             
012300                             PIC X(7).                                    
012400*                                 ORDERINGÅNG LEV FRÅN NDC                
012500        05 MOD-KVOI-LED-VV-UT                                             
012600                             PIC -(6)9.                                   
012700*                                 ORDERINGÅNG FÖRSKUTEN                   
012800        05 MOD-KVOI-LED-DD-UT                                             
012900                             PIC -(6)9.                                   
013000*                                 ORDERINGÅNG FÖRSKUTEN                   
013100        05 MOD-KVOI-LED-VV-IN-ATTR                                        
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400        05 MOD-KVOI-LED-VV-IN                                             
013500                             PIC X(7).                                    
013600*                                 ORDERINGÅNG FÖRSKUTEN                   
013700        05 MOD-KVOI-LED-DD-IN-ATTR                                        
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000        05 MOD-KVOI-LED-DD-IN                                             
014100                             PIC X(7).                                    
014200*                                 ORDERINGÅNG FÖRSKUTEN                   
014300        05 MOD-KVOI-REF-VV-UT                                             
014400                             PIC -(6)9.                                   
014500*                                 ORDERINGÅNG LEV FRÅN REFILL             
014600        05 MOD-KVOT-REF-VV-UT                                             
014700                             PIC Z(6)9.                                   
014800*                                 ORDERTRÄFF, REFILL                      
014900*                                                                         
015000        05 MOD-KVOI-REF-DD-UT                                             
015100                             PIC -(6)9.                                   
015200*                                 ORDERINGÅNG LEV FRÅN REFILL             
015300        05 MOD-KVOI-REF-VV-IN-ATTR                                        
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600        05 MOD-KVOI-REF-VV-IN                                             
015700                             PIC X(7).                                    
015800*                                 ORDERINGÅNG LEV FRÅN REFILL             
015900        05 MOD-KVOI-REF-DD-IN-ATTR                                        
016000                             PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200        05 MOD-KVOI-REF-DD-IN                                             
016300                             PIC X(7).                                    
016400*                                 ORDERINGÅNG LEV FRÅN REFILL             
016500     03 MOD-TEMFSINF         PIC X(55).                                   
016600*                                 INFORMATIONSMEDDELANDE                  
016700*** END OF VILMAII-COPY LENGTH= 386 BYTES                                 
