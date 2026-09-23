000100 01  MOD-W2O36101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2036100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-GEMINFO.                                                      
001600        05 MOD-BEART         PIC X(21).                                   
001700        05 MOD-REPL-BY       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900        05 MOD-REPLACES      PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100        05 MOD-KVQPACK-1     PIC Z(4)9.                                   
002200*                                 ANTAL I Q1 FÖRPACKNING                  
002300        05 MOD-BLOCKCODE-NDC OCCURS 6 TIMES                               
002400                             PIC X.                                       
002500        05 MOD-FREEZECODE-NDC                                             
002600                             OCCURS 6 TIMES                               
002700                             PIC X.                                       
002800        05 MOD-KDERS         PIC Z9.                                      
002900*                                 ERSÄTTNINGSKOD                          
003000        05 MOD-KDPRODSL      PIC Z9.                                      
003100*                                 PRODUKTSLAG                             
003200        05 MOD-KDPSLLOC      PIC 9(2).                                    
003300*                                 PRODUKTSLAG LOKALT                      
003400     03 MOD-CDC-INFO.                                                     
003500        05 MOD-IDDC-CDC      PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700        05 MOD-KVLS-CDC      PIC -(6)9.                                   
003800*                                 LAGERSALDO                              
003900        05 MOD-KVDISP-CDC    PIC -(6)9.                                   
004000*                                 DISPONIBELT LAGER                       
004100        05 MOD-KVUTRS-CDC    PIC -(7)9.                                   
004200*                                 UTREDNINGSSALDO                         
004300        05 MOD-KVOKS-CDC     PIC -(6)9.                                   
004400*                                 ORDERKÖSALDO                            
004500        05 MOD-KVRESS-CDC    PIC -(6)9.                                   
004600*                                 RESERVERAT ANTAL ARTIKLAR               
004700        05 MOD-KVROS-CDC     PIC -(6)9.                                   
004800*                                 RESTORDERSALDO                          
004900        05 MOD-KVEFRS-CDC    PIC -(6)9.                                   
005000*                                 EJ FAKTURERAT ANTAL STYCK               
005100        05 MOD-KVAKS-CDC     PIC -(7)9.                                   
005200*                                 DEL AV AK SOM LIGGER I CDC              
005300        05 MOD-KVAKS-PAV-CDC PIC -(7)9.                                   
005400*                                 DEL AV AK PÅ VÄG                        
005500        05 MOD-KVBEART-CDC   PIC -(6)9.                                   
005600*                                 BESTÄLLT ANTAL STYCKEN                  
005700        05 MOD-KDLEVSP-CDC   PIC Z9.                                      
005800*                                 SPÄRRKOD LEVERANS                       
005900        05 MOD-KVSPARR-KVAL-CDC                                           
006000                             PIC Z(6)9.                                   
006100*                                 SPÄRRAT ANTAL KVALITETSFEL              
006200     03 MOD-NDC-INFO         OCCURS 6 TIMES.                              
006300        05 MOD-IDDC-NDC      PIC X(2).                                    
006400*                                 IDENTIFIERARE LAGER                     
006500        05 MOD-KVLS-NDC      PIC -(6)9.                                   
006600*                                 LAGERSALDO                              
006700        05 MOD-KVDISP-NDC    PIC -(6)9.                                   
006800*                                 DISPONIBELT LAGER                       
006900        05 MOD-KVUTRS-NDC    PIC -(7)9.                                   
007000*                                 UTREDNINGSSALDO                         
007100        05 MOD-KVOKS-NDC     PIC -(6)9.                                   
007200*                                 ORDERKÖSALDO                            
007300        05 MOD-KVRESS-NDC    PIC -(6)9.                                   
007400*                                 RESERVERAT ANTAL ARTIKLAR               
007500        05 MOD-KVROS-NDC     PIC -(6)9.                                   
007600*                                 RESTORDERSALDO                          
007700        05 MOD-KVEFRS-NDC    PIC -(6)9.                                   
007800*                                 EJ FAKTURERAT ANTAL STYCK               
007900        05 MOD-KVAKS-NDC     PIC -(7)9.                                   
008000*                                 DEL AV AK SOM LIGGER I SDC              
008100        05 MOD-KVAKS-PAV-NDC PIC -(7)9.                                   
008200*                                 DEL AV AK PÅ VÄG                        
008300        05 MOD-KVBEART       PIC -(6)9.                                   
008400*                                 BESTÄLLT ANTAL STYCKEN                  
008500        05 MOD-KDLEVSP-NDC   PIC Z9.                                      
008600*                                 SPÄRRKOD LEVERANS                       
008700        05 MOD-KVSPARR-KVAL-NDC                                           
008800                             PIC Z(6)9.                                   
008900*                                 SPÄRRAT ANTAL KVALITETSFEL              
009000     03 MOD-TEMFSINF         PIC X(55).                                   
009100*                                 INFORMATIONSMEDDELANDE                  
009200*** END OF VILMAII-COPY LENGTH= 771 BYTES                                 
