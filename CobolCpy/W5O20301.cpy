000100 01  MOD-W5O20301.                                                        
000200*                                 MOD-COPYTEXT FÖR W5020300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-BEART            PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
001600*                                 FUNKTIONSGRUPP                          
001700     03 MOD-KDPSLLOC         PIC X(2).                                    
001800*                                 PRODUKTSLAG LOKALT                      
001900     03 MOD-KVAKS-PAV-41     PIC -(7)9.                                   
002000*                                 DEL AV AK PÅ VÄG                        
002100     03 MOD-KVAKS-PAV-42     PIC -(7)9.                                   
002200*                                 DEL AV AK PÅ VÄG                        
002300     03 MOD-KVAKS-PAV-43     PIC -(7)9.                                   
002400*                                 DEL AV AK PÅ VÄG                        
002500     03 MOD-KVAKS-PAV-51     PIC -(7)9.                                   
002600*                                 DEL AV AK PÅ VÄG                        
002700     03 MOD-KVAKS-SDC-41     PIC -(7)9.                                   
002800*                                 DEL AV AK SOM LIGGER I SDC              
002900     03 MOD-KVAKS-SDC-42     PIC -(7)9.                                   
003000*                                 DEL AV AK SOM LIGGER I SDC              
003100     03 MOD-KVAKS-SDC-43     PIC -(7)9.                                   
003200*                                 DEL AV AK SOM LIGGER I SDC              
003300     03 MOD-KVAKS-SDC-51     PIC -(7)9.                                   
003400*                                 DEL AV AK SOM LIGGER I SDC              
003500     03 MOD-KVLS-41          PIC -(7)9.                                   
003600*                                 LAGERSALDO                              
003700     03 MOD-KVLS-42          PIC -(7)9.                                   
003800*                                 LAGERSALDO                              
003900     03 MOD-KVLS-43          PIC -(7)9.                                   
004000*                                 LAGERSALDO                              
004100     03 MOD-KVLS-51          PIC -(7)9.                                   
004200*                                 LAGERSALDO                              
004300     03 MOD-KVEFRS-41        PIC -(7)9.                                   
004400*                                 EJ FAKTURERAT ANTAL STYCK               
004500     03 MOD-KVEFRS-42        PIC -(7)9.                                   
004600*                                 EJ FAKTURERAT ANTAL STYCK               
004700     03 MOD-KVEFRS-43        PIC -(7)9.                                   
004800*                                 EJ FAKTURERAT ANTAL STYCK               
004900     03 MOD-KVEFRS-51        PIC -(7)9.                                   
005000*                                 EJ FAKTURERAT ANTAL STYCK               
005100     03 MOD-KVUTRS-41        PIC -(7)9.                                   
005200*                                 UTREDNINGSSALDO                         
005300     03 MOD-KVUTRS-42        PIC -(7)9.                                   
005400*                                 UTREDNINGSSALDO                         
005500     03 MOD-KVUTRS-43        PIC -(7)9.                                   
005600*                                 UTREDNINGSSALDO                         
005700     03 MOD-KVUTRS-51        PIC -(7)9.                                   
005800*                                 UTREDNINGSSALDO                         
005900     03 MOD-PRAVCOST-41      PIC Z(6)9.9(2).                              
006000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006100     03 MOD-PRAVCOST-42      PIC Z(6)9.9(2).                              
006200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006300     03 MOD-PRAVCOST-43      PIC Z(6)9.9(2).                              
006400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006500     03 MOD-PRAVCOST-51      PIC Z(6)9.9(2).                              
006600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006700     03 MOD-SUAVCOST-41      PIC -(8)9.9(2).                              
006800*                                 SUMMA MEDELVÄRDESKOSTNAD I              
006900*                                 UTL.VALUTA                              
007000     03 MOD-SUAVCOST-42      PIC -(8)9.9(2).                              
007100*                                 SUMMA MEDELVÄRDESKOSTNAD I              
007200*                                 UTL.VALUTA                              
007300     03 MOD-SUAVCOST-43      PIC -(8)9.9(2).                              
007400*                                 SUMMA MEDELVÄRDESKOSTNAD I              
007500*                                 UTL.VALUTA                              
007600     03 MOD-SUAVCOST-51      PIC -(8)9.9(2).                              
007700*                                 SUMMA MEDELVÄRDESKOSTNAD I              
007800*                                 UTL.VALUTA                              
007900     03 MOD-IDLEVNR-41       PIC X(5).                                    
008000*                                 LEVERANTÖRNUMMER                        
008100     03 MOD-IDLEVNR-42       PIC X(5).                                    
008200*                                 LEVERANTÖRNUMMER                        
008300     03 MOD-IDLEVNR-43       PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500     03 MOD-IDLEVNR-51       PIC X(5).                                    
008600*                                 LEVERANTÖRNUMMER                        
008700     03 MOD-PRARTBEL-41      PIC Z(7)9.9(5).                              
008800*                                 BESTPRIS LEVERANTÖRENS VALUTA           
008900     03 MOD-PRARTBEL-42      PIC Z(7)9.9(5).                              
009000*                                 BESTPRIS LEVERANTÖRENS VALUTA           
009100     03 MOD-PRARTBEL-43      PIC Z(7)9.9(5).                              
009200*                                 BESTPRIS LEVERANTÖRENS VALUTA           
009300     03 MOD-PRARTBEL-51      PIC Z(7)9.9(5).                              
009400*                                 BESTPRIS LEVERANTÖRENS VALUTA           
009500     03 MOD-PRKURS-41        PIC Z(5)9.9(5).                              
009600*                                 VALUTAKURS                              
009700     03 MOD-PRKURS-42        PIC Z(5)9.9(5).                              
009800*                                 VALUTAKURS                              
009900     03 MOD-PRKURS-43        PIC Z(5)9.9(5).                              
010000*                                 VALUTAKURS                              
010100     03 MOD-PRKURS-51        PIC Z(5)9.9(5).                              
010200*                                 VALUTAKURS                              
010300     03 MOD-PRARTBEU-41      PIC Z(4)9.9(2).                              
010400*                                 BESTPRIS UTLÄNDSK VALUTA                
010500     03 MOD-PRARTBEU-42      PIC Z(4)9.9(2).                              
010600*                                 BESTPRIS UTLÄNDSK VALUTA                
010700     03 MOD-PRARTBEU-43      PIC Z(4)9.9(2).                              
010800*                                 BESTPRIS UTLÄNDSK VALUTA                
010900     03 MOD-PRARTBEU-51      PIC Z(4)9.9(2).                              
011000*                                 BESTPRIS UTLÄNDSK VALUTA                
011100     03 MOD-TEMFSINF         PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 554 BYTES                                 
