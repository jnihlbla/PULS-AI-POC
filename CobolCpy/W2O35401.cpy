000100 01  MOD-W2O35401.                                                        
000200*                                 MOD-COPYTEXT FÖR REFILLBILD             
000300*                                 ETA INQUIRY                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-BEART            PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800     03 MOD-GRP-RULL         OCCURS 14 TIMES.                             
001900        05 MOD-TEXT-DC       PIC X(6).                                    
002000        05 MOD-KVLS-DC       PIC -(7)9.                                   
002100*                                 LAGERSALDO                              
002200        05 MOD-KVAK-DC       PIC Z(6)9.                                   
002300*                                 DEL AV AK SOM LIGGER I SDC              
002400        05 MOD-KVAVIS-DC-WC  PIC Z(6)9.                                   
002500*                                 AVISERAT ANTAL                          
002600        05 MOD-KVAVIS-DC-W1  PIC Z(6)9.                                   
002700*                                 AVISERAT ANTAL                          
002800        05 MOD-KVAVIS-DC-W2  PIC Z(6)9.                                   
002900*                                 AVISERAT ANTAL                          
003000        05 MOD-KVAVIS-DC-W3  PIC Z(6)9.                                   
003100*                                 AVISERAT ANTAL                          
003200        05 MOD-KVAVIS-DC-W4  PIC Z(6)9.                                   
003300*                                 AVISERAT ANTAL                          
003400        05 MOD-KVAVIS-DC-W5  PIC Z(6)9.                                   
003500*                                 AVISERAT ANTAL                          
003600        05 MOD-KVAVIS-DC-W6  PIC Z(6)9.                                   
003700*                                 AVISERAT ANTAL                          
003800     03 MOD-TEMFSINF         PIC X(55).                                   
003900*                                 INFORMATIONSMEDDELANDE                  
004000*** END OF VILMAII-COPY LENGTH= 1126 BYTES                                
