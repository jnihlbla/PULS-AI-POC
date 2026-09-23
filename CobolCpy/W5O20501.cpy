000100 01  MOD-W5O20501.                                                        
000200*                                 MOD-COPYTEXT FÖR W5020500               
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
001300     03 MOD-IDDC-ATTR        PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDFTG-UT         PIC 9(2).                                    
002000*                                 FÖRETAGSID EKONOM REDOVISNING           
002100     03 MOD-BEART            PIC X(25).                                   
002200*                                 ARTIKELBENÄMNING                        
002300     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
002400*                                 FUNKTIONSGRUPP                          
002500     03 MOD-KDPRODSL         PIC X(2).                                    
002600*                                 PRODUKTSLAG                             
002700     03 MOD-KDPSLLOC         PIC X(2).                                    
002800*                                 PRODUKTSLAG LOKALT                      
002900     03 MOD-IDDC-01          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-PRMATRL-01       PIC Z(6)9.9(2).                              
003200*                                 FAST PRIS UNDER LÖPANDE ÅR              
003300     03 MOD-KVAKS-PAV-01     PIC -(7)9.                                   
003400*                                 DEL AV AK PÅ VÄG                        
003500     03 MOD-KVAKS-SDC-01     PIC -(7)9.                                   
003600*                                 DEL AV AK SOM LIGGER I SDC              
003700     03 MOD-KVLS-01          PIC -(7)9.                                   
003800*                                 LAGERSALDO                              
003900     03 MOD-KVEFRS-01        PIC -(7)9.                                   
004000*                                 EJ FAKTURERAT ANTAL STYCK               
004100     03 MOD-KVUTRS-01        PIC -(7)9.                                   
004200*                                 UTREDNINGSSALDO                         
004300     03 MOD-PRAVCOST-01      PIC Z(6)9.9(2).                              
004400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004500     03 MOD-SUAVCOST-01      PIC -(8)9.9(2).                              
004600*                                 SUMMA MEDELVÄRDESKOSTNAD I              
004700*                                 UTL.VALUTA                              
004800     03 MOD-IDLEVNR-01       PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000     03 MOD-PRARTBEL-01      PIC Z(7)9.9(5).                              
005100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
005200     03 MOD-PRKURS-01        PIC Z(5)9.9(5).                              
005300*                                 VALUTAKURS                              
005400     03 MOD-PRARTBEL-02      PIC Z(7)9.9(2).                              
005500*                                 BESTÄLLNINGSPRIS   PRARTBEL-002         
005600*                                 I LEVERANTÖRS VALUTA                    
005700     03 MOD-PRARTBTO-SC      PIC Z(8)9.9(2).                              
005800*                                 BRUTTOPRIS PER SÄLJBOLAG                
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 288 BYTES                                 
