000100 01  W51233.                                                              
000200*                                 ANVÄNDS VID NEDLÄSNING AV               
000300*                                 INFO FRÅN WDK6 FÖR                      
000400*                                 VARULAGERVÄRDERING.                     
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDLEVNR              PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001200*                                 PRODUKTSLAG                             
001300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP                          
001500     03 KVAKS                PIC S9(7)           COMP-3.                  
001600*                                 ANKOMSTSALDO                            
001700     03 KVEFRS               PIC S9(7)           COMP-3.                  
001800*                                 EJ FAKTURERAT ANTAL STYCK               
001900     03 KVLS                 PIC S9(7)           COMP-3.                  
002000*                                 LAGERSALDO                              
002100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002200*                                 ARTIKELSTANDARDPRIS                     
002300     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
002400*                                 INKÖPSPRIS                              
002500     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
002600*                                 TULLFAKTOR                              
002700     03 BEST-PRISER          OCCURS 5 TIMES.                              
002800        05 TIPRLIST          PIC S9(7)           COMP-3.                  
002900*                                 PRISLISTEDATUM (AAMMDD)                 
003000        05 SUINLEV-PR        PIC S9(3)           COMP-3.                  
003100*                                 ANTAL INLEV. TILL DETTA PRIS            
003200        05 PRARTBEL-PR       PIC S9(8)V9(5)      COMP-3.                  
003300*                                 DETTA BESTÄLLNINGSPRIS                  
003400*                                 (I LEVERANTÖRENS VALUTA)                
003500        05 KDVALISO          PIC X(3).                                    
003600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003700*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
