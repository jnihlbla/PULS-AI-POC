000100 01  MOD-W4O23201-CTX.                                                    
000200*                                 MOD AREA FÖR ORDERRADER                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR          PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR         PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR5         PIC X(5).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-KDORDKL          PIC X.                                       
001500*                                 ORDERKLASS                              
001600     03 MOD-KDFRAKT          PIC Z9.                                      
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 MOD-TEDDI            PIC X(11).                                   
001900*                                 TEXTFÄLT DDI                            
002000     03 MOD-KDVALISO         PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200     03 MOD-W4O23201-001-GRP OCCURS 14 TIMES.                             
002300        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-IDARTNR-006   PIC X(11).                                   
002600*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
002700        05 MOD-KVBEART-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-KVBEART       PIC Z(5)9.                                   
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100        05 MOD-BERADREF      PIC X(10).                                   
003200*                                 KUNDENS RADREFERENS                     
003300        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
003600*                                 ARTIKELPRIS NETTO                       
003700        05 MOD-FLINVEST-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-FLINVEST      PIC X.                                       
004000*                                 BYTES INVENTERINGSFLAGGA                
004100     03 MOD-FILLERX1         PIC X.                                       
004200     03 MOD-TEMFSINF         PIC X(55).                                   
004300*                                 INFORMATIONSMEDDELANDE                  
004400*** END OF VILMAII-COPY LENGTH= 776 BYTES                                 
