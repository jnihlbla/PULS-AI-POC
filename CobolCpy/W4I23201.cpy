000100 01  MID-W4I23201-CTX.                                                    
000200*                                 INMATNING ORDERRADER                    
000300     03 MID-IDDISTR          PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR5         PIC X(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-KDORDKL          PIC X.                                       
001000*                                 ORDERKLASS                              
001100     03 MID-KDFRAKT          PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-W4I23201-001-GRP OCCURS 14 TIMES.                             
001400        05 MID-IDARTNR-006   PIC X(11).                                   
001500*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
001600        05 MID-KVBEART       PIC X(6).                                    
001700*                                 BESTÄLLT ANTAL STYCKEN                  
001800        05 MID-BERADREF      PIC X(10).                                   
001900*                                 KUNDENS RADREFERENS                     
002000        05 MID-PRARTNTO      PIC X(10).                                   
002100*                                 ARTIKELPRIS NETTO                       
002200        05 MID-FLINVEST      PIC X.                                       
002300*                                 BYTES INVENTERINGSFLAGGA                
002400*** END OF VILMAII-COPY LENGTH= 550 BYTES                                 
