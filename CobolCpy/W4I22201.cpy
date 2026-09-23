000100 01  MID-W4I22201-CTX.                                                    
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
001300     03 MID-FLVORKO          PIC X.                                       
001400*                                 VOR-KÖ FLAGGA                           
001500     03 MID-FLFORBI          PIC X.                                       
001600*                                 FÖRBIORDERFLAGGA                        
001700     03 MID-KDTRTYP          PIC X.                                       
001800*                                 IMS TRANSAKTIONSTYP                     
001900     03 MID-W4I22201-001-GRP OCCURS 14 TIMES.                             
002000        05 MID-IDARTNR-006   PIC X(11).                                   
002100*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
002200        05 MID-KVBEART       PIC X(6).                                    
002300*                                 BESTÄLLT ANTAL STYCKEN                  
002400        05 MID-BERADREF      PIC X(10).                                   
002500*                                 KUNDENS RADREFERENS                     
002600        05 MID-PRARTNTO      PIC X(10).                                   
002700*                                 ARTIKELPRIS NETTO                       
002800        05 MID-FLINVEST      PIC X.                                       
002900*                                 BYTES INVENTERINGSFLAGGA                
003000        05 MID-KDVRINFO      PIC X.                                       
003100*                                 PÅVERKAN I VR/DSP SYSTEM                
003200*** END OF VILMAII-COPY LENGTH= 567 BYTES                                 
