000100 01  MID-W4I20601.                                                        
000200*                                 INMATNING ORDERRADER                    
000300     03 MID-IDDISTR          PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR5         PIC X(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDORDNR5-REG     PIC X(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 MID-KDORDKL          PIC X.                                       
001200*                                 ORDERKLASS                              
001300     03 MID-KDFRAKT          PIC X(2).                                    
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 MID-KDTRTYP          PIC X.                                       
001600*                                 IMS TRANSAKTIONSTYP                     
001700     03 MID-RADER            OCCURS 14 TIMES.                             
001800        05 MID-IDARTNR       PIC X(11).                                   
001900*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
002000        05 MID-KVBEART       PIC X(6).                                    
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200        05 MID-BERADREF      PIC X(10).                                   
002300*                                 KUNDENS RADREFERENS                     
002400        05 MID-PRARTNTO      PIC X(10).                                   
002500*                                 ARTIKELPRIS NETTO                       
002600        05 MID-TITPO         PIC X(6).                                    
002700*                                 PLANERAD ORDERDATUM                     
002800        05 MID-FLRESTN       PIC X.                                       
002900*                                 RESTNOTERING ?                          
003000        05 MID-FLSLATT       PIC X.                                       
003100*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003200*                                 LL BERÄKNAS ELLER EJ                    
003300*                                 OM FLRESTN = J OCH FLSLATT = J,         
003400*                                  DÅ BERÄKNAS KVSLATT                    
003500        05 MID-KDKVBRYT      PIC X.                                       
003600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003700        05 MID-FLINVEST      PIC X.                                       
003800*                                 BYTES INVENTERINGSFLAGGA                
003900        05 MID-KDVRINFO      PIC X.                                       
004000*                                 PÅVERKAN I VR/DSP SYSTEM                
004100*** END OF VILMAII-COPY LENGTH= 696 BYTES                                 
