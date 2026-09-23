000100 01  MID-W4I24201.                                                        
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
001300     03 MID-KDTRTYP          PIC X.                                       
001400*                                 IMS TRANSAKTIONSTYP                     
001500     03 MID-BEVOLREF         PIC X(10).                                   
001600*                                 VOLVO REFERENS                          
001700     03 MID-FLTILLK          OCCURS 14 TIMES                              
001800                             PIC X.                                       
001900*                                 TILLKOMMANDE ARTIKEL ?                  
002000     03 MID-RADER            OCCURS 14 TIMES.                             
002100        05 MID-IDARTNR       PIC X(11).                                   
002200*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
002300        05 MID-KVBEART       PIC X(6).                                    
002400*                                 BESTÄLLT ANTAL STYCKEN                  
002500        05 MID-BERADREF      PIC X(10).                                   
002600*                                 KUNDENS RADREFERENS                     
002700        05 MID-TITPO         PIC X(6).                                    
002800*                                 PLANERAD ORDERDATUM                     
002900        05 MID-FLRESTN       PIC X.                                       
003000*                                 RESTNOTERING ?                          
003100        05 MID-FLSLATT       PIC X.                                       
003200*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003300*                                 LL BERÄKNAS ELLER EJ                    
003400*                                 OM FLRESTN = J OCH FLSLATT = J,         
003500*                                  DÅ BERÄKNAS KVSLATT                    
003600        05 MID-KDKVBRYT      PIC X.                                       
003700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003800        05 MID-FLORDING      PIC X.                                       
003900*** END OF VILMAII-COPY LENGTH= 561 BYTES                                 
