000100 01  MID-W4I24401.                                                        
000200*                                 TILLÄGG ORDERRADER EXTERN ORDER         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR-IN       PIC X(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-KDORDKL-UT       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-KDFRAKT-UT       PIC X(2).                                    
001800*                                 FRAKTSÄTT C1-C2 TILL KUND               
001900     03 MID-KDTRTYP          PIC X.                                       
002000*                                 IMS TRANSAKTIONSTYP                     
002100     03 MID-RADER            OCCURS 14 TIMES.                             
002200        05 MID-IDARTNR       PIC X(11).                                   
002300*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
002400        05 MID-KVBEART       PIC X(6).                                    
002500*                                 BESTÄLLT ANTAL STYCKEN                  
002600        05 MID-BERADREF      PIC X(10).                                   
002700*                                 KUNDENS RADREFERENS                     
002800        05 MID-TITPO         PIC X(6).                                    
002900*                                 PLANERAD ORDERDATUM                     
003000        05 MID-FLRESTN       PIC X.                                       
003100*                                 RESTNOTERING ?                          
003200        05 MID-FLSLATT       PIC X.                                       
003300*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
003400*                                 LL BERÄKNAS ELLER EJ                    
003500*                                 OM FLRESTN = J OCH FLSLATT = J,         
003600*                                  DÅ BERÄKNAS KVSLATT                    
003700        05 MID-KDKVBRYT      PIC X.                                       
003800*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003900*** END OF VILMAII-COPY LENGTH= 538 BYTES                                 
