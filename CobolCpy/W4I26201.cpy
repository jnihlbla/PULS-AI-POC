000100 01  MID-W4I26201-CTX.                                                    
000200*                                 INMATNING PROFORMARADER                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR-IN       PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR-UT       PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MID-KDFRAKT-UT       PIC X(2).                                    
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 MID-KDPROTYP-UT      PIC X.                                       
002200*                                 TYP AV PROFORMA                         
002300     03 MID-KDTRTYP          PIC X.                                       
002400*                                 IMS TRANSAKTIONSTYP                     
002500     03 MID-FLSLUT           PIC X.                                       
002600*                                 AVSLUTNINGSFLAGGA                       
002700     03 MID-W4I26201-GRP     OCCURS 14 TIMES.                             
002800        05 MID-IDARTNR-006   PIC X(11).                                   
002900*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003000        05 MID-KVBEART       PIC X(6).                                    
003100*                                 BESTÄLLT ANTAL STYCKEN                  
003200        05 MID-BERADREF      PIC X(10).                                   
003300*                                 KUNDENS RADREFERENS                     
003400        05 MID-PRARTNTO      PIC X(10).                                   
003500*                                 ARTIKELPRIS NETTO                       
003600        05 MID-KDKVBRYT      PIC X.                                       
003700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003800        05 MID-FLINVEST      PIC X.                                       
003900*                                 BYTES INVENTERINGSFLAGGA                
004000*** END OF VILMAII-COPY LENGTH= 595 BYTES                                 
