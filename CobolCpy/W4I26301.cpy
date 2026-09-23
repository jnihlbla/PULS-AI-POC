000100 01  MID-W4I26301.                                                        
000200*                                 SVARSBILD PROFORMARADER                 
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
002000*                                 FRAKTSÄTT C1-C2 TILL KUND               
002100     03 MID-KDPROTYP-UT      PIC X.                                       
002200*                                 TYP AV PROFORMA                         
002300     03 MID-KDORDBEK-NEXT    PIC 9(2).                                    
002400*                                 ORDERBEKRÄFTELSEKOD                     
002500     03 MID-KDBEHX-NEXT      PIC X.                                       
002600*                                 BEHANDLINGSKOD-X                        
002700     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
003000*                                 LÖPNUMMER                               
003100     03 MID-IDSEKVNR-NEXT    PIC 9(3).                                    
003200*                                 GENERELLT SEKVENSNUMMER                 
003300     03 MID-RAD              OCCURS 13 TIMES.                             
003400        05 MID-KDORDBEK      PIC 9(2).                                    
003500*                                 ORDERBEKRÄFTELSEKOD                     
003600        05 MID-KDBEHX        PIC X.                                       
003700*                                 BEHANDLINGSKOD-X                        
003800        05 MID-IDARTNR.                                                   
003900           07 MID-IDARTNR-1--9                                            
004000                             PIC 9(9).                                    
004100*                                 ARTIKELNUMMER                           
004200           07 MID-FILLER     PIC X.                                       
004300           07 MID-REKSIFFR   PIC 9.                                       
004400*                                 KONTROLLSIFFRA                          
004500        05 MID-NYCKLAR       PIC X(16).                                   
004600*** END OF VILMAII-COPY LENGTH= 455 BYTES                                 
