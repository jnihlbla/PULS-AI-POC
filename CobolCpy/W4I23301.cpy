000100 01  MID-W4I23301.                                                        
000200*                                 BESKRIVNING AV INDATA I MID FÖR         
000300*                                 SVARSPROGRAM                            
000400*                                                                         
000500     03 MID-IDDISTR-IN       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDORDNR-IN       PIC X(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR-UT       PIC X(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MID-FLANNULL         PIC X.                                       
002000*                                 ANNULLATION                             
002100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
002400*                                 LÖPNUMMER                               
002500     03 MID-IDSEKVNR-NEXT    PIC 9(3).                                    
002600*                                 GENERELLT SEKVENSNUMMER                 
002700     03 MID-IDDC-NEXT        PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-KDORDBEK-NEXT    PIC 9(2).                                    
003000*                                 ORDERBEKRÄFTELSEKOD                     
003100     03 MID-RAD              OCCURS 13 TIMES.                             
003200        05 MID-KDORDBEK      PIC 9(2).                                    
003300*                                 ORDERBEKRÄFTELSEKOD                     
003400        05 MID-IDARTNR.                                                   
003500           07 MID-IDARTNR-1--9                                            
003600                             PIC 9(9).                                    
003700*                                 ARTIKELNUMMER                           
003800           07 MID-FILLER     PIC X.                                       
003900           07 MID-REKSIFFR   PIC 9.                                       
004000*                                 KONTROLLSIFFRA                          
004100        05 MID-IDDC-RAD      PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300        05 MID-KEYS          PIC X(6).                                    
004400*** END OF VILMAII-COPY LENGTH= 324 BYTES                                 
