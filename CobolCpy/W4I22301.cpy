000100 01  MID-W4I22301.                                                        
000200*                                 BESKRIVNING AV INDATA I MID FÖR         
000300*                                 SVARSPROGRAM                            
000400*                                                                         
000500*                                                                         
000600     03 MID-IDDISTR-IN       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR-IN       PIC X(5).                                    
001100*                                 ORDERNUMMER                             
001200     03 MID-IDDISTR-UT       PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MID-IDORDNR-UT       PIC X(5).                                    
001700*                                 ORDERNUMMER                             
001800     03 MID-KDORDKL-UT       PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 MID-FLANNULL         PIC X.                                       
002100*                                 ANNULLATION                             
002200     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
002500*                                 LÖPNUMMER                               
002600     03 MID-IDSEKVNR-NEXT    PIC 9(3).                                    
002700*                                 GENERELLT SEKVENSNUMMER                 
002800     03 MID-IDDC-NEXT        PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MID-KDORDBEK-NEXT    PIC 9(2).                                    
003100*                                 ORDERBEKRÄFTELSEKOD                     
003200     03 MID-RAD              OCCURS 13 TIMES.                             
003300        05 MID-KDORDBEK      PIC 9(2).                                    
003400*                                 ORDERBEKRÄFTELSEKOD                     
003500        05 MID-KDBEHX        PIC X.                                       
003600*                                 BEHANDLINGSKOD-X                        
003700        05 MID-IDARTNR.                                                   
003800           07 MID-IDARTNR-1--9                                            
003900                             PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100           07 MID-FILLER     PIC X.                                       
004200           07 MID-REKSIFFR   PIC 9.                                       
004300*                                 KONTROLLSIFFRA                          
004400        05 MID-IDDC-RAD      PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600        05 MID-IDKUNDRF-RO   PIC 9(7).                                    
004700*                                 ORDERNUMMER                             
004800        05 MID-KEYS          PIC X(18).                                   
004900*** END OF VILMAII-COPY LENGTH= 584 BYTES                                 
