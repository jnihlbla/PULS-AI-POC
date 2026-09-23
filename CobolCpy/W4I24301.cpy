000100 01  MID-W4I24301.                                                        
000200*                                 BESKRIVNING AV INDATA I MID FÖR         
000300*                                 SVARSPROGRAM EXTERN                     
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
003400        05 MID-KDBEHX        PIC X.                                       
003500*                                 BEHANDLINGSKOD-X                        
003600        05 MID-IDARTNR.                                                   
003700           07 MID-IDARTNR-1--9                                            
003800                             PIC 9(9).                                    
003900*                                 ARTIKELNUMMER                           
004000           07 MID-FILLER     PIC X.                                       
004100           07 MID-REKSIFFR   PIC 9.                                       
004200*                                 KONTROLLSIFFRA                          
004300        05 MID-IDDC-RAD      PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500        05 MID-IDKUNDRF-RO   PIC 9(7).                                    
004600*                                 ORDERNUMMER                             
004700        05 MID-KEYS          PIC X(18).                                   
004800*** END OF VILMAII-COPY LENGTH= 584 BYTES                                 
