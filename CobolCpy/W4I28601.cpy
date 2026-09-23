000100 01  MID-W4I28601.                                                        
000200*                                 RESPONSE FROM FLS/MIC                   
000300*                                 USED IN W4028600                        
000400     03 MID-IDDISTR          PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDKUNDNR         PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 MID-IDORDNR7         PIC 9(7).                                    
000900*                                 ORDERNUMMER                             
001000     03 MID-IDDC             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-TIMESTAMP        PIC X(19).                                   
001300     03 MID-FLRESP           PIC X.                                       
001400     03 MID-FLSTATUS         PIC X.                                       
001500     03 MID-PARTNER          OCCURS 2 TIMES.                              
001600        05 MID-KDPARTNR      PIC X(9).                                    
001700        05 MID-BEPARTNR      PIC X(25).                                   
001800        05 MID-IDPARMA       PIC X(9).                                    
001900     03 MID-IDARTNR          OCCURS 100 TIMES                             
002000                             PIC 9(8).                                    
002100*                                 ARTIKELNUMMER                           
002200*** END OF VILMAII-COPY LENGTH= 926 BYTES                                 
