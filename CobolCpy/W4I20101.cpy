000100 01  MID-W4I20101.                                                        
000200*                                 MID-COPYTEXT FÖR W4020100               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MID-IDORDNR7-UT      PIC X(7).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-INPUT.                                                        
002000*                                 DATADEL MID, ÄNDRING ORDERHUVUD         
002100        05 MID-BELAGINS-GRP.                                              
002200*                                 LAGERINSTRUKTIONER                      
002300           07 MID-BELAGINS-DEL1                                           
002400                             PIC X(60).                                   
002500*                                 DEL AV LAGERINSTRUKTION                 
002600           07 MID-BELAGINS-DEL2                                           
002700                             PIC X(60).                                   
002800*                                 DEL AV LAGERINSTRUKTION                 
002900        05 MID-BEGMT.                                                     
003000*                                 GODSMOTTAGARNAMN                        
003100           07 MID-BEGMT-RAD1 PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 1                  
003300           07 MID-BEGMT-RAD2 PIC X(35).                                   
003400*                                 GODSMOTTAGARNAMN RAD 2                  
003500        05 MID-ADGMT.                                                     
003600*                                 GODSMOTTAGARADRESS                      
003700           07 MID-ADGMT-GATA PIC X(35).                                   
003800*                                 GODSMOTTAGARADRESS GATA                 
003900           07 MID-ADGMT-PADR PIC X(35).                                   
004000*                                 GODSMOTTAGARADRESS POSTADRESS           
004100           07 MID-ADGMT-LAND PIC X(35).                                   
004200*                                 GODSMOTTAGARADRESS LAND                 
004300        05 MID-BEGMRK.                                                    
004400*                                 GODSMÄRKE                               
004500           07 MID-BEGMRK-RAD1                                             
004600                             PIC X(30).                                   
004700*                                 GODSMÄRKE  RAD1                         
004800           07 MID-BEGMRK-RAD2                                             
004900                             PIC X(30).                                   
005000*                                 GODSMÄRKE  RAD2                         
005100        05 MID-KDFRAKT       PIC X(2).                                    
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300        05 MID-BEKUNDRF      PIC X(15).                                   
005400*                                 KUNDENS REFERENS                        
005500*** END OF VILMAII-COPY LENGTH= 410 BYTES                                 
