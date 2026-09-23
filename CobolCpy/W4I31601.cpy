000100 01  MID-W4I31601.                                                        
000200*                                 MID-COPYTEXT PGM W40316                 
000300*                                 KOLLI RÄTTNING                          
000400     03 MID-IDANSTNR-IN      PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600     03 MID-IDANSTNR-UT      PIC X(5).                                    
000700*                                 ANSTÄLLNINGSNUMMER                      
000800     03 MID-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MID-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MID-IDORDNR-IN       PIC X(5).                                    
001700*                                 ORDERNUMMER                             
001800     03 MID-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 MID-IDKOLLI-IN       PIC X(5).                                    
002100*                                 KOLLINUMMER                             
002200     03 MID-IDKOLLI-UT       PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400     03 MID-IDPRODNR-IN      PIC X(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDPRODNR-UT      PIC X(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800     03 MID-IDDC-IN          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MID-IDDC-UT          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MID-IDRADNR-SPAR     PIC 9(4).                                    
003300*                                 RADNUMMER                               
003400     03 MID-KVORDRAD-SPAR    PIC 9(5).                                    
003500*                                 ANTAL ORDERRADER                        
003600     03 MID-IDRADNR-START    PIC X(4).                                    
003700*                                 RADNUMMER                               
003800     03 MID-FLBACKA-ALLA     PIC X.                                       
003900*                                 ALLMÄN SVARSFLAGGA                      
004000     03 MID-RAD              OCCURS 13 TIMES.                             
004100        05 MID-IDRADNR       PIC X(4).                                    
004200*                                 RADNUMMER                               
004300        05 MID-KVLEVART      PIC X(6).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500        05 MID-FLBACKA       PIC X.                                       
004600*                                 ALLMÄN SVARSFLAGGA                      
004700*** END OF VILMAII-COPY LENGTH= 225 BYTES                                 
