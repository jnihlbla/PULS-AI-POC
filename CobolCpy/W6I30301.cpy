000100 01  MID-W6I30301.                                                        
000200*                                                                         
000300     03 MID-IDFAKT-IN        PIC X(7).                                    
000400*                                 FAKTURANUMMER                           
000500     03 MID-IDFAKT-UT        PIC X(7).                                    
000600*                                 FAKTURANUMMER                           
000700     03 MID-IDKUNDRF-IN      PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 MID-IDKUNDRF-UT      PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDSPRAK-IN       PIC X(3).                                    
002000*                                 NATIONALITETSTECKEN                     
002100*                                 SPRÅKIDENTIFIKATION                     
002200     03 MID-IDSPRAK-UT       PIC X(3).                                    
002300*                                 NATIONALITETSTECKEN                     
002400*                                 SPRÅKIDENTIFIKATION                     
002500     03 MID-IDDC-IN          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MID-IDDC-UT          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MID-DAINLEV-ENTER    PIC 9(16).                                   
003400*                                 INLEVERANS NUMMER                       
003500     03 MID-DAINLEV-NEXT     PIC 9(16).                                   
003600*                                 INLEVERANS NUMMER                       
003700     03 MID-KOLLI-KLART      PIC X.                                       
003800     03 MID-IDUSER-003       PIC X(5).                                    
003900*                                 ANSVARIGT USERID INLÄGGN.(R32)          
004000     03 MID-INPUT            OCCURS 12 TIMES.                             
004100*                                                                         
004200        05 MID-CMD-IN        PIC X(3).                                    
004300        05 MID-KVANTMOT-IN   PIC X(6).                                    
004400*                                 ANTAL MOTTAGET                          
004500        05 MID-IDARTNR       PIC 9(9).                                    
004600*                                 ARTIKELNUMMER                           
004700        05 MID-ADLAGOMR      PIC X(2).                                    
004800*                                 LAGEROMRÅDE                             
004900        05 MID-ADGANG        PIC X(2).                                    
005000*                                 GÅNG                                    
005100        05 MID-ADPLATS       PIC X(5).                                    
005200*                                 LAGERPLATSNUMMER                        
005300        05 MID-KVSKROT-IN    PIC X(7).                                    
005400*                                 ANTAL SENASTE SKROTORDER                
005500     03 MID-NEW.                                                          
005600*                                                                         
005700        05 MID-KVANTMOT-INM  PIC X(6).                                    
005800*                                 ANTAL MOTTAGET                          
005900        05 MID-IDARTNR-INM   PIC X(9).                                    
006000*                                 ARTIKELNUMMER                           
006100        05 MID-ADLAGOMR-INM  PIC X(2).                                    
006200*                                 LAGEROMRÅDE                             
006300        05 MID-ADGANG-INM    PIC X(2).                                    
006400*                                 GÅNG                                    
006500        05 MID-ADPLATS-INM   PIC X(5).                                    
006600*                                 LAGERPLATSNUMMER                        
006700        05 MID-CMD-INM       PIC X(3).                                    
006800        05 MID-KVSKROT-INM   PIC X(7).                                    
006900*                                 ANTAL SENASTE SKROTORDER                
007000     03 MID-MODFAELT-IN      PIC X(500).                                  
007100*** END OF VILMAII-COPY LENGTH= 1064 BYTES                                
