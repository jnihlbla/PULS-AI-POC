000100 01  MID-W4I32301.                                                        
000200*                                 MID-COPYTEXT FÖR FRÅGE-BILD             
000300*                                 PACK-UNDERLAG INFO                      
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
002000     03 MID-IDPURAD-IN       PIC X(5).                                    
002100     03 MID-IDPURAD-UT       PIC X(5).                                    
002200     03 MID-IDPRODNR-IN      PIC X(7).                                    
002300*                                 PRODUKTIONSNUMMER                       
002400     03 MID-IDPRODNR-UT      PIC X(7).                                    
002500*                                 PRODUKTIONSNUMMER                       
002600     03 MID-IDDC-IN          PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800     03 MID-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MID-FORSTA-IDPURAD   PIC 9(4).                                    
003100*                                 RADNUMMER PÅ PACKUNDERLAG               
003200     03 MID-FORSTA-IDKOLLI   PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400     03 MID-FORSTA-IDPLKLST  PIC 9(3).                                    
003500*                                 PLOCKLISTNUMMER                         
003600     03 MID-FORSTA-KOLLI-IDPRODNR                                         
003700                             PIC 9(7).                                    
003800*                                 PRODUKTIONSNUMMER                       
003900     03 MID-FORSTA-Q4-IDORDER                                             
004000                             PIC X(7).                                    
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200     03 MID-FORSTA-Q4-IDARTNR                                             
004300                             PIC X(9).                                    
004400*                                 ARTIKELNUMMER                           
004500     03 MID-FORSTA-Q4-IDLOPNR                                             
004600                             PIC 9(3).                                    
004700*                                 LÖPNUMMER                               
