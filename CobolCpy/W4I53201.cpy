000100 01  W4I53201.                                                            
000200*                                                                         
000300     03 IDDISTR-IN           PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDDISTR-UT           PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR-IN          PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDNR-UT          PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 KDFRAKT-IN           PIC X(2).                                    
001200*                                 FRAKTSÄTT C1-C2 TILL KUND               
001300     03 KDFRAKT-UT           PIC X(2).                                    
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 IDSKEPPN-IN          PIC X(7).                                    
001600*                                 SKEPPNINGSNUMMER                        
001700     03 IDSKEPPN-UT          PIC X(7).                                    
001800*                                 SKEPPNINGSNUMMER                        
001900     03 IDDC-IN              PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 IDDC-UT              PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 R4533-35-FAELT.                                                   
002400*                                 VÄRDEN FÖR KOMMUNIKATION MED            
002500*                                 W40533 OCH W40535                       
002600*                                                                         
002700        05 R4533-35-TESTFAELT                                             
002800                             PIC X(4).                                    
002900        05 FILLER            PIC X(201).                                  
003000     03 SPARADE-VAERDEN.                                                  
003100        05 SPARAD-NYCKEL     PIC X(31).                                   
003200        05 SPARAD-RDE5-FILLER REDEFINES SPARAD-NYCKEL.                    
003300           07 SPARAD-RDE5.                                                
003400              09 SPARAT-IDKUNDNR-5                                        
003500                             PIC X(7).                                    
003600*                                 KUNDNUMMER                              
003700              09 SPARAT-KDORDSTA-5                                        
003800                             PIC X(2).                                    
003900              09 SPARAT-IDPRODNR-5                                        
004000                             PIC X(6).                                    
004100*                                 PRODUKTIONSNUMMER                       
004200              09 SPARAT-IDKUNDRF-5                                        
004300                             PIC X(10).                                   
004400*                                 KUNDENS REFERENS (ORDERID)              
004500              09 SPARAT-IDPLKLST-5                                        
004600                             PIC X(3).                                    
004700*                                 PLOCKLISTNUMMER                         
004800           07 FILLER         PIC X(3).                                    
004900        05 SPARAD-WDE7 REDEFINES SPARAD-NYCKEL.                           
005000           07 SPARAT-IDKUNDNR-7                                           
005100                             PIC X(7).                                    
005200*                                 KUNDNUMMER                              
005300           07 SPARAT-IDPRODNR-7                                           
005400                             PIC X(6).                                    
005500*                                 PRODUKTIONSNUMMER                       
005600           07 SPARAT-IDKOLLI-7                                            
005700                             PIC X(5).                                    
005800*                                 KOLLINUMMER                             
005900           07 SPARAT-IDKUNDRF-7                                           
006000                             PIC X(10).                                   
006100*                                 KUNDENS REFERENS (ORDERID)              
006200           07 SPARAT-IDPLKLST-7                                           
006300                             PIC X(3).                                    
006400*                                 PLOCKLISTNUMMER                         
