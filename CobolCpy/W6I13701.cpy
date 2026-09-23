000100 01  MID-W6I13701.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I13701                                
000400     03 MID-GROUP.                                                        
000500*                                 LINES                                   
000600        05 MID-ADINLOMR-PRT  PIC X(4).                                    
000700*                                 PRINTERPLACERING                        
000800        05 MID-IDINLVGN-IN   PIC X(3).                                    
000900*                                 VAGNSIDENTITET                          
001000        05 MID-IDINLVGN-UT   PIC X(3).                                    
001100*                                 VAGNSIDENTITET                          
001200        05 MID-ADINLOMR-IN   PIC X(4).                                    
001300*                                 INLEVERANSOMRÅDE                        
001400        05 MID-ADINLOMR-UT   PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600        05 MID-ADINLOMR-NXT-IN                                            
001700                             PIC X(4).                                    
001800*                                 INLEVERANSOMRÅDE NÄSTA                  
001900        05 MID-ADINLOMR-NXT-UT                                            
002000                             PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE NÄSTA                  
002200        05 MID-KDINLQ-IN     PIC X.                                       
002300*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002400        05 MID-KDINLQ-UT     PIC X.                                       
002500*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002600        05 MID-BEFT-FOM-IN   PIC X(2).                                    
002700*                                 FÖRPACKNINGSTYP                         
002800        05 MID-BEFT-FOM-UT   PIC X(2).                                    
002900*                                 FÖRPACKNINGSTYP                         
003000        05 MID-BEFT-TOM-IN   PIC X(2).                                    
003100*                                 FÖRPACKNINGSTYP                         
003200        05 MID-BEFT-TOM-UT   PIC X(2).                                    
003300*                                 FÖRPACKNINGSTYP                         
003400        05 MID-FLINLFB-IN    PIC X.                                       
003500*                                 VALD TILL FÖRBEHANDLING                 
003600        05 MID-FLINLFB-UT    PIC X.                                       
003700*                                 VALD TILL FÖRBEHANDLING                 
003800        05 MID-IDLEVNR-KOLLI-IN                                           
003900                             PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER KOLLI                  
004100        05 MID-IDLEVNR-KOLLI-UT                                           
004200                             PIC X(5).                                    
004300*                                 LEVERANTÖRNUMMER KOLLI                  
004400        05 MID-IDOKOLLI-IN   PIC X(9).                                    
004500*                                 ODETTE KOLLINUMMER                      
004600        05 MID-IDOKOLLI-UT   PIC X(9).                                    
004700*                                 ODETTE KOLLINUMMER                      
004800        05 MID-IDLOPNRM-IN   PIC X(9).                                    
004900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005000*                                 (0VVDLLLLK)                             
005100        05 MID-IDLOPNRM-UT   PIC X(9).                                    
005200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005300*                                 (0VVDLLLLK)                             
005400        05 MID-IDDC-IN       PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600        05 MID-IDDC-UT       PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800     03 MID-INPUT1.                                                       
005900        05 MID-ADLAGOMR-UPD  PIC X(2).                                    
006000*                                 LAGEROMRÅDE                             
006100        05 MID-ADINLOMR-UPD  PIC X(4).                                    
006200*                                 INLEVERANSOMRÅDE                        
006300        05 MID-FLPRIO-UPD    PIC X.                                       
006400*                                 PRIORITERAD                             
006500     03 MID-ADLAGOMR         PIC X(2).                                    
006600*                                 LAGEROMRÅDE                             
006700     03 MID-INPUT2.                                                       
006800        05 MID-RAD           OCCURS 11 TIMES.                             
006900           07 MID-IDLOPNRM-RAD                                            
007000                             PIC X(8).                                    
007100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007200*                                 (0VVDLLLLK)                             
007300           07 MID-IDRADNR-RAD                                             
007400                             PIC X(3).                                    
007500*                                 RADNUMMER                               
