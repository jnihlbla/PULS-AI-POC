000100 01  MID-W6I13501.                                                        
000200*                                 MID-COPYTEXT FÖR W6013500               
000300     03 MID-GROUP.                                                        
000400*                                 LINES                                   
000500        05 MID-ADINLOMR-PRT  PIC X(4).                                    
000600*                                 PRINTERPLACERING                        
000700        05 MID-IDINLVGN-IN   PIC X(3).                                    
000800*                                 VAGNSIDENTITET                          
000900        05 MID-IDINLVGN-UT   PIC X(3).                                    
001000*                                 VAGNSIDENTITET                          
001100        05 MID-ADINLOMR-IN   PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE                        
001300        05 MID-ADINLOMR-UT   PIC X(4).                                    
001400*                                 INLEVERANSOMRÅDE                        
001500        05 MID-ADINLOMR-NXT-IN                                            
001600                             PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE NÄSTA                  
001800        05 MID-ADINLOMR-NXT-UT                                            
001900                             PIC X(4).                                    
002000*                                 INLEVERANSOMRÅDE NÄSTA                  
002100        05 MID-KDINLQ-IN     PIC X.                                       
002200*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002300        05 MID-KDINLQ-UT     PIC X.                                       
002400*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002500        05 MID-BEFT-FOM-IN   PIC X(2).                                    
002600*                                 FÖRPACKNINGSTYP                         
002700        05 MID-BEFT-FOM-UT   PIC X(2).                                    
002800*                                 FÖRPACKNINGSTYP                         
002900        05 MID-BEFT-TOM-IN   PIC X(2).                                    
003000*                                 FÖRPACKNINGSTYP                         
003100        05 MID-BEFT-TOM-UT   PIC X(2).                                    
003200*                                 FÖRPACKNINGSTYP                         
003300        05 MID-FLINLFB-IN    PIC X.                                       
003400*                                 VALD TILL FÖRBEHANDLING                 
003500        05 MID-FLINLFB-UT    PIC X.                                       
003600*                                 VALD TILL FÖRBEHANDLING                 
003700        05 MID-IDLEVNR-KOLLI-IN                                           
003800                             PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER KOLLI                  
004000        05 MID-IDLEVNR-KOLLI-UT                                           
004100                             PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER KOLLI                  
004300        05 MID-IDOKOLLI-IN   PIC X(9).                                    
004400*                                 ODETTE KOLLINUMMER                      
004500        05 MID-IDOKOLLI-UT   PIC X(9).                                    
004600*                                 ODETTE KOLLINUMMER                      
004700        05 MID-IDLOPNRM-IN   PIC X(9).                                    
004800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004900*                                 (0VVDLLLLK)                             
005000        05 MID-IDLOPNRM-UT   PIC X(9).                                    
005100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005200*                                 (0VVDLLLLK)                             
005300        05 MID-IDDC-IN       PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500        05 MID-IDDC-UT       PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700     03 MID-RAD              OCCURS 14 TIMES.                             
005800        05 MID-RAD           OCCURS 2 TIMES.                              
005900           07 MID-IDLEVNR-KOLLI                                           
006000                             PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER KOLLI                  
006200           07 MID-IDOKOLLI   PIC X(9).                                    
006300*                                 ODETTE KOLLINUMMER                      
006400     03 MID-FLGODK           PIC X.                                       
