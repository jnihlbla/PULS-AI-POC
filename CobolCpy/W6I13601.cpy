000100 01  MID-W6I13601.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I13601                                
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
005800     03 MID-FLPREPRA         PIC X.                                       
005900*                                 FÖRBEHANDLINGSRAPPORTSFLAGGA            
006000     03 MID-RAD              OCCURS 6 TIMES.                              
006100        05 MID-KDFLETI       PIC X(2).                                    
006200*                                 FLAGGA/ETIKETTVAL                       
006300        05 MID-KVFLETI       PIC X(2).                                    
006400*                                 ANTAL FLAGGOR EL ETIKETTER              
006500        05 MID-KVINLART      PIC X(6).                                    
006600*                                 ANTAL I PARTIRAD                        
006700        05 MID-KDKLIPRI      PIC X.                                       
006800*                                 PRIORITETSKOD KOLLI                     
006900        05 MID-FLSATS        PIC X.                                       
007000*                                 SATSARTIKEL                             
007100        05 MID-FLPREPKL      PIC X.                                       
007200*                                 FÖRPACKAT?                              
007300     03 MID-KVAVIS-MOT       PIC X(6).                                    
007400*                                 AVISERAT ANTAL                          
007500     03 MID-IDANSTNR         PIC X(5).                                    
007600*                                 ANSTÄLLNINGSNUMMER                      
007700     03 MID-FLSVS            PIC X.                                       
007800*                                 ANGER GENERELLT OM NÅGOT AVSER          
007900*                                 SVS                                     
008000*** END OF VILMAII-COPY LENGTH= 179 BYTES                                 
