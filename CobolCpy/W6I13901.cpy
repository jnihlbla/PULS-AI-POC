000100 01  MID-W6I13901.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I13901                                
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
005800     03 MID-IDKR-ENTER       PIC 9(5).                                    
005900*                                 KONTROLLRAPPORT NUMMER                  
006000     03 MID-IDKR-NEXT        PIC 9(5).                                    
006100*                                 KONTROLLRAPPORT NUMMER                  
006200     03 MID-IDKVAINF-ENTER   PIC 9(2).                                    
006300*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
006400     03 MID-IDKVAINF-NEXT    PIC 9(2).                                    
006500*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
006600     03 MID-FLAGGA-KR-HOPP   PIC X.                                       
006700*                                 GOKDKÄND                                
006800     03 MID-INPUT.                                                        
006900        05 MID-IDUSER-PRI    PIC X(8).                                    
007000*                                 ANVÄNDARENS SÄKERHETS ID                
007100        05 MID-FLAGGA-PRI    PIC X.                                       
007200*                                 GOKDKÄND                                
007300        05 MID-BEANST        PIC X(25).                                   
007400*                                 ANSTÄLLDS NAMN                          
007500        05 MID-IDUSER-SEK    PIC X(8).                                    
007600*                                 ANVÄNDARENS SÄKERHETS ID                
007700        05 MID-FLAGGA-SEK    PIC X.                                       
007800*                                 GOKDKÄND                                
007900        05 MID-IDUSER-ADM    PIC X(8).                                    
008000*                                 ANVÄNDARENS SÄKERHETS ID                
008100        05 MID-FLAGGA-ADM    PIC X.                                       
008200*                                 GOKDKÄND                                
008300        05 MID-FLAGGA-GODK   PIC X.                                       
008400*                                 GOKDKÄND                                
008500        05 MID-IDUSER-APR    PIC X(8).                                    
008600*                                 ANVÄNDARENS SÄKERHETS ID                
008700*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
