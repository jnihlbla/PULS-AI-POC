000100 01  MOD-W6O34701.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O34701                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-ADLAGOMR-IN      PIC Z9.                                      
001300*                                 LAGEROMRÅDE                             
001400     03 MOD-ADLAGOMR-UT      PIC Z9.                                      
001500*                                 LAGEROMRÅDE                             
001600     03 MOD-ADGANG-FOM-IN    PIC Z9.                                      
001700*                                 GÅNG                                    
001800     03 MOD-ADGANG-FOM-UT    PIC Z9.                                      
001900*                                 GÅNG                                    
002000     03 MOD-ADGANG-TOM-IN    PIC Z9.                                      
002100*                                 GÅNG                                    
002200     03 MOD-ADGANG-TOM-UT    PIC Z9.                                      
002300*                                 GÅNG                                    
002400     03 MOD-ADSEC11-FOM-IN   PIC Z(2)9.                                   
002500*                                 LAGERPLATSNUMMER                        
002600     03 MOD-ADSEC11-FOM-UT   PIC Z(2)9.                                   
002700*                                 LAGERPLATSNUMMER                        
002800     03 MOD-ADSEC11-TOM-IN   PIC Z(2)9.                                   
002900*                                 LAGERPLATSNUMMER                        
003000     03 MOD-ADSEC11-TOM-UT   PIC Z(2)9.                                   
003100*                                 LAGERPLATSNUMMER                        
003200     03 MOD-ADLEVEL11-FOM-IN PIC 9.                                       
003300*                                 LAGERPLATSNUMMER                        
003400     03 MOD-ADLEVEL11-FOM-UT PIC 9.                                       
003500*                                 LAGERPLATSNUMMER                        
003600     03 MOD-ADLEVEL11-TOM-IN PIC 9.                                       
003700*                                 LAGERPLATSNUMMER                        
003800     03 MOD-ADLEVEL11-TOM-UT PIC 9.                                       
003900*                                 LAGERPLATSNUMMER                        
004000     03 MOD-KDLOC-IN         PIC X.                                       
004100*                                 TYP AV LAGERPLATS                       
004200     03 MOD-KDLOC-UT         PIC X.                                       
004300*                                 TYP AV LAGERPLATS                       
004400     03 MOD-KDFREQ-IN        PIC X(2).                                    
004500*                                 FREQUENCY CODE                          
004600     03 MOD-KDFREQ-UT        PIC X(2).                                    
004700*                                 FREQUENCY CODE                          
004800     03 MOD-TELOC-IN         PIC X.                                       
004900*                                 LOCATION INFORMATION                    
005000     03 MOD-TELOC-UT         PIC X.                                       
005100*                                 LOCATION INFORMATION                    
005200     03 MOD-KDPRT            PIC X(3).                                    
005300*                                 PRINTERKOD                              
005400     03 MOD-INDATA           OCCURS 14 TIMES.                             
005500*                                 LOCATION INFORMATION                    
005600        05 MOD-ADLAGOMR-LINE PIC Z9.                                      
005700*                                 LAGEROMRÅDE                             
005800        05 MOD-ADGANG-LINE   PIC Z(2)9.                                   
005900*                                 GÅNG                                    
006000        05 MOD-ADPLATS-LINE  PIC X(5).                                    
006100*                                 LAGERPLATSNUMMER                        
006200        05 MOD-KDLOC-LINE    PIC X.                                       
006300*                                 TYP AV LAGERPLATS                       
006400        05 MOD-KDFREQ-LINE   PIC X(2).                                    
006500*                                 FREQUENCY CODE                          
006600        05 MOD-KDSTOR-LINE   PIC X(3).                                    
006700*                                 STORAGE CODE                            
006800        05 MOD-TELOC-LINE    PIC X(15).                                   
006900*                                 LOCATION INFORMATION                    
007000        05 MOD-IDARTNR-LINE  PIC X(9).                                    
007100*                                 ARTIKELNUMMER                           
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 702 BYTES                                 
