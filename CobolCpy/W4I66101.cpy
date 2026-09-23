000100 01  MID-W4I66101.                                                        
000200*                                 MID-COPYTEXT FÖR W40661                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDTRPTNR-UT      PIC X(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 MID-ADFLGEO-IN       PIC X(3).                                    
000800*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
000900     03 MID-ADFLGEO-UT       PIC X(3).                                    
001000*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001100     03 MID-ADFLOMR-IN       PIC X(3).                                    
001200*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001300     03 MID-ADFLOMR-UT       PIC X(3).                                    
001400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-IDDISTR-B1       PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MID-IDKUNDNR-B1      PIC 9(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MID-KDFRAKT-B1       PIC 9(2).                                    
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 MID-KDORDKLX-B1      PIC X.                                       
002600*                                 ORDERKLASS + BLANK                      
002700     03 MID-IDDISTR-BN       PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MID-IDKUNDNR-BN      PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MID-KDFRAKT-BN       PIC 9(2).                                    
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 MID-KDORDKLX-BN      PIC X.                                       
003400*                                 ORDERKLASS + BLANK                      
003500     03 MID-FLUTLAST         PIC X.                                       
003600*                                 KOLLI I UTLASTNINGSLAGER                
003700     03 MID-FLTOTMS          PIC X.                                       
003800*                                 SEND TO TMS FLAGGA                      
003900     03 MID-RAD              OCCURS 12 TIMES.                             
004000*                                                                         
004100        05 MID-KDCMD         PIC X.                                       
004200         88 MID-KDCMD-INGENTING                                           
004300                             VALUE ' '.                                   
004400         88 MID-KDCMD-DELETE VALUE 'D'                                    
004500                             'B'.                                         
004600         88 MID-KDCMD-REPLACE                                             
004700                             VALUE 'R'                                    
004800                             'Ä'.                                         
004900         88 MID-KDCMD-INSERT VALUE 'I'                                    
005000                             'N'                                          
005100                             'A'.                                         
005200         88 MID-KDCMD-SELECT VALUE 'S'                                    
005300                             'V'.                                         
005400         88 MID-KDCMD-PRINT  VALUE 'P'                                    
005500                             'P'.                                         
005600         88 MID-KDCMD-COPY   VALUE 'C'                                    
005700                             'K'.                                         
005800*                                 RAD-UPPDATERINGSKOMMANDO                
005900*                                  BLANK  = INGENTING                     
006000*                                  D , B  = DELETE                        
006100*                                  R , Ä  = REPLACE                       
006200*                                  I,N,A  = INSERT                        
006300*                                  S , V  = SELECT                        
006400*                                  P , P  = PRINT                         
006500*                                  C , K  = COPY                          
006600        05 MID-IDDISTR-FOM   PIC X(4).                                    
006700*                                 DISTRIKTNUMMER                          
006800        05 MID-IDDISTR-TOM   PIC X(4).                                    
006900*                                 DISTRIKTNUMMER                          
007000        05 MID-IDKUNDNR-FOM  PIC X(6).                                    
007100*                                 KUNDNUMMER                              
007200        05 MID-IDKUNDNR-TOM  PIC X(6).                                    
007300*                                 KUNDNUMMER                              
007400        05 MID-KDFRAKT       PIC X(2).                                    
007500*                                 FRAKTSÄTT DC TILL KUND                  
007600        05 MID-KDORDKLX      PIC X.                                       
007700*                                 ORDERKLASS + BLANK                      
007800        05 MID-IDDC-CROSS    PIC X(2).                                    
007900*                                 DC FÖR CROSS DOCKING                    
008000        05 MID-TEFLNOTE      PIC X(20).                                   
008100*                                 NOTERING FÄRDIGLAGRET                   
008200     03 MID-NYUPPLAEGG.                                                   
008300*                                                                         
008400        05 MID-IDDISTR-FOM-NY                                             
008500                             PIC 9(4).                                    
008600*                                 DISTRIKTNUMMER                          
008700        05 MID-IDDISTR-TOM-NY                                             
008800                             PIC 9(4).                                    
008900*                                 DISTRIKTNUMMER                          
009000        05 MID-IDKUNDNR-FOM-NY                                            
009100                             PIC 9(6).                                    
009200*                                 KUNDNUMMER                              
009300        05 MID-IDKUNDNR-TOM-NY                                            
009400                             PIC 9(6).                                    
009500*                                 KUNDNUMMER                              
009600        05 MID-KDFRAKT-NY    PIC 9(2).                                    
009700*                                 FRAKTSÄTT DC TILL KUND                  
009800        05 MID-KDORDKLX-NY   PIC X.                                       
009900*                                 ORDERKLASS + BLANK                      
010000        05 MID-IDDC-CROSS-NY PIC X(2).                                    
010100*                                 DC FÖR CROSS DOCKING                    
010200        05 MID-TEFLNOTE-NY   PIC X(20).                                   
010300*                                 NOTERING FÄRDIGLAGRET                   
010400*** END OF VILMAII-COPY LENGTH= 647 BYTES                                 
