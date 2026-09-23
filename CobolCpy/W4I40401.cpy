000100 01  MID-W4I40401.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-REF-IN      PIC X(2).                                    
000600*                                 SÄNDANDE LAGER FÖR REFILL               
000700     03 MID-FLTABORT         PIC X.                                       
000800*                                 BORTTAGSFLAGGA                          
000900     03 MID-INPUT.                                                        
001000        05 MID-KDFRAKT-BPS-IN                                             
001100                             PIC 9(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300        05 MID-KDFRAKT-SBPS-IN                                            
001400                             PIC 9(2).                                    
001500*                                 FRAKTSÄTT DC TILL KUND                  
001600        05 MID-IDDISTR-REFILL-IN                                          
001700                             PIC 9(4).                                    
001800*                                 REFILL DISTRIKT                         
001900        05 MID-IDDISTR-RETUR-IN                                           
002000                             PIC 9(4).                                    
002100*                                 RETUR DISTRIKT                          
002200        05 MID-IDDISTR-QRETUR-IN                                          
002300                             PIC 9(4).                                    
002400*                                 KAVLITET RETUR DISTRIKT                 
002500        05 MID-IDDISTR-SKROT-IN                                           
002600                             PIC 9(4).                                    
002700*                                 SKROT DISTRIKT                          
002800        05 MID-IDDISTR-QSKROT-IN                                          
002900                             PIC 9(4).                                    
003000*                                 KVALITET SKROT DISTRIKT                 
003100        05 MID-IDDISTR-RSKROT-IN                                          
003200                             PIC 9(4).                                    
003300*                                 SKROT DISTRIKT FÖR RETURER              
003400        05 MID-IDDISTR-MIX-IN                                             
003500                             PIC 9(4).                                    
003600*                                 DISTRIKT JUSTERING/BLAND.ART.           
003700        05 MID-IDDISTR-JUST-IN                                            
003800                             PIC 9(4).                                    
003900*                                 DISTRIKT FÖR JUSTERINGSORDER            
004000        05 MID-IDKUNDNR-SORD-IN                                           
004100                             PIC 9(6).                                    
004200*                                 KUNDNUMMER FÖR SNABBORDER               
004300        05 MID-IDKUNDNR-RETUR-IN                                          
004400                             PIC 9(6).                                    
004500*                                 KUNDNUMMER FÖR RETUR                    
004600        05 MID-IDKUNDNR-QRETUR-IN                                         
004700                             PIC 9(6).                                    
004800*                                 KUNDNUMMER FÖR KVALITETSRETUR           
004900        05 MID-IDKUNDNR-SKROT-IN                                          
005000                             PIC 9(6).                                    
005100*                                 KUNDNUMMER FÖR SKROT                    
005200        05 MID-IDKUNDNR-QSKROT-IN                                         
005300                             PIC 9(6).                                    
005400*                                 KUNDNUMMER FÖR KVALITET SKROT           
005500        05 MID-IDKUNDNR-RSKROT-IN                                         
005600                             PIC 9(6).                                    
005700*                                 KUNDNUMMER FÖR SKROT AV RETUR           
005800        05 MID-IDKUNDNR-MIX-IN                                            
005900                             PIC 9(6).                                    
006000*                                 KUND JUSTERING/BLAND.ART.               
006100        05 MID-IDKUNDNR-JUST-IN                                           
006200                             PIC 9(6).                                    
006300*                                 KUND FÖR JUSTERINGSORDER                
006400        05 MID-IDKUNDNR-BPS-IN                                            
006500                             PIC 9(6).                                    
006600*                                 KUNDNUMMER FÖR BYPASSORDER              
006700        05 MID-IDKUNDNR-TRETUR-IN                                         
006800                             PIC 9(6).                                    
006900*                                 KUNDNUMMER FÖR TOTALRETUR               
007000        05 MID-IDKUNDNR-SQRET-IN                                          
007100                             PIC 9(6).                                    
007200*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
007300        05 MID-IDKUNDNR-SBPS-IN                                           
007400                             PIC 9(6).                                    
007500*                                 KUND FÖR SNABB BYPASSORDER              
007600        05 MID-IDKUNDNR-SRETUR-IN                                         
007700                             PIC 9(6).                                    
007800*                                 KUNDNUMMER FÖR SNABBRETUR               
007900        05 MID-IDPERSON-REM-IN                                            
008000                             PIC 9(3).                                    
008100*                                 PERSONKOD REMISS                        
008200        05 MID-IDKST-JUST-IN PIC X(10).                                   
008300*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
008400        05 MID-IDKONTO-JUST-IN                                            
008500                             PIC 9(10).                                   
008600*                                 KONTO FÖR JUSTERINGSORDER               
008700        05 MID-IDANALYS-JUST-IN                                           
008800                             PIC X(12).                                   
008900*                                 ANALYSNR FÖR JUSTERINGSORDER            
009000        05 MID-KDRT-JUST-IN  PIC 9(2).                                    
009100*                                 REDOVISN.TYP JUSTERINGSORDER            
009200        05 MID-IDKST-SKROT-IN                                             
009300                             PIC X(10).                                   
009400*                                 KOSTNADSST. FÖR SKROTORDER              
009500        05 MID-IDKONTO-SKROT-IN                                           
009600                             PIC 9(10).                                   
009700*                                 KONTO FÖR SKROTORDER                    
009800        05 MID-IDANALYS-SKROT-IN                                          
009900                             PIC X(12).                                   
010000*                                 ANALYSNR FÖR SKROTORDER                 
010100        05 MID-KDRT-SKROT-IN PIC 9(2).                                    
010200*                                 REDOVISNINGSTYP SKROTORDER              
010300        05 MID-IDKST-MIX-IN  PIC X(10).                                   
010400*                                 KOSTNADSST. BLANDADE ARTIKLAR           
010500        05 MID-IDKONTO-MIX-IN                                             
010600                             PIC 9(10).                                   
010700*                                 KONTO FÖR JUSTERING/BLAND.ART.          
010800        05 MID-IDANALYS-MIX-IN                                            
010900                             PIC X(12).                                   
011000*                                 ANALYSNR JUSTERING/BLAND.ART.           
011100        05 MID-KDRT-MIX-IN   PIC 9(2).                                    
011200*                                 REDOVISN. JUSTERING/BLAND. ART          
011300*** END OF VILMAII-COPY LENGTH= 224 BYTES                                 
