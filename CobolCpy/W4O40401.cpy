000100 01  MOD-W4O40401.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDC-REF-IN      PIC X(2).                                    
001200*                                 SÄNDANDE LAGER FÖR REFILL               
001300     03 MOD-IDDC-REF-UT      PIC X(2).                                    
001400*                                 SÄNDANDE LAGER FÖR REFILL               
001500     03 MOD-FLTABORT-ATTR    PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-FLTABORT         PIC X.                                       
001800*                                 BORTTAGSFLAGGA                          
001900     03 MOD-KDFRAKT-BPS      PIC Z9.                                      
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 MOD-KDFRAKT-BPS-IN-ATTR                                           
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-KDFRAKT-BPS-IN   PIC X(2).                                    
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 MOD-KDFRAKT-SBPS     PIC Z9.                                      
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800     03 MOD-KDFRAKT-SBPS-IN-ATTR                                          
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-KDFRAKT-SBPS-IN  PIC X(2).                                    
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 MOD-IDDISTR-REFILL   PIC Z(3)9.                                   
003400*                                 REFILL DISTRIKT                         
003500     03 MOD-IDDISTR-REFILL-IN-ATTR                                        
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-IDDISTR-REFILL-IN                                             
003900                             PIC Z(3)9.                                   
004000*                                 REFILL DISTRIKT                         
004100     03 MOD-IDDISTR-RETUR    PIC Z(3)9.                                   
004200*                                 RETUR DISTRIKT                          
004300     03 MOD-IDDISTR-RETUR-IN-ATTR                                         
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDDISTR-RETUR-IN PIC Z(3)9.                                   
004700*                                 RETUR DISTRIKT                          
004800     03 MOD-IDDISTR-QRETUR   PIC Z(3)9.                                   
004900*                                 KAVLITET RETUR DISTRIKT                 
005000     03 MOD-IDDISTR-QRETUR-IN-ATTR                                        
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-IDDISTR-QRETUR-IN                                             
005400                             PIC Z(3)9.                                   
005500*                                 KAVLITET RETUR DISTRIKT                 
005600     03 MOD-IDDISTR-SKROT    PIC Z(3)9.                                   
005700*                                 SKROT DISTRIKT                          
005800     03 MOD-IDDISTR-SKROT-IN-ATTR                                         
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-IDDISTR-SKROT-IN PIC Z(3)9.                                   
006200*                                 SKROT DISTRIKT                          
006300     03 MOD-IDDISTR-QSKROT   PIC Z(3)9.                                   
006400*                                 KVALITET SKROT DISTRIKT                 
006500     03 MOD-IDDISTR-QSKROT-IN-ATTR                                        
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-IDDISTR-QSKROT-IN                                             
006900                             PIC Z(3)9.                                   
007000*                                 KVALITET SKROT DISTRIKT                 
007100     03 MOD-IDDISTR-RSKROT   PIC Z(3)9.                                   
007200*                                 SKROT DISTRIKT FÖR RETURER              
007300     03 MOD-IDDISTR-RSKROT-IN-ATTR                                        
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-IDDISTR-RSKROT-IN                                             
007700                             PIC Z(3)9.                                   
007800*                                 SKROT DISTRIKT FÖR RETURER              
007900     03 MOD-IDDISTR-MIX      PIC Z(3)9.                                   
008000*                                 DISTRIKT JUSTERING/BLAND.ART.           
008100     03 MOD-IDDISTR-MIX-IN-ATTR                                           
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-IDDISTR-MIX-IN   PIC Z(3)9.                                   
008500*                                 DISTRIKT JUSTERING/BLAND.ART.           
008600     03 MOD-IDDISTR-JUST     PIC Z(3)9.                                   
008700*                                 DISTRIKT FÖR JUSTERINGSORDER            
008800     03 MOD-IDDISTR-JUST-IN-ATTR                                          
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-IDDISTR-JUST-IN  PIC Z(3)9.                                   
009200*                                 DISTRIKT FÖR JUSTERINGSORDER            
009300     03 MOD-IDKUNDNR-SORD    PIC Z(5)9.                                   
009400*                                 KUNDNUMMER FÖR SNABBORDER               
009500     03 MOD-IDKUNDNR-SORD-IN-ATTR                                         
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-IDKUNDNR-SORD-IN PIC Z(5)9.                                   
009900*                                 KUNDNUMMER FÖR SNABBORDER               
010000     03 MOD-IDKUNDNR-RETUR   PIC Z(5)9.                                   
010100*                                 KUNDNUMMER FÖR RETUR                    
010200     03 MOD-IDKUNDNR-RETUR-IN-ATTR                                        
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDKUNDNR-RETUR-IN                                             
010600                             PIC Z(5)9.                                   
010700*                                 KUNDNUMMER FÖR RETUR                    
010800     03 MOD-IDKUNDNR-QRETUR  PIC Z(5)9.                                   
010900*                                 KUNDNUMMER FÖR KVALITETSRETUR           
011000     03 MOD-IDKUNDNR-QRETUR-IN-ATTR                                       
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-IDKUNDNR-QRETUR-IN                                            
011400                             PIC Z(5)9.                                   
011500*                                 KUNDNUMMER FÖR KVALITETSRETUR           
011600     03 MOD-IDKUNDNR-SKROT   PIC Z(5)9.                                   
011700*                                 KUNDNUMMER FÖR SKROT                    
011800     03 MOD-IDKUNDNR-SKROT-IN-ATTR                                        
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-IDKUNDNR-SKROT-IN                                             
012200                             PIC Z(5)9.                                   
012300*                                 KUNDNUMMER FÖR SKROT                    
012400     03 MOD-IDKUNDNR-QSKROT  PIC Z(5)9.                                   
012500*                                 KUNDNUMMER FÖR KVALITET SKROT           
012600     03 MOD-IDKUNDNR-QSKROT-IN-ATTR                                       
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 MOD-IDKUNDNR-QSKROT-IN                                            
013000                             PIC Z(5)9.                                   
013100*                                 KUNDNUMMER FÖR KVALITET SKROT           
013200     03 MOD-IDKUNDNR-RSKROT  PIC Z(5)9.                                   
013300*                                 KUNDNUMMER FÖR SKROT AV RETUR           
013400     03 MOD-IDKUNDNR-RSKROT-IN-ATTR                                       
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 MOD-IDKUNDNR-RSKROT-IN                                            
013800                             PIC Z(5)9.                                   
013900*                                 KUNDNUMMER FÖR SKROT AV RETUR           
014000     03 MOD-IDKUNDNR-MIX     PIC Z(5)9.                                   
014100*                                 KUND JUSTERING/BLAND.ART.               
014200     03 MOD-IDKUNDNR-MIX-IN-ATTR                                          
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-IDKUNDNR-MIX-IN  PIC Z(5)9.                                   
014600*                                 KUND JUSTERING/BLAND.ART.               
014700     03 MOD-IDKUNDNR-JUST    PIC Z(5)9.                                   
014800*                                 KUND FÖR JUSTERINGSORDER                
014900     03 MOD-IDKUNDNR-JUST-IN-ATTR                                         
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200     03 MOD-IDKUNDNR-JUST-IN PIC Z(5)9.                                   
015300*                                 KUND FÖR JUSTERINGSORDER                
015400     03 MOD-IDKUNDNR-BPS     PIC Z(5)9.                                   
015500*                                 KUNDNUMMER FÖR BYPASSORDER              
015600     03 MOD-IDKUNDNR-BPS-IN-ATTR                                          
015700                             PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900     03 MOD-IDKUNDNR-BPS-IN  PIC Z(5)9.                                   
016000*                                 KUNDNUMMER FÖR BYPASSORDER              
016100     03 MOD-IDKUNDNR-TRETUR  PIC Z(5)9.                                   
016200*                                 KUNDNUMMER FÖR TOTALRETUR               
016300     03 MOD-IDKUNDNR-TRETUR-IN-ATTR                                       
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-IDKUNDNR-TRETUR-IN                                            
016700                             PIC Z(5)9.                                   
016800*                                 KUNDNUMMER FÖR TOTALRETUR               
016900     03 MOD-IDKUNDNR-SQRET   PIC Z(5)9.                                   
017000*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
017100     03 MOD-IDKUNDNR-SQRET-IN-ATTR                                        
017200                             PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-IDKUNDNR-SQRET-IN                                             
017500                             PIC Z(5)9.                                   
017600*                                 KUNDNUMMER FÖR SNABB KVAL.RETUR         
017700     03 MOD-IDKUNDNR-SBPS    PIC Z(5)9.                                   
017800*                                 KUND FÖR SNABB BYPASSORDER              
017900     03 MOD-IDKUNDNR-SBPS-IN-ATTR                                         
018000                             PIC X(2).                                    
018100*                                 MFS ATTRIBUTFÄLT                        
018200     03 MOD-IDKUNDNR-SBPS-IN PIC Z(5)9.                                   
018300*                                 KUND FÖR SNABB BYPASSORDER              
018400     03 MOD-IDKUNDNR-SRETUR  PIC Z(5)9.                                   
018500*                                 KUNDNUMMER FÖR SNABBRETUR               
018600     03 MOD-IDKUNDNR-SRETUR-IN-ATTR                                       
018700                             PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900     03 MOD-IDKUNDNR-SRETUR-IN                                            
019000                             PIC Z(5)9.                                   
019100*                                 KUNDNUMMER FÖR SNABBRETUR               
019200     03 MOD-IDPERSON-REM     PIC Z(2)9.                                   
019300*                                 PERSONKOD REMISS                        
019400     03 MOD-IDPERSON-REM-IN-ATTR                                          
019500                             PIC X(2).                                    
019600*                                 MFS ATTRIBUTFÄLT                        
019700     03 MOD-IDPERSON-REM-IN  PIC Z(2)9.                                   
019800*                                 PERSONKOD REMISS                        
019900     03 MOD-IDKST-JUST       PIC X(10).                                   
020000*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
020100     03 MOD-IDKST-JUST-IN-ATTR                                            
020200                             PIC X(2).                                    
020300*                                 MFS ATTRIBUTFÄLT                        
020400     03 MOD-IDKST-JUST-IN    PIC X(10).                                   
020500*                                 KOSTNADSST. FÖR JUSTERINGSORDER         
020600     03 MOD-IDKONTO-JUST     PIC Z(9)9.                                   
020700*                                 KONTO FÖR JUSTERINGSORDER               
020800     03 MOD-IDKONTO-JUST-IN-ATTR                                          
020900                             PIC X(2).                                    
021000*                                 MFS ATTRIBUTFÄLT                        
021100     03 MOD-IDKONTO-JUST-IN  PIC Z(9)9.                                   
021200*                                 KONTO FÖR JUSTERINGSORDER               
021300     03 MOD-IDANALYS-JUST    PIC X(12).                                   
021400*                                 ANALYSNR FÖR JUSTERINGSORDER            
021500     03 MOD-IDANALYS-JUST-IN-ATTR                                         
021600                             PIC X(2).                                    
021700*                                 MFS ATTRIBUTFÄLT                        
021800     03 MOD-IDANALYS-JUST-IN PIC X(12).                                   
021900*                                 ANALYSNR FÖR JUSTERINGSORDER            
022000     03 MOD-KDRT-JUST        PIC Z9.                                      
022100*                                 REDOVISN.TYP JUSTERINGSORDER            
022200     03 MOD-KDRT-JUST-IN-ATTR                                             
022300                             PIC X(2).                                    
022400*                                 MFS ATTRIBUTFÄLT                        
022500     03 MOD-KDRT-JUST-IN     PIC Z9.                                      
022600*                                 REDOVISN.TYP JUSTERINGSORDER            
022700     03 MOD-IDKST-SKROT      PIC X(10).                                   
022800*                                 KOSTNADSST. FÖR SKROTORDER              
022900     03 MOD-IDKST-SKROT-IN-ATTR                                           
023000                             PIC X(2).                                    
023100*                                 MFS ATTRIBUTFÄLT                        
023200     03 MOD-IDKST-SKROT-IN   PIC X(10).                                   
023300*                                 KOSTNADSST. FÖR SKROTORDER              
023400     03 MOD-IDKONTO-SKROT    PIC Z(9)9.                                   
023500*                                 KONTO FÖR SKROTORDER                    
023600     03 MOD-IDKONTO-SKROT-IN-ATTR                                         
023700                             PIC X(2).                                    
023800*                                 MFS ATTRIBUTFÄLT                        
023900     03 MOD-IDKONTO-SKROT-IN PIC Z(9)9.                                   
024000*                                 KONTO FÖR SKROTORDER                    
024100     03 MOD-IDANALYS-SKROT   PIC X(12).                                   
024200*                                 ANALYSNR FÖR SKROTORDER                 
024300     03 MOD-IDANALYS-SKROT-IN-ATTR                                        
024400                             PIC X(2).                                    
024500*                                 MFS ATTRIBUTFÄLT                        
024600     03 MOD-IDANALYS-SKROT-IN                                             
024700                             PIC X(12).                                   
024800*                                 ANALYSNR FÖR SKROTORDER                 
024900     03 MOD-KDRT-SKROT       PIC Z9.                                      
025000*                                 REDOVISNINGSTYP SKROTORDER              
025100     03 MOD-KDRT-SKROT-IN-ATTR                                            
025200                             PIC X(2).                                    
025300*                                 MFS ATTRIBUTFÄLT                        
025400     03 MOD-KDRT-SKROT-IN    PIC Z9.                                      
025500*                                 REDOVISNINGSTYP SKROTORDER              
025600     03 MOD-IDKST-MIX        PIC X(10).                                   
025700*                                 KOSTNADSST. BLANDADE ARTIKLAR           
025800     03 MOD-IDKST-MIX-IN-ATTR                                             
025900                             PIC X(2).                                    
026000*                                 MFS ATTRIBUTFÄLT                        
026100     03 MOD-IDKST-MIX-IN     PIC X(10).                                   
026200*                                 KOSTNADSST. BLANDADE ARTIKLAR           
026300     03 MOD-IDKONTO-MIX      PIC Z(9)9.                                   
026400*                                 KONTO FÖR JUSTERING/BLAND.ART.          
026500     03 MOD-IDKONTO-MIX-IN-ATTR                                           
026600                             PIC X(2).                                    
026700*                                 MFS ATTRIBUTFÄLT                        
026800     03 MOD-IDKONTO-MIX-IN   PIC Z(9)9.                                   
026900*                                 KONTO FÖR JUSTERING/BLAND.ART.          
027000     03 MOD-IDANALYS-MIX     PIC X(12).                                   
027100*                                 ANALYSNR JUSTERING/BLAND.ART.           
027200     03 MOD-IDANALYS-MIX-IN-ATTR                                          
027300                             PIC X(2).                                    
027400*                                 MFS ATTRIBUTFÄLT                        
027500     03 MOD-IDANALYS-MIX-IN  PIC X(12).                                   
027600*                                 ANALYSNR JUSTERING/BLAND.ART.           
027700     03 MOD-KDRT-MIX         PIC Z9.                                      
027800*                                 REDOVISN. JUSTERING/BLAND. ART          
027900     03 MOD-KDRT-MIX-IN-ATTR PIC X(2).                                    
028000*                                 MFS ATTRIBUTFÄLT                        
028100     03 MOD-KDRT-MIX-IN      PIC Z9.                                      
028200*                                 REDOVISN. JUSTERING/BLAND. ART          
028300     03 MOD-TEMFSINF         PIC X(55).                                   
028400*                                 INFORMATIONSMEDDELANDE                  
028500*** END OF VILMAII-COPY LENGTH= 620 BYTES                                 
