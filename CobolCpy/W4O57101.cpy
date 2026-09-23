000100 01  MOD-W4O57101.                                                        
000200*                                 COPYTEXT F÷R MID W4O57101               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDARTNR-IN       PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDPRODSL-IN      PIC X(2).                                    
002500*                                 PRODUKTSLAG                             
002600     03 MOD-KDPRODSL-UT      PIC X(2).                                    
002700*                                 PRODUKTSLAG                             
002800     03 MOD-IDORDNR-IN       PIC X(5).                                    
002900*                                 ORDERNUMMER                             
003000     03 MOD-IDORDNR-UT       PIC X(5).                                    
003100*                                 ORDERNUMMER                             
003200     03 MOD-KDTPOTYP-IN      PIC X.                                       
003300*                                 TYP AV TIDPLANERAD ORDER                
003400     03 MOD-KDTPOTYP-UT      PIC X.                                       
003500*                                 TYP AV TIDPLANERAD ORDER                
003600     03 MOD-IDDC-IN          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800     03 MOD-IDDC-UT          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 MOD-SPARADE-NYCKLAR.                                              
004100*                                       SPARADE NYCKLAR PF8               
004200        05 MOD-IDDISTR-SPAR  PIC 9(4).                                    
004300*                                 DISTRIKTNUMMER                          
004400        05 MOD-IDKUNDNR-SPAR PIC 9(6).                                    
004500*                                 KUNDNUMMER                              
004600        05 MOD-IDARTNR-SPAR  PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800        05 MOD-KDORDKL-SPAR  PIC 9.                                       
004900*                                 ORDERKLASS                              
005000        05 MOD-KDPRODSL-SPAR PIC 9(2).                                    
005100*                                 PRODUKTSLAG                             
005200        05 MOD-IDORDNR-SPAR  PIC 9(5).                                    
005300*                                 ORDERNUMMER                             
005400        05 MOD-KDTPOTYP-SPAR PIC 9.                                       
005500*                                 TYP AV TIDPLANERAD ORDER                
005600        05 MOD-IDDC-SPAR     PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800        05 MOD-IDLOPNR-SPAR  PIC 9(2).                                    
005900*                                 L÷PNUMMER                               
006000     03 MOD-RAD              OCCURS 14 TIMES                              
006100                             INDEXED MOD-IX-1.                            
006200*                                                                         
006300*                                                                         
006400        05 MOD-IDKUNDNR      PIC Z(6).                                    
006500*                                 KUNDNUMMER                              
006600        05 MOD-IDARTNR       PIC Z(7)9.                                   
006700*                                 ARTIKELNUMMER                           
006800        05 MOD-KVART         PIC Z(6)9.                                   
006900*                                 ANTAL ARTNR PER BRYTBEGREPP             
007000        05 MOD-IDORDNR       PIC Z(5).                                    
007100*                                 ORDERNUMMER                             
007200        05 MOD-KDORDKL       PIC Z.                                       
007300*                                 ORDERKLASS                              
007400        05 MOD-TIRODAT       PIC 9(6).                                    
007500*                                 RESTORDERDATUM         (≈≈MMDD)         
007600        05 MOD-TITPO         PIC 9(6).                                    
007700*                                 PLANERAD ORDERDATUM                     
007800        05 MOD-KDPRODSL      PIC Z9.                                      
007900*                                 PRODUKTSLAG                             
008000        05 MOD-IDANSK        PIC Z(2)9.                                   
008100*                                 ANSKAFFARNUMMER                         
008200        05 MOD-IDDC          PIC X(2).                                    
008300*                                 IDENTIFIERARE LAGER                     
008400     03 MOD-SPARADE-NYCKLAR-ENT.                                          
008500*                                      SPARADE NYCKLAR ENTER              
008600        05 MOD-IDDISTR-SPAR-E                                             
008700                             PIC 9(4).                                    
008800*                                 DISTRIKTNUMMER                          
008900        05 MOD-IDKUNDNR-SPAR-E                                            
009000                             PIC 9(6).                                    
009100*                                 KUNDNUMMER                              
009200        05 MOD-IDARTNR-SPAR-E                                             
009300                             PIC 9(9).                                    
009400*                                 ARTIKELNUMMER                           
009500        05 MOD-KDORDKL-SPAR-E                                             
009600                             PIC 9.                                       
009700*                                 ORDERKLASS                              
009800        05 MOD-KDPRODSL-SPAR-E                                            
009900                             PIC 9(2).                                    
010000*                                 PRODUKTSLAG                             
010100        05 MOD-IDORDNR-SPAR-E                                             
010200                             PIC 9(5).                                    
010300*                                 ORDERNUMMER                             
010400        05 MOD-KDTPOTYP-SPAR-E                                            
010500                             PIC 9.                                       
010600*                                 TYP AV TIDPLANERAD ORDER                
010700        05 MOD-IDDC-SPAR-E   PIC X(2).                                    
010800*                                 IDENTIFIERARE LAGER                     
010900     03 MOD-TEMFSINF         PIC X(55).                                   
011000*                                 INFORMATIONSMEDDELANDE                  
011100*** END OF VILMAII-COPY LENGTH= 865 BYTES                                 
