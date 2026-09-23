000100 01  MID-W4I57201.                                                        
000200*                                 COPYTEXT FÖR MID W4I57201               
000300*                                                                         
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 MID-IDDISTR-UT       PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MID-IDARTNR-IN       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MID-IDARTNR-UT       PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MID-KDORDKL-IN       PIC X.                                       
001700*                                 ORDERKLASS                              
001800     03 MID-KDORDKL-UT       PIC X.                                       
001900*                                 ORDERKLASS                              
002000     03 MID-KDPRODSL-IN      PIC X(2).                                    
002100*                                 PRODUKTSLAG                             
002200     03 MID-KDPRODSL-UT      PIC X(2).                                    
002300*                                 PRODUKTSLAG                             
002400     03 MID-IDORDNR-IN       PIC X(5).                                    
002500*                                 ORDERNUMMER                             
002600     03 MID-IDORDNR-UT       PIC X(5).                                    
002700*                                 ORDERNUMMER                             
002800     03 MID-KDTPOTYP-IN      PIC X.                                       
002900*                                 TYP AV TIDPLANERAD ORDER                
003000     03 MID-KDTPOTYP-UT      PIC X.                                       
003100*                                 TYP AV TIDPLANERAD ORDER                
003200     03 MID-IDDC-IN          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MID-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MID-SPARADE-NYCKLAR.                                              
003700*                                       SPARADE NYCKLAR PF8               
003800        05 MID-IDDISTR-SPAR  PIC X(4).                                    
003900*                                 DISTRIKTNUMMER                          
004000        05 MID-IDKUNDNR-SPAR PIC X(6).                                    
004100*                                 KUNDNUMMER                              
004200        05 MID-IDARTNR-SPAR  PIC X(9).                                    
004300*                                 ARTIKELNUMMER                           
004400        05 MID-KDORDKL-SPAR  PIC X.                                       
004500*                                 ORDERKLASS                              
004600        05 MID-KDPRODSL-SPAR PIC X(2).                                    
004700*                                 PRODUKTSLAG                             
004800        05 MID-IDORDNR-SPAR  PIC X(5).                                    
004900*                                 ORDERNUMMER                             
005000        05 MID-KDTPOTYP-SPAR PIC X.                                       
005100*                                 TYP AV TIDPLANERAD ORDER                
005200        05 MID-KVRAD-SPAR    PIC 9(6).                                    
005300*                                 ANTAL ORDERRADER                        
005400        05 MID-IDLOPNR-SPAR  PIC X(2).                                    
005500*                                 LÖPNUMMER                               
005600        05 MID-IDDC-SPAR     PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800        05 MID-LOPNRTAB      OCCURS 13 TIMES.                             
005900*                                  LOPNR TABELL                           
006000*                                                                         
006100           07 MID-IDLOPNR    PIC X(2).                                    
006200*                                 LÖPNUMMER                               
006300     03 MID-OPP-RAD.                                                      
006400*                                  RAD FÖR FÖRÄNDRINGAR                   
006500*                                                                         
006600        05 MID-KVART-OPP     PIC X(7).                                    
006700*                                 ANTAL ARTNR PER BRYTBEGREPP             
006800        05 MID-KDORDKL-OPP   PIC X.                                       
006900*                                 ORDERKLASS                              
007000        05 MID-KDFRAKT-OPP   PIC X(2).                                    
007100*                                 FRAKTSÄTT DC TILL KUND                  
007200        05 MID-PRARTNTO-OPP  PIC X(10).                                   
007300*                                 ARTIKELPRIS NETTO                       
007400     03 MID-RAD              OCCURS 13 TIMES.                             
007500*                                  RAD                                    
007600*                                                                         
007700        05 MID-VALKOD        PIC X.                                       
007800*                                 ÄNDRINGSFLAGGA                          
007900        05 MID-IDKUNDNR      PIC X(6).                                    
008000*                                 KUNDNUMMER                              
008100        05 MID-IDARTNR       PIC X(9).                                    
008200*                                 ARTIKELNUMMER                           
008300        05 MID-KVART         PIC X(7).                                    
008400*                                 ANTAL ARTNR PER BRYTBEGREPP             
008500        05 MID-IDORDNR       PIC X(5).                                    
008600*                                 ORDERNUMMER                             
008700        05 MID-KDORDKL       PIC X.                                       
008800*                                 ORDERKLASS                              
008900        05 MID-KDFRAKT       PIC X(3).                                    
009000*                                 FRAKTSÄTT DC TILL KUND                  
009100        05 MID-TITPO         PIC X(6).                                    
009200*                                 PLANERAD ORDERDATUM                     
009300        05 MID-PRARTNTO      PIC X(10).                                   
009400*                                 ARTIKELPRIS NETTO                       
009500        05 MID-TEASTRIX      PIC X.                                       
009600*                                 ASTERISK                                
009700        05 MID-KVQPACK-1     PIC X(5).                                    
009800*                                 ANTAL I Q1 FÖRPACKNING                  
009900        05 MID-KDRAPRIO      PIC X(3).                                    
010000*                                 PRIORITETSKOD PÅ RADEN                  
010100        05 MID-IDDC          PIC X(2).                                    
010200*                                 IDENTIFIERARE LAGER                     
010300     03 MID-SPARADE-NYCKLAR-E.                                            
010400*                                       SPARADE NYCKLAR ENTER             
010500        05 MID-IDDISTR-SPAR-E                                             
010600                             PIC X(4).                                    
010700*                                 DISTRIKTNUMMER                          
010800        05 MID-IDKUNDNR-SPAR-E                                            
010900                             PIC X(6).                                    
011000*                                 KUNDNUMMER                              
011100        05 MID-IDARTNR-SPAR-E                                             
011200                             PIC X(9).                                    
011300*                                 ARTIKELNUMMER                           
011400        05 MID-KDORDKL-SPAR-E                                             
011500                             PIC X.                                       
011600*                                 ORDERKLASS                              
011700        05 MID-KDPRODSL-SPAR-E                                            
011800                             PIC X(2).                                    
011900*                                 PRODUKTSLAG                             
012000        05 MID-IDORDNR-SPAR-E                                             
012100                             PIC X(5).                                    
012200*                                 ORDERNUMMER                             
012300        05 MID-KDTPOTYP-SPAR-E                                            
012400                             PIC X.                                       
012500*                                 TYP AV TIDPLANERAD ORDER                
012600        05 MID-IDDC-SPAR-E   PIC X(2).                                    
012700*                                 IDENTIFIERARE LAGER                     
012800*** END OF VILMAII-COPY LENGTH= 941 BYTES                                 
