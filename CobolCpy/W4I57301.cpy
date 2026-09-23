000100 01  MID-W4I57301.                                                        
000200*                                 COPYTEXT FÖR MID W4I57301               
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
005400        05 MID-IDLOPNR-SPAR  PIC 9(2).                                    
005500*                                 LÖPNUMMER                               
005600        05 MID-IDDC-SPAR     PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800        05 MID-LOPNRTAB      OCCURS 13 TIMES                              
005900                             INDEXED MID-IX-1.                            
006000*                                  LOPNR TABELL                           
006100*                                                                         
006200           07 MID-IDLOPNR    PIC X(2).                                    
006300*                                 LÖPNUMMER                               
006400     03 MID-OPP-RAD.                                                      
006500*                                  RAD FÖR FÖRÄNDRINGAR                   
006600*                                                                         
006700        05 MID-KVART-OPP     PIC X(7).                                    
006800*                                 ANTAL ARTNR PER BRYTBEGREPP             
006900        05 MID-KDORDKL-OPP   PIC X.                                       
007000*                                 ORDERKLASS                              
007100        05 MID-KDFRAKT-OPP   PIC X(2).                                    
007200*                                 FRAKTSÄTT DC TILL KUND                  
007300        05 MID-PRARTNTO-OPP  PIC X(10).                                   
007400*                                 ARTIKELPRIS NETTO                       
007500     03 MID-RAD              OCCURS 13 TIMES                              
007600                             INDEXED MID-IX-1.                            
007700*                                  RAD                                    
007800*                                                                         
007900        05 MID-VALKOD        PIC X.                                       
008000*                                 ÄNDRINGSFLAGGA                          
008100        05 MID-IDKUNDNR      PIC X(6).                                    
008200*                                 KUNDNUMMER                              
008300        05 MID-IDARTNR       PIC X(9).                                    
008400*                                 ARTIKELNUMMER                           
008500        05 MID-KVART         PIC X(7).                                    
008600*                                 ANTAL ARTNR PER BRYTBEGREPP             
008700        05 MID-BEART         PIC X(15).                                   
008800*                                 ARTIKELBENÄMNING      BEART-002         
008900        05 MID-KDORDKL       PIC X.                                       
009000*                                 ORDERKLASS                              
009100        05 MID-KDFRAKT       PIC X(2).                                    
009200*                                 FRAKTSÄTT DC TILL KUND                  
009300        05 MID-IDORDNR       PIC X(5).                                    
009400*                                 ORDERNUMMER                             
009500        05 MID-KDFAKTYP      PIC X.                                       
009600*                                 FAKTURATYP                              
009700        05 MID-KDFARLIG      PIC X.                                       
009800*                                 KOD FÖR FARLIGT GODS                    
009900        05 MID-PRARTNTO      PIC X(10).                                   
010000*                                 ARTIKELPRIS NETTO                       
010100        05 MID-TEASTRIX      PIC X.                                       
010200*                                 ASTERISK                                
010300        05 MID-KDTPOTYP      PIC X.                                       
010400*                                 TYP AV TIDPLANERAD ORDER                
010500        05 MID-IDDC          PIC X(2).                                    
010600*                                 IDENTIFIERARE LAGER                     
010700     03 MID-SPARADE-NYCKLAR-E.                                            
010800*                                       SPARADE NYCKLAR ENTER             
010900        05 MID-IDDISTR-SPAR-E                                             
011000                             PIC X(4).                                    
011100*                                 DISTRIKTNUMMER                          
011200        05 MID-IDKUNDNR-SPAR-E                                            
011300                             PIC X(6).                                    
011400*                                 KUNDNUMMER                              
011500        05 MID-IDARTNR-SPAR-E                                             
011600                             PIC X(9).                                    
011700*                                 ARTIKELNUMMER                           
011800        05 MID-KDORDKL-SPAR-E                                             
011900                             PIC X.                                       
012000*                                 ORDERKLASS                              
012100        05 MID-KDPRODSL-SPAR-E                                            
012200                             PIC X(2).                                    
012300*                                 PRODUKTSLAG                             
012400        05 MID-IDORDNR-SPAR-E                                             
012500                             PIC X(5).                                    
012600*                                 ORDERNUMMER                             
012700        05 MID-KDTPOTYP-SPAR-E                                            
012800                             PIC X.                                       
012900*                                 TYP AV TIDPLANERAD ORDER                
013000        05 MID-IDDC-SPAR-E   PIC X(2).                                    
013100*                                 IDENTIFIERARE LAGER                     
013200*** END OF VILMAII-COPY LENGTH= 980 BYTES                                 
