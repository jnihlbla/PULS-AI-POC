000100 01  MOD-W2O12501.                                                        
000200*                                 COPYTEXT FÖR MOD W2O12501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDANSK-IN        PIC X(3).                                    
001600*                                 ANSKAFFARNUMMER                         
001700     03 MOD-IDANSK-UT        PIC X(3).                                    
001800*                                 ANSKAFFARNUMMER                         
001900     03 MOD-KDROO-IN         PIC X.                                       
002000*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
002100     03 MOD-KDROO-UT         PIC X.                                       
002200*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
002300     03 MOD-KDTPOTYP-FOM-IN  PIC X.                                       
002400*                                 TYP AV TIDPLANERAD ORDER                
002500     03 MOD-KDTPOTYP-FOM-UT  PIC X.                                       
002600*                                 TYP AV TIDPLANERAD ORDER                
002700     03 MOD-KDTPOTYP-TOM-IN  PIC X.                                       
002800*                                 TYP AV TIDPLANERAD ORDER                
002900     03 MOD-KDTPOTYP-TOM-UT  PIC X.                                       
003000*                                 TYP AV TIDPLANERAD ORDER                
003100     03 MOD-KDSTARAD-IN      PIC X.                                       
003200*                                 RADSTATUSKOD                            
003300     03 MOD-KDSTARAD-UT      PIC X.                                       
003400*                                 RADSTATUSKOD                            
003500     03 MOD-SPARADE-NYCKLAR.                                              
003600*                                       SPARADE NYCKLAR PF8               
003700        05 MOD-IDARTNR-SPAR  PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900        05 MOD-KDRAPRIO-SPAR PIC 9(3).                                    
004000*                                 PRIORITETSKOD PÅ RADEN                  
004100        05 MOD-TIRODAT-SPAR  PIC 9(6).                                    
004200*                                 RESTORDERDATUM         (ÅÅMMDD)         
004300        05 MOD-TIREGTID-SPAR PIC 9(6).                                    
004400*                                 REGISTRERINGSTID                        
004500        05 MOD-IDDISTR-SPAR  PIC 9(4).                                    
004600*                                 DISTRIKTNUMMER                          
004700        05 MOD-IDKUNDNR-SPAR PIC 9(7).                                    
004800*                                 KUNDNUMMER                              
004900        05 MOD-IDKUNDRF-SPAR PIC X(10).                                   
005000*                                 KUNDENS REFERENS (ORDERID)              
005100        05 MOD-IDLOPNR-SPAR  PIC 9(3).                                    
005200*                                 LÖPNUMMER                               
005300        05 MOD-IDANSK-SPAR   PIC 9(3).                                    
005400*                                 ANSKAFFARNUMMER                         
005500        05 MOD-KDROO-SPAR    PIC 9.                                       
005600*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
005700        05 MOD-KVRAD-SPAR    PIC 9(6).                                    
005800*                                 ANTAL ORDERRADER                        
005900     03 MOD-OPP-RAD.                                                      
006000*                                  RAD FÖR FÖRÄNDRINGAR                   
006100*                                                                         
006200        05 MOD-KVART-OPP-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KVART-OPP     PIC Z(6)9.                                   
006600*                                 ANTAL ARTNR PER BRYTBEGREPP             
006700        05 MOD-TITPO-OPP-ATTR                                             
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-TITPO-OPP     PIC X(5).                                    
007100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007200     03 MOD-RAD              OCCURS 13 TIMES                              
007300                             INDEXED MOD-IX-1.                            
007400*                                  RAD FÖR FÖRÄNDRINGAR                   
007500*                                                                         
007600        05 MOD-VALKOD-ATTR   PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-VALKOD        PIC X.                                       
007900*                                 ÄNDRINGSFLAGGA                          
008000        05 MOD-IDARTNR       PIC Z(9).                                    
008100*                                 ARTIKELNUMMER                           
008200        05 MOD-IDDISTR       PIC Z(3)9.                                   
008300*                                 DISTRIKTNUMMER                          
008400        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
008500*                                 KUNDNUMMER                              
008600        05 MOD-IDORDNR       PIC Z(6)9.                                   
008700*                                 ORDERNUMMER                             
008800        05 MOD-KDTPOTYP      PIC 9.                                       
008900*                                 TYP AV TIDPLANERAD ORDER                
009000        05 MOD-KVART         PIC Z(6)9.                                   
009100*                                 ANTAL ARTNR PER BRYTBEGREPP             
009200        05 MOD-TIRODAT       PIC X(6).                                    
009300*                                 RESTORDERDATUM         (ÅÅMMDD)         
009400        05 MOD-TIRES         PIC X(6).                                    
009500*                                 RESERVATIONSDATUM                       
009600        05 MOD-KDORDKL       PIC Z.                                       
009700*                                 ORDERKLASS                              
009800        05 MOD-KDROO         PIC Z.                                       
009900*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
010000        05 MOD-IDANSK        PIC Z(2)9.                                   
010100*                                 ANSKAFFARNUMMER                         
010200        05 MOD-TITPO         PIC X(5).                                    
010300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
010400        05 MOD-KDRAPRIO      PIC Z(2)9.                                   
010500*                                 PRIORITETSKOD PÅ RADEN                  
010600        05 MOD-IDLOPNR       PIC Z9.                                      
010700*                                 LÖPNUMMER                               
010800     03 MOD-SPARADE-NYCKLAR-E.                                            
010900*                                   SPARADE NYCKLAR ENTER                 
011000        05 MOD-IDARTNR-SPAR-E                                             
011100                             PIC 9(9).                                    
011200*                                 ARTIKELNUMMER                           
011300        05 MOD-KDRAPRIO-SPAR-E                                            
011400                             PIC 9(3).                                    
011500*                                 PRIORITETSKOD PÅ RADEN                  
011600        05 MOD-TIRODAT-SPAR-E                                             
011700                             PIC 9(6).                                    
011800*                                 RESTORDERDATUM         (ÅÅMMDD)         
011900        05 MOD-TIREGTID-SPAR-E                                            
012000                             PIC 9(6).                                    
012100*                                 REGISTRERINGSTID                        
012200        05 MOD-IDDISTR-SPAR-E                                             
012300                             PIC 9(4).                                    
012400*                                 DISTRIKTNUMMER                          
012500        05 MOD-IDKUNDNR-SPAR-E                                            
012600                             PIC 9(7).                                    
012700*                                 KUNDNUMMER                              
012800        05 MOD-IDKUNDRF-SPAR-E                                            
012900                             PIC X(10).                                   
013000*                                 KUNDENS REFERENS (ORDERID)              
013100        05 MOD-IDLOPNR-SPAR-E                                             
013200                             PIC 9(3).                                    
013300*                                 LÖPNUMMER                               
013400        05 MOD-IDANSK-SPAR-E PIC 9(3).                                    
013500*                                 ANSKAFFARNUMMER                         
013600        05 MOD-KDROO-SPAR-E  PIC 9.                                       
013700*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
013800     03 MOD-TEMFSINF         PIC X(55).                                   
013900*                                 INFORMATIONSMEDDELANDE                  
014000*** END OF VILMAII-COPY LENGTH= 1097 BYTES                                
