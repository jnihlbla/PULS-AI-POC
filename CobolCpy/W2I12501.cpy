000100 01  MID-W2I12501.                                                        
000200*                                 COPYTEXT FÖR MID W2I12501               
000300*                                                                         
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MID-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MID-IDANSK-IN        PIC X(3).                                    
001300*                                 ANSKAFFARNUMMER                         
001400     03 MID-IDANSK-UT        PIC X(3).                                    
001500*                                 ANSKAFFARNUMMER                         
001600     03 MID-KDROO-IN         PIC X.                                       
001700*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
001800     03 MID-KDROO-UT         PIC X.                                       
001900*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
002000     03 MID-KDTPOTYP-FOM-IN  PIC X.                                       
002100*                                 TYP AV TIDPLANERAD ORDER                
002200     03 MID-KDTPOTYP-FOM-UT  PIC X.                                       
002300*                                 TYP AV TIDPLANERAD ORDER                
002400     03 MID-KDTPOTYP-TOM-IN  PIC X.                                       
002500*                                 TYP AV TIDPLANERAD ORDER                
002600     03 MID-KDTPOTYP-TOM-UT  PIC X.                                       
002700*                                 TYP AV TIDPLANERAD ORDER                
002800     03 MID-KDSTARAD-IN      PIC X.                                       
002900*                                 RADSTATUSKOD                            
003000     03 MID-KDSTARAD-UT      PIC X.                                       
003100*                                 RADSTATUSKOD                            
003200     03 MID-SPARADE-NYCKLAR.                                              
003300*                                       SPARADE NYCKLAR PF8               
003400        05 MID-IDARTNR-SPAR  PIC 9(9).                                    
003500*                                 ARTIKELNUMMER                           
003600        05 MID-KDRAPRIO-SPAR PIC 9(3).                                    
003700*                                 PRIORITETSKOD PÅ RADEN                  
003800        05 MID-TIRODAT-SPAR  PIC 9(6).                                    
003900*                                 RESTORDERDATUM         (ÅÅMMDD)         
004000        05 MID-TIREGTID-SPAR PIC 9(6).                                    
004100*                                 REGISTRERINGSTID                        
004200        05 MID-IDDISTR-SPAR  PIC 9(4).                                    
004300*                                 DISTRIKTNUMMER                          
004400        05 MID-IDKUNDNR-SPAR PIC 9(7).                                    
004500*                                 KUNDNUMMER                              
004600        05 MID-IDKUNDRF-SPAR PIC X(10).                                   
004700*                                 KUNDENS REFERENS (ORDERID)              
004800        05 MID-IDLOPNR-SPAR  PIC 9(3).                                    
004900*                                 LÖPNUMMER                               
005000        05 MID-IDANSK-SPAR   PIC 9(3).                                    
005100*                                 ANSKAFFARNUMMER                         
005200        05 MID-KDROO-SPAR    PIC 9.                                       
005300*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
005400        05 MID-KVRAD-SPAR    PIC 9(6).                                    
005500*                                 ANTAL ORDERRADER                        
005600     03 MID-OPP-RAD.                                                      
005700*                                  RAD FÖR FÖRÄNDRINGAR                   
005800*                                                                         
005900        05 MID-KVART-OPP     PIC 9(7).                                    
006000*                                 ANTAL ARTNR PER BRYTBEGREPP             
006100        05 MID-TITPO-OPP     PIC 9(5).                                    
006200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006300     03 MID-RAD              OCCURS 13 TIMES                              
006400                             INDEXED MID-IX-1.                            
006500*                                  RAD                                    
006600*                                                                         
006700        05 MID-VALKOD        PIC X.                                       
006800*                                 ÄNDRINGSFLAGGA                          
006900        05 MID-IDARTNR       PIC X(9).                                    
007000*                                 ARTIKELNUMMER                           
007100        05 MID-IDDISTR       PIC X(4).                                    
007200*                                 DISTRIKTNUMMER                          
007300        05 MID-IDKUNDNR      PIC X(6).                                    
007400*                                 KUNDNUMMER                              
007500        05 MID-IDORDNR7      PIC X(7).                                    
007600*                                 ORDERNUMMER                             
007700        05 MID-KDTPOTYP      PIC X.                                       
007800*                                 TYP AV TIDPLANERAD ORDER                
007900        05 MID-KVART         PIC X(7).                                    
008000*                                 ANTAL ARTNR PER BRYTBEGREPP             
008100        05 MID-TIRODAT       PIC X(6).                                    
008200*                                 RESTORDERDATUM         (ÅÅMMDD)         
008300        05 MID-TIRES         PIC X(6).                                    
008400*                                 RESERVATIONSDATUM                       
008500        05 MID-KDORDKL       PIC X.                                       
008600*                                 ORDERKLASS                              
008700        05 MID-KDROO         PIC X.                                       
008800*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
008900        05 MID-IDANSK        PIC X(3).                                    
009000*                                 ANSKAFFARNUMMER                         
009100        05 MID-TITPO         PIC X(5).                                    
009200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
009300        05 MID-KDRAPRIO      PIC X(3).                                    
009400*                                 PRIORITETSKOD PÅ RADEN                  
009500        05 MID-IDLOPNR       PIC X(2).                                    
009600*                                 LÖPNUMMER                               
009700     03 MID-SPARADE-NYCKLAR-E.                                            
009800*                                       SPARADE NYCKLAR ENTER             
009900        05 MID-IDARTNR-SPAR-E                                             
010000                             PIC 9(9).                                    
010100*                                 ARTIKELNUMMER                           
010200        05 MID-KDRAPRIO-SPAR-E                                            
010300                             PIC 9(3).                                    
010400*                                 PRIORITETSKOD PÅ RADEN                  
010500        05 MID-TIRODAT-SPAR-E                                             
010600                             PIC 9(6).                                    
010700*                                 RESTORDERDATUM         (ÅÅMMDD)         
010800        05 MID-TIREGTID-SPAR-E                                            
010900                             PIC 9(6).                                    
011000*                                 REGISTRERINGSTID                        
011100        05 MID-IDDISTR-SPAR-E                                             
011200                             PIC 9(4).                                    
011300*                                 DISTRIKTNUMMER                          
011400        05 MID-IDKUNDNR-SPAR-E                                            
011500                             PIC 9(7).                                    
011600*                                 KUNDNUMMER                              
011700        05 MID-IDKUNDRF-SPAR-E                                            
011800                             PIC X(10).                                   
011900*                                 KUNDENS REFERENS (ORDERID)              
012000        05 MID-IDLOPNR-SPAR-E                                             
012100                             PIC 9(3).                                    
012200*                                 LÖPNUMMER                               
012300        05 MID-IDANSK-SPAR-E PIC 9(3).                                    
012400*                                 ANSKAFFARNUMMER                         
012500        05 MID-KDROO-SPAR-E  PIC 9.                                       
012600*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
012700*** END OF VILMAII-COPY LENGTH= 968 BYTES                                 
