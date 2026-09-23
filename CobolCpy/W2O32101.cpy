000100 01  MOD-W2O32101-CTX.                                                    
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-W2O32101-001-GRP.                                             
000700*                                       SPARADE NYCKLAR                   
000800        05 MOD-IDUSER-SPAR   PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000        05 MOD-TIREGDAT-SPAR PIC 9(6).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200        05 MOD-TIREGTID-SPAR PIC 9(6).                                    
001300*                                 REGISTRERINGSTID                        
001400     03 MOD-W2O32101-002-GRP.                                             
001500*                                     URVAL                               
001600        05 MOD-IDANSK-FOM-IN-ATTR                                         
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-IDANSK-FOM-IN PIC Z(2)9.                                   
002000*                                 ANSKAFFARNUMMER                         
002100        05 MOD-IDANSK-FOM-UT PIC Z9(2).                                   
002200*                                 ANSKAFFARNUMMER                         
002300        05 MOD-IDANSK-TOM-IN-ATTR                                         
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-IDANSK-TOM-IN PIC Z(2)9.                                   
002700*                                 ANSKAFFARNUMMER                         
002800        05 MOD-IDANSK-TOM-UT PIC Z9(2).                                   
002900*                                 ANSKAFFARNUMMER                         
003000        05 MOD-KDSORT1-IN-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KDSORT1-IN    PIC 9.                                       
003400*                                 SORTERINGSKOD                           
003500        05 MOD-KDSORT1-UT    PIC 9.                                       
003600*                                 SORTERINGSKOD                           
003700        05 MOD-IDLEVNR-IN-ATTR                                            
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDLEVNR-IN    PIC X(5).                                    
004100*                                 LEVERANTÖRNUMMER                        
004200        05 MOD-IDLEVNR-UT    PIC X(5).                                    
004300*                                 LEVERANTÖRNUMMER                        
004400        05 MOD-KDPRODSL-IN-ATTR                                           
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-KDPRODSL-IN   PIC Z9.                                      
004800*                                 PRODUKTSLAG                             
004900        05 MOD-KDPRODSL-UT   PIC Z9.                                      
005000*                                 PRODUKTSLAG                             
005100        05 MOD-IDDISTR-FOM-IN-ATTR                                        
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDDISTR-FOM-IN                                             
005500                             PIC Z(3)9.                                   
005600*                                 DISTRIKTNUMMER                          
005700        05 MOD-IDDISTR-FOM-UT                                             
005800                             PIC Z(3)9.                                   
005900*                                 DISTRIKTNUMMER                          
006000        05 MOD-IDDISTR-TOM-IN-ATTR                                        
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDDISTR-TOM-IN                                             
006400                             PIC Z(3)9.                                   
006500*                                 DISTRIKTNUMMER                          
006600        05 MOD-IDDISTR-TOM-UT                                             
006700                             PIC Z(3)9.                                   
006800*                                 DISTRIKTNUMMER                          
006900        05 MOD-KDBASLM-FOM-IN-ATTR                                        
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-KDBASLM-FOM-IN                                             
007300                             PIC X(6).                                    
007400*                                 BASLAGERMARKNAD                         
007500        05 MOD-KDBASLM-FOM-UT                                             
007600                             PIC X(6).                                    
007700*                                 BASLAGERMARKNAD                         
007800        05 MOD-KDBASLM-TOM-IN-ATTR                                        
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-KDBASLM-TOM-IN                                             
008200                             PIC X(6).                                    
008300*                                 BASLAGERMARKNAD                         
008400        05 MOD-KDBASLM-TOM-UT                                             
008500                             PIC X(6).                                    
008600*                                 BASLAGERMARKNAD                         
008700        05 MOD-KDTPOTYP-FOM-IN-ATTR                                       
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-KDTPOTYP-FOM-IN                                            
009100                             PIC 9.                                       
009200*                                 TYP AV TIDPLANERAD ORDER                
009300        05 MOD-KDTPOTYP-FOM-UT                                            
009400                             PIC 9.                                       
009500*                                 TYP AV TIDPLANERAD ORDER                
009600        05 MOD-KDTPOTYP-TOM-IN-ATTR                                       
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-KDTPOTYP-TOM-IN                                            
010000                             PIC 9.                                       
010100*                                 TYP AV TIDPLANERAD ORDER                
010200        05 MOD-KDTPOTYP-TOM-UT                                            
010300                             PIC 9.                                       
010400*                                 TYP AV TIDPLANERAD ORDER                
010500        05 MOD-TITPO-FOM-IN-ATTR                                          
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-TITPO-FOM-IN  PIC 9(4).                                    
010900*                                 ÅR - VECKA  (ÅÅVV)                      
011000        05 MOD-TITPO-FOM-UT  PIC 9(4).                                    
011100*                                 ÅR - VECKA  (ÅÅVV)                      
011200        05 MOD-TITPO-TOM-IN-ATTR                                          
011300                             PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500        05 MOD-TITPO-TOM-IN  PIC 9(4).                                    
011600*                                 ÅR - VECKA  (ÅÅVV)                      
011700        05 MOD-TITPO-TOM-UT  PIC 9(4).                                    
011800*                                 ÅR - VECKA  (ÅÅVV)                      
011900     03 MOD-W2O32101-003-GRP OCCURS 42 TIMES.                             
012000*                                       ARTIKLAR                          
012100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 MOD-IDARTNR       PIC Z(8)9.                                   
012400*                                 ARTIKELNUMMER                           
012500     03 MOD-TEMFSINF         PIC X(55).                                   
012600*                                 INFORMATIONSMEDDELANDE                  
012700*** END OF VILMAII-COPY LENGTH= 695 BYTES                                 
