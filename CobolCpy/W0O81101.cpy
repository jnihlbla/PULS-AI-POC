000100 01  MOD-W0O81101.                                                        
000200*                                 MOD-COPYTEXT TILL W00811                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDRAPRIO-IN      PIC Z(2)9.                                   
000800*                                 PRIORITETSKOD PÅ RADEN                  
000900     03 MOD-KDRAPRIO-UT      PIC X(3).                                    
001000*                                 PRIORITETSKOD PÅ RADEN                  
001100     03 MOD-FLAENDR-IN       PIC X.                                       
001200*                                 ÄNDRINGSFLAGGA                          
001300     03 MOD-FLAENDR-UT       PIC X.                                       
001400*                                 ÄNDRINGSFLAGGA                          
001500     03 MOD-SPARADE-NYCKLAR-ENTER.                                        
001600        05 MOD-KDTPOTYP-ENTER                                             
001700                             PIC 9.                                       
001800*                                 TYP AV TIDPLANERAD ORDER                
001900        05 MOD-KDORDKL-ENTER PIC 9.                                       
002000*                                 ORDERKLASS                              
002100        05 MOD-IDDISTR-FOM-ENTER                                          
002200                             PIC X(4).                                    
002300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002400        05 MOD-IDDISTR-TOM-ENTER                                          
002500                             PIC X(4).                                    
002600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002700     03 MOD-SPARADE-NYCKLAR-PF8.                                          
002800        05 MOD-KDTPOTYP-PF8  PIC 9.                                       
002900*                                 TYP AV TIDPLANERAD ORDER                
003000        05 MOD-KDORDKL-PF8   PIC 9.                                       
003100*                                 ORDERKLASS                              
003200        05 MOD-IDDISTR-FOM-PF8                                            
003300                             PIC X(4).                                    
003400*                                 LÄGSTA DISTRIKTNR I INTERVALL           
003500        05 MOD-IDDISTR-TOM-PF8                                            
003600                             PIC X(4).                                    
003700*                                 HÖGSTA DISTRIKTNR I INTERVALL           
003800     03 MOD-BERAPRIO-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-BERAPRIO         PIC X(10).                                   
004100*                                 PRIORITETSBENÄMNING                     
004200     03 MOD-REROFORD-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-REROFORD         PIC Z(2)9.                                   
004500*                                 RESTORDERFÖRDELNINGSFAKTOR              
004600     03 MOD-FLPRIO-ATTR      PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLPRIO           PIC X.                                       
004900*                                 PRIORITERAD                             
005000     03 MOD-KVVECKOR-TECK-ATTR                                            
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KVVECKOR-TECK    PIC Z9.                                      
005400*                                 ANTAL VECKOR FÖR HEL ROTÄCKNING         
005500     03 MOD-RELEVFOR-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-RELEVFOR         PIC 9.9(2).                                  
005800*                                 RELATIONSKOEFFICIENT RESTORDER          
005900     03 MOD-RAD              OCCURS 11 TIMES.                             
006000        05 MOD-KANTKOD-RAD-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-KANTKOD-RAD   PIC X.                                       
006400*                                 ÄNDRINGSFLAGGA                          
006500        05 MOD-KDTPOTYP-RAD-ATTR                                          
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-KDTPOTYP-RAD  PIC 9.                                       
006900*                                 TYP AV TIDPLANERAD ORDER                
007000        05 MOD-KDORDKL-RAD-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-KDORDKL-RAD   PIC 9.                                       
007400*                                 ORDERKLASS                              
007500        05 MOD-IDDISTR-FOM-RAD-ATTR                                       
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-IDDISTR-FOM-RAD                                            
007900                             PIC Z(3)9.                                   
008000*                                 LÄGSTA DISTRIKTNR I INTERVALL           
008100        05 MOD-IDDISTR-TOM-RAD-ATTR                                       
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDDISTR-TOM-RAD                                            
008500                             PIC Z(3)9.                                   
008600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
008700        05 MOD-KDTPOTYP-SPAR PIC 9.                                       
008800*                                 TYP AV TIDPLANERAD ORDER                
008900        05 MOD-KDORDKL-SPAR  PIC 9.                                       
009000*                                 ORDERKLASS                              
009100        05 MOD-IDDISTR-FOM-SPAR                                           
009200                             PIC X(4).                                    
009300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
009400        05 MOD-IDDISTR-TOM-SPAR                                           
009500                             PIC X(4).                                    
009600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
009700     03 MOD-TEMFSINF         PIC X(61).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END COPY W0O81101C0  LENGTH=504                                       
