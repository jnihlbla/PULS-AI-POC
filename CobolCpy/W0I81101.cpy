000100 01  MID-W0I81101.                                                        
000200*                                 MID-COPYTEXT FÖR W00811                 
000300     03 MID-KDRAPRIO-IN      PIC X(3).                                    
000400*                                 PRIORITETSKOD PÅ RADEN                  
000500     03 MID-KDRAPRIO-UT      PIC X(3).                                    
000600*                                 PRIORITETSKOD PÅ RADEN                  
000700     03 MID-FLAENDR-IN       PIC X.                                       
000800*                                 ÄNDRINGSFLAGGA                          
000900     03 MID-FLAENDR-UT       PIC X.                                       
001000*                                 ÄNDRINGSFLAGGA                          
001100     03 MID-SPARADE-NYCKLAR-ENTER.                                        
001200        05 MID-KDTPOTYP-ENTER                                             
001300                             PIC 9.                                       
001400*                                 TYP AV TIDPLANERAD ORDER                
001500        05 MID-KDORDKL-ENTER PIC 9.                                       
001600*                                 ORDERKLASS                              
001700        05 MID-IDDISTR-FOM-ENTER                                          
001800                             PIC X(4).                                    
001900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002000        05 MID-IDDISTR-TOM-ENTER                                          
002100                             PIC X(4).                                    
002200*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002300     03 MID-SPARADE-NYCKLAR-PF8.                                          
002400        05 MID-KDTPOTYP-PF8  PIC 9.                                       
002500*                                 TYP AV TIDPLANERAD ORDER                
002600        05 MID-KDORDKL-PF8   PIC 9.                                       
002700*                                 ORDERKLASS                              
002800        05 MID-IDDISTR-FOM-PF8                                            
002900                             PIC X(4).                                    
003000*                                 LÄGSTA DISTRIKTNR I INTERVALL           
003100        05 MID-IDDISTR-TOM-PF8                                            
003200                             PIC X(4).                                    
003300*                                 HÖGSTA DISTRIKTNR I INTERVALL           
003400     03 MID-BERAPRIO         PIC X(10).                                   
003500*                                 PRIORITETSBENÄMNING                     
003600     03 MID-REROFORD         PIC X(3).                                    
003700*                                 RESTORDERFÖRDELNINGSFAKTOR              
003800     03 MID-FLPRIO           PIC X.                                       
003900*                                 PRIORITERAD                             
004000     03 MID-KVVECKOR-TECK    PIC X(2).                                    
004100*                                 ANTAL VECKOR FÖR HEL ROTÄCKNING         
004200     03 MID-RELEVFOR         PIC X(4).                                    
004300*                                 RELATIONSKOEFFICIENT RESTORDER          
004400     03 MID-RAD              OCCURS 11 TIMES.                             
004500        05 MID-KANTKOD-RAD   PIC X.                                       
004600*                                 ÄNDRINGSFLAGGA                          
004700        05 MID-KDTPOTYP-RAD  PIC X.                                       
004800*                                 TYP AV TIDPLANERAD ORDER                
004900        05 MID-KDORDKL-RAD   PIC X.                                       
005000*                                 ORDERKLASS                              
005100        05 MID-IDDISTR-FOM-RAD                                            
005200                             PIC X(4).                                    
005300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
005400        05 MID-IDDISTR-TOM-RAD                                            
005500                             PIC X(4).                                    
005600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
005700        05 MID-KDTPOTYP-SPAR PIC 9.                                       
005800*                                 TYP AV TIDPLANERAD ORDER                
005900        05 MID-KDORDKL-SPAR  PIC 9.                                       
006000*                                 ORDERKLASS                              
006100        05 MID-IDDISTR-FOM-SPAR                                           
006200                             PIC X(4).                                    
006300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
006400        05 MID-IDDISTR-TOM-SPAR                                           
006500                             PIC X(4).                                    
006600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
006700*** END COPY W0I81101C0  LENGTH=279                                       
