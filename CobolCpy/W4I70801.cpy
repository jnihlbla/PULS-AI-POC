000100 01  MID-W4I70801.                                                        
000200*                                 MID-COPYTEXT FÖR W40708                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDANSTNR-IN      PIC X(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 MID-RAD-INFO         OCCURS 10 TIMES.                             
000800*                                 NYCKELFÄLT PÅ RADEN                     
000900        05 MID-KDCMD         PIC X.                                       
001000         88 MID-KDCMD-INGENTING                                           
001100                             VALUE ' '.                                   
001200         88 MID-KDCMD-DELETE VALUE 'D'                                    
001300                             'B'.                                         
001400         88 MID-KDCMD-REPLACE                                             
001500                             VALUE 'R'                                    
001600                             'Ä'.                                         
001700         88 MID-KDCMD-INSERT VALUE 'I'                                    
001800                             'N'.                                         
001900         88 MID-KDCMD-SELECT VALUE 'S'                                    
002000                             'V'.                                         
002100         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002200                             'P'.                                         
002300*                                 RAD-UPPDATERINGSKOMMANDO                
002400*                                  BLANK  = INGENTING                     
002500*                                  D , B  = DELETE                        
002600*                                  R , Ä  = REPLACE                       
002700*                                  I , N  = INSERT                        
002800*                                  S , V  = SELECT                        
002900*                                  P , P  = PRINT                         
003000        05 MID-IDDISTR-FOM   PIC X(4).                                    
003100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
003200        05 MID-IDDISTR-TOM   PIC X(4).                                    
003300*                                 HÖGSTA DISTRIKTNR I INTERVALL           
003400        05 MID-SUKRENOT-FOM  PIC X(6).                                    
003500*                                 KREDITNOTASUMMA FOM                     
003600        05 MID-SUKRENOT-TOM  PIC X(6).                                    
003700*                                 KREDITNOTASUMMA TOM                     
003800     03 MID-INPUT.                                                        
003900*                                 INMATNINGSFÄLT RAD 19                   
004000        05 MID-KDCMD-RAD-19  PIC X.                                       
004100         88 MID-KDCMD-INGENTING                                           
004200                             VALUE ' '.                                   
004300         88 MID-KDCMD-DELETE VALUE 'D'                                    
004400                             'B'.                                         
004500         88 MID-KDCMD-REPLACE                                             
004600                             VALUE 'R'                                    
004700                             'Ä'.                                         
004800         88 MID-KDCMD-INSERT VALUE 'I'                                    
004900                             'N'.                                         
005000         88 MID-KDCMD-SELECT VALUE 'S'                                    
005100                             'V'.                                         
005200         88 MID-KDCMD-PRINT  VALUE 'P'                                    
005300                             'P'.                                         
005400*                                 RAD-UPPDATERINGSKOMMANDO                
005500*                                  BLANK  = INGENTING                     
005600*                                  D , B  = DELETE                        
005700*                                  R , Ä  = REPLACE                       
005800*                                  I , N  = INSERT                        
005900*                                  S , V  = SELECT                        
006000*                                  P , P  = PRINT                         
006100        05 MID-IDDISTR-FOM-UPD                                            
006200                             PIC X(4).                                    
006300*                                 LÄGSTA DISTRIKTNR I INTERVALL           
006400        05 MID-IDDISTR-TOM-UPD                                            
006500                             PIC X(4).                                    
006600*                                 HÖGSTA DISTRIKTNR I INTERVALL           
006700        05 MID-SUKRENOT-FOM-UPD                                           
006800                             PIC X(6).                                    
006900*                                 KREDITNOTASUMMA FOM                     
007000        05 MID-SUKRENOT-TOM-UPD                                           
007100                             PIC X(6).                                    
007200*                                 KREDITNOTASUMMA TOM                     
007300        05 MID-IDANSTNR-UPD  PIC X(5).                                    
007400*                                 ANSTÄLLNINGSNUMMER                      
007500        05 MID-BEANST-UPD    PIC X(25).                                   
007600*                                 ANSTÄLLDS NAMN                          
007700        05 MID-FLKREPRT-UPD  PIC X.                                       
007800*                                 UTSKRIFTSFLAGGA KREDITNOTA              
007900*** END OF VILMAII-COPY LENGTH= 271 BYTES                                 
