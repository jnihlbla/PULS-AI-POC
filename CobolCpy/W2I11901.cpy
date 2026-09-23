000100 01  MID-W2I11901.                                                        
000200*                                 MID-COPYTEXT FÖR W2011900               
000300     03 MID-IDLEVNR-START-IN PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-KDMAIL-START-IN  PIC X(4).                                    
000600*                                 TYP AV MAIL UTSKICK                     
000700     03 MID-IDDC-KLEV-START-IN                                            
000800                             PIC X(2).                                    
000900*                                 DC FÖR KONTAKT LEVERANTÖR               
001000     03 MID-COMMAND-LINE     OCCURS 10 TIMES.                             
001100*                                 MID COMMAND LINE FOR 2119 SCREE         
001200*                                 N                                       
001300        05 MID-KDCMD-IN      PIC X.                                       
001400         88 MID-KDCMD-INGENTING                                           
001500                             VALUE ' '.                                   
001600         88 MID-KDCMD-DELETE VALUE 'D'                                    
001700                             'B'.                                         
001800         88 MID-KDCMD-REPLACE                                             
001900                             VALUE 'R'                                    
002000                             'Ä'.                                         
002100         88 MID-KDCMD-INSERT VALUE 'I'                                    
002200                             'N'                                          
002300                             'A'.                                         
002400         88 MID-KDCMD-SELECT VALUE 'S'                                    
002500                             'V'.                                         
002600         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002700                             'P'.                                         
002800         88 MID-KDCMD-COPY   VALUE 'C'                                    
002900                             'K'.                                         
003000*                                 RAD-UPPDATERINGSKOMMANDO                
003100*                                  BLANK  = INGENTING                     
003200*                                  D , B  = DELETE                        
003300*                                  R , Ä  = REPLACE                       
003400*                                  I,N,A  = INSERT                        
003500*                                  S , V  = SELECT                        
003600*                                  P , P  = PRINT                         
003700*                                  C , K  = COPY                          
003800        05 MID-IDLEVNR-INFO  PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000        05 MID-IDATTENT-INFO PIC 9(2).                                    
004100*                                 ATTENTION NUMMER                        
004200        05 MID-KDMAIL-INFO   PIC X(4).                                    
004300*                                 TYP AV MAIL UTSKICK                     
004400        05 MID-IDDC-KLEV-INFO                                             
004500                             PIC X(2).                                    
004600*                                 DC FÖR KONTAKT LEVERANTÖR               
004700     03 MID-INPUT.                                                        
004800*                                 MID FOR 2119 SCREEN                     
004900        05 MID-IDLEVNR-IN    PIC X(5).                                    
005000*                                 LEVERANTÖRNUMMER                        
005100        05 MID-IDATTENT-IN   PIC 9(2).                                    
005200*                                 ATTENTION NUMMER                        
005300        05 MID-KDMAIL-IN     PIC X(4).                                    
005400*                                 TYP AV MAIL UTSKICK                     
005500        05 MID-IDDC-KLEV-IN  PIC X(2).                                    
005600*                                 DC FÖR KONTAKT LEVERANTÖR               
005700*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
