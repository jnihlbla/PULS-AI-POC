000100 01  MID-W4I75401.                                                        
000200*                                 MID-COPYTEXT FÖR W40754                 
000300     03 MID-TABELL-RAD       OCCURS 12 TIMES.                             
000400*                                 TABELLRADER                             
000500        05 MID-KDCMD         PIC X.                                       
000600         88 MID-KDCMD-INGENTING                                           
000700                             VALUE ' '.                                   
000800         88 MID-KDCMD-DELETE VALUE 'D'                                    
000900                             'B'.                                         
001000         88 MID-KDCMD-REPLACE                                             
001100                             VALUE 'R'                                    
001200                             'Ä'.                                         
001300         88 MID-KDCMD-INSERT VALUE 'I'                                    
001400                             'N'                                          
001500                             'A'.                                         
001600         88 MID-KDCMD-SELECT VALUE 'S'                                    
001700                             'V'.                                         
001800         88 MID-KDCMD-PRINT  VALUE 'P'                                    
001900                             'P'.                                         
002000         88 MID-KDCMD-COPY   VALUE 'C'                                    
002100                             'K'.                                         
002200*                                 RAD-UPPDATERINGSKOMMANDO                
002300*                                  BLANK  = INGENTING                     
002400*                                  D , B  = DELETE                        
002500*                                  R , Ä  = REPLACE                       
002600*                                  I,N,A  = INSERT                        
002700*                                  S , V  = SELECT                        
002800*                                  P , P  = PRINT                         
002900*                                  C , K  = COPY                          
003000        05 MID-IDDC          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MID-KDCMD-E          PIC X.                                       
003300      88 MID-KDCMD-INGENTING VALUE ' '.                                   
003400      88 MID-KDCMD-DELETE    VALUE 'D'                                    
003500                             'B'.                                         
003600      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
003700                             'Ä'.                                         
003800      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003900                             'N'                                          
004000                             'A'.                                         
004100      88 MID-KDCMD-SELECT    VALUE 'S'                                    
004200                             'V'.                                         
004300      88 MID-KDCMD-PRINT     VALUE 'P'                                    
004400                             'P'.                                         
004500      88 MID-KDCMD-COPY      VALUE 'C'                                    
004600                             'K'.                                         
004700*                                 RAD-UPPDATERINGSKOMMANDO                
004800*                                  BLANK  = INGENTING                     
004900*                                  D , B  = DELETE                        
005000*                                  R , Ä  = REPLACE                       
005100*                                  I,N,A  = INSERT                        
005200*                                  S , V  = SELECT                        
005300*                                  P , P  = PRINT                         
005400*                                  C , K  = COPY                          
005500     03 MID-IDDC-E           PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700     03 MID-KDANMORS-GRP     OCCURS 19 TIMES.                             
005800*                                 GRUPP AV KDANMORS                       
005900        05 MID-KDANMORS-E    PIC X(2).                                    
006000*                                 ORSAK TILL LEVERANSANMÄRKNING           
006100*** END OF VILMAII-COPY LENGTH= 77 BYTES                                  
