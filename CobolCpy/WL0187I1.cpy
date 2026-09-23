000100 01  REQU-WL0187I1.                                                       
000200*                                 REQUEST TO PGM WL0187                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 REQU-IDLBBET-KEY     PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 REQU-KDFARLIG-KEY    PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 REQU-KDMATT          PIC X.                                       
001200*                                 MÅTTKOD                                 
001300     03 REQU-FLAVSLUTA       PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 REQU-IDDISTR-DOLD    PIC 9(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 REQU-KVRADER-MAX     PIC 9(5).                                    
001800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001900*                                 EDAN.                                   
002000     03 REQU-RADER           OCCURS 1 TO 500 TIMES                        
002100                             DEPENDING ON REQU-KVRADER-MAX.               
002200*                                 RADER                                   
002300        05 REQU-KDCMD-RAD    PIC X.                                       
002400         88 REQU-KDCMD-INGENTING                                          
002500                             VALUE ' '.                                   
002600         88 REQU-KDCMD-DELETE                                             
002700                             VALUE 'D'                                    
002800                             'B'.                                         
002900         88 REQU-KDCMD-REPLACE                                            
003000                             VALUE 'R'                                    
003100                             'Ä'.                                         
003200         88 REQU-KDCMD-INSERT                                             
003300                             VALUE 'I'                                    
003400                             'N'                                          
003500                             'A'.                                         
003600         88 REQU-KDCMD-SELECT                                             
003700                             VALUE 'S'                                    
003800                             'V'.                                         
003900         88 REQU-KDCMD-PRINT VALUE 'P'                                    
004000                             'P'.                                         
004100         88 REQU-KDCMD-COPY  VALUE 'C'                                    
004200                             'K'.                                         
004300*                                 RAD-UPPDATERINGSKOMMANDO                
004400*                                  BLANK  = INGENTING                     
004500*                                  D , B  = DELETE                        
004600*                                  R , Ä  = REPLACE                       
004700*                                  I,N,A  = INSERT                        
004800*                                  S , V  = SELECT                        
004900*                                  P , P  = PRINT                         
005000*                                  C , K  = COPY                          
005100        05 REQU-IDDISTR      PIC 9(4).                                    
005200*                                 DISTRIKTNUMMER                          
005300        05 REQU-IDKUNDNR     PIC 9(6).                                    
005400*                                 KUNDNUMMER                              
005500        05 REQU-KDFAKTYP     PIC X.                                       
005600*                                 FAKTURATYP                              
005700        05 REQU-IDORDNR7     PIC 9(7).                                    
005800*                                 ORDERNUMMER                             
005900        05 REQU-IDKOLLI      PIC 9(5).                                    
006000*                                 KOLLINUMMER                             
006100        05 REQU-IDPRODNR     PIC 9(7).                                    
006200*                                 PRODUKTIONSNUMMER                       
006300        05 REQU-FLCROSS      PIC X.                                       
006400*                                 CROSS-DOCK KOLLI FLAGGA                 
006500*** END OF VILMAII-COPY LENGTH= 16029 BYTES                               
