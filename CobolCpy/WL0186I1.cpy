000100 01  REQU-WL0186I1.                                                       
000200*                                 REQUEST TO PGM WL0186                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 REQU-IDLBBET-KEY     PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 REQU-KDFARLIG-KEY    PIC X.                                       
001000*                                 KOD FÖR FARLIGT GODS                    
001100     03 REQU-TIRFSDAT-KEY    PIC 9(6).                                    
001200*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001300     03 REQU-KDMATT          PIC X.                                       
001400*                                 MÅTTKOD                                 
001500     03 REQU-FLSIDLAST       PIC X.                                       
001600*                                 LASTA HEL SIDA?                         
001700     03 REQU-KVRADER-MAX     PIC 9(5).                                    
001800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001900*                                 EDAN.                                   
002000     03 REQU-RADER           OCCURS 1 TO 500 TIMES                        
002100                             DEPENDING ON REQU-KVRADER-MAX.               
002200        05 REQU-KDCMD-RAD    PIC X.                                       
002300         88 REQU-KDCMD-INGENTING                                          
002400                             VALUE ' '.                                   
002500         88 REQU-KDCMD-DELETE                                             
002600                             VALUE 'D'                                    
002700                             'B'.                                         
002800         88 REQU-KDCMD-REPLACE                                            
002900                             VALUE 'R'                                    
003000                             'Ä'.                                         
003100         88 REQU-KDCMD-INSERT                                             
003200                             VALUE 'I'                                    
003300                             'N'                                          
003400                             'A'.                                         
003500         88 REQU-KDCMD-SELECT                                             
003600                             VALUE 'S'                                    
003700                             'V'.                                         
003800         88 REQU-KDCMD-PRINT VALUE 'P'                                    
003900                             'P'.                                         
004000         88 REQU-KDCMD-COPY  VALUE 'C'                                    
004100                             'K'.                                         
004200*                                 RAD-UPPDATERINGSKOMMANDO                
004300*                                  BLANK  = INGENTING                     
004400*                                  D , B  = DELETE                        
004500*                                  R , Ä  = REPLACE                       
004600*                                  I,N,A  = INSERT                        
004700*                                  S , V  = SELECT                        
004800*                                  P , P  = PRINT                         
004900*                                  C , K  = COPY                          
005000        05 REQU-IDDISTR      PIC 9(4).                                    
005100*                                 DISTRIKTNUMMER                          
005200        05 REQU-IDKUNDNR     PIC 9(6).                                    
005300*                                 KUNDNUMMER                              
005400        05 REQU-IDORDNR7     PIC 9(7).                                    
005500*                                 ORDERNUMMER                             
005600        05 REQU-IDKOLLI      PIC 9(5).                                    
005700*                                 KOLLINUMMER                             
005800        05 REQU-IDPRODNR     PIC 9(7).                                    
005900*                                 PRODUKTIONSNUMMER                       
006000        05 REQU-FLCROSS      PIC X.                                       
006100*                                 CROSS-DOCK KOLLI FLAGGA                 
006200*** END OF VILMAII-COPY LENGTH= 15531 BYTES                               
