000100 01  MID-W4I45601.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-KDTRPDOCT-IN     PIC X.                                       
001200*                                 TYP AV TRANSPORTDOKUMENT                
001300     03 MID-KDTRPDOCT-UT     PIC X.                                       
001400*                                 TYP AV TRANSPORTDOKUMENT                
001500     03 MID-INPUT.                                                        
001600        05 MID-KDCMD         OCCURS 9 TIMES                               
001700                             PIC X.                                       
001800*                                 RAD-UPPDATERINGSKOMMANDO                
001900*                                  BLANK  = INGENTING                     
002000*                                  D , B  = DELETE                        
002100*                                  R , Ä  = REPLACE                       
002200*                                  I , N  = INSERT                        
002300*                                  S , V  = SELECT                        
002400*                                  P , P  = PRINT                         
002500*                                  C , K  = COPY                          
002600     03 MID-UPD.                                                          
002700*                                 UPPDATERINGSRAD                         
002800        05 MID-IDDC-UPD      PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000        05 MID-IDDISTR-UPD   PIC 9(4).                                    
003100*                                 DISTRIKTNUMMER                          
003200        05 MID-IDKUNDNR-UPD  PIC X(6).                                    
003300*                                 KUNDNUMMER                              
003400        05 MID-KVCOPIES-KLIS-UPD                                          
003500                             PIC X.                                       
003600*                                 ANTAL COPIOR VID PRINTNING              
003700        05 MID-KVCOPIES-VERS-UPD                                          
003800                             PIC X.                                       
003900*                                 ANTAL COPIOR VID PRINTNING              
004000        05 MID-KVCOPIES-STAT-UPD                                          
004100                             PIC X.                                       
004200*                                 ANTAL COPIOR VID PRINTNING              
004300        05 MID-KVCOPIES-SPED-UPD                                          
004400                             PIC X.                                       
004500*                                 ANTAL COPIOR VID PRINTNING              
004600        05 MID-KVCOPIES-PACK-UPD                                          
004700                             PIC X.                                       
004800*                                 ANTAL COPIOR VID PRINTNING              
004900        05 MID-KVCOPIES-GMTL-UPD                                          
005000                             PIC X.                                       
005100*                                 ANTAL COPIOR VID PRINTNING              
005200        05 MID-KVCOPIES-KULB-UPD                                          
005300                             PIC X.                                       
005400*                                 ANTAL COPIOR VID PRINTNING              
005500        05 MID-KVCOPIES-NAPR-UPD                                          
005600                             PIC X.                                       
005700*                                 ANTAL COPIOR VID PRINTNING              
005800        05 MID-KVCOPIES-BLAD-UPD                                          
005900                             PIC X.                                       
006000*                                 ANTAL COPIOR VID PRINTNING              
006100        05 MID-KVCOPIES-TRPT-UPD                                          
006200                             PIC X.                                       
006300        05 MID-IDDC-REC-UPD  PIC X(2).                                    
006400*                                 MOTTAGANDE LAGER                        
006500        05 MID-IDLTERM-UPD   PIC X(8).                                    
006600*                                 LOGISKT TERMINALNAMN                    
006700        05 MID-KVDAGAR-UPD   PIC 9(3).                                    
006800*                                 ANTAL DAGAR                             
006900        05 MID-FLSKRIV-ONDEM-UPD                                          
007000                             PIC X.                                       
007100*                                 J/Y = SKRIV BEGÄRD LISTA                
007200*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
