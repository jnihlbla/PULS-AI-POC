000100 01  MID-W6I13201.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I13201                                
000400     03 MID-GROUP.                                                        
000500*                                 LINES                                   
000600        05 MID-ADINLOMR-PRT  PIC X(4).                                    
000700*                                 PRINTERPLACERING                        
000800        05 MID-IDINLVGN-IN   PIC X(3).                                    
000900*                                 VAGNSIDENTITET                          
001000        05 MID-IDINLVGN-UT   PIC X(3).                                    
001100*                                 VAGNSIDENTITET                          
001200        05 MID-ADINLOMR-IN   PIC X(4).                                    
001300*                                 INLEVERANSOMRÅDE                        
001400        05 MID-ADINLOMR-UT   PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600        05 MID-ADINLOMR-NXT-IN                                            
001700                             PIC X(4).                                    
001800*                                 INLEVERANSOMRÅDE NÄSTA                  
001900        05 MID-ADINLOMR-NXT-UT                                            
002000                             PIC X(4).                                    
002100*                                 INLEVERANSOMRÅDE NÄSTA                  
002200        05 MID-KDINLQ-IN     PIC X.                                       
002300*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002400        05 MID-KDINLQ-UT     PIC X.                                       
002500*                                 INLEVERANSKÖTYP KOLLI/PARTI             
002600        05 MID-BEFT-FOM-IN   PIC X(2).                                    
002700*                                 FÖRPACKNINGSTYP                         
002800        05 MID-BEFT-FOM-UT   PIC X(2).                                    
002900*                                 FÖRPACKNINGSTYP                         
003000        05 MID-BEFT-TOM-IN   PIC X(2).                                    
003100*                                 FÖRPACKNINGSTYP                         
003200        05 MID-BEFT-TOM-UT   PIC X(2).                                    
003300*                                 FÖRPACKNINGSTYP                         
003400        05 MID-FLINLFB-IN    PIC X.                                       
003500*                                 VALD TILL FÖRBEHANDLING                 
003600        05 MID-FLINLFB-UT    PIC X.                                       
003700*                                 VALD TILL FÖRBEHANDLING                 
003800        05 MID-IDLEVNR-KOLLI-IN                                           
003900                             PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER KOLLI                  
004100        05 MID-IDLEVNR-KOLLI-UT                                           
004200                             PIC X(5).                                    
004300*                                 LEVERANTÖRNUMMER KOLLI                  
004400        05 MID-IDOKOLLI-IN   PIC X(9).                                    
004500*                                 ODETTE KOLLINUMMER                      
004600        05 MID-IDOKOLLI-UT   PIC X(9).                                    
004700*                                 ODETTE KOLLINUMMER                      
004800        05 MID-IDLOPNRM-IN   PIC X(9).                                    
004900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005000*                                 (0VVDLLLLK)                             
005100        05 MID-IDLOPNRM-UT   PIC X(9).                                    
005200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005300*                                 (0VVDLLLLK)                             
005400        05 MID-IDDC-IN       PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600        05 MID-IDDC-UT       PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200     03 MID-IDFS-ENTER       PIC X(8).                                    
006300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
006400     03 MID-IDFS-NEXT        PIC X(8).                                    
006500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
006600     03 MID-TIAVIDAT-ENTER   PIC 9(6).                                    
006700*                                 AVISERINGSDATUM (YYMMDD)                
006800     03 MID-TIAVIDAT-NEXT    PIC 9(6).                                    
006900*                                 AVISERINGSDATUM (YYMMDD)                
007000     03 MID-IDRADNR-INL-ENTER                                             
007100                             PIC 9(9).                                    
007200*                                 ARTIKELNUMMER                           
007300     03 MID-IDRADNR-INL-NEXT PIC 9(9).                                    
007400*                                 ARTIKELNUMMER                           
007500     03 MID-IDRADNR-ENTER    PIC 9(5).                                    
007600*                                 RADNUMMER                               
007700     03 MID-IDRADNR-NEXT     PIC 9(5).                                    
007800*                                 RADNUMMER                               
007900     03 MID-INPUT.                                                        
008000        05 MID-KDCMDVAL-RAD  OCCURS 12 TIMES                              
008100                             PIC X(3).                                    
008200*                                 GENERELL KOMMANDOKOD                    
008300     03 MID-RAD              OCCURS 12 TIMES.                             
008400        05 MID-IDLOPNRM-RAD  PIC 9(9).                                    
008500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008600*                                 (0VVDLLLLK)                             
008700        05 MID-IDRADNR-RAD   PIC 9(3).                                    
008800*                                 RADNUMMER                               
008900     03 MID-IDINLVGN-SPAR    OCCURS 51 TIMES                              
009000                             PIC 9(3).                                    
009100*                                 VAGNSIDENTITET                          
009200     03 MID-KVINLCAR         PIC X(5).                                    
009300*                                 ANTAL VAGNAR PÅ KÖ                      
009400     03 MID-KVRADER-HIT      PIC X(5).                                    
009500*                                 ANTAL VISADE RADER HITTILS              
009600     03 MID-KVRADER-TOT      PIC X(5).                                    
009700*                                 TOTALT ANTAL RADER                      
009800     03 MID-KVRADER-PRIO     PIC X(5).                                    
009900*                                 ANTAL PRIORITERADE RADER                
010000     03 MID-KDINLPRIO-ENTER  PIC 9(2).                                    
010100*                                 PRIORITETSGRUPP                         
010200     03 MID-KDINLPRIO-NEXT   PIC 9(2).                                    
010300*                                 PRIORITETSGRUPP                         
010400*** END OF VILMAII-COPY LENGTH= 511 BYTES                                 
