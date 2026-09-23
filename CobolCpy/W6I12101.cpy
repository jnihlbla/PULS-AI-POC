000100 01  MID-W6I12101.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I12101                                
000400     03 MID-GROUP.                                                        
000500*                                 LINES                                   
000600        05 MID-IDARTNR-IN    PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900        05 MID-IDARTNR-UT    PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200        05 MID-IDLOPNRM-IN   PIC X(8).                                    
001300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001400*                                 (0VVDLLLLK)                             
001500*                                 SERIAL NO RECEIVING REPORT              
001600*                                 (0WWDLLLLC)                             
001700        05 MID-IDLOPNRM-UT   PIC X(8).                                    
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200        05 MID-IDLEVNR-IN    PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500        05 MID-IDLEVNR-UT    PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800        05 MID-IDFS-IN       PIC X(8).                                    
002900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003000*                                 ADVICE NOTE NUMBER ODETTE               
003100        05 MID-IDFS-UT       PIC X(8).                                    
003200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003300*                                 ADVICE NOTE NUMBER ODETTE               
003400        05 MID-IDLBBET-IN    PIC X(12).                                   
003500*                                 LASTBÄRARBETECKNING                     
003600*                                 TRAILER NUMBER                          
003700        05 MID-IDLBBET-UT    PIC X(12).                                   
003800*                                 LASTBÄRARBETECKNING                     
003900*                                 TRAILER NUMBER                          
004000        05 MID-IDDC-IN       PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300        05 MID-IDDC-UT       PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500*                                 WAREHOUSE IDENTIFIER                    
004600        05 MID-ADINLOMR-PRT  PIC X(4).                                    
004700*                                 PRINTERPLACERING                        
004800*                                 PLACE OF A PRINTER                      
004900     03 MID-FLKLAR-TOT-IN    PIC X.                                       
005000*                                 AVSLUTNINGSMARKERING                    
005100*                                 FINISHED FLAG                           
005200     03 MID-FLKLAR-TOT-UT    PIC X.                                       
005300*                                 AVSLUTNINGSMARKERING                    
005400*                                 FINISHED FLAG                           
005500     03 MID-ENTER-IDARTNR    PIC 9(9).                                    
005600*                                 ARTIKELNUMMER                           
005700*                                 PART NUMBER                             
005800     03 MID-ENTER-IDLEVNR    PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006100     03 MID-ENTER-IDFS       PIC X(8).                                    
006200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
006300*                                 ADVICE NOTE NUMBER ODETTE               
006400     03 MID-NEXT-IDARTNR     PIC 9(9).                                    
006500*                                 ARTIKELNUMMER                           
006600*                                 PART NUMBER                             
006700     03 MID-NEXT-IDLEVNR     PIC X(5).                                    
006800*                                 LEVERANTÖRNUMMER                        
006900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007000     03 MID-NEXT-IDFS        PIC X(8).                                    
007100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007200*                                 ADVICE NOTE NUMBER ODETTE               
007300     03 MID-NEXT             PIC X.                                       
007400     03 MID-RAD              OCCURS 13 TIMES.                             
007500*                                 LINES                                   
007600        05 MID-KDCMDVAL-INPUT                                             
007700                             PIC X(3).                                    
007800*                                 GENERELL KOMMANDOKOD                    
007900*                                 GENERAL COMMAND-CODE                    
008000        05 MID-IDARTNR       PIC X(8).                                    
008100*                                 ARTIKELNUMMER                           
008200*                                 PART NUMBER                             
008300        05 MID-IDLEVNR       PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008600        05 MID-IDFS          PIC X(8).                                    
008700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
008800*                                 ADVICE NOTE NUMBER ODETTE               
008900     03 MID-FLKLAR-BIL       PIC X.                                       
009000*                                 AVSLUTNINGSMARKERING                    
009100*                                 FINISHED FLAG                           
009200     03 MID-ADINLOMR         PIC X(4).                                    
009300*                                 INLEVERANSOMRÅDE                        
009400*                                 RECEIVING AREA                          
009500     03 MID-TELOSSN1         PIC X(39).                                   
009600     03 MID-TELOSSN2         PIC X(66).                                   
009700*** END OF VILMAII-COPY LENGTH= 561 BYTES                                 
