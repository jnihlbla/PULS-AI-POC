000100 01  MID-W2I30101.                                                        
000200*                                 MID-COPYTEXT F÷R W30101                 
000300     03 MID-IDANSK-FOM-IN    PIC X(3).                                    
000400*                                 ANSKAFFARNUMMER                         
000500     03 MID-IDANSK-FOM-UT    PIC X(3).                                    
000600*                                 ANSKAFFARNUMMER                         
000700     03 MID-IDANSK-TOM-IN    PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MID-IDANSK-TOM-UT    PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MID-FLBYGGB-IN       PIC X.                                       
001200*                                 FLAGGA BYGGBAR SATSORDER                
001300     03 MID-FLBYGGB-UT       PIC X.                                       
001400*                                 FLAGGA BYGGBAR SATSORDER                
001500     03 MID-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDORDNSB-IN      PIC X(4).                                    
002000*                                 SATSORDERNUMMER-BAS                     
002100     03 MID-IDORDNSS-IN      PIC X.                                       
002200*                                 SATSORDERNUMMER-SUFFIX                  
002300     03 MID-IDORDNSB-UT      PIC X(4).                                    
002400*                                 SATSORDERNUMMER-BAS                     
002500     03 MID-IDORDNSS-UT      PIC X.                                       
002600*                                 SATSORDERNUMMER-SUFFIX                  
002700     03 MID-ING-IDARTNR-IN   PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MID-ING-IDARTNR-UT   PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MID-KDSATKMB-IN      PIC X.                                       
003200*                                 KOMBINATIONSKOD SATS                    
003300     03 MID-KDSATKMB-UT      PIC X.                                       
003400*                                 KOMBINATIONSKOD SATS                    
003500     03 MID-IDANSK-ENTER     PIC 9(3).                                    
003600*                                 ANSKAFFARNUMMER                         
003700     03 MID-IDANSK-NEXT      PIC 9(3).                                    
003800*                                 ANSKAFFARNUMMER                         
003900     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MID-IDORDNSB-ENTER   PIC 9(4).                                    
004400*                                 SATSORDERNUMMER-BAS                     
004500     03 MID-IDORDNSS-ENTER   PIC 9.                                       
004600*                                 SATSORDERNUMMER-SUFFIX                  
004700     03 MID-IDORDNSB-NEXT    PIC 9(4).                                    
004800*                                 SATSORDERNUMMER-BAS                     
004900     03 MID-IDORDNSS-NEXT    PIC 9.                                       
005000*                                 SATSORDERNUMMER-SUFFIX                  
005100     03 MID-TIREGDAT-ENTER   PIC 9(6).                                    
005200*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005300     03 MID-TIREGDAT-NEXT    PIC 9(6).                                    
005400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005500     03 MID-FLBYGGB-ENTER    PIC X.                                       
005600*                                 FLAGGA BYGGBAR SATSORDER                
005700     03 MID-FLBYGGB-NEXT     PIC X.                                       
005800*                                 FLAGGA BYGGBAR SATSORDER                
005900     03 MID-CMD-RAD          OCCURS 14 TIMES                              
006000                             PIC X.                                       
006100*                                 BEHANDLINGSKOD-X                        
006200     03 MID-IDORDNST-RAD     OCCURS 14 TIMES.                             
006300*                                 SATSORDERNUMMER-TOTALT                  
006400        05 MID-IDORDNSB      PIC 9(4).                                    
006500*                                 SATSORDERNUMMER-BAS                     
006600        05 MID-IDORDNSS      PIC 9.                                       
006700*                                 SATSORDERNUMMER-SUFFIX                  
006800*** END COPY W2I30101C0  LENGTH=194                                       
