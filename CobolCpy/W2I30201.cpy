000100 01  MID-W2I30201.                                                        
000200*                                 MID-COPYTEXT FÖR W30201                 
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
003500     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
003800*                                 ARTIKELNUMMER                           
003900     03 MID-INPUT.                                                        
004000*                                 INDATA FÖR UPPDATERING                  
004100        05 MID-KVDELA-UPDATE PIC X(6).                                    
004200*                                 ANTAL BYGGBARA SATSER                   
004300        05 MID-FLANNULL-REST-UPDATE                                       
004400                             PIC X.                                       
004500*                                 ANNULLATION                             
004600        05 MID-FLANNULL-HELA-UPDATE                                       
004700                             PIC X.                                       
004800*                                 ANNULLATION                             
004900        05 MID-FLSATPRI-UPDATE                                            
005000                             PIC X.                                       
005100*                                 MANUELL PRIORITERING AV SATS            
005200*** END COPY W2I30201C0  LENGTH=89                                        
