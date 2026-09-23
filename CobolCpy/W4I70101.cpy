000100 01  MID-W4I70101.                                                        
000200*                                 MID-COPYTEXT FÖR W4070100               
000300     03 MID-KDARBTYP-IN      PIC X(8).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-KDARBTYP-UT      PIC X(8).                                    
000600*                                 TYP AV ARBETE                           
000700     03 MID-KDANMORS-IN      PIC X(2).                                    
000800*                                 ORSAK TILL LEVERANSANMÄRKNING           
000900     03 MID-KDANMORS-UT      PIC X(2).                                    
001000*                                 ORSAK TILL LEVERANSANMÄRKNING           
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-KDARBTYP-ENTER   PIC X(8).                                    
002000*                                 TYP AV ARBETE                           
002100     03 MID-KDANMORS-ENTER   PIC X(2).                                    
002200*                                 ORSAK TILL LEVERANSANMÄRKNING           
002300     03 MID-IDRADNR-ENTER    PIC 9(3).                                    
002400*                                 RADNUMMER                               
002500     03 MID-KDARBTYP-NEXT    PIC X(8).                                    
002600*                                 TYP AV ARBETE                           
002700     03 MID-KDANMORS-NEXT    PIC X(2).                                    
002800*                                 ORSAK TILL LEVERANSANMÄRKNING           
002900     03 MID-IDRADNR-NEXT     PIC 9(3).                                    
003000*                                 RADNUMMER                               
003100     03 MID-IDDC-ENTER       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MID-IDDC-NEXT        PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MID-INPUT.                                                        
003600*                                 RADINFORMATION                          
003700        05 MID-IDRADNR-UPPD  PIC X(3).                                    
003800*                                 RADNUMMER                               
003900        05 MID-KDANMORS-UPPD PIC X(2).                                    
004000*                                 ORSAK TILL LEVERANSANMÄRKNING           
004100        05 MID-IDDC-UPPD     PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300        05 MID-KDORDKL-UPPD  PIC 9.                                       
004400*                                 ORDERKLASS                              
004500        05 MID-ADLAGOMR-UPPD PIC 9(2).                                    
004600*                                 LAGEROMRÅDE                             
004700        05 MID-IDDISTR-FOM-UPPD                                           
004800                             PIC X(4).                                    
004900*                                 DISTRIKTNUMMER                          
005000        05 MID-IDDISTR-TOM-UPPD                                           
005100                             PIC X(4).                                    
005200*                                 DISTRIKTNUMMER                          
005300        05 MID-IDKUNDNR-FOM-UPPD                                          
005400                             PIC X(6).                                    
005500*                                 KUNDNUMMER                              
005600        05 MID-IDKUNDNR-TOM-UPPD                                          
005700                             PIC X(6).                                    
005800*                                 KUNDNUMMER                              
005900        05 MID-KDARB-IDPERS-UPPD                                          
006000                             PIC X(6).                                    
006100        05 MID-FLBORT-UPPD   PIC X.                                       
006200*                                 ALLMÄN FLAGGA                           
006300*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
