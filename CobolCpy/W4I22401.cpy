000100 01  MID-W4I22401.                                                        
000200*                                 COPYTEXT FÖR MID W4I22401               
000300     03 MID-IDDISTR-1-IN     PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-2-IN     PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDDISTR-3-IN     PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-4-IN     PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDANSK-IN        PIC X(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300     03 MID-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-IDDISTR-1-UT     PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MID-IDDISTR-2-UT     PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MID-IDDISTR-3-UT     PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MID-IDDISTR-4-UT     PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MID-IDANSK-UT        PIC X(3).                                    
002400*                                 ANSKAFFARNUMMER                         
002500     03 MID-IDARTNR-UT       PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MID-IDDISTR-ENTER    PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MID-IDANSK-ENTER     PIC 9(3).                                    
003000*                                 ANSKAFFARNUMMER                         
003100     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MID-IDLOPNR-ENTER    PIC 9(3).                                    
003400*                                 LÖPNUMMER                               
003500     03 MID-IDORDER-ENTER    PIC 9(7).                                    
003600*                                 ORDERNUMMER                             
003700     03 MID-IDDISTR-NEXT     PIC 9(4).                                    
003800*                                 DISTRIKTNUMMER                          
003900     03 MID-IDANSK-NEXT      PIC 9(3).                                    
004000*                                 ANSKAFFARNUMMER                         
004100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
004400*                                 LÖPNUMMER                               
004500     03 MID-IDORDER-NEXT     PIC 9(7).                                    
004600*                                 ORDERNUMMER                             
004700     03 MID-DISTR-INDX-ENTER PIC 9.                                       
004800     03 MID-DISTR-INDX-NEXT  PIC 9.                                       
004900     03 MID-IDORDNR-VOR      PIC 9(5).                                    
005000*                                 ORDERNUMMER                             
005100     03 MID-INPUT.                                                        
005200*                                 COPYTEXT FÖR MID W4I22401               
005300        05 MID-INDATA-CMD.                                                
005400*                                 COPYTEXT FÖR MID W4I22401               
005500           07 MID-CMD        OCCURS 13 TIMES                              
005600                             PIC X.                                       
005700*                                 BEHANDLINGSKOD-X                        
005800        05 MID-INDATA-MAERKE.                                             
005900*                                 COPYTEXT FÖR MID W4I22401               
006000           07 MID-MAERKE     OCCURS 13 TIMES                              
006100                             PIC X(2).                                    
006200*                                 MÄRKNINGSTEXT FÖR                       
006300*                                 VOR-KÖN                                 
006400        05 MID-INDATA-BERADREF.                                           
006500*                                 COPYTEXT FÖR MID W4I22401               
006600           07 MID-BERADREF   OCCURS 13 TIMES                              
006700                             PIC X(7).                                    
006800     03 MID-IDDISTR          OCCURS 13 TIMES                              
006900                             PIC X(4).                                    
007000*                                 DISTRIKTNUMMER                          
007100     03 MID-IDKUNDNR         OCCURS 13 TIMES                              
007200                             PIC X(6).                                    
007300*                                 KUNDNUMMER                              
007400     03 MID-IDORDER          OCCURS 13 TIMES                              
007500                             PIC 9(7).                                    
007600*                                 VOLVO PARTS ORDERNUMMER                 
007700     03 MID-IDANSK           OCCURS 13 TIMES                              
007800                             PIC X(3).                                    
007900*                                 ANSKAFFARNUMMER                         
008000     03 MID-IDARTNR          OCCURS 13 TIMES                              
008100                             PIC X(9).                                    
008200*                                 ARTIKELNUMMER                           
008300     03 MID-IDLOPNR          OCCURS 13 TIMES                              
008400                             PIC 9(3).                                    
008500*                                 LÖPNUMMER                               
008600*** END OF VILMAII-COPY LENGTH= 661 BYTES                                 
