000100 01  MID-W2I47101.                                                        
000200*                                 MID-COPYTEXT FÖR W2047100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDANSK-IN        PIC X(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300     03 MID-IDANSK-UT        PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MID-IDLEVNR-IN       PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MID-IDLEVNR-UT       PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 MID-KDLARM-IN        PIC X(3).                                    
002000*                                 LARMORSAKSKOD                           
002100     03 MID-KDLARM-UT        PIC X(3).                                    
002200*                                 LARMORSAKSKOD                           
002300     03 MID-INPUT.                                                        
002400*                                 RADINFORMATION                          
002500        05 MID-KDCMD         OCCURS 14 TIMES                              
002600                             PIC X.                                       
002700         88 MID-KDCMD-INGENTING                                           
002800                             VALUE ' '.                                   
002900         88 MID-KDCMD-DELETE VALUE 'D'                                    
003000                             'B'.                                         
003100         88 MID-KDCMD-REPLACE                                             
003200                             VALUE 'R'                                    
003300                             'Ä'.                                         
003400         88 MID-KDCMD-INSERT VALUE 'I'                                    
003500                             'N'                                          
003600                             'A'.                                         
003700         88 MID-KDCMD-SELECT VALUE 'S'                                    
003800                             'V'.                                         
003900         88 MID-KDCMD-PRINT  VALUE 'P'                                    
004000                             'P'.                                         
004100         88 MID-KDCMD-COPY   VALUE 'C'                                    
004200                             'K'.                                         
004300*                                 RAD-UPPDATERINGSKOMMANDO                
004400*                                  BLANK  = INGENTING                     
004500*                                  D , B  = DELETE                        
004600*                                  R , Ä  = REPLACE                       
004700*                                  I,N,A  = INSERT                        
004800*                                  S , V  = SELECT                        
004900*                                  P , P  = PRINT                         
005000*                                  C , K  = COPY                          
005100        05 MID-FLNYLARM      OCCURS 14 TIMES                              
005200                             PIC X.                                       
005300*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
005400     03 MID-IDDC             OCCURS 14 TIMES                              
005500                             PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700     03 MID-KDLARM           OCCURS 14 TIMES                              
005800                             PIC 9(3).                                    
005900*                                 LARMORSAKSKOD                           
006000*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
