000100 01  MID-W5I10701.                                                        
000200*                                 MID-COPYTEXT F÷R BILD                   
000300*                                 INLEVERANSINFORMATION                   
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-DATUM-IN         PIC X(6).                                    
001300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001400     03 MID-DATUM-UT         PIC X(6).                                    
001500*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001600     03 MID-IDFS-IN          PIC X(8).                                    
001700*                                 F÷LJESEDELSNUMMER ENL ODETTE            
001800     03 MID-IDFS-UT          PIC X(8).                                    
001900*                                 F÷LJESEDELSNUMMER ENL ODETTE            
002000     03 MID-IDPTYP-IN        PIC X(3).                                    
002100*                                 POSTTYP                                 
002200     03 MID-IDPTYP-UT        PIC X(3).                                    
002300*                                 POSTTYP                                 
002400     03 MID-IDLEVNR-IN       PIC X(5).                                    
002500*                                 LEVERANT÷RNUMMER                        
002600     03 MID-IDLEVNR-UT       PIC X(5).                                    
002700*                                 LEVERANT÷RNUMMER                        
002800     03 MID-KDRT-IN          PIC X(2).                                    
002900*                                 REDOVISNINGSTYP                         
003000     03 MID-KDRT-UT          PIC X(2).                                    
003100*                                 REDOVISNINGSTYP                         
003200     03 MID-IDINLEV-NEXT     PIC 9(15).                                   
003300*                                 INLEVERANS NUMMER                       
003400     03 MID-TABELLRAD        OCCURS 14 TIMES.                             
003500*                                 GRUPP MED TABELL RADER                  
003600        05 MID-CMD           PIC X.                                       
003700*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
