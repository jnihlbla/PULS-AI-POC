000100 01  W4O50701.                                                            
000200*                                 COPYTEXT F÷R MOD W4050701               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR-IN          PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 IDORDNR7-IN          PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 IDDC-IN              PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDARTNR-IN           PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 IDKOLLI-IN           PIC X(5).                                    
001900*                                 KOLLINUMMER                             
002000     03 IDPRODNR-IN          PIC X(7).                                    
002100*                                 PRODUKTIONSNUMMER                       
002200     03 IDDISTR-UT           PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 IDKUNDNR-UT          PIC X(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 IDORDNR7-UT          PIC X(7).                                    
002700*                                 ORDERNUMMER                             
002800     03 IDDC-UT              PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 IDARTNR-UT           PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 IDKOLLI-UT           PIC X(5).                                    
003300*                                 KOLLINUMMER                             
003400     03 IDPRODNR-UT          PIC X(7).                                    
003500*                                 PRODUKTIONSNUMMER                       
003600     03 TIFAKT-FIRST         PIC 9(6).                                    
003700*                                 FAKTURERINGSDATUM (≈≈MMDD)              
003800     03 TIFAKT-NEXT          PIC 9(6).                                    
003900*                                 FAKTURERINGSDATUM (≈≈MMDD)              
004000     03 AREA.                                                             
004100*                                                                         
004200        05 RAD               OCCURS 6 TIMES                               
004300                             INDEXED RAD-IX.                              
004400           07 KOL            OCCURS 7 TIMES                               
004500                             INDEXED KOL-IX.                              
004600              09 TIAAMMDD    PIC 9(6).                                    
004700*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004800              09 FILLER      PIC X(2).                                    
004900     03 TEMFSINF             PIC X(55).                                   
005000*                                 INFORMATIONSMEDDELANDE                  
005100*** END OF VILMAII-COPY LENGTH= 527 BYTES                                 
