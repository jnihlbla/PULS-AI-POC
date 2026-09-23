000100 01  W4O28401.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O28401                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDDISTR-UT           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR-IN          PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 IDKUNDNR-UT          PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 IDORDNR-IN           PIC X(7).                                    
001700*                                 ORDERNUMMER                             
001800     03 IDORDNR-UT           PIC X(7).                                    
001900*                                 ORDERNUMMER                             
002000     03 IDDC-IN              PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 IDDC-UT              PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 IDARTNR-IN           PIC X(9).                                    
002500*                                 ARTIKELNUMMER                           
002600     03 IDARTNR-UT           PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800     03 KDORDBEK-IN          PIC X(2).                                    
002900*                                 ORDERBEKRÄFTELSEKOD                     
003000     03 KDORDBEK-UT          PIC X(2).                                    
003100*                                 ORDERBEKRÄFTELSEKOD                     
003200     03 KDFRAKT-IN           PIC X(2).                                    
003300*                                 FRAKTSÄTT C1-C2 TILL KUND               
003400     03 KDFRAKT-UT           PIC X(2).                                    
003500*                                 FRAKTSÄTT C1-C2 TILL KUND               
003600     03 KDORDKL-IN           PIC X.                                       
003700*                                 ORDERKLASS                              
003800     03 KDORDKL-UT           PIC X.                                       
003900*                                 ORDERKLASS                              
004000     03 TIHIST-NEXT          PIC 9(6).                                    
004100*                                 FLYTTNINGSDATUM                         
004200     03 TIHIST-ENTER         PIC 9(6).                                    
004300*                                 FLYTTNINGSDATUM                         
004400     03 IDDC-NEXT            PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600     03 IDDC-ENTER           PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800     03 RAD                  OCCURS 6 TIMES.                              
004900        05 TIHIST            OCCURS 7 TIMES                               
005000                             PIC 9(6).                                    
005100*                                 FLYTTNINGSDATUM                         
005200     03 TEMFSINF             PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 433 BYTES                                 
