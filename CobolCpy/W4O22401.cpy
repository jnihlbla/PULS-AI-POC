000100 01  W4O22401.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O22401                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-1-IN         PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDDISTR-2-IN         PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDDISTR-3-IN         PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 IDDISTR-4-IN         PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 IDANSK-IN            PIC X(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800     03 IDARTNR-IN           PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 IDDISTR-1-UT         PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 IDDISTR-2-UT         PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 IDDISTR-3-UT         PIC X(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600     03 IDDISTR-4-UT         PIC X(4).                                    
002700*                                 DISTRIKTNUMMER                          
002800     03 IDANSK-UT            PIC X(3).                                    
002900*                                 ANSKAFFARNUMMER                         
003000     03 IDARTNR-UT           PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 IDDISTR-ENTER        PIC 9(4).                                    
003300*                                 DISTRIKTNUMMER                          
003400     03 IDANSK-ENTER         PIC 9(3).                                    
003500*                                 ANSKAFFARNUMMER                         
003600     03 IDARTNR-ENTER        PIC 9(9).                                    
003700*                                 ARTIKELNUMMER                           
003800     03 IDLOPNR-ENTER        PIC 9(3).                                    
003900*                                 LÖPNUMMER                               
004000     03 IDORDER-ENTER        PIC 9(7).                                    
004100*                                 ORDERNUMMER                             
004200     03 IDDISTR-NEXT         PIC 9(4).                                    
004300*                                 DISTRIKTNUMMER                          
004400     03 IDANSK-NEXT          PIC 9(3).                                    
004500*                                 ANSKAFFARNUMMER                         
004600     03 IDARTNR-NEXT         PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800     03 IDLOPNR-NEXT         PIC 9(3).                                    
004900*                                 LÖPNUMMER                               
005000     03 IDORDER-NEXT         PIC 9(7).                                    
005100*                                 ORDERNUMMER                             
005200     03 DISTR-INDX-ENTER     PIC 9.                                       
005300     03 DISTR-INDX-NEXT      PIC 9.                                       
005400     03 W4O22401-001-GRP     OCCURS 13 TIMES.                             
005500*                                 COPYTEXT FÖR MOD W4O22401-001-G         
005600*                                 RP                                      
005700        05 CMD-ATTR          PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 CMD               PIC X.                                       
006000*                                 BEHANDLINGSKOD-X                        
006100     03 W4O22401-004-GRP     OCCURS 13 TIMES.                             
006200*                                 COPYTEXT FÖR MOD W4O22401-004-G         
006300*                                 RP                                      
006400        05 TIREGDAT-ATTR     PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 TIREGDAT          PIC 9(6).                                    
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800     03 W4O22401-002-GRP     OCCURS 13 TIMES.                             
006900*                                 COPYTEXT FÖR MOD W4O22401-002-G         
007000*                                 RP                                      
007100        05 TEVORMRK-ATTR     PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 TEVORMRK          PIC X(2).                                    
007400*                                 MÄRKNINGSTEXT FÖR                       
007500*                                 VOR-KÖN                                 
007600     03 IDDISTR              OCCURS 13 TIMES                              
007700                             PIC Z(3)9.                                   
007800*                                 DISTRIKTNUMMER                          
007900     03 IDKUNDNR             OCCURS 13 TIMES                              
008000                             PIC Z(5)9.                                   
008100*                                 KUNDNUMMER                              
008200     03 IDORDNR7             OCCURS 13 TIMES                              
008300                             PIC Z(6)9.                                   
008400*                                 ORDERNUMMER                             
008500     03 IDANSK               OCCURS 13 TIMES                              
008600                             PIC Z(2)9.                                   
008700*                                 ANSKAFFARNUMMER                         
008800     03 IDARTNR              OCCURS 13 TIMES                              
008900                             PIC Z(8)9.                                   
009000*                                 ARTIKELNUMMER                           
009100     03 KVBEART              OCCURS 13 TIMES                              
009200                             PIC Z(5)9.                                   
009300*                                 BESTÄLLT ANTAL STYCKEN                  
009400     03 DIFF                 OCCURS 13 TIMES                              
009500                             PIC X(6).                                    
009600     03 KDORDBEK             OCCURS 13 TIMES                              
009700                             PIC 9(2).                                    
009800*                                 ORDERBEKRÄFTELSEKOD                     
009900     03 W4O22401-003-GRP     OCCURS 13 TIMES.                             
010000*                                 COPYTEXT FÖR MOD W4O22401-003-G         
010100*                                 RP                                      
010200        05 BERADREF-ATTR     PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 BERADREF          PIC X(7).                                    
010500     03 IDDC                 OCCURS 13 TIMES                              
010600                             PIC X(2).                                    
010700*                                 IDENTIFIERARE LAGER                     
010800     03 IDORDER              OCCURS 13 TIMES                              
010900                             PIC 9(7).                                    
011000*                                 ORDERNUMMER                             
011100     03 IDLOPNR              OCCURS 13 TIMES                              
011200                             PIC 9(3).                                    
011300*                                 LÖPNUMMER                               
011400     03 TEMFSINF             PIC X(55).                                   
011500*                                 INFORMATIONSMEDDELANDE                  
011600*** END OF VILMAII-COPY LENGTH= 1236 BYTES                                
