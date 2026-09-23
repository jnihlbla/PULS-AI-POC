000100 01  MOD-W90411O1.                                                        
000200*                                 MOD COPYTEXT FÖR W90411O1               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(2).                                    
000800     03 MOD-FILLER           PIC X(9).                                    
000900     03 MOD-FILLER           PIC X(25).                                   
001000     03 MOD-FILLER           PIC X(2).                                    
001100     03 MOD-FILLER           PIC 9(2).                                    
001200     03 MOD-FILLER           PIC X(2).                                    
001300     03 MOD-FILLER           PIC 9(2).                                    
001400     03 MOD-FILLER           PIC X(2).                                    
001500     03 MOD-FILLER           PIC X(2).                                    
001600     03 MOD-IDPROENH-GRP     OCCURS 3 TIMES                               
001700                             INDEXED MOD-IDPROENH-IND.                    
001800        05 MOD-FILLER        PIC X(2).                                    
001900        05 MOD-FILLER        PIC X(8).                                    
002000     03 MOD-IDBERED-IN-ATTR  PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-FILLER           PIC X(2).                                    
002300     03 MOD-KDPRODSL-IN-ATTR PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-FILLER           PIC X(2).                                    
002600     03 MOD-KDSORT-IN-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-FILLER           PIC X(2).                                    
002900     03 MOD-IDPROENH-IN-GRP  OCCURS 3 TIMES                               
003000                             INDEXED MOD-IDPROENH-IN-IND.                 
003100        05 MOD-IDPROENH-IN-ATTR                                           
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-IDPROENH-IN   PIC X(2).                                    
003500     03 MOD-FILLER           PIC X(2).                                    
003600     03 MOD-FILLER           PIC X.                                       
003700     03 MOD-FILLER           PIC X(2).                                    
003800     03 MOD-FILLER           PIC X(4).                                    
003900     03 MOD-FILLER           PIC X(2).                                    
004000     03 MOD-FILLER           PIC X.                                       
004100     03 MOD-IDKAT-GRP        OCCURS 3 TIMES                               
004200                             INDEXED MOD-IDKAT-IND.                       
004300        05 MOD-FILLER        PIC X(2).                                    
004400        05 MOD-FILLER        PIC X(5).                                    
004500     03 MOD-KDUART-IN-ATTR   PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FILLER           PIC X(2).                                    
004800     03 MOD-IDPROJ-IN-ATTR   PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FILLER           PIC X(2).                                    
005100     03 MOD-FLPISK-IN-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-FILLER           PIC X(2).                                    
005400     03 MOD-IDKAT-IN-GRP     OCCURS 3 TIMES                               
005500                             INDEXED MOD-IDKAT-IN-IND.                    
005600        05 MOD-IDKAT-IN-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDKAT-IN      PIC X(2).                                    
005900     03 MOD-FLLSRDEL-ATTR    PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-FILLER           PIC X.                                       
006200     03 MOD-FILLER           PIC X(2).                                    
006300     03 MOD-FILLER           PIC X(4).                                    
006400     03 MOD-FILLER           PIC X(2).                                    
006500     03 MOD-FILLER           PIC 9.                                       
006600     03 MOD-FILLER           PIC X(2).                                    
006700     03 MOD-FILLER           PIC Z(4)9.                                   
006800     03 MOD-FILLER           PIC X(2).                                    
006900     03 MOD-FILLER           PIC X(2).                                    
007000     03 MOD-IDPROJK-IN-ATTR  PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-FILLER           PIC X(2).                                    
007300     03 MOD-KDBPSR-IN-ATTR   PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-FILLER           PIC X(2).                                    
007600     03 MOD-TISOP-IN-ATTR    PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-FILLER           PIC X(2).                                    
007900     03 MOD-FILLER           PIC X(2).                                    
008000     03 MOD-FILLER           PIC 9(4).                                    
008100     03 MOD-FILLER           PIC X(2).                                    
008200     03 MOD-FILLER           PIC X(8).                                    
008300     03 MOD-FILLER           PIC X(2).                                    
008400     03 MOD-FILLER           PIC 9(9).                                    
008500     03 MOD-IDFKNGRP-IN-ATTR PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-FILLER           PIC X(2).                                    
008800     03 MOD-IDPROJUP-IN-ATTR PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-FILLER           PIC X(2).                                    
009100     03 MOD-IDARTNR-MOTSV-IN-ATTR                                         
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-FILLER           PIC X(2).                                    
009500     03 MOD-FILLER           PIC X(2).                                    
009600     03 MOD-FILLER           PIC X.                                       
009700     03 MOD-FILLER           PIC X(2).                                    
009800     03 MOD-FILLER           PIC X(10).                                   
009900     03 MOD-FILLER           PIC X(2).                                    
010000     03 MOD-FILLER           PIC 9(3).                                    
010100     03 MOD-FILLER           PIC X(2).                                    
010200     03 MOD-FILLER           PIC Z(6)9.                                   
010300     03 MOD-FILLER           PIC X(2).                                    
010400     03 MOD-KDAGE            PIC X.                                       
010500*                                 AGE-CODE                                
010600     03 MOD-FILLER           PIC X(2).                                    
010700     03 MOD-FILLER           PIC X(2).                                    
010800     03 MOD-IDRITN-IN-ATTR   PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000     03 MOD-FILLER           PIC X(2).                                    
011100     03 MOD-KVARTVAGN-IN-ATTR                                             
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400     03 MOD-FILLER           PIC X(2).                                    
011500     03 MOD-FILLER           PIC X(2).                                    
011600     03 MOD-FILLER           PIC X(2).                                    
011700     03 MOD-KDAGE-IN-ATTR    PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-FILLER           PIC X(2).                                    
012000     03 MOD-IDAO-GRP         OCCURS 5 TIMES                               
012100                             INDEXED MOD-IDAO-IND.                        
012200        05 MOD-FILLER        PIC X(2).                                    
012300        05 MOD-FILLER        PIC X(10).                                   
012400     03 MOD-IDAO-IN-GRP      OCCURS 5 TIMES                               
012500                             INDEXED MOD-IDAO-IN-IND.                     
012600        05 MOD-IDAO-IN-ATTR  PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-FILLER        PIC X(2).                                    
012900     03 MOD-TEORSAK-IN-UT-ATTR                                            
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-FILLER           PIC X(50).                                   
013300*                                                                         
013400     03 MOD-FILLER           PIC X(2).                                    
013500     03 MOD-FILLER           PIC X(40).                                   
013600     03 MOD-TEMFSINF         PIC X(55).                                   
013700*                                 INFORMATIONSMEDDELANDE                  
013800*** END OF VILMAII-COPY LENGTH= 558 BYTES                                 
