000010*** EDIT ALLOWED                                                          
000100     EJECT                                                                
000200*                                                                         
000300*                 I M P O R T A N T                                       
000400*    IF ANY CHANGES ARE DONE IN THIS COPY-BOOK, YOU HAVE TO               
000500*    RECOMPILE PROGRAM R5111400 AND R5844000.                             
000600*    THE COPY EXIST ALSO IN F1PUV2.PROD.COBOLCPY                          
000700*    IF YOU ADD OR DELETE ANY CDFINPRO VALUE CHECK TABLES                 
000800*    FOR PROGRAM R523PCCC                                                 
000900*                                                                         
001000*                                                                         
001100*    1998-03-23 MAX LINDSKOG                                              
001110*    1999-06-08 CHANGED BY MAX LINDSKOG                                   
001200*                                                                         
001300*--------- CROSS TABEL CDFINPRO / PROFIT & COST CENTER                    
001400*                                                                         
001500*          TO GET PROFIT & COST CENTE FOR SAP/R3                          
001600*          IN TRANS FROM LEVA 1                                           
001700*          THIS INFORMATION ARE SEND IN TRANS A463500 (MR-TRANS)          
001800*                                                                         
001900*                                                                         
002000* TABLE VALUE;                                                            
002100*          POS 01-03 CDFINPRO                                             
002200*          POS 05-14 PROFIT CENTER                                        
002300*          POS 16-25 COST CENTER INCOMING FREIGHT                         
002400*          POS 27-36 COST CENTER PRICE VARIANCE                           
002500*          POS 38-47 COST CENTER EXCHANGE RATE VARIANCE                   
002600*                                                                         
002700* ONE TABLE FOR EACH COMPANY                                              
002800*   - CDFINPRO-LEVA-FTG01  VCE                                            
002900*   - CDFINPRO-LEVA-FTG03  VTC/VBC                                        
003000*   - CDFINPRO-LEVA-FTG04  PENTA                                          
003100*                                                                         
003200*                                                                         
003300*                                                                         
003400*               0        1         2         3         4                  
003500*  RULER ----> '12345678901234567890123456789012345678901234567'.         
003600*                                                                         
003700 01  CDFINPRO-LEVA-FTG01.                                                 
003800     05  FILLER PIC X(47)                                                 
003900         VALUE '01  5323       107        106        106       '.         
004000     05  FILLER PIC X(47)                                                 
004100         VALUE '02  5322       102        101        101       '.         
004200     05  FILLER PIC X(47)                                                 
004300         VALUE '03  5324       112        111        111       '.         
004400     05  FILLER PIC X(47)                                                 
004500         VALUE '04  5333       157        156        156       '.         
004600     05  FILLER PIC X(47)                                                 
004700         VALUE '05  5325       117        116        116       '.         
004800     05  FILLER PIC X(47)                                                 
004900         VALUE '06  5328       132        131        131       '.         
005000     05  FILLER PIC X(47)                                                 
005100         VALUE '07  5326       122        121        121       '.         
005200     05  FILLER PIC X(47)                                                 
005300         VALUE '08  5405       305        304        304       '.         
005400     05  FILLER PIC X(47)                                                 
005500         VALUE '09  5327       127        126        126       '.         
005600     05  FILLER PIC X(47)                                                 
005700         VALUE '10  5330       142        141        141       '.         
005800     05  FILLER PIC X(47)                                                 
005900         VALUE '11  5329       137        136        136       '.         
006000     05  FILLER PIC X(47)                                                 
006100         VALUE '12  5331       147        146        146       '.         
006200     05  FILLER PIC X(47)                                                 
006300         VALUE '13  5406       315        314        314       '.         
006400     05  FILLER PIC X(47)                                                 
006500         VALUE '14  5407       325        324        324       '.         
006600     05  FILLER PIC X(47)                                                 
006700         VALUE '15  5425       335        334        334       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '16  5756       500        499        499       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '17  5759       518        517        517       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '18  5762       536        535        535       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '19  5765       554        553        553       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '51  5600       426        425        425       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '52  5602       438        437        437       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '53  5596       402        401        401       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '54  5604       450        449        449       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '55  5598       414        413        413       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '56  5768       603        605        605       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '57  5757       506        505        505       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '58  5760       524        523        523       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '59  5763       542        541        541       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '61  5601       432        431        431       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '62  5603       444        443        443       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '63  5597       408        407        407       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '64  5605       456        455        455       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '65  5599       420        419        419       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '66  5769       611        613        613       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '67  5758       512        611        611       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '68  5761       530        529        529       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '69  5764       548        547        547       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '71  5766       560        559        559       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '72  5754       488        487        487       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '73  5772       635        637        637       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '74  5770       619        621        621       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '81  5767       566        565        565       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '82  5755       494        493        493       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '83  5773       643        645        645       '.         
006800     05  FILLER PIC X(47)                                                 
006700         VALUE '84  5771       627        629        629       '.         
006800     05  FILLER PIC X(47)                                                 
006900         VALUE '90  5332       152        151        151       '.         
007000     05  FILLER PIC X(47)                                                 
007100         VALUE '98  5332       152        151        151       '.         
007200     05  FILLER PIC X(47)                                                 
007300         VALUE '99  5334       162        161        161       '.         
007400*                                                                         
007500 01  CONV-APC-FTG01 REDEFINES CDFINPRO-LEVA-FTG01.                        
007600     05  CAF1 OCCURS 48 INDEXED BY CAF1-IX.                               
007700         10 CAF1-CDFINPRO            PIC X(3).                            
007800         10 FILLER                   PIC X.                               
007900         10 CAF1-PRCTR               PIC X(10).                           
008000         10 FILLER                   PIC X.                               
008100         10 CAF1-COSTCTR-FREIGHT     PIC X(10).                           
008200         10 FILLER                   PIC X.                               
008300         10 CAF1-COSTCTR-PRICE-DIFF  PIC X(10).                           
008400         10 FILLER                   PIC X.                               
008500         10 CAF1-COSTCTR-EXCH-DIFF   PIC X(10).                           
008600*                                                                         
008700 01  MAX-CAF1                        PIC 9(3) VALUE 48.                   
008800*                                                                         
008900 01  DEFAULT-VALUE-FTG01.                                                 
009000     05 DEF-01-PRCTR                 PIC X(10) VALUE '5335      '.        
009100     05 DEF-01-COSTCTR               PIC X(10) VALUE '179       '.        
009200*                                                                         
009300     EJECT                                                                
009400*                                                                         
009500*               0        1         2         3         4                  
009600*  RULER ----> '12345678901234567890123456789012345678901234567'.         
009700*                                                                         
009800 01  CDFINPRO-LEVA-FTG03.                                                 
009900     05  FILLER PIC X(47)                                                 
010000         VALUE '42  9100       352        351        351       '.         
010100     05  FILLER PIC X(47)                                                 
010200         VALUE '43  9100       352        351        351       '.         
010300     05  FILLER PIC X(47)                                                 
010400         VALUE '44  9100       362        361        361       '.         
010500     05  FILLER PIC X(47)                                                 
010600         VALUE '45  9100       357        356        356       '.         
010700     05  FILLER PIC X(47)                                                 
010800         VALUE '46  9100       357        356        356       '.         
010900     05  FILLER PIC X(47)                                                 
011000         VALUE '47  9100       352        351        351       '.         
011100     05  FILLER PIC X(47)                                                 
011200         VALUE '48  9100       352        351        351       '.         
011300     05  FILLER PIC X(47)                                                 
011400         VALUE '49  9100       352        351        351       '.         
011500     05  FILLER PIC X(47)                                                 
011600         VALUE '52  9100       337        336        336       '.         
011700     05  FILLER PIC X(47)                                                 
011800         VALUE '53  9100       337        336        336       '.         
011900     05  FILLER PIC X(47)                                                 
012000         VALUE '54  9100       347        346        346       '.         
012100     05  FILLER PIC X(47)                                                 
012200         VALUE '57  9100       337        336        336       '.         
012300     05  FILLER PIC X(47)                                                 
012400         VALUE '61  9100       368        367        367       '.         
012500     05  FILLER PIC X(47)                                                 
012600         VALUE '62  9100       368        367        367       '.         
012700     05  FILLER PIC X(47)                                                 
012800         VALUE '63  9100       368        367        367       '.         
012900     05  FILLER PIC X(47)                                                 
013000         VALUE '64  9100       378        377        377       '.         
013100     05  FILLER PIC X(47)                                                 
013200         VALUE '65  9100       368        367        367       '.         
013300     05  FILLER PIC X(47)                                                 
013400         VALUE '66  9100       373        372        372       '.         
013500     05  FILLER PIC X(47)                                                 
013600         VALUE '67  9100       373        372        372       '.         
013700     05  FILLER PIC X(47)                                                 
013800         VALUE '72  1127       544        543        765       '.         
013900     05  FILLER PIC X(47)                                                 
014000         VALUE '73  1127       544        543        765       '.         
014100     05  FILLER PIC X(47)                                                 
014200         VALUE '74  1129       556        555        765       '.         
014300     05  FILLER PIC X(47)                                                 
014400         VALUE '75  1128       550        549        765       '.         
014500     05  FILLER PIC X(47)                                                 
014600         VALUE '76  1128       550        549        765       '.         
014700     05  FILLER PIC X(47)                                                 
014800         VALUE '78  1127       544        543        765       '.         
014900     05  FILLER PIC X(47)                                                 
015000         VALUE '79  1127       544        543        765       '.         
015100*                                                                         
015200 01  CONV-APC-FTG03 REDEFINES CDFINPRO-LEVA-FTG03.                        
015300     05  CAF3 OCCURS 26 INDEXED BY CAF3-IX.                               
015400         10 CAF3-CDFINPRO            PIC X(3).                            
015500         10 FILLER                   PIC X.                               
015600         10 CAF3-PRCTR               PIC X(10).                           
015700         10 FILLER                   PIC X.                               
015800         10 CAF3-COSTCTR-FREIGHT     PIC X(10).                           
015900         10 FILLER                   PIC X.                               
016000         10 CAF3-COSTCTR-PRICE-DIFF  PIC X(10).                           
016100         10 FILLER                   PIC X.                               
016200         10 CAF3-COSTCTR-EXCH-DIFF   PIC X(10).                           
016300*                                                                         
016400 01  MAX-CAF3                        PIC 9(3) VALUE 26.                   
016500*                                                                         
016600 01  DEFAULT-VALUE-FTG03.                                                 
016700     05 DEF-03-PRCTR                 PIC X(10) VALUE '3105      '.        
016800     05 DEF-03-COSTCTR               PIC X(10) VALUE '366       '.        
016900*                                                                         
017000     EJECT                                                                
017100*                                                                         
017200*               0        1         2         3         4                  
017300*  RULER ----> '12345678901234567890123456789012345678901234567'.         
017400*                                                                         
017500 01  CDFINPRO-LEVA-FTG04.                                                 
017600     05  FILLER PIC X(47)                                                 
017700         VALUE '01  9100       282        281        764       '.         
017800     05  FILLER PIC X(47)                                                 
017900         VALUE '02  9100       287        286        764       '.         
018000     05  FILLER PIC X(47)                                                 
018100         VALUE '03  9100       297        296        764       '.         
018200     05  FILLER PIC X(47)                                                 
018300         VALUE '04  9100       317        316        764       '.         
018400     05  FILLER PIC X(47)                                                 
018500         VALUE '05  9100       292        291        764       '.         
018600     05  FILLER PIC X(47)                                                 
018700         VALUE '06  9100       302        301        764       '.         
018800     05  FILLER PIC X(47)                                                 
018900         VALUE '07  9100       307        306        764       '.         
019000     05  FILLER PIC X(47)                                                 
019100         VALUE '08  9100       312        311        764       '.         
019200     05  FILLER PIC X(47).                                                
019300*                                                                         
019400 01  CONV-APC-FTG04 REDEFINES CDFINPRO-LEVA-FTG04.                        
019500     05  CAF4 OCCURS 8 INDEXED BY CAF4-IX.                                
019600         10 CAF4-CDFINPRO            PIC X(3).                            
019700         10 FILLER                   PIC X.                               
019800         10 CAF4-PRCTR               PIC X(10).                           
019900         10 FILLER                   PIC X.                               
020000         10 CAF4-COSTCTR-FREIGHT     PIC X(10).                           
020100         10 FILLER                   PIC X.                               
020200         10 CAF4-COSTCTR-PRICE-DIFF  PIC X(10).                           
020300         10 FILLER                   PIC X.                               
020400         10 CAF4-COSTCTR-EXCH-DIFF   PIC X(10).                           
020500*                                                                         
020600 01  MAX-CAF4                        PIC 9(3) VALUE 8.                    
020700*                                                                         
020800 01  DEFAULT-VALUE-FTG04.                                                 
020900     05 DEF-04-PRCTR                 PIC X(10) VALUE '4145      '.        
021000     05 DEF-04-COSTCTR               PIC X(10) VALUE '321       '.        
021100*                                                                         
