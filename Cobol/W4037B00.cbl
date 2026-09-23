000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4037B00.                                                
000003 AUTHOR.         BERT ANDERSSON                                           
000004 DATE-WRITTEN.   FEB  2009.                                               
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION.                                                            
000008*        REDIGERING OCH UTSKRIFT AV PLOCKETIKETTER                        
000009*        UTSKRIFT MED HJÄLP AV D&P                                        
000010*                                                                         
000011*        DETTA SUBPROGRAM REDIGERAR OCH SKRIVER                           
000012*        PLOCKETIKETTER                                                   
000013*        LÄSER AKTUELL PLOCKSATS (WL400311).                              
000014*        BEHANDLAR ALLA PLE-RADER SOM INGÅR I PLOCKSATSEN.                
000015*        PROGRAMMET STARTAS OM EFTER ETT ANTAL BEHANDLADE SIDOR.          
000016*        NÄR ALLA PLE-RADER HAR BEHANDLATS TAGES PLOCKSATS BORT.          
000017*        VARJE RAD I UTSKRIFTEN BESTÅR AV TVÅ ETIKETTER.                  
000018*                                                                         
000019*                                                                         
000020*    INDATA.                                                              
000021*        TRANSAKTION: W4T37BU                                             
000022*        MID:         W4I37A01                                            
000023*                                                                         
000024*    UTDATA.                                                              
000025*        TRANSAKTION: TILL ZETES 3IV VIA VCOM                             
000026*                                                                         
000027*    CHANGE LOG                                                           
000028*                                                                         
000029*                                                                         
000030 DATA DIVISION.                                                           
000031                                                                          
000032 WORKING-STORAGE SECTION.                                                 
000033                                                                          
000034 77  IDPGM                       PIC X(8)    VALUE 'W4037B00'.            
000035 77  FILLER                      PIC X(8)    VALUE 'ERRORTEX'.            
000036 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
000037                                                                          
000038 77  KDRC-DISPLAY                PIC Z(5).                                
000039                                                                          
000040 77  FILLER                      PIC X(08)   VALUE 'AKTUELL:'.            
000041 77  WS-CURRENT-SECTION          PIC X(48)   VALUE SPACE.                 
000042                                                                          
000043 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC'.             
000044 77  WS-CURRENT-IMS-SECTION      PIC X(48)   VALUE SPACE.                 
000045                                                                          
000046 77  JA                          PIC X       VALUE 'J'.                   
000047 77  YES                         PIC X       VALUE 'Y'.                   
000048 77  NEJ                         PIC X       VALUE 'N'.                   
000049 77  RECORD-TYPE-PICK-TASK       PIC X(16)   VALUE 'PickTask'.            
000050 77  RECORD-TYPE-CUST-SUM       PIC X(24) VALUE 'CustomerSummary'.        
000051                                                                          
000052 77  VBAR                        PIC X(01) VALUE X'BB'.                   
000053 77  WS-KDFDKRAV                 PIC 9(03) VALUE ZERO.                    
000054                                                                          
000055 77  WS-4006-IDPRC-FIRST         PIC X(04)   VALUE SPACE.                 
000056 77  WS-BEFDKRAV                 PIC X(40)   VALUE SPACE.                 
000057 77  WS-FLPMT3IV                 PIC X(01)   VALUE SPACE.                 
000058                                                                          
000059 77  TECKEN-IX                   PIC S9(9)   VALUE +0.                    
000060 77  FILLER                      PIC X(08)   VALUE 'IXIXIXIX'.            
000061 77  MAX-ANTAL-ORDERDELAR        PIC S9(3)   VALUE 99    COMP-3.          
000062 77  ODEL-ANV-IX                 PIC S9(9)   VALUE +0.                    
000063 01  ODEL-IX-TABELL.                                                      
000064     03 ODEL-NYTT-IX OCCURS 99   PIC S9(3)   COMP-3.                      
000065                                                                          
000066 77  WS-IDLOPNR-ORD              PIC  9(3).                               
000067                                                                          
000068 77  WS-OHUV-IDKUNDNR            PIC 9(7)    VALUE ZERO.                  
000069 77  WS-OHUV-KDORDKL             PIC 9(2)    VALUE ZERO.                  
000070 77  SPAR-IDORDER                PIC S9(9)   COMP-3.                      
000071 77  WS-DATUM-TID                PIC 9(11).                               
000072 77  WS-DATUM                    PIC 9(6).                                
000073 77  WS-TID                      PIC 9(6).                                
000074 77  WS-4010-IDPLKLST            PIC 9(3).                                
000075 77  WS-TOTAL-KVRADER            PIC 9(7)    VALUE ZERO.                  
000076 77  WS-TOTAL-VKORDNTO           PIC S9(6)V9(1) COMP-3.                   
000077 77  WS-TOTAL-VLORDNTO           PIC S9(4)V9(3) COMP-3.                   
000078 77  WS-KDARTURS-NUM             PIC 9(2).                                
000079                                                                          
000080 77  IX1                         PIC S9(9)   VALUE +0.                    
000081 77  IX2                         PIC S9(9)   VALUE +0.                    
000082                                                                          
000083 01     WS-IDUSER.                                                        
000084   03   FILLER                    PIC X(3).                               
000085   03   WS-IDANSTNR-5             PIC X(5).                               
000086                                                                          
000087 01      WS-IDARTNR              PIC 9(9).                                
000088 01  FILLER REDEFINES WS-IDARTNR.                                         
000089   03    WS-IDARTNR-FIRST-6      PIC 9(6).                                
000090   03    WS-IDARTNR-LAST-3       PIC 9(3).                                
000091                                                                          
000092 01      WS-KLOCKAN.                                                      
000093   03    WS-TIHHMMSS             PIC 9(6).                                
000094   03    FILLER                  PIC X(2).                                
000095                                                                          
000096 01      WS-ADPLATS              PIC 9(5).                                
000097                                                                          
000098 01      WS-ADLOC3IV.                                                     
000099   03    WS-ADLAGOMR             PIC 9(2).                                
000100   03    WS-ADGANG               PIC 9(2).                                
000101   03    WS-ADLOC3IV-SPACE       PIC X(1).                                
000102   03    WS-ADPLATS-GRP.                                                  
000103     05  WS-ADPLATS3               PIC 9(3).                              
000104     05  WS-ADPLATS4               PIC 9(1).                              
000105     05  WS-ADPLATS5               PIC 9(1).                              
000106*                                                                         
000107 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
000108 01  FILLER REDEFINES TEST-IDDISTR.                                       
000109*    03   -COPY WWDIST03.                                                 
000110 01  FILLER REDEFINES TEST-IDDISTR.                                       
000111*    03   -COPY WWDIST08.                                                 
000112                                                                          
000113***********************                                                   
000114*  ARBETSAREA FÖR PRC *                                                   
000115***********************                                                   
000116 01  W-SPAR-IDPRC.                                                        
000117*    03  -COPY WDGX4448   -PRE W-SPAR-                                    
000118                                                                          
000119****************************                                              
000120*  SPAR-AREA PLOCKSATS PU  *                                              
000121****************************                                              
000122 01  W-PU-PLOCKSATS.                                                      
000123*    03  -COPY WDGX4008   -PRE W-PU-                                      
000124                                                                          
000125 77  ALLT-SW                     PIC X.                                   
000126     88  ALLT-OK                             VALUE 'J'.                   
000127     88  ALLT-FEL                            VALUE 'N'.                   
000128                                                                          
000129 77  NO-LINES-SW                 PIC X.                                   
000130     88  NO-LINES                            VALUE 'J'.                   
000131                                                                          
000132 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000133     88  GODK-MID                            VALUE '4375' '437B'.         
000134                                                                          
000135 01  MESSAGE-CODES.                                                       
000136     03  ERR-ITEMS-MISSING       PIC X(3)    VALUE '029'.                 
000137                                                                          
000138*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
000139 01  GENERAL-SUBPROGRAMS.                                                 
000140     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000141     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000142     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000143     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
000144     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
000145     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000146                                                                          
000147 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000148                                                                          
000149*    --- PARAMETERS TO WMEDKONV                                           
000150*01  -COPY WMEDAREA                                                       
000151                                                                          
000152*    -- WORK FIELD FOR EDITING WMEDKONV MESSAGE                           
000153 01  TEMP-MESSAGE                PIC X(50).                               
000154     EJECT                                                                
000155*    --- WORK-AREAS FOR MESSAGE PROCESSING                                
000156*                                                                         
000157*    --- PARAMETERS TO WZ01SEND                                           
000158 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
000159     SKIP3                                                                
000160*01  -COPY WZ01SEND                                                       
000161 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
000162     SKIP3                                                                
000163 01  SEND-AREA                   PIC X(1000).                             
000164     EJECT                                                                
000165 01  FILLER                      PIC X(16)   VALUE 'RESPONSE HDR'.        
000166     SKIP3                                                                
000167*01  -COPY W403RESP                                                       
000168     SKIP3                                                                
000169*01  -COPY W403CUST                                                       
000170     SKIP3                                                                
000171*01  -COPY W403PICT                                                       
000172     SKIP3                                                                
000173*                                                                         
000174 01  FILLER                    PIC X(16) VALUE 'KDARTURSÖVERSÄTT'.        
000175*01  -COPY W400ARTU                                                       
000176     SKIP3                                                                
000177 01  FILLER                    PIC X(16) VALUE 'FIELD-LENGTHS'.           
000178 01  FIELD-LENGTHS.                                                       
000179     03  LIDMTYP3IV              PIC S9(4) BINARY.                        
000180     03  LIDPRC                  PIC S9(4) BINARY.                        
000181     03  LIDMSG3IV               PIC S9(4) BINARY.                        
000182     03  LIMVER3IV               PIC S9(4) BINARY.                        
000183     03  LTISTAMP3IV             PIC S9(4) BINARY.                        
000184     03  LIDRTYP3IV              PIC S9(4) BINARY.                        
000185     03  LIDSNO3IV               PIC S9(4) BINARY.                        
000186     03  LIDUSER                 PIC S9(4) BINARY.                        
000187     03  LIDPRODNR               PIC S9(4) BINARY.                        
000188     03  LIDPLKLST               PIC S9(4) BINARY.                        
000189     03  LIDDISTR                PIC S9(4) BINARY.                        
000190     03  LIDKUNDNR               PIC S9(4) BINARY.                        
000191     03  LIDORDNR5               PIC S9(4) BINARY.                        
000192     03  LIDLOPNR-ORD            PIC S9(4) BINARY.                        
000193     03  LKDEMBTYP               PIC S9(4) BINARY.                        
000194     03  LBEEMBTYP               PIC S9(4) BINARY.                        
000195     03  LKVRADER                PIC S9(4) BINARY.                        
000196     03  LVKORDNTO               PIC S9(4) BINARY.                        
000197     03  LVLORDNTO               PIC S9(4) BINARY.                        
000198     03  LFLPMT3IV               PIC S9(4) BINARY.                        
000199     03  LBELAGINS-GRP           PIC S9(4) BINARY.                        
000200     03  LBELAGINS-DEL1          PIC S9(4) BINARY.                        
000201     03  LBELAGINS-DEL2          PIC S9(4) BINARY.                        
000202     03  LBEFDKRAV               PIC S9(4) BINARY.                        
000203     03  LFLURSRAP               PIC S9(4) BINARY.                        
000204     03  LKDRESP3IV              PIC S9(4) BINARY.                        
000205     03  LBERESP3IV              PIC S9(4) BINARY.                        
000206     03  LIDPURAD                PIC S9(4) BINARY.                        
000207     03  LADLOC3IV               PIC S9(4) BINARY.                        
000208     03  LADPLATS-1-3            PIC S9(4) BINARY.                        
000209     03  LADGANG                 PIC S9(4) BINARY.                        
000210     03  LADLAGOMR               PIC S9(4) BINARY.                        
000211     03  LIDLCD3IV               PIC S9(4) BINARY.                        
000212     03  LKVAVBART               PIC S9(4) BINARY.                        
000213     03  LKDSORT                 PIC S9(4) BINARY.                        
000214     03  LTESORT                 PIC S9(4) BINARY.                        
000215     03  LIDARTNR                PIC S9(4) BINARY.                        
000216     03  LBEART                  PIC S9(4) BINARY.                        
000217     03  LKDARTURS-NUM           PIC S9(4) BINARY.                        
000218     03  LBEARTURS-SVE           PIC S9(4) BINARY.                        
000219     03  LIDDC                   PIC S9(4) BINARY.                        
000220     03  LIDANSTNR               PIC S9(4) BINARY.                        
000221     03  LADPLATS-5              PIC S9(4) BINARY.                        
000222     03  LADPLATS-4              PIC S9(4) BINARY.                        
000223     03  LKDLCD3IV               PIC S9(4) BINARY.                        
000224*                                                                         
000225*    --- MID-AREA                                                         
000226*                                                                         
000227 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
000228                                                                          
000229*01  -COPY WMSGAREA                                                       
000230*                                                                         
000231 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
000232                                                                          
000233*01  -COPY W4I37A01                                                       
000234     SKIP3                                                                
000235 01  P-TO-P-SW1.                                                          
000236     03  PTOP1-LL                PIC S9(4)   VALUE 34 COMP SYNC.          
000237     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
000238     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
000239     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T378X'.             
000240     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
000241     03  FILLER                  PIC  X(4)   VALUE '437B'.                
000242     03  PTOP1-KDMFSFOR          PIC  X(1).                               
000243     03  -COPY W4I37801  -PRE PTOP1-                                      
000244*                                                                         
000245 01  FILLER                      PIC X(08) VALUE 'DLI:'.                  
000246*                                                                         
000247 01  NYCKLAR-TILL-DLI.                                                    
000248                                                                          
000249     03  W-4001-IDHTYP-X.                                                 
000250         05  W-4001-IDHTYP       PIC  X(4)  VALUE '4001'.                 
000251         05  W-4001-IDPRODNR     PIC  9(7).                               
000252         05  W-4001-IDPLKLST     PIC  9(3).                               
000253         05  W-4001-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
000254                                                                          
000255     03  W-4003-IDHTYP-X.                                                 
000256         05  W-4003-IDHTYP       PIC  X(4)  VALUE '4003'.                 
000257         05  W-4003-IDPRODNR     PIC  9(7).                               
000258         05  W-4003-IDPLKLST     PIC  9(3).                               
000259         05  W-4003-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
000260                                                                          
000261     03  W-4006-IDHTYP-X.                                                 
000262         05  W-4006-KDPRT        PIC  X(3).                               
000263         05  W-4006-KDSS-PLE     PIC  X(1).                               
000264         05  W-4006-ADLAGOMR     PIC S9(3) COMP-3.                        
000265         05  W-4006-ADGANG       PIC S9(3) COMP-3.                        
000266         05  W-4006-ADPLATS      PIC S9(5) COMP-3.                        
000267         05  W-4006-IDARTNR      PIC S9(9) COMP-3.                        
000268         05  W-4006-IDLOPNR      PIC S9(3) COMP-3.                        
000269                                                                          
000270     03  W-4006-MAX-KDPRT        PIC X(3)   VALUE '999'.                  
000271                                                                          
000272     03  W-4007-IDHTYP-X.                                                 
000273         05  W-4007-IDHTYP       PIC  X(4)  VALUE '4007'.                 
000274         05  W-4007-IDPRODNR     PIC  9(7).                               
000275         05  W-4007-IDPLKLST     PIC  9(3).                               
000276         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
000277                                                                          
000278     03  W-4010-IDHTYP-X.                                                 
000279         05  W-4010-IDORDER      PIC S9(7) COMP-3.                        
000280         05  W-4010-KDPRT        PIC  X(3).                               
000281         05  W-4010-KDSS-PU      PIC  X(1).                               
000282         05  W-4010-ADLAGOMR     PIC S9(3) COMP-3.                        
000283         05  W-4010-ADGANG       PIC S9(3) COMP-3.                        
000284         05  W-4010-ADPLATS      PIC S9(5) COMP-3.                        
000285         05  W-4010-IDARTNR      PIC S9(9) COMP-3.                        
000286         05  W-4010-IDLOPNR      PIC S9(3) COMP-3.                        
000287                                                                          
000288     03  W-4010-KDPRT-MAX-X.                                              
000289         05  W-4010-KDPRT-MAX    PIC  X(3) VALUE '999'.                   
000290                                                                          
000291     03  W-4732-IDHTYP-X.                                                 
000292         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
000293         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
000294         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
000295                                                                          
000296     03  W-4447-IDHTYP-X.                                                 
000297         05  W-4447-IDHTYP       PIC  X(4)  VALUE '4447'.                 
000298         05  W-4447-IDDC         PIC  X(2).                               
000299         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
000300                                                                          
000301     03  W-4448-IDPRC-X.                                                  
000302         05  W-4448-IDPRC-KEY    PIC  X(4).                               
000303         05  W-4448-LOW-VALUE    PIC  X(1)  VALUE LOW-VALUE.              
000304                                                                          
000305     03  W-4535-IDHTYP-X.                                                 
000306         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
000307         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
000308         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
000309                                                                          
000310     03  W-4536-IDSKYLT-X.                                                
000311         05  W-4536-IDSKYLT      PIC  X(3).                               
000312         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
000313                                                                          
000314     03  W-IDORDER-X.                                                     
000315         05  W-IDORDER           PIC S9(7) COMP-3.                        
000316                                                                          
000317     03  W-IDDC-X.                                                        
000318         05  W-IDDC              PIC X(2).                                
000319                                                                          
000320     03  W-IDGMT-X.                                                       
000321         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
000322         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
000323                                                                          
000324     03  W-WDQ301KY-MIN-X.                                                
000325         05  W-Q301-MIN-IDORDER  PIC S9(7) COMP-3.                        
000326         05  W-Q301-MIN-IDDC     PIC X(2).                                
000327         05  W-Q301-MIN-IDPRODNR PIC S9(7) COMP-3.                        
000328         05  W-Q301-MIN-IDPLKLST PIC S9(3) COMP-3.                        
000329                                                                          
000330     03  W-WDQ301KY-MAX-X.                                                
000331         05  W-Q301-MAX-IDORDER  PIC S9(7) COMP-3.                        
000332         05  W-Q301-MAX-IDDC     PIC X(2).                                
000333         05  W-Q301-MAX-IDPRODNR PIC S9(7) COMP-3.                        
000334         05  W-Q301-MAX-IDPLKLST PIC S9(3) COMP-3.                        
000335                                                                          
000336     03  W-IDDC-B6-X.                                                     
000337         05 W-IDDC-B6            PIC X(2).                                
000338                                                                          
000339     03  W-IDPRC-B6-X.                                                    
000340         05  W-IDPRC-B6          PIC X(4)    VALUE SPACE.                 
000341                                                                          
000342     03  W-WDQ301KY-X.                                                    
000343         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
000344         05  W-Q301KY-IDDC           PIC X(2).                            
000345         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
000346         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
000347                                                                          
000348 01  FILLER                  PIC X(16) VALUE 'IMS-WS STATUS-WS'.          
000349 01  STATUS-WS                   PIC XX.                                  
000350     88  SEGMENT-FINNS           VALUE '  '.                              
000351     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
000352     88  DATABAS-END             VALUE 'GE'.                              
000353                                                                          
000354 01  STATUS-WDGX4006-WS          PIC XX.                                  
000355     88  SEGMENT-WDGX4006-FINNS  VALUE '  '.                              
000356     88  SEGMENT-WDGX4006-SAKNAS VALUE 'GE'.                              
000357                                                                          
000358 01  STATUS-WDGX4007-WS          PIC XX.                                  
000359     88  SEGMENT-WDGX4007-FINNS  VALUE '  '.                              
000360     88  SEGMENT-WDGX4007-SAKNAS VALUE 'GE'.                              
000361                                                                          
000362 01  STATUS-WDGX4010-WS          PIC XX.                                  
000363     88  SEGMENT-WDGX4010-FINNS  VALUE '  '.                              
000364     88  SEGMENT-WDGX4010-SAKNAS VALUE 'GE'.                              
000365                                                                          
000366 01  GODK-STATUSKODER.                                                    
000367     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000368                                                                          
000369 01  FILLER                  PIC X(08) VALUE 'SSA-AREA'.                  
000370 01  SSA1                        PIC X(192).                              
000371 01  SSA2                        PIC X(96).                               
000372                                                                          
000373*    --- IMS FUNKTIONSKODER                                               
000374*01  -COPY W0003                                                          
000375                                                                          
000376 01  FILLER               PIC X(16)   VALUE 'WDGX4003'.                   
000377 01  DLI-IO-WDGX4003.                                                     
000378*    03  -COPY WDGX4003                                                   
000379                                                                          
000380 01  FILLER               PIC X(16)   VALUE 'WDGX4004'.                   
000381 01  DLI-IO-WDGX4004.                                                     
000382*    03  -COPY WDGX4004                                                   
000383                                                                          
000384 01  FILLER               PIC X(16)   VALUE 'WDGX4006'.                   
000385 01  DLI-IO-WDGX4006.                                                     
000386*    03  -COPY WDGX4006                                                   
000387                                                                          
000388 01  FILLER               PIC X(16)   VALUE 'WDGX4007'.                   
000389 01  DLI-IO-WDGX4007.                                                     
000390*    03  -COPY WDGX4007                                                   
000391                                                                          
000392 01  FILLER               PIC X(16)   VALUE 'WDGX4008'.                   
000393 01  DLI-IO-WDGX4008.                                                     
000394*    03  -COPY WDGX4008                                                   
000395                                                                          
000396 01  FILLER               PIC X(16)   VALUE 'WDGX4448'.                   
000397 01  DLI-IO-WDGX4448.                                                     
000398*    03  -COPY WDGX4448                                                   
000399                                                                          
000400 01  FILLER               PIC X(16)   VALUE 'WDGX4536'.                   
000401 01  DLI-IO-WDGX4536.                                                     
000402*    03  -COPY WDGX4536                                                   
000403                                                                          
000404 01  FILLER               PIC X(16)   VALUE 'WDGX4732'.                   
000405 01  DLI-IO-WDGX4732.                                                     
000406*    03  -COPY WDGX4732                                                   
000407                                                                          
000408 01  FILLER               PIC X(16)   VALUE 'WDGX4010'.                   
000409 01  DLI-IO-WDGX4010.                                                     
000410*    03  -COPY WDGX4010                                                   
000411                                                                          
000412 01  FILLER               PIC X(16)   VALUE 'WDB201  '.                   
000413 01  DLI-IO-WDB201.                                                       
000414*    03  -COPY WDB201                                                     
000415                                                                          
000416 01  FILLER               PIC X(16)   VALUE 'WDQ201-12'.                  
000417 01  DLI-IO-WDQ201-12.                                                    
000418*    03  -COPY WDQ201                                                     
000419*    03  -COPY WDQ212                                                     
000420                                                                          
000421 01  FILLER               PIC X(16)   VALUE 'WDQ301  '.                   
000422 01  DLI-IO-WDQ301.                                                       
000423*    03  -COPY WDQ301                                                     
000424                                                                          
000425 01  FILLER               PIC X(16)   VALUE 'WDB601  '.                   
000426 01  DLI-IO-WDB601.                                                       
000427*    03  -COPY WDB601                                                     
000428                                                                          
000429     EJECT                                                                
000430                                                                          
000431                                                                          
000432 LINKAGE SECTION.                                                         
000433*                                                                         
000434                                                                          
000435*01  -COPY W0009  -PRE MSG-                                               
000436                                                                          
000437*01  -COPY W0009  -PRE ALT1-                                              
000438     EJECT                                                                
000439*01  -COPY W0008  -PRE 4003-                                              
000440     05  FILLER                  PIC X.                                   
000441                                                                          
000442*01  -COPY W0008  -PRE 4007-                                              
000443     05  FILLER                  PIC X.                                   
000444                                                                          
000445*01  -COPY W0008  -PRE 4535-                                              
000446     05  FILLER                  PIC X.                                   
000447                                                                          
000448*01  -COPY W0008  -PRE 4732-                                              
000449     05  FILLER                  PIC X.                                   
000450                                                                          
000451*01  -COPY W0008  -PRE WDB2-                                              
000452     05  FILLER                  PIC X.                                   
000453                                                                          
000454*01  -COPY W0008  -PRE WDQ2-                                              
000455     05  FILLER                  PIC X.                                   
000456                                                                          
000457*01  -COPY W0008  -PRE WDQ3-                                              
000458     05  FILLER                  PIC X.                                   
000459                                                                          
000460*01  -COPY W0008  -PRE WDB6-                                              
000461     05  FILLER                  PIC X.                                   
000462                                                                          
000463                                                                          
000464  PROCEDURE DIVISION USING MSG-PCB  ALT1-PCB                              
000465                           4003-PCB 4007-PCB                              
000466                           4535-PCB 4732-PCB WDB2-PCB WDQ2-PCB            
000467                           WDQ3-PCB WDB6-PCB.                             
000468  MAIN SECTION.                                                           
000469     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB                              
000470                           4003-PCB 4007-PCB                              
000471                           4535-PCB 4732-PCB WDB2-PCB WDQ2-PCB            
000472                           WDQ3-PCB WDB6-PCB.                             
000473                                                                          
000474     PERFORM IMS-GU-MSG                                                   
000475     IF SEGMENT-FINNS                                                     
000476       PERFORM A-INIT                                                     
000477       PERFORM B-LAES-PU-INFO-4007-4010                                   
000478       PERFORM S90-OPEN-SEND-RESPASSIGNMENT                               
000479                                                                          
000480       PERFORM D-BEHANDLA-HEADER                                          
000481       PERFORM S02-SEND-RESPONSE                                          
000482*                                                                         
000483       IF SEGMENT-WDGX4010-FINNS                                          
000484         IF 4010-IDARTNR > +0                                             
000485           PERFORM E-BEHANDLA-CUSTOMER-SUMMARY                            
000486           PERFORM F-LAES-PLE-INFO-4003                                   
000487           PERFORM G-BEHANDLA-PICKTASK                                    
000488         END-IF                                                           
000489       END-IF                                                             
000490*                                                                         
000491       PERFORM H-RENSA-PLOCKSATS                                          
000492       PERFORM I-SKICKA-IMSTRANS                                          
000493                                                                          
000494       PERFORM S94-CLOSE-SEND                                             
000495     END-IF                                                               
000496                                                                          
000497     MOVE ZERO TO RETURN-CODE                                             
000498     GOBACK.                                                              
000499                                                                          
000500 A-INIT SECTION.                                                          
000501     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
000502                                                                          
000503     MOVE JA  TO ALLT-SW                                                  
000504                                                                          
000505     MOVE NEJ TO NO-LINES-SW                                              
000506                                                                          
000507     IF MSG-KDTRANS-1  NOT = 'W4037BU '                                   
000508        MOVE NEJ TO ALLT-SW                                               
000509     END-IF                                                               
000510                                                                          
000511     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
000512                                                                          
000513                                                                          
000514     MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I37A01                     
000515     MOVE MSG-KDMFSFOR-1                 TO PTOP1-KDMFSFOR                
000516                                                                          
000517     MOVE "S  "             TO MED-IDSKYLT                                
000518                                                                          
000519     MOVE ZERO              TO RESP-KDRESP3IV                             
000520     MOVE SPACE             TO RESP-BERESP3IV                             
000521     MOVE SPACE             TO TEMP-MESSAGE                               
000522                                                                          
000523     MOVE '1'               TO PTOP1-KDMFSFOR                             
000524                                                                          
000525     ACCEPT WS-DATUM         FROM TIME                                    
000526     ACCEPT WS-KLOCKAN       FROM TIME                                    
000527                                                                          
000528     MOVE ZERO       TO SPAR-IDORDER                                      
000529                                                                          
000530     MOVE '999'      TO W-4010-KDPRT-MAX                                  
000531     MOVE '1'        TO PTOP1-KDMFSFOR                                    
000532     .                                                                    
000533                                                                          
000534 B-LAES-PU-INFO-4007-4010 SECTION.                                        
000535     MOVE 'B-LAES-PU-INFO-4007-4010' TO WS-CURRENT-SECTION                
000536                                                                          
000537     MOVE MID-IDPRODNR          TO W-4007-IDPRODNR                        
000538     MOVE MID-IDPLKLST          TO W-4007-IDPLKLST                        
000539                                                                          
000540     PERFORM IMS-04-GU-WDGX4008                                           
000541                                                                          
000542     IF SEGMENT-WDGX4007-FINNS                                            
000543       PERFORM IMS-06-GNP-WDGX4010-OKVAL                                  
000544       IF SEGMENT-FINNS                                                   
000545         IF 4010-IDARTNR = +0                                             
000546           MOVE JA              TO NO-LINES-SW                            
000547         END-IF                                                           
000548       END-IF                                                             
000549     END-IF                                                               
000550     .                                                                    
000551                                                                          
000552 D-BEHANDLA-HEADER   SECTION.                                             
000553     MOVE 'D-BEHANDLA-HEADER' TO WS-CURRENT-SECTION                       
000554                                                                          
000555     MOVE SPACE TO SEND-AREA                                              
000556     MOVE 1 TO SEND-KVDLEN                                                
000557                                                                          
000558*IDMSG3IV                                                                 
000559     MOVE 4008-IDMSG3IV        TO RESP-IDMSG3IV                           
000560     MOVE ZERO TO LIDMSG3IV                                               
000561     INSPECT FUNCTION REVERSE(RESP-IDMSG3IV)                              
000562          TALLYING LIDMSG3IV FOR LEADING SPACE                            
000563     COMPUTE LIDMSG3IV = LENGTH OF RESP-IDMSG3IV - LIDMSG3IV              
000564                                                                          
000565     STRING RESP-IDMSG3IV(1:LIDMSG3IV) VBAR                               
000566     DELIMITED BY SIZE                                                    
000567     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000568                                                                          
000569*IDMTYP3IV                                                                
000570     MOVE 'ResponseAssignment' TO RESP-IDMTYP3IV                          
000571     MOVE ZERO TO LIDMTYP3IV                                              
000572     INSPECT FUNCTION REVERSE(RESP-IDMTYP3IV)                             
000573          TALLYING LIDMTYP3IV FOR LEADING SPACE                           
000574     COMPUTE LIDMTYP3IV = LENGTH OF RESP-IDMTYP3IV - LIDMTYP3IV           
000575                                                                          
000576     STRING RESP-IDMTYP3IV(1:LIDMTYP3IV) VBAR                             
000577     DELIMITED BY SIZE                                                    
000578     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000579*                                                                         
000580*TISTAMP3IV                                                               
000581     MOVE FUNCTION CURRENT-DATE(1:16) TO RESP-TISTAMP3IV                  
000582     MOVE '0'                         TO RESP-TISTAMP3IV(17:1)            
000583     MOVE ZERO TO LTISTAMP3IV                                             
000584     INSPECT FUNCTION REVERSE(RESP-TISTAMP3IV)                            
000585          TALLYING LTISTAMP3IV FOR LEADING SPACE                          
000586     COMPUTE LTISTAMP3IV = LENGTH OF RESP-TISTAMP3IV - LTISTAMP3IV        
000587                                                                          
000588     STRING RESP-TISTAMP3IV (1:LTISTAMP3IV) VBAR                          
000589     DELIMITED BY SIZE                                                    
000590     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000591*                                                                         
000592*IDDC                                                                     
000593     MOVE MID-IDDC             TO RESP-IDDC                               
000594     MOVE ZERO TO LIDDC                                                   
000595     INSPECT FUNCTION REVERSE(RESP-IDDC)                                  
000596          TALLYING LIDDC FOR LEADING SPACE                                
000597     COMPUTE LIDDC = LENGTH OF RESP-IDDC - LIDDC                          
000598                                                                          
000599     STRING RESP-IDDC (1:LIDDC) VBAR                                      
000600     DELIMITED BY SIZE                                                    
000601     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000602*                                                                         
000603*IDANSTNR                                                                 
000604     MOVE MID-IDUSER           TO WS-IDUSER                               
000605     MOVE WS-IDANSTNR-5        TO RESP-IDANSTNR                           
000606     MOVE 1 TO LIDANSTNR                                                  
000607     INSPECT RESP-IDANSTNR                                                
000608       TALLYING LIDANSTNR FOR LEADING SPACE                               
000609     STRING RESP-IDANSTNR (LIDANSTNR:) VBAR                               
000610       DELIMITED BY SIZE                                                  
000611       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000612                                                                          
000613*IDSNO3IV                                                                 
000614     MOVE 4008-IDSNO3IV        TO RESP-IDSNO3IV                           
000615     MOVE ZERO TO LIDSNO3IV                                               
000616     INSPECT FUNCTION REVERSE(RESP-IDSNO3IV)                              
000617          TALLYING LIDSNO3IV FOR LEADING SPACE                            
000618     COMPUTE LIDSNO3IV = LENGTH OF RESP-IDSNO3IV - LIDSNO3IV              
000619                                                                          
000620     STRING RESP-IDSNO3IV (1:LIDSNO3IV) VBAR                              
000621     DELIMITED BY SIZE                                                    
000622     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000623                                                                          
000624*FELMEDDELANDE   OM INGA RADER FINNS.                                     
000625     IF NO-LINES                                                          
000626**       --- LINES MISSING                                                
000627       MOVE SPACE                TO TEMP-MESSAGE                          
000628       MOVE ERR-ITEMS-MISSING    TO MED-IDMFSFEL                          
000629       CALL WMEDKONV USING MED-WMEDAREA                                   
000630                                                                          
000631       MOVE MED-IDMFSFEL       TO RESP-KDRESP3IV                          
000632       MOVE MED-TEMFSFEL       TO RESP-BERESP3IV                          
000633     ELSE                                                                 
000634       MOVE ZERO               TO RESP-KDRESP3IV                          
000635       MOVE SPACE              TO RESP-BERESP3IV                          
000636     END-IF                                                               
000637*                                                                         
000638*KDRESP3IH                                                                
000639     MOVE 1 TO LKDRESP3IV                                                 
000640     INSPECT RESP-KDRESP3IV                                               
000641       TALLYING LKDRESP3IV FOR LEADING SPACE                              
000642     STRING RESP-KDRESP3IV (LKDRESP3IV:) VBAR                             
000643       DELIMITED BY SIZE                                                  
000644       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000645                                                                          
000646*BERESP3IV                                                                
000647     MOVE ZERO TO LBERESP3IV                                              
000648     INSPECT FUNCTION REVERSE(RESP-BERESP3IV)                             
000649          TALLYING LBERESP3IV FOR LEADING SPACE                           
000650     COMPUTE LBERESP3IV = LENGTH OF RESP-BERESP3IV - LBERESP3IV           
000651                                                                          
000652     IF LBERESP3IV > ZERO                                                 
000653       STRING RESP-BERESP3IV(1:LBERESP3IV)                                
000654       DELIMITED BY SIZE                                                  
000655       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000656     END-IF                                                               
000657                                                                          
000658*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
000659*    -- ADJUST TO CORRECT LENGTH                                          
000660     SUBTRACT 1 FROM SEND-KVDLEN                                          
000661     .                                                                    
000662 E-BEHANDLA-CUSTOMER-SUMMARY   SECTION.                                   
000663     MOVE 'E-BEHANDLA-CUSTOMER-SUM '   TO WS-CURRENT-SECTION              
000664                                                                          
000665     MOVE +0  TO ODEL-ANV-IX                                              
000666                                                                          
000667     PERFORM UNTIL SEGMENT-SAKNAS                                         
000668                                                                          
000669       IF SPAR-IDORDER NOT = 4010-IDORDER                                 
000670                                                                          
000671          PERFORM S01-LAES-ORDERHUVUD                                     
000672                                                                          
000673          ADD 1 TO ODEL-ANV-IX                                            
000674          MOVE ODEL-ANV-IX TO ODEL-NYTT-IX(4010-IDLOPNR-ORD)              
000675                                                                          
000676          PERFORM EA-FLYTTA-TILL-UTAREA                                   
000677          PERFORM S93-SEND-RESPONSE                                       
000678                                                                          
000679          MOVE 4010-IDORDER TO SPAR-IDORDER                               
000680       END-IF                                                             
000681                                                                          
000682       PERFORM IMS-06-GNP-WDGX4010-OKVAL                                  
000683     END-PERFORM                                                          
000684     .                                                                    
000685                                                                          
000686 EA-FLYTTA-TILL-UTAREA SECTION.                                           
000687     MOVE 'EA-SKAPA-PACKHUVUD '   TO WS-CURRENT-SECTION                   
000688                                                                          
000689     PERFORM EAA-LAES-ORDERDEL                                            
000690     PERFORM EAD-FLYTTA-TILL-UTAREA                                       
000691     .                                                                    
000692                                                                          
000693 EAA-LAES-ORDERDEL SECTION.                                               
000694     MOVE 'EAA-LAES-ORDERDEL'     TO WS-CURRENT-SECTION                   
000695                                                                          
000696     MOVE 4010-IDORDER  TO W-Q301-MIN-IDORDER                             
000697                           W-Q301-MAX-IDORDER                             
000698     MOVE 4010-IDDC     TO W-Q301-MIN-IDDC                                
000699                           W-Q301-MAX-IDDC                                
000700     MOVE 4010-IDPRODNR TO W-Q301-MIN-IDPRODNR                            
000701                           W-Q301-MAX-IDPRODNR                            
000702     MOVE 4010-IDPLKLST TO W-Q301-MIN-IDPLKLST                            
000703                           W-Q301-MAX-IDPLKLST                            
000704                                                                          
000705     PERFORM IMS-08-GU-WDQ301                                             
000706     .                                                                    
000707                                                                          
000708 EAD-FLYTTA-TILL-UTAREA SECTION.                                          
000709     MOVE 'EAD-REDIGERA-PACKHUVUD'    TO WS-CURRENT-SECTION               
000710                                                                          
000711     MOVE SPACE TO SEND-AREA                                              
000712     MOVE 1     TO SEND-KVDLEN                                            
000713                                                                          
000714*    READ WDB6 FOR DCS- TEST LATER IN THIS SECTION                        
000715     IF W-IDDC NOT = W-IDDC-B6                                            
000716        MOVE W-IDDC TO W-IDDC-B6                                          
000717        PERFORM IMS-GU-WDB601                                             
000718     END-IF                                                               
000719                                                                          
000720*IDRTYP3IV                                                                
000721     MOVE 'CustomerSummary'    TO CUST-IDRTYP3IV                          
000722     MOVE ZERO TO LIDRTYP3IV                                              
000723     INSPECT FUNCTION REVERSE(CUST-IDRTYP3IV)                             
000724          TALLYING LIDRTYP3IV   FOR LEADING SPACE                         
000725     COMPUTE LIDRTYP3IV = LENGTH OF CUST-IDRTYP3IV - LIDRTYP3IV           
000726                                                                          
000727     STRING CUST-IDRTYP3IV (1:LIDRTYP3IV) VBAR                            
000728     DELIMITED BY SIZE                                                    
000729     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000730                                                                          
000731*IDPRODNR                                                                 
000732     MOVE 4010-IDPRODNR     TO CUST-IDPRODNR                              
000733     MOVE 1 TO LIDPRODNR                                                  
000734     INSPECT CUST-IDPRODNR                                                
000735       TALLYING LIDPRODNR FOR LEADING SPACE                               
000736     STRING CUST-IDPRODNR (LIDPRODNR:) VBAR                               
000737       DELIMITED BY SIZE                                                  
000738       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000739                                                                          
000740*IDPLKLST                                                                 
000741     MOVE 4010-IDPLKLST     TO CUST-IDPLKLST                              
000742     MOVE 1 TO LIDPLKLST                                                  
000743     INSPECT CUST-IDPLKLST                                                
000744       TALLYING LIDPLKLST FOR LEADING SPACE                               
000745     STRING CUST-IDPLKLST (LIDPLKLST:) VBAR                               
000746       DELIMITED BY SIZE                                                  
000747       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000748                                                                          
000749*IDDISTR                                                                  
000750     MOVE OHUV-IDDISTR      TO CUST-IDDISTR                               
000751     MOVE 1 TO LIDDISTR                                                   
000752     INSPECT CUST-IDDISTR                                                 
000753       TALLYING LIDDISTR  FOR LEADING SPACE                               
000754     STRING CUST-IDDISTR  (LIDDISTR:) VBAR                                
000755       DELIMITED BY SIZE                                                  
000756       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000757                                                                          
000758*IDKUNDNR                                                                 
000759     MOVE OHUV-IDKUNDNR     TO CUST-IDKUNDNR                              
000760     MOVE 1 TO LIDKUNDNR                                                  
000761     INSPECT CUST-IDKUNDNR                                                
000762       TALLYING LIDKUNDNR FOR LEADING SPACE                               
000763     STRING CUST-IDKUNDNR (LIDKUNDNR:) VBAR                               
000764       DELIMITED BY SIZE                                                  
000765       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000766                                                                          
000767*IDORDNR5                                                                 
000768     MOVE OHUV-IDORDNR7     TO CUST-IDORDNR5                              
000769     MOVE 1 TO LIDORDNR5                                                  
000770     INSPECT CUST-IDORDNR5                                                
000771       TALLYING LIDORDNR5 FOR LEADING SPACE                               
000772     STRING CUST-IDORDNR5 (LIDORDNR5:) VBAR                               
000773       DELIMITED BY SIZE                                                  
000774       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000775                                                                          
000776*IDPRC                                                                    
000777     MOVE 4010-IDPRC        TO CUST-IDPRC                                 
000778     MOVE ZERO TO LIDPRC                                                  
000779     INSPECT FUNCTION REVERSE(CUST-IDPRC)                                 
000780          TALLYING LIDPRC  FOR LEADING SPACE                              
000781     COMPUTE LIDPRC = LENGTH OF CUST-IDPRC - LIDPRC                       
000782                                                                          
000783     STRING CUST-IDPRC (1:LIDPRC) VBAR                                    
000784     DELIMITED BY SIZE                                                    
000785     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000786                                                                          
000787*IDLOPNR-ORD                                                              
000788*    IDLOPNR FRÅN 4010 KAN INTE ANVÄNDAS EFTERSOM                         
000789*    VISSA ORDERDELAR KAN VARA TOMMA OCH SKALL RÄKNAS BORT.               
000790     MOVE ODEL-NYTT-IX(4010-IDLOPNR-ORD) TO CUST-IDLOPNR-ORD              
000791                                                                          
000792     MOVE 1 TO LIDLOPNR-ORD                                               
000793     INSPECT CUST-IDLOPNR-ORD                                             
000794       TALLYING LIDLOPNR-ORD FOR LEADING SPACE                            
000795     STRING CUST-IDLOPNR-ORD (LIDLOPNR-ORD:) VBAR                         
000796       DELIMITED BY SIZE                                                  
000797       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000798                                                                          
000799*KDEMBTYP                                                                 
000800     MOVE 4010-KDFDKRAV           TO WS-KDFDKRAV                          
000801     EVALUATE TRUE                                                        
000802     WHEN WS-KDFDKRAV(3:1) = 1                                            
000803*      LÅDA                                                               
000804       MOVE 1                     TO CUST-KDEMBTYP                        
000805     WHEN WS-KDFDKRAV(3:1) = 2                                            
000806*      PAKET                                                              
000807       MOVE 2                     TO CUST-KDEMBTYP                        
000808     WHEN WS-KDFDKRAV(3:1) = 3                                            
000809*      BUNT                                                               
000810       MOVE 3                     TO CUST-KDEMBTYP                        
000811     WHEN WS-KDFDKRAV(3:1) = 4                                            
000812*      HÄCK                                                               
000813       MOVE 4                     TO CUST-KDEMBTYP                        
000814     WHEN WS-KDFDKRAV(3:1) = 5                                            
000815*      STYCK                                                              
000816       MOVE 5                     TO CUST-KDEMBTYP                        
000817     WHEN WS-KDFDKRAV(3:1) = 6                                            
000818*      CONTAINER                                                          
000819       MOVE 6                     TO CUST-KDEMBTYP                        
000820     WHEN WS-KDFDKRAV(3:1) = 7                                            
000821*      PALL                                                               
000822       MOVE 7                     TO CUST-KDEMBTYP                        
000823     WHEN OTHER                                                           
000824*      I ANNAT FALL LÅDA                                                  
000825       MOVE 1                     TO CUST-KDEMBTYP                        
000826     END-EVALUATE                                                         
000827                                                                          
000828     STRING CUST-KDEMBTYP VBAR                                            
000829     DELIMITED BY SIZE                                                    
000830     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000831                                                                          
000832*BEEMBTYP                                                                 
000833     EVALUATE TRUE                                                        
000834     WHEN WS-KDFDKRAV(3:1) = 1                                            
000835       MOVE 'låda'                TO CUST-BEEMBTYP                        
000836     WHEN WS-KDFDKRAV(3:1) = 2                                            
000837       MOVE 'paket'               TO CUST-BEEMBTYP                        
000838     WHEN WS-KDFDKRAV(3:1) = 3                                            
000839       MOVE 'bunt'                TO CUST-BEEMBTYP                        
000840     WHEN WS-KDFDKRAV(3:1) = 4                                            
000841       MOVE 'häck'                TO CUST-BEEMBTYP                        
000842     WHEN WS-KDFDKRAV(3:1) = 5                                            
000843       MOVE 'styck'               TO CUST-BEEMBTYP                        
000844     WHEN WS-KDFDKRAV(3:1) = 6                                            
000845       MOVE 'container'           TO CUST-BEEMBTYP                        
000846     WHEN WS-KDFDKRAV(3:1) = 7                                            
000847       MOVE 'pall'                TO CUST-BEEMBTYP                        
000848     WHEN OTHER                                                           
000849       MOVE 'låda'                TO CUST-BEEMBTYP                        
000850     END-EVALUATE                                                         
000851                                                                          
000852     MOVE ZERO TO LBEEMBTYP                                               
000853     INSPECT FUNCTION REVERSE(CUST-BEEMBTYP)                              
000854          TALLYING LBEEMBTYP FOR LEADING SPACE                            
000855     COMPUTE LBEEMBTYP =                                                  
000856             LENGTH OF CUST-BEEMBTYP - LBEEMBTYP                          
000857                                                                          
000858     STRING CUST-BEEMBTYP (1:LBEEMBTYP) VBAR                              
000859     DELIMITED BY SIZE                                                    
000860     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000861                                                                          
000862*DestinationCheckDigit - OBS! inget dataelement behövs ännu.              
000863     STRING VBAR                                                          
000864       DELIMITED BY SIZE                                                  
000865       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000866                                                                          
000867*KVRADER                                                                  
000868     MOVE ODEL-KVRADER      TO CUST-KVRADER                               
000869     MOVE 1 TO LKVRADER                                                   
000870     INSPECT CUST-KVRADER                                                 
000871       TALLYING LKVRADER FOR LEADING SPACE                                
000872     STRING CUST-KVRADER (LKVRADER:) VBAR                                 
000873       DELIMITED BY SIZE                                                  
000874       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000875                                                                          
000876*VKORDNTO                                                                 
000877     MOVE ODEL-VKORDNTO     TO CUST-VKORDNTO                              
000878     MOVE 1 TO LVKORDNTO                                                  
000879     INSPECT CUST-VKORDNTO                                                
000880       TALLYING LVKORDNTO  FOR LEADING SPACE                              
000881     STRING CUST-VKORDNTO (LVKORDNTO:) VBAR                               
000882       DELIMITED BY SIZE                                                  
000883       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000884                                                                          
000885*VLORDNTO                                                                 
000886     MOVE ODEL-VLORDNTO     TO CUST-VLORDNTO                              
000887     MOVE 1 TO LVLORDNTO                                                  
000888     INSPECT CUST-VLORDNTO                                                
000889       TALLYING LVLORDNTO  FOR LEADING SPACE                              
000890     STRING CUST-VLORDNTO (LVLORDNTO:) VBAR                               
000891       DELIMITED BY SIZE                                                  
000892       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
000893                                                                          
000894*FLPMT3IV                                                                 
000895     MOVE 4010-KDFDKRAV           TO W-4535-KDFDKRAV                      
000896     EVALUATE TRUE                                                        
000897       WHEN  DCS-CDC                                                      
000898         OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                             
000899          MOVE 'S  '           TO W-4536-IDSKYLT                          
000900       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
000901          MOVE 'I  '           TO W-4536-IDSKYLT                          
000902       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
000903          MOVE 'E  '           TO W-4536-IDSKYLT                          
000904       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
000905          MOVE 'GB '           TO W-4536-IDSKYLT                          
000906       WHEN OTHER                                                         
000907          MOVE 'GB '           TO W-4536-IDSKYLT                          
000908     END-EVALUATE                                                         
000909                                                                          
000910     PERFORM IMS-09-GU-WDGX4536                                           
000911     IF SEGMENT-FINNS                                                     
000912       MOVE 4536-BEFDKRAV      TO WS-BEFDKRAV                             
000913     ELSE                                                                 
000914       MOVE SPACE              TO WS-BEFDKRAV                             
000915     END-IF                                                               
000916                                                                          
000917     IF 4536-BEFDKRAV     > SPACE                                         
000918     OR OHUV-BELAGINS-GRP > SPACE                                         
000919       MOVE JA                  TO CUST-FLPMT3IV                          
000920       MOVE JA                  TO WS-FLPMT3IV                            
000921     ELSE                                                                 
000922       MOVE NEJ                 TO CUST-FLPMT3IV                          
000923       MOVE NEJ                 TO WS-FLPMT3IV                            
000924     END-IF                                                               
000925                                                                          
000926     MOVE ZERO TO LFLPMT3IV                                               
000927     INSPECT FUNCTION REVERSE(CUST-FLPMT3IV)                              
000928          TALLYING LFLPMT3IV  FOR LEADING SPACE                           
000929     COMPUTE LFLPMT3IV = LENGTH OF CUST-FLPMT3IV - LFLPMT3IV              
000930                                                                          
000931     STRING CUST-FLPMT3IV (1:LFLPMT3IV) VBAR                              
000932     DELIMITED BY SIZE                                                    
000933     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000934                                                                          
000935*BELAGINS-GRP                                                             
000936     IF WS-FLPMT3IV = JA                                                  
000937       IF OHUV-BELAGINS-GRP = SPACE                                       
000938         MOVE OHUV-BELAGINS-GRP       TO CUST-BELAGINS-GRP                
000939       ELSE                                                               
000940          IF OHUV-BELAGINS-DEL1 = SPACE                                   
000941             MOVE OHUV-BELAGINS-DEL2  TO CUST-BELAGINS-DEL1               
000942          ELSE                                                            
000943             MOVE OHUV-BELAGINS-DEL1  TO CUST-BELAGINS-DEL1               
000944             IF OHUV-BELAGINS-DEL2 NOT = SPACE                            
000945                                                                          
000946               MOVE OHUV-BELAGINS-DEL2 TO CUST-BELAGINS-DEL2              
000947             END-IF                                                       
000948          END-IF                                                          
000949       END-IF                                                             
000950     ELSE                                                                 
000951       MOVE SPACE                     TO CUST-BELAGINS-GRP                
000952     END-IF                                                               
000953                                                                          
000954     MOVE ZERO TO LBELAGINS-GRP                                           
000955     INSPECT FUNCTION REVERSE(CUST-BELAGINS-GRP)                          
000956          TALLYING LBELAGINS-GRP FOR LEADING SPACE                        
000957     COMPUTE LBELAGINS-GRP =                                              
000958             LENGTH OF CUST-BELAGINS-GRP - LBELAGINS-GRP                  
000959                                                                          
000960     IF LBELAGINS-GRP > ZERO                                              
000961       CONTINUE                                                           
000962     ELSE                                                                 
000963       MOVE +1 TO LBELAGINS-GRP                                           
000964     END-IF                                                               
000965     STRING CUST-BELAGINS-GRP (1:LBELAGINS-GRP) VBAR                      
000966     DELIMITED BY SIZE                                                    
000967     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000968                                                                          
000969*BEFDKRAV GES VÄRDE OVAN                                                  
000970     IF WS-FLPMT3IV = JA                                                  
000971       MOVE WS-BEFDKRAV        TO CUST-BEFDKRAV                           
000972     ELSE                                                                 
000973       MOVE SPACE              TO CUST-BEFDKRAV                           
000974     END-IF                                                               
000975                                                                          
000976     MOVE ZERO TO LBEFDKRAV                                               
000977     INSPECT FUNCTION REVERSE(CUST-BEFDKRAV)                              
000978          TALLYING LBEFDKRAV  FOR LEADING SPACE                           
000979     COMPUTE LBEFDKRAV = LENGTH OF CUST-BEFDKRAV - LBEFDKRAV              
000980                                                                          
000981     IF LBEFDKRAV > ZERO                                                  
000982       CONTINUE                                                           
000983     ELSE                                                                 
000984       MOVE +1 TO LBEFDKRAV                                               
000985     END-IF                                                               
000986     STRING CUST-BEFDKRAV (1:LBEFDKRAV) VBAR                              
000987     DELIMITED BY SIZE                                                    
000988     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
000989                                                                          
000990*FLURSRAP3IV                                                              
000991     IF DIST08-URSP-RAPP                                                  
000992     OR DIST08-URSP-SPX                                                   
000993     OR (DCS-CDC AND DIST08-URSP-RAPP-CDC)                                
000994        MOVE JA                TO CUST-FLURSRAP                           
000995     ELSE                                                                 
000996        MOVE NEJ               TO CUST-FLURSRAP                           
000997     END-IF                                                               
000998                                                                          
000999     MOVE ZERO TO LFLURSRAP                                               
001000     INSPECT FUNCTION REVERSE(CUST-FLURSRAP)                              
001001          TALLYING LFLURSRAP  FOR LEADING SPACE                           
001002     COMPUTE LFLURSRAP =                                                  
001003             LENGTH OF CUST-FLURSRAP - LFLURSRAP                          
001004                                                                          
001005     IF LFLURSRAP > ZERO                                                  
001006       CONTINUE                                                           
001007     ELSE                                                                 
001008       MOVE +1 TO LFLURSRAP                                               
001009     END-IF                                                               
001010     STRING CUST-FLURSRAP (1:LFLURSRAP)                                   
001011     DELIMITED BY SIZE                                                    
001012     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001013                                                                          
001014*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
001015*    -- ADJUST TO CORRECT LENGTH                                          
001016     SUBTRACT 1 FROM SEND-KVDLEN                                          
001017     .                                                                    
001018                                                                          
001019 F-LAES-PLE-INFO-4003 SECTION.                                            
001020     MOVE 'F-LAES-PLE-INFO-4003' TO WS-CURRENT-SECTION                    
001021                                                                          
001022     MOVE MID-IDPRODNR        TO W-4003-IDPRODNR                          
001023     MOVE MID-IDPLKLST        TO W-4003-IDPLKLST                          
001024                                                                          
001025     PERFORM IMS-01-GHU-WDGX4004                                          
001026     .                                                                    
001027                                                                          
001028 G-BEHANDLA-PICKTASK SECTION.                                             
001029     MOVE 'G-BEHANDLA-PICKTASK' TO WS-CURRENT-SECTION                     
001030                                                                          
001031     PERFORM UNTIL SEGMENT-SAKNAS                                         
001032                OR DATABAS-END                                            
001033        PERFORM GA-LAES-PLOCKRAD                                          
001034        PERFORM GB-REDIGERA-PRINTRADER                                    
001035        IF SEGMENT-WDGX4006-FINNS                                         
001036          PERFORM S93-SEND-RESPONSE                                       
001037        END-IF                                                            
001038     END-PERFORM                                                          
001039     .                                                                    
001040                                                                          
001041 GA-LAES-PLOCKRAD SECTION.                                                
001042     MOVE 'GA-LAES-PLOCKRAD   ' TO WS-CURRENT-SECTION                     
001043                                                                          
001044     PERFORM IMS-03-GNP-WDGX4006-OKVAL                                    
001045     .                                                                    
001046                                                                          
001047 GB-REDIGERA-PRINTRADER SECTION.                                          
001048     MOVE 'GB-REDIGERA-PRINT'    TO WS-CURRENT-SECTION                    
001049                                                                          
001050     MOVE SPACE TO SEND-AREA                                              
001051     MOVE 1     TO SEND-KVDLEN                                            
001052*IDRTYP3IV                                                                
001053     MOVE 'PickTask'             TO PICT-IDRTYP3IV                        
001054     MOVE ZERO TO LIDRTYP3IV                                              
001055     INSPECT FUNCTION REVERSE(PICT-IDRTYP3IV)                             
001056          TALLYING LIDRTYP3IV   FOR LEADING SPACE                         
001057     COMPUTE LIDRTYP3IV = LENGTH OF PICT-IDRTYP3IV - LIDRTYP3IV           
001058                                                                          
001059     STRING PICT-IDRTYP3IV (1:LIDRTYP3IV) VBAR                            
001060     DELIMITED BY SIZE                                                    
001061     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001062*IDPURAD                                                                  
001063     MOVE 4006-IDRADNR           TO PICT-IDPURAD                          
001064     MOVE 1 TO LIDPURAD                                                   
001065     INSPECT PICT-IDPURAD                                                 
001066       TALLYING LIDPURAD FOR LEADING SPACE                                
001067     STRING PICT-IDPURAD (LIDPURAD:) VBAR                                 
001068       DELIMITED BY SIZE                                                  
001069       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001070                                                                          
001071*IDLOPNR-ORD                                                              
001072     MOVE ODEL-NYTT-IX(4006-IDLOPNR-ORD)                                  
001073                                 TO PICT-IDLOPNR-ORD                      
001074     MOVE 1 TO LIDLOPNR-ORD                                               
001075     INSPECT PICT-IDLOPNR-ORD                                             
001076       TALLYING LIDLOPNR-ORD FOR LEADING SPACE                            
001077     STRING PICT-IDLOPNR-ORD (LIDLOPNR-ORD:) VBAR                         
001078       DELIMITED BY SIZE                                                  
001079       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001080                                                                          
001081*ADLOC3IV                                                                 
001082     MOVE 4006-ADLAGOMR-ORD      TO WS-ADLAGOMR                           
001083     MOVE 4006-ADGANG            TO WS-ADGANG                             
001084                                                                          
001085     MOVE 4006-ADPLATS           TO WS-ADPLATS                            
001086                                                                          
001087     MOVE WS-ADPLATS(1:3)        TO WS-ADPLATS3                           
001088     MOVE WS-ADPLATS(4:1)        TO WS-ADPLATS4                           
001089     MOVE WS-ADPLATS(5:1)        TO WS-ADPLATS5                           
001090                                                                          
001091     MOVE SPACE                  TO WS-ADLOC3IV-SPACE                     
001092     MOVE WS-ADLOC3IV            TO PICT-ADLOC3IV                         
001093                                                                          
001094     MOVE ZERO TO LADLOC3IV                                               
001095     INSPECT FUNCTION REVERSE(PICT-ADLOC3IV)                              
001096          TALLYING LADLOC3IV   FOR LEADING SPACE                          
001097     COMPUTE LADLOC3IV = LENGTH OF PICT-ADLOC3IV - LADLOC3IV              
001098                                                                          
001099     STRING PICT-ADLOC3IV (1:LADLOC3IV) VBAR                              
001100     DELIMITED BY SIZE                                                    
001101     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001102                                                                          
001103*ADPLATS-5                                                                
001104     MOVE WS-ADPLATS5            TO PICT-ADPLATS-5                        
001105     MOVE 1 TO LADPLATS-5                                                 
001106     INSPECT PICT-ADPLATS-5                                               
001107       TALLYING LADPLATS-5   FOR LEADING SPACE                            
001108     STRING PICT-ADPLATS-5   (LADPLATS-5:) VBAR                           
001109       DELIMITED BY SIZE                                                  
001110       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001111*                                                                         
001112*ADPLATS-4-1                                                              
001113     MOVE WS-ADPLATS4            TO PICT-ADPLATS-4                        
001114     MOVE 1 TO LADPLATS-4                                                 
001115     INSPECT PICT-ADPLATS-4                                               
001116       TALLYING LADPLATS-4 FOR LEADING SPACE                              
001117     STRING PICT-ADPLATS-4 (LADPLATS-4:) VBAR                             
001118       DELIMITED BY SIZE                                                  
001119       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001120*                                                                         
001121                                                                          
001122*ADPLATS-1-3                                                              
001123     MOVE WS-ADPLATS3            TO PICT-ADPLATS-1-3                      
001124     MOVE 1 TO LADPLATS-1-3                                               
001125     INSPECT PICT-ADPLATS-1-3                                             
001126       TALLYING LADPLATS-1-3 FOR LEADING SPACE                            
001127     STRING PICT-ADPLATS-1-3 (LADPLATS-1-3:) VBAR                         
001128       DELIMITED BY SIZE                                                  
001129       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001130*                                                                         
001131     MOVE WS-ADPLATS4            TO PICT-ADPLATS-4                        
001132     MOVE WS-ADPLATS5            TO PICT-ADPLATS-5                        
001133*                                                                         
001134*ADGANG                                                                   
001135     MOVE 4006-ADGANG            TO PICT-ADGANG                           
001136     MOVE 1 TO LADGANG                                                    
001137     INSPECT PICT-ADGANG                                                  
001138       TALLYING LADGANG FOR LEADING SPACE                                 
001139     STRING PICT-ADGANG (LADGANG:) VBAR                                   
001140       DELIMITED BY SIZE                                                  
001141       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001142*                                                                         
001143*ADLAGOMR                                                                 
001144     MOVE 4006-ADLAGOMR-ORD      TO PICT-ADLAGOMR                         
001145     MOVE 1 TO LADLAGOMR                                                  
001146     INSPECT PICT-ADLAGOMR                                                
001147       TALLYING LADLAGOMR FOR LEADING SPACE                               
001148     STRING PICT-ADLAGOMR (LADLAGOMR:) VBAR                               
001149       DELIMITED BY SIZE                                                  
001150       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001151*                                                                         
001152*KDLCD3IV                                                                 
001153     MOVE 4006-IDARTNR           TO WS-IDARTNR                            
001154     MOVE WS-IDARTNR-LAST-3      TO PICT-KDLCD3IV                         
001155                                                                          
001156     MOVE 1 TO LKDLCD3IV                                                  
001157     INSPECT PICT-KDLCD3IV                                                
001158       TALLYING LKDLCD3IV FOR LEADING SPACE                               
001159     STRING PICT-KDLCD3IV (LKDLCD3IV:) VBAR                               
001160       DELIMITED BY SIZE                                                  
001161       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001162*                                                                         
001163*KVAVBART                                                                 
001164     MOVE 4006-KVAVBART          TO PICT-KVAVBART                         
001165     MOVE 1 TO LKVAVBART                                                  
001166     INSPECT PICT-KVAVBART                                                
001167       TALLYING LKVAVBART FOR LEADING SPACE                               
001168     STRING PICT-KVAVBART (LKVAVBART:) VBAR                               
001169       DELIMITED BY SIZE                                                  
001170       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001171*KDSORT                                                                   
001172     MOVE 4006-KDSORT            TO PICT-KDSORT                           
001173     MOVE ZERO TO LKDSORT                                                 
001174     INSPECT FUNCTION REVERSE(PICT-KDSORT)                                
001175          TALLYING LKDSORT FOR LEADING SPACE                              
001176     COMPUTE LKDSORT = LENGTH OF PICT-KDSORT - LKDSORT                    
001177                                                                          
001178     STRING PICT-KDSORT (1:LKDSORT) VBAR                                  
001179     DELIMITED BY SIZE                                                    
001180     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001181*TESORT                                                                   
001182     EVALUATE TRUE                                                        
001183     WHEN 4006-KDSORT = 'ST'                                              
001184       MOVE 'styck'              TO PICT-TESORT                           
001185     WHEN 4006-KDSORT = 'PA'                                              
001186       MOVE 'pair'              TO PICT-TESORT                            
001187     WHEN 4006-KDSORT = 'KG'                                              
001188       MOVE 'kilogram'           TO PICT-TESORT                           
001189     WHEN 4006-KDSORT = 'M ' OR ' M'                                      
001190       MOVE 'meter'              TO PICT-TESORT                           
001191     WHEN 4006-KDSORT = ' L' OR 'L '                                      
001192       MOVE 'liter'              TO PICT-TESORT                           
001193     WHEN 4006-KDSORT = 'SA'                                              
001194       MOVE 'sats'               TO PICT-TESORT                           
001195     WHEN 4006-KDSORT = 'MM'                                              
001196       MOVE 'millimeter'         TO PICT-TESORT                           
001197     WHEN 4006-KDSORT = ' G' OR 'G '                                      
001198       MOVE 'gram'               TO PICT-TESORT                           
001199     WHEN 4006-KDSORT = 'C2'                                              
001200       MOVE 'kvadratcentimeter'  TO PICT-TESORT                           
001201     WHEN 4006-KDSORT = 'M2'                                              
001202       MOVE 'kvadratmeter'       TO PICT-TESORT                           
001203     WHEN 4006-KDSORT = 'M3'                                              
001204       MOVE 'kubikmeter'         TO PICT-TESORT                           
001205     WHEN 4006-KDSORT = 'ML'                                              
001206       MOVE 'milliliter'         TO PICT-TESORT                           
001207     WHEN 4006-KDSORT = 'SW'                                              
001208       MOVE 'softwär'            TO PICT-TESORT                           
001209     WHEN 4006-KDSORT = 'TM'                                              
001210       MOVE 'tejlor mejd offers' TO PICT-TESORT                           
001211     WHEN 4006-KDSORT = 'HW'                                              
001212       MOVE 'hardwär'            TO PICT-TESORT                           
001213     WHEN OTHER                                                           
001214       MOVE 'enhet saknas'       TO PICT-TESORT                           
001215     END-EVALUATE                                                         
001216*                                                                         
001217     MOVE ZERO TO LTESORT                                                 
001218     INSPECT FUNCTION REVERSE(PICT-TESORT)                                
001219          TALLYING LTESORT FOR LEADING SPACE                              
001220     COMPUTE LTESORT = LENGTH OF PICT-TESORT - LTESORT                    
001221                                                                          
001222     STRING PICT-TESORT (1:LTESORT) VBAR                                  
001223     DELIMITED BY SIZE                                                    
001224     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001225*IDARTNR                                                                  
001226     MOVE 4006-IDARTNR           TO PICT-IDARTNR                          
001227     MOVE 1 TO LIDARTNR                                                   
001228     INSPECT PICT-IDARTNR                                                 
001229       TALLYING LIDARTNR  FOR LEADING SPACE                               
001230     STRING PICT-IDARTNR  (LIDARTNR:) VBAR                                
001231       DELIMITED BY SIZE                                                  
001232       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
001233*BEART                                                                    
001234     MOVE 4006-BEART             TO PICT-BEART                            
001235     MOVE ZERO TO LBEART                                                  
001236     INSPECT FUNCTION REVERSE(PICT-BEART)                                 
001237          TALLYING LBEART   FOR LEADING SPACE                             
001238     COMPUTE LBEART = LENGTH OF PICT-BEART - LBEART                       
001239                                                                          
001240     STRING PICT-BEART (1:LBEART) VBAR                                    
001241     DELIMITED BY SIZE                                                    
001242     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001243*                                                                         
001244*KDARTURS-NUM & BEARTURS-ENG                                              
001245*    URSPRUNG-TEST MED DIST08 BORTTAGEN 130201 SE LOGGAR                  
001246*    BEARTURS SKALL ALLTID VARA PÅ ENGELSKA                               
001247*                                                                         
001248       MOVE 4006-KDARTURS        TO ARTU-KDARTURS                         
001249       IF ARTU-KDARTURS = SPACE                                           
001250*        -- FIX FOR INVALID DATA IN TEST                                  
001251           MOVE 'GB'             TO ARTU-KDARTURS                         
001252       END-IF                                                             
001253       MOVE 4006-IDDISTR         TO ARTU-IDDISTR                          
001254       MOVE MID-IDDC             TO ARTU-IDDC                             
001255       CALL W400ARTU USING ARTU-W400ARTU                                  
001256                                                                          
001257       MOVE ARTU-KDARTURS-NUM  TO WS-KDARTURS-NUM                         
001258       MOVE WS-KDARTURS-NUM    TO PICT-KDARTURS-NUM                       
001259       MOVE ARTU-BEARTURS-SVE  TO PICT-BEARTURS-SVE                       
001260*                                                                         
001261*KDARTURS-NUM                                                             
001262     MOVE ZERO TO LKDARTURS-NUM                                           
001263     INSPECT FUNCTION REVERSE(PICT-KDARTURS-NUM)                          
001264          TALLYING LKDARTURS-NUM   FOR LEADING SPACE                      
001265     COMPUTE LKDARTURS-NUM =                                              
001266             LENGTH OF PICT-KDARTURS-NUM - LKDARTURS-NUM                  
001267                                                                          
001268     STRING PICT-KDARTURS-NUM (1:LKDARTURS-NUM) VBAR                      
001269     DELIMITED BY SIZE                                                    
001270     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001271*                                                                         
001272*BEARTURS-SVE                                                             
001273     MOVE ZERO TO LBEARTURS-SVE                                           
001274     INSPECT FUNCTION REVERSE(PICT-BEARTURS-SVE)                          
001275          TALLYING LBEARTURS-SVE   FOR LEADING SPACE                      
001276     COMPUTE LBEARTURS-SVE =                                              
001277             LENGTH OF PICT-BEARTURS-SVE - LBEARTURS-SVE                  
001278                                                                          
001279     IF LBEARTURS-SVE = ZERO                                              
001280       MOVE 1           TO LBEARTURS-SVE                                  
001281     END-IF                                                               
001282                                                                          
001283     STRING PICT-BEARTURS-SVE (1:LBEARTURS-SVE)                           
001284     DELIMITED BY SIZE                                                    
001285     INTO SEND-AREA WITH POINTER SEND-KVDLEN                              
001286                                                                          
001287*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
001288*    -- ADJUST TO CORRECT LENGTH                                          
001289     SUBTRACT 1 FROM SEND-KVDLEN                                          
001290     .                                                                    
001291                                                                          
001292 H-RENSA-PLOCKSATS SECTION.                                               
001293     MOVE 'H-RENSA-PLOCKSATS  '   TO WS-CURRENT-SECTION                   
001294                                                                          
001295     MOVE MID-IDPRODNR        TO W-4003-IDPRODNR                          
001296     MOVE MID-IDPLKLST        TO W-4003-IDPLKLST                          
001297                                                                          
001298     PERFORM IMS-GHU-4003-WL400301                                        
001299     IF SEGMENT-FINNS                                                     
001300*      RENSA ETIKETT H-TYP DATA.                                          
001301       PERFORM IMS-DLET-4003-WL400301                                     
001302     END-IF                                                               
001303                                                                          
001304     .                                                                    
001305                                                                          
001306 I-SKICKA-IMSTRANS SECTION.                                               
001307     MOVE 'STA I-SKICKA-IMSTRANS'   TO WS-CURRENT-SECTION                 
001308                                                                          
001309     MOVE MID-IDPRODNR       TO PTOP1-MID-IDPRODNR                        
001310     MOVE MID-IDPLKLST       TO PTOP1-MID-IDPLKLST                        
001311     MOVE MID-IDPRC          TO PTOP1-MID-IDPRC                           
001312     MOVE MID-IDLOPNR        TO PTOP1-MID-IDLOPNR                         
001313*                                                                         
001314     PERFORM IMS-ISRT-MSG-ALT1                                            
001315     .                                                                    
001316                                                                          
001317 S01-LAES-ORDERHUVUD SECTION.                                             
001318     MOVE 'S01-LAES-ORDERHUVUD'   TO WS-CURRENT-SECTION                   
001319                                                                          
001320     MOVE 4010-IDORDER     TO W-IDORDER                                   
001321     MOVE 4010-IDDC        TO W-IDDC                                      
001322     PERFORM IMS-07-GHU-WDQ201-12                                         
001323     MOVE OHUV-IDDISTR     TO TEST-IDDISTR                                
001324     MOVE OHUV-IDKUNDNR    TO WS-OHUV-IDKUNDNR                            
001325     MOVE OHUV-KDORDKL     TO WS-OHUV-KDORDKL                             
001326     .                                                                    
001327                                                                          
001328 S90-OPEN-SEND-RESPASSIGNMENT         SECTION.                            
001329     MOVE 'S90-OPEN-SEND' TO WS-CURRENT-SECTION                           
001330                                                                          
001331     MOVE 'OPEN'                         TO SEND-KDFUNC                   
001332     MOVE 'CARPARTS.3IV2.RESPASSIGNMENT' TO SEND-ADDISPABS                
001333     MOVE 4008-ADDISPXTRA                TO SEND-ADDISPXTRA               
001334                                                                          
001335     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001336                         SEND-OPEN-AREA                                   
001337                                                                          
001338     IF SEND-KDRC > 0                                                     
001339       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
001340       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
001341       DELIMITED BY SIZE INTO ERROR-TEXT                                  
001342       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001343     END-IF                                                               
001344     .                                                                    
001345                                                                          
001346 S02-SEND-RESPONSE SECTION.                                               
001347     MOVE 'S02-SEND-RESPONSE ' TO WS-CURRENT-SECTION                      
001348                                                                          
001349     MOVE 'PUT'                     TO SEND-KDFUNC                        
001350     CALL WZ01SEND USING  SEND-CONTROL-AREA                               
001351                          SEND-KVDLEN                                     
001352                          SEND-AREA                                       
001353     IF SEND-KDRC > 0                                                     
001354       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
001355       STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                      
001356       DELIMITED BY SIZE INTO ERROR-TEXT                                  
001357       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001358     END-IF                                                               
001359     .                                                                    
001360 S93-SEND-RESPONSE      SECTION.                                          
001361     MOVE 'S93-SEND-RESPONSE'  TO WS-CURRENT-SECTION                      
001362                                                                          
001363     MOVE 'PUT'                           TO SEND-KDFUNC                  
001364     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001365                         SEND-KVDLEN                                      
001366                         SEND-AREA                                        
001367     IF SEND-KDRC > ZERO                                                  
001368       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
001369       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
001370       DELIMITED BY SIZE INTO ERROR-TEXT                                  
001371       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001372     END-IF                                                               
001373     .                                                                    
001374                                                                          
001375 S94-CLOSE-SEND         SECTION.                                          
001376     MOVE 'S94-CLOSE-SEND' TO WS-CURRENT-SECTION                          
001377                                                                          
001378     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
001379     CALL WZ01SEND USING SEND-CONTROL-AREA                                
001380                                                                          
001381     IF SEND-KDRC > 0                                                     
001382       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
001383       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
001384       DELIMITED BY SIZE INTO ERROR-TEXT                                  
001385       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
001386     END-IF                                                               
001387     .                                                                    
001388                                                                          
001389* --- IMS SEKTIONER ---                                                   
001390 IMS-GU-MSG SECTION.                                                      
001391     MOVE 'IMS-GU-MSG'         TO WS-CURRENT-IMS-SECTION                  
001392                                                                          
001393     MOVE '  QC' TO GODK-STATUSKODER                                      
001394     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001395     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001396     PERFORM IMS-STATUSKONTROLL                                           
001397     .                                                                    
001398                                                                          
001399 IMS-ISRT-MSG-ALT1 SECTION.                                               
001400     MOVE 'IMS-ISRT-MSG-ALT1'  TO WS-CURRENT-IMS-SECTION                  
001401                                                                          
001402     MOVE SPACE TO GODK-STATUSKODER                                       
001403     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
001404     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
001405     PERFORM IMS-STATUSKONTROLL                                           
001406     .                                                                    
001407                                                                          
001408 IMS-01-GHU-WDGX4004  SECTION.                                            
001409     MOVE 'IMS-01'  TO WS-CURRENT-IMS-SECTION                             
001410                                                                          
001411     STRING 'WL400301(WDGXKEY  =' W-4003-IDHTYP-X ')'                     
001412          DELIMITED BY SIZE INTO SSA1                                     
001413     MOVE 'WL400311 '         TO SSA2                                     
001414     MOVE '  GE'            TO GODK-STATUSKODER                           
001415     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-WDGX4004 SSA1 SSA2            
001416     MOVE 4003-STATUS-CODE    TO STATUS-WS                                
001417     PERFORM IMS-STATUSKONTROLL                                           
001418     .                                                                    
001419                                                                          
001420*IMS-02-GNP-WDGX4006-KVAL SECTION.                                        
001421*    MOVE 'IMS-02'  TO WS-CURRENT-IMS-SECTION                             
001422*                                                                         
001423*    STRING 'WL400321(WDGXKEY  =' W-4006-IDHTYP-X ')'                     
001424*         DELIMITED BY SIZE INTO SSA1                                     
001425*    MOVE '  '              TO GODK-STATUSKODER                           
001426*    CALL CBLTDLI USING GNP 4003-PCB DLI-IO-WDGX4006 SSA1                 
001427*    MOVE 4003-STATUS-CODE    TO STATUS-WS                                
001428*    PERFORM IMS-STATUSKONTROLL                                           
001429*    .                                                                    
001430                                                                          
001431 IMS-03-GNP-WDGX4006-OKVAL SECTION.                                       
001432     MOVE 'IMS-03'  TO WS-CURRENT-IMS-SECTION                             
001433                                                                          
001434     STRING 'WL400321(KDPRT    <' W-4006-MAX-KDPRT ')'                    
001435          DELIMITED BY SIZE INTO SSA1                                     
001436     MOVE '  GBGE' TO GODK-STATUSKODER                                    
001437     CALL CBLTDLI USING GNP 4003-PCB DLI-IO-WDGX4006 SSA1                 
001438     MOVE 4003-STATUS-CODE    TO STATUS-WS                                
001439     MOVE 4003-STATUS-CODE    TO STATUS-WDGX4006-WS                       
001440     PERFORM IMS-STATUSKONTROLL                                           
001441     .                                                                    
001442                                                                          
001443 IMS-04-GU-WDGX4008   SECTION.                                            
001444     MOVE 'IMS-04'    TO WS-CURRENT-IMS-SECTION                           
001445                                                                          
001446     STRING 'WL400701(WDGXKEY  =' W-4007-IDHTYP-X ')'                     
001447          DELIMITED BY SIZE INTO SSA1                                     
001448     MOVE 'WL400711 '         TO SSA2                                     
001449     MOVE '  GE'              TO GODK-STATUSKODER                         
001450     CALL CBLTDLI USING GU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2             
001451     MOVE 4007-STATUS-CODE    TO STATUS-WS STATUS-WDGX4007-WS             
001452     PERFORM IMS-STATUSKONTROLL                                           
001453     .                                                                    
001454                                                                          
001455*IMS-05-GNP-WDGX4010-KVAL SECTION.                                        
001456*    MOVE 'IMS-05'    TO WS-CURRENT-IMS-SECTION                           
001457*                                                                         
001458*    STRING 'WL400721(KY4010   =' W-4010-IDHTYP-X ')'                     
001459*         DELIMITED BY SIZE INTO SSA1                                     
001460*    MOVE '    '              TO GODK-STATUSKODER                         
001461*    CALL CBLTDLI USING GNP 4007-PCB DLI-IO-WDGX4010 SSA1                 
001462*    MOVE 4007-STATUS-CODE    TO STATUS-WS                                
001463*    PERFORM IMS-STATUSKONTROLL                                           
001464*    .                                                                    
001465                                                                          
001466 IMS-06-GNP-WDGX4010-OKVAL SECTION.                                       
001467     MOVE 'IMS-06'    TO WS-CURRENT-IMS-SECTION                           
001468                                                                          
001469     STRING 'WL400721(KDPRT    <' W-4010-KDPRT-MAX-X ')'                  
001470          DELIMITED BY SIZE INTO SSA1                                     
001471     MOVE '  GE'              TO GODK-STATUSKODER                         
001472     CALL CBLTDLI USING GNP 4007-PCB DLI-IO-WDGX4010 SSA1                 
001473     MOVE 4007-STATUS-CODE    TO STATUS-WS                                
001474     MOVE 4007-STATUS-CODE    TO STATUS-WDGX4010-WS                       
001475     PERFORM IMS-STATUSKONTROLL                                           
001476     .                                                                    
001477                                                                          
001478 IMS-07-GHU-WDQ201-12 SECTION.                                            
001479     MOVE 'IMS-07' TO WS-CURRENT-IMS-SECTION                              
001480                                                                          
001481     STRING 'WDQ201  *D(IDORDER  =' W-IDORDER-X ')'                       
001482          DELIMITED BY SIZE INTO SSA1                                     
001483     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
001484          DELIMITED BY SIZE INTO SSA2                                     
001485     MOVE '  GE' TO GODK-STATUSKODER                                      
001486     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-WDQ201-12 SSA1 SSA2           
001487     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
001488     PERFORM IMS-STATUSKONTROLL                                           
001489     .                                                                    
001490                                                                          
001491 IMS-08-GU-WDQ301   SECTION.                                              
001492     MOVE 'IMS-08' TO WS-CURRENT-IMS-SECTION                              
001493                                                                          
001494     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
001495                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
001496          DELIMITED BY SIZE INTO SSA1                                     
001497     MOVE '  '                TO GODK-STATUSKODER                         
001498     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
001499     MOVE WDQ3-STATUS-CODE    TO STATUS-WS                                
001500     PERFORM IMS-STATUSKONTROLL                                           
001501     .                                                                    
001502                                                                          
001503 IMS-09-GU-WDGX4536  SECTION.                                             
001504     MOVE 'IMS-09' TO WS-CURRENT-IMS-SECTION                              
001505                                                                          
001506     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
001507          DELIMITED BY SIZE INTO SSA1                                     
001508     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
001509          DELIMITED BY SIZE INTO SSA2                                     
001510     MOVE '  GE'              TO GODK-STATUSKODER                         
001511     CALL CBLTDLI USING GU 4535-PCB DLI-IO-WDGX4536 SSA1 SSA2             
001512     MOVE 4535-STATUS-CODE    TO STATUS-WS                                
001513     PERFORM IMS-STATUSKONTROLL                                           
001514     .                                                                    
001515                                                                          
001516                                                                          
001517*IMS-10-GHU-WDGX4008  SECTION.                                            
001518*    MOVE 'IMS-10' TO WS-CURRENT-IMS-SECTION                              
001519*                                                                         
001520*    STRING 'WL400701(WDGXKEY  =' W-4007-IDHTYP-X ')'                     
001521*         DELIMITED BY SIZE INTO SSA1                                     
001522*    MOVE 'WL400711 '         TO SSA2                                     
001523*    MOVE '    '              TO GODK-STATUSKODER                         
001524*    CALL CBLTDLI USING GHU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2            
001525*    MOVE 4007-STATUS-CODE    TO STATUS-WS                                
001526*    PERFORM IMS-STATUSKONTROLL                                           
001527*    .                                                                    
001528                                                                          
001529 IMS-GHU-4003-WL400301 SECTION.                                           
001530     MOVE 'IMS-GHU-4003' TO WS-CURRENT-IMS-SECTION                        
001531                                                                          
001532     STRING 'WL400301(WDGXKEY  =' W-4003-IDHTYP-X ')'                     
001533          DELIMITED BY SIZE INTO SSA1                                     
001534     MOVE '  GE' TO GODK-STATUSKODER                                      
001535     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-WDGX4003 SSA1                 
001536     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
001537     PERFORM IMS-STATUSKONTROLL                                           
001538     .                                                                    
001539                                                                          
001540 IMS-DLET-4003-WL400301 SECTION.                                          
001541     MOVE 'IMS-DLET-4003' TO WS-CURRENT-IMS-SECTION                       
001542                                                                          
001543     MOVE '    ' TO GODK-STATUSKODER                                      
001544     CALL CBLTDLI USING DLET 4003-PCB DLI-IO-WDGX4003                     
001545     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
001546     PERFORM IMS-STATUSKONTROLL                                           
001547     .                                                                    
001548                                                                          
001549 IMS-GU-WDB601    SECTION.                                                
001550                                                                          
001551     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001552          DELIMITED BY SIZE INTO SSA1                                     
001553     MOVE '  GE' TO GODK-STATUSKODER                                      
001554     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601    SSA1                 
001555     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001556     PERFORM IMS-STATUSKONTROLL                                           
001557     .                                                                    
001558                                                                          
001559 IMS-STATUSKONTROLL SECTION.                                              
001560                                                                          
001561     SET STATUS-IX TO 1                                                   
001562     SEARCH GODK-STATUS                                                   
001563       AT END CALL FELLOG                                                 
001564       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
001565     END-SEARCH                                                           
001566     .                                                                    
