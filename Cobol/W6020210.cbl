000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W6020210.                                                
000004*AUTHOR.         INGER NILSSON / RAHUL REDDY.                             
000005*DATE-WRITTEN.   91/07/22 / JUNE 2012.                                    
000006                                                                          
000007**   REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        KONTROLLRAPPORT REGISTRERA FEL.                                  
000011*                                                                         
000012*        PROGRAMMET LÄSER     WLLEVA (WDF1)                               
000013*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
000014*        PROGRAMMET LÄSER             WDK7                                
000015*        PROGRAMMET LÄSER     WLINLE (WDL2)                               
000016*        PROGRAMMET LÄSER             WDL6                                
000017*        PROGRAMMET LÄSER     WLXXLA (WDG7)                               
000018*        PROGRAMMET LÄSER     WDP3                                        
000019*                                                                         
000020*    INDATA.                                                              
000021*        TRANSAKTION: W6T202                                              
000022*        REQU       :W60202I1                                             
000023*                                                                         
000024*    UTDATA.                                                              
000025*        RESP       : W60202O1                                            
000026*                                                                         
000027*    CHANGE LOG:                                                          
000028*      13/11/13 - REDDY RAHUL     - IR CORRECTIONS                        
000029*                                   INITIALIZE NEW FIELDS OF W6H7         
000030*                                   SCR 3235165                           
000031                                                                          
000032     SKIP3                                                                
000033 ENVIRONMENT DIVISION.                                                    
000034     EJECT                                                                
000035 DATA DIVISION.                                                           
000036 WORKING-STORAGE SECTION.                                                 
000037                                                                          
000038*    -- CHECKED BY WY2000                                                 
000039 77  IDPGM                       PIC X(08)   VALUE 'W6020210'.            
000040 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000041                                                                          
000042*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000043 77  W-BENAEMN                   PIC  X(25).                              
000044 77  W-KVKRBEH                   PIC  9(02)V9      VALUE 0.               
000045 77  ADM-KR                      PIC  X     VALUE 'K'.                    
000046 77  WS-KDKRSTA                  PIC  X     VALUE SPACE.                  
000047 77  WS-KVANTMOT                 PIC S9(07) COMP-3 VALUE +0.              
000048 77  WS-KVART-AAVV               PIC S9(07) COMP-3 VALUE +0.              
000049 77  WS-KVART-KONTR              PIC S9(07) COMP-3 VALUE +0.              
000050 77  WS-KVART-EJ-GODK            PIC S9(07) COMP-3 VALUE +0.              
000051 77  WS-KVART-RET                PIC S9(07) COMP-3 VALUE +0.              
000052 77  WS-KVART-SKROT              PIC S9(07) COMP-3 VALUE +0.              
000053 77  WS-KVART-SKROT-LDC          PIC S9(07) COMP-3 VALUE +0.              
000054 77  WS-KVART-KJUST              PIC S9(07) COMP-3 VALUE +0.              
000055 77  WS-KVART-BEH                PIC S9(07) COMP-3 VALUE +0.              
000056 77  WS-KVART-SJUST              PIC S9(07) COMP-3 VALUE +0.              
000057 77  WS-KVAVIS                   PIC S9(07) COMP-3 VALUE +0.              
000058 77  WS-KVLS                     PIC S9(07) COMP-3 VALUE +0.              
000059 77  WS-DAAVSDAT                 PIC  9(08).                              
000060 77  WS-KDKRJUST                 PIC  X(01)        VALUE SPACE.           
000061 77  WS-FLKLAR                   PIC  X(01)        VALUE SPACE.           
000062 77  WS-FLKVALSP                 PIC  X(01)        VALUE SPACE.           
000063 77  WS-FLBUFJUS                 PIC  X(01)        VALUE SPACE.           
000064 77  WS-KDDISP                   PIC  9(2)         VALUE ZERO.            
000065 77  WS-KDKRUTF                  PIC  X            VALUE SPACE.           
000066 77  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
000067 77  W-R40-ANTAL                 PIC 9(8)          VALUE ZERO.            
000068 01  WSS-IDKRFEL              PIC  X(02).                                 
000069 01  FILLER REDEFINES WSS-IDKRFEL.                                        
000070         05  WSS-IDKRFEL-1    PIC  X(01).                                 
000071         05  WSS-IDKRFEL-2    PIC  X(01).                                 
000072 01  FILLER.                                                              
000073     03  WS-IDKRFEL.                                                      
000074         05  WS-IDKRFEL-POS1     PIC  X(01).                              
000075         05  WS-IDKRFEL-POS2     PIC  X(01).                              
000076 01  W-MFSINF.                                                            
000077     03 FILLER                   PIC  X(30).                              
000078     03 W-MFSINF-IDKR            PIC  Z(05).                              
000079 77  DATUM                       PIC 9(06).                               
000080 77  JA                          PIC X       VALUE 'J'.                   
000081 77  YES                         PIC X       VALUE 'Y'.                   
000082 77  NEJ                         PIC X       VALUE 'N'.                   
000083 01  ALL-SPACE.                                                           
000084     03 FILLER                   PIC X(70)   VALUE SPACE.                 
000085 01  ALL-PLUS.                                                            
000086     03 FILLER                   PIC X(70)   VALUE ALL '+'.               
000087                                                                          
000088 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
000089*01  -COPY WTRAUTF8                                                       
000090                                                                          
000091 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
000092 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
000093 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
000094                                                                          
000095 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
000096 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
000097                                                                          
000098 01  ALL-UTF8-SPACE.                                                      
000099     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
000100 01  ALL-UTF8-PLUS.                                                       
000101     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
000102                                                                          
000103 77  W-SPAR-IDLEVNR              PIC  X(05)  VALUE SPACE.                 
000104*                                                                         
000105*   --- DISPOSITIONSKODSTEXTER                                            
000106 01  KDDISPTEXT-A1.                                                       
000107     03 DISPTEXT-A1.                                                      
000108       05 A1-TEXT1  PIC X(22) VALUE 'SHIPMENT WILL BE RETUR'.             
000109       05 A1-TEXT2  PIC X(22) VALUE 'NED.                  '.             
000110       05 A1-TEXT3  PIC X(22) VALUE '                      '.             
000111 01  KDDISPTEXT-A2.                                                       
000112     03 DISPTEXT-A2.                                                      
000113     05 A2-TEXT1  PIC X(22) VALUE 'DEFECTIVE PARTS WILL B'.               
000114     05 A2-TEXT2  PIC X(22) VALUE 'E SORTED OUT AT YOUR E'.               
000115     05 A2-TEXT3  PIC X(22) VALUE 'XPENSE.               '.               
000116 01  KDDISPTEXT-A3.                                                       
000117     03 DISPTEXT-A3.                                                      
000118     05 A3-TEXT1  PIC X(22) VALUE 'PARTS WILL BE SCRAPPED'.               
000119     05 A3-TEXT2  PIC X(22) VALUE ' AT YOUR EXPENSE.     '.               
000120     05 A3-TEXT3  PIC X(22) VALUE '                      '.               
000121 01  KDDISPTEXT-A4.                                                       
000122     03 DISPTEXT-A4.                                                      
000123     05 A4-TEXT1  PIC X(22) VALUE 'PARTNUMBER/QUANTITY WI'.               
000124     05 A4-TEXT2  PIC X(22) VALUE 'LL BE CHANGED AND OUR '.               
000125     05 A4-TEXT3  PIC X(22) VALUE 'STOCK BALANCE UPDATED.'.               
000126 01  KDDISPTEXT-A5.                                                       
000127     03 DISPTEXT-A5.                                                      
000128     05 A5-TEXT1  PIC X(22) VALUE 'YOU WILL BE INFORMED A'.               
000129     05 A5-TEXT2  PIC X(22) VALUE 'S SOON AS POSSIBLE    '.               
000130     05 A5-TEXT3  PIC X(22) VALUE '                      '.               
000131 01  KDDISPTEXT-A6.                                                       
000132     03 DISPTEXT-A6.                                                      
000133     05 A6-TEXT1  PIC X(22) VALUE 'DELIVERY DOCUMENTATION'.               
000134     05 A6-TEXT2  PIC X(22) VALUE ' IS NOT ACC. TO VOLVO '.               
000135     05 A6-TEXT3  PIC X(22) VALUE 'SPECIFICATION         '.               
000136 01  KDDISPTEXT-T1.                                                       
000137     03 DISPTEXT-T1.                                                      
000138     05 T1-TEXT1  PIC X(22) VALUE 'SHIPMENT WILL BE RETUR'.               
000139     05 T1-TEXT2  PIC X(22) VALUE 'NED.                  '.               
000140     05 T1-TEXT3  PIC X(22) VALUE '                      '.               
000141 01  KDDISPTEXT-T2.                                                       
000142     03 DISPTEXT-T2.                                                      
000143     05 T2-TEXT1  PIC X(22) VALUE 'DEFECTIVE PARTS WILL B'.               
000144     05 T2-TEXT2  PIC X(22) VALUE 'E SORTED OUT AT YOUR E'.               
000145     05 T2-TEXT3  PIC X(22) VALUE 'XPENSE.               '.               
000146 01  KDDISPTEXT-T3.                                                       
000147     03 DISPTEXT-T3.                                                      
000148     05 T3-TEXT1  PIC X(22) VALUE 'DEFECTIVE PARTS WILL B'.               
000149     05 T3-TEXT2  PIC X(22) VALUE 'E ADJUSTED AT YOUR EXP'.               
000150     05 T3-TEXT3  PIC X(22) VALUE 'ENSE                  '.               
000151 01  KDDISPTEXT-T4.                                                       
000152     03 DISPTEXT-T4.                                                      
000153     05 T4-TEXT1  PIC X(22) VALUE 'PARTS WILL BE TEMPORAR'.               
000154     05 T4-TEXT2  PIC X(22) VALUE 'ILY USED UNDER DEVIATI'.               
000155     05 T4-TEXT3  PIC X(22) VALUE 'ON,EXTENDED PROC.INSP.'.               
000156 01  KDDISPTEXT-T5.                                                       
000157     03 DISPTEXT-T5.                                                      
000158     05 T5-TEXT1  PIC X(22) VALUE 'PARTS WILL BE SCRAPPED'.               
000159     05 T5-TEXT2  PIC X(22) VALUE ' AT YOUR EXPENSE.     '.               
000160     05 T5-TEXT3  PIC X(22) VALUE '                      '.               
000161 01  KDDISPTEXT-T6.                                                       
000162     03 DISPTEXT-T6.                                                      
000163     05 T6-TEXT1  PIC X(22) VALUE 'YOU WILL BE INFORMED L'.               
000164     05 T6-TEXT2  PIC X(22) VALUE 'ATER.                 '.               
000165     05 T6-TEXT3  PIC X(22) VALUE '                      '.               
000166 01  KDDISPTEXT-T77.                                                      
000167     03 DISPTEXT-T77.                                                     
000168     05 T6-TEXT1  PIC X(22) VALUE 'MARKET RETURN AGREED W'.               
000169     05 T6-TEXT2  PIC X(22) VALUE 'ITH VOLVO CARS.       '.               
000170     05 T6-TEXT3  PIC X(22) VALUE '                      '.               
000171 01  KDDISPTEXT-T91.                                                      
000172     03 DISPTEXT-T91.                                                     
000173     05 T91-TEXT1  PIC X(22) VALUE 'AGREED UPON WITH VOLVO'.              
000174     05 T91-TEXT2  PIC X(22) VALUE '.                     '.              
000175     05 T91-TEXT3  PIC X(22) VALUE '                      '.              
000176*                                                                         
000177 01  KDHANDCOTXT-01.                                                      
000178     03 HANDLTXT-01.                                                      
000179     05 01-TEXT1  PIC X(28) VALUE 'HANDLING COSTS WILL BE DEBIT'.         
000180     05 01-TEXT2  PIC X(23) VALUE 'ED YOU AT A LATER DATE.'.              
000181 01  KDHANDCOTXT-02.                                                      
000182     03 HANDLTXT-02.                                                      
000183     05 02-TEXT1  PIC X(28) VALUE 'HANDLING COSTS WILL BE GIVEN'.         
000184     05 02-TEXT2  PIC X(23) VALUE ' IN THE REPORT.        '.              
000185*                                                                         
000186 01  W-IDFS                      PIC X(8).                                
000187 01  FILLER REDEFINES W-IDFS.                                             
000188     03 W-IDFS-TKN               PIC X     OCCURS 8.                      
000189 01  W-IDAVINR                   PIC 9(7).                                
000190 01  FILLER REDEFINES W-IDAVINR.                                          
000191     03 W-IDAVINR-TKN            PIC 9     OCCURS 7.                      
000192*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000193 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000194 77  IDFS-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
000195 77  IDAVINR-IX                  PIC S9(4)  VALUE +0    COMP SYNC.        
000196                                                                          
000197*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000198 77  WS-IDKR                     PIC 9(5)    VALUE ZERO.                  
000199                                                                          
000200 77  WS-IDSKYLT                  PIC X(2)    VALUE SPACE.                 
000201                                                                          
000202 77  W-NYUPPLAGG                 PIC X       VALUE 'N'.                   
000203                                                                          
000204 77  UPPD-KR-SW                  PIC X       VALUE 'N'.                   
000205     88  UPPD-KR                             VALUE 'J'.                   
000206                                                                          
000207 77  LEVADR-SAKNAS-SW            PIC X       VALUE 'N'.                   
000208     88  LEVADR-SAKNAS                       VALUE 'J'.                   
000209                                                                          
000210 77  PARTINR-FINNS-SW            PIC X       VALUE 'N'.                   
000211     88  PARTINR-FINNS                       VALUE 'J'.                   
000212                                                                          
000213 77  W-JUSTERA-KVANTMOT-SW       PIC X       VALUE 'J'.                   
000214     88  JUSTERA-KVANTMOT-FEL                VALUE 'N'.                   
000215                                                                          
000216 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000217     88  INDATA-OK                           VALUE 'J'.                   
000218     88  INDATA-FEL                          VALUE 'N'.                   
000219                                                                          
000220 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000221     88  NYCKLAR-OK                          VALUE 'J'.                   
000222     88  NYCKLAR-FEL                         VALUE 'N'.                   
000223                                                                          
000224 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000225     88  ALLT-OK                             VALUE 'J'.                   
000226                                                                          
000227 77  TRYCK-PF11-SW               PIC X       VALUE 'N'.                   
000228     88  TRYCK-PF11                          VALUE 'J'.                   
000229                                                                          
000230 77  ADM-KR-SW                   PIC X       VALUE 'N'.                   
000231     88  END-ADM-KR                          VALUE 'J'.                   
000232                                                                          
000233 77  KRFEL-RAD1-SW               PIC X       VALUE 'N'.                   
000234     88  KRFEL-RAD1-IFYLLD                   VALUE 'J'.                   
000235                                                                          
000236 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +1000 COMP.            
000237     EJECT                                                                
000238 01  TEST-IDLEVNR                PIC X(5).                                
000239*01  FILLER -COPY WWLEVHF -RED TEST-IDLEVNR.                              
000240     EJECT                                                                
000241 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
000242*01  FILLER -COPY WWBYT03 -RED TEST-IDARTNR.                              
000243     EJECT                                                                
000244*      --- VALID IDDC CODES                                               
000245*                                                                         
000246*01    -COPY WWDCKONS                                                     
000247*01    -COPY WWDC99                                                       
000248       EJECT                                                              
000249*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000250 01  GENERELLA-SUBPROGRAM.                                                
000251     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000252     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
000253     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000254     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000255     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000256     EJECT                                                                
000257 01  MESSAGE-CODES.                                                       
000258     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
000259     03  ERR-PARTINR-FINNS       PIC X(3)    VALUE '358'.                 
000260     03  ERR-LEVADR-SAKNAS       PIC X(3)    VALUE '041'.                 
000261     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
000262     03  ERR-INGET-ANDRAT        PIC X(3)    VALUE '004'.                 
000263     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
000264     03  INF-ARTIKEL-SAKNAS      PIC X(3)    VALUE '025'.                 
000265     03  INF-FARLIGT-GODS        PIC X(3)    VALUE '362'.                 
000266     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
000267     03  INF-UPPDAT-OTILLATET    PIC X(3)    VALUE '007'.                 
000268     03  INF-ANT-KR-FINNS        PIC X(3)    VALUE '351'.                 
000269     03  INF-KVAL-KR-FINNS       PIC X(3)    VALUE '352'.                 
000270     03  QUANT-TOO-BIG           PIC X(3)    VALUE '330'.                 
000271     EJECT                                                                
000272*01  -COPY WDECAREA                                                       
000273     EJECT                                                                
000274*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000275*                                                                         
000276 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000277     SKIP3                                                                
000278*01  -COPY WMFSAREA                                                       
000279     EJECT                                                                
000280*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000281*                                                                         
000282     EJECT                                                                
000283 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000284     SKIP3                                                                
000285 01  NYCKLAR-TILL-DLI.                                                    
000286     03  W-IDDC-X.                                                        
000287         05  W-IDDC              PIC X(02)    VALUE SPACE.                
000288     03  W-IDKR-X.                                                        
000289         05  W-IDKR              PIC 9(05)    VALUE ZERO.                 
000290     03  W-IDLOPNRM-X.                                                    
000291         05  W-IDLOPNRM          PIC S9(09)   VALUE ZERO COMP-3.          
000292     03  W-W6H7CSEQ-X.                                                    
000293         05  W-IDLOPNRM-CSEQ     PIC S9(09)   VALUE ZERO COMP-3.          
000294         05  W-DAAVSDAT-CSEQ     PIC  9(08)   VALUE ZERO.                 
000295     03  W-IDLEVNR-X.                                                     
000296         05  W-IDLEVNR           PIC  X(05)   VALUE SPACE.                
000297     03  W-IDLEVG-X.                                                      
000298         05  W-IDLEVG            PIC S9(05)   VALUE ZERO COMP-3.          
000299     03  W-IDARTNR-X.                                                     
000300         05  W-IDARTNR           PIC S9(09)   VALUE ZERO COMP-3.          
000301     03  W-KDSEGKEY-X.                                                    
000302         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
000303     03  W-IDSKYLT-X.                                                     
000304         05  W-IDSKYLT           PIC X(03)    VALUE SPACE.                
000305     03  W-W6GX-6001-KEY-X.                                               
000306         05  W-IDHTYP-6001       PIC X(04)    VALUE '6001'.               
000307         05  W-FILLER            PIC X(26)    VALUE LOW-VALUE.            
000308     03  W-WDGX-4825-KEY-X.                                               
000309         05  W-IDHTYP-4825       PIC X(04)    VALUE '4825'.               
000310         05  W-IDSKYLT-4825      PIC X(03)    VALUE 'GB '.                
000311         05  W-FILLER            PIC X(23)    VALUE LOW-VALUE.            
000312     03  W-WDGX-4826-KEY-X.                                               
000313         05  W-IDKRFEL           PIC X(02)    VALUE SPACE.                
000314         05  W-FILLER            PIC X(08)    VALUE LOW-VALUE.            
000315     03  W-KDARBTYP-X.                                                    
000316         05  W-KDARBTYP          PIC X(8)     VALUE 'QUAL    '.           
000317     03  W-IDPERSON-X.                                                    
000318         05  W-IDPERSON          PIC S9(3)    VALUE +0 COMP-3.            
000319     03  W-W6D101KY-X.                                                    
000320         05  W-W6D101-IDDC       PIC X(2)    VALUE SPACE.                 
000321         05  W-W6D101-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
000322         05  W-W6D101-IDFS       PIC X(8)    VALUE SPACE.                 
000323         05  W-W6D101-TIAVIDAT   PIC S9(7)   VALUE +0 COMP-3.             
000324     03  W-IDRADNRI-X.                                                    
000325         05  W-IDRADNRI          PIC S9(5)   VALUE +0 COMP-3.             
000326     SKIP2                                                                
000327*    --- STATUS-KOD FRÅN IMS                                              
000328 01  STATUS-WS                   PIC XX.                                  
000329     88  SEGMENT-FINNS                       VALUE '  '.                  
000330     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000331     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000332     SKIP2                                                                
000333 01  GODK-STATUSKODER.                                                    
000334     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000335     SKIP3                                                                
000336 01  SSA1                        PIC X(128).                              
000337 01  SSA2                        PIC X(64).                               
000338 01  SSA3                        PIC X(64).                               
000339     EJECT                                                                
000340*    --- IMS FUNKTIONSKODER                                               
000341*01  -COPY W0003                                                          
000342     EJECT                                                                
000343*    ---  DLI INPUT-OUTPUT AREA                                           
000344 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
000345     SKIP3                                                                
000346 01  DLI-IO-AREA1.                                                        
000347     03  IO-AREA1                PIC X(500)  VALUE SPACE.                 
000348     SKIP3                                                                
000349     03  W6H701   REDEFINES IO-AREA1.                                     
000350*        05  -COPY W6H701                                                 
000351     EJECT                                                                
000352     03  W6H713   REDEFINES IO-AREA1.                                     
000353*        05  -COPY W6H713                                                 
000354     EJECT                                                                
000355 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
000356     SKIP3                                                                
000357 01  DLI-IO-AREA2.                                                        
000358     03  IO-AREA2                PIC X(1321)  VALUE SPACE.                
000359     SKIP3                                                                
000360     03  W6H714   REDEFINES IO-AREA2.                                     
000361*        05  -COPY W6H714                                                 
000362     EJECT                                                                
000363 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
000364     SKIP3                                                                
000365 01  DLI-IO-AREA3.                                                        
000366     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
000367     03  WLLEVA01 REDEFINES IO-AREA3.                                     
000368*        05  -COPY WDF101                                                 
000369     SKIP3                                                                
000370     03  WLLEVA14 REDEFINES IO-AREA3.                                     
000371*        05  -COPY WDF106                                                 
000372     EJECT                                                                
000373 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
000374     SKIP3                                                                
000375 01  DLI-IO-AREA4.                                                        
000376     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
000377     03  WLARTC01 REDEFINES IO-AREA4.                                     
000378*        05  -COPY WDK601   -PRE ARTC-                                    
000379     EJECT                                                                
000380     03  WLARTC11 REDEFINES IO-AREA4.                                     
000381*        05  -COPY WDK611   -PRE ARTC-                                    
000382     EJECT                                                                
000383 01  DLI-IO-WDK711.                                                       
000384*    03  -COPY WDK711                                                     
000385     EJECT                                                                
000386 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
000387     SKIP3                                                                
000388 01  DLI-IO-AREA5.                                                        
000389     03  IO-AREA5                PIC X(100) VALUE SPACE.                  
000390     03  WLXXLA11 REDEFINES IO-AREA5.                                     
000391*        05  -COPY WDGX4826                                               
000392     EJECT                                                                
000393 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
000394     SKIP3                                                                
000395 01  DLI-IO-P311.                                                         
000396*    05  -COPY WDP311                                                     
000397     EJECT                                                                
000398 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
000399     SKIP3                                                                
000400 01  DLI-IO-AREA7.                                                        
000401     03  IO-AREA7                PIC X(120) VALUE SPACE.                  
000402     03  WDD311   REDEFINES IO-AREA7.                                     
000403*        05  -COPY WDD311                                                 
000404     EJECT                                                                
000405 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
000406     SKIP3                                                                
000407 01  DLI-IO-AREA8.                                                        
000408     03  IO-AREA8                PIC X(110)  VALUE SPACE.                 
000409     03  W6G110   REDEFINES IO-AREA8.                                     
000410*        05  -COPY W6GX6002                                               
000411     EJECT                                                                
000412 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA10'.        
000413     SKIP3                                                                
000414 01  DLI-IO-AREA10.                                                       
000415     03  IO-AREA10               PIC X(102) VALUE SPACE.                  
000416     03  WLINLE21 REDEFINES IO-AREA10.                                    
000417*        05  -COPY WDL221   -PRE INLE-                                    
000418     EJECT                                                                
000419 01  DLI-IO-AREA11.                                                       
000420     03  IO-AREA11               PIC X(275) VALUE SPACE.                  
000421     03  W6LEVA01 REDEFINES IO-AREA11.                                    
000422*        05  -COPY W6F111                                                 
000423     EJECT                                                                
000424 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA12'.        
000425     SKIP3                                                                
000426 01  DLI-IO-AREA12.                                                       
000427     03  IO-AREA12               PIC X(150)  VALUE SPACE.                 
000428     03  W6INLA01 REDEFINES IO-AREA12.                                    
000429*        05  -COPY W6D101                                                 
000430     03  W6INLA11 REDEFINES IO-AREA12.                                    
000431*        05  -COPY W6D111                                                 
000432     EJECT                                                                
000433     03  W6INLC11 REDEFINES IO-AREA12.                                    
000434*        05  -COPY W6D1B1                                                 
000435     EJECT                                                                
000436 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA13'.        
000437     SKIP3                                                                
000438 01  DLI-IO-AREA13.                                                       
000439     03  IO-AREA13               PIC X(600)  VALUE SPACE.                 
000440     SKIP3                                                                
000441     03  W6H701   REDEFINES IO-AREA13.                                    
000442*        05  -COPY W6H701 -PRE CSEQ-                                      
000443     EJECT                                                                
000444 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA14'.        
000445     SKIP3                                                                
000446 01  DLI-IO-AREA14.                                                       
000447     03  IO-AREA14               PIC X(444)  VALUE SPACE.                 
000448     SKIP3                                                                
000449     03  W6L101   REDEFINES IO-AREA14.                                    
000450*        05  -COPY W6L101                                                 
000451     EJECT                                                                
000452 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA15'.        
000453     SKIP3                                                                
000454 01  DLI-IO-AREA15.                                                       
000455     03  WDB601.                                                          
000456*        05  -COPY WDB601                                                 
000457     EJECT                                                                
000458 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDL611'.        
000459     SKIP3                                                                
000460 01  DLI-IO-WDL611.                                                       
000461     03  WDL611.                                                          
000462*        05  -COPY WDL611                                                 
000463     EJECT                                                                
000464 LINKAGE SECTION.                                                         
000465                                                                          
000466 01  REQU-AREA.                                                           
000467*    03 -COPY WZ01REQU                                                    
000468*    03 -COPY W60202I1                                                    
000469     EJECT                                                                
000470 01  RESP-AREA.                                                           
000471*    03 -COPY WZ01RESP                                                    
000472*    03 -COPY W60202O1                                                    
000473     EJECT                                                                
000474*01  -COPY W0008  -PRE KVAE-                                              
000475     05  FILLER                  PIC X.                                   
000476     EJECT                                                                
000477*01  -COPY W0008  -PRE KVAI-                                              
000478     05  FILLER                  PIC X.                                   
000479     EJECT                                                                
000480*01  -COPY W0008  -PRE INLC-                                              
000481     05  FILLER                  PIC X.                                   
000482     EJECT                                                                
000483*01  -COPY W0008  -PRE BENA-                                              
000484     05  FILLER                  PIC X.                                   
000485     EJECT                                                                
000486*01  -COPY W0008  -PRE LEVA-                                              
000487     05  FILLER                  PIC X.                                   
000488     EJECT                                                                
000489*01  -COPY W0008  -PRE ARTC-                                              
000490     05  FILLER                  PIC X.                                   
000491     EJECT                                                                
000492*01  -COPY W0008  -PRE XXLA-                                              
000493     05  FILLER                  PIC X.                                   
000494     EJECT                                                                
000495*01  -COPY W0008  -PRE WDP3-                                              
000496     05  FILLER                  PIC X.                                   
000497     EJECT                                                                
000498*01  -COPY W0008  -PRE LOPB-                                              
000499     05  FILLER                  PIC X.                                   
000500     EJECT                                                                
000501*01  -COPY W0008  -PRE INLE-                                              
000502     05  FILLER                  PIC X.                                   
000503     EJECT                                                                
000504*01  -COPY W0008  -PRE W6F1-                                              
000505     05  FILLER                  PIC X.                                   
000506     EJECT                                                                
000507*01  -COPY W0008  -PRE W6INLA-                                            
000508     05  FILLER                  PIC X.                                   
000509     EJECT                                                                
000510*01  -COPY W0008  -PRE UPFA-                                              
000511     05  FILLER                  PIC X.                                   
000512     EJECT                                                                
000513*01  -COPY W0008  -PRE WDB6-                                              
000514     05  FILLER                  PIC X.                                   
000515     EJECT                                                                
000516*01  -COPY W0008  -PRE WDK7-                                              
000517     05  FILLER                  PIC X.                                   
000518     EJECT                                                                
000519*01  -COPY W0008  -PRE WDL6-                                              
000520     05  FILLER                  PIC X.                                   
000521     EJECT                                                                
000522 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
000523                           KVAE-PCB KVAI-PCB INLC-PCB BENA-PCB            
000524                           LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB            
000525                           LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB          
000526                           UPFA-PCB WDB6-PCB WDK7-PCB WDL6-PCB.           
000527     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA                            
000528                           KVAE-PCB KVAI-PCB INLC-PCB BENA-PCB            
000529                           LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB            
000530                           LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB          
000531                           UPFA-PCB WDB6-PCB WDK7-PCB WDL6-PCB.           
000532                                                                          
000533     PERFORM A-INIT                                                       
000534     PERFORM B-KOLLA-NYCKLAR                                              
000535     IF NYCKLAR-OK                                                        
000536       IF REQU-UPDATE OR REQU-UPD-X OR REQU-UPD-V                         
000537         PERFORM C-KOLLA-INPUT                                            
000538         IF INDATA-OK                                                     
000539           PERFORM D-UPPDATERA                                            
000540         END-IF                                                           
000541       ELSE                                                               
000542         IF REQU-FIRST                                                    
000543           CONTINUE                                                       
000544         ELSE                                                             
000545           PERFORM E-SAMMA-SIDA                                           
000546         END-IF                                                           
000547       END-IF                                                             
000548       IF ALLT-OK                                                         
000549         PERFORM F-LAES-VISA-INFO                                         
000550       END-IF                                                             
000551     END-IF                                                               
000552     IF NOT REQU-UPD-X                                                    
000553       CONTINUE                                                           
000554     ELSE                                                                 
000555       IF NYCKLAR-OK AND INDATA-OK                                        
000556         CONTINUE                                                         
000557       ELSE                                                               
000558         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
000559       END-IF                                                             
000560     END-IF                                                               
000561                                                                          
000562     MOVE ZERO                   TO RETURN-CODE                           
000563     GOBACK                                                               
000564     .                                                                    
000565     EJECT                                                                
000566 A-INIT SECTION.                                                          
000567                                                                          
000568     MOVE ALL '+'                TO RESP-W60202O1                         
000569     PERFORM MFS-FORM-ATTR                                                
000570     MOVE 001                    TO RESP-IDMSGVER                         
000571     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
000572                                    RESP-IDMSG-INFO                       
000573                                    RESP-IDELMT-ERROR                     
000574                                                                          
000575     ACCEPT DATUM              FROM DATE                                  
000576                                                                          
000577     .                                                                    
000578     EJECT                                                                
000579 B-KOLLA-NYCKLAR SECTION.                                                 
000580                                                                          
000581     EVALUATE REQU-IDSPRAK                                                
000582       WHEN 'SV'                                                          
000583        MOVE WS-IDSKYLT-SE TO W-IDSKYLT-X WS-IDSKYLT                      
000584        MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                
000585                                                                          
000586       WHEN 'ZH'                                                          
000587        MOVE WS-IDSKYLT-CN TO W-IDSKYLT-X WS-IDSKYLT                      
000588        MOVE WS-CP-UNICODE TO TRAUTF8-KDCP                                
000589                                                                          
000590       WHEN OTHER                                                         
000591        MOVE WS-IDSKYLT-GB TO W-IDSKYLT-X WS-IDSKYLT                      
000592        MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                
000593     END-EVALUATE                                                         
000594                                                                          
000595*    -- KONTROLL AV IDKR                                                  
000596                                                                          
000597     MOVE REQU-IDKR-KEY          TO WS-IDKR                               
000598                                                                          
000599     IF WS-IDKR NUMERIC                                                   
000600       MOVE WS-IDKR              TO W-IDKR                                
000601     ELSE                                                                 
000602       MOVE NEJ                  TO NYCKLAR-SW                            
000603     END-IF                                                               
000604                                                                          
000605     IF NYCKLAR-OK                                                        
000606       MOVE WS-IDKR              TO RESP-IDKR-KEY                         
000607     END-IF                                                               
000608                                                                          
000609     IF NYCKLAR-FEL                                                       
000610       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
000611       MOVE 'IDKR'               TO RESP-IDELMT-ERROR                     
000612       PERFORM MFS-RENSA-FAELT-IN                                         
000613       PERFORM MFS-RENSA-FAELT-UT                                         
000614     END-IF                                                               
000615     .                                                                    
000616     EJECT                                                                
000617 C-KOLLA-INPUT SECTION.                                                   
000618     IF REQU-INPUT = ALL '+'                                              
000619       MOVE ERR-INGET-ANDRAT     TO RESP-IDMSG-INFO                       
000620       PERFORM MFS-ROER-EJ-FAELT-IN-UT                                    
000621       PERFORM IMS-GU-W6KVAE01                                            
000622       IF SEGMENT-FINNS                                                   
000623         PERFORM MFS-SPAERRA-FAELT                                        
000624         PERFORM MFS-SPAERRA-FAELT2                                       
000625       END-IF                                                             
000626       MOVE NEJ                  TO ALLT-SW                               
000627       MOVE NEJ                  TO INDATA-SW                             
000628     ELSE                                                                 
000629       IF REQU-IDLOPNRM-UPD NOT = ALL '+'                                 
000630         IF REQU-KVANTMOT-UPD = ALL '+' OR                                
000631            REQU-KDKRUTF-UPD = '4'                                        
000632           MOVE JA             TO W-NYUPPLAGG                             
000633         ELSE                                                             
000634           PERFORM CA-KONTROLL-MOTTAGET-ANTAL                             
000635         END-IF                                                           
000636       ELSE                                                               
000637         IF REQU-IDARTNR-UPD NOT = ALL '+' OR                             
000638            REQU-IDLEVNR-UPD NOT = ALL '+'                                
000639           MOVE JA               TO W-NYUPPLAGG                           
000640         END-IF                                                           
000641       END-IF                                                             
000642                                                                          
000643       IF INDATA-OK                                                       
000644         IF W-NYUPPLAGG = NEJ                                             
000645           PERFORM IMS-GU-W6KVAE01                                        
000646           IF SEGMENT-FINNS                                               
000647             PERFORM MFS-SPAERRA-FAELT2                                   
000648             MOVE KR-KVANTMOT    TO WS-KVANTMOT                           
000649             MOVE KR-KVART-AAVV  TO WS-KVART-AAVV                         
000650             MOVE KR-KVART-KONTR TO WS-KVART-KONTR                        
000651             MOVE KR-KVART-EJ-GODK                                        
000652                                 TO WS-KVART-EJ-GODK                      
000653             MOVE KR-KVART-RET   TO WS-KVART-RET                          
000654             MOVE KR-KVART-SKROT TO WS-KVART-SKROT                        
000655             MOVE KR-KVART-SKROT-LDC                                      
000656                                 TO WS-KVART-SKROT-LDC                    
000657             MOVE KR-FLKVALSP    TO WS-FLKVALSP                           
000658             MOVE KR-FLBUFJUS    TO WS-FLBUFJUS                           
000659             MOVE KR-KVART-KJUST TO WS-KVART-KJUST                        
000660             MOVE KR-KVART-BEH   TO WS-KVART-BEH                          
000661             MOVE KR-KVART-SJUST TO WS-KVART-SJUST                        
000662             MOVE KR-KVAVIS      TO WS-KVAVIS                             
000663             MOVE KR-IDLEVNR     TO W-IDLEVNR                             
000664             MOVE KR-IDLOPNRM    TO W-IDLOPNRM                            
000665             MOVE KR-IDKRFEL     TO WS-IDKRFEL                            
000666             MOVE KR-KDKRSTA     TO WS-KDKRSTA                            
000667             MOVE KR-KDDISP      TO WS-KDDISP                             
000668             MOVE KR-KDKRUTF     TO WS-KDKRUTF                            
000669           ELSE                                                           
000670             MOVE NEJ            TO INDATA-SW                             
000671             MOVE ERR-WRONG-KEY  TO RESP-IDMSG-ERROR                      
000672             MOVE 'IDKR'         TO RESP-IDELMT-ERROR                     
000673             PERFORM MFS-RENSA-FAELT-UT                                   
000674           END-IF                                                         
000675         END-IF                                                           
000676                                                                          
000677*    -- KONTROLL AV PARTINR                                               
000678         MOVE ALL-SPACE          TO RESP-IDLOPNRM-UT                      
000679         IF REQU-IDLOPNRM-UPD NOT = ALL '+'                               
000680           MOVE REQU-IDLOPNRM-UPD                                         
000681                                 TO W-IDLOPNRM                            
000682           PERFORM IMS-GU-W6INLC01                                        
000683           IF SEGMENT-SAKNAS                                              
000684             MOVE MFS-NUM-FAELT-FEL                                       
000685                                 TO RESP-IDLOPNRM-UPD-ATTR                
000686             MOVE 'LOPNR SAKNAS PÅ INLC'                                  
000687                                 TO FELTEXT                               
000688             MOVE NEJ            TO INDATA-SW                             
000689           ELSE                                                           
000690             MOVE SEQB-IDLEVNR   TO W-IDLEVNR                             
000691                                    W-W6D101-IDLEVNR                      
000692             MOVE SEQB-IDDC      TO W-W6D101-IDDC                         
000693             MOVE SEQB-IDFS      TO W-W6D101-IDFS                         
000694             MOVE SEQB-TIAVIDAT  TO W-W6D101-TIAVIDAT                     
000695             MOVE SEQB-IDRADNR-INL                                        
000696                                 TO W-IDRADNRI                            
000697             PERFORM IMS-GU-W6INLA11                                      
000698             MOVE ART-KVAVIS     TO WS-KVAVIS                             
000699             MOVE MFS-NUM-FAELT-RAETT                                     
000700                                 TO RESP-IDLOPNRM-UPD-ATTR                
000701           END-IF                                                         
000702         ELSE                                                             
000703*    -- KONTROLL AV IDARTNR IDLEVNR                                       
000704           MOVE ALL-SPACE        TO RESP-IDLEVNR-UT                       
000705           MOVE ALL-SPACE        TO RESP-IDARTNR-UT                       
000706           IF REQU-IDLEVNR-UPD = ALL '+' AND                              
000707              REQU-IDARTNR-UPD = ALL '+'                                  
000708             CONTINUE                                                     
000709           ELSE                                                           
000710              IF REQU-IDARTNR-UPD IS NUMERIC                              
000711                 CONTINUE                                                 
000712              ELSE                                                        
000713                 MOVE ZERO TO  REQU-IDARTNR-UPD                           
000714              END-IF                                                      
000715             IF (REQU-IDLEVNR-UPD = SPACE    OR                           
000716                 REQU-IDLEVNR-UPD = ALL '+') OR                           
000717                 (REQU-IDARTNR-UPD = +0       OR                          
000718                 REQU-IDARTNR-UPD = ALL '+')                              
000719               MOVE 'IDLEVNR OCH IDARTNR FEL'                             
000720                                 TO FELTEXT                               
000721               MOVE MFS-ALFA-FAELT-FEL                                    
000722                                 TO RESP-IDLEVNR-UPD-ATTR                 
000723               MOVE MFS-NUM-FAELT-FEL                                     
000724                                 TO RESP-IDARTNR-UPD-ATTR                 
000725               MOVE NEJ          TO INDATA-SW                             
000726             ELSE                                                         
000727               MOVE REQU-IDARTNR-UPD                                      
000728                                 TO W-IDARTNR                             
000729               MOVE REQU-IDLEVNR-UPD                                      
000730                                 TO W-IDLEVNR                             
000731                                    TEST-IDLEVNR                          
000732               IF IDLEVNR-HF                                              
000733                 MOVE 'IDLEVNR-HF '                                       
000734                                 TO FELTEXT                               
000735                 MOVE MFS-ALFA-FAELT-FEL                                  
000736                                 TO RESP-IDLEVNR-UPD-ATTR                 
000737                 MOVE NEJ        TO INDATA-SW                             
000738               ELSE                                                       
000739                 MOVE W-IDARTNR  TO TEST-IDARTNR                          
000740                 IF BYT03-OBJEKT                                          
000741                   MOVE NEJ      TO INDATA-SW                             
000742                   MOVE JA       TO UPPD-KR-SW                            
000743                 END-IF                                                   
000744                 IF INDATA-OK                                             
000745                   PERFORM IMS-GU-WLARTC01                                
000746                   IF SEGMENT-FINNS                                       
000747                     MOVE ARTC-ART-IDLEVNR                                
000748                                 TO W-SPAR-IDLEVNR                        
000749                   ELSE                                                   
000750                     MOVE INF-ARTIKEL-SAKNAS                              
000751                                 TO RESP-IDMSG-INFO                       
000752                     MOVE 'IDARTNR'                                       
000753                                 TO RESP-IDELMT-ERROR                     
000754                     MOVE NEJ    TO INDATA-SW                             
000755                   END-IF                                                 
000756                 END-IF                                                   
000757                 IF INDATA-OK                                             
000758* HÄR LIGGER KONTROLL ATT DET NÅGON GÅNG GJORTS EN INLEVERANS MED         
000759* DEN LEVERANTÖR SOM ÄR ANGIVEN I MID-EN. DET BEHÖVER ALLTSÅ              
000760* INTE VARA ARTIKELNS HUVUDLEVERANTÖR                                     
000761*                                                                         
000762                   MOVE REQU-IDDC-KEY TO W-IDDC                           
000763                                         WS-IDDC                          
000764                   IF NDC-CN OR NDC-US                                    
000765                     PERFORM IMS-GU-WDL611                                
000766                   ELSE                                                   
000767                     PERFORM IMS-GU-WLINLE21                              
000768                   END-IF                                                 
000769                                                                          
000770                   IF REQU-UPD-V                                          
000771* FÖR ATT KUNNA SKICKA GODS PÅ RENOVERING TILL ANNAN LEV ÄN               
000772* DEN SOM LEVERERAT GODSET - UPPDATERING MED PF23.                        
000773                     CONTINUE                                             
000774                   ELSE                                                   
000775                     IF SEGMENT-SAKNAS                                    
000776                       IF NDC-CN OR NDC-US                                
000777                         MOVE NEJ TO INDATA-SW                            
000778                       ELSE                                               
000779                         IF W-IDLEVNR NOT = W-SPAR-IDLEVNR                
000780                           MOVE NEJ TO INDATA-SW                          
000781                         END-IF                                           
000782                       END-IF                                             
000783                     END-IF                                               
000784                   END-IF                                                 
000785                   IF INDATA-OK                                           
000786                     MOVE MFS-ALFA-FAELT-RAETT                            
000787                                 TO RESP-IDLEVNR-UPD-ATTR                 
000788                     MOVE MFS-NUM-FAELT-RAETT                             
000789                                 TO RESP-IDARTNR-UPD-ATTR                 
000790                   ELSE                                                   
000791                     MOVE MFS-ALFA-FAELT-FEL                              
000792                                 TO RESP-IDLEVNR-UPD-ATTR                 
000793                     MOVE MFS-NUM-FAELT-FEL                               
000794                                 TO RESP-IDARTNR-UPD-ATTR                 
000795                     MOVE NEJ    TO INDATA-SW                             
000796                   END-IF                                                 
000797                 END-IF                                                   
000798               END-IF                                                     
000799             END-IF                                                       
000800           END-IF                                                         
000801         END-IF                                                           
000802                                                                          
000803*    -- KONTROLL AV LEVERANTÖRS KONTORSADRESS                             
000804         IF INDATA-OK AND                                                 
000805            W-NYUPPLAGG = JA                                              
000806           PERFORM IMS-GU-WLLEVA14                                        
000807           IF SEGMENT-SAKNAS                                              
000808             MOVE 'LEV SAKNAS PÅ WLLEVA14'                                
000809                                 TO FELTEXT                               
000810             MOVE NEJ            TO INDATA-SW                             
000811             MOVE JA             TO LEVADR-SAKNAS-SW                      
000812           END-IF                                                         
000813         END-IF                                                           
000814                                                                          
000815*    -- KONTROLL AV IDLEVG                                                
000816         MOVE ALL-SPACE          TO RESP-IDLEVG-UT                        
000817         IF REQU-IDLEVG-UPD NOT = ALL '+'                                 
000818           IF REQU-IDLEVG-UPD = ZERO                                      
000819             MOVE MFS-NUM-FAELT-RAETT                                     
000820                                 TO RESP-IDLEVG-UPD-ATTR                  
000821           ELSE                                                           
000822             MOVE REQU-IDLEVG-UPD                                         
000823                                 TO W-IDLEVG                              
000824             PERFORM IMS-GU-W6LEVA11                                      
000825             IF SEGMENT-SAKNAS                                            
000826               MOVE MFS-NUM-FAELT-FEL                                     
000827                                 TO RESP-IDLEVG-UPD-ATTR                  
000828               MOVE NEJ          TO INDATA-SW                             
000829             ELSE                                                         
000830               MOVE MFS-NUM-FAELT-RAETT                                   
000831                                 TO RESP-IDLEVG-UPD-ATTR                  
000832             END-IF                                                       
000833           END-IF                                                         
000834         END-IF                                                           
000835                                                                          
000836         IF INDATA-OK                                                     
000837           IF REQU-IDKRFEL-UPD NOT = ALL '+'                              
000838             MOVE JA             TO KRFEL-RAD1-SW                         
000839             IF REQU-IDKRFEL-UPD = 'PA' OR 'PB'                           
000840               MOVE MFS-ALFA-FAELT-FEL                                    
000841                                 TO RESP-IDKRFEL-UPD-ATTR                 
000842               MOVE NEJ          TO INDATA-SW                             
000843             ELSE                                                         
000844               MOVE REQU-IDKRFEL-UPD                                      
000845                                 TO W-IDKRFEL                             
000846               PERFORM IMS-GU-WLXXLA11                                    
000847               IF SEGMENT-SAKNAS                                          
000848                 MOVE MFS-ALFA-FAELT-FEL                                  
000849                                 TO RESP-IDKRFEL-UPD-ATTR                 
000850                 MOVE NEJ        TO INDATA-SW                             
000851               ELSE                                                       
000852                 PERFORM IMS-GU-UPFA01                                    
000853                 IF UPPF-KDKVASTA-ADM = +3                                
000854                   IF UPPF-KDKVASTA-PRI = +3 OR                           
000855                      UPPF-KDKVASTA-SEK = +3                              
000856                     MOVE NEJ    TO ADM-KR-SW                             
000857                   ELSE                                                   
000858                     MOVE JA     TO ADM-KR-SW                             
000859                   END-IF                                                 
000860                 END-IF                                                   
000861                 IF END-ADM-KR AND                                        
000862                    (WS-KDKRSTA = '0' OR '1')                             
000863                   MOVE REQU-IDKRFEL-UPD                                  
000864                                 TO WSS-IDKRFEL                           
000865                   IF WSS-IDKRFEL-1 NOT = ADM-KR                          
000866                     MOVE NEJ    TO INDATA-SW                             
000867                     MOVE MFS-ALFA-FAELT-FEL                              
000868                                 TO RESP-IDKRFEL-UPD-ATTR                 
000869                   ELSE                                                   
000870                     MOVE MFS-ALFA-FAELT-RAETT                            
000871                                 TO RESP-IDKRFEL-UPD-ATTR                 
000872                   END-IF                                                 
000873                 ELSE                                                     
000874                   MOVE MFS-ALFA-FAELT-RAETT                              
000875                                 TO RESP-IDKRFEL-UPD-ATTR                 
000876                 END-IF                                                   
000877               END-IF                                                     
000878             END-IF                                                       
000879           ELSE                                                           
000880             IF WS-IDKRFEL  NOT = SPACE                                   
000881               MOVE JA           TO KRFEL-RAD1-SW                         
000882             END-IF                                                       
000883           END-IF                                                         
000884         ELSE                                                             
000885           IF REQU-IDKRFEL-UPD NOT = ALL '+'                              
000886             MOVE MFS-ALFA-FAELT-FEL                                      
000887                                 TO RESP-IDKRFEL-UPD-ATTR                 
000888           END-IF                                                         
000889         END-IF                                                           
000890                                                                          
000891*    -- RESULTAT OCH ÅTGÄRD                                               
000892         IF INDATA-OK                                                     
000893           PERFORM CB-KONTROLL-RESULTAT-ATGARD                            
000894         ELSE                                                             
000895           IF REQU-KVART-KONTR-UPD NOT = ALL '+'                          
000896             IF REQU-KVART-KONTR-UPD NOT NUMERIC                          
000897               MOVE MFS-NUM-FAELT-FEL                                     
000898                                 TO RESP-KVART-KONTR-UPD-ATTR             
000899               MOVE NEJ          TO INDATA-SW                             
000900             ELSE                                                         
000901               MOVE MFS-NUM-FAELT-RAETT                                   
000902                                 TO RESP-KVART-KONTR-UPD-ATTR             
000903             END-IF                                                       
000904           END-IF                                                         
000905           IF REQU-KVART-EJ-GODK-UPD NOT = ALL '+'                        
000906             IF REQU-KVART-EJ-GODK-UPD NOT NUMERIC                        
000907               MOVE MFS-NUM-FAELT-FEL                                     
000908                                 TO RESP-KVART-EJ-GODK-UPD-ATTR           
000909               MOVE NEJ          TO INDATA-SW                             
000910             ELSE                                                         
000911               MOVE MFS-NUM-FAELT-RAETT                                   
000912                                 TO RESP-KVART-EJ-GODK-UPD-ATTR           
000913             END-IF                                                       
000914           END-IF                                                         
000915           IF REQU-KVART-RET-UPD NOT = ALL '+'                            
000916             IF REQU-KVART-RET-UPD NOT NUMERIC                            
000917               MOVE MFS-NUM-FAELT-FEL                                     
000918                                 TO RESP-KVART-RET-UPD-ATTR               
000919               MOVE NEJ          TO INDATA-SW                             
000920             ELSE                                                         
000921               MOVE MFS-NUM-FAELT-RAETT                                   
000922                                 TO RESP-KVART-RET-UPD-ATTR               
000923             END-IF                                                       
000924           END-IF                                                         
000925           IF REQU-KVART-SKROT-UPD NOT = ALL '+'                          
000926             IF REQU-KVART-SKROT-UPD NOT NUMERIC                          
000927               MOVE MFS-NUM-FAELT-FEL                                     
000928                                 TO RESP-KVART-SKROT-UPD-ATTR             
000929               MOVE NEJ          TO INDATA-SW                             
000930               MOVE 'KVART-SKORT NOT NUMERIC'                             
000931                                 TO FELTEXT                               
000932             ELSE                                                         
000933               MOVE MFS-NUM-FAELT-RAETT                                   
000934                                 TO RESP-KVART-SKROT-UPD-ATTR             
000935             END-IF                                                       
000936           END-IF                                                         
000937                                                                          
000938           IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                      
000939             IF REQU-KVART-SKROT-LDC-UPD NOT NUMERIC                      
000940               MOVE MFS-NUM-FAELT-FEL                                     
000941                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
000942               MOVE NEJ          TO INDATA-SW                             
000943               MOVE 'KVART-SKORT-LDC NOT NUMERIC'                         
000944                                 TO FELTEXT                               
000945             ELSE                                                         
000946               MOVE MFS-NUM-FAELT-RAETT                                   
000947                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
000948             END-IF                                                       
000949           END-IF                                                         
000950                                                                          
000951           IF REQU-KVART-KJUST-UPD NOT = ALL '+'                          
000952             IF REQU-KVART-KJUST-UPD NOT NUMERIC                          
000953               MOVE MFS-NUM-FAELT-FEL                                     
000954                                 TO RESP-KVART-KJUST-UPD-ATTR             
000955               MOVE NEJ          TO INDATA-SW                             
000956             ELSE                                                         
000957               MOVE MFS-NUM-FAELT-RAETT                                   
000958                                 TO RESP-KVART-KJUST-UPD-ATTR             
000959             END-IF                                                       
000960           END-IF                                                         
000961           IF REQU-KVART-BEH-UPD NOT = ALL '+'                            
000962             IF REQU-KVART-BEH-UPD NOT NUMERIC                            
000963               MOVE MFS-NUM-FAELT-FEL                                     
000964                                 TO RESP-KVART-BEH-UPD-ATTR               
000965               MOVE NEJ          TO INDATA-SW                             
000966             ELSE                                                         
000967               MOVE MFS-NUM-FAELT-RAETT                                   
000968                                 TO RESP-KVART-BEH-UPD-ATTR               
000969             END-IF                                                       
000970           END-IF                                                         
000971           IF REQU-KVART-SJUST-UPD NOT = ALL '+'                          
000972             IF REQU-KVART-SJUST-UPD NOT NUMERIC                          
000973               MOVE MFS-NUM-FAELT-FEL                                     
000974                                 TO RESP-KVART-SJUST-UPD-ATTR             
000975               MOVE NEJ          TO INDATA-SW                             
000976             ELSE                                                         
000977               MOVE MFS-NUM-FAELT-RAETT                                   
000978                                 TO RESP-KVART-SJUST-UPD-ATTR             
000979             END-IF                                                       
000980           END-IF                                                         
000981         END-IF                                                           
000982                                                                          
000983*    -- KONTROLL ATT MAN INTE LÄGGER UPP DUBBLA KR I INLEVERANS           
000984         IF INDATA-OK        AND                                          
000985            W-IDLOPNRM  > +0 AND                                          
000986            W-NYUPPLAGG = JA                                              
000987           PERFORM CC-KONTROLL-IDLOPNRM                                   
000988         END-IF                                                           
000989                                                                          
000990*    -- KONTROLL AV KDPERSON                                              
000991         IF REQU-KDPERSON-UPD NOT = ALL '+' AND SPACE                     
000992           IF REQU-KDPERSON-UPD NOT NUMERIC                               
000993             MOVE MFS-ALFA-FAELT-FEL                                      
000994                                 TO RESP-KDPERSON-UPD-ATTR                
000995             MOVE NEJ            TO INDATA-SW                             
000996           ELSE                                                           
000997             MOVE REQU-KDPERSON-UPD                                       
000998                                 TO W-IDPERSON                            
000999             PERFORM IMS-GU-WDP311                                        
001000             IF SEGMENT-SAKNAS                                            
001001               MOVE MFS-ALFA-FAELT-FEL                                    
001002                                 TO RESP-KDPERSON-UPD-ATTR                
001003               MOVE NEJ          TO INDATA-SW                             
001004             ELSE                                                         
001005               MOVE MFS-ALFA-FAELT-RAETT                                  
001006                                 TO RESP-KDPERSON-UPD-ATTR                
001007               MOVE PERS-IDNAMN  TO W-BENAEMN                             
001008             END-IF                                                       
001009           END-IF                                                         
001010         END-IF                                                           
001011                                                                          
001012*    -- KONTROLL AV KVKRBEH                                               
001013         IF REQU-KVKRBEH-UPD NOT = ALL '+'                                
001014           MOVE REQU-KVKRBEH-UPD TO DEC-IDFRIDATA                         
001015           MOVE 2                TO DEC-KVHELTAL                          
001016           MOVE 1                TO DEC-KVDECIMAL                         
001017                                                                          
001018           CALL WDECEDIT      USING DEC-WDECAREA                          
001019                                                                          
001020           IF DEC-KDSVAR-OK                                               
001021             MOVE MFS-ALFA-FAELT-RAETT                                    
001022                                 TO RESP-KVKRBEH-UPD-ATTR                 
001023             MOVE DEC-IDEDITDATA TO W-KVKRBEH                             
001024           ELSE                                                           
001025             MOVE MFS-ALFA-FAELT-FEL                                      
001026                                 TO RESP-KVKRBEH-UPD-ATTR                 
001027             MOVE NEJ            TO INDATA-SW                             
001028           END-IF                                                         
001029         END-IF                                                           
001030                                                                          
001031*    -- KONTROLL AV KDDISP  (DISPOSITION CODE)                            
001032*    -- GODKÄNDA VÄRDEN 1-6 SAMT 77 OCH 91 FÖR T-RAPPORT                  
001033*    -- GODKÄNDA VÄRDEN 1 + 3-5 FÖR A-RAPPORT                             
001034         IF REQU-KDDISP-UPD NOT = ALL '+'                                 
001035           IF REQU-IDKRFEL-UPD(1:1) = 'P' OR                              
001036              KR-IDKRFEL(1:1) = 'P'                                       
001037             IF REQU-KDDISP-UPD = 1 OR 3 OR 4 OR 5                        
001038               MOVE MFS-ALFA-FAELT-RAETT                                  
001039                                 TO RESP-KDDISP-UPD-ATTR                  
001040             ELSE                                                         
001041               MOVE NEJ          TO INDATA-SW                             
001042               MOVE MFS-ALFA-FAELT-FEL                                    
001043                                 TO RESP-KDDISP-UPD-ATTR                  
001044             END-IF                                                       
001045           ELSE                                                           
001046             IF REQU-IDKRFEL-UPD(1:1) = 'K'                               
001047               IF REQU-KDDISP-UPD = 6                                     
001048                 MOVE MFS-ALFA-FAELT-RAETT                                
001049                                 TO RESP-KDDISP-UPD-ATTR                  
001050               ELSE                                                       
001051                 MOVE NEJ        TO INDATA-SW                             
001052                 MOVE MFS-ALFA-FAELT-FEL                                  
001053                                 TO RESP-KDDISP-UPD-ATTR                  
001054               END-IF                                                     
001055             ELSE                                                         
001056               IF (REQU-KDDISP-UPD > 0 AND                                
001057                   REQU-KDDISP-UPD < 7) OR                                
001058                  REQU-KDDISP-UPD = 77 OR                                 
001059                  REQU-KDDISP-UPD = 91                                    
001060                 MOVE MFS-ALFA-FAELT-RAETT                                
001061                                 TO RESP-KDDISP-UPD-ATTR                  
001062               ELSE                                                       
001063                 MOVE NEJ        TO INDATA-SW                             
001064                 MOVE MFS-ALFA-FAELT-FEL                                  
001065                                 TO RESP-KDDISP-UPD-ATTR                  
001066               END-IF                                                     
001067             END-IF                                                       
001068           END-IF                                                         
001069         END-IF                                                           
001070                                                                          
001071*    -- KONTROLL AV KDHANDCO (HANDLING COST)                              
001072*    -- GODKÄNDA VÄRDEN 1-2                                               
001073         IF REQU-KDHANDCO-UPD NOT = ALL '+'                               
001074           IF REQU-KDHANDCO-UPD = 1 OR 2                                  
001075             MOVE MFS-ALFA-FAELT-RAETT                                    
001076                                 TO RESP-KDHANDCO-UPD-ATTR                
001077           ELSE                                                           
001078             MOVE NEJ            TO INDATA-SW                             
001079             MOVE MFS-ALFA-FAELT-FEL                                      
001080                                 TO RESP-KDHANDCO-UPD-ATTR                
001081           END-IF                                                         
001082         END-IF                                                           
001083       END-IF                                                             
001084                                                                          
001085       IF INDATA-FEL                                                      
001086         IF UPPD-KR                                                       
001087           MOVE INF-UPPDAT-OTILLATET                                      
001088                                 TO RESP-IDMSG-ERROR                      
001089         END-IF                                                           
001090         IF JUSTERA-KVANTMOT-FEL                                          
001091           MOVE INF-UPPDAT-OTILLATET                                      
001092                                 TO RESP-IDMSG-ERROR                      
001093         END-IF                                                           
001094         IF PARTINR-FINNS                                                 
001095           MOVE ERR-PARTINR-FINNS                                         
001096                                 TO RESP-IDMSG-ERROR                      
001097         ELSE                                                             
001098           IF LEVADR-SAKNAS                                               
001099             MOVE ERR-LEVADR-SAKNAS                                       
001100                                 TO RESP-IDMSG-ERROR                      
001101             MOVE 'NAME/ADDR'    TO RESP-IDELMT-ERROR                     
001102           ELSE                                                           
001103             MOVE ERR-CORR-HILITE-FLDS                                    
001104                                 TO RESP-IDMSG-ERROR                      
001105           END-IF                                                         
001106         END-IF                                                           
001107         PERFORM MFS-SPAERRA-FAELT                                        
001108         PERFORM MFS-ROER-EJ-FAELT-IN-UT                                  
001109         MOVE NEJ                TO ALLT-SW                               
001110       END-IF                                                             
001111     END-IF                                                               
001112     .                                                                    
001113     EJECT                                                                
001114 CA-KONTROLL-MOTTAGET-ANTAL SECTION.                                      
001115     MOVE MFS-NUM-FAELT-RAETT    TO RESP-KVANTMOT-UPD-ATTR                
001116                                                                          
001117     MOVE REQU-IDLOPNRM-UPD      TO W-IDLOPNRM-CSEQ                       
001118     MOVE REQU-TIAVSDAT-UPD      TO W-DAAVSDAT-CSEQ                       
001119     IF REQU-TIAVSDAT-UPD NOT = ZERO                                      
001120       IF REQU-TIAVSDAT-UPD < 500000                                      
001121         MOVE 20                 TO W-DAAVSDAT-CSEQ (1:2)                 
001122       ELSE                                                               
001123         IF REQU-TIAVSDAT-UPD < 999999                                    
001124           MOVE 19               TO W-DAAVSDAT-CSEQ (1:2)                 
001125         ELSE                                                             
001126           MOVE 99999999         TO W-DAAVSDAT-CSEQ                       
001127         END-IF                                                           
001128       END-IF                                                             
001129     END-IF                                                               
001130     PERFORM IMS-GU-W6KVAI01                                              
001131     IF SEGMENT-SAKNAS                                                    
001132       MOVE JA                   TO W-NYUPPLAGG                           
001133     ELSE                                                                 
001134       IF CSEQ-KR-IDKRFEL  = 'PA' OR 'PB' OR 'K '                         
001135         MOVE CSEQ-KR-IDKR       TO W-IDKR                                
001136         IF (CSEQ-KR-KDKRSTA = '0' OR '1' AND                             
001137             CSEQ-KR-FLANNULL = NEJ)       OR                             
001138            (CSEQ-KR-KDKRSTA > '1'        AND                             
001139             CSEQ-KR-FLKRGODK = NEJ)                                      
001140           CONTINUE                                                       
001141         ELSE                                                             
001142           MOVE MFS-NUM-FAELT-FEL                                         
001143                                 TO RESP-IDLOPNRM-UPD-ATTR                
001144           MOVE MFS-NUM-FAELT-FEL                                         
001145                                 TO RESP-KVANTMOT-UPD-ATTR                
001146           MOVE NEJ              TO W-JUSTERA-KVANTMOT-SW                 
001147           MOVE NEJ              TO INDATA-SW                             
001148         END-IF                                                           
001149       ELSE                                                               
001150         PERFORM IMS-GN-W6KVAI01                                          
001151         IF SEGMENT-FINNS                                                 
001152           IF CSEQ-KR-IDKRFEL  = 'PA' OR 'PB' OR 'K '                     
001153             MOVE CSEQ-KR-IDKR   TO W-IDKR                                
001154             IF (CSEQ-KR-KDKRSTA = '0' OR '1' AND                         
001155                 CSEQ-KR-FLANNULL = NEJ)       OR                         
001156                (CSEQ-KR-KDKRSTA > '1'        AND                         
001157                 CSEQ-KR-FLKRGODK = NEJ)                                  
001158               CONTINUE                                                   
001159             ELSE                                                         
001160               MOVE MFS-NUM-FAELT-FEL                                     
001161                                 TO RESP-IDLOPNRM-UPD-ATTR                
001162               MOVE MFS-NUM-FAELT-FEL                                     
001163                                 TO RESP-KVANTMOT-UPD-ATTR                
001164               MOVE NEJ          TO W-JUSTERA-KVANTMOT-SW                 
001165               MOVE NEJ          TO INDATA-SW                             
001166             END-IF                                                       
001167           END-IF                                                         
001168         ELSE                                                             
001169           MOVE JA               TO W-NYUPPLAGG                           
001170         END-IF                                                           
001171       END-IF                                                             
001172     END-IF                                                               
001173     .                                                                    
001174     EJECT                                                                
001175 CB-KONTROLL-RESULTAT-ATGARD SECTION.                                     
001176     PERFORM CBA-FORMELL-KONTROLL                                         
001177     IF INDATA-OK                                                         
001178       IF REQU-IDKRFEL-UPD NOT = ALL '+'                                  
001179         MOVE REQU-IDKRFEL-UPD   TO WS-IDKRFEL                            
001180       END-IF                                                             
001181                                                                          
001182       IF WS-KVANTMOT > +0                                                
001183         IF W-IDLOPNRM       = +0 OR                                      
001184            WS-KVART-KONTR   > +0 OR                                      
001185            WS-KVART-EJ-GODK > +0 OR                                      
001186            WS-KVART-BEH     > +0                                         
001187           MOVE NEJ              TO INDATA-SW                             
001188         END-IF                                                           
001189       ELSE                                                               
001190         IF WS-KVART-EJ-GODK > WS-KVART-KONTR                             
001191           MOVE NEJ              TO INDATA-SW                             
001192         ELSE                                                             
001193           IF W-IDLOPNRM = +0                                             
001194             IF WS-KVART-SJUST > +0 OR                                    
001195                WS-KVART-SJUST < +0                                       
001196               MOVE NEJ          TO INDATA-SW                             
001197             END-IF                                                       
001198           END-IF                                                         
001199         END-IF                                                           
001200       END-IF                                                             
001201       IF INDATA-OK                                                       
001202         IF REQU-KVANTMOT-UPD NOT = ALL '+'                               
001203           IF REQU-KVANTMOT-UPD = WS-KVAVIS AND                           
001204              REQU-KDKRUTF-UPD NOT = '4'    AND                           
001205              WS-KDKRUTF NOT = '4'                                        
001206             MOVE NEJ            TO INDATA-SW                             
001207           ELSE                                                           
001208             MOVE +0             TO WS-KVART-SKROT                        
001209                                    WS-KVART-SKROT-LDC                    
001210                                    WS-KVART-KJUST                        
001211                                    WS-KVART-BEH                          
001212             MOVE +0             TO REQU-KVART-SKROT-UPD                  
001213                                    REQU-KVART-SKROT-LDC-UPD              
001214                                    REQU-KVART-KJUST-UPD                  
001215                                    REQU-KVART-BEH-UPD                    
001216             IF REQU-KDKRUTF-UPD = '4'                                    
001217               MOVE +0           TO WS-KVART-SJUST                        
001218                                    REQU-KVART-SJUST-UPD                  
001219               MOVE 'PA'         TO REQU-IDKRFEL-UPD                      
001220                                    WS-IDKRFEL                            
001221             ELSE                                                         
001222               MOVE +0           TO WS-KVART-RET                          
001223                                    REQU-KVART-RET-UPD                    
001224               COMPUTE WS-KVART-AAVV =                                    
001225                            REQU-KVANTMOT-UPD - WS-KVAVIS                 
001226               MOVE WS-KVART-AAVV                                         
001227                                 TO WS-KVART-SJUST                        
001228                                    REQU-KVART-SJUST-UPD                  
001229               IF WS-KVART-AAVV > +0                                      
001230                 MOVE 'PA'       TO REQU-IDKRFEL-UPD                      
001231                                    WS-IDKRFEL                            
001232               ELSE                                                       
001233                 MOVE 'PB'       TO REQU-IDKRFEL-UPD                      
001234                                    WS-IDKRFEL                            
001235               END-IF                                                     
001236             END-IF                                                       
001237           END-IF                                                         
001238         END-IF                                                           
001239** FÅR ENDAST UPPDATERA FÄLTET SALDOJUSTERING OM DET ÄR EN                
001240** UNDERLEVERANS                                                          
001241         IF INDATA-OK                                                     
001242           IF NOT REQU-UPD-X                                              
001243             IF (REQU-KVART-SJUST-UPD NOT = ALL '+' AND SPACE) AND        
001244                (WS-IDKRFEL = 'PA')                                       
001245               MOVE NEJ          TO INDATA-SW                             
001246             END-IF                                                       
001247           END-IF                                                         
001248           IF WS-KVART-SJUST > +0                                         
001249             IF WS-IDKRFEL-POS1 = 'P'                                     
001250               CONTINUE                                                   
001251             ELSE                                                         
001252               MOVE NEJ          TO INDATA-SW                             
001253             END-IF                                                       
001254           END-IF                                                         
001255           IF WS-KVART-SJUST < +0                                         
001256             IF WS-IDKRFEL-POS1  = 'K' OR 'P'                             
001257               CONTINUE                                                   
001258             ELSE                                                         
001259               MOVE NEJ          TO INDATA-SW                             
001260             END-IF                                                       
001261           END-IF                                                         
001262           IF INDATA-FEL                                                  
001263             MOVE MFS-ALFA-FAELT-FEL                                      
001264                                 TO RESP-KDDISP-UPD-ATTR                  
001265           END-IF                                                         
001266         END-IF                                                           
001267         IF INDATA-OK                                                     
001268           IF WS-KVANTMOT > +0                                            
001269             IF WS-KVART-RET   > +0     OR                                
001270                WS-KVART-SKROT > +0     OR                                
001271                WS-KVART-SKROT-LDC > +0 OR                                
001272                WS-KVART-SJUST > +0     OR                                
001273                WS-KVART-SJUST < +0                                       
001274               IF WS-IDKRFEL  = 'PA'                                      
001275                 CONTINUE                                                 
001276               ELSE                                                       
001277                 IF WS-KVART-AAVV NOT = WS-KVART-SJUST                    
001278                   MOVE NEJ      TO INDATA-SW                             
001279                 END-IF                                                   
001280               END-IF                                                     
001281             END-IF                                                       
001282           END-IF                                                         
001283         END-IF                                                           
001284       END-IF                                                             
001285     END-IF                                                               
001286     IF INDATA-FEL                                                        
001287       IF REQU-KVANTMOT-UPD NOT = ALL '+'                                 
001288         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVANTMOT-UPD-ATTR                
001289       END-IF                                                             
001290       IF REQU-KVART-KONTR-UPD NOT = ALL '+'                              
001291         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-KONTR-UPD-ATTR             
001292       END-IF                                                             
001293       IF REQU-KVART-EJ-GODK-UPD NOT = ALL '+'                            
001294         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-EJ-GODK-UPD-ATTR           
001295       END-IF                                                             
001296       IF REQU-KVART-RET-UPD NOT = ALL '+'                                
001297         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-RET-UPD-ATTR               
001298       END-IF                                                             
001299       IF REQU-KVART-SKROT-UPD NOT = ALL '+'                              
001300         MOVE 'ETT INDATA FEL2      '                                     
001301                                 TO FELTEXT                               
001302         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SKROT-UPD-ATTR             
001303       END-IF                                                             
001304       IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                          
001305         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001306         MOVE 'ETT INDATA FEL       '                                     
001307                                 TO FELTEXT                               
001308       END-IF                                                             
001309       IF REQU-KVART-KJUST-UPD NOT = ALL '+'                              
001310         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-KJUST-UPD-ATTR             
001311       END-IF                                                             
001312       IF REQU-KVART-BEH-UPD NOT = ALL '+'                                
001313         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-BEH-UPD-ATTR               
001314       END-IF                                                             
001315       IF REQU-KVART-SJUST-UPD NOT = ALL '+'                              
001316         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SJUST-UPD-ATTR             
001317       END-IF                                                             
001318     END-IF                                                               
001319     .                                                                    
001320     EJECT                                                                
001321 CBA-FORMELL-KONTROLL SECTION.                                            
001322     IF REQU-KVANTMOT-UPD NOT = ALL '+'                                   
001323       IF KR-KDKRSTA > '1'                                                
001324** FÅR EJ ÄNDRA ANTAL ELLER PRIS EFTER ATT KR GÅTT UPP I STATUS 5.        
001325         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVANTMOT-UPD-ATTR                
001326         MOVE NEJ                TO INDATA-SW                             
001327       ELSE                                                               
001328         IF REQU-KVANTMOT-UPD NOT NUMERIC                                 
001329           MOVE MFS-NUM-FAELT-FEL                                         
001330                                 TO RESP-KVANTMOT-UPD-ATTR                
001331           MOVE NEJ              TO INDATA-SW                             
001332         ELSE                                                             
001333           MOVE MFS-NUM-FAELT-RAETT                                       
001334                                 TO RESP-KVANTMOT-UPD-ATTR                
001335           MOVE REQU-KVANTMOT-UPD                                         
001336                                 TO WS-KVANTMOT                           
001337         END-IF                                                           
001338       END-IF                                                             
001339     END-IF                                                               
001340     IF REQU-KVART-KONTR-UPD NOT = ALL '+'                                
001341       IF KR-KDKRSTA > '1'                                                
001342         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-KONTR-UPD-ATTR             
001343         MOVE NEJ                TO INDATA-SW                             
001344       ELSE                                                               
001345         IF REQU-KVART-KONTR-UPD NOT NUMERIC                              
001346           MOVE MFS-NUM-FAELT-FEL                                         
001347                                 TO RESP-KVART-KONTR-UPD-ATTR             
001348           MOVE NEJ              TO INDATA-SW                             
001349         ELSE                                                             
001350           MOVE MFS-NUM-FAELT-RAETT                                       
001351                                 TO RESP-KVART-KONTR-UPD-ATTR             
001352           MOVE REQU-KVART-KONTR-UPD                                      
001353                                 TO WS-KVART-KONTR                        
001354         END-IF                                                           
001355       END-IF                                                             
001356     END-IF                                                               
001357     IF REQU-KVART-EJ-GODK-UPD NOT = ALL '+'                              
001358       IF KR-KDKRSTA > '1'                                                
001359         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-EJ-GODK-UPD-ATTR           
001360         MOVE NEJ                TO INDATA-SW                             
001361       ELSE                                                               
001362         IF REQU-KVART-EJ-GODK-UPD NOT NUMERIC                            
001363           MOVE MFS-NUM-FAELT-FEL                                         
001364                                 TO RESP-KVART-EJ-GODK-UPD-ATTR           
001365           MOVE NEJ              TO INDATA-SW                             
001366         ELSE                                                             
001367           MOVE MFS-NUM-FAELT-RAETT                                       
001368                                 TO RESP-KVART-EJ-GODK-UPD-ATTR           
001369           MOVE REQU-KVART-EJ-GODK-UPD                                    
001370                                 TO WS-KVART-EJ-GODK                      
001371         END-IF                                                           
001372       END-IF                                                             
001373     END-IF                                                               
001374     IF REQU-KVART-RET-UPD NOT = ALL '+'                                  
001375       IF KR-KDKRSTA > '1'                                                
001376         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-RET-UPD-ATTR               
001377         MOVE NEJ                TO INDATA-SW                             
001378       ELSE                                                               
001379         IF REQU-KVART-RET-UPD NOT NUMERIC                                
001380           MOVE MFS-NUM-FAELT-FEL                                         
001381                                 TO RESP-KVART-RET-UPD-ATTR               
001382           MOVE NEJ              TO INDATA-SW                             
001383         ELSE                                                             
001384           MOVE MFS-NUM-FAELT-RAETT                                       
001385                                 TO RESP-KVART-RET-UPD-ATTR               
001386           MOVE REQU-KVART-RET-UPD                                        
001387                                 TO WS-KVART-RET                          
001388         END-IF                                                           
001389       END-IF                                                             
001390     END-IF                                                               
001391     IF REQU-KVART-SKROT-UPD NOT = ALL '+'                                
001392       IF KR-KDKRSTA > '1'                                                
001393         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SKROT-UPD-ATTR             
001394         MOVE NEJ                TO INDATA-SW                             
001395       ELSE                                                               
001396         IF REQU-KVART-SKROT-UPD NOT NUMERIC                              
001397           MOVE MFS-NUM-FAELT-FEL                                         
001398                                 TO RESP-KVART-SKROT-UPD-ATTR             
001399           MOVE NEJ              TO INDATA-SW                             
001400         ELSE                                                             
001401           MOVE MFS-NUM-FAELT-RAETT                                       
001402                                 TO RESP-KVART-SKROT-UPD-ATTR             
001403           MOVE REQU-KVART-SKROT-UPD                                      
001404                                 TO WS-KVART-SKROT                        
001405         END-IF                                                           
001406       END-IF                                                             
001407     END-IF                                                               
001408     IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                            
001409       IF KR-IDLOPNRM >  1                                                
001410                                                                          
001411         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001412         MOVE NEJ                TO INDATA-SW                             
001413       ELSE                                                               
001414         IF REQU-KVART-SKROT-LDC-UPD NOT NUMERIC                          
001415           MOVE MFS-NUM-FAELT-FEL                                         
001416                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001417           MOVE NEJ              TO INDATA-SW                             
001418         ELSE                                                             
001419           MOVE MFS-NUM-FAELT-RAETT                                       
001420                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001421           MOVE REQU-KVART-SKROT-LDC-UPD                                  
001422                                 TO WS-KVART-SKROT-LDC                    
001423         END-IF                                                           
001424         IF REQU-KVART-SKROT-UPD NOT = ALL '+' AND                        
001425            REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                        
001426           IF REQU-KVART-SKROT-LDC-UPD > REQU-KVART-SKROT-UPD             
001427             MOVE MFS-NUM-FAELT-FEL                                       
001428                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001429             MOVE NEJ            TO INDATA-SW                             
001430           END-IF                                                         
001431         END-IF                                                           
001432         IF REQU-KVANTMOT-UPD   NOT = ALL '+' OR                          
001433*           REQU-KVART-KONTR-UPD NOT = ALL '+' OR                         
001434*           REQU-KVART-EJ-GODK-UPD NOT = ALL '+' OR                       
001435            REQU-KVART-RET-UPD  NOT = ALL '+' OR                          
001436*           REQU-KVART-SKROT-UPD NOT = ALL '+' OR                         
001437*           REQU-KVART-SKROT-LDC-UPD NOT = ALL '+' OR                     
001438            REQU-KVART-KJUST-UPD NOT = ALL '+' OR                         
001439            REQU-KVART-BEH-UPD  NOT = ALL '+' OR                          
001440            REQU-KVART-SJUST-UPD NOT = ALL '+'                            
001441           MOVE 'SALDO FEL '     TO FELTEXT                               
001442           MOVE MFS-NUM-FAELT-FEL                                         
001443                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001444           MOVE NEJ              TO INDATA-SW                             
001445         END-IF                                                           
001446       END-IF                                                             
001447     END-IF                                                               
001448*                                                                         
001449                                                                          
001450     IF REQU-KVART-RET-UPD  NOT = ALL '+' OR                              
001451        REQU-KVART-KJUST-UPD NOT = ALL '+' OR                             
001452        REQU-KVART-BEH-UPD  NOT = ALL '+' OR                              
001453        REQU-KVART-SJUST-UPD NOT = ALL '+'                                
001454*       REQU-KVART-KONTR-UPD NOT = ALL '+' OR                             
001455*       REQU-KVART-EJ-GODK-UPD NOT = ALL '+' OR                           
001456*       REQU-KVART-SKROT-UPD NOT = ALL '+' OR                             
001457*       REQU-KVART-SKROT-LDC-UPD NOT = ALL '+' OR                         
001458*       REQU-KVANTMOT-UPD   NOT = ALL '+' OR                              
001459       IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                          
001460         IF REQU-KVART-SKROT-LDC-UPD > 0                                  
001461           MOVE NEJ              TO INDATA-SW                             
001462         END-IF                                                           
001463       END-IF                                                             
001464       IF W-NYUPPLAGG = NEJ                                               
001465         IF KR-KVART-SKROT-LDC > 0                                        
001466           MOVE NEJ              TO INDATA-SW                             
001467         END-IF                                                           
001468       END-IF                                                             
001469     END-IF                                                               
001470     IF REQU-KVART-KJUST-UPD NOT = ALL '+'                                
001471       IF REQU-KVART-KJUST-UPD NOT NUMERIC                                
001472         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-KJUST-UPD-ATTR             
001473         MOVE NEJ                TO INDATA-SW                             
001474       ELSE                                                               
001475         MOVE MFS-NUM-FAELT-RAETT                                         
001476                                 TO RESP-KVART-KJUST-UPD-ATTR             
001477         MOVE REQU-KVART-KJUST-UPD                                        
001478                                 TO WS-KVART-KJUST                        
001479       END-IF                                                             
001480     END-IF                                                               
001481     IF REQU-KVART-BEH-UPD NOT = ALL '+'                                  
001482       IF KR-KDKRSTA > '1'                                                
001483         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-BEH-UPD-ATTR               
001484         MOVE NEJ                TO INDATA-SW                             
001485       ELSE                                                               
001486         IF REQU-KVART-BEH-UPD NOT NUMERIC                                
001487           MOVE MFS-NUM-FAELT-FEL                                         
001488                                 TO RESP-KVART-BEH-UPD-ATTR               
001489           MOVE NEJ              TO INDATA-SW                             
001490         ELSE                                                             
001491           MOVE MFS-NUM-FAELT-RAETT                                       
001492                                 TO RESP-KVART-BEH-UPD-ATTR               
001493           MOVE REQU-KVART-BEH-UPD                                        
001494                                 TO WS-KVART-BEH                          
001495         END-IF                                                           
001496       END-IF                                                             
001497     END-IF                                                               
001498     IF REQU-KVART-SJUST-UPD NOT = ALL '+'                                
001499       IF KR-KDKRSTA > '1'                                                
001500         MOVE MFS-NUM-FAELT-FEL  TO RESP-KVART-SJUST-UPD-ATTR             
001501         MOVE NEJ                TO INDATA-SW                             
001502       ELSE                                                               
001503         MOVE REQU-KVART-SJUST-UPD                                        
001504                                 TO DEC-IDFRIDATA                         
001505         MOVE 6                  TO DEC-KVHELTAL                          
001506         MOVE 0                  TO DEC-KVDECIMAL                         
001507         CALL WDECEDIT        USING DEC-WDECAREA                          
001508         IF DEC-KDSVAR-OK                                                 
001509           MOVE DEC-IDEDITDATA   TO WS-KVART-SJUST                        
001510         ELSE                                                             
001511           MOVE MFS-NUM-FAELT-FEL                                         
001512                                 TO RESP-KVART-SJUST-UPD-ATTR             
001513           MOVE NEJ              TO INDATA-SW                             
001514         END-IF                                                           
001515       END-IF                                                             
001516     END-IF                                                               
001517     .                                                                    
001518     EJECT                                                                
001519 CC-KONTROLL-IDLOPNRM SECTION.                                            
001520     MOVE REQU-IDLOPNRM-UPD      TO W-IDLOPNRM-CSEQ                       
001521     MOVE REQU-TIAVSDAT-UPD      TO W-DAAVSDAT-CSEQ                       
001522     IF REQU-TIAVSDAT-UPD NOT = ZERO                                      
001523       IF REQU-TIAVSDAT-UPD < 500000                                      
001524         MOVE 20                 TO W-DAAVSDAT-CSEQ (1:2)                 
001525       ELSE                                                               
001526         IF REQU-TIAVSDAT-UPD < 999999                                    
001527           MOVE 19               TO W-DAAVSDAT-CSEQ (1:2)                 
001528         ELSE                                                             
001529           MOVE 99999999         TO W-DAAVSDAT-CSEQ                       
001530         END-IF                                                           
001531       END-IF                                                             
001532     END-IF                                                               
001533     PERFORM IMS-GU-W6KVAI01                                              
001534     IF SEGMENT-FINNS                                                     
001535       IF REQU-KVANTMOT-UPD = ALL '+'                                     
001536         IF CSEQ-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                        
001537           CONTINUE                                                       
001538         ELSE                                                             
001539           IF REQU-IDKRFEL-UPD = 'K '                                     
001540             CONTINUE                                                     
001541           ELSE                                                           
001542             MOVE MFS-NUM-FAELT-FEL                                       
001543                                 TO RESP-IDLOPNRM-UPD-ATTR                
001544             MOVE JA             TO PARTINR-FINNS-SW                      
001545             MOVE CSEQ-KR-IDKRFEL                                         
001546                                 TO FELTEXT                               
001547             MOVE NEJ            TO INDATA-SW                             
001548           END-IF                                                         
001549         END-IF                                                           
001550       ELSE                                                               
001551         IF CSEQ-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                        
001552           MOVE MFS-NUM-FAELT-FEL                                         
001553                                 TO RESP-IDLOPNRM-UPD-ATTR                
001554           MOVE JA               TO PARTINR-FINNS-SW                      
001555           MOVE NEJ              TO INDATA-SW                             
001556         END-IF                                                           
001557       END-IF                                                             
001558     END-IF                                                               
001559     IF INDATA-OK                                                         
001560       PERFORM IMS-GN-W6KVAI01                                            
001561       IF SEGMENT-FINNS                                                   
001562         MOVE MFS-NUM-FAELT-FEL  TO RESP-IDLOPNRM-UPD-ATTR                
001563         MOVE JA                 TO PARTINR-FINNS-SW                      
001564         MOVE NEJ                TO INDATA-SW                             
001565       END-IF                                                             
001566     END-IF                                                               
001567     .                                                                    
001568     EJECT                                                                
001569 D-UPPDATERA SECTION.                                                     
001570     IF W-NYUPPLAGG = JA                                                  
001571       PERFORM DB-INITIERA-W6H701                                         
001572     ELSE                                                                 
001573       PERFORM IMS-GHU-W6KVAE01                                           
001574       IF KR-KDKRSTA  > '1' AND                                           
001575          KR-FLKRGODK = NEJ                                               
001576         PERFORM DA-JUSTERING                                             
001577       END-IF                                                             
001578     END-IF                                                               
001579                                                                          
001580     IF REQU-IDLOPNRM-UPD NOT = ALL '+'                                   
001581       MOVE ART-IDLOPNRM         TO KR-IDLOPNRM                           
001582       MOVE W-W6D101-IDFS        TO W-IDFS                                
001583       PERFORM S10-KONV-IDFS                                              
001584       MOVE W-IDAVINR            TO KR-IDAVINR                            
001585       MOVE ART-KVAVIS           TO KR-KVAVIS                             
001586       MOVE W-W6D101-TIAVIDAT    TO KR-DAAVSDAT                           
001587       IF W-W6D101-TIAVIDAT NOT = ZERO                                    
001588         IF W-W6D101-TIAVIDAT < 500000                                    
001589           MOVE 20               TO KR-DAAVSDAT (1:2)                     
001590         ELSE                                                             
001591           IF W-W6D101-TIAVIDAT < 999999                                  
001592             MOVE 19             TO KR-DAAVSDAT (1:2)                     
001593           ELSE                                                           
001594             MOVE 99999999       TO KR-DAAVSDAT                           
001595            END-IF                                                        
001596          END-IF                                                          
001597        END-IF                                                            
001598        MOVE ART-IDARTNR         TO KR-IDARTNR                            
001599        MOVE W-W6D101-IDLEVNR    TO KR-IDLEVNR                            
001600     END-IF                                                               
001601                                                                          
001602     IF REQU-IDARTNR-UPD NOT = ALL '+'                                    
001603       MOVE REQU-IDARTNR-UPD     TO KR-IDARTNR                            
001604     END-IF                                                               
001605                                                                          
001606     IF W-NYUPPLAGG = JA                                                  
001607       MOVE FUNCTION CURRENT-DATE (1:8)                                   
001608                                 TO WS-DAREGDAT                           
001609       COMPUTE KR-DAREGDAT-9KOMPL = 99999999 - WS-DAREGDAT                
001610       MOVE REQU-IDDC-KEY        TO KR-IDDC                               
001611                                    W-IDDC                                
001612                                    WS-IDDC                               
001613       PERFORM IMS-GU-WDB601                                              
001614       MOVE DCS-IDFTG            TO KR-IDFTG                              
001615       IF REQU-IDKRFEL-UPD = 'K '                                         
001616         MOVE '1'                TO KR-KDKRSTA                            
001617       ELSE                                                               
001618         MOVE '0'                TO KR-KDKRSTA                            
001619       END-IF                                                             
001620     END-IF                                                               
001621                                                                          
001622     IF REQU-IDLEVNR-UPD NOT = ALL '+'                                    
001623       MOVE REQU-IDLEVNR-UPD     TO KR-IDLEVNR                            
001624     END-IF                                                               
001625                                                                          
001626     IF REQU-IDLEVG-UPD NOT = ALL '+'                                     
001627       MOVE REQU-IDLEVG-UPD      TO KR-IDLEVG                             
001628     END-IF                                                               
001629                                                                          
001630     IF REQU-UPD-X                                                        
001631       IF REQU-KDKRUTF-UPD NOT = ALL '+'                                  
001632         MOVE REQU-KDKRUTF-UPD   TO KR-KDKRUTF                            
001633       END-IF                                                             
001634     END-IF                                                               
001635                                                                          
001636     IF REQU-KVANTMOT-UPD NOT = ALL '+'                                   
001637       MOVE REQU-KVANTMOT-UPD    TO KR-KVANTMOT                           
001638       MOVE WS-KVART-AAVV        TO KR-KVART-AAVV                         
001639     END-IF                                                               
001640     IF REQU-KVART-KONTR-UPD NOT = ALL '+'                                
001641       MOVE REQU-KVART-KONTR-UPD TO KR-KVART-KONTR                        
001642     END-IF                                                               
001643     IF REQU-KVART-EJ-GODK-UPD NOT = ALL '+'                              
001644       MOVE REQU-KVART-EJ-GODK-UPD                                        
001645                                 TO KR-KVART-EJ-GODK                      
001646     END-IF                                                               
001647     IF REQU-KVART-RET-UPD NOT = ALL '+'                                  
001648       MOVE REQU-KVART-RET-UPD   TO KR-KVART-RET                          
001649       IF KR-KVART-RET > +0                                               
001650         MOVE NEJ                TO KR-FLKROMK                            
001651       END-IF                                                             
001652     END-IF                                                               
001653     IF REQU-KVART-SKROT-UPD NOT = ALL '+'                                
001654       MOVE REQU-KVART-SKROT-UPD TO KR-KVART-SKROT                        
001655       IF KR-KVART-SKROT > +0                                             
001656         MOVE NEJ                TO KR-FLKROMK                            
001657       END-IF                                                             
001658     END-IF                                                               
001659     IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                            
001660       MOVE REQU-KVART-SKROT-LDC-UPD                                      
001661                                 TO KR-KVART-SKROT-LDC                    
001662**      IF KR-KVART-SKROT-LDC > +0                                        
001663**        MOVE NEJ             TO KR-FLKROMK                              
001664**      END-IF                                                            
001665     END-IF                                                               
001666     IF REQU-FLKVALSP-UPD NOT = ALL '+'                                   
001667       MOVE REQU-FLKVALSP-UPD    TO KR-FLKVALSP                           
001668     END-IF                                                               
001669     IF REQU-FLBUFJUS-UPD NOT = ALL '+'                                   
001670       MOVE REQU-FLBUFJUS-UPD    TO KR-FLBUFJUS                           
001671     END-IF                                                               
001672     IF REQU-KVART-KJUST-UPD NOT = ALL '+'                                
001673       MOVE REQU-KVART-KJUST-UPD TO KR-KVART-KJUST                        
001674       IF KR-KVART-KJUST > +0                                             
001675         MOVE NEJ                TO KR-FLKROMK                            
001676       END-IF                                                             
001677     END-IF                                                               
001678     IF REQU-KVART-BEH-UPD NOT = ALL '+'                                  
001679       MOVE REQU-KVART-BEH-UPD   TO KR-KVART-BEH                          
001680       IF KR-KVART-SKROT > +0                                             
001681         MOVE NEJ                TO KR-FLKROMK                            
001682       END-IF                                                             
001683     END-IF                                                               
001684     IF REQU-KVART-SJUST-UPD NOT = ALL '+'                                
001685       MOVE WS-KVART-SJUST       TO KR-KVART-SJUST                        
001686     END-IF                                                               
001687     IF REQU-IDKRFEL-UPD NOT = ALL '+'                                    
001688       MOVE REQU-IDKRFEL-UPD     TO KR-IDKRFEL                            
001689     END-IF                                                               
001690                                                                          
001691     IF REQU-KDDISP-UPD NOT = ALL '+'                                     
001692       MOVE REQU-KDDISP-UPD      TO KR-KDDISP                             
001693     END-IF                                                               
001694                                                                          
001695     IF REQU-KDHANDCO-UPD NOT = ALL '+'                                   
001696       MOVE REQU-KDHANDCO-UPD    TO KR-KDHANDCO                           
001697     END-IF                                                               
001698                                                                          
001699     IF REQU-UPD-X                                                        
001700       IF KR-BEKRBEH = SPACE AND REQU-BEKRBEH-UPD NOT = ALL '+'           
001701         MOVE REQU-BEKRBEH-UPD   TO KR-BEKRBEH                            
001702       END-IF                                                             
001703     ELSE                                                                 
001704       IF REQU-BEKRBEH-UPD NOT = ALL '+'                                  
001705         MOVE REQU-BEKRBEH-UPD   TO KR-BEKRBEH                            
001706       ELSE                                                               
001707         IF REQU-KDPERSON-UPD NOT = ALL '+'                               
001708           MOVE W-BENAEMN        TO KR-BEKRBEH                            
001709         END-IF                                                           
001710       END-IF                                                             
001711     END-IF                                                               
001712                                                                          
001713     IF REQU-KVKRBEH-UPD NOT = ALL '+'                                    
001714       MOVE W-KVKRBEH            TO KR-KVKRBEH                            
001715     END-IF                                                               
001716                                                                          
001717     IF REQU-TEKRPLT-UPD NOT = ALL '+'                                    
001718       MOVE REQU-TEKRPLT-UPD     TO KR-TEKRPLT                            
001719     END-IF                                                               
001720                                                                          
001721     IF KR-BEKRBEH        = SPACE OR                                      
001722        KR-KDKRSTA        > '1'   OR                                      
001723        KR-IDKRFEL        = SPACE OR                                      
001724        KR-KVKRBEH        = ZERO  OR                                      
001725        KR-KVART-RET + KR-KVART-SKROT + KR-KVART-KJUST +                  
001726        KR-KVART-BEH - WS-KVART-SJUST = +0                                
001727       CONTINUE                                                           
001728     ELSE                                                                 
001729       MOVE '1'                  TO KR-KDKRSTA                            
001730     END-IF                                                               
001731                                                                          
001732     IF W-NYUPPLAGG = JA                                                  
001733       PERFORM IMS-GU-W6LOPB01                                            
001734       PERFORM IMS-GHNP-W6LOPB11                                          
001735       MOVE 6002-IDKR (02)       TO KR-IDKR                               
001736                                    W-IDKR                                
001737       IF 6002-IDKR (02) = 99999                                          
001738         MOVE +1                 TO 6002-IDKR (02)                        
001739       ELSE                                                               
001740         ADD +1                  TO 6002-IDKR (02)                        
001741       END-IF                                                             
001742       PERFORM IMS-REPL-W6LOPB11                                          
001743                                                                          
001744       PERFORM IMS-ISRT-W6KVAE01                                          
001745     ELSE                                                                 
001746       PERFORM IMS-REPL-W6KVAE01                                          
001747     END-IF                                                               
001748     MOVE INF-UPDATE-DONE        TO RESP-IDMSG-INFO                       
001749     PERFORM MFS-RENSA-FAELT-IN                                           
001750     PERFORM MFS-FORM-ATTR                                                
001751     .                                                                    
001752     EJECT                                                                
001753 DA-JUSTERING SECTION.                                                    
001754                                                                          
001755     IF KR-KDKRJUST = SPACE                                               
001756       MOVE '3'                  TO WS-KDKRJUST                           
001757     ELSE                                                                 
001758       MOVE KR-KDKRJUST          TO WS-KDKRJUST                           
001759     END-IF                                                               
001760                                                                          
001761     IF KR-IDLOPNRM = +0                                                  
001762       PERFORM S01-UPPD-JUSTERING                                         
001763     ELSE                                                                 
001764       MOVE KR-IDLOPNRM          TO W-IDLOPNRM                            
001765       MOVE KR-IDARTNR           TO W-IDARTNR                             
001766       PERFORM IMS-GU-W6INLC01                                            
001767       IF SEGMENT-FINNS                                                   
001768         MOVE SEQB-IDLEVNR       TO W-IDLEVNR                             
001769                                    W-W6D101-IDLEVNR                      
001770         MOVE SEQB-IDDC          TO W-W6D101-IDDC                         
001771         MOVE SEQB-IDFS          TO W-W6D101-IDFS                         
001772         MOVE SEQB-TIAVIDAT      TO W-W6D101-TIAVIDAT                     
001773         MOVE SEQB-IDRADNR-INL   TO W-IDRADNRI                            
001774         PERFORM IMS-GU-W6INLA11                                          
001775         IF SEGMENT-FINNS                                                 
001776           MOVE ART-FLKLAR       TO WS-FLKLAR                             
001777         ELSE                                                             
001778           MOVE JA               TO WS-FLKLAR                             
001779         END-IF                                                           
001780       ELSE                                                               
001781         MOVE JA                 TO WS-FLKLAR                             
001782       END-IF                                                             
001783       IF WS-FLKLAR   = JA                                                
001784         PERFORM S01-UPPD-JUSTERING                                       
001785       ELSE                                                               
001786         IF KR-KDKRSTA = '3'                                              
001787           PERFORM IMS-GNP-W6KVAE12                                       
001788           IF SEGMENT-FINNS                                               
001789             PERFORM IMS-GHU-W6KVAE01                                     
001790             PERFORM S01-UPPD-JUSTERING                                   
001791           ELSE                                                           
001792             PERFORM IMS-GHU-W6KVAE01                                     
001793           END-IF                                                         
001794         ELSE                                                             
001795           IF KR-KDKRSTA = '4' OR '5'                                     
001796             PERFORM S01-UPPD-JUSTERING                                   
001797           END-IF                                                         
001798         END-IF                                                           
001799       END-IF                                                             
001800     END-IF                                                               
001801                                                                          
001802     IF REQU-KVANTMOT-UPD   = ALL '+'      AND                            
001803        REQU-KVART-RET-UPD  = ALL '+'      AND                            
001804        REQU-KVART-SKROT-UPD = ALL '+'     AND                            
001805        REQU-KVART-SKROT-LDC-UPD = ALL '+' AND                            
001806        REQU-KVART-KJUST-UPD = ALL '+'     AND                            
001807        REQU-KVART-BEH-UPD  = ALL '+'      AND                            
001808        REQU-KVART-SJUST-UPD = ALL '+'     AND                            
001809        REQU-IDKRFEL-UPD    = ALL '+'      AND                            
001810        REQU-KDDISP-UPD     = ALL '+'      AND                            
001811        REQU-KDHANDCO-UPD   = ALL '+'                                     
001812       CONTINUE                                                           
001813     ELSE                                                                 
001814       MOVE WS-KDKRJUST          TO KR-KDKRJUST                           
001815     END-IF                                                               
001816     .                                                                    
001817     EJECT                                                                
001818 DB-INITIERA-W6H701 SECTION.                                              
001819     MOVE SPACE                  TO KR-ADATTENT                           
001820                                    KR-BEKRANS                            
001821                                    KR-BEKRBEH                            
001822                                    KR-BEKRPACK                           
001823                                    KR-KDKRATG                            
001824                                    KR-IDKRATLF                           
001825                                    KR-IDKRFEL                            
001826                                    KR-KDKRJUST                           
001827                                    KR-KDKRSTA                            
001828                                    KR-KDKRUTF                            
001829                                    KR-TEKRPLT                            
001830                                    KR-TEKRSPEC-ATID                      
001831                                    KR-TEKRSPEC-MAT                       
001832                                    KR-TEKRSPEC-OMK                       
001833                                    KR-KDFAXVAL                           
001834                                    KR-KDMEMVAL                           
001835                                    KR-IDLEVNR                            
001836                                    KR-FLKVALSP                           
001837                                    KR-FLBUFJUS                           
001838     MOVE ZERO                   TO KR-IDARTNR                            
001839                                    KR-IDAVINR                            
001840                                    KR-IDFTG                              
001841                                    KR-IDLEVG                             
001842                                    KR-IDLOPNRM                           
001843                                    KR-IDDC                               
001844                                    KR-KDDISP                             
001845                                    KR-KDHANDCO                           
001846                                    KR-KVANTMOT                           
001847                                    KR-KVARBTID                           
001848                                    KR-KVART-AAVV                         
001849                                    KR-KVART-BEH                          
001850                                    KR-KVART-EJ-GODK                      
001851                                    KR-KVART-KJUST                        
001852                                    KR-KVART-KONTR                        
001853                                    KR-KVART-RET                          
001854                                    KR-KVART-SJUST                        
001855                                    KR-KVART-SKROT                        
001856                                    KR-KVART-SKROT-LDC                    
001857                                    KR-KVART-TIDGK                        
001858                                    KR-KVAVIS                             
001859                                    KR-KVKRBEH                            
001860                                    KR-KVKRKNTR                           
001861                                    KR-KVKRPACK                           
001862                                    KR-SUMAT                              
001863                                    KR-SUOMK                              
001864                                    KR-DAAVSDAT                           
001865                                    KR-TIKRANS                            
001866                                    KR-TIKRPACK                           
001867                                    KR-DAREGDAT-9KOMPL                    
001868                                    KR-SUKRLIM                            
001869                                                                          
001870     MOVE NEJ                    TO KR-FLANNULL                           
001871                                    KR-FLINKANS                           
001872                                    KR-FLKRGODK                           
001873                                    KR-FLKROMK                            
001874                                    KR-FLEJKNTRL                          
001875     MOVE JA                     TO KR-FLKRLFEL                           
001876                                    KR-FLKRLIM                            
001877                                                                          
001878     IF REQU-IDLOPNRM-UPD NOT = ALL '+'                                   
001879       IF ART-KVAVIS = 0                                                  
001880         MOVE NEJ                TO KR-FLARBDEB                           
001881       ELSE                                                               
001882         MOVE JA                 TO KR-FLARBDEB                           
001883       END-IF                                                             
001884     ELSE                                                                 
001885       MOVE ' '                  TO KR-FLARBDEB                           
001886     END-IF                                                               
001896     .                                                                    
001897     EJECT                                                                
001898 E-SAMMA-SIDA SECTION.                                                    
001899                                                                          
001900     IF REQU-INPUT = ALL '+'                                              
001901       PERFORM MFS-RENSA-FAELT-IN                                         
001902     ELSE                                                                 
001903       MOVE JA                   TO TRYCK-PF11-SW                         
001904       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
001905       PERFORM EA-MID-INDATA-TILL-MOD                                     
001906     END-IF                                                               
001907     .                                                                    
001908     EJECT                                                                
001909 EA-MID-INDATA-TILL-MOD SECTION.                                          
001910                                                                          
001911     IF REQU-IDLOPNRM-UPD NOT = ALL '+'                                   
001912       MOVE REQU-IDLOPNRM-UPD    TO RESP-IDLOPNRM-UPD                     
001913       MOVE MFS-ADD-LAES-IN-FAELT                                         
001914                                 TO RESP-IDLOPNRM-UPD-ATTR                
001915     ELSE                                                                 
001916       MOVE ALL-SPACE            TO RESP-IDLOPNRM-UPD                     
001917     END-IF                                                               
001918                                                                          
001919     IF REQU-IDARTNR-UPD NOT = ALL '+'                                    
001920       MOVE REQU-IDARTNR-UPD     TO RESP-IDARTNR-UPD                      
001921       MOVE MFS-ADD-LAES-IN-FAELT                                         
001922                                 TO RESP-IDARTNR-UPD-ATTR                 
001923     ELSE                                                                 
001924       MOVE ALL-SPACE            TO RESP-IDARTNR-UPD                      
001925     END-IF                                                               
001926                                                                          
001927     IF REQU-IDLEVNR-UPD NOT = ALL '+'                                    
001928       MOVE REQU-IDLEVNR-UPD     TO RESP-IDLEVNR-UPD                      
001929       MOVE MFS-ADD-LAES-IN-FAELT                                         
001930                                 TO RESP-IDLEVNR-UPD-ATTR                 
001931     ELSE                                                                 
001932       MOVE ALL-SPACE            TO RESP-IDLEVNR-UPD                      
001933     END-IF                                                               
001934                                                                          
001935     IF REQU-IDLEVG-UPD NOT = ALL '+'                                     
001936       MOVE REQU-IDLEVG-UPD      TO RESP-IDLEVG-UPD                       
001937       MOVE MFS-ADD-LAES-IN-FAELT                                         
001938                                 TO RESP-IDLEVG-UPD-ATTR                  
001939     ELSE                                                                 
001940       MOVE ALL-SPACE            TO RESP-IDLEVG-UPD                       
001941     END-IF                                                               
001942                                                                          
001943     IF REQU-KVANTMOT-UPD NOT = ALL '+'                                   
001944       MOVE REQU-KVANTMOT-UPD    TO RESP-KVANTMOT-UPD                     
001945       MOVE MFS-ADD-LAES-IN-FAELT                                         
001946                                 TO RESP-KVANTMOT-UPD-ATTR                
001947     ELSE                                                                 
001948       MOVE ALL-SPACE            TO RESP-KVANTMOT-UPD                     
001949     END-IF                                                               
001950                                                                          
001951     IF REQU-KVART-KONTR-UPD NOT = ALL '+'                                
001952       MOVE REQU-KVART-KONTR-UPD TO RESP-KVART-KONTR-UPD                  
001953       MOVE MFS-ADD-LAES-IN-FAELT                                         
001954                                 TO RESP-KVART-KONTR-UPD-ATTR             
001955     ELSE                                                                 
001956       MOVE ALL-SPACE            TO RESP-KVART-KONTR-UPD                  
001957     END-IF                                                               
001958                                                                          
001959     IF REQU-KVART-EJ-GODK-UPD NOT = ALL '+'                              
001960       MOVE REQU-KVART-EJ-GODK-UPD                                        
001961                                 TO RESP-KVART-EJ-GODK-UPD                
001962       MOVE MFS-ADD-LAES-IN-FAELT                                         
001963                                 TO RESP-KVART-EJ-GODK-UPD-ATTR           
001964     ELSE                                                                 
001965       MOVE ALL-SPACE            TO RESP-KVART-EJ-GODK-UPD                
001966     END-IF                                                               
001967                                                                          
001968     IF REQU-KVART-RET-UPD NOT = ALL '+'                                  
001969       MOVE REQU-KVART-RET-UPD   TO RESP-KVART-RET-UPD                    
001970       MOVE MFS-ADD-LAES-IN-FAELT                                         
001971                                 TO RESP-KVART-RET-UPD-ATTR               
001972     ELSE                                                                 
001973       MOVE ALL-SPACE            TO RESP-KVART-RET-UPD                    
001974     END-IF                                                               
001975                                                                          
001976     IF REQU-KVART-SKROT-UPD NOT = ALL '+'                                
001977       MOVE REQU-KVART-SKROT-UPD TO RESP-KVART-SKROT-UPD                  
001978       MOVE MFS-ADD-LAES-IN-FAELT                                         
001979                                 TO RESP-KVART-SKROT-UPD-ATTR             
001980     ELSE                                                                 
001981       MOVE ALL-SPACE            TO RESP-KVART-SKROT-UPD                  
001982     END-IF                                                               
001983                                                                          
001984     IF REQU-KVART-SKROT-LDC-UPD NOT = ALL '+'                            
001985       MOVE REQU-KVART-SKROT-LDC-UPD                                      
001986                                 TO RESP-KVART-SKROT-LDC-UPD              
001987       MOVE MFS-ADD-LAES-IN-FAELT                                         
001988                                 TO RESP-KVART-SKROT-LDC-UPD-ATTR         
001989     ELSE                                                                 
001990       MOVE ALL-SPACE            TO RESP-KVART-SKROT-LDC-UPD              
001991     END-IF                                                               
001992                                                                          
001993     IF REQU-FLKVALSP-UPD NOT = ALL '+'                                   
001994       MOVE REQU-FLKVALSP-UPD    TO RESP-FLKVALSP-UPD                     
001995       MOVE MFS-ADD-LAES-IN-FAELT                                         
001996                                 TO RESP-FLKVALSP-UPD-ATTR                
001997     ELSE                                                                 
001998       MOVE ALL-SPACE            TO RESP-FLKVALSP-UPD                     
001999     END-IF                                                               
002000                                                                          
002001     IF REQU-FLBUFJUS-UPD NOT = ALL '+'                                   
002002       MOVE REQU-FLBUFJUS-UPD    TO RESP-FLBUFJUS-UPD                     
002003       MOVE MFS-ADD-LAES-IN-FAELT                                         
002004                                 TO RESP-FLBUFJUS-UPD-ATTR                
002005     ELSE                                                                 
002006       MOVE ALL-SPACE            TO RESP-FLBUFJUS-UPD                     
002007     END-IF                                                               
002008                                                                          
002009     IF REQU-KVART-KJUST-UPD NOT = ALL '+'                                
002010       MOVE REQU-KVART-KJUST-UPD TO RESP-KVART-KJUST-UPD                  
002011       MOVE MFS-ADD-LAES-IN-FAELT                                         
002012                                 TO RESP-KVART-KJUST-UPD-ATTR             
002013     ELSE                                                                 
002014       MOVE ALL-SPACE            TO RESP-KVART-KJUST-UPD                  
002015     END-IF                                                               
002016                                                                          
002017     IF REQU-KVART-BEH-UPD NOT = ALL '+'                                  
002018       MOVE REQU-KVART-BEH-UPD   TO RESP-KVART-BEH-UPD                    
002019       MOVE MFS-ADD-LAES-IN-FAELT                                         
002020                                 TO RESP-KVART-BEH-UPD-ATTR               
002021     ELSE                                                                 
002022       MOVE ALL-SPACE            TO RESP-KVART-BEH-UPD                    
002023     END-IF                                                               
002024                                                                          
002025     IF REQU-KVART-SJUST-UPD NOT = ALL '+'                                
002026       MOVE REQU-KVART-SJUST-UPD TO RESP-KVART-SJUST-UPD                  
002027       MOVE MFS-ADD-LAES-IN-FAELT                                         
002028                                 TO RESP-KVART-SJUST-UPD-ATTR             
002029     ELSE                                                                 
002030       MOVE ALL-SPACE            TO RESP-KVART-SJUST-UPD                  
002031     END-IF                                                               
002032                                                                          
002033     IF REQU-KVKRBEH-UPD NOT = ALL '+'                                    
002034       MOVE REQU-KVKRBEH-UPD     TO RESP-KVKRBEH-UPD                      
002035       MOVE MFS-ADD-LAES-IN-FAELT                                         
002036                                 TO RESP-KVKRBEH-UPD-ATTR                 
002037     END-IF                                                               
002038     .                                                                    
002039     EJECT                                                                
002040 F-LAES-VISA-INFO SECTION.                                                
002041                                                                          
002042     MOVE SPACE                  TO RESP-TEDISP                           
002043                                    RESP-IDKRFEL-UPD                      
002044                                    RESP-BEKRFEL                          
002045     PERFORM IMS-GU-W6KVAE01                                              
002046                                                                          
002047     IF SEGMENT-SAKNAS                                                    
002048       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
002049       MOVE 'IDKR'               TO RESP-IDELMT-ERROR                     
002050       PERFORM MFS-RENSA-FAELT-UT                                         
002051     ELSE                                                                 
002052       PERFORM MFS-SPAERRA-FAELT                                          
002053       PERFORM MFS-SPAERRA-FAELT2                                         
002054       MOVE KR-IDKR              TO RESP-IDKR-KEY                         
002055       MOVE KR-IDLOPNRM          TO RESP-IDLOPNRM-UT                      
002056       MOVE KR-IDAVINR           TO RESP-IDAVINR                          
002057       MOVE KR-KVAVIS            TO RESP-KVAVIS                           
002058       MOVE KR-DAAVSDAT          TO WS-DAAVSDAT                           
002059       MOVE WS-DAAVSDAT (3:6)    TO RESP-TIAVSDAT-UPD                     
002060       MOVE KR-IDARTNR           TO RESP-IDARTNR-UT                       
002061       MOVE KR-IDARTNR           TO W-IDARTNR                             
002062       MOVE REQU-IDDC-KEY        TO W-IDDC                                
002063       PERFORM IMS-GU-WDB601                                              
002064       MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                            
002065       IF DCS-UNICODE-IDSKYLT                                             
002066          MOVE 'UTF8'             TO TRAUTF8-KDCP                         
002067       ELSE                                                               
002068          MOVE '278 '             TO TRAUTF8-KDCP                         
002069       END-IF                                                             
002070       PERFORM IMS-GU-BENA-TEXT                                           
002071       IF SEGMENT-FINNS                                                   
002072         MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                   
002073       ELSE                                                               
002074         MOVE SPACE              TO TRAUTF8-TECONV-FROM                   
002075                                    TEXT-BEART                            
002076         MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                          
002077       END-IF                                                             
002078       IF TRAUTF8-TECONV-FROM = SPACES                                    
002079        MOVE 'GB'  TO W-IDSKYLT                                           
002080        MOVE '278' TO TRAUTF8-KDCP                                        
002081        PERFORM IMS-GU-BENA-TEXT                                          
002082        MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                         
002083       END-IF                                                             
002084                                                                          
002085       IF REQU-IDMSGVER = '001'                                           
002086*        CALL FROM WEB, CONVERT TO UNICODE IF NOT ALREADY SO              
002087*        AND STRIP TRAILING SPACE                                         
002088         CALL WTRAUTF8 USING TRAUTF8-AREA                                 
002089         MOVE TRAUTF8-TECONV-TO TO RESP-BEART                             
002090       ELSE                                                               
002091*        CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                     
002092         MOVE TEXT-BEART         TO RESP-BEART                            
002093       END-IF                                                             
002094*                                                                         
002095       COMPUTE WS-DAREGDAT = 99999999 - KR-DAREGDAT-9KOMPL                
002096       MOVE WS-DAREGDAT (3:6)    TO RESP-TIREGDAT                         
002097       MOVE KR-IDLEVNR           TO RESP-IDLEVNR-UT                       
002098       MOVE KR-IDLEVG            TO RESP-IDLEVG-UT                        
002099       MOVE KR-IDLEVNR           TO W-IDLEVNR                             
002100       PERFORM IMS-GU-WLLEVA14                                            
002101       IF SEGMENT-FINNS                                                   
002102         MOVE ADR-BELEV          TO RESP-BELEV                            
002103       ELSE                                                               
002104         MOVE SPACE              TO RESP-BELEV                            
002105       END-IF                                                             
002106       MOVE KR-KDKRSTA           TO RESP-KDKRSTA                          
002107       MOVE KR-KVANTMOT          TO RESP-KVANTMOT-UT                      
002108       MOVE KR-KVART-AAVV        TO RESP-KVART-AAVV                       
002109       MOVE KR-KVART-KONTR       TO RESP-KVART-KONTR-UT                   
002110       MOVE KR-KVART-EJ-GODK     TO RESP-KVART-EJ-GODK-UT                 
002111       MOVE KR-KVART-RET         TO RESP-KVART-RET-UT                     
002112       MOVE KR-KVART-SKROT       TO RESP-KVART-SKROT-UT                   
002113       MOVE KR-KVART-SKROT-LDC   TO RESP-KVART-SKROT-LDC-UT               
002114       MOVE KR-FLKVALSP          TO RESP-FLKVALSP-UT                      
002115       MOVE KR-FLBUFJUS          TO RESP-FLBUFJUS-UT                      
002116       MOVE KR-KVART-KJUST       TO RESP-KVART-KJUST-UT                   
002117       MOVE KR-KVART-BEH         TO RESP-KVART-BEH-UT                     
002118       MOVE KR-KVART-SJUST       TO RESP-KVART-SJUST-UT                   
002119       IF TRYCK-PF11                                                      
002120         PERFORM FB-PRESS-PF11                                            
002121       ELSE                                                               
002122         IF KR-IDKRFEL  NOT = SPACE                                       
002123           MOVE KR-IDKRFEL       TO RESP-IDKRFEL-UPD                      
002124                                    W-IDKRFEL                             
002125           PERFORM IMS-GU-WLXXLA11                                        
002126           MOVE 4826-BEKRFEL     TO RESP-BEKRFEL                          
002127         END-IF                                                           
002128         MOVE KR-BEKRBEH         TO RESP-BEKRBEH-UPD                      
002129         MOVE KR-TEKRPLT         TO RESP-TEKRPLT-UPD                      
002130         PERFORM MFS-RENSA-FAELT-IN                                       
002131       END-IF                                                             
002132                                                                          
002133       MOVE KR-KVKRBEH           TO RESP-KVKRBEH-UT                       
002134       MOVE KR-KDDISP            TO RESP-KDDISP-UPD                       
002135                                                                          
002136       IF KR-IDKRFEL(1:1) = 'P'                                           
002137                                                                          
002138         MOVE MFS-STAENG-FAELT-NOMOD                                      
002139                                 TO RESP-IDKRFEL-UPD-ATTR                 
002140                                    RESP-KVART-SJUST-UPD-ATTR             
002141         IF KR-KDDISP = 01                                                
002142           MOVE KDDISPTEXT-A1    TO RESP-TEDISP                           
002143         END-IF                                                           
002144         IF KR-KDDISP = 03                                                
002145           MOVE KDDISPTEXT-A3    TO RESP-TEDISP                           
002146         END-IF                                                           
002147         IF KR-KDDISP = 04                                                
002148           MOVE KDDISPTEXT-A4    TO RESP-TEDISP                           
002149         END-IF                                                           
002150         IF KR-KDDISP = 05                                                
002151           MOVE KDDISPTEXT-A5    TO RESP-TEDISP                           
002152         END-IF                                                           
002153       ELSE                                                               
002154         IF KR-IDKRFEL(1:1) = 'K'                                         
002155                                                                          
002156* FÅR EJ UPPDATERA ANTALS- ELLER FELKODSFÄLT PÅ ADMINISTRATIVA KR         
002157           MOVE MFS-STAENG-FAELT-NOMOD                                    
002158                                 TO RESP-IDLEVG-UPD-ATTR                  
002159                                    RESP-IDLEVNR-UPD-ATTR                 
002160                                    RESP-IDARTNR-UPD-ATTR                 
002161                                    RESP-KVANTMOT-UPD-ATTR                
002162                                    RESP-KVART-RET-UPD-ATTR               
002163                                    RESP-KVART-SKROT-UPD-ATTR             
002164                                    RESP-KVART-SKROT-LDC-UPD-ATTR         
002165                                    RESP-KVART-KONTR-UPD-ATTR             
002166                                    RESP-KVART-KJUST-UPD-ATTR             
002167                                    RESP-KVART-EJ-GODK-UPD-ATTR           
002168                                    RESP-KVART-BEH-UPD-ATTR               
002169                                    RESP-KVART-SJUST-UPD-ATTR             
002170                                    RESP-IDKRFEL-UPD-ATTR                 
002171                                                                          
002172                                                                          
002173           IF KR-KDDISP = 06                                              
002174             MOVE KDDISPTEXT-A6  TO RESP-TEDISP                           
002175           END-IF                                                         
002176         ELSE                                                             
002177           IF KR-KDDISP = 01                                              
002178             MOVE KDDISPTEXT-T1  TO RESP-TEDISP                           
002179           END-IF                                                         
002180           IF KR-KDDISP = 02                                              
002181             MOVE KDDISPTEXT-T2  TO RESP-TEDISP                           
002182           END-IF                                                         
002183           IF KR-KDDISP = 03                                              
002184             MOVE KDDISPTEXT-T3  TO RESP-TEDISP                           
002185           END-IF                                                         
002186           IF KR-KDDISP = 04                                              
002187             MOVE KDDISPTEXT-T4  TO RESP-TEDISP                           
002188           END-IF                                                         
002189           IF KR-KDDISP = 05                                              
002190             MOVE KDDISPTEXT-T5  TO RESP-TEDISP                           
002191           END-IF                                                         
002192           IF KR-KDDISP = 06                                              
002193             MOVE KDDISPTEXT-T6  TO RESP-TEDISP                           
002194           END-IF                                                         
002195           IF KR-KDDISP = 77                                              
002196             MOVE KDDISPTEXT-T77 TO RESP-TEDISP                           
002197           END-IF                                                         
002198           IF KR-KDDISP = 91                                              
002199             MOVE KDDISPTEXT-T91 TO RESP-TEDISP                           
002200           END-IF                                                         
002201         END-IF                                                           
002202       END-IF                                                             
002203                                                                          
002204       MOVE KR-KDHANDCO          TO RESP-KDHANDCO-UPD                     
002205                                                                          
002206       IF KR-KDHANDCO = 1                                                 
002207         MOVE  KDHANDCOTXT-01    TO RESP-TEHANDCO                         
002208       ELSE                                                               
002209         IF KR-KDHANDCO = 2                                               
002210           MOVE  KDHANDCOTXT-02  TO RESP-TEHANDCO                         
002211         ELSE                                                             
002212           MOVE  SPACE           TO RESP-TEHANDCO                         
002213         END-IF                                                           
002214       END-IF                                                             
002215                                                                          
002216       PERFORM IMS-GNP-W6KVAE14                                           
002217       IF SEGMENT-FINNS                                                   
002218         IF WS-IDSKYLT = 'S '                                             
002219           MOVE JA               TO RESP-FLAGGA-FELTEXT                   
002220         ELSE                                                             
002221           MOVE YES              TO RESP-FLAGGA-FELTEXT                   
002222         END-IF                                                           
002223       ELSE                                                               
002224         MOVE NEJ                TO RESP-FLAGGA-FELTEXT                   
002225       END-IF                                                             
002226                                                                          
002227       MOVE KR-IDARTNR           TO W-IDARTNR                             
002228       PERFORM IMS-GU-WLARTC11                                            
002229       IF SEGMENT-FINNS                                                   
002230         IF ARTC-CLAG-KDFARLIG = +4 OR                                    
002231            ARTC-CLAG-KDFARLIG = +7                                       
002232           MOVE INF-FARLIGT-GODS TO RESP-IDMSG-ERROR                      
002233         END-IF                                                           
002234** VARNA DÅ R40 ORSAKAR MINUS-SALDO                                       
002235         IF KR-IDLOPNRM = ZERO AND                                        
002236            KR-FLKRGODK = NEJ                                             
002237           COMPUTE W-R40-ANTAL = KR-KVART-RET +                           
002238                                 KR-KVART-SKROT - KR-KVART-TIDGK          
002239           END-COMPUTE                                                    
002240           IF W-R40-ANTAL > ZERO                                          
002241             MOVE KR-IDDC  TO WS-IDDC                                     
002242                              W-IDDC                                      
002243             IF NDC-CN OR NDC-US                                          
002244               PERFORM IMS-GU-WDK711                                      
002245               MOVE SLAG-KVLS      TO WS-KVLS                             
002246             ELSE                                                         
002247               MOVE ARTC-CLAG-KVLS TO WS-KVLS                             
002248             END-IF                                                       
002249             IF W-R40-ANTAL > WS-KVLS                                     
002250               MOVE QUANT-TOO-BIG                                         
002251                                 TO RESP-IDMSG-ERROR                      
002252             END-IF                                                       
002253           END-IF                                                         
002254         END-IF                                                           
002255       ELSE                                                               
002256         MOVE INF-ARTIKEL-SAKNAS TO RESP-IDMSG-INFO                       
002257         MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                     
002258       END-IF                                                             
002259       PERFORM FA-LAES-VISA-KVAL-ANTAL-KR                                 
002260     END-IF                                                               
002261     .                                                                    
002262     EJECT                                                                
002263 FA-LAES-VISA-KVAL-ANTAL-KR SECTION.                                      
002264                                                                          
002265     IF KR-IDLOPNRM > +0                                                  
002266       MOVE KR-IDLOPNRM          TO W-IDLOPNRM-CSEQ                       
002267       MOVE KR-DAAVSDAT          TO W-DAAVSDAT-CSEQ                       
002268       PERFORM IMS-GU-W6KVAI01                                            
002269       IF SEGMENT-FINNS                                                   
002270         IF CSEQ-KR-IDKR NOT = W-IDKR                                     
002271           IF CSEQ-KR-FLANNULL = NEJ                                      
002272             IF CSEQ-KR-IDKRFEL  = 'PA' OR 'PB' OR 'K '                   
002273               MOVE INF-ANT-KR-FINNS                                      
002274                                 TO RESP-IDMSG-INFO                       
002275             ELSE                                                         
002276               MOVE INF-KVAL-KR-FINNS                                     
002277                                 TO RESP-IDMSG-INFO                       
002278             END-IF                                                       
002279             MOVE CSEQ-KR-IDKR   TO RESP-IDKR-INFO-MSG                    
002280           END-IF                                                         
002281         ELSE                                                             
002282           PERFORM IMS-GN-W6KVAI01                                        
002283           IF SEGMENT-FINNS                                               
002284             IF CSEQ-KR-FLANNULL = NEJ                                    
002285               IF CSEQ-KR-IDKRFEL = 'PA' OR 'PB' OR 'K '                  
002286                 MOVE INF-ANT-KR-FINNS                                    
002287                                 TO RESP-IDMSG-INFO                       
002288               ELSE                                                       
002289                 MOVE INF-KVAL-KR-FINNS                                   
002290                                 TO RESP-IDMSG-INFO                       
002291               END-IF                                                     
002292               MOVE CSEQ-KR-IDKR TO RESP-IDKR-INFO-MSG                    
002293             END-IF                                                       
002294           END-IF                                                         
002295         END-IF                                                           
002296       END-IF                                                             
002297     END-IF                                                               
002298     .                                                                    
002299     EJECT                                                                
002300 FB-PRESS-PF11 SECTION.                                                   
002301                                                                          
002302     IF REQU-IDKRFEL-UPD = ALL '+'                                        
002303       IF KR-IDKRFEL NOT = SPACE                                          
002304         MOVE KR-IDKRFEL         TO RESP-IDKRFEL-UPD                      
002305                                    W-IDKRFEL                             
002306         PERFORM IMS-GU-WLXXLA11                                          
002307         MOVE 4826-BEKRFEL       TO RESP-BEKRFEL                          
002308       END-IF                                                             
002309     ELSE                                                                 
002310       MOVE REQU-IDKRFEL-UPD     TO RESP-IDKRFEL-UPD                      
002311                                    W-IDKRFEL                             
002312       MOVE MFS-ADD-LAES-IN-FAELT                                         
002313                                 TO RESP-IDKRFEL-UPD-ATTR                 
002314       PERFORM IMS-GU-WLXXLA11                                            
002315       IF SEGMENT-FINNS                                                   
002316         MOVE 4826-BEKRFEL       TO RESP-BEKRFEL                          
002317       ELSE                                                               
002318         MOVE SPACE              TO RESP-BEKRFEL                          
002319       END-IF                                                             
002320     END-IF                                                               
002321     IF REQU-BEKRBEH-UPD = ALL '+'                                        
002322       MOVE KR-BEKRBEH           TO RESP-BEKRBEH-UPD                      
002323     ELSE                                                                 
002324       MOVE REQU-BEKRBEH-UPD     TO RESP-BEKRBEH-UPD                      
002325       MOVE MFS-ADD-LAES-IN-FAELT                                         
002326                                 TO RESP-BEKRBEH-UPD-ATTR                 
002327     END-IF                                                               
002328     IF REQU-TEKRPLT-UPD = ALL '+'                                        
002329       MOVE KR-TEKRPLT           TO RESP-TEKRPLT-UPD                      
002330     ELSE                                                                 
002331       MOVE REQU-TEKRPLT-UPD     TO RESP-TEKRPLT-UPD                      
002332       MOVE MFS-ADD-LAES-IN-FAELT                                         
002333                                 TO RESP-TEKRPLT-UPD-ATTR                 
002334     END-IF                                                               
002335     IF REQU-KDPERSON-UPD NOT = ALL '+'                                   
002336       IF REQU-KDPERSON-UPD NUMERIC                                       
002337         MOVE REQU-KDPERSON-UPD  TO RESP-KDPERSON-UPD                     
002338         MOVE MFS-ADD-LAES-IN-FAELT                                       
002339                                 TO RESP-KDPERSON-UPD-ATTR                
002340       ELSE                                                               
002341         MOVE ALL-SPACE          TO RESP-KDPERSON-UPD                     
002342       END-IF                                                             
002343     END-IF                                                               
002344     .                                                                    
002345     EJECT                                                                
002346 S01-UPPD-JUSTERING SECTION.                                              
002347                                                                          
002348     PERFORM S01A-INITIERA-W6H713                                         
002349     IF REQU-KVART-RET-UPD NOT = ALL '+'                                  
002350       IF REQU-KVART-RET-UPD NOT = KR-KVART-RET                           
002351         COMPUTE JUST-KVART-RET =                                         
002352                          REQU-KVART-RET-UPD - KR-KVART-RET               
002353       END-IF                                                             
002354     END-IF                                                               
002355                                                                          
002356     IF REQU-KVART-SKROT-UPD NOT = ALL '+'                                
002357       IF REQU-KVART-SKROT-UPD NOT = KR-KVART-SKROT                       
002358         COMPUTE JUST-KVART-SKROT =                                       
002359                  REQU-KVART-SKROT-UPD - KR-KVART-SKROT                   
002360       END-IF                                                             
002361     END-IF                                                               
002362                                                                          
002363     IF REQU-KVART-SJUST-UPD NOT = ALL '+'                                
002364       IF WS-KVART-SJUST NOT = KR-KVART-SJUST                             
002365         IF WS-KVART-SJUST > +0 AND KR-KVART-SJUST > +0 OR                
002366            WS-KVART-SJUST < +0 AND KR-KVART-SJUST < +0                   
002367           COMPUTE JUST-KVART-SJUST =                                     
002368                     WS-KVART-SJUST - KR-KVART-SJUST                      
002369         ELSE                                                             
002370           COMPUTE JUST-KVART-SJUST =                                     
002371                   - (KR-KVART-SJUST - WS-KVART-SJUST)                    
002372         END-IF                                                           
002373       END-IF                                                             
002374     END-IF                                                               
002375                                                                          
002376     IF JUST-KVART-RET   NOT = +0 OR                                      
002377        JUST-KVART-SKROT NOT = +0 OR                                      
002378        JUST-KVART-SJUST NOT = +0                                         
002379       MOVE '1'                  TO JUST-KDSEGKEY                         
002380       MOVE WS-KDKRJUST          TO JUST-KDKRJUST                         
002381       MOVE KR-KDKRSTA           TO JUST-KDKRSTA                          
002382       MOVE DATUM                TO JUST-TIREGDAT                         
002383       PERFORM IMS-ISRT-W6KVAE13                                          
002384     END-IF                                                               
002385                                                                          
002386     PERFORM IMS-GHU-W6KVAE01                                             
002387     .                                                                    
002388     EJECT                                                                
002389 S01A-INITIERA-W6H713 SECTION.                                            
002390     MOVE SPACE                  TO JUST-KDKRJUST                         
002391                                    JUST-KDKRSTA                          
002392     MOVE ZERO                   TO JUST-KVARBTID                         
002393                                    JUST-KVART-RET                        
002394                                    JUST-KVART-SJUST                      
002395                                    JUST-KVART-SKROT                      
002396                                    JUST-SUMAT                            
002397                                    JUST-SUOMK                            
002398                                    JUST-TIREGDAT                         
002399     .                                                                    
002400     EJECT                                                                
002401 S10-KONV-IDFS      SECTION.                                              
002402                                                                          
002403     MOVE ZERO                   TO W-IDAVINR                             
002404     MOVE +8                     TO IDFS-IX                               
002405     MOVE +7                     TO IDAVINR-IX                            
002406     PERFORM UNTIL IDFS-IX < +1                                           
002407       IF IDAVINR-IX > +0                                                 
002408         IF W-IDFS-TKN (IDFS-IX) NUMERIC                                  
002409           MOVE W-IDFS-TKN (IDFS-IX)                                      
002410                                 TO W-IDAVINR-TKN (IDAVINR-IX)            
002411           SUBTRACT 1          FROM IDAVINR-IX                            
002412         END-IF                                                           
002413       END-IF                                                             
002414       SUBTRACT +1             FROM IDFS-IX                               
002415     END-PERFORM                                                          
002416     .                                                                    
002417     EJECT                                                                
002418 MFS-RENSA-FAELT-UT SECTION.                                              
002419                                                                          
002420*    --- ALLA UTDATA-FÄLT                                                 
002421*    --- INKL. BLÄDDRINGSNYCKLAR                                          
002422     IF REQU-IDMSGVER = 001                                               
002423       MOVE ALL-UTF8-SPACE       TO RESP-BEART                            
002424     ELSE                                                                 
002425       MOVE ALL-SPACE            TO RESP-BEART                            
002426     END-IF                                                               
002427     MOVE ALL-SPACE              TO RESP-IDLOPNRM-UT                      
002428                                    RESP-IDAVINR                          
002429                                    RESP-KVAVIS                           
002430                                    RESP-TIAVSDAT-UPD                     
002431                                    RESP-IDARTNR-UT                       
002432                                    RESP-IDLEVNR-UT                       
002433                                    RESP-IDLEVG-UT                        
002434                                    RESP-BELEV                            
002435                                    RESP-KVANTMOT-UT                      
002436                                    RESP-KVART-AAVV                       
002437                                    RESP-KVART-KONTR-UT                   
002438                                    RESP-KVART-EJ-GODK-UT                 
002439                                    RESP-KVART-RET-UT                     
002440                                    RESP-KVART-SKROT-UT                   
002441                                    RESP-KVART-SKROT-LDC-UT               
002442                                    RESP-FLKVALSP-UT                      
002443                                    RESP-FLBUFJUS-UT                      
002444                                    RESP-KVART-KJUST-UT                   
002445                                    RESP-KVART-BEH-UT                     
002446                                    RESP-KVART-SJUST-UT                   
002447                                    RESP-IDKRFEL-UPD                      
002448                                    RESP-BEKRFEL                          
002449                                    RESP-KDDISP-UPD                       
002450                                    RESP-TEDISP                           
002451                                    RESP-KDHANDCO-UPD                     
002452                                    RESP-TEHANDCO                         
002453                                    RESP-FLAGGA-FELTEXT                   
002454                                    RESP-BEKRBEH-UPD                      
002455                                    RESP-KVKRBEH-UT                       
002456                                    RESP-TEKRPLT-UPD                      
002457                                    RESP-TIREGDAT                         
002458                                    RESP-KDKRSTA                          
002459     .                                                                    
002460     SKIP2                                                                
002461 MFS-RENSA-FAELT-IN SECTION.                                              
002462                                                                          
002463*    --- ALLA INDATA-FÄLT                                                 
002464     MOVE ALL-SPACE         TO                                            
002465                               RESP-IDLOPNRM-UPD                          
002466                               RESP-IDARTNR-UPD                           
002467                               RESP-IDLEVNR-UPD                           
002468                               RESP-IDLEVG-UPD                            
002469                               RESP-KVANTMOT-UPD                          
002470                               RESP-KVART-KONTR-UPD                       
002471                               RESP-KVART-EJ-GODK-UPD                     
002472                               RESP-KVART-RET-UPD                         
002473                               RESP-KVART-SKROT-UPD                       
002474                               RESP-KVART-SKROT-LDC-UPD                   
002475                               RESP-FLKVALSP-UPD                          
002476                               RESP-FLBUFJUS-UPD                          
002477                               RESP-KVART-KJUST-UPD                       
002478                               RESP-KVART-BEH-UPD                         
002479                               RESP-KVART-SJUST-UPD                       
002480                               RESP-KVKRBEH-UPD                           
002481                               RESP-KDPERSON-UPD                          
002482     .                                                                    
002483     EJECT                                                                
002484 MFS-ROER-EJ-FAELT-IN-UT  SECTION.                                        
002485                                                                          
002486*    --- ALLA UTDATA-FÄLT                                                 
002487*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
002488     IF REQU-IDMSGVER = 001                                               
002489       MOVE ALL-UTF8-PLUS        TO RESP-BEART                            
002490     ELSE                                                                 
002491       MOVE ALL-SPACE            TO RESP-BEART                            
002492     END-IF                                                               
002493     MOVE ALL-PLUS          TO                                            
002494                               RESP-IDKR-KEY                              
002495                               RESP-IDLOPNRM-UT                           
002496                               RESP-IDAVINR                               
002497                               RESP-KVAVIS                                
002498                               RESP-TIAVSDAT-UPD                          
002499                               RESP-IDARTNR-UT                            
002500                               RESP-TIREGDAT                              
002501                               RESP-IDLEVNR-UT                            
002502                               RESP-IDLEVG-UT                             
002503                               RESP-BELEV                                 
002504                               RESP-KDKRSTA                               
002505                               RESP-KVANTMOT-UT                           
002506                               RESP-KVART-AAVV                            
002507                               RESP-KVART-KONTR-UT                        
002508                               RESP-KVART-EJ-GODK-UT                      
002509                               RESP-KVART-RET-UT                          
002510                               RESP-KVART-SKROT-UT                        
002511                               RESP-KVART-SKROT-LDC-UT                    
002512                               RESP-FLKVALSP-UT                           
002513                               RESP-FLBUFJUS-UT                           
002514                               RESP-KVART-KJUST-UT                        
002515                               RESP-KVART-BEH-UT                          
002516                               RESP-KVART-SJUST-UT                        
002517                               RESP-IDKRFEL-UPD                           
002518                               RESP-BEKRFEL                               
002519                               RESP-KDDISP-UPD                            
002520                               RESP-TEDISP                                
002521                               RESP-KDHANDCO-UPD                          
002522                               RESP-FLAGGA-FELTEXT                        
002523                               RESP-KDPERSON-UPD                          
002524                               RESP-BEKRBEH-UPD                           
002525                               RESP-KVKRBEH-UT                            
002526                               RESP-TEKRPLT-UPD                           
002527                                                                          
002528                               RESP-IDLOPNRM-UPD                          
002529                               RESP-IDARTNR-UPD                           
002530                               RESP-IDLEVNR-UPD                           
002531                               RESP-IDLEVG-UPD                            
002532                               RESP-KVANTMOT-UPD                          
002533                               RESP-KVART-KONTR-UPD                       
002534                               RESP-KVART-EJ-GODK-UPD                     
002535                               RESP-KVART-RET-UPD                         
002536                               RESP-KVART-SKROT-UPD                       
002537                               RESP-KVART-SKROT-LDC-UPD                   
002538                               RESP-FLKVALSP-UPD                          
002539                               RESP-FLBUFJUS-UPD                          
002540                               RESP-KVART-KJUST-UPD                       
002541                               RESP-KVART-BEH-UPD                         
002542                               RESP-KVART-SJUST-UPD                       
002543                               RESP-KVKRBEH-UPD                           
002544     .                                                                    
002545     EJECT                                                                
002546 MFS-FORM-ATTR SECTION.                                                   
002547                                                                          
002548*    --- ALLA INDATA-FÄLT                                                 
002549     MOVE MFS-FORMATETS-ATTR TO RESP-IDLOPNRM-UPD-ATTR                    
002550                                RESP-IDARTNR-UPD-ATTR                     
002551                                RESP-IDLEVNR-UPD-ATTR                     
002552                                RESP-IDLEVG-UPD-ATTR                      
002553                                RESP-KVANTMOT-UPD-ATTR                    
002554                                RESP-KVART-KONTR-UPD-ATTR                 
002555                                RESP-KVART-EJ-GODK-UPD-ATTR               
002556                                RESP-KVART-RET-UPD-ATTR                   
002557                                RESP-KVART-SKROT-UPD-ATTR                 
002558                                RESP-KVART-SKROT-LDC-UPD-ATTR             
002559                                RESP-KVART-KJUST-UPD-ATTR                 
002560                                RESP-KVART-BEH-UPD-ATTR                   
002561                                RESP-KVART-SJUST-UPD-ATTR                 
002562                                RESP-IDKRFEL-UPD-ATTR                     
002563                                RESP-KDDISP-UPD-ATTR                      
002564                                RESP-KDHANDCO-UPD-ATTR                    
002565                                RESP-KDPERSON-UPD-ATTR                    
002566                                RESP-BEKRBEH-UPD-ATTR                     
002567                                RESP-KVKRBEH-UPD-ATTR                     
002568                                RESP-TEKRPLT-UPD-ATTR                     
002569     .                                                                    
002570     SKIP2                                                                
002571 MFS-SPAERRA-FAELT SECTION.                                               
002572                                                                          
002573     IF KR-IDKR > 1                                                       
002574     AND (KR-FLKRGODK = 'J' OR KR-FLANNULL = JA)                          
002575        MOVE MFS-STAENG-FAELT-NOMOD TO RESP-IDLEVG-UPD-ATTR               
002576                                       RESP-IDLEVNR-UPD-ATTR              
002577                                       RESP-IDARTNR-UPD-ATTR              
002578                                       RESP-KVANTMOT-UPD-ATTR             
002579                                       RESP-KVART-RET-UPD-ATTR            
002580                                       RESP-KVART-SKROT-UPD-ATTR          
002581                                    RESP-KVART-SKROT-LDC-UPD-ATTR         
002582                                       RESP-KVART-KONTR-UPD-ATTR          
002583                                       RESP-KVART-KJUST-UPD-ATTR          
002584                                       RESP-KVART-EJ-GODK-UPD-ATTR        
002585                                       RESP-KVART-BEH-UPD-ATTR            
002586                                       RESP-KVART-SJUST-UPD-ATTR          
002587                                       RESP-IDKRFEL-UPD-ATTR              
002588                                       RESP-KDDISP-UPD-ATTR               
002589                                       RESP-KDHANDCO-UPD-ATTR             
002590                                       RESP-KDPERSON-UPD-ATTR             
002591                                       RESP-BEKRBEH-UPD-ATTR              
002592                                       RESP-KVKRBEH-UPD-ATTR              
002593     END-IF                                                               
002594     .                                                                    
002595     EJECT                                                                
002596 MFS-SPAERRA-FAELT2 SECTION.                                              
002597     IF KR-KDKRSTA > 2                                                    
002598        MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLKVALSP-UPD-ATTR             
002599                                       RESP-FLBUFJUS-UPD-ATTR             
002600     END-IF                                                               
002601     IF REQU-IDLOPNRM-UPD > 0                                             
002602     OR KR-IDLOPNRM > 0                                                   
002603        MOVE MFS-STAENG-FAELT-NOMOD TO RESP-FLKVALSP-UPD-ATTR             
002604                                       RESP-FLBUFJUS-UPD-ATTR             
002605     END-IF                                                               
002606     .                                                                    
002607     EJECT                                                                
002608* --- IMS SEKTIONER ---                                                   
002609     SKIP3                                                                
002610 IMS-GU-W6LOPB01 SECTION.                                                 
002611     STRING 'W6LOPB01(W6GXKEY  =' W-W6GX-6001-KEY-X ')'                   
002612          DELIMITED BY SIZE INTO SSA1                                     
002613     MOVE '  ' TO GODK-STATUSKODER                                        
002614     CALL CBLTDLI USING GU LOPB-PCB DLI-IO-AREA8 SSA1                     
002615     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
002616     PERFORM IMS-STATUSKONTROLL                                           
002617     .                                                                    
002618     SKIP2                                                                
002619 IMS-GHNP-W6LOPB11 SECTION.                                               
002620     MOVE 'W6LOPB11 '         TO SSA1                                     
002621     MOVE '  ' TO GODK-STATUSKODER                                        
002622     CALL CBLTDLI USING GHNP LOPB-PCB DLI-IO-AREA8 SSA1                   
002623     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
002624     PERFORM IMS-STATUSKONTROLL                                           
002625     .                                                                    
002626     SKIP2                                                                
002627 IMS-REPL-W6LOPB11 SECTION.                                               
002628     MOVE '  ' TO GODK-STATUSKODER                                        
002629     CALL CBLTDLI USING REPL LOPB-PCB DLI-IO-AREA8                        
002630     MOVE LOPB-STATUS-CODE TO STATUS-WS                                   
002631     PERFORM IMS-STATUSKONTROLL                                           
002632     .                                                                    
002633     EJECT                                                                
002634 IMS-GU-W6KVAE01 SECTION.                                                 
002635     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
002636          DELIMITED BY SIZE INTO SSA1                                     
002637     MOVE '  GE' TO GODK-STATUSKODER                                      
002638     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA1 SSA1                     
002639     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002640     PERFORM IMS-STATUSKONTROLL                                           
002641     .                                                                    
002642     SKIP2                                                                
002643 IMS-GNP-W6KVAE12 SECTION.                                                
002644     MOVE 'W6KVAE12 '         TO SSA1                                     
002645     MOVE '  GE' TO GODK-STATUSKODER                                      
002646     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA1 SSA1                    
002647     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002648     PERFORM IMS-STATUSKONTROLL                                           
002649     .                                                                    
002650     EJECT                                                                
002651 IMS-GU-W6KVAI01 SECTION.                                                 
002652     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
002653          DELIMITED BY SIZE INTO SSA1                                     
002654     MOVE '  GE' TO GODK-STATUSKODER                                      
002655     CALL CBLTDLI USING GU KVAI-PCB DLI-IO-AREA13 SSA1                    
002656     MOVE KVAI-STATUS-CODE TO STATUS-WS                                   
002657     PERFORM IMS-STATUSKONTROLL                                           
002658     .                                                                    
002659     SKIP2                                                                
002660 IMS-GN-W6KVAI01 SECTION.                                                 
002661     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
002662          DELIMITED BY SIZE INTO SSA1                                     
002663     MOVE '  GE' TO GODK-STATUSKODER                                      
002664     CALL CBLTDLI USING GN KVAI-PCB DLI-IO-AREA13 SSA1                    
002665     MOVE KVAI-STATUS-CODE TO STATUS-WS                                   
002666     PERFORM IMS-STATUSKONTROLL                                           
002667     .                                                                    
002668     EJECT                                                                
002669 IMS-GHU-W6KVAE01 SECTION.                                                
002670     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
002671          DELIMITED BY SIZE INTO SSA1                                     
002672     MOVE '  GE' TO GODK-STATUSKODER                                      
002673     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA1 SSA1                    
002674     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002675     PERFORM IMS-STATUSKONTROLL                                           
002676     .                                                                    
002677     SKIP2                                                                
002678 IMS-REPL-W6KVAE01 SECTION.                                               
002679     MOVE '  ' TO GODK-STATUSKODER                                        
002680     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA1                        
002681     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002682     PERFORM IMS-STATUSKONTROLL                                           
002683     .                                                                    
002684     SKIP2                                                                
002685 IMS-ISRT-W6KVAE01 SECTION.                                               
002686     MOVE 'W6KVAE01 '         TO SSA1                                     
002687     MOVE '  ' TO GODK-STATUSKODER                                        
002688     CALL CBLTDLI USING ISRT KVAE-PCB DLI-IO-AREA1 SSA1                   
002689     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002690     PERFORM IMS-STATUSKONTROLL                                           
002691     .                                                                    
002692     SKIP2                                                                
002693 IMS-ISRT-W6KVAE13 SECTION.                                               
002694     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
002695          DELIMITED BY SIZE INTO SSA1                                     
002696     MOVE 'W6KVAE13 '         TO SSA2                                     
002697     MOVE '  ' TO GODK-STATUSKODER                                        
002698     CALL CBLTDLI USING ISRT KVAE-PCB DLI-IO-AREA1 SSA1 SSA2              
002699     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002700     PERFORM IMS-STATUSKONTROLL                                           
002701     .                                                                    
002702     EJECT                                                                
002703 IMS-GNP-W6KVAE14 SECTION.                                                
002704     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
002705          DELIMITED BY SIZE INTO SSA1                                     
002706     MOVE 'W6KVAE14 '         TO SSA2                                     
002707     MOVE '  GEGB' TO GODK-STATUSKODER                                    
002708     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA2 SSA1 SSA2               
002709     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
002710     PERFORM IMS-STATUSKONTROLL                                           
002711     .                                                                    
002712     EJECT                                                                
002713 IMS-GU-WLLEVA14 SECTION.                                                 
002714     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
002715          DELIMITED BY SIZE INTO SSA1                                     
002716     MOVE 'WLLEVA14 '         TO SSA2                                     
002717     MOVE '  GE' TO GODK-STATUSKODER                                      
002718     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1 SSA2                
002719     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
002720     PERFORM IMS-STATUSKONTROLL                                           
002721     .                                                                    
002722     EJECT                                                                
002723 IMS-GU-W6LEVA11 SECTION.                                                 
002724     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
002725          DELIMITED BY SIZE INTO SSA1                                     
002726     STRING 'W6LEVA11(IDLEVG   =' W-IDLEVG-X ')'                          
002727          DELIMITED BY SIZE INTO SSA2                                     
002728     MOVE '  GE' TO GODK-STATUSKODER                                      
002729     CALL CBLTDLI USING GU W6F1-PCB DLI-IO-AREA11 SSA1 SSA2               
002730     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
002731     PERFORM IMS-STATUSKONTROLL                                           
002732     .                                                                    
002733     EJECT                                                                
002734 IMS-GU-WDK711 SECTION.                                                   
002735     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
002736       DELIMITED BY SIZE INTO SSA1                                        
002737     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
002738       DELIMITED BY SIZE INTO SSA2                                        
002739     MOVE '  '                 TO GODK-STATUSKODER                        
002740     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
002741     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
002742     PERFORM IMS-STATUSKONTROLL                                           
002743     .                                                                    
002744     SKIP2                                                                
002745 IMS-GU-WLARTC01 SECTION.                                                 
002746     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
002747          DELIMITED BY SIZE INTO SSA1                                     
002748     MOVE '  GE' TO GODK-STATUSKODER                                      
002749     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1                     
002750     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
002751     PERFORM IMS-STATUSKONTROLL                                           
002752     .                                                                    
002753     SKIP2                                                                
002754 IMS-GU-WLARTC11 SECTION.                                                 
002755     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
002756          DELIMITED BY SIZE INTO SSA1                                     
002757     MOVE 'WLARTC11 ' TO SSA2                                             
002758     MOVE '  GE' TO GODK-STATUSKODER                                      
002759     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
002760     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
002761     PERFORM IMS-STATUSKONTROLL                                           
002762     .                                                                    
002763     EJECT                                                                
002764 IMS-GU-WLXXLA11 SECTION.                                                 
002765     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
002766          DELIMITED BY SIZE INTO SSA1                                     
002767     STRING 'WLXXLA11(WDGXKEY  =' W-WDGX-4826-KEY-X ')'                   
002768          DELIMITED BY SIZE INTO SSA2                                     
002769     MOVE '  GE' TO GODK-STATUSKODER                                      
002770     CALL CBLTDLI USING GU XXLA-PCB DLI-IO-AREA5 SSA1 SSA2                
002771     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
002772     PERFORM IMS-STATUSKONTROLL                                           
002773     .                                                                    
002774     EJECT                                                                
002775 IMS-GU-WDP311 SECTION.                                                   
002776     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
002777          DELIMITED BY SIZE INTO SSA1                                     
002778     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
002779          DELIMITED BY SIZE INTO SSA2                                     
002780     MOVE '  GE' TO GODK-STATUSKODER                                      
002781     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
002782     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
002783     PERFORM IMS-STATUSKONTROLL                                           
002784     .                                                                    
002785     EJECT                                                                
002786 IMS-GU-BENA-TEXT SECTION.                                                
002787     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
002788            DELIMITED BY SIZE INTO SSA1                                   
002789     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
002790            DELIMITED BY SIZE INTO SSA2                                   
002791     MOVE '  GE'                TO GODK-STATUSKODER                       
002792     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA7 SSA1 SSA2                
002793     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
002794     PERFORM IMS-STATUSKONTROLL                                           
002795     .                                                                    
002796     EJECT                                                                
002797 IMS-GU-WLINLE21   SECTION.                                               
002798     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
002799          DELIMITED BY SIZE INTO SSA1                                     
002800     MOVE 'WLINLE11 '         TO SSA2                                     
002801     STRING 'WLINLE21(IDLEVNR  =' W-IDLEVNR-X ')'                         
002802          DELIMITED BY SIZE INTO SSA3                                     
002803     MOVE '  GE' TO GODK-STATUSKODER                                      
002804     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA10 SSA1 SSA2 SSA3          
002805     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
002806     PERFORM IMS-STATUSKONTROLL                                           
002807     .                                                                    
002808     SKIP2                                                                
002809 IMS-GU-W6INLA11 SECTION.                                                 
002810     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X  ')'                       
002811          DELIMITED BY SIZE INTO SSA1                                     
002812     STRING 'W6INLA11(IDRADNRI =' W-IDRADNRI-X  ')'                       
002813          DELIMITED BY SIZE INTO SSA2                                     
002814     MOVE '  ' TO GODK-STATUSKODER                                        
002815     CALL CBLTDLI USING GU W6INLA-PCB DLI-IO-AREA12 SSA1 SSA2             
002816     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
002817     PERFORM IMS-STATUSKONTROLL                                           
002818     .                                                                    
002819     EJECT                                                                
002820 IMS-GU-W6INLC01 SECTION.                                                 
002821     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
002822          DELIMITED BY SIZE INTO SSA1                                     
002823     MOVE '  GE' TO GODK-STATUSKODER                                      
002824     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA12 SSA1                    
002825     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
002826     PERFORM IMS-STATUSKONTROLL                                           
002827     .                                                                    
002828     EJECT                                                                
002829 IMS-GU-UPFA01 SECTION.                                                   
002830     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
002831          DELIMITED BY SIZE INTO SSA1                                     
002832     MOVE '  GE' TO GODK-STATUSKODER                                      
002833     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA14 SSA1                    
002834     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
002835     PERFORM IMS-STATUSKONTROLL                                           
002836     .                                                                    
002837     EJECT                                                                
002838 IMS-GU-WDB601 SECTION.                                                   
002839     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
002840          DELIMITED BY SIZE INTO SSA1                                     
002841     MOVE '  GE' TO GODK-STATUSKODER                                      
002842     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA15 SSA1                    
002843     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
002844     PERFORM IMS-STATUSKONTROLL                                           
002845     .                                                                    
002846     EJECT                                                                
002847 IMS-GU-WDL611 SECTION.                                                   
002848     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
002849          DELIMITED BY SIZE INTO SSA1                                     
002850     STRING 'WDL611  (IDDC     =' W-IDDC-X                                
002851                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
002852          DELIMITED BY SIZE INTO SSA2                                     
002853     MOVE '  GE' TO GODK-STATUSKODER                                      
002854     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL611 SSA1 SSA2               
002855     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
002856     PERFORM IMS-STATUSKONTROLL                                           
002857     .                                                                    
002858     EJECT                                                                
002859 IMS-STATUSKONTROLL SECTION.                                              
002860                                                                          
002861     SET STATUS-IX TO 1                                                   
002862     SEARCH GODK-STATUS                                                   
002863       AT END                                                             
002864         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002865         DELIMITED BY SIZE INTO FELTEXT                                   
002866         CALL FELLOG                                                      
002867       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
002868     END-SEARCH                                                           
002869     .                                                                    
