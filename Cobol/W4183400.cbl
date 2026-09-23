000001 ID DIVISION.                                                             
000003                                                                          
000004 PROGRAM-ID.     W4183400.                                                
000005 AUTHOR.         SUSANNE OLSSON.                                          
000006 DATE-WRITTEN.   02/04/30.                                                
000007 DATE-COMPILED.                                                           
000008                                                                          
000009*    FUNKTION:                                                            
000010*        INFIL MED POSTER SOM ÄR KREDITERADE I BILL-IT.                   
000011*        SKAPAR KREDITINFO. TILL VIPS                                     
000012*        SKAPAR KREDITINFO. TILL EKONOMI FÖR DIREKTLEVERANSER SOM         
000013*        SKALL GÅ VIDARE MOT LEVERANTÖREN.                                
000014*        SKAPAR EN UTFIL MED POSTER SOM SKALL UPPDATERA BASER.            
000015*        SKAPAR LAB-TRANS FÖR REFILL OCH VOR USA/CAN TILL EKONOMI         
000016*        RUTIN W510D2                                                     
000017*                                                                         
000018*    E-TRACKER 1752877 DATUM 20050215                                     
000019*    E-TRACKER 1658417 DATUM 20060302                                     
000020*    E-TRACKER 850114  DATUM 20070403                                     
000021*    E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1  DAT 2011-12-15         
000022*                                                                         
000023*    ÄNDRING: 2004-10 FÖR CENTRAL PRICING NA                              
000024*        SKAPAR LAB-TRANSAR TILL EKONOMI NORDAMERIKA.                     
000025*                                                                         
000026*        PROGRAMMET LÄSER      WDA2                                       
000027*        PROGRAMMET LÄSER      WDK6                                       
000028*        PROGRAMMET LÄSER      WDK7                                       
000029*        PROGRAMMET LÄSER      WDL5                                       
000030*        PROGRAMMET LÄSER      WDB2                                       
000031*                                                                         
000032*    ABENDKODER:                                                          
000033*        U0016 -  . . . .                                                 
000034*        U1000 -  . . . .                                                 
000035*                                                                         
000036                                                                          
000037     SKIP3                                                                
000038 ENVIRONMENT DIVISION.                                                    
000039     SKIP2                                                                
000040 INPUT-OUTPUT SECTION.                                                    
000041                                                                          
000042 FILE-CONTROL.                                                            
000043     SKIP2                                                                
000044*          --- KREDITPOSTER FRÅN BILLIT                                   
000045     SELECT W41832                     ASSIGN TO W41834D1.                
000046     SKIP2                                                                
000047*          --- KREDITINFO TILL EKONOMI FÖR DIREKTLEV. MOT LEVERANT        
000048     SELECT W418AJ                     ASSIGN TO W41834D2.                
000049     SKIP2                                                                
000050*          --- KREDITINFO TILL VIPS                                       
000051     SELECT W418AK                     ASSIGN TO W41834D3.                
000052     SKIP2                                                                
000053*          --- KREDITPOSTER SOM SKALL UPPDATERA SALDON, SAP/R3 ETC        
000054     SELECT W418AL                     ASSIGN TO W41834D4.                
000055     EJECT                                                                
000056     SKIP2                                                                
000057*          --- TILL EKONOMI-LAB  KONTERING FÖR USA/CAN.                   
000058     SELECT W418AO                     ASSIGN TO W41834D6.                
000059     EJECT                                                                
000060     SKIP2                                                                
000061     EJECT                                                                
000062 DATA DIVISION.                                                           
000063     SKIP2                                                                
000064 FILE SECTION.                                                            
000065     SKIP3                                                                
000066 FD  W41832                                                               
000067     RECORDING       F                                                    
000068     BLOCK CONTAINS  0.                                                   
000069                                                                          
000070*01  -COPY W41832      -L.                                                
000071     SKIP3                                                                
000072 FD  W418AK                                                               
000073     RECORDING       V                                                    
000074     BLOCK CONTAINS  0.                                                   
000075                                                                          
000076*01  POST -COPY W461021 -PRE  UTAK-  -L.                                  
000077     SKIP3                                                                
000078 FD  W418AJ                                                               
000079     RECORDING       F                                                    
000080     BLOCK CONTAINS  0.                                                   
000081                                                                          
000082*01  POST -COPY W41843 -PRE  UTAJ-  -L.                                   
000083     SKIP3                                                                
000084 FD  W418AL                                                               
000085     RECORDING       V                                                    
000086     BLOCK CONTAINS  0.                                                   
000087                                                                          
000088*01  POST -COPY W41834A -PRE  UT34A-  -L.                                 
000089                                                                          
000090*01  POST -COPY W41834B -PRE  UT34B-  -L.                                 
000091     SKIP3                                                                
000092                                                                          
000093 FD  W418AO                                                               
000094     RECORDING       F                                                    
000095     BLOCK CONTAINS  0.                                                   
000096                                                                          
000097*01  POST -COPY W41833  -PRE  720-   -L.                                  
000098     EJECT                                                                
000099 WORKING-STORAGE SECTION.                                                 
000100                                                                          
000101 77  IDPGM                       PIC X(8)    VALUE 'W4183400'.            
000110 77  JA                          PIC X       VALUE 'J'.                   
000111 77  NEJ                         PIC X       VALUE 'N'.                   
000112 77  POSTER-FINNS                PIC X       VALUE 'N'.                   
000113 77  NY-KNOTA                    PIC X(1)    VALUE 'J'.                   
000114 77  W-SPAR-PRFRAKT              PIC 9(7)V9(2) VALUE ZERO.                
000115 77  W-SPAR-PRFOERS              PIC 9(7)V9(2) VALUE ZERO.                
000116 77  W-SPAR-PRLEGKST             PIC 9(7)V9(2) VALUE ZERO.                
000117 77  SPAR-TIFAKT-PULS            PIC 9(6).                                
000118                                                                          
000119 77  SW-FIRST-TIME-32A           PIC X       VALUE 'J'.                   
000120     88  FIRST-TIME-32A                      VALUE 'J'.                   
000121                                                                          
000122 77  W41832-EOF-SW               PIC X       VALUE 'N'.                   
000123     88  END-OF-W41832                       VALUE 'J'.                   
000124                                                                          
000125 77  SKRIV-W418AJ-SW             PIC X       VALUE 'N'.                   
000126     88  SKRIV-W418AJ-OK                     VALUE 'J'.                   
000127     EJECT                                                                
000128                                                                          
000129 77  SKRIV-W418AO-SW             PIC X       VALUE 'N'.                   
000130     88  SKRIV-W418AO-OK                     VALUE 'J'.                   
000131     EJECT                                                                
000132                                                                          
000133 77  SKRIV-W418AK-SW             PIC X       VALUE 'J'.                   
000134     88  SKRIV-W418AK-OK                     VALUE 'J'.                   
000135     88  SKRIV-W418AK-NOT-OK                 VALUE 'N'.                   
000136     EJECT                                                                
000137                                                                          
000138 01  FILLER                      PIC X(08)   VALUE 'WWKUND16'.            
000139*    -COPY WWKUND16.                                                      
000140                                                                          
000141                                                                          
000142     EJECT                                                                
000143                                                                          
000144 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
000145                                                                          
000146 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
000147*01  FILLER  -COPY WWDIST79   -RED TEST-IDDISTR.                          
000148     EJECT                                                                
000149*01  FILLER  -COPY WWDIST07   -RED TEST-IDDISTR.                          
000150     EJECT                                                                
000151*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
000152     EJECT                                                                
000153*01  FILLER  -COPY WWDIST35   -RED TEST-IDDISTR.                          
000154     EJECT                                                                
000155                                                                          
000156*      --- VALID IDDC CODES                                               
000157*                                                                         
000158*01  -COPY WWDC99                                                         
000159     EJECT                                                                
000160                                                                          
000161 01  ARBETSAREA                   PIC X(16)   VALUE                       
000162                                            'ARBETSAREA     '.            
000163 01  SPAR-AREA.                                                           
000164     03  SPAR-IDDISTR             PIC 9(4).                               
000165     03  SPAR-IDKUNDNR            PIC 9(6).                               
000166     03  SPAR-IDRAPPNR            PIC 9(7).                               
000167     03  SPAR-IDKNOTNR            PIC 9(7).                               
000168     SKIP2                                                                
000169 01  JFR-AREA.                                                            
000170     03  JFR-IDDISTR              PIC 9(4).                               
000171     03  JFR-IDKUNDNR             PIC 9(6).                               
000172     03  JFR-IDRAPPNR             PIC 9(7).                               
000173     03  JFR-IDKNOTNR             PIC 9(7).                               
000174     SKIP2                                                                
000175 01  BERAKNINGSAREA.                                                      
000176     03  WS-PRLANDCO-TOT         PIC 9(7)V9(2) VALUE ZERO.                
000177                                                                          
000178                                                                          
000179 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000180 01  FILLER REDEFINES DAGENS-DATUM.                                       
000181     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000182     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000183     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000184     EJECT                                                                
000185 01  DYNAMISKA-SUBPROGRAM.                                                
000186*                                                                         
000187     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000188     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000189     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000190     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000191     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
000192     SKIP2                                                                
000193*    --- PARAMETRAR TILL ABEND                                            
000194                                                                          
000195 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000196 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000197 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000198     SKIP2                                                                
000199 01  FELTEXT.                                                             
000200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000201     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000202     EJECT                                                                
000203*    --- PARAMETRAR TILL POSTSUM                                          
000204*                                                                         
000205*01  -COPY W0005   -PRE  POSTSUM-                                         
000206     EJECT                                                                
000207                                                                          
000208*    ---  LÄNKAREA TILL W418OKOD                                          
000209     SKIP3                                                                
000210*    03 -COPY W418OKOD           -PRE OKOD-.                              
000211     EJECT                                                                
000212 01  IN-AREA-START               PIC X(24)   VALUE                        
000213                                 'IN-AREA-START  '.                       
000214     SKIP2                                                                
000215                                                                          
000216*01  AREA -COPY W41832     -PRE IN-                                       
000217     EJECT                                                                
000218 01  UTAK-AREA-START             PIC X(24)   VALUE                        
000219                                 'UTAK-AREA-START  '.                     
000220     SKIP2                                                                
000221                                                                          
000222*01  AREA -COPY W461021     -PRE UTAK-                                    
000223     EJECT                                                                
000224 01  UTAJ-AREA-START             PIC X(24)   VALUE                        
000225                                 'UTAJ-AREA-START  '.                     
000226     SKIP2                                                                
000227                                                                          
000228*01  AREA -COPY W41843     -PRE UTAJ-                                     
000229                                                                          
000230     EJECT                                                                
000231 01  UTAL-AREA-START             PIC X(24)   VALUE                        
000232                                 'UTAL-AREA-START  '.                     
000233     SKIP2                                                                
000234*01  FILLER -COPY W41834A  -PRE 34A-                                      
000235     EJECT                                                                
000236*01  FILLER -COPY W41834B  -PRE 34B-                                      
000237     EJECT                                                                
000238                                                                          
000239 01  UTAO-AREA-START             PIC X(24)   VALUE                        
000240                                 'UTAO-AREA-START  '.                     
000241     SKIP2                                                                
000242                                                                          
000243*01  AREA -COPY W41833     -PRE 720-                                      
000244     EJECT                                                                
000245*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000246*                                                                         
000247     EJECT                                                                
000248 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000249     SKIP3                                                                
000250 01  NYCKLAR-TILL-DLI.                                                    
000251     03  W-IDLEVANM-X.                                                    
000252         05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.           
000253         05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.           
000254         05 W-IDRAPPNR           PIC  9(7)   VALUE ZERO.                  
000255                                                                          
000256     03  W-WDA211KY-X.                                                    
000257         05  W-IDARTNR-WDA2      PIC S9(9)   VALUE ZERO COMP-3.           
000258         05  W-IDRADNR-WDA2      PIC S9(5)   VALUE ZERO COMP-3.           
000259                                                                          
000260     03  W-IDARTNR-X.                                                     
000261         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000262                                                                          
000263     03  W-IDARTNR-K7-X.                                                  
000264         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
000265                                                                          
000266     03  W-KDSEGKEY-X.                                                    
000267         05  W-KDSEGKEY          PIC X(1)   VALUE '1'.                    
000268                                                                          
000269     03  W-IDDC-X.                                                        
000270         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
000271                                                                          
000275     03 W-IDFAKT-X.                                                       
000276       05 W-IDFAKT              PIC S9(7)   COMP-3 VALUE ZERO.            
000278                                                                          
000279     03 W-IDGMTREF-X.                                                     
000280       05 W-IDDISTR-L5          PIC S9(5)   COMP-3 VALUE ZERO.            
000290       05 W-IDKUNDNR-L5         PIC S9(7)   COMP-3 VALUE ZERO.            
000291       05 W-IDKUNDRF-L5         PIC X(10).                                
000292                                                                          
000293     03 W-WDL511KY-X.                                                     
000294       05 W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.           
000295       05 W-IDKOLLI-X.                                                    
000296         07 W-IDKOLLI           PIC S9(5)   VALUE ZERO  COMP-3.           
000297                                                                          
000300     03  W-IDGMT-X.                                                       
000310         05 W-IDDISTR-WDB2       PIC S9(5)    COMP-3.                     
000311         05 W-IDKUNDNR-WDB2      PIC S9(7)    COMP-3.                     
000312                                                                          
000313     03  W-IDGMT-MIN-X.                                                   
000314         05 W-IDDISTR-WDB2-MIN   PIC S9(5)    COMP-3.                     
000315         05 W-IDKUNDNR-WDB2-MIN  PIC S9(7)    COMP-3.                     
000316                                                                          
000317     03  W-IDGMT-MAX-X.                                                   
000318         05 W-IDDISTR-WDB2-MAX   PIC S9(5)    COMP-3.                     
000319         05 W-IDKUNDNR-WDB2-MAX  PIC S9(7)    COMP-3.                     
000320                                                                          
000321     EJECT                                                                
000322                                                                          
000323 01  TEST-IDARTNR                PIC 9(9)    COMP-3.                      
000324*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR.                            
000325     EJECT                                                                
000326                                                                          
000327*    --- STATUS-KOD FRÅN IMS                                              
000328 01  STATUS-WS                   PIC XX.                                  
000329     88  SEGMENT-FINNS                       VALUE '  '.                  
000330     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000331     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000332     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000333     SKIP2                                                                
000334 01  GODK-STATUSKODER.                                                    
000335     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000336     SKIP3                                                                
000337 01  SSA1                        PIC X(128).                              
000338 01  SSA2                        PIC X(64).                               
000339     EJECT                                                                
000340*    --- IMS FUNKTIONSKODER                                               
000341*01  -COPY W0003                                                          
000342     EJECT                                                                
000343*    ---  DLI INPUT-OUTPUT AREA                                           
000344 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
000345 01  DLI-IO-WDA201.                                                       
000346*    03  -COPY WDA201                                                     
000347     EJECT                                                                
000348 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
000349 01  DLI-IO-WDA211.                                                       
000350*    03  -COPY WDA211                                                     
000351 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000352 01  DLI-IO-WDK601.                                                       
000353*    03  -COPY WDK601                                                     
000354     EJECT                                                                
000355 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
000356 01  DLI-IO-WDK611.                                                       
000357*    03  -COPY WDK611                                                     
000358 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK621'.                      
000359 01  DLI-IO-WDK621.                                                       
000360*    03  -COPY WDK621                                                     
000361     EJECT                                                                
000362 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
000363 01  DLI-IO-WDK701.                                                       
000364*    03  -COPY WDK701                                                     
000365     EJECT                                                                
000366 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
000367 01  DLI-IO-WDK711.                                                       
000368*    03  -COPY WDK711                                                     
000369 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL501'.                      
000370 01  DLI-IO-WDL501.                                                       
000371*    03  -COPY WDL501                                                     
000372     EJECT                                                                
000373 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL511'.                      
000374 01  DLI-IO-WDL511.                                                       
000375*    03  -COPY WDL511                                                     
000376     EJECT                                                                
000377 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL521'.                      
000378 01  DLI-IO-WDL521.                                                       
000379*    03  -COPY WDL521                                                     
000380     EJECT                                                                
000381 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
000382 01  DLI-IO-WDB201.                                                       
000383*    03  -COPY WDB201                                                     
000384     EJECT                                                                
000385 LINKAGE SECTION.                                                         
000386                                                                          
000387                                                                          
000388*01  -COPY W0008  -PRE WDA2-                                              
000390     05  FILLER                  PIC X.                                   
000391                                                                          
000392*01  -COPY W0008  -PRE WDK6-                                              
000393     05  FILLER                  PIC X.                                   
000394                                                                          
000395*01  -COPY W0008  -PRE WDK7-                                              
000396     05  FILLER                  PIC X.                                   
000397                                                                          
000398*01  -COPY W0008  -PRE WDL5-                                              
000399     05  FILLER                  PIC X.                                   
000400                                                                          
000401*01  -COPY W0008  -PRE WDB2-                                              
000402     05  FILLER                  PIC X.                                   
000403     EJECT                                                                
000404 PROCEDURE DIVISION  USING WDA2-PCB WDK6-PCB                              
000405     WDK7-PCB WDL5-PCB WDB2-PCB.                                          
000406 MAIN SECTION.                                                            
000407     ENTRY 'DLITCBL' USING WDA2-PCB WDK6-PCB                              
000408     WDK7-PCB WDL5-PCB WDB2-PCB.                                          
000409                                                                          
000410                                                                          
000411     PERFORM A-INIT                                                       
000412                                                                          
000413     PERFORM S01-LAES-W41832                                              
000414     PERFORM UNTIL END-OF-W41832                                          
000415                                                                          
000416       IF NY-KNOTA = JA                                                   
000417         MOVE IN-IDDISTR     TO SPAR-IDDISTR                              
000418         MOVE IN-IDKUNDNR    TO SPAR-IDKUNDNR                             
000419         MOVE IN-IDRAPPNR    TO SPAR-IDRAPPNR                             
000420         MOVE IN-IDKNOTNR    TO SPAR-IDKNOTNR                             
000421         MOVE NEJ            TO NY-KNOTA                                  
000422       END-IF                                                             
000423                                                                          
000424       IF SPAR-AREA = JFR-AREA                                            
000425         PERFORM B-BEHANDLA-RADER                                         
000426         PERFORM S01-LAES-W41832                                          
000427       ELSE                                                               
000428         PERFORM S02-SKRIV-RADER                                          
000429         MOVE NEJ  TO SKRIV-W418AJ-SW                                     
000430                      SKRIV-W418AO-SW                                     
000431         MOVE JA   TO SKRIV-W418AK-SW                                     
000432         PERFORM C-HUVUDPOST-W418AL                                       
000433       END-IF                                                             
000434                                                                          
000435     END-PERFORM                                                          
000436                                                                          
000437     IF POSTER-FINNS = JA                                                 
000438       PERFORM S02-SKRIV-RADER                                            
000439       PERFORM C-HUVUDPOST-W418AL                                         
000440     END-IF                                                               
000441                                                                          
000442     PERFORM Z-FINIT                                                      
000443                                                                          
000444     MOVE ZERO TO RETURN-CODE                                             
000445     GOBACK                                                               
000446     .                                                                    
000447     EJECT                                                                
000448 A-INIT SECTION.                                                          
000449                                                                          
000450     OPEN INPUT  W41832                                                   
000451                                                                          
000452     OPEN OUTPUT W418AK                                                   
000453                 W418AJ                                                   
000454                 W418AL                                                   
000455                 W418AO                                                   
000456                                                                          
000458     MOVE LOW-VALUE                       TO W-IDGMTREF-X                 
000459                                             W-IDGMT-MIN-X                
000460                                             W-IDGMT-X                    
000462     MOVE HIGH-VALUE                      TO W-IDGMT-MAX-X                
000463                                                                          
000464     MOVE NEJ                             TO SKRIV-W418AJ-SW              
000465                                             SKRIV-W418AO-SW              
000466     MOVE JA                              TO SKRIV-W418AK-SW              
000467                                                                          
000468     ACCEPT DAGENS-DATUM  FROM DATE                                       
000469     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000470                                                                          
000471     .                                                                    
000472     EJECT                                                                
000473 B-BEHANDLA-RADER SECTION.                                                
000474                                                                          
000475     EVALUATE IN-IDPTYP                                                   
000476       WHEN '32C'                                                         
000477         PERFORM BA-BEHANDLA-TKOST                                        
000478       WHEN '32A'                                                         
000479         IF FIRST-TIME-32A                                                
000480           MOVE NEJ  TO SW-FIRST-TIME-32A                                 
000481         ELSE                                                             
000482           PERFORM S02-SKRIV-RADER                                        
000483           MOVE NEJ  TO SKRIV-W418AJ-SW                                   
000484                        SKRIV-W418AO-SW                                   
000485           MOVE JA   TO SKRIV-W418AK-SW                                   
000486         END-IF                                                           
000487                                                                          
000488         MOVE IN-IDDISTR   TO W-IDDISTR                                   
000489         MOVE IN-IDKUNDNR  TO W-IDKUNDNR                                  
000490         MOVE IN-IDRAPPNR  TO W-IDRAPPNR                                  
000491         MOVE IN-IDARTNR   TO W-IDARTNR-WDA2                              
000492                              W-IDARTNR                                   
000493                              W-IDARTNR-K7                                
000494         MOVE IN-IDRADNR   TO W-IDRADNR-WDA2                              
000495         PERFORM IMS-GU-WDA201                                            
000496         PERFORM IMS-GNP-WDA211                                           
000497                                                                          
000498         IF LEV-KDANMORS = '74'                                           
000499           MOVE NEJ  TO SKRIV-W418AK-SW                                   
000500         ELSE                                                             
000501           MOVE LEV-KDANMORS TO OKOD-KDANMORS                             
000502           CALL W418OKOD USING OKOD-W418OKOD                              
000503                                                                          
000504           PERFORM BB-FLYTTA-POST-VIPS                                    
000505                                                                          
000506           MOVE IN-IDDC      TO WS-IDDC                                   
000507           IF GOOD-DDC                                                    
000508             IF OKOD-FL-LEVERANTOER = JA                                  
000509               PERFORM BC-FLYTTA-POST-EKO                                 
000510               MOVE JA  TO SKRIV-W418AJ-SW                                
000511             END-IF                                                       
000512           END-IF                                                         
000513                                                                          
000514           MOVE IN-IDDISTR TO TEST-IDDISTR                                
000515           MOVE IN-IDKUNDNR TO KUND16-IDKUNDNR                            
000516           IF DIST35-REFILL-NA OR DIST35-NA-CDC-RETURN OR                 
000517              DIST35-REFILL-NA-JAP OR                                     
000518             (DIST18-SCRAP-NDC-QUAL AND KUND16-NDC-SKROT) OR              
000519              DIST07-USA-RET-DISCR OR DIST07-CAN-RET-DISCR                
000520                                                                          
000521             PERFORM BF-FLYTTA-EKO-LAB                                    
000522             MOVE JA  TO SKRIV-W418AO-SW                                  
000523           END-IF                                                         
000524                                                                          
000525*- FIX FÖR TUULA SOM VILL HA EN LISTA PÅ ALLA KREDITERADE RADER ,         
000526*- DÄR FAKTURADAT. < 040530 (INST.HELGEN FÖR ÖVERGÅNG TILL DDI).          
000527           IF LEV-KDANMORS = '52' OR '53' OR '90' OR '92' OR              
000528                     '93' OR '98' OR '80' OR '40' OR '96' OR '74'         
000529             CONTINUE                                                     
000530           ELSE                                                           
000531             MOVE IN-IDDISTR  TO TEST-IDDISTR                             
000532           END-IF                                                         
000533         END-IF                                                           
000534                                                                          
000535         PERFORM BD-FLYTTA-POST-UPPDAT                                    
000536       WHEN '32B'                                                         
000537         MOVE IN-PRLANDCO-RAD      TO 34B-PRLANDCO-RAD                    
000538                                      UTAJ-PRLANDCO                       
000539                                      UTAK-KRED-PREMBHNT                  
000540                                      720-PRLANDCO                        
000541         ADD IN-PRLANDCO-RAD       TO WS-PRLANDCO-TOT                     
000542     END-EVALUATE                                                         
000543     .                                                                    
000544     EJECT                                                                
000545 BA-BEHANDLA-TKOST   SECTION.                                             
000546                                                                          
000547     IF IN-PRFRAKT > ZERO                                                 
000548       MOVE IN-PRFRAKT    TO W-SPAR-PRFRAKT                               
000549     END-IF                                                               
000550     IF IN-PRFOERS > ZERO                                                 
000551       MOVE IN-PRFOERS    TO W-SPAR-PRFOERS                               
000552     END-IF                                                               
000553     IF IN-PRLEGKST > ZERO                                                
000554       MOVE IN-PRLEGKST   TO W-SPAR-PRLEGKST                              
000555     END-IF                                                               
000556     .                                                                    
000557     EJECT                                                                
000558 BB-FLYTTA-POST-VIPS SECTION.                                             
000559                                                                          
000560     MOVE '021'                           TO UTAK-KRED-IDPTYP             
000561     MOVE IN-IDDISTR                      TO UTAK-KRED-IDDISTR            
000562                                             TEST-IDDISTR                 
000563     MOVE IN-IDKUNDNR                     TO UTAK-KRED-IDKUNDNR           
000564     MOVE IN-IDDC                         TO UTAK-KRED-IDDC               
000565     MOVE IN-IDRAPPNR                     TO UTAK-KRED-IDRAPPNR           
000566     MOVE IN-IDKNOTNR                     TO UTAK-KRED-IDKNOTNR           
000567     MOVE IN-TIKNOTA                      TO UTAK-KRED-TIM-KN             
000568                                                                          
000569     MOVE LEV-IDFAKT                      TO UTAK-KRED-IDFAKT             
000570                                                                          
000571     MOVE LEV-IDORDNR7                    TO UTAK-KRED-IDORDNR            
000572     MOVE IN-IDARTNR                      TO UTAK-KRED-IDARTNR            
000573                                             W-IDARTNR                    
000574     MOVE ZERO                            TO UTAK-KRED-REKSIFFR           
000575     MOVE IN-IDRADNR                      TO UTAK-KRED-IDRADNR            
000576     MOVE LEV-KDANMORS                    TO UTAK-KRED-KDANMORS           
000577                                                                          
000578*- KOD 25 GÄLLER BARA INOM PULS, BYTS TILLBAKA I VIPS.                    
000579     IF UTAK-KRED-KDANMORS = 25                                           
000580       MOVE 20                            TO UTAK-KRED-KDANMORS           
000581     END-IF                                                               
000582                                                                          
000583     PERFORM IMS-GU-WDK601                                                
000584     PERFORM IMS-GNP-WDK611                                               
000585     MOVE CLAG-KDPSLLOC                   TO UTAK-KRED-KDPSLLOC           
000586                                                                          
000587     MOVE IN-KVKREANT                     TO UTAK-KRED-KVKREANT           
000588                                                                          
000589     IF DIST79-DEALER-PRICE OR                                            
000591        DIST79-ECOM-PRICE                                                 
000592       MOVE IN-PRARTNTO                TO UTAK-KRED-PRARTBTO-LOC          
000593       MOVE ZERO                       TO UTAK-KRED-PRARTBTO              
000594       MOVE IN-SUVAT-FAKT              TO UTAK-KRED-SUVAT-FAKT            
000595       MOVE IN-KDVALISO                TO UTAK-KRED-KDVALISO              
000596       MOVE IN-KDVAT                   TO UTAK-KRED-KDVAT                 
000597       MOVE LEV-PRARTSTD               TO UTAK-KRED-PRARTSTD              
000598       MOVE LEV-PRARTSJK               TO UTAK-KRED-PRARTSJK              
000599       MOVE IN-SULNELOC                TO UTAK-KRED-SULNELOC              
000600       MOVE IN-SUKRENTO                TO UTAK-KRED-SUKRENTO              
000601       MOVE IN-SUKRETOT                TO UTAK-KRED-SUKRETOT              
000602     ELSE                                                                 
000603       MOVE IN-PRARTNTO                TO UTAK-KRED-PRARTBTO              
000604       MOVE ZERO                       TO UTAK-KRED-PRARTBTO-LOC          
000605       MOVE IN-SUVAT-FAKT              TO UTAK-KRED-SUVAT-FAKT            
000606       MOVE IN-KDVALISO                TO UTAK-KRED-KDVALISO              
000607       MOVE SPACE                      TO UTAK-KRED-KDVAT                 
000608       MOVE ZERO                       TO UTAK-KRED-PRARTSTD              
000609       MOVE ZERO                       TO UTAK-KRED-PRARTSJK              
000610       MOVE ZERO                       TO UTAK-KRED-SULNELOC              
000611       MOVE ZERO                       TO UTAK-KRED-SUKRENTO              
000612       MOVE ZERO                       TO UTAK-KRED-SUKRETOT              
000613     END-IF                                                               
000614                                                                          
000615     MOVE ZERO                            TO UTAK-KRED-PREMBHNT           
000616     PERFORM BBA-HAEMTA-BYTES-UPPG                                        
000617     MOVE ANM-PRFRAKT                     TO UTAK-KRED-PRFRAKT            
000618     MOVE ANM-PRLEGKST                    TO UTAK-KRED-PRLEGKST           
000619     MOVE ANM-PRFOERS                     TO UTAK-KRED-PRFOERS            
000620     MOVE IN-SUVAT-LINE                   TO UTAK-KRED-PRMOMS             
000621     .                                                                    
000622     EJECT                                                                
000623 BBA-HAEMTA-BYTES-UPPG SECTION.                                           
000624                                                                          
000625     MOVE IN-IDDC                  TO W-IDDC                              
000626                                                                          
000627     PERFORM IMS-GU-WDK711                                                
000628     IF BYT16-BYTES                                                       
000629       IF SEGMENT-FINNS                                                   
000630         MOVE SLAG-PRAVCOST               TO UTAK-KRED-PRAVCOST           
000631         ADD 6000                         TO W-IDARTNR-K7                 
000632         PERFORM IMS-GU-WDK711                                            
000633         IF SEGMENT-FINNS                                                 
000634            MOVE SLAG-PRAVCOST            TO                              
000635                                           UTAK-KRED-PRAVCOST-CORE        
000636         ELSE                                                             
000637            MOVE +0                       TO                              
000638                                           UTAK-KRED-PRAVCOST-CORE        
000639         END-IF                                                           
000640       ELSE                                                               
000641         MOVE +0                          TO UTAK-KRED-PRAVCOST           
000642                                           UTAK-KRED-PRAVCOST-CORE        
000643       END-IF                                                             
000644     ELSE                                                                 
000645        IF BYT16-RADIO                                                    
000646          IF SEGMENT-FINNS                                                
000647             MOVE SLAG-PRAVCOST           TO UTAK-KRED-PRAVCOST           
000648             ADD 1000                     TO W-IDARTNR-K7                 
000649             PERFORM IMS-GU-WDK711                                        
000650             IF SEGMENT-FINNS                                             
000651                MOVE SLAG-PRAVCOST        TO                              
000652                                           UTAK-KRED-PRAVCOST-CORE        
000653             ELSE                                                         
000654                MOVE +0                   TO                              
000655                                           UTAK-KRED-PRAVCOST-CORE        
000656             END-IF                                                       
000657          ELSE                                                            
000658             MOVE +0                      TO UTAK-KRED-PRAVCOST           
000659                                           UTAK-KRED-PRAVCOST-CORE        
000660          END-IF                                                          
000661        ELSE                                                              
000662          IF SEGMENT-FINNS                                                
000663            MOVE SLAG-PRAVCOST            TO UTAK-KRED-PRAVCOST           
000664            MOVE +0                       TO                              
000665                                           UTAK-KRED-PRAVCOST-CORE        
000666          ELSE                                                            
000667            MOVE +0                       TO UTAK-KRED-PRAVCOST           
000668                                           UTAK-KRED-PRAVCOST-CORE        
000669          END-IF                                                          
000670        END-IF                                                            
000671     END-IF                                                               
000672     .                                                                    
000673     EJECT                                                                
000674 BC-FLYTTA-POST-EKO  SECTION.                                             
000675                                                                          
000676     MOVE 'VIR'                           TO UTAJ-IDPTYP                  
000677     MOVE IN-IDDISTR                      TO UTAJ-IDDISTR                 
000679                                             TEST-IDDISTR                 
000680     MOVE IN-IDKUNDNR                     TO UTAJ-IDKUNDNR                
000682     MOVE IN-IDRAPPNR                     TO UTAJ-IDRAPPNR                
000683     MOVE IN-IDDC                         TO UTAJ-IDDC                    
000684     MOVE IN-IDARTNR                      TO UTAJ-IDARTNR                 
000685     MOVE LEV-KDANMORS                    TO UTAJ-KDANMORS                
000686     MOVE IN-KVKREANT                     TO UTAJ-KVLEVANM                
000687                                                                          
000688*--- PRISET SKALL VARA UTL.BEST.PRIS ENL. TUULA                           
000689     PERFORM S20-TA-FRAM-LEV-PRIS                                         
000690                                                                          
000691     MOVE LEV-IDORDNR7                    TO UTAJ-IDORDNR5                
000692     MOVE LEV-IDKOLLI                     TO UTAJ-IDKOLLI                 
000693                                             W-IDKOLLI                    
000694     MOVE IN-IDKNOTNR                     TO UTAJ-IDKNOTNR                
000695                                                                          
000696     MOVE LEV-IDFAKT                      TO W-IDFAKT                     
000698                                                                          
000701     PERFORM IMS-GU-WDL501                                                
000702                                                                          
000703     IF SEGMENT-FINNS                                                     
000704       MOVE W-IDDISTR                     TO W-IDDISTR-L5                 
000705       MOVE W-IDKUNDNR                    TO W-IDKUNDNR-L5                
000706       MOVE LEV-IDKUNDRF                  TO W-IDKUNDRF-L5                
000707       MOVE LEV-IDKOLLI                   TO W-IDKOLLI                    
000711       PERFORM IMS-GNP-WDL511                                             
000712       IF SEGMENT-FINNS                                                   
000713         MOVE FAKC-IDPRODNR               TO UTAJ-IDPRODNR                
000714                                             W-IDPRODNR                   
000715         MOVE LEV-IDARTNR                 TO W-IDARTNR                    
000716         PERFORM IMS-GNP-WDL521                                           
000717         IF SEGMENT-FINNS                                                 
000718            MOVE FAKL-IDLEVNR             TO UTAJ-IDLEVNR                 
000719         ELSE                                                             
000720           MOVE SPACE                     TO UTAJ-IDLEVNR                 
000721         END-IF                                                           
000722       ELSE                                                               
000723         MOVE SPACE                       TO UTAJ-IDLEVNR                 
000724         MOVE ZERO                        TO UTAJ-IDPRODNR                
000725       END-IF                                                             
000726     ELSE                                                                 
000727       MOVE ZERO                          TO UTAJ-IDPRODNR                
000728       MOVE SPACE                         TO UTAJ-IDLEVNR                 
000729     END-IF                                                               
000731                                                                          
000734     MOVE ZERO                            TO UTAJ-PRLANDCO                
000735     MOVE ZERO                            TO UTAJ-PREMBHNT                
000736     MOVE W-SPAR-PRFOERS                  TO UTAJ-PRFOERS                 
000737     MOVE W-SPAR-PRFRAKT                  TO UTAJ-PRFRAKT                 
000738     MOVE W-SPAR-PRLEGKST                 TO UTAJ-PRLEGKST                
000739     MOVE IN-SUVAT-LINE                   TO UTAJ-PRMOMS                  
000740                                                                          
000741     .                                                                    
000742     EJECT                                                                
000743 BD-FLYTTA-POST-UPPDAT SECTION.                                           
000744                                                                          
000745     MOVE '34B'            TO 34B-IDPTYP                                  
000746     MOVE IN-IDDISTR       TO 34B-IDDISTR                                 
000747                              34A-IDDISTR                                 
000748                              TEST-IDDISTR                                
000749     MOVE IN-IDKUNDNR      TO 34B-IDKUNDNR                                
000750                              34A-IDKUNDNR                                
000751     MOVE IN-IDRAPPNR      TO 34B-IDRAPPNR                                
000752                              34A-IDRAPPNR                                
000753     MOVE IN-IDDC          TO 34B-IDDC                                    
000754                              34A-IDDC                                    
000755     MOVE IN-IDKNOTNR      TO 34B-IDKNOTNR                                
000756                              34A-IDKNOTNR                                
000757     MOVE IN-TIKNOTA       TO 34B-TIKNOTA                                 
000758                              34A-TIKNOTA                                 
000759     MOVE IN-IDARTNR       TO 34B-IDARTNR                                 
000760     MOVE IN-IDRADNR       TO 34B-IDRADNR                                 
000761     MOVE IN-KDVAT         TO 34B-KDVAT                                   
000762                                                                          
000763*- ÄNDRING 2005-02-23 E'TRACKER 1752877                                   
000764     MOVE IN-KDVALISO      TO 34B-KDVALISO                                
000765                              34A-KDVALISO                                
000766     MOVE IN-PRKURS        TO 34B-PRKURS                                  
000767                              34A-PRKURS                                  
000768                                                                          
000769     MOVE IN-KVKREANT      TO 34B-KVKREANT                                
000770     MOVE IN-PRARTNTO      TO 34B-PRARTNTO                                
000771     MOVE ZERO             TO 34B-PRLANDCO-RAD                            
000772     MOVE IN-SUKRENTO      TO 34A-SUKRENTO                                
000773     MOVE IN-SUVAT-FAKT    TO 34A-SUVAT-FAKT                              
000774     MOVE IN-SUKRETOT      TO 34A-SUKRETOT                                
000775     MOVE IN-KDTRADP       TO 34A-KDTRADP                                 
000776                              34B-KDTRADP                                 
000777     .                                                                    
000778     EJECT                                                                
000779 BF-FLYTTA-EKO-LAB SECTION.                                               
000780                                                                          
000781     MOVE '720'                           TO 720-IDPTYP                   
000782     IF GOOD-DDC                                                          
000783       MOVE '11'                          TO 720-IDDC                     
000784     ELSE                                                                 
000785       MOVE LEV-IDDC                      TO 720-IDDC                     
000790     END-IF                                                               
000800                                                                          
000810     MOVE IN-IDDISTR                      TO 720-IDDISTR                  
000820                                             W-IDDISTR-WDB2               
000830                                             W-IDDISTR-WDB2-MIN           
000831                                             W-IDDISTR-WDB2-MAX           
000832     MOVE IN-IDKUNDNR                     TO W-IDKUNDNR-WDB2              
000833     PERFORM IMS-GET-WDB201-UNIK                                          
000834     IF SEGMENT-SAKNAS                                                    
000835        PERFORM IMS-GET-WDB201                                            
000836     END-IF                                                               
000837                                                                          
000838     IF GMT-FLSAMFAK = JA                                                 
000839        MOVE ZERO                         TO 720-IDKUNDNR                 
000840     ELSE                                                                 
000841        MOVE IN-IDKUNDNR                  TO 720-IDKUNDNR                 
000842     END-IF                                                               
000843                                                                          
000844     MOVE IN-IDKNOTNR                     TO 720-IDKNOTNR                 
000845     MOVE IN-IDRAPPNR                     TO 720-IDRAPPNR                 
000846     MOVE IN-TIKNOTA                      TO 720-DAKRENOT                 
000847     MOVE 20                              TO 720-DAKRENOT(1:2)            
000848                                                                          
000849***-KURSEN TILL W51032/LAB.GAMLA KREDITERINGEN (EJ CENTRAL PRICE)         
000850***-SKICKAR USD/SEK KURS RESP CAD/SEK.W51032 RÄKNAR UT SUKREUTL.          
000851***-NÄR NORDAMERIKA GÅR ÖVER TILL CP, SKICKAR MAN ISTÄLLET KURSEN         
000852***-MELLAN KUNDEN/BET OCH FAKTURAN.USD/CAD KURS FR BILL-IT ELLER 1        
000853     MOVE IN-PRKURS-FAKBET                TO 720-PRKURS                   
000854                                                                          
000855     MOVE W-SPAR-PRFOERS                  TO 720-PRFOERS                  
000856     MOVE W-SPAR-PRFRAKT                  TO 720-PRFRAKT                  
000857     MOVE W-SPAR-PRLEGKST                 TO 720-PRLEGKST                 
000858     MOVE ZERO                            TO 720-PRMOMS                   
000859                                             720-SUKRENTO                 
000860                                             720-SUKRENOT                 
000861                                             720-SUKREUTL                 
000862     MOVE ZERO                            TO 720-REVAT                    
000863     MOVE IN-IDARTNR                      TO 720-IDARTNR                  
000864     MOVE LEV-KDANMORS                    TO 720-KDANMORS                 
000865     MOVE LEV-FLLSBOK                     TO 720-FLLSBOK                  
000866                                                                          
000867     PERFORM IMS-GU-WDK601                                                
000868     MOVE ART-KDPRODSL                    TO 720-KDPRODSL                 
000869     PERFORM IMS-GNP-WDK611                                               
000870     MOVE CLAG-KDPSLLOC                   TO 720-KDPSLLOC                 
000871                                                                          
000872     MOVE IN-KVKREANT                     TO 720-KVKREANT                 
000873     MOVE IN-PRLANDCO-RAD                 TO 720-PRLANDCO                 
000874     MOVE IN-PRARTNTO                     TO 720-PRARTNTO                 
000875                                                                          
000876     IF NDC-US OR NDC-CA                                                  
000877        MOVE LEV-IDDC                     TO W-IDDC                       
000878        PERFORM IMS-GU-WDK711                                             
000879        IF SEGMENT-FINNS                                                  
000880           MOVE SLAG-PRAVCOST             TO 720-PRAVCOST                 
000881        ELSE                                                              
000882           MOVE +0                        TO 720-PRAVCOST                 
000883        END-IF                                                            
000884     ELSE                                                                 
000885        MOVE +0                           TO 720-PRAVCOST                 
000886     END-IF                                                               
000887                                                                          
000888     .                                                                    
000889     EJECT                                                                
000890 C-HUVUDPOST-W418AL SECTION.                                              
000891                                                                          
000892     MOVE '34A'            TO 34A-IDPTYP                                  
000893     MOVE WS-PRLANDCO-TOT  TO 34A-PRLANDCO                                
000894     MOVE W-SPAR-PRFRAKT   TO 34A-PRFRAKT                                 
000895     MOVE W-SPAR-PRFOERS   TO 34A-PRFOERS                                 
000896     MOVE W-SPAR-PRLEGKST  TO 34A-PRLEGKST                                
000897                                                                          
000898*- VID NYTT RAPPORTNR SKALL STATUSEN ÄNDRAS PÅ WDA201.                    
000899*- FRÅN KDLEVANM=9 TILL 4 ELLER 8 I PGM W4183300.                         
000900     IF END-OF-W41832                                                     
000901        MOVE JA      TO 34A-FLSLUT                                        
000902     ELSE                                                                 
000903       IF JFR-IDDISTR  = SPAR-IDDISTR  AND                                
000904          JFR-IDKUNDNR = SPAR-IDKUNDNR AND                                
000905          JFR-IDRAPPNR = SPAR-IDRAPPNR                                    
000906                                                                          
000907          MOVE NEJ     TO 34A-FLSLUT                                      
000908       ELSE                                                               
000909          MOVE JA      TO 34A-FLSLUT                                      
000910       END-IF                                                             
000911     END-IF                                                               
000912                                                                          
000913     PERFORM S14-SKRIV-HUV-W418AL                                         
000914                                                                          
000915     MOVE ZERO      TO WS-PRLANDCO-TOT                                    
000916                       W-SPAR-PRFRAKT                                     
000917                       W-SPAR-PRFOERS                                     
000918                       W-SPAR-PRLEGKST                                    
000919     MOVE JA        TO NY-KNOTA                                           
000920     MOVE JA        TO SW-FIRST-TIME-32A                                  
000921     .                                                                    
000922     EJECT                                                                
000923 Z-FINIT SECTION.                                                         
000924     CLOSE W41832                                                         
000925           W418AK                                                         
000926           W418AJ                                                         
000927           W418AL                                                         
000928           W418AO                                                         
000929     SKIP2                                                                
000930     MOVE 'S' TO POSTSUM-OPKOD                                            
000931     CALL POSTSUM USING POSTSUM-PARM                                      
000932     .                                                                    
000933     EJECT                                                                
000934 S01-LAES-W41832  SECTION.                                                
000935     READ W41832 INTO IN-AREA                                             
000936     AT END                                                               
000937        MOVE HIGH-VALUE TO IN-AREA                                        
000938        SET END-OF-W41832 TO TRUE                                         
000939                                                                          
000940     NOT AT END                                                           
000941        MOVE 'W41832' TO POSTSUM-FDNAMN                                   
000942        MOVE 'W41834D1' TO POSTSUM-DDNAMN2                                
000943        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
000944        CALL POSTSUM USING POSTSUM-PARM                                   
000945                                                                          
000946        MOVE JA TO POSTER-FINNS                                           
000947        MOVE IN-IDDISTR  TO JFR-IDDISTR                                   
000948        MOVE IN-IDKUNDNR TO JFR-IDKUNDNR                                  
000949        MOVE IN-IDRAPPNR TO JFR-IDRAPPNR                                  
000950        MOVE IN-IDKNOTNR TO JFR-IDKNOTNR                                  
000951     END-READ                                                             
000952     .                                                                    
000953     EJECT                                                                
000954 S02-SKRIV-RADER  SECTION.                                                
000955                                                                          
000956     IF SKRIV-W418AJ-OK                                                   
000957       PERFORM S11-SKRIV-W418AJ                                           
000958     END-IF                                                               
000959     IF SKRIV-W418AO-OK                                                   
000960       PERFORM S16-SKRIV-W418AO                                           
000961     END-IF                                                               
000962     IF SKRIV-W418AK-OK                                                   
000963       PERFORM S12-SKRIV-W418AK                                           
000964     END-IF                                                               
000965     PERFORM S13-SKRIV-RAD-W418AL                                         
000966     .                                                                    
000967     EJECT                                                                
000968 S11-SKRIV-W418AJ SECTION.                                                
000969                                                                          
000970     WRITE UTAJ-POST FROM UTAJ-AREA                                       
000971                                                                          
000972     MOVE UTAJ-IDPTYP TO POSTSUM-TRANSTYP                                 
000973     MOVE 'W418AJ' TO POSTSUM-FDNAMN                                      
000974     MOVE 'W41834D2' TO POSTSUM-DDNAMN2                                   
000975     CALL POSTSUM USING POSTSUM-PARM                                      
000976     .                                                                    
000977     EJECT                                                                
000978 S12-SKRIV-W418AK SECTION.                                                
000979                                                                          
000980     WRITE UTAK-POST FROM UTAK-AREA                                       
000981                                                                          
000982     MOVE UTAK-KRED-IDPTYP TO POSTSUM-TRANSTYP                            
000983     MOVE 'W418AK' TO POSTSUM-FDNAMN                                      
000984     MOVE 'W41834D3' TO POSTSUM-DDNAMN2                                   
000985     CALL POSTSUM USING POSTSUM-PARM                                      
000986     .                                                                    
000987     EJECT                                                                
000988 S13-SKRIV-RAD-W418AL SECTION.                                            
000989                                                                          
000990                                                                          
000991     WRITE UT34B-POST FROM 34B-W41834B                                    
000992                                                                          
000993     MOVE 34B-IDPTYP TO POSTSUM-TRANSTYP                                  
000994     MOVE 'W418AL' TO POSTSUM-FDNAMN                                      
000995     MOVE 'W41834D4' TO POSTSUM-DDNAMN2                                   
000996     CALL POSTSUM USING POSTSUM-PARM                                      
000997     .                                                                    
000998     EJECT                                                                
000999 S14-SKRIV-HUV-W418AL SECTION.                                            
001000                                                                          
001001     WRITE UT34A-POST FROM 34A-W41834A                                    
001002                                                                          
001003     MOVE 34A-IDPTYP TO POSTSUM-TRANSTYP                                  
001004     MOVE 'W418AL' TO POSTSUM-FDNAMN                                      
001005     MOVE 'W41834D4' TO POSTSUM-DDNAMN2                                   
001006     CALL POSTSUM USING POSTSUM-PARM                                      
001007     .                                                                    
001008     EJECT                                                                
001009 S16-SKRIV-W418AO SECTION.                                                
001010                                                                          
001011     WRITE 720-POST  FROM 720-AREA                                        
001012                                                                          
001013     MOVE 720-IDPTYP  TO POSTSUM-TRANSTYP                                 
001014     MOVE 'W418AO' TO POSTSUM-FDNAMN                                      
001015     MOVE 'W41834D6' TO POSTSUM-DDNAMN2                                   
001016     CALL POSTSUM USING POSTSUM-PARM                                      
001017     .                                                                    
001018     EJECT                                                                
001019 S20-TA-FRAM-LEV-PRIS SECTION.                                            
001020                                                                          
001030     PERFORM IMS-GU-WDK601                                                
001031     PERFORM IMS-GNP-WDK621-FIRST                                         
001032                                                                          
001033     IF SEGMENT-FINNS                                                     
001034       IF PRL-KDSTATUS-PR = 1  AND                                        
001035          PRL-SUINLEV-PR > 0   AND                                        
001036          PRL-FLHUVLEV = JA                                               
001037         MOVE  PRL-IDLEVNR      TO UTAJ-IDLEVNR-PRIS                      
001038         MOVE  PRL-KDVALISO     TO UTAJ-KDVALISO                          
001039         MOVE  PRL-PRARTBEL-PR  TO UTAJ-PRARTBTO                          
001040         COMPUTE UTAJ-PRARTBTO ROUNDED =                                  
001041                   (UTAJ-KVLEVANM * PRL-PRARTBEL-PR )                     
001042       ELSE                                                               
001043         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
001044                OR (PRL-KDSTATUS-PR = 1 AND                               
001045                    PRL-SUINLEV-PR > 0  AND                               
001046                    PRL-FLHUVLEV = JA )                                   
001047           PERFORM IMS-GNP-WDK621                                         
001048           IF PRL-KDSTATUS-PR = 1  AND                                    
001049               PRL-SUINLEV-PR > 0  AND                                    
001050                 PRL-FLHUVLEV = JA                                        
001051             MOVE  PRL-IDLEVNR      TO UTAJ-IDLEVNR-PRIS                  
001052             MOVE  PRL-KDVALISO     TO UTAJ-KDVALISO                      
001053             COMPUTE UTAJ-PRARTBTO ROUNDED =                              
001054                   (UTAJ-KVLEVANM * PRL-PRARTBEL-PR )                     
001055           ELSE                                                           
001056             MOVE  SPACE            TO UTAJ-IDLEVNR-PRIS                  
001057             MOVE  SPACE            TO UTAJ-KDVALISO                      
001058             MOVE  ZERO             TO UTAJ-PRARTBTO                      
001059           END-IF                                                         
001060         END-PERFORM                                                      
001061       END-IF                                                             
001062     ELSE                                                                 
001063       MOVE  SPACE            TO UTAJ-IDLEVNR-PRIS                        
001064       MOVE  SPACE            TO UTAJ-KDVALISO                            
001065       MOVE  ZERO             TO UTAJ-PRARTBTO                            
001066     END-IF                                                               
001067                                                                          
001068     .                                                                    
001069     EJECT                                                                
001070* --- IMS SEKTIONER ---                                                   
001071                                                                          
001072     EJECT                                                                
001073 IMS-GU-WDA201 SECTION.                                                   
001074                                                                          
001075     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
001076          DELIMITED BY SIZE INTO SSA1                                     
001077     MOVE '  GE' TO GODK-STATUSKODER                                      
001078     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
001079     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001080     PERFORM IMS-STATUSKONTROLL                                           
001081     .                                                                    
001082     EJECT                                                                
001083 IMS-GNP-WDA211 SECTION.                                                  
001084                                                                          
001085     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
001086          DELIMITED BY SIZE INTO SSA1                                     
001087     MOVE '  GE' TO GODK-STATUSKODER                                      
001088     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-WDA211 SSA1                   
001089     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001090     PERFORM IMS-STATUSKONTROLL                                           
001091     .                                                                    
001092     EJECT                                                                
001093 IMS-GU-WDK601 SECTION.                                                   
001094                                                                          
001095     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
001096          DELIMITED BY SIZE INTO SSA1                                     
001097     MOVE '  GE' TO GODK-STATUSKODER                                      
001098     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
001099     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
001100     PERFORM IMS-STATUSKONTROLL                                           
001101     .                                                                    
001102     EJECT                                                                
001103 IMS-GNP-WDK611 SECTION.                                                  
001104                                                                          
001105     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
001106          DELIMITED BY SIZE INTO SSA1                                     
001107     MOVE '  GE' TO GODK-STATUSKODER                                      
001108     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
001109     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
001110     PERFORM IMS-STATUSKONTROLL                                           
001111     .                                                                    
001112     EJECT                                                                
001113 IMS-GNP-WDK621-FIRST SECTION.                                            
001114                                                                          
001115     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
001116     MOVE 'WDK621  *F' TO SSA2                                            
001117     MOVE '  GE' TO GODK-STATUSKODER                                      
001118     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1 SSA2              
001119     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
001120     PERFORM IMS-STATUSKONTROLL                                           
001121     .                                                                    
001122     SKIP3                                                                
001123 IMS-GNP-WDK621   SECTION.                                                
001124                                                                          
001125     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
001126     MOVE 'WDK621   ' TO SSA2                                             
001127     MOVE '  GE' TO GODK-STATUSKODER                                      
001128     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1 SSA2              
001129     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
001130     PERFORM IMS-STATUSKONTROLL                                           
001131     .                                                                    
001132     EJECT                                                                
001133 IMS-GU-WDK711 SECTION.                                                   
001134                                                                          
001135     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
001136            DELIMITED BY SIZE INTO SSA1                                   
001137     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001138            DELIMITED BY SIZE INTO SSA2                                   
001139                                                                          
001140     MOVE '  GE' TO GODK-STATUSKODER                                      
001141     CALL CBLTDLI USING                                                   
001142           GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2                            
001143     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001144     PERFORM IMS-STATUSKONTROLL                                           
001145     .                                                                    
001146     EJECT                                                                
001147 IMS-GU-WDL501      SECTION.                                              
001148                                                                          
001149     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
001150          DELIMITED BY SIZE INTO SSA1                                     
001160     MOVE '  GE'           TO GODK-STATUSKODER                            
001161     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
001162     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
001163     PERFORM IMS-STATUSKONTROLL                                           
001164     .                                                                    
001165     EJECT                                                                
001166 IMS-GNP-WDL511      SECTION.                                             
001167                                                                          
001168     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
001169                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
001170          DELIMITED BY SIZE INTO SSA1                                     
001171     MOVE '  GE'           TO GODK-STATUSKODER                            
001172     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
001173     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
001174     PERFORM IMS-STATUSKONTROLL                                           
001175     .                                                                    
001176     EJECT                                                                
001177 IMS-GNP-WDL521     SECTION.                                              
001178                                                                          
001179     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
001180          DELIMITED BY SIZE INTO SSA1                                     
001181     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-X ')'                         
001182          DELIMITED BY SIZE INTO SSA2                                     
001183     MOVE '  GE'           TO GODK-STATUSKODER                            
001184     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
001185     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
001186     PERFORM IMS-STATUSKONTROLL                                           
001187     .                                                                    
001188     EJECT                                                                
001189 IMS-GET-WDB201 SECTION.                                                  
001190                                                                          
001191     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
001192                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
001193            DELIMITED BY SIZE INTO SSA1                                   
001194                                                                          
001195     MOVE '  GE' TO GODK-STATUSKODER                                      
001196     CALL CBLTDLI USING                                                   
001197           GU WDB2-PCB DLI-IO-WDB201 SSA1                                 
001198     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
001200     PERFORM IMS-STATUSKONTROLL                                           
001201     .                                                                    
001202     EJECT                                                                
001203 IMS-GET-WDB201-UNIK SECTION.                                             
001204                                                                          
001205     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
001206            DELIMITED BY SIZE INTO SSA1                                   
001207                                                                          
001208     MOVE '  GE' TO GODK-STATUSKODER                                      
001209     CALL CBLTDLI USING                                                   
001210           GU WDB2-PCB DLI-IO-WDB201 SSA1                                 
001211     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
001212     PERFORM IMS-STATUSKONTROLL                                           
001213     .                                                                    
001214     EJECT                                                                
001215 IMS-STATUSKONTROLL SECTION.                                              
001216                                                                          
001217     SET STATUS-IX TO 1                                                   
001218     SEARCH GODK-STATUS                                                   
001219       AT END                                                             
001220         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001221           DELIMITED BY SIZE INTO FELTEXT                                 
001222         DISPLAY FELTEXT                                                  
001223         CALL FELLOG                                                      
001224       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001225         CONTINUE                                                         
001226     END-SEARCH                                                           
001227     .                                                                    
