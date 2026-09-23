000001 ID  DIVISION.                                                            
000002     SKIP2                                                                
000003 PROGRAM-ID.    W0927800.                                                 
000004 AUTHOR.        LENA FRÖNELL.                                             
000005 DATE-WRITTEN.  JUNI 1983.                                                
000006                                                                          
000007     REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        EN FIL FRÅN INKÖP GÖRS OM TILL R05:OR, R22:OR SAMT R23:OW        
000011*        SAMT EN ANNAN FIL FRÅN PV SOM GÖRS OM TILL R22:OR                
000012*                                                                         
000013*        EN TREDJE FIL SOM KOMMER FRÅN CARPAK GÖRS OM TILL R05:OR         
000014*        R22:OR, R23:OR ELLER 985:OR. FÖR ATT HANTERA CARPACFILEN         
000015*        LÄSES WDK660 OCH WDF101 SAMT 'W.PROD.LEVNRC(+0)'                 
000016*                                                                         
000017*                                                                         
000018*        POSTTYP 985 SKRIVS UT PÅ FIL W09286 FÖR ATT GÅ TILL              
000019*        SYSTEM W553 FÖR UPPDATERING AV BESTÄLLNINGSPRIS PÅ WDK6          
000020*                                                                         
000021*                                                                         
000022*   OBS  RISK FÖR NAMNBLANDNING!                                          
000023*                                                                         
000024*        PÅ UT-FILEN W09285 SKRIVS POSTTYP R05                            
000025*                                          R22                            
000026*                                          R23                            
000027*                                          R01                            
000028*                                                                         
000029*        PÅ UT-FILEN W09286 SKRIVS POSTTYP 985                            
000030*                                                                         
000031*                                                                         
000032*        PROGRAMMET LÄSER WDK6 (WLARTC)                                   
000033*                         WDF1 (WLLEVA)                                   
000034*                                                                         
000035*    ÄNDRINGAR:                                                           
000036*     2001-01-11: LEVNR = X(5).  HÅRDKODAD TEST I  K- SEKTIONEN           
000037*     E-TRACKER 1286763, MÄRKNING AV PRISÄNDRING PÅ ONDEMANDLISTOR        
000038*     2004-11-26: E-TRACKER 1567349 /L.A                                  
000039*     2008-06-27: E-TRACKER 6613080 /J NIHLBLAD                           
000040*                                                                         
000041     EJECT                                                                
000042 ENVIRONMENT DIVISION.                                                    
000043     SKIP2                                                                
000044 INPUT-OUTPUT SECTION.                                                    
000045*                                                                         
000046 FILE-CONTROL.                                                            
000047     SKIP2                                                                
000048     SELECT A31731-INFIL                 ASSIGN TO UT-S-W09278D1.         
000049**                      *** TRANSAR FRÅN INKÖP                            
000050     SELECT W09285-UTFIL                 ASSIGN TO UT-S-W09278D5.         
000051**                      *** R01 R05 R22 R23                               
000052     SELECT W09286-UTFIL                 ASSIGN TO UT-S-W09278D6.         
000053**                      *** 985:OR                                        
000054     EJECT                                                                
000055 DATA DIVISION.                                                           
000056                                                                          
000057 FILE SECTION.                                                            
000058     SKIP2                                                                
000059 FD  A31731-INFIL                                                         
000060     RECORDING      V                                                     
000061     BLOCK CONTAINS 0.                                                    
000062     SKIP2                                                                
000063 01  FILLER                       PIC X(58).                              
000064     SKIP3                                                                
000065 FD  W09285-UTFIL                                                         
000066     RECORDING      V                                                     
000067     BLOCK CONTAINS 0.                                                    
000068     SKIP2                                                                
000069*01  POST -COPY W092R05T     -PRE W09285- -L.                             
000070     SKIP3                                                                
000071 FD  W09286-UTFIL                                                         
000072     RECORDING      F                                                     
000073     BLOCK CONTAINS 0.                                                    
000074     SKIP2                                                                
000075*01  POST -COPY W55310       -PRE W09286- -L.                             
000076     SKIP3                                                                
000077 WORKING-STORAGE SECTION.                                                 
000078                                                                          
000079                                                                          
000080*    -- CHECKED BY WY2000                                                 
000081 77  IDPGM                       PIC X(8)    VALUE 'W0927800'.            
000082 77  JA                          PIC X(1)    VALUE 'J'.                   
000083 77  NEJ                         PIC X(1)    VALUE 'N'.                   
000084*                            END-OF-FILE-SWITCHAR                         
000085 77  A31731-EOF                  PIC X(1)    VALUE 'N'.                   
000086 77  XCP090-EOF                  PIC X(1)    VALUE 'N'.                   
000087 77  WS-KDPRODSL                 PIC S9(3)   COMP-3.                      
000088     SKIP2                                                                
000089*    --- PARAMETRAR TILL ABEND                                            
000090                                                                          
000091 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000092 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000093 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
000094                                                                          
000095 77  LEVNRC-EOF-SW               PIC X       VALUE 'N'.                   
000096     88  END-OF-LEVNRC                       VALUE 'J'.                   
000097                                                                          
000098 77  SUPPLIER-EOF-SW             PIC X       VALUE 'N'.                   
000099     88  END-OF-SUPPLIER                     VALUE 'J'.                   
000100     SKIP2                                                                
000101 01  DYNAMISKA-SUBPROGRAM.                                                
000102*                                                                         
000103   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
000104   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
000105   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
000106   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
000107                                                                          
000108 01  FELTEXT.                                                             
000109     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000110     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000111     EJECT                                                                
000112 01  WS-IDELMT.                                                           
000113   03  WS-IDINK-X                PIC X(5).                                
000114   03  FILLER                    PIC X(11)  VALUE SPACE.                  
000115                                                                          
000116 01  WS-INKNR-NY                 PIC X(25) VALUE SPACE.                   
000117 01  FILLER  REDEFINES WS-INKNR-NY.                                       
000118   03  WS-INKNR                  PIC 9(3).                                
000119   03  FILLER                    PIC X(22).                               
000120 01  FILLER  REDEFINES WS-INKNR-NY.                                       
000121   03  WS-IDINK                  PIC X(4).                                
000122   03  FILLER                    PIC X(21).                               
000123                                                                          
000124 01  WS-BESTNR                   PIC 9(12).                               
000125 01  FILLER  REDEFINES WS-BESTNR.                                         
000126   03  WS-BESTPREF               PIC 9(3).                                
000127   03  WS-BESTLNR                PIC 9(6).                                
000128   03  FILLER REDEFINES WS-BESTLNR.                                       
000129      05 WS-BESTLNR-POS1         PIC 9(1).                                
000130      05 FILLER                  PIC 9(5).                                
000131   03  WS-BESTSUFF               PIC 9(3).                                
000132                                                                          
000133 01  WS-BESTNR1.                                                          
000134   03  WS-BESTPREF1              PIC 9(3).                                
000135   03  WS-BESTLNR1               PIC 9(6).                                
000136   03  WS-BESTSUFF1              PIC 9(3).                                
000137                                                                          
000138 01  WS-BESTNR2.                                                          
000139   03  WS-BESTPREF2              PIC 9(3).                                
000140   03  WS-BESTLNR2               PIC 9(7).                                
000141   03  WS-BESTSUFF2              PIC 9(3).                                
000142                                                                          
000143 01  WS-ARTNR                    PIC 9(8).                                
000144 01  WS-LEVNUM-NUM               PIC 9(5).                                
000145                                                                          
000146*    --- FÄLT FÖR IDLEVNR-KONVERTERING                                    
000147 01  WS-IDLEVNR-NUM              PIC S9(5)       COMP-3.                  
000148 01  WS-IDLEVNR-DISPLAY          PIC 9(5).                                
000149 01  WS-IDLEVNR-ALFA             PIC X(5).                                
000150                                                                          
000151*    ---                                                                  
000152 01  WS-DATUM-UTSKR              PIC 9(6).                                
000153 01  WS-ANT-BESTANN              PIC 9(7).                                
000154 01  WS-POSTTYP-UT               PIC X(3).                                
000155                                                                          
000156 01  WS-PRICE                    PIC 9(7)V999.                            
000157 01  WS-NEW-PRICE                PIC 9(7)V999.                            
000158 01  WS-NEW-PRICE-HELTAL REDEFINES WS-NEW-PRICE PIC 9(10).                
000159                                                                          
000160 01  ARBETSAREOR.                                                         
000161    03  WS-KDGK                  PIC S9(01) COMP-3.                       
000162    03  WS-KDERS                 PIC S9(03) COMP-3.                       
000163    03  WS-R05TEST-IDARTNR       PIC S9(09) COMP-3 VALUE ZERO.            
000164                                                                          
000165 01  IX-TAB                      PIC S9(5) COMP-3 VALUE ZERO.             
000166 01  MAX-IX-TAB                  PIC S9(5) COMP-3 VALUE +999.             
000167                                                                          
000168     EJECT                                                                
000169*                            PARAMETRAR TILL POSTSUM                      
000170*                                                                         
000171*01  -COPY W0005      -PRE  POSTSUM-                                      
000172     EJECT                                                                
000173 01  FILLER                      PIC X(24)   VALUE                        
000174                                            'IN-AREA-START  '.            
000175                                                                          
000176 01  IN-AREA.                                                             
000177   03  POSTTYP                   PIC S9(3) COMP-3.                        
000178   03  FILLER                    PIC X(56).                               
000179*     *** DESSA C-TXTER INNEH. BÅDE PACKAT OCH ALFANUM LEVNR              
000180*01  AREA  -COPY A311G940     -PRE IN940-  -RED IN-AREA.                  
000181     EJECT                                                                
000182*01  AREA  -COPY A311G941     -PRE IN941-  -RED IN-AREA.                  
000183     EJECT                                                                
000184*01  AREA  -COPY A310G984     -PRE IN984-  -RED IN-AREA.                  
000185     EJECT                                                                
000186*01  AREA  -COPY A310G985     -PRE IN985-  -RED IN-AREA.                  
000187     EJECT                                                                
000188 01  IN-AREA5.                                                            
000189   03  POSTTYP5                  PIC X(3).                                
000190   03  FILLER                    PIC X(97).                               
000191                                                                          
000192 01  TABIN-AREA-START            PIC X(24)   VALUE                        
000193                                 'TABIN-AREA-START  '.                    
000194     SKIP2                                                                
000195                                                                          
000196 01  FILLER                      PIC X(24)  VALUE 'UT-AREA'.              
000197 01  UT-AREA.                                                             
000198   03  FILLER                    PIC X(80)  VALUE SPACE.                  
000199                                                                          
000200*01  AREA  -PRE UTR05-  -COPY W092R05T   -RED UT-AREA.                    
000201     EJECT                                                                
000202*01  AREA  -PRE UTR2X-  -COPY W212R22K   -RED UT-AREA.                    
000203     EJECT                                                                
000204*01  AREA  -PRE UTR01-  -COPY W213R01T   -RED UT-AREA.                    
000205     EJECT                                                                
000206 01  FILLER                      PIC X(24)  VALUE 'UT-985'.               
000207                                                                          
000208*01  AREA  -PRE UT985-  -COPY W55310                                      
000209     EJECT                                                                
000210 01  FILLER                      PIC X(24)  VALUE 'UT-986'.               
000211                                                                          
000212*01  AREA  -PRE UT986-  -COPY W55310                                      
000213     EJECT                                                                
000214                                                                          
000215*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000216                                                                          
000217 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000218     SKIP3                                                                
000219 01  NYCKLAR-TILL-DLI.                                                    
000220     03  W-IDARTNR-X.                                                     
000221         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000222     03  W-KDSEGKEY-X.                                                    
000223         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
000224     03  W-IDLEVNR-X.                                                     
000225         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000226     SKIP2                                                                
000227*    --- STATUS-KOD FRÅN IMS                                              
000228 01  STATUS-WS                   PIC XX.                                  
000229     88  SEGMENT-FINNS                       VALUE '  '.                  
000230     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000231     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000232     SKIP2                                                                
000233 01  GODK-STATUSKODER.                                                    
000234     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000235     SKIP3                                                                
000236 01  SSA1                        PIC X(64).                               
000237 01  SSA2                        PIC X(64).                               
000238     EJECT                                                                
000239*    --- IMS FUNKTIONSKODER                                               
000240*01  -COPY W0003                                                          
000241     EJECT                                                                
000242*    ---  DLI INPUT-OUTPUT AREA                                           
000243 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-K601'.        
000244     SKIP3                                                                
000245 01  DLI-IO-AREA-K601.                                                    
000246*    03  -COPY WDK601                                                     
000247     SKIP3                                                                
000248                                                                          
000249 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-K611'.        
000250     SKIP3                                                                
000251 01  DLI-IO-AREA-K611.                                                    
000254*    03  -COPY WDK611                                                     
000255     EJECT                                                                
000256                                                                          
000257 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-F101'.        
000258     SKIP3                                                                
000259 01  DLI-IO-AREA-F101.                                                    
000260*    03  -COPY WDF101  -PRE WDF1-                                         
000262     EJECT                                                                
000263                                                                          
000264                                                                          
000265 LINKAGE SECTION.                                                         
000266                                                                          
000267                                                                          
000268*01  -COPY W0008  -PRE ARTC-                                              
000269     05  FILLER                  PIC X.                                   
000270     EJECT                                                                
000271*01  -COPY W0008  -PRE LEVA-                                              
000272     05  FILLER                  PIC X.                                   
000273     EJECT                                                                
000274 PROCEDURE DIVISION  USING ARTC-PCB LEVA-PCB.                             
000275     ENTRY 'DLITCBL' USING ARTC-PCB LEVA-PCB.                             
000276     SKIP2                                                                
000277     PERFORM A-INIT                                                       
000278                                                                          
000279     PERFORM S01-LAS-A31731                                               
000280     PERFORM UNTIL A31731-EOF = JA                                        
000281       EVALUATE POSTTYP                                                   
000282          WHEN 984 PERFORM B-SKAPA-R05                                    
000283          WHEN 985 PERFORM S13-SKRIV-W09286-985-POST                      
000284          WHEN 940 PERFORM C-BESTAEM-POSTTYP-UT                           
000285                   PERFORM D-SKAPA-R22-ELLER-R23                          
000286          WHEN 941 PERFORM E-SKAPA-R22                                    
000287       END-EVALUATE                                                       
000288       PERFORM S01-LAS-A31731                                             
000289     END-PERFORM                                                          
000290                                                                          
000291     PERFORM Z-FINIT                                                      
000292     SKIP2                                                                
000293     MOVE ZERO TO RETURN-CODE                                             
000294     GOBACK                                                               
000295     .                                                                    
000296     EJECT                                                                
000297                                                                          
000298                                                                          
000299 A-INIT SECTION.                                                          
000300     SKIP2                                                                
000301     OPEN INPUT A31731-INFIL                                              
000302     OPEN OUTPUT W09285-UTFIL                                             
000303                 W09286-UTFIL                                             
000304     SKIP2                                                                
000305     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000306     .                                                                    
000307     EJECT                                                                
000308                                                                          
000309                                                                          
000310 B-SKAPA-R05 SECTION.                                                     
000311     SKIP2                                                                
000312     MOVE SPACE          TO UTR05-W092R05T                                
000313     MOVE 'R05'          TO UTR05-IDPTYP                                  
000314     MOVE ZERO           TO UTR05-KDCLAGER                                
000315     MOVE IN984-ARTNR    TO WS-ARTNR                                      
000316     MOVE WS-ARTNR       TO UTR05-IDARTNR                                 
000317     MOVE 'IDINK'        TO WS-IDINK-X                                    
000318     MOVE WS-IDELMT      TO UTR05-IDELMT                                  
000319     MOVE ' '            TO UTR05-KDTECKEN-NYTT                           
000320     MOVE SPACE             TO WS-INKNR-NY                                
000321     IF IN984-INKNR-NY > ZERO                                             
000322        MOVE IN984-INKNR-NY TO WS-INKNR                                   
000323     ELSE                                                                 
000324        MOVE IN984-IDINK    TO WS-IDINK                                   
000325     END-IF                                                               
000326     MOVE WS-INKNR-NY       TO UTR05-IDFVARDE-NYTT                        
000327     PERFORM S11-SKRIV-W09285                                             
000328     .                                                                    
000329     EJECT                                                                
000330                                                                          
000331                                                                          
000332 C-BESTAEM-POSTTYP-UT SECTION.                                            
000333                                                                          
000334     MOVE IN940-BESTPREF TO WS-BESTPREF                                   
000335     MOVE IN940-BESTLNR  TO WS-BESTLNR                                    
000336     IF (WS-BESTLNR-POS1 = 9 OR WS-BESTPREF = 004) AND                    
000337        (IN940-ANT-BESTANN = 0)                                           
000338        MOVE 'R23' TO WS-POSTTYP-UT                                       
000339     ELSE                                                                 
000340        MOVE 'R22' TO WS-POSTTYP-UT                                       
000341     END-IF                                                               
000342     .                                                                    
000343     EJECT                                                                
000344                                                                          
000345                                                                          
000346 D-SKAPA-R22-ELLER-R23 SECTION.                                           
000347                                                                          
000348     MOVE WS-POSTTYP-UT  TO UTR2X-IDPTYP                                  
000349     MOVE IN940-ARTNR    TO WS-ARTNR                                      
000350     MOVE WS-ARTNR       TO UTR2X-IDARTNR                                 
000351                                                                          
000352     MOVE IN940-BESTPREF    TO WS-BESTPREF                                
000353     MOVE IN940-BESTLNR     TO WS-BESTLNR                                 
000354     MOVE IN940-BESTSUFF    TO WS-BESTSUFF                                
000355     MOVE WS-BESTNR         TO UTR2X-IDBEST                               
000356     MOVE IN940-GSDB-LEV    TO UTR2X-IDLEVNR-BEST                         
000357     MOVE IN940-DATUM-UTSKR TO WS-DATUM-UTSKR                             
000358     MOVE WS-DATUM-UTSKR    TO UTR2X-TIBEST                               
000359     MOVE IN940-ANT-BESTANN TO WS-ANT-BESTANN                             
000360     MOVE WS-ANT-BESTANN    TO UTR2X-KVBEST                               
000361     MOVE '1'               TO UTR2X-KDBEH-BEST                           
000362     MOVE SPACE             TO UTR2X-TENOT-BESTPRIS                       
000363*                                                                         
000364     IF WS-POSTTYP-UT = 'R23'                                             
000365       MOVE IN940-IDLEVNR-SHIP TO UTR2X-IDLEVNR-SHIP                      
000366     ELSE                                                                 
000367       MOVE SPACE              TO UTR2X-IDLEVNR-SHIP                      
000368     END-IF                                                               
000369*                                                                         
000370     PERFORM S11-SKRIV-W09285                                             
000371     .                                                                    
000372     EJECT                                                                
000373                                                                          
000374                                                                          
000375 E-SKAPA-R22 SECTION.                                                     
000376     SKIP2                                                                
000377     MOVE 'R22'               TO UTR2X-IDPTYP                             
000378     MOVE IN941-ARTNR         TO WS-ARTNR                                 
000379     MOVE WS-ARTNR            TO UTR2X-IDARTNR                            
000380                                                                          
000381     MOVE IN941-BESTPREF      TO WS-BESTPREF                              
000382     MOVE IN941-BESTLNR       TO WS-BESTLNR                               
000383     MOVE IN941-BESTSUFF      TO WS-BESTSUFF                              
000384     MOVE WS-BESTNR           TO UTR2X-IDBEST                             
000385                                                                          
000386     MOVE IN941-GSDB-LEV      TO UTR2X-IDLEVNR-BEST                       
000387                                                                          
000388     MOVE IN941-DATUM-UTSKR   TO WS-DATUM-UTSKR                           
000389     MOVE WS-DATUM-UTSKR      TO UTR2X-TIBEST                             
000390     MOVE ZERO                TO UTR2X-KVBEST                             
000391                                                                          
000392     MOVE '5'                 TO UTR2X-KDBEH-BEST                         
000393     MOVE SPACE               TO UTR2X-TENOT-BESTPRIS                     
000394     MOVE SPACE               TO UTR2X-IDLEVNR-SHIP                       
000395     PERFORM S12-SKRIV-W09285                                             
000396     .                                                                    
000397     EJECT                                                                
000398                                                                          
000399 Z-FINIT SECTION.                                                         
000400     SKIP2                                                                
000401     CLOSE A31731-INFIL                                                   
000402           W09285-UTFIL                                                   
000403           W09286-UTFIL                                                   
000404     SKIP2                                                                
000405     MOVE 'S' TO POSTSUM-OPKOD                                            
000406     CALL POSTSUM USING POSTSUM-PARM                                      
000407     .                                                                    
000408     EJECT                                                                
000409                                                                          
000410                                                                          
000411 S01-LAS-A31731 SECTION.                                                  
000412                                                                          
000413     READ A31731-INFIL INTO IN-AREA                                       
000414     AT END                                                               
000415        MOVE JA TO A31731-EOF                                             
000416     NOT AT END                                                           
000417        MOVE 'TRANS '   TO POSTSUM-FDNAMN                                 
000418        MOVE 'W09278D1' TO POSTSUM-DDNAMN2                                
000419        MOVE POSTTYP    TO POSTSUM-TRANSTYP                               
000420        CALL POSTSUM USING POSTSUM-PARM                                   
000421     END-READ                                                             
000422     .                                                                    
000423     EJECT                                                                
000424 S11-SKRIV-W09285 SECTION.                                                
000425                                                                          
000426     IF POSTTYP = 984                                                     
000427        WRITE W09285-POST FROM UTR05-AREA                                 
000428        MOVE UTR05-IDPTYP TO POSTSUM-TRANSTYP                             
000429     ELSE                                                                 
000430        WRITE W09285-POST FROM UTR2X-AREA                                 
000431        MOVE UTR2X-IDPTYP TO POSTSUM-TRANSTYP                             
000432     END-IF                                                               
000433                                                                          
000434     MOVE 'UTFIL '   TO POSTSUM-FDNAMN                                    
000435     MOVE 'W09278D5' TO POSTSUM-DDNAMN2                                   
000436     CALL POSTSUM USING POSTSUM-PARM                                      
000437     .                                                                    
000438     EJECT                                                                
000439                                                                          
000440                                                                          
000441 S12-SKRIV-W09285 SECTION.                                                
000442                                                                          
000443     WRITE W09285-POST FROM UTR2X-AREA                                    
000444     MOVE UTR2X-IDPTYP TO POSTSUM-TRANSTYP                                
000445                                                                          
000446     MOVE 'UTFIL '   TO POSTSUM-FDNAMN                                    
000447     MOVE 'W09278D5' TO POSTSUM-DDNAMN2                                   
000448     CALL POSTSUM USING POSTSUM-PARM                                      
000449     .                                                                    
000450     SKIP2                                                                
000451 S13-SKRIV-W09286-985-POST SECTION.                                       
000452                                                                          
000453     MOVE '985'                TO UT985-IDPTYP                            
000454     MOVE IN985-ARTNR          TO UT985-IDARTNR                           
000455     MOVE IN985-GSDB-LEV       TO UT985-IDLEVNR                           
000456     MOVE IN985-BESTPRIS       TO UT985-PRARTBEL                          
000457     MOVE IN985-KDENH-BEST     TO UT985-KDANTENH                          
000458     MOVE IN985-DATUM-BESTPRIS TO UT985-TIPRLIST                          
000459     MOVE IN985-KDVAL-ISO      TO UT985-KDVALISO                          
000460     MOVE 'INKOP   '           TO UT985-IDUSER                            
000461     IF IN985-KDFPKPRI = 'Y' OR 'J' OR 'N' OR  '?' OR ' '                 
000462       MOVE IN985-KDFPKPRI       TO UT985-KDFPKPRI                        
000463     ELSE                                                                 
000464       MOVE SPACE                TO UT985-KDFPKPRI                        
000465     END-IF                                                               
000466     WRITE W09286-POST FROM UT985-AREA                                    
000467                                                                          
000468     MOVE '985'                TO POSTSUM-TRANSTYP                        
000469     MOVE 'W09286'             TO POSTSUM-FDNAMN                          
000470     MOVE 'W09278D6'           TO POSTSUM-DDNAMN2                         
000471     CALL POSTSUM USING POSTSUM-PARM                                      
000472     .                                                                    
000473                                                                          
000474 S99-ABEND SECTION.                                                       
000475                                                                          
000476     SKIP2                                                                
000477     MOVE 'S' TO POSTSUM-OPKOD                                            
000478     CALL POSTSUM USING POSTSUM-PARM                                      
000479     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000480     .                                                                    
000481     EJECT                                                                
000482* --- IMS SEKTIONER ---                                                   
000483     SKIP3                                                                
000484                                                                          
000485 IMS-GET-ART SECTION.                                                     
000486     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000487          DELIMITED BY SIZE INTO SSA1                                     
000488     MOVE '  GE' TO GODK-STATUSKODER                                      
000489     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-K601 SSA1                 
000490     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000491     PERFORM IMS-STATUSKONTROLL                                           
000492     .                                                                    
000493     SKIP3                                                                
000494                                                                          
000495                                                                          
000496 IMS-GET-CLAG SECTION.                                                    
000497     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
000498     MOVE '  GE' TO GODK-STATUSKODER                                      
000499     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-K611 SSA1                
000500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000501     PERFORM IMS-STATUSKONTROLL                                           
000502     .                                                                    
000503     EJECT                                                                
000504                                                                          
000505                                                                          
000506 IMS-GET-WDF101 SECTION.                                                  
000507     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
000508          DELIMITED BY SIZE INTO SSA1                                     
000509     MOVE '  GE' TO GODK-STATUSKODER                                      
000510     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-F101 SSA1                 
000511     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
000512     PERFORM IMS-STATUSKONTROLL                                           
000513     .                                                                    
000514     SKIP3                                                                
000515                                                                          
000516                                                                          
000517                                                                          
000518 IMS-STATUSKONTROLL SECTION.                                              
000519     SKIP2                                                                
000520     SET STATUS-IX TO 1                                                   
000521     SEARCH GODK-STATUS                                                   
000522       AT END                                                             
000523         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
000524         DISPLAY FELTEXT                                                  
000525         CALL FELLOG                                                      
000526       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000527         CONTINUE                                                         
000528     END-SEARCH                                                           
000529     .                                                                    
