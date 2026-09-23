000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4784000.                                                
000003 AUTHOR.         BO SVENSSON.                                             
000004 DATE-WRITTEN.   97/08/25.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        SB.                                                              
000009*        SKAPAR POSTER FÖR ALLA ORDERDELAR MED GÅRDAGENS RFS              
000010*        TILL FILER FÖR UPPFÖLJNING.                                      
000011*                                                                         
000012*        FIL  W47840 FöR PRC UPPFÖLJNING                                  
000013*             W47841 FöR RFS UPPFÖLJNING                                  
000014*             W47842 FöR D&P UPPFÖLJNING                                  
000015*                                                                         
000016*        PROGRAMMET LÄSER      WDQ3   SB                                  
000017*                              WDE6                                       
000018*                              WDQ2                                       
000019*                                                                         
000020*    ABENDKODER:                                                          
000021*        U0016 -  . . . .                                                 
000022*        U1000 -  . . . .                                                 
000023*                                                                         
000024                                                                          
000025     SKIP3                                                                
000026 ENVIRONMENT DIVISION.                                                    
000027     SKIP2                                                                
000028 INPUT-OUTPUT SECTION.                                                    
000029                                                                          
000030 FILE-CONTROL.                                                            
000031     SKIP2                                                                
000032*          --- FIL TILL RFS-PRC UPPF                                      
000033     SELECT W47840                     ASSIGN TO W47840D1.                
000034*          --- FIL TILL RFS-TRP UPPF                                      
000035     SELECT W47841                     ASSIGN TO W47840D2.                
000036*          --- FIL TILL RFS-D&P UPPF                                      
000037     SELECT W47842                     ASSIGN TO W47840D3.                
000038     EJECT                                                                
000039 DATA DIVISION.                                                           
000040     SKIP2                                                                
000041 FILE SECTION.                                                            
000042     SKIP3                                                                
000043 FD  W47840                                                               
000044     RECORDING       F                                                    
000045     BLOCK CONTAINS  0.                                                   
000046                                                                          
000047*01  POST -COPY W47840 -PRE  PRC-  -L.                                    
000048     EJECT                                                                
000049 FD  W47841                                                               
000050     RECORDING       F                                                    
000051     BLOCK CONTAINS  0.                                                   
000052                                                                          
000053*01  POST -COPY W47841 -PRE  TRP-  -L.                                    
000054     EJECT                                                                
000055 FD  W47842                                                               
000056     RECORDING       F                                                    
000057     BLOCK CONTAINS  0.                                                   
000058                                                                          
000059*01  POST -COPY W47842 -PRE  DoP-  -L.                                    
000060     EJECT                                                                
000061 WORKING-STORAGE SECTION.                                                 
000062                                                                          
000063     SKIP3                                                                
000064*    -- CHECKED BY WY2000                                                 
000065 77  IDPGM                       PIC X(8)    VALUE 'W4784000'.            
000066 77  JA                          PIC X       VALUE 'J'.                   
000067 77  NEJ                         PIC X       VALUE 'N'.                   
000068                                                                          
000069     EJECT                                                                
000070 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
000071 01  FILLER REDEFINES DAGENS-DATUM.                                       
000072     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
000073     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000074     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000075     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000076 01  DAGENS-DATUM-VECKA          PIC 9(2)    VALUE ZERO.                  
000077     EJECT                                                                
000078 01  DYNAMISKA-SUBPROGRAM.                                                
000079*                                                                         
000080     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000081     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000082     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000083     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000084     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000085     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000086     SKIP2                                                                
000087*    --- PARAMETRAR TILL ABEND                                            
000088                                                                          
000089 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000090 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000091 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000092     SKIP2                                                                
000093 01  FELTEXT.                                                             
000094     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000095     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000096     EJECT                                                                
000097*    --- PARAMETRAR TILL DATKORT                                          
000098*                                                                         
000099 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47840'.              
000100     SKIP2                                                                
000101 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
000102     SKIP2                                                                
000103*01  -COPY WDATKORT                                                       
000104                                                                          
000105*    --- PARAMETRAR TILL WDATKONV                                         
000106 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
000107*01  -COPY WDATAREA                                                       
000108                                                                          
000109     EJECT                                                                
000110*      --- VALID IDDC CODES                                               
000111*                                                                         
000112*01    -COPY WWDC99                                                       
000113       EJECT                                                              
000114*    --- PARAMETRAR TILL POSTSUM                                          
000115*                                                                         
000116*01  -COPY W0005   -PRE  POSTSUM-                                         
000117     EJECT                                                                
000118 01  UT-AREA-START               PIC X(24)   VALUE                        
000119                                 'UT-AREA-START  '.                       
000120     SKIP2                                                                
000121                                                                          
000122*01  AREA -COPY W47840     -PRE UTPRC-                                    
000123     EJECT                                                                
000124*01  AREA -COPY W47841     -PRE UTTRP-                                    
000125     EJECT                                                                
000126*01  AREA -COPY W47842     -PRE UTDoP-                                    
000127     EJECT                                                                
000128*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000129*                                                                         
000130     EJECT                                                                
000131 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000132     SKIP3                                                                
000133 01  NYCKLAR-TILL-DLI.                                                    
000134     03  W-IDPRODNR-X.                                                    
000135         05  W-IDPRODNR          PIC S9(7)    COMP-3 VALUE ZERO.          
000136     03  W-IDORDER-X.                                                     
000137         05  W-IDORDER           PIC S9(7)    COMP-3 VALUE ZERO.          
000138     SKIP2                                                                
000139*    --- STATUS-KOD FRÅN IMS                                              
000140 01  STATUS-WS                   PIC XX.                                  
000141     88  SEGMENT-FINNS                       VALUE '  '.                  
000142     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000143     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000144     SKIP2                                                                
000145 01  GODK-STATUSKODER.                                                    
000146     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000147     SKIP3                                                                
000148 01  SSA1                        PIC X(64).                               
000149 01  SSA2                        PIC X(64).                               
000150     EJECT                                                                
000151*    --- IMS FUNKTIONSKODER                                               
000152*01  -COPY W0003                                                          
000153     EJECT                                                                
000154*    ---  DLI INPUT-OUTPUT AREA                                           
000155 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3'.                        
000156 01  DLI-IO-WDQ3.                                                         
000157*    03  -COPY WDQ301                                                     
000158     EJECT                                                                
000159 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
000160 01  DLI-IO-WDE601.                                                       
000161*    03  -COPY WDE601                                                     
000162     EJECT                                                                
000163 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
000164 01  DLI-IO-WDQ201.                                                       
000165*    03  -COPY WDQ201                                                     
000166 LINKAGE SECTION.                                                         
000167                                                                          
000168     EJECT                                                                
000169*01  -COPY W0008  -PRE WDQ3-                                              
000170     05  FILLER                  PIC X.                                   
000171     EJECT                                                                
000172*01  -COPY W0008  -PRE WDE6-                                              
000173     05  FILLER                  PIC X.                                   
000174     EJECT                                                                
000175*01  -COPY W0008  -PRE WDQ2-                                              
000176     05  FILLER                  PIC X.                                   
000177     EJECT                                                                
000178 PROCEDURE DIVISION  USING WDQ3-PCB WDE6-PCB WDQ2-PCB.                    
000179 MAIN SECTION.                                                            
000180     ENTRY 'DLITCBL' USING WDQ3-PCB WDE6-PCB WDQ2-PCB.                    
000181                                                                          
000182                                                                          
000183     PERFORM A-INIT                                                       
000184                                                                          
000185     PERFORM IMS-GET-WDQ3                                                 
000186     PERFORM UNTIL SEGMENT-SLUT                                           
000187       EVALUATE WDQ3-SEG-NAME-FB                                          
000188         WHEN 'WDQ301'                                                    
000189           MOVE ODEL-IDDC     TO WS-IDDC                                  
000190           IF  DAGENS-DATUM = ODEL-DARFSDAT                               
000191*          AND (NDC-US OR NDC-CA OR NDC-JP OR NDC-AU)                     
000192               PERFORM B-SKAPA-TRANSAR                                    
000193           END-IF                                                         
000194       END-EVALUATE                                                       
000195       PERFORM IMS-GET-WDQ3                                               
000196     END-PERFORM                                                          
000197     PERFORM Z-FINIT                                                      
000198                                                                          
000199     MOVE ZERO TO RETURN-CODE                                             
000200     GOBACK                                                               
000201     .                                                                    
000202     EJECT                                                                
000203 A-INIT SECTION.                                                          
000204                                                                          
000205     OPEN OUTPUT W47840                                                   
000206                 W47841                                                   
000207                 W47842                                                   
000208                                                                          
000209     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000210     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
000211     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
000212     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
000213     MOVE D-VECKA     TO DAGENS-DATUM-VECKA                               
000214     IF DAGENS-DATUM-AAR < 50                                             
000215       MOVE 20        TO DAGENS-DATUM-SEKEL                               
000216     ELSE                                                                 
000217       MOVE 19        TO DAGENS-DATUM-SEKEL                               
000218     END-IF                                                               
000219     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000220                                                                          
000221     MOVE DAGENS-DATUM  TO DAT-I-TIDATUM                                  
000222     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
000223     CALL WDATKONV USING   DAT-KDDATFORM,                                 
000224                           DAT-I-TIDATUM                                  
000225                           DAT-O-TIDATUM,                                 
000226                           DAT-KDSVAR                                     
000227                                                                          
000228     IF NOT DAT-KDSVAR-OK                                                 
000229        MOVE 'FEL FRÅN WDATKONV I A-INIT'                                 
000230                        TO FELTEXT-STR                                    
000231        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
000232     END-IF                                                               
000233     .                                                                    
000234                                                                          
000235     EJECT                                                                
000236 B-SKAPA-TRANSAR SECTION.                                                 
000237                                                                          
000238     MOVE ODEL-IDDC             TO UTTRP-IDDC                             
000239                                   UTPRC-IDDC                             
000240                                   UTDoP-IDDC                             
000241     MOVE DAGENS-DATUM (3:6)    TO UTDoP-TIDATUM                          
000242     MOVE DAGENS-DATUM-AAR      TO UTDoP-TIAAVV(1:2)                      
000243     MOVE DAGENS-DATUM-VECKA    TO UTDoP-TIAAVV(3:2)                      
000244     MOVE DAT-TIAARP            TO UTDoP-TIAARP                           
000245                                                                          
000246     MOVE ODEL-DARFSDAT (3:6)   TO UTTRP-TIRFSDAT                         
000247                                   UTPRC-TIRFSDAT                         
000248     MOVE ODEL-DARFS            TO UTDoP-TIRFSTID                         
000249                                                                          
000250     MOVE ODEL-IDTRP      TO UTTRP-IDTRP                                  
000251     MOVE ODEL-IDPRC      TO UTPRC-IDPRC                                  
000252                             UTDoP-IDPRC                                  
000253     MOVE ODEL-KVRADER    TO UTTRP-KVORDRAD                               
000254                             UTPRC-KVORDRAD                               
000255                             UTDoP-KVORDRAD                               
000256     MOVE +1              TO UTTRP-KVORDER                                
000257                             UTPRC-KVORDER                                
000258                             UTDoP-KVORDER                                
000259     IF ODEL-KDODELSTA = 'U'                                              
000260       MOVE ODEL-KVRADER  TO UTTRP-KVORDRAD-UTSKR                         
000261                             UTPRC-KVORDRAD-UTSKR                         
000262                             UTDoP-KVORDRAD-UTSKR                         
000263       MOVE +1            TO UTTRP-KVORDER-UTSKR                          
000264                             UTPRC-KVORDER-UTSKR                          
000265                             UTDoP-KVORDER-UTSKR                          
000266       MOVE ZERO          TO UTTRP-KVORDRAD-PACK                          
000267                             UTPRC-KVORDRAD-PACK                          
000268                             UTDoP-KVORDRAD-PACK                          
000269                             UTTRP-KVORDER-PACK                           
000270                             UTPRC-KVORDER-PACK                           
000271                             UTDoP-KVORDER-PACK                           
000272     ELSE                                                                 
000273       IF ODEL-KDODELSTA = 'P'                                            
000274         MOVE ODEL-KVRADER TO UTTRP-KVORDRAD-PACK                         
000275                              UTPRC-KVORDRAD-PACK                         
000276                              UTDoP-KVORDRAD-PACK                         
000277                              UTTRP-KVORDRAD-UTSKR                        
000278                              UTPRC-KVORDRAD-UTSKR                        
000279                              UTDoP-KVORDRAD-UTSKR                        
000280         MOVE +1           TO UTTRP-KVORDER-UTSKR                         
000281                              UTPRC-KVORDER-UTSKR                         
000282                              UTDoP-KVORDER-UTSKR                         
000283                              UTTRP-KVORDER-PACK                          
000284                              UTPRC-KVORDER-PACK                          
000285                              UTDoP-KVORDER-PACK                          
000286       ELSE                                                               
000287         MOVE ZERO         TO UTTRP-KVORDRAD-PACK                         
000288                              UTTRP-KVORDRAD-UTSKR                        
000289                              UTPRC-KVORDRAD-PACK                         
000290                              UTPRC-KVORDRAD-UTSKR                        
000291                              UTDoP-KVORDRAD-PACK                         
000292                              UTDoP-KVORDRAD-UTSKR                        
000293                              UTTRP-KVORDER-UTSKR                         
000294                              UTPRC-KVORDER-UTSKR                         
000295                              UTDoP-KVORDER-UTSKR                         
000296                              UTTRP-KVORDER-PACK                          
000297                              UTPRC-KVORDER-PACK                          
000298                              UTDoP-KVORDER-PACK                          
000299       END-IF                                                             
000300     END-IF                                                               
000301                                                                          
000302     MOVE UTDoP-KVORDER-PACK    TO UTDoP-KVORDER-PACK                     
000303                                                                          
000304     IF ODEL-KDODELSTA = 'P' OR 'U'                                       
000305       MOVE ODEL-IDPRODNR TO W-IDPRODNR                                   
000306                             UTTRP-IDPRODNR                               
000307       PERFORM IMS-GU-WDE601                                              
000308       IF SEGMENT-FINNS                                                   
000309         MOVE VORD-KVKOLLI      TO UTTRP-KVKOLLI                          
000310                                   UTDoP-KVKOLLI                          
000311         MOVE VORD-KVKOLLI-FL   TO UTTRP-KVKOLLI-LAST                     
000312                                   UTDoP-KVKOLLI-LAST                     
000313         MOVE VORD-KVKOLLI-FAKT TO UTTRP-KVKOLLI-FAKT                     
000314         MOVE VORD-FLDIRLEV     TO UTDoP-FLDIRLEV                         
000315       ELSE                                                               
000316         MOVE ZERO              TO UTTRP-KVKOLLI                          
000317                                   UTDoP-KVKOLLI                          
000318                                   UTTRP-KVKOLLI-LAST                     
000319                                   UTDoP-KVKOLLI-LAST                     
000320                                   UTTRP-KVKOLLI-FAKT                     
000321         MOVE NEJ               TO UTDoP-FLDIRLEV                         
000322       END-IF                                                             
000323     ELSE                                                                 
000324         MOVE ZERO              TO UTTRP-KVKOLLI                          
000325                                   UTDoP-KVKOLLI                          
000326                                   UTTRP-KVKOLLI-LAST                     
000327                                   UTDoP-KVKOLLI-LAST                     
000328                                   UTTRP-KVKOLLI-FAKT                     
000329                                   UTTRP-IDPRODNR                         
000330         MOVE NEJ               TO UTDoP-FLDIRLEV                         
000331     END-IF                                                               
000332                                                                          
000333     MOVE ODEL-IDDISTR  TO UTDoP-IDDISTR                                  
000334     MOVE ODEL-IDKUNDNR TO UTDoP-IDKUNDNR                                 
000335     MOVE ODEL-IDORDNR7 TO UTDoP-IDORDNR7                                 
000336     MOVE ODEL-IDORDER  TO W-IDORDER                                      
000337     PERFORM IMS-GU-WDQ201                                                
000338     IF SEGMENT-FINNS                                                     
000339       MOVE OHUV-KDORDKL      TO UTTRP-KDORDKL                            
000340                                 UTPRC-KDORDKL                            
000341                                 UTDoP-KDORDKL                            
000342     ELSE                                                                 
000343       MOVE +3               TO UTTRP-KDORDKL                             
000344                                UTPRC-KDORDKL                             
000345                                UTDoP-KDORDKL                             
000346     END-IF                                                               
000347                                                                          
000348     IF NDC-US OR NDC-CA OR NDC-JP OR NDC-AU                              
000349        PERFORM S11-SKRIV-W47841                                          
000350        PERFORM S12-SKRIV-W47840                                          
000351     end-if                                                               
000352     IF NOT CDC-SE                                                        
000353        PERFORM S13-SKRIV-W47842                                          
000354     END-IF                                                               
000355     .                                                                    
000356     EJECT                                                                
000357 Z-FINIT SECTION.                                                         
000358     CLOSE W47840                                                         
000359           W47841                                                         
000360           W47842                                                         
000361     SKIP2                                                                
000362     MOVE 'S' TO POSTSUM-OPKOD                                            
000363     CALL POSTSUM USING POSTSUM-PARM                                      
000364     .                                                                    
000365     EJECT                                                                
000366 S11-SKRIV-W47841 SECTION.                                                
000367                                                                          
000368     WRITE TRP-POST FROM UTTRP-AREA                                       
000369                                                                          
000370     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
000371     MOVE 'W47841' TO POSTSUM-FDNAMN                                      
000372     MOVE 'W47840D2' TO POSTSUM-DDNAMN2                                   
000373     CALL POSTSUM USING POSTSUM-PARM                                      
000374     .                                                                    
000375     EJECT                                                                
000376 S12-SKRIV-W47840 SECTION.                                                
000377                                                                          
000378     WRITE PRC-POST FROM UTPRC-AREA                                       
000379                                                                          
000380     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
000381     MOVE 'W47840' TO POSTSUM-FDNAMN                                      
000382     MOVE 'W47840D1' TO POSTSUM-DDNAMN2                                   
000383     CALL POSTSUM USING POSTSUM-PARM                                      
000384     .                                                                    
000385     EJECT                                                                
000386 S13-SKRIV-W47842 SECTION.                                                
000387                                                                          
000388     WRITE DoP-POST FROM UTDoP-AREA                                       
000389                                                                          
000390     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
000391     MOVE 'W47842' TO POSTSUM-FDNAMN                                      
000392     MOVE 'W47840D3' TO POSTSUM-DDNAMN2                                   
000393     CALL POSTSUM USING POSTSUM-PARM                                      
000394     .                                                                    
000395     EJECT                                                                
000396*S99-ABEND SECTION.                                                       
000397*                                                                         
000398*    SKIP2                                                                
000399*    MOVE 'S' TO POSTSUM-OPKOD                                            
000400*    CALL POSTSUM USING POSTSUM-PARM                                      
000401*    CALL ABEND USING RKOD-ABEND                                          
000402*    .                                                                    
000403*    EJECT                                                                
000404* --- IMS SEKTIONER ---                                                   
000405     SKIP3                                                                
000406     EJECT                                                                
000407 IMS-GET-WDQ3   SECTION.                                                  
000408                                                                          
000409     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ3                           
000410     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
000411     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000412     PERFORM IMS-STATUSKONTROLL                                           
000413     .                                                                    
000414     EJECT                                                                
000415 IMS-GU-WDE601 SECTION.                                                   
000416                                                                          
000417     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
000418          DELIMITED BY SIZE INTO SSA1                                     
000419     MOVE '  GE' TO GODK-STATUSKODER                                      
000420     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
000421     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000422     PERFORM IMS-STATUSKONTROLL                                           
000423     .                                                                    
000424     EJECT                                                                
000425 IMS-GU-WDQ201 SECTION.                                                   
000426                                                                          
000427     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
000428          DELIMITED BY SIZE INTO SSA1                                     
000429     MOVE '  GE' TO GODK-STATUSKODER                                      
000430     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
000431     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
000432     PERFORM IMS-STATUSKONTROLL                                           
000433     .                                                                    
000434     EJECT                                                                
000435 IMS-STATUSKONTROLL SECTION.                                              
000436                                                                          
000437     SET STATUS-IX TO 1                                                   
000438     SEARCH GODK-STATUS                                                   
000439       AT END                                                             
000440         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000441           DELIMITED BY SIZE INTO FELTEXT                                 
000442         DISPLAY FELTEXT                                                  
000443         CALL FELLOG                                                      
000444       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000445         CONTINUE                                                         
000446     END-SEARCH                                                           
000447     .                                                                    
000448     EJECT                                                                
