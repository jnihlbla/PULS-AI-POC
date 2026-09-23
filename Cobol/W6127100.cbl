000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6127100.                                                
000003 AUTHOR.         JOHAN NIHLBLAD.                                          
000004 DATE-WRITTEN.   FEB-09.                                                  
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*                                                                         
000008*    FUNKTION:                                                            
000009*        DAGLIG NEDLÄSNING WDL6                                           
000010*        MEN FÖRST, LÄS IGENOM HELA DC-BASEN WDB6                         
000011*                                                                         
000012                                                                          
000013     SKIP3                                                                
000014 ENVIRONMENT DIVISION.                                                    
000015     SKIP2                                                                
000016 INPUT-OUTPUT SECTION.                                                    
000017                                                                          
000018 FILE-CONTROL.                                                            
000019     SKIP2                                                                
000020*          --- DAGLIG NEDLÄSNING WDL6 R34                                 
000021     SELECT UTFIL                      ASSIGN TO W61271D1.                
000022*          --- BINNING DATE FROM WDL6                                     
000023     SELECT UTFIL2                     ASSIGN TO W61271D2.                
000024     EJECT                                                                
000025 DATA DIVISION.                                                           
000026     SKIP2                                                                
000027 FILE SECTION.                                                            
000028     SKIP3                                                                
000029 FD  UTFIL                                                                
000030     RECORDING       F                                                    
000031     BLOCK CONTAINS  0.                                                   
000032                                                                          
000033*01  POST -COPY W414100A -PRE  UT-  -L.                                   
000034     EJECT                                                                
000035 FD  UTFIL2                                                               
000036     RECORDING       F                                                    
000037     BLOCK CONTAINS  0.                                                   
000038                                                                          
000039*01  POST -COPY W61271A  -PRE  UT2-  -L.                                  
000040     EJECT                                                                
000041 WORKING-STORAGE SECTION.                                                 
000042                                                                          
000043 77  IDPGM                       PIC X(8)    VALUE 'W6127100'.            
000044 77  JA                          PIC X       VALUE 'J'.                   
000045 77  NEJ                         PIC X       VALUE 'N'.                   
000046 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
000047 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
000048                                                                          
000049 01  W-IDLOPNRM                  PIC 9(9).                                
000050 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
000051  03 FILLER                      PIC 9(1).                                
000052  03 W-VVD                       PIC 9(3).                                
000053  03 W-LLLL                      PIC 9(4).                                
000054  03 W-K                         PIC 9(1).                                
000055                                                                          
000056                                                                          
000057 01    WS-IDDC-TABELL.                                                    
000058    03 WS-VALID-IDDC  OCCURS 200.                                         
000059       05 TAB-IDDC            PIC X(2).                                   
000060       05 TAB-KDDC            PIC X(2).                                   
000061       05 TAB-IDLEVNR         PIC X(5).                                   
000062 01    WS-TIINLINL-TABLE.                                                 
000063    03 WS-LAST-TIINLINL OCCURS 200.                                       
000064       05 TAB-IDDC-L6         PIC X(2) VALUE SPACES.                      
000065       05 TAB-TIINLINL        PIC 9(6) VALUE ZERO.                        
000066                                                                          
000067 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000068 01  FILLER REDEFINES DAGENS-DATUM.                                       
000069     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000070     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000071     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000072                                                                          
000073 01  DAGENS-DATUM-9KOMPL         PIC 9(6)    VALUE ZERO.                  
000074                                                                          
000075 01  WS-VECKA-DAG                PIC 9(3)    VALUE ZERO.                  
000076 01  FILLER REDEFINES WS-VECKA-DAG.                                       
000077     03  WS-VECKA                PIC 9(2).                                
000078     03  WS-DAG                  PIC 9(1).                                
000079                                                                          
000080 01  DYNAMISKA-SUBPROGRAM.                                                
000081     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000082     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000083     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000084     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000085                                                                          
000086 01  FELTEXT.                                                             
000087     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000088     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000089     EJECT                                                                
000090*    --- PARAMETRAR TILL DATKORT                                          
000091*                                                                         
000092 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61271'.              
000093 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
000094*01  -COPY WDATKORT                                                       
000095     EJECT                                                                
000096*    -COPY WY2000W1                                                       
000097*    --- PARAMETRAR TILL POSTSUM                                          
000098*                                                                         
000099*01  -COPY W0005   -PRE  POSTSUM-                                         
000100     EJECT                                                                
000101 01  UT-AREA-START               PIC X(24)   VALUE                        
000102                                 'UT-AREA-START  '.                       
000103                                                                          
000104*01  AREA -COPY W414100A   -PRE UT-                                       
000105     EJECT                                                                
000106 01  UT2-AREA-START              PIC X(24)   VALUE                        
000107                                 'UT2-AREA-START  '.                      
000108                                                                          
000109*01  AREA -COPY W61271A    -PRE UT2-                                      
000110     EJECT                                                                
000111*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000112*                                                                         
000113 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000114                                                                          
000115 01  NYCKLAR-TILL-DLI.                                                    
000116     03  W-IDARTNR-X.                                                     
000117         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000118                                                                          
000119     03  W-IDSKYLT-X.                                                     
000120         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
000121                                                                          
000122     03  W-IDDC-X.                                                        
000123         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000124                                                                          
000125     03  W-KDSEGKEY-X.                                                    
000126         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
000127                                                                          
000128                                                                          
000129*    --- STATUS-KOD FRÅN IMS                                              
000130 01  STATUS-WS                   PIC XX.                                  
000131     88  SEGMENT-FINNS                       VALUE '  '.                  
000132     88  SEG-LVL-CHG                         VALUE 'GA'.                  
000133     88  BASEN-SLUT                          VALUE 'GB'.                  
000134                                                                          
000135 01  GODK-STATUSKODER.                                                    
000136     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000137     SKIP3                                                                
000138 01  SSA1                        PIC X(160).                              
000139 01  SSA2                        PIC X(64).                               
000140     EJECT                                                                
000141*    --- IMS FUNKTIONSKODER                                               
000142*01  -COPY W0003                                                          
000143     EJECT                                                                
000144*    ---  DLI INPUT-OUTPUT AREA                                           
000145 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
000146 01  DLI-IO-WDL6.                                                         
000147     03 IO-AREA     PIC X(600) VALUE SPACE.                               
000148         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
000149*            05 -COPY WDL601                                              
000150     EJECT                                                                
000151         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
000152*            05 -COPY WDL611                                              
000153     EJECT                                                                
000154 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000155 01   DLI-IO-AREA-B601.                                                   
000156*     03  -COPY WDB601                                                    
000157                                                                          
000158 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
000159 01  DLI-IO-BENA01.                                                       
000160*    03  -COPY WDD301                                                     
000161     EJECT                                                                
000162                                                                          
000163 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
000164 01  DLI-IO-BENA11.                                                       
000165*    03  -COPY WDD311                                                     
000166     EJECT                                                                
000167                                                                          
000168 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
000169 01  DLI-IO-AREA-WDK711.                                                  
000170*    03  -COPY WDK711                                                     
000171     EJECT                                                                
000172 LINKAGE SECTION.                                                         
000173                                                                          
000174*01  -COPY W0008  -PRE WDL6-                                              
000175     05  FILLER                  PIC X.                                   
000176                                                                          
000177*01  -COPY W0008  -PRE WDB6-                                              
000178     05  FILLER                  PIC X.                                   
000179                                                                          
000180*01  -COPY W0008  -PRE  BENA-                                             
000181     05  FILLER                  PIC X.                                   
000182                                                                          
000183*01  -COPY W0008  -PRE WDK7-                                              
000184     05  FILLER                  PIC X.                                   
000185                                                                          
000186     EJECT                                                                
000187 PROCEDURE DIVISION  USING WDL6-PCB WDB6-PCB BENA-PCB                     
000188                           WDK7-PCB.                                      
000189 MAIN SECTION.                                                            
000190     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB BENA-PCB                     
000191                           WDK7-PCB.                                      
000192                                                                          
000193     PERFORM A-INIT                                                       
000194                                                                          
000195     PERFORM IMS-GET-WDL6                                                 
000196     PERFORM UNTIL BASEN-SLUT                                             
000197       EVALUATE WDL6-SEG-NAME-FB                                          
000198         WHEN 'WDL601'                                                    
000199           MOVE ART-IDARTNR TO UT-100-IDARTNR                             
000200                               W-IDARTNR                                  
000201                               UT2-IDARTNR                                
000202         WHEN 'WDL611'                                                    
000203           IF INL-IDPTYP = 'R34'                                          
000204           AND INL-KDRT = ZERO                                            
000205             PERFORM B-URVAL                                              
000206           END-IF                                                         
000207           PERFORM C-GET-BIN-DATE                                         
000208       END-EVALUATE                                                       
000209       PERFORM IMS-GET-WDL6                                               
000210       IF SEG-LVL-CHG                                                     
000211          PERFORM D-WRITE-DC                                              
000212       END-IF                                                             
000213     END-PERFORM                                                          
000214                                                                          
000215     PERFORM Z-FINIT                                                      
000216     MOVE ZERO TO RETURN-CODE                                             
000217     GOBACK                                                               
000218     .                                                                    
000219     EJECT                                                                
000220 A-INIT SECTION.                                                          
000221                                                                          
000222     OPEN OUTPUT UTFIL                                                    
000223                 UTFIL2                                                   
000224                                                                          
000225     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000226     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
000227     MOVE D-MAANAD TO DAGENS-DATUM-MAANAD                                 
000228     MOVE D-DAG    TO DAGENS-DATUM-DAG                                    
000229                      WS-DAG                                              
000230     MOVE D-VECKA  TO WS-VECKA                                            
000231     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
000232     COMPUTE DAGENS-DATUM-9KOMPL = 999999 - DAGENS-DATUM                  
000233                                                                          
000234     INITIALIZE WS-IDDC-TABELL                                            
000235     MOVE +1 TO WS-IDDC-IX                                                
000236                MAX-IDDC-IX                                               
000237     PERFORM IMS-GN-WDB601                                                
000238     PERFORM UNTIL BASEN-SLUT                                             
000239        MOVE DCS-IDDC         TO TAB-IDDC(WS-IDDC-IX)                     
000240        MOVE DCS-KDDC         TO TAB-KDDC(WS-IDDC-IX)                     
000241        MOVE DCS-IDLEVNR-DC   TO TAB-IDLEVNR(WS-IDDC-IX)                  
000242        ADD +1 TO WS-IDDC-IX                                              
000243                  MAX-IDDC-IX                                             
000244        IF WS-IDDC-IX > 200                                               
000245           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
000246           CALL FELLOG                                                    
000247        END-IF                                                            
000248        PERFORM IMS-GN-WDB601                                             
000249     END-PERFORM                                                          
000250     .                                                                    
000251     EJECT                                                                
000252 B-URVAL SECTION.                                                         
000253                                                                          
000254     IF INL-DAINLEV(3:6) = DAGENS-DATUM-9KOMPL                            
000255*    IF INL-DAINLEV(3:6) > 909790                                         
000256       MOVE +1 TO WS-IDDC-IX                                              
000257       PERFORM UNTIL (INL-IDDC = TAB-IDDC (WS-IDDC-IX))                   
000258                      OR WS-IDDC-IX > MAX-IDDC-IX                         
000259         ADD +1 TO WS-IDDC-IX                                             
000260       END-PERFORM                                                        
000263       MOVE INL-IDDC                TO UT-100-IDDC-REC                    
000264                                       W-IDDC                             
000265       MOVE INL-KVANTMOT            TO UT-100-KVANTAL                     
000266       IF INL-KVANTMOT > ZERO                                             
000267         MOVE 'UNDER DEL'           TO UT-100-AVVIKELSETYP                
000268       ELSE                                                               
000269         MOVE 'OVER DEL'            TO UT-100-AVVIKELSETYP                
000270       END-IF                                                             
000271*      COMPUTE UT-100-KVANTAL = INL-KVAVIS - INL-KVANTMOT                 
000272       PERFORM BA-HAEMTA-SEND-DC                                          
000273       PERFORM BB-HAEMTA-BENAMN                                           
000274       PERFORM S11-SKRIV-UTFIL                                            
000275     END-IF                                                               
000277     .                                                                    
000278     EJECT                                                                
000279 BA-HAEMTA-SEND-DC SECTION.                                               
000280                                                                          
000281     MOVE +1 TO WS-IDDC-IX                                                
000282     PERFORM UNTIL (INL-IDLEVNR = TAB-IDLEVNR (WS-IDDC-IX))               
000283                    OR WS-IDDC-IX > MAX-IDDC-IX                           
000284       ADD +1 TO WS-IDDC-IX                                               
000285     END-PERFORM                                                          
000286     MOVE ZERO                        TO UT-100-ADLAGOMR                  
000287                                         UT-100-ADGANG                    
000288                                         UT-100-ADPLATS                   
000289     MOVE SPACE                       TO UT-100-IDDC-SEND                 
000290     MOVE TAB-IDDC(WS-IDDC-IX) TO UT-100-IDDC-SEND                        
000291                                                                          
000292     PERFORM IMS-GU-WDK711                                                
000293     IF SEGMENT-FINNS                                                     
000294       MOVE SLAG-ADLAGOMR              TO UT-100-ADLAGOMR                 
000295       MOVE SLAG-ADGANG                TO UT-100-ADGANG                   
000296       MOVE SLAG-ADPLATS               TO UT-100-ADPLATS                  
000297     ELSE                                                                 
000298       MOVE ZERO                       TO UT-100-ADLAGOMR                 
000299       MOVE ZERO                       TO UT-100-ADGANG                   
000300       MOVE ZERO                       TO UT-100-ADPLATS                  
000301     END-IF                                                               
000302     .                                                                    
000303     EJECT                                                                
000304 BB-HAEMTA-BENAMN SECTION.                                                
000305                                                                          
000306     PERFORM IMS-GU-BENA01-BSEQ                                           
000307     IF SEGMENT-FINNS                                                     
000308       MOVE 'GB' TO W-IDSKYLT                                             
000309       PERFORM IMS-GNP-BENA11                                             
000310       IF SEGMENT-FINNS                                                   
000311         MOVE TEXT-BEART        TO UT-100-BEART                           
000312       ELSE                                                               
000313         MOVE SPACE             TO UT-100-BEART                           
000314       END-IF                                                             
000315     END-IF                                                               
000316     .                                                                    
000317     EJECT                                                                
000318 C-GET-BIN-DATE SECTION.                                                  
000319* GET THE LATEST BINNING DATE FOR EVERY DC                                
000320     MOVE +1                TO WS-IDDC-IX                                 
000321     PERFORM UNTIL (INL-IDDC = TAB-IDDC (WS-IDDC-IX))                     
000322                    OR WS-IDDC-IX > MAX-IDDC-IX                           
000323       ADD +1               TO WS-IDDC-IX                                 
000324     END-PERFORM                                                          
000325     IF WS-IDDC-IX > MAX-IDDC-IX                                          
000326       CONTINUE                                                           
000327     ELSE                                                                 
000328       MOVE INL-TIINLINL    TO TMP1-YYMMDD                                
000329                                                                          
000330       MOVE TAB-TIINLINL (WS-IDDC-IX)                                     
000331                            TO TMP2-YYMMDD                                
000332       PERFORM WY2000P1                                                   
000333       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
000334         MOVE INL-TIINLINL  TO TAB-TIINLINL (WS-IDDC-IX)                  
000335                                                                          
000336         MOVE INL-IDDC      TO TAB-IDDC-L6 (WS-IDDC-IX)                   
000337       END-IF                                                             
000338     END-IF                                                               
000339     .                                                                    
000340     EJECT                                                                
000341 D-WRITE-DC  SECTION.                                                     
000342* WRITE INTO OUTPUT FROM TEMP TABLE                                       
000343     MOVE +1                             TO WS-IDDC-IX                    
000344     PERFORM UNTIL WS-IDDC-IX > MAX-IDDC-IX                               
000345        IF TAB-IDDC-L6 (WS-IDDC-IX) NOT = SPACES                          
000346           MOVE TAB-IDDC-L6(WS-IDDC-IX)  TO UT2-IDDC                      
000347           MOVE TAB-TIINLINL(WS-IDDC-IX) TO UT2-TIINLINL                  
000348           PERFORM S12-SKRIV-UTFIL2                                       
000349        END-IF                                                            
000350        ADD +1                           TO WS-IDDC-IX                    
000351     END-PERFORM                                                          
000352* INITIALIZE THE TEMP TABLE                                               
000353     MOVE +1                             TO WS-IDDC-IX                    
000354     PERFORM UNTIL WS-IDDC-IX > MAX-IDDC-IX                               
000355           MOVE SPACES                  TO                                
000356                              TAB-IDDC-L6(WS-IDDC-IX)                     
000357           MOVE ZERO                     TO                               
000358                              TAB-TIINLINL(WS-IDDC-IX)                    
000359           ADD +1                        TO WS-IDDC-IX                    
000360     END-PERFORM                                                          
000361     .                                                                    
000362     EJECT                                                                
000363 Z-FINIT SECTION.                                                         
000364                                                                          
000365     CLOSE UTFIL                                                          
000366           UTFIL2                                                         
000367                                                                          
000368     MOVE 'S' TO POSTSUM-OPKOD                                            
000369     CALL POSTSUM USING POSTSUM-PARM                                      
000370     .                                                                    
000371     SKIP3                                                                
000372 S11-SKRIV-UTFIL SECTION.                                                 
000373                                                                          
000374     WRITE UT-POST FROM UT-AREA                                           
000375                                                                          
000376     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
000377     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
000378     MOVE 'W61271D1' TO POSTSUM-DDNAMN2                                   
000379     CALL POSTSUM USING POSTSUM-PARM                                      
000380     .                                                                    
000381     EJECT                                                                
000382 S12-SKRIV-UTFIL2 SECTION.                                                
000383                                                                          
000384     WRITE UT2-POST  FROM UT2-AREA                                        
000385                                                                          
000386     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
000387     MOVE 'UTFIL2'   TO POSTSUM-FDNAMN                                    
000388     MOVE 'W61271D2' TO POSTSUM-DDNAMN2                                   
000389     CALL POSTSUM USING POSTSUM-PARM                                      
000390     .                                                                    
000391     EJECT                                                                
000392* --- IMS SEKTIONER ---                                                   
000393                                                                          
000394 IMS-GET-WDL6 SECTION.                                                    
000395     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
000396     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
000397     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000398     PERFORM IMS-STATUSKONTROLL                                           
000399     .                                                                    
000400     SKIP3                                                                
000401 IMS-GN-WDB601    SECTION.                                                
000402     MOVE 'WDB601  ' TO SSA1                                              
000403     MOVE '  GB'     TO GODK-STATUSKODER                                  
000404     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000405     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000406     PERFORM IMS-STATUSKONTROLL                                           
000407     .                                                                    
000408     SKIP3                                                                
000409 IMS-GU-BENA01-BSEQ SECTION.                                              
000410                                                                          
000411     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
000412          DELIMITED BY SIZE INTO SSA1                                     
000413     MOVE '  GE' TO GODK-STATUSKODER                                      
000414     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA01 SSA1                    
000415     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000416     PERFORM IMS-STATUSKONTROLL                                           
000417     .                                                                    
000418     SKIP3                                                                
000419 IMS-GNP-BENA11 SECTION.                                                  
000420                                                                          
000421     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
000422          DELIMITED BY SIZE INTO SSA1                                     
000423     MOVE '  GE' TO GODK-STATUSKODER                                      
000424     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-BENA11 SSA1                   
000425     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
000426     PERFORM IMS-STATUSKONTROLL                                           
000427     .                                                                    
000428     EJECT                                                                
000429                                                                          
000430 IMS-GU-WDK711 SECTION.                                                   
000431                                                                          
000432     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000433          DELIMITED BY SIZE INTO SSA1                                     
000434     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
000435          DELIMITED BY SIZE INTO SSA2                                     
000436     MOVE '  GE' TO GODK-STATUSKODER                                      
000437     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
000438     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000439     PERFORM IMS-STATUSKONTROLL                                           
000440     .                                                                    
000441     EJECT                                                                
000442 IMS-STATUSKONTROLL SECTION.                                              
000443     SET STATUS-IX TO 1                                                   
000444     SEARCH GODK-STATUS                                                   
000445       AT END                                                             
000446         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000447           DELIMITED BY SIZE INTO FELTEXT                                 
000448         DISPLAY FELTEXT                                                  
000449         CALL FELLOG                                                      
000450       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000451         CONTINUE                                                         
000452     END-SEARCH                                                           
000453     .                                                                    
000454*    -COPY WY2000P1                                                       
