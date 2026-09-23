000101 ID DIVISION.                                                             
000201 PROGRAM-ID.                 W4123600.                                    
000301 AUTHOR.                     GÖRAN KJELLSON.                              
000401     DATE-WRITTEN.           VÅREN 2019                                   
000501*                                                                         
000601     REMARKS.                                                             
000701*                                                                         
000801*    FUNKTION.                                                            
000901*                                                                         
001001*    LÄSER ORDERRADER WDQ4C (FÖR RADER MED PRE-OKS >0)                    
001101*          ORDERRADER WDQ4  (FÖR NOLLNING AV PRE-OKS                      
001102*          OHUV-REG   WDQ2  (FÖR TIREPDAT)                                
001103*          DC-REG     WDB6  (FÖR KVDAGAR-POKS)                            
001104*          ART-REG    WDK7  (FÖR JUSTERING AV KVOKS-BULK)                 
001105*                                                                         
001106*                                                                         
001301                                                                          
001401                                                                          
001501 ENVIRONMENT DIVISION.                                                    
001601 INPUT-OUTPUT SECTION.                                                    
001701*                                                                         
002401                                                                          
002501 DATA DIVISION.                                                           
002701 FILE SECTION.                                                            
003601                                                                          
003701 WORKING-STORAGE SECTION.                                                 
007601                                                                          
007602 77  IDPGM                       PIC X(08)   VALUE 'W4123600'.            
007603 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
007604 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
007605                                                                          
007606 77  W-HELP-DATE                 PIC 9(6)    VALUE ZERO.                  
007607                                                                          
007701 01  RETURKODER.                                                          
007801   03  RKOD                      PIC S9(4) COMP SYNC VALUE ZERO.          
007901   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) COMP SYNC VALUE +16.           
008001   03  RKOD-ABEND-MED-DUMP       PIC S9(4) COMP SYNC VALUE +33.           
008101                                                                          
008201*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
008301                                                                          
008401 01  DYNAMISKA-SUBPROGRAM.                                                
008501     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
008601     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008701     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008801     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008802     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008803                                                                          
008804 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
008805*    -COPY WZ20DAYS                                                       
008806 01  CURRENT-DATE                PIC 9(6).                                
009001                                                                          
009501                                                                          
009601*    ---- PARAMETRAR TILL POSTSUM                                         
009701*01  -COPY W0005      -PRE POSTSUM-.                                      
009801                                                                          
009901                                                                          
010401                                                                          
010501*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
010601                                                                          
010701 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
010801                                                                          
010802 01  NYCKLAR-TILL-DLI.                                                    
010918                                                                          
010919     03  W-WDQ4C1KY-X.                                                    
010920         05  W-IDARTNR-Q4        PIC S9(9)   VALUE ZERO COMP-3.           
010921         05  W-IDDC-Q4           PIC  X(2)   VALUE SPACE.                 
010922         05  W-IDORDER-Q4        PIC S9(7)   VALUE ZERO COMP-3.           
010923         05  W-IDLOPNR-Q4        PIC S9(3)   VALUE ZERO COMP-3.           
010924         05  W-ADLAGOMR-Q4       PIC S9(3)   VALUE ZERO COMP-3.           
010925         05  W-ADGANG-Q4         PIC S9(3)   VALUE ZERO COMP-3.           
010926         05  W-ADPLATS-Q4        PIC S9(5)   VALUE ZERO COMP-3.           
010928     03  W-WDQ401KY-X.                                                    
010929         05 W-WDQ401KY           PIC X(20)   VALUE SPACE.                 
010930                                                                          
010931     03  W-IDDC-B6-X.                                                     
010932         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
010933                                                                          
010934     03  W-IDORDER-X.                                                     
010935         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
010936                                                                          
010937     03  W-IDARTNR-X.                                                     
010938         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010939                                                                          
010940     03  W-IDDC-X.                                                        
010941         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
010942                                                                          
010943     03  W-XRST-CHK.                                                      
010944         07 W-XRST-LENGTH        PIC S9(9) COMP SYNC VALUE +32.           
010945         07 W-XRST-AREA          PIC X(32) VALUE SPACES.                  
010946         07 W-CHKP-LENGTH        PIC S9(9) COMP SYNC VALUE +32.           
010947         07 W-CHKP-AREA          PIC X(32) VALUE SPACES.                  
010948                                                                          
010949         07 W-CHKP-CNTR          PIC 9(3)  VALUE ZERO.                    
010950         07 W-CHKP-MAX           PIC 9(3)  VALUE 200.                     
010951     EJECT                                                                
010952                                                                          
010960*    ---- STATUSKOD FRÅN IMS                                              
011001                                                                          
011101 01  STATUS-WS                   PIC XX.                                  
011201     88  SEGMENT-FINNS                      VALUE '  '.                   
011301     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
011401     88  SEGMENT-SLUT                       VALUE 'GB'.                   
011501                                                                          
011601 01  GODK-STATUSKODER.                                                    
011701   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
011801                                                                          
011901 01  ALL-SSA.                                                             
011902     03 SSA1                     PIC X(64).                               
011903     03 SSA2                     PIC X(64).                               
012001                                                                          
012101                                                                          
012201*01      -COPY W0003.                                                     
012301                                                                          
012901                                                                          
013001 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDQ4C1'.           
013101 01  DLI-IO-WDQ4C1.                                                       
013201*    03  -COPY WDQ4C1                                                     
013202                                                                          
013203 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDQ401'.           
013204 01  DLI-IO-WDQ401.                                                       
013205*    03  -COPY WDQ401                                                     
013206                                                                          
013207 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDQ201'.           
013208 01  DLI-IO-WDQ201.                                                       
013209*    03  -COPY WDQ201                                                     
013210                                                                          
013211 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDB601'.           
013212 01  DLI-IO-WDB601.                                                       
013213*    03  -COPY WDB601                                                     
013214                                                                          
013215 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
013216 01  DLI-IO-WDK711.                                                       
013220*    03  -COPY WDK711                                                     
014101                                                                          
014201                                                                          
014301 LINKAGE SECTION.                                                         
014302                                                                          
014303*01  -COPY W0009  -PRE MSG-                                               
014401                                                                          
014501*01  -COPY W0008  -PRE WDQ4C-                                             
014601     05  FILLER                  PIC X.                                   
015101                                                                          
015102*01  -COPY W0008  -PRE WDQ4-                                              
015103     05  FILLER                  PIC X.                                   
015104                                                                          
015105*01  -COPY W0008  -PRE WDQ2-                                              
015106     05  FILLER                  PIC X.                                   
015107                                                                          
015108*01  -COPY W0008  -PRE WDB6-                                              
015109     05  FILLER                  PIC X.                                   
015110                                                                          
015111*01  -COPY W0008  -PRE WDK7-                                              
015112     05  FILLER                  PIC X.                                   
015120                                                                          
015201                                                                          
016001 PROCEDURE DIVISION  USING MSG-PCB  WDQ4C-PCB WDQ4-PCB                    
016002                           WDQ2-PCB WDB6-PCB WDK7-PCB.                    
016101     ENTRY 'DLITCBL' USING MSG-PCB  WDQ4C-PCB WDQ4-PCB                    
016102                           WDQ2-PCB WDB6-PCB WDK7-PCB.                    
016201                                                                          
016301     PERFORM A-INIT                                                       
016401     PERFORM IMS-GN-WDQ4C                                                 
016501     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
016502       IF SEQC-KVOKS-PREL > ZERO                                          
016503          PERFORM B-KOLLA-UPD-KVOKS-PREL                                  
016504          IF W-CHKP-CNTR > W-CHKP-MAX                                     
016505             PERFORM IMS-CHECKPOINT                                       
016506                                                                          
016507*RESET POINTER AFTER CHECKPOINT BEFORE NEXT GN                            
016508             MOVE SEQC-IDARTNR  TO W-IDARTNR-Q4                           
016509             MOVE SEQC-IDDC     TO W-IDDC-Q4                              
016510             MOVE SEQC-IDORDER  TO W-IDORDER-Q4                           
016511             MOVE SEQC-IDLOPNR  TO W-IDLOPNR-Q4                           
016512             MOVE SEQC-ADLAGOMR TO W-ADLAGOMR-Q4                          
016513             MOVE SEQC-ADGANG   TO W-ADGANG-Q4                            
016514             MOVE SEQC-ADPLATS  TO W-ADPLATS-Q4                           
016515             PERFORM IMS-GU-WDQ4C                                         
016516          END-IF                                                          
016520       END-IF                                                             
016701       PERFORM IMS-GN-WDQ4C                                               
017301     END-PERFORM                                                          
017401                                                                          
017601     MOVE ZERO TO RETURN-CODE                                             
017701     GOBACK                                                               
017801     .                                                                    
017901                                                                          
018001                                                                          
018101 A-INIT SECTION.                                                          
018701                                                                          
018702     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
018703     PERFORM IMS-RESTART                                                  
018704     ACCEPT CURRENT-DATE FROM DATE                                        
018901     .                                                                    
019001                                                                          
024400                                                                          
024500 B-KOLLA-UPD-KVOKS-PREL SECTION.                                          
024600                                                                          
024700     MOVE 'B-KOLLA-UPD     ' TO CURRENT-SECTION                           
024800                                                                          
024801     IF W-IDDC-B6 NOT = SEQC-IDDC                                         
024810        MOVE SEQC-IDDC       TO W-IDDC-B6                                 
024820        PERFORM IMS-GU-WDB601                                             
024821     END-IF                                                               
024822     IF W-IDORDER NOT = SEQC-IDORDER                                      
024823        MOVE SEQC-IDORDER    TO W-IDORDER                                 
024824        PERFORM IMS-GU-WDQ201                                             
024825     END-IF                                                               
024826     MOVE 'YYMMDD'               TO DAYS-KDDATFMT1                        
024827     MOVE CURRENT-DATE           TO DAYS-TIDATE1                          
024828     MOVE 'YYMMDD'               TO DAYS-KDDATFMT2                        
024830     MOVE OHUV-TIREPDAT          TO W-HELP-DATE                           
024831     MOVE W-HELP-DATE            TO DAYS-TIDATE2                          
024832     MOVE ZERO                   TO DAYS-KVDAYS                           
024833     MOVE SPACE                  TO DAYS-IDCALEND                         
024834                                                                          
024835     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
024836                                                                          
024840     IF DAYS-KDRC = ZERO                                                  
024842        IF DAYS-KVDAYS < DCS-KVDAGAR-POKS                                 
024843           MOVE SEQC-IDARTNR  TO W-IDARTNR                                
024844           MOVE SEQC-IDDC     TO W-IDDC                                   
024845           PERFORM IMS-GHU-WDK711                                         
024848           COMPUTE SLAG-KVOKS-BULK = SLAG-KVOKS-BULK +                    
024849                                      SEQC-KVOKS-PREL                     
024850           PERFORM IMS-REPL-WDK711                                        
024851           MOVE SEQC-IDWDQ401 TO W-WDQ401KY                               
024852           PERFORM IMS-GHU-WDQ401                                         
024853           MOVE ZERO          TO ORAD-KVOKS-PREL                          
024854           PERFORM IMS-REPL-WDQ401                                        
024855           ADD 1 TO W-CHKP-CNTR                                           
024856        END-IF                                                            
024857     ELSE                                                                 
024858        CALL FELLOG                                                       
024859     END-IF                                                               
024860                                                                          
024900     .                                                                    
025000                                                                          
025100                                                                          
027700*    ---- IMS SEKTIONER                                                   
027801 IMS-GU-WDQ4C SECTION.                                                    
027802     MOVE 'IMS-GU-WDQ4C    ' TO CURRENT-IMS-SECTION                       
027900                                                                          
028004     MOVE SPACE            TO ALL-SSA                                     
028005     STRING 'WDQ4C1  (WDQ4C1KY>=' W-WDQ4C1KY-X ')'                        
028006          DELIMITED BY SIZE INTO SSA1                                     
028007     MOVE '    '           TO GODK-STATUSKODER                            
028101     CALL CBLTDLI USING GU WDQ4C-PCB DLI-IO-WDQ4C1 SSA1                   
028201     MOVE WDQ4C-STATUS-CODE TO STATUS-WS                                  
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028410                                                                          
028420                                                                          
028421 IMS-GN-WDQ4C SECTION.                                                    
028422     MOVE 'IMS-GN-WDQ4C    ' TO CURRENT-IMS-SECTION                       
028423                                                                          
028424     MOVE SPACE            TO ALL-SSA                                     
028425     MOVE 'WDQ4C1 '        TO SSA1                                        
028426     MOVE '  GEGB'         TO GODK-STATUSKODER                            
028427     CALL CBLTDLI USING GN WDQ4C-PCB DLI-IO-WDQ4C1 SSA1                   
028428     MOVE WDQ4C-STATUS-CODE TO STATUS-WS                                  
028429     PERFORM IMS-STATUSKONTROLL                                           
028430     .                                                                    
028431                                                                          
028432                                                                          
028433 IMS-GHU-WDQ401 SECTION.                                                  
028434     MOVE 'IMS-GHU-WDQ401  ' TO CURRENT-IMS-SECTION                       
028435                                                                          
028436     MOVE SPACE            TO ALL-SSA                                     
028437     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY ')'                          
028438          DELIMITED BY SIZE INTO SSA1                                     
028439     MOVE '    '           TO GODK-STATUSKODER                            
028440     CALL CBLTDLI USING GHU WDQ4-PCB DLI-IO-WDQ401 SSA1                   
028441     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
028442     PERFORM IMS-STATUSKONTROLL                                           
028443     .                                                                    
028444                                                                          
028445                                                                          
028446 IMS-REPL-WDQ401 SECTION.                                                 
028447     MOVE 'IMS-REPL-WDQ401 ' TO CURRENT-IMS-SECTION                       
028450                                                                          
028460     MOVE SPACE              TO ALL-SSA                                   
028470     MOVE '    '               TO GODK-STATUSKODER                        
028480     CALL CBLTDLI USING REPL WDQ4-PCB DLI-IO-WDQ401                       
028490     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
028491     PERFORM IMS-STATUSKONTROLL                                           
028492     .                                                                    
028493                                                                          
028494                                                                          
028500 IMS-GU-WDQ201 SECTION.                                                   
028501     MOVE 'IMS-GU-WDQ201   ' TO CURRENT-IMS-SECTION                       
028502                                                                          
028503     MOVE SPACE            TO ALL-SSA                                     
028505     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
028506          DELIMITED BY SIZE INTO SSA1                                     
028507     MOVE '    ' TO GODK-STATUSKODER                                      
028508     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
028509     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
028510     PERFORM IMS-STATUSKONTROLL                                           
028511     .                                                                    
028512                                                                          
028513                                                                          
028514 IMS-GU-WDB601    SECTION.                                                
028515     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
028516                                                                          
028517     MOVE SPACE              TO ALL-SSA                                   
028518     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
028519          DELIMITED BY SIZE INTO SSA1                                     
028520     MOVE '    ' TO GODK-STATUSKODER                                      
028521     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
028522     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
028523     PERFORM IMS-STATUSKONTROLL                                           
028527     .                                                                    
028528                                                                          
028529                                                                          
028530 IMS-GHU-WDK711 SECTION.                                                  
028531     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
028532                                                                          
028533     MOVE SPACE              TO ALL-SSA                                   
028534     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
028535          DELIMITED BY SIZE  INTO SSA1                                    
028536     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
028537          DELIMITED BY SIZE  INTO SSA2                                    
028538     MOVE '    '               TO GODK-STATUSKODER                        
028539     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
028540     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
028541     PERFORM IMS-STATUSKONTROLL                                           
028542     .                                                                    
028543                                                                          
028544                                                                          
028545 IMS-REPL-WDK711 SECTION.                                                 
028546     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
028547                                                                          
028548     MOVE SPACE              TO ALL-SSA                                   
028549     MOVE '    '               TO GODK-STATUSKODER                        
028550     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
028551     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
028552     PERFORM IMS-STATUSKONTROLL                                           
028553     .                                                                    
028554                                                                          
028555                                                                          
028556 IMS-RESTART SECTION.                                                     
028557                                                                          
028558     MOVE SPACES TO W-XRST-AREA                                           
028559     MOVE '  '   TO GODK-STATUSKODER                                      
028560     CALL CBLTDLI USING XRST MSG-PCB                                      
028561                        W-XRST-LENGTH W-XRST-AREA                         
028562                        W-CHKP-LENGTH W-CHKP-AREA                         
028563     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028564     PERFORM IMS-STATUSKONTROLL                                           
028565     .                                                                    
028566                                                                          
028567 IMS-CHECKPOINT SECTION.                                                  
028568                                                                          
028569      MOVE ZERO TO W-CHKP-CNTR                                            
028570      MOVE IDPGM TO W-XRST-AREA                                           
028571      MOVE '  ' TO GODK-STATUSKODER                                       
028572      CALL CBLTDLI USING CHKP MSG-PCB                                     
028573                         W-XRST-LENGTH W-XRST-AREA                        
028574                         W-CHKP-LENGTH W-CHKP-AREA                        
028575      MOVE MSG-STATUS-CODE TO STATUS-WS                                   
028576      PERFORM IMS-STATUSKONTROLL                                          
028580     .                                                                    
029500                                                                          
029601 IMS-STATUSKONTROLL SECTION.                                              
029701                                                                          
029801     SET STATUS-IX TO 1                                                   
029901     SEARCH GODK-STATUS                                                   
030001       AT END CALL FELLOG                                                 
030101       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
030201     END-SEARCH                                                           
030301     .                                                                    
030401     EJECT                                                                
