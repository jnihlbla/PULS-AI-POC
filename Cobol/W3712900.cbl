000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3712900.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   DEC-1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM LÄSER AKTUELLA POÄNG-POSTER, KOMPLETTERAR OCH I VISSA        
001100*        FALL DUBBLERAR DESSA OCH SKRIVER;                                
001200*        . KOMPLETTERADE/DUBBLERADE POÄNG-POSTER TILL                     
001300*          RAPPORTER/HISTORIK                                             
001400*                                                                         
002200*       -PGM LÄSER WDGX3160 (WDR1)                                        
002700*                                                                         
003700*    ABENDKODER:                                                          
003800*        U0016 -  . . . .                                                 
003900*        U1000 -  . . . .                                                 
004000*                                                                         
004100                                                                          
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 INPUT-OUTPUT SECTION.                                                    
004600                                                                          
004700 FILE-CONTROL.                                                            
004900*          --- RETURER TILL RAPPORTER/HISTORIK                            
005000     SELECT W37107                     ASSIGN TO W37129D1.                
005100                                                                          
005500*          --- KOMPL. RETURER TILL RAPPORTER/HISTORIK                     
005600     SELECT W37102                     ASSIGN TO W37129D2.                
006600     EJECT                                                                
006610                                                                          
006700 DATA DIVISION.                                                           
006800                                                                          
006900 FILE SECTION.                                                            
007100 FD  W37107                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500*01  POINT-REC     -COPY W37109      -L.                                  
007800     EJECT                                                                
007900                                                                          
008810 FD  W37102                                                               
008820     RECORDING       F                                                    
008830     BLOCK CONTAINS  0.                                                   
008840                                                                          
008850*01  KOMPPOINT-REC -COPY W37109      -L.                                  
011480     EJECT                                                                
011490                                                                          
011500 WORKING-STORAGE SECTION.                                                 
011700*    -- CHECKED BY WY2000                                                 
011800 77  IDPGM                        PIC X(8)    VALUE 'W3712900'.           
011900 77  JA                           PIC X       VALUE 'J'.                  
012000 77  NEJ                          PIC X       VALUE 'N'.                  
012100 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012300 77  W37107-EOF-SW                PIC X       VALUE 'N'.                  
012400     88  END-OF-W37107                        VALUE 'J'.                  
012401 77  SW-FIRST-TIME                PIC X       VALUE 'J'.                  
012402     88  FIRST-TIME                           VALUE 'J'.                  
012403 77  SW-RETURN-IN-TRANSIT         PIC X       VALUE 'N'.                  
012404     88  RETURN-IN-TRANSIT                    VALUE 'J'.                  
012410 77  WS-SPAR-IDBYTRAP-UPD         PIC S9(7)   VALUE +0 COMP-3.            
012500 77  WS-SPAR-IDDISTR-UPD          PIC S9(5)   VALUE +0 COMP-3.            
012600 77  WS-SPAR-KDEXCHA-UPD          PIC S9(3)   VALUE +0 COMP-3.            
012600 77  WS-KDBYTREF-NUM              PIC S9(3)   VALUE +0 COMP-3.            
012610                                                                          
012620 77  WS-SPAR-IDDISTR-TRANSIT      PIC S9(5)   VALUE +0 COMP-3.            
012630 77  WS-SPAR-KDEXCHA-TRANSIT      PIC S9(3)   VALUE +0 COMP-3.            
012640                                                                          
012700 77  WS-SUPOINT                   PIC S9(7)   VALUE +0 COMP-3.            
013110                                                                          
013200 01  FELTEXT                      PIC X(80).                              
013201     EJECT                                                                
013210                                                                          
019000 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR         PIC 9(2).                               
019300     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
019400     03  DAGENS-DATUM-DAG         PIC 9(2).                               
019500                                                                          
019600 01  WS-DAREGDAT.                                                         
019700     03  WS-DAREGDAT-SEKEL       PIC 9(2).                                
019800     03  WS-DAREGDAT-AAMMDD      PIC 9(6).                                
019900                                                                          
020150 01  WS-TIAAVV-NUM               PIC 9(4).                                
020160 01  WS-JFR-EXTENDED             PIC S9(5)   COMP-3.                      
020170 01  WS-JFR-DAGENS               PIC S9(5)   COMP-3.                      
020200     EJECT                                                                
020210                                                                          
020300 01  DYNAMISKA-SUBPROGRAM.                                                
020400*                                                                         
020500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
021300     EJECT                                                                
021310                                                                          
021400*    --- PARAMETRAR TILL ABEND                                            
021500                                                                          
021600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021900     EJECT                                                                
021910                                                                          
021920 01  W009VADD-DATUM              PIC S9(5)   VALUE ZERO COMP-3.           
021930 01  W009VADD-ANTAL              PIC S9(3)   VALUE ZERO COMP-3.           
021940     EJECT                                                                
021950                                                                          
022110*    --- PARAMETRAR TILL DATKORT                                          
022120*                                                                         
022200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37129'.              
022300                                                                          
022400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022500                                                                          
022600*01  -COPY WDATKORT                                                       
022700     EJECT                                                                
022710                                                                          
022800*    --- PARAMETRAR TILL POSTSUM                                          
022900*                                                                         
023000*01  -COPY W0005   -PRE  POSTSUM-                                         
023100     EJECT                                                                
023300                                                                          
023310*    --- EXCHANGE RECEIVING CODES IN W3172 SCREEN                         
023320*                                                                         
023330*01  -COPY WWBYT15                                                        
023340     EJECT                                                                
023350                                                                          
023400*-----------------------------------------PARAMETRAR TILL                 
023500*                                         SUBPROGRAM WDATKONV             
023600 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
023700*01  -COPY WDATAREA.                                                      
023800     EJECT                                                                
023900                                                                          
024500 01  IN-AREA-START               PIC X(24)   VALUE                        
024600                                 'IN-AREA-START  '.                       
024700*01  AREA -COPY W37109     -PRE POINT-                                    
025100     EJECT                                                                
025110                                                                          
025200 01  KOMPPOINT-AREA-START        PIC X(24)   VALUE                        
025300                                 'UT-AREA-START  '.                       
025310                                                                          
025400*01  AREA -COPY W37109     -PRE KOMPPOINT-                                
025410     EJECT                                                                
025500                                                                          
026300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026400*                                                                         
026710 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026720                                                                          
026800 01  NYCKLAR-TILL-DLI.                                                    
026810     03  W-3159-IDHTYP-X.                                                 
026820         05  W-3159-IDHTYP       PIC  X(4)   VALUE '3159'.                
026830         05  W-3159-LOW-VALUE    PIC  X(26)  VALUE LOW-VALUE.             
026840                                                                          
026850     03  W-3160-KDBYTREF-X.                                               
026862         05  W-3160-KDBYTREF     PIC  X(3)   VALUE SPACE.                 
026870     EJECT                                                                
030500                                                                          
030600*    --- STATUS-KOD FRÅN IMS                                              
030700 01  STATUS-WS                   PIC XX.                                  
030800     88  SEGMENT-FINNS                       VALUE '  '.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000                                                                          
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300                                                                          
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031800     EJECT                                                                
031810                                                                          
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032110                                                                          
032200*    ---  DLI INPUT-OUTPUT AREA                                           
035010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3160'.                    
035020 01  DLI-IO-WDGX3160.                                                     
035030*    03  -COPY WDGX3160                                                   
035040     EJECT                                                                
035050                                                                          
035500 LINKAGE SECTION.                                                         
035700*01  -COPY W0008  -PRE 3160-                                              
035800     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037110                                                                          
037200 PROCEDURE DIVISION  USING 3160-PCB.                                      
037400 MAIN SECTION.                                                            
037500     ENTRY 'DLITCBL' USING 3160-PCB.                                      
037700                                                                          
037800     PERFORM A-INIT                                                       
037900                                                                          
038200     PERFORM S01-LAES-W37107                                              
038201                                                                          
038300     PERFORM UNTIL END-OF-W37107                                          
038500       PERFORM B-BEARBETA                                                 
038700       PERFORM S01-LAES-W37107                                            
038800     END-PERFORM                                                          
038900                                                                          
039000     PERFORM Z-FINIT                                                      
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039700                                                                          
039710 A-INIT SECTION.                                                          
039800     OPEN INPUT  W37107                                                   
039900                                                                          
040000     OPEN OUTPUT W37102                                                   
040500                                                                          
040600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
040700     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
040800     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
040900     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
041000     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
042100     .                                                                    
042200     EJECT                                                                
042400                                                                          
042410 B-BEARBETA SECTION.                                                      
042504     MOVE POINT-AREA          TO KOMPPOINT-AREA                           
042505                                                                          
042507     MOVE POINT-KDBYTREF      TO BYT15-KDBYTREF                           
042508***EXCLUDE WARRANTY CODES FROM CALCULATION**                              
042510     IF POINT-KDBYTREF NOT = SPACE           AND                          
042511            (NOT  BYT15-KDBYTREF-GAR-SALD)   AND                          
042512            (NOT  BYT15-KDBYTREF-GAR-EJ-SALD)                             
042513                                                                          
042514       MOVE POINT-KDBYTREF    TO W-3160-KDBYTREF                          
042514                                 WS-KDBYTREF-NUM                          
042515       PERFORM IMS-GU-WDGX3160                                            
042516                                                                          
042520       IF 3160-KVPOINT = ZERO                                             
               IF  WS-KDBYTREF-NUM < 100                                        
042600           MOVE 3160-TENOTE     TO KOMPPOINT-TENOTE                       
042611           MOVE ZERO TO KOMPPOINT-SUPOINT                                 
042700           PERFORM S02-SKRIV-W37102                                       
               ELSE                                                             
042600           MOVE 3160-TENOTE     TO KOMPPOINT-TENOTE                       
042700           PERFORM S02-SKRIV-W37102                                       
042710         END-IF                                                           
042800       ELSE                                                               
042802         MOVE '   '           TO KOMPPOINT-KDBYTREF                       
042810         PERFORM S02-SKRIV-W37102                                         
042811                                                                          
042812         MOVE POINT-KDBYTREF  TO KOMPPOINT-KDBYTREF                       
042820         MOVE 'X'             TO KOMPPOINT-KDBYTSTA-OBJ                   
042821         MOVE 3160-TENOTE     TO KOMPPOINT-TENOTE                         
042822         COMPUTE KOMPPOINT-KVPOINT = 3160-KVPOINT * -1                    
042823         END-COMPUTE                                                      
042824                                                                          
042825         COMPUTE KOMPPOINT-SUPOINT = KOMPPOINT-KVPOINT                    
042826                                   * KOMPPOINT-KVRETUR-GODK               
042828         END-COMPUTE                                                      
042830         MOVE ZERO            TO KOMPPOINT-KVRETUR-GODK                   
042831                                                                          
042840         PERFORM S02-SKRIV-W37102                                         
042900       END-IF                                                             
043000     ELSE                                                                 
043010                                                                          
043100       PERFORM S02-SKRIV-W37102                                           
043200     END-IF                                                               
445000     .                                                                    
445100     EJECT                                                                
445110                                                                          
445377 Z-FINIT SECTION.                                                         
445380     CLOSE W37107                                                         
445400           W37102                                                         
445900                                                                          
446000     MOVE 'S' TO POSTSUM-OPKOD                                            
446100     CALL POSTSUM USING POSTSUM-PARM                                      
446200     .                                                                    
446300     EJECT                                                                
446310                                                                          
446400 S01-LAES-W37107  SECTION.                                                
446600     READ W37107 INTO POINT-AREA                                          
446700     AT END                                                               
446800        MOVE HIGH-VALUE         TO POINT-AREA                             
446900        SET END-OF-W37107       TO TRUE                                   
447000                                                                          
447100     NOT AT END                                                           
447200        MOVE 'W37107'           TO POSTSUM-FDNAMN                         
447300        MOVE 'W37129D1'         TO POSTSUM-DDNAMN2                        
447400        MOVE SPACE              TO POSTSUM-TRANSTYP                       
447500        CALL POSTSUM USING POSTSUM-PARM                                   
447600     END-READ                                                             
447700     .                                                                    
447800                                                                          
450010 S02-SKRIV-W37102 SECTION.                                                
450030     MOVE SPACE                 TO   KOMPPOINT-REC                        
450040     WRITE KOMPPOINT-REC        FROM KOMPPOINT-AREA                       
450050                                                                          
450060     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
450070     MOVE 'W37102'              TO   POSTSUM-FDNAMN                       
450080     MOVE 'W37129D2'            TO   POSTSUM-DDNAMN2                      
450090     CALL POSTSUM USING POSTSUM-PARM                                      
450091     .                                                                    
466890     EJECT                                                                
466891                                                                          
466900* --- IMS SEKTIONER ---                                                   
469520                                                                          
469521 IMS-GU-WDGX3160 SECTION.                                                 
469530     STRING 'WDR101  (WDGXKEY  =' W-3159-IDHTYP-X ')'                     
469540          DELIMITED BY SIZE INTO SSA1                                     
469550     STRING 'WDGX3160(KDBYTREF =' W-3160-KDBYTREF-X ')'                   
469560          DELIMITED BY SIZE INTO SSA2                                     
469570     MOVE '  '             TO GODK-STATUSKODER                            
469580     CALL CBLTDLI USING GU   3160-PCB DLI-IO-WDGX3160 SSA1 SSA2           
469590     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
469591     PERFORM IMS-STATUSKONTROLL                                           
469592     .                                                                    
473900     EJECT                                                                
474100                                                                          
474110 IMS-STATUSKONTROLL SECTION.                                              
474200     SET STATUS-IX TO 1                                                   
474300     SEARCH GODK-STATUS                                                   
474400       AT END                                                             
474500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
474600           DELIMITED BY SIZE INTO FELTEXT                                 
474700         DISPLAY FELTEXT                                                  
474800         CALL FELLOG                                                      
474900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
475000         CONTINUE                                                         
475100     END-SEARCH                                                           
475200     .                                                                    
