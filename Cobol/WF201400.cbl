000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000500*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF201400.                                                
001300 AUTHOR.         ANDERS HENRIKSSON.                                       
001400 DATE-WRITTEN.   02-04-16.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   CREATES FILE (GENERAL LEDGER DATA) FROM DOCUMENT-TYPE AND             
002000*   BUSINESS RELATION TABLES.                                             
002410*                                                                         
002430*   PGM READS                                                             
002440*   - ROWS IN TABLE T01PROC                                               
002441*   - ROWS IN TABLE T01DHEA                                               
002442*   - ROWS IN TABLE T01DLIN                                               
002450*   - ROWS IN TABLE T01DOTY                                               
002460*   - ROWS IN TABLE T01BURE                                               
002470*   - ROWS IN TABLE T01FCUS                                               
002480*   - ROWS IN TABLE T01CURR                                               
002490*   - ROWS IN TABLE T01LSEL                                               
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003012*    ---- UTFIL: GENERAL LEDGER DATA                                      
003013     SELECT  WF2014        ASSIGN TO WF2014D1.                            
003014                                                                          
003015     SELECT  WF2024        ASSIGN TO WF2014D2.                            
003016     EJECT                                                                
003017                                                                          
003018 DATA DIVISION.                                                           
003019 FILE SECTION.                                                            
003020 FD  WF2014                                                               
003021     RECORDING  F                                                         
003022     BLOCK CONTAINS 0.                                                    
003030                                                                          
003040*01  POST-WF2014 -COPY WF2014   -L.                                       
003041                                                                          
003042 FD  WF2024                                                               
003043     RECORDING  F                                                         
003044     BLOCK CONTAINS 0.                                                    
003045                                                                          
003046*01  POST-WF2024 -COPY WF2014   -L.                                       
003050     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004110                                                                          
004200 77  IDPGM                        PIC X(8)   VALUE 'WF201400'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004430     EJECT                                                                
004440                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006320*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
006330 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006340 77  KDRC-DISPLAY                PIC Z(5).                                
006341 77  WS-DAGENS-DATUM             PIC X(8)    VALUE '00000000'.            
006342 77  WS-DAGENS-DATUM-NUM         PIC 9(8).                                
006343 77  WS-IDSYSTEM                 PIC X(4)    VALUE 'WF02'.                
006344 77  WS-IDLEGSEL-CRS             PIC X(4).                                
006350                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006502                                                                          
006503 01  UTFIL-AREA-START            PIC X(24)   VALUE                        
006504                                             'UTFIL-AREA-START'.          
006505     SKIP2                                                                
006507*01  -COPY WF2014 -PRE WS-                                                
006509     EJECT                                                                
006550                                                                          
006553 01  WS-DASTADAT-KEY            PIC X(8)     VALUE SPACE.                 
006560 01  WS-AREA.                                                             
006588     03 WS-DAFINDOC-2           PIC 9(8)     VALUE ZERO.                  
006590     03 WS-DAREFDAT-2           PIC 9(8)     VALUE ZERO.                  
006600     03 WS-IDLEGSEL-2           PIC X(4)     VALUE SPACE.                 
006700     03 WS-IDPARTNR-2           PIC X(9)     VALUE SPACE.                 
006710     03 WS-KDFINDOC-2           PIC X(4)     VALUE SPACE.                 
006720     03 WS-KDVALISO-2           PIC X(3)     VALUE SPACE.                 
006730     03 WS-DHEA-KDVALISO        PIC X(3)     VALUE SPACE.                 
006770                                                                          
006790     03 BURE-FLGL               PIC X(1)     VALUE SPACE.                 
006793                                                                          
006794 01  WS-DIVERSE-MULTIFETCH.                                               
006795     03 WS-MX                    PIC S9(3)  COMP-3.                       
006796     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
006797                                                                          
006798 01  WS-DIV-TABELLER.                                                     
006799     03 WS-IDLEGSEL   OCCURS 100 PIC X(4).                                
006800     03 WS-IDPARTNR   OCCURS 100 PIC X(9).                                
006801     03 WS-KDFINDOC   OCCURS 100 PIC X(4).                                
006802     03 WS-DAREFDAT   OCCURS 100 PIC X(8).                                
006803     03 WS-DAFINDOC   OCCURS 100 PIC X(8).                                
006804     03 WS-IDEXCUST-1 OCCURS 100 PIC X(15).                               
006805     03 WS-IDEXCUST-2 OCCURS 100 PIC X(15).                               
006806     03 WS-IDEXCUST-3 OCCURS 100 PIC X(15).                               
006807     03 WS-IDOPTION-1 OCCURS 100 PIC X(15).                               
006808     03 WS-IDOPTION-2 OCCURS 100 PIC X(15).                               
006809     03 WS-IDOPTION-3 OCCURS 100 PIC X(15).                               
006810     03 WS-IDOPTION-4 OCCURS 100 PIC X(15).                               
006811     03 WS-IDOPTION-5 OCCURS 100 PIC X(15).                               
006812     03 WS-IDACCNT-1  OCCURS 100 PIC X(15).                               
006813     03 WS-IDACCNT-2  OCCURS 100 PIC X(15).                               
006814     03 WS-IDACCNT-3  OCCURS 100 PIC X(15).                               
006815     03 WS-IDACCNT-4  OCCURS 100 PIC X(15).                               
006816                                                                          
006817     03 DHEA-KDVALISO OCCURS 100 PIC X(3).                                
006820                                                                          
006900                                                                          
010602*******  WORK-AREAS FOR DB2-SECTIONS                                      
010603*    --- WORK-AREAS FOR OUTPUT-FILE                                       
010604 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
010605                                 'OUTPUT-TAB   '.                         
010606*01  -COPY WF2014T                                                        
010607     EJECT                                                                
010610                                                                          
010611 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA  '.         
010612*01  -COPY T01PROC    -PRE T01PROC-                                       
010613                                                                          
010614 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA  '.         
010615*01  -COPY T01DHEA    -PRE T01DHEA-                                       
010616                                                                          
010617 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA  '.         
010618*01  -COPY T01DLIN    -PRE T01DLIN-                                       
010619                                                                          
010620 01  FILLER                       PIC X(16)  VALUE 'DOTY-AREA  '.         
010621*01  -COPY T01DOTY    -PRE T01DOTY-                                       
010622                                                                          
010623 01  FILLER                       PIC X(16)  VALUE 'BURE-AREA  '.         
010624*01  -COPY T01BURE    -PRE T01BURE-                                       
010625                                                                          
010626 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA  '.         
010627*01  -COPY T01FCUS    -PRE T01FCUS-                                       
010628                                                                          
010629 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA  '.         
010630*01  -COPY T01CURR    -PRE T01CURR-                                       
010631     EJECT                                                                
010633                                                                          
010634 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA  '.         
010635*01  -COPY T01LSEL    -PRE T01LSEL-                                       
010636     EJECT                                                                
010637                                                                          
010638     EXEC SQL INCLUDE T01PROC  END-EXEC.                                  
010639     EJECT                                                                
010640     EXEC SQL INCLUDE T01DHEA  END-EXEC.                                  
010641     EJECT                                                                
010642     EXEC SQL INCLUDE T01DLIN  END-EXEC.                                  
010643     EJECT                                                                
010644     EXEC SQL INCLUDE T01DOTY  END-EXEC.                                  
010645     EJECT                                                                
010646     EXEC SQL INCLUDE T01BURE  END-EXEC.                                  
010647     EJECT                                                                
010648     EXEC SQL INCLUDE T01FCUS  END-EXEC.                                  
010649     EJECT                                                                
010650     EXEC SQL INCLUDE T01CURR  END-EXEC.                                  
010651     EJECT                                                                
010652     EXEC SQL INCLUDE T01LSEL  END-EXEC.                                  
010653     EJECT                                                                
010654                                                                          
010655 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
010656       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010657                                                                          
010658***** STATUS-CODE FROM DB2                                                
010659 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
010660                                                                          
010661 01  DB2-WS.                                                              
010662   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
010663     88  CURSOR-OK                           VALUE +000.                  
010664     88  LINES-FOUND                         VALUE +000.                  
010665     88  LINES-MISSING                       VALUE +100.                  
010666     88  RESOURCE-WRONG                      VALUE 904.                   
010667                                                                          
010668   03  GOOD-SQLCODES.                                                     
010669     05  GOOD-SQLCODE OCCURS 5                                            
010670         INDEXED BY SQLCODE-IX    PIC 999.                                
010671     EJECT                                                                
010680                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
012500                                                                          
012510     PERFORM Z-FINISH                                                     
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012910                                                                          
013000 A-INIT SECTION.                                                          
013100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
013101     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
013107                                                                          
013110     OPEN OUTPUT WF2014                                                   
013120                 WF2024                                                   
014100     .                                                                    
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014987     PERFORM DB2-OPEN-CRS-LSEL                                            
014988     PERFORM DB2-FETCH-CRS-LSEL                                           
014994     PERFORM UNTIL LINES-MISSING                                          
014995       PERFORM DB2-SELECT-T01PROC-TAB                                     
014996       IF T01PROC-KDBEH = 'P'                                             
014997         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
014998         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
014999       END-IF                                                             
015000       PERFORM DB2-DCL-OPN-CRS1                                           
015001       PERFORM DB2-FETCH-CRS1                                             
015002       IF SQLERRD(3) > 0                                                  
015003         MOVE 000     TO SQLCODE-WS                                       
015004       END-IF                                                             
015005       PERFORM UNTIL LINES-MISSING                                        
015007         MOVE SQLERRD(3) TO WS-MULTIFETCH                                 
015008         MOVE ZERO       TO WS-MX                                         
015009         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
015010           ADD +1        TO WS-MX                                         
015011**** INT2 SHOULD NOT BE IN THE FILE                                       
015012           IF WS-KDFINDOC(WS-MX) = 'INT2'                                 
015013             CONTINUE                                                     
015014           ELSE                                                           
015015             MOVE WS-IDLEGSEL     (WS-MX) TO WS-IDLEGSEL-2                
015016             MOVE WS-KDFINDOC     (WS-MX) TO WS-KDFINDOC-2                
015017             MOVE WS-IDPARTNR     (WS-MX) TO WS-IDPARTNR-2                
015018             MOVE GL-KDVALISO     (WS-MX) TO WS-KDVALISO-2                
015019             PERFORM DB2-SELECT-T01BURE-FCUS-DOTY                         
015020             IF LINES-FOUND                                               
015021               PERFORM F-CREATE-SEQUENCEFILE                              
015022               PERFORM S11-WRITE-WF20X4                                   
015023             END-IF                                                       
015024           END-IF                                                         
015025         END-PERFORM                                                      
015026         IF WS-MULTIFETCH = 100                                           
015027           PERFORM DB2-FETCH-CRS1                                         
015028           IF SQLERRD(3) > 0                                              
015029             MOVE 000     TO SQLCODE-WS                                   
015030           END-IF                                                         
015031         ELSE                                                             
015032           MOVE 100     TO SQLCODE-WS                                     
015033         END-IF                                                           
015034       END-PERFORM                                                        
015035       PERFORM DB2-CLOSE-CRS1                                             
015036       PERFORM DB2-FETCH-CRS-LSEL                                         
015037     END-PERFORM                                                          
015040     PERFORM DB2-CLOSE-CRS-LSEL                                           
015103     .                                                                    
015105                                                                          
015106 F-CREATE-SEQUENCEFILE SECTION.                                           
015107     MOVE  WS-DAREFDAT(WS-MX)   TO WS-DAREFDAT-2                          
015108     MOVE  WS-DAFINDOC(WS-MX)   TO WS-DAFINDOC-2                          
015109                                                                          
015110     MOVE  WS-IDLEGSEL(WS-MX)   TO WS-GL-IDLEGSEL                         
015113     MOVE  WS-IDPARTNR(WS-MX)   TO WS-GL-IDPARTNR                         
015114     MOVE  WS-KDFINDOC(WS-MX)   TO WS-GL-KDFINDOC                         
015133     MOVE  WS-DAREFDAT-2        TO WS-GL-DAREFDAT                         
015134     MOVE  WS-DAFINDOC-2        TO WS-GL-DAFINDOC                         
015135     MOVE  WS-IDEXCUST-1(WS-MX) TO WS-GL-IDEXCUST(1)                      
015136     MOVE  WS-IDEXCUST-2(WS-MX) TO WS-GL-IDEXCUST(2)                      
015137**** FIX FOR TIS                                                          
015138     IF WS-KDFINDOC(WS-MX) = 'INV2'                                       
015139       MOVE SPACE TO WS-IDEXCUST-3(WS-MX)                                 
015140     END-IF                                                               
015141     MOVE  WS-IDEXCUST-3(WS-MX) TO WS-GL-IDEXCUST(3)                      
015142     MOVE  WS-IDOPTION-1(WS-MX) TO WS-GL-IDOPTION(1)                      
015143     MOVE  WS-IDOPTION-2(WS-MX) TO WS-GL-IDOPTION(2)                      
015144     MOVE  WS-IDOPTION-3(WS-MX) TO WS-GL-IDOPTION(3)                      
015150     MOVE  WS-IDOPTION-4(WS-MX) TO WS-GL-IDOPTION(4)                      
015151     MOVE  WS-IDOPTION-5(WS-MX) TO WS-GL-IDOPTION(5)                      
015152     MOVE  WS-IDACCNT-1(WS-MX)  TO WS-GL-IDACCNT(1)                       
015153     MOVE  WS-IDACCNT-2(WS-MX)  TO WS-GL-IDACCNT(2)                       
015154     MOVE  WS-IDACCNT-3(WS-MX)  TO WS-GL-IDACCNT(3)                       
015155     MOVE  WS-IDACCNT-4(WS-MX)  TO WS-GL-IDACCNT(4)                       
015156                                                                          
015157     MOVE  GL-KDVALISO(WS-MX)   TO WS-GL-KDVALISO                         
015158     MOVE  GL-PRKURS(WS-MX)     TO WS-GL-PRKURS                           
015159     MOVE  GL-IDLANDX3-SEND(WS-MX) TO WS-GL-IDLANDX3-SEND                 
015160     MOVE  GL-FLSOFT(WS-MX)     TO WS-GL-FLSOFT                           
015161     MOVE  GL-FLFREE(WS-MX)     TO WS-GL-FLFREE                           
015162     MOVE  GL-IDFINDOC(WS-MX)   TO WS-GL-IDFINDOC                         
015163     MOVE  GL-IDLANDX3-BET(WS-MX) TO WS-GL-IDLANDX3-BET                   
015164     MOVE  GL-SUNTO-SERV(WS-MX) TO WS-GL-SUNTO-SERV                       
015165     MOVE  GL-SUBTO-SERV(WS-MX) TO WS-GL-SUBTO-SERV                       
015166     MOVE  GL-SUNTO-PART(WS-MX) TO WS-GL-SUNTO-PART                       
015167     MOVE  GL-SUBTO-PART(WS-MX) TO WS-GL-SUBTO-PART                       
015168     MOVE  GL-SUNTO-TOT(WS-MX)  TO WS-GL-SUNTO-TOT                        
015169     MOVE  GL-SUBTO-TOT(WS-MX)  TO WS-GL-SUBTO-TOT                        
015170     MOVE  GL-SUVAT-BILLIT-TOT(WS-MX) TO WS-GL-SUVAT-BILLIT-TOT           
015171     MOVE  GL-KDTRADP(WS-MX)    TO WS-GL-KDTRADP                          
015172     MOVE  GL-IDSYSTEM-SEND(WS-MX) TO WS-GL-IDSYSTEM-SEND                 
015173     MOVE  GL-KDBETALV(WS-MX)   TO WS-GL-KDBETALV                         
015174     MOVE  GL-IDBUNDLE(WS-MX)   TO WS-GL-IDBUNDLE                         
015175     MOVE  GL-IDREF(WS-MX)      TO WS-GL-IDREF                            
015176     MOVE  GL-IDARTNR-FINANCE(WS-MX) TO WS-GL-IDARTNR-FINANCE             
015177     MOVE  GL-PRARTBTO(WS-MX)   TO WS-GL-PRARTBTO                         
015178     MOVE  GL-PRARTNTO(WS-MX)   TO WS-GL-PRARTNTO                         
015179     MOVE  GL-REARTRAB(WS-MX)   TO WS-GL-REARTRAB                         
015180     MOVE  GL-REVAT(WS-MX)      TO WS-GL-REVAT                            
015181     MOVE  GL-KDVAT(WS-MX)      TO WS-GL-KDVAT                            
015182     MOVE  GL-KVLEVART(WS-MX)   TO WS-GL-KVLEVART                         
015183     MOVE  GL-FLSPECPR(WS-MX)   TO WS-GL-FLSPECPR                         
015184     MOVE  GL-KDANMORS(WS-MX)   TO WS-GL-KDANMORS                         
015185     MOVE  GL-IDDC(WS-MX)       TO WS-GL-IDDC                             
015186     MOVE  GL-SUNTO(WS-MX)      TO WS-GL-SUNTO                            
015187     MOVE  GL-SUBTO(WS-MX)      TO WS-GL-SUBTO                            
015188     MOVE  GL-SUVAT-BILLIT(WS-MX) TO WS-GL-SUVAT-BILLIT                   
015189     MOVE  GL-KDFRAKT(WS-MX)    TO WS-GL-KDFRAKT                          
015190     MOVE  GL-KDVALISO(WS-MX)   TO WS-GL-KDVALISO                         
015191     MOVE  GL-KDPARTTY(WS-MX)   TO WS-GL-KDPARTTY                         
015192     MOVE  GL-KDPARTGR(WS-MX)   TO WS-GL-KDPARTGR                         
015193     MOVE  DHEA-KDVALISO(WS-MX) TO WS-DHEA-KDVALISO                       
015195                                                                          
015196     PERFORM DB2-SELECT-MAX-T01CURR                                       
015197     PERFORM DB2-SELECT-T01CURR                                           
015198     MOVE T01CURR-PRKURS        TO WS-GL-PRKURS                           
015199     MOVE T01CURR-REVALUTA      TO WS-GL-REVALUTA                         
015200                                                                          
015201     PERFORM DB2-SELECT-T01CURR-INV                                       
015202     MOVE T01CURR-PRKURS        TO WS-GL-PRKURS-INV                       
015203     MOVE T01CURR-REVALUTA      TO WS-GL-REVALUTA-INV                     
015204     MOVE T01CURR-KDVALISO      TO WS-GL-KDVALISO-INV                     
015205     .                                                                    
015206                                                                          
015207 S11-WRITE-WF20X4 SECTION.                                                
015208     IF WS-GL-IDLEGSEL        = 'VCCS'                                    
015209       WRITE POST-WF2014    FROM WS-GL-WF2014                             
015210     ELSE                                                                 
015211       IF WS-GL-IDLEGSEL(1:2) = 'SC'                                      
015212         WRITE POST-WF2024  FROM WS-GL-WF2014                             
015213       END-IF                                                             
015214     END-IF                                                               
015215     .                                                                    
015216                                                                          
015217 Z-FINISH SECTION.                                                        
015218     CLOSE WF2014                                                         
015219     CLOSE WF2024                                                         
015220     .                                                                    
015221                                                                          
015230* --- DB2 SECTIONS  ---                                                   
017220 DB2-DCL-OPN-CRS1 SECTION.                                                
017231     MOVE 000100 TO GOOD-SQLCODES                                         
017240     EXEC SQL                                                             
017241        DECLARE CRS1 CURSOR WITH ROWSET POSITIONING FOR                   
017250        SELECT A.IDLEGSEL                                                 
017251             , A.KDVALISO                                                 
017260             , A.PRKURS                                                   
017261             , A.IDLANDX3_SEND                                            
017262             , A.IDPARTNR                                                 
017263             , A.KDFINDOC                                                 
017264             , A.FLSOFT                                                   
017265             , A.FLFREE                                                   
017267             , A.DAFINDOC                                                 
017268             , A.IDFINDOC                                                 
017270             , A.IDLANDX3_BET                                             
017271             , A.SUNTO_SERV                                               
017272             , A.SUBTO_SERV                                               
017273             , A.SUNTO_PART                                               
017274             , A.SUBTO_PART                                               
017275             , A.SUNTO_TOT                                                
017276             , A.SUBTO_TOT                                                
017278             , A.SUVAT_BILLIT_TOT                                         
017279             , A.KDTRADP                                                  
017280             , A.IDSYSTEM_SEND                                            
017281             , A.KDBETALV                                                 
017282             , B.IDEXCUST_1                                               
017290             , B.IDEXCUST_2                                               
017300             , B.IDEXCUST_3                                               
017400             , B.IDBUNDLE                                                 
017410             , B.IDREF                                                    
017420             , B.DAREFDAT                                                 
017430             , B.IDOPTION_1                                               
017440             , B.IDOPTION_2                                               
017450             , B.IDOPTION_3                                               
017460             , B.IDOPTION_4                                               
017470             , B.IDOPTION_5                                               
017480             , B.IDARTNR_FINANCE                                          
017481             , B.PRARTBTO                                                 
017482             , B.PRARTNTO                                                 
017483             , B.REARTRAB                                                 
017484             , B.REVAT                                                    
017485             , B.KDVAT                                                    
017486             , B.KVLEVART                                                 
017487             , B.FLSPECPR                                                 
017488             , B.KDANMORS                                                 
017489             , B.IDDC                                                     
017490             , B.SUNTO                                                    
017491             , B.SUBTO                                                    
017492             , B.SUVAT_BILLIT                                             
017493             , B.IDACCNT_1                                                
017494             , B.IDACCNT_2                                                
017495             , B.IDACCNT_3                                                
017496             , B.IDACCNT_4                                                
017497             , B.KDFRAKT                                                  
017499             , D.KDVALISO                                                 
017500             , D.KDPARTTY                                                 
017501             , D.KDPARTGR                                                 
017502                                                                          
017503        FROM   T01DHEA A                                                  
017504             , T01DLIN B                                                  
017506             , T01FCUS D                                                  
017507                                                                          
017509        WHERE  A.IDLEGSEL      = :T01PROC-IDLEGSEL                        
017510        AND    A.DAEXDAT       = :T01PROC-DAEXDAT                         
017511        AND    A.TIEXTID       = :T01PROC-TIEXTID                         
017512        AND    A.IDLEGSEL      = B.IDLEGSEL                               
017513        AND    A.DAEXDAT       = B.DAEXDAT                                
017514        AND    A.TIEXTID       = B.TIEXTID                                
017515        AND    A.KDVALISO      = B.KDVALISO                               
017516        AND    A.IDLANDX3_SEND = B.IDLANDX3_SEND                          
017517        AND    A.IDLEVNR       = B.IDLEVNR                                
017518        AND    A.IDPARTNR      = B.IDPARTNR                               
017519        AND    A.KDFINDOC      = B.KDFINDOC                               
017520        AND    A.FLSOFT        = B.FLSOFT                                 
017521        AND    A.FLFREE        = B.FLFREE                                 
017522        AND    A.FLPRIV        = B.FLPRIV                                 
017523        AND    A.IDBREAK_1     = B.IDBREAK_1                              
017524        AND    A.IDBREAK_2     = B.IDBREAK_2                              
017525        AND    D.IDLEGSEL      = A.IDLEGSEL                               
017526        AND    D.IDPARTNR      = A.IDPARTNR                               
017527        AND    D.KDSTATUS      = 1                                        
017528        AND    D.DADELDAT      = '00000000'                               
017529     END-EXEC                                                             
017530                                                                          
017531     EXEC SQL                                                             
017532        OPEN CRS1                                                         
017533     END-EXEC                                                             
017534                                                                          
017535     MOVE SQLCODE        TO SQLCODE-WS                                    
017536     PERFORM DB2-STATUS-CHECK                                             
017537     .                                                                    
017538                                                                          
017539 DB2-FETCH-CRS1 SECTION.                                                  
017540     MOVE 000100         TO GOOD-SQLCODES                                 
017541                                                                          
017542     EXEC SQL                                                             
017543              FETCH NEXT ROWSET FROM CRS1 FOR 100 ROWS                    
017544              INTO  :WS-IDLEGSEL                                          
017545                  , :GL-KDVALISO                                          
017550                  , :GL-PRKURS                                            
017560                  , :GL-IDLANDX3-SEND                                     
017570                  , :WS-IDPARTNR                                          
017580                  , :WS-KDFINDOC                                          
017581                  , :GL-FLSOFT                                            
017590                  , :GL-FLFREE                                            
017591                  , :WS-DAFINDOC                                          
017592                  , :GL-IDFINDOC                                          
017593                  , :GL-IDLANDX3-BET                                      
017594                  , :GL-SUNTO-SERV                                        
017595                  , :GL-SUBTO-SERV                                        
017596                  , :GL-SUNTO-PART                                        
017597                  , :GL-SUBTO-PART                                        
017598                  , :GL-SUNTO-TOT                                         
017599                  , :GL-SUBTO-TOT                                         
017601                  , :GL-SUVAT-BILLIT-TOT                                  
017602                  , :GL-KDTRADP                                           
017603                  , :GL-IDSYSTEM-SEND                                     
017604                  , :GL-KDBETALV                                          
017605                  , :WS-IDEXCUST-1                                        
017606                  , :WS-IDEXCUST-2                                        
017607                  , :WS-IDEXCUST-3                                        
017608                  , :GL-IDBUNDLE                                          
017609                  , :GL-IDREF                                             
017610                  , :WS-DAREFDAT                                          
017611                  , :WS-IDOPTION-1                                        
017612                  , :WS-IDOPTION-2                                        
017620                  , :WS-IDOPTION-3                                        
017630                  , :WS-IDOPTION-4                                        
017640                  , :WS-IDOPTION-5                                        
017650                  , :GL-IDARTNR-FINANCE                                   
017660                  , :GL-PRARTBTO                                          
017670                  , :GL-PRARTNTO                                          
017680                  , :GL-REARTRAB                                          
017690                  , :GL-REVAT                                             
017691                  , :GL-KDVAT                                             
017700                  , :GL-KVLEVART                                          
017810                  , :GL-FLSPECPR                                          
017820                  , :GL-KDANMORS                                          
017830                  , :GL-IDDC                                              
017840                  , :GL-SUNTO                                             
017850                  , :GL-SUBTO                                             
017860                  , :GL-SUVAT-BILLIT                                      
017870                  , :WS-IDACCNT-1                                         
017880                  , :WS-IDACCNT-2                                         
017890                  , :WS-IDACCNT-3                                         
017891                  , :WS-IDACCNT-4                                         
017892                  , :GL-KDFRAKT                                           
017894                  , :DHEA-KDVALISO                                        
017896                  , :GL-KDPARTTY                                          
017897                  , :GL-KDPARTGR                                          
017900     END-EXEC                                                             
017910                                                                          
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
019011                                                                          
019306 DB2-CLOSE-CRS1 SECTION.                                                  
019307                                                                          
019308     EXEC SQL                                                             
019309        CLOSE CRS1                                                        
019310     END-EXEC                                                             
019311     .                                                                    
019313                                                                          
019314 DB2-OPEN-CRS-LSEL SECTION.                                               
019315     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
019316     SELECT   T01LSEL.IDLEGSEL                                            
019317                                                                          
019318     FROM     T01LSEL                                                     
019319                                                                          
019320     WHERE    KDSTATUS = 1                                                
019330     END-EXEC                                                             
019331                                                                          
019332     EXEC SQL OPEN T01LSEL-CRS                                            
019333     END-EXEC                                                             
019334                                                                          
019335     MOVE 000            TO GOOD-SQLCODES                                 
019336     MOVE SQLCODE        TO SQLCODE-WS                                    
019337     PERFORM DB2-STATUS-CHECK                                             
019338     .                                                                    
019339                                                                          
019340 DB2-FETCH-CRS-LSEL SECTION.                                              
019341     EXEC SQL FETCH T01LSEL-CRS INTO                                      
019342            :WS-IDLEGSEL-CRS                                              
019343     END-EXEC                                                             
019344                                                                          
019345     MOVE 000100         TO GOOD-SQLCODES                                 
019346     MOVE SQLCODE        TO SQLCODE-WS                                    
019347     PERFORM DB2-STATUS-CHECK                                             
019348     .                                                                    
019349                                                                          
019350 DB2-CLOSE-CRS-LSEL SECTION.                                              
019351     EXEC SQL CLOSE T01LSEL-CRS                                           
019352     END-EXEC                                                             
019353     .                                                                    
019354     EJECT                                                                
019355                                                                          
019356 DB2-SELECT-T01BURE-FCUS-DOTY SECTION.                                    
019357     MOVE 000100  TO GOOD-SQLCODES                                        
019358     EXEC SQL                                                             
019359         SELECT A.FLGL                                                    
019360                                                                          
019361         INTO :BURE-FLGL                                                  
019363                                                                          
019364         FROM    T01BURE A                                                
019365               , T01FCUS B                                                
019366               , T01DOTY C                                                
019367                                                                          
019368         WHERE A.IDLEGSEL = :WS-IDLEGSEL-2                                
019369         AND   B.IDLEGSEL = :WS-IDLEGSEL-2                                
019370         AND   C.IDLEGSEL = :WS-IDLEGSEL-2                                
019371         AND   A.KDFINDOC = :WS-KDFINDOC-2                                
019372         AND   C.KDFINDOC = :WS-KDFINDOC-2                                
019373         AND   B.IDPARTNR = :WS-IDPARTNR-2                                
019374         AND   A.IDLEGSEL = B.IDLEGSEL                                    
019375         AND   A.KDPARTTY = B.KDPARTTY                                    
019376         AND   A.KDPARTGR = B.KDPARTGR                                    
019377         AND   A.FLGL     = :JA                                           
019378         AND   C.FLGL     = :JA                                           
019379         AND   A.KDSTATUS = 001                                           
019380         AND   B.KDSTATUS = 001                                           
019381         AND   C.KDSTATUS = 001                                           
019382         AND   A.DADELDAT = '00000000'                                    
019383         AND   B.DADELDAT = '00000000'                                    
019384         AND   C.DADELDAT = '00000000'                                    
019385     END-EXEC                                                             
019386     MOVE SQLCODE TO SQLCODE-WS                                           
019387     PERFORM DB2-STATUS-CHECK                                             
019388     .                                                                    
019389                                                                          
019390 DB2-SELECT-MAX-T01CURR SECTION.                                          
019391     MOVE 000            TO GOOD-SQLCODES                                 
019392                                                                          
019393     EXEC SQL                                                             
019394     SELECT   MAX(T01CURR.DASTADAT)                                       
019395                                                                          
019396     INTO     :WS-DASTADAT-KEY                                            
019397                                                                          
019398     FROM     T01CURR                                                     
019399                                                                          
019400     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL-2                           
019401        AND  (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
019402        OR    T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
019403     END-EXEC                                                             
019404                                                                          
019405     MOVE SQLCODE        TO SQLCODE-WS                                    
019406     PERFORM DB2-STATUS-CHECK                                             
019407     .                                                                    
019408                                                                          
019409 DB2-SELECT-T01CURR SECTION.                                              
019410     MOVE 000            TO GOOD-SQLCODES                                 
019411                                                                          
019412     EXEC SQL                                                             
019413     SELECT   KDVALISO                                                    
019414             ,PRKURS                                                      
019415             ,REVALUTA                                                    
019416                                                                          
019417     INTO     :T01CURR-KDVALISO                                           
019418             ,:T01CURR-PRKURS                                             
019419             ,:T01CURR-REVALUTA                                           
019420                                                                          
019421     FROM     T01CURR                                                     
019422                                                                          
019423     WHERE    IDLEGSEL = :WS-GL-IDLEGSEL                                  
019424       AND    KDVALISO = :WS-KDVALISO-2                                   
019425       AND    DASTADAT = :WS-DASTADAT-KEY                                 
019426     END-EXEC                                                             
019427                                                                          
019428     MOVE SQLCODE        TO SQLCODE-WS                                    
019429     PERFORM DB2-STATUS-CHECK                                             
019430     .                                                                    
019431                                                                          
019432 DB2-SELECT-T01CURR-INV SECTION.                                          
019433     MOVE 000            TO GOOD-SQLCODES                                 
019434                                                                          
019435     EXEC SQL                                                             
019436     SELECT   KDVALISO                                                    
019437             ,PRKURS                                                      
019438             ,REVALUTA                                                    
019439                                                                          
019440     INTO     :T01CURR-KDVALISO                                           
019441             ,:T01CURR-PRKURS                                             
019442             ,:T01CURR-REVALUTA                                           
019443                                                                          
019444     FROM     T01CURR                                                     
019445                                                                          
019446     WHERE    IDLEGSEL = :WS-GL-IDLEGSEL                                  
019447       AND    KDVALISO = :WS-DHEA-KDVALISO                                
019448       AND    DASTADAT = :WS-DASTADAT-KEY                                 
019449     END-EXEC                                                             
019450                                                                          
019451     MOVE SQLCODE        TO SQLCODE-WS                                    
019452     PERFORM DB2-STATUS-CHECK                                             
019453     .                                                                    
019454                                                                          
019455 DB2-SELECT-T01PROC-TAB   SECTION.                                        
019456     MOVE 000    TO GOOD-SQLCODES                                         
019457                                                                          
019458     EXEC SQL                                                             
019459           SELECT  DAEXDAT                                                
019460                ,  TIEXTID                                                
019461                ,  KDBEH                                                  
019462                ,  IDLEGSEL                                               
019463                                                                          
019464           INTO   :T01PROC-DAEXDAT                                        
019465                , :T01PROC-TIEXTID                                        
019466                , :T01PROC-KDBEH                                          
019467                , :T01PROC-IDLEGSEL                                       
019468                                                                          
019469           FROM    T01PROC                                                
019470                                                                          
019471           WHERE   IDSYSTEM = :WS-IDSYSTEM     AND                        
019472                   IDLEGSEL = :WS-IDLEGSEL-CRS                            
019473     END-EXEC                                                             
019474                                                                          
019475     MOVE SQLCODE TO SQLCODE-WS                                           
019476     PERFORM DB2-STATUS-CHECK                                             
019477     .                                                                    
019478                                                                          
019479 DB2-STATUS-CHECK  SECTION.                                               
019480                                                                          
019481     SET SQLCODE-IX TO 1                                                  
019482     SEARCH GOOD-SQLCODE                                                  
019483       AT END                                                             
019484          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
019485          DELIMITED BY SIZE INTO ERROR-TEXT                               
019486          CALL ABEND USING RKOD-ABEND-DB2                                 
019487       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
019488          CONTINUE                                                        
019490     END-SEARCH                                                           
019500     .                                                                    
