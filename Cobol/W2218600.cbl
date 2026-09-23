000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2218600.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   93/10/13.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*     FRAMSTÄLLA EN FIL                                                   
001100*             MED LEVNR  OM    FLLEVPLP = JA OCH DET ÄR                   
001200*                              PERIODSLUTKÖRNING                          
001300*                        ELLER DAGENS-DATUM = ETT ANGIVET                 
001400*                              KÖRNINGSDATUM (TISEND-PER)                 
001500*                    (TILLAGT FEB-03)                                     
001600*                        ELLER FLLEVVB = JA OCH DET ÄR                    
001700*                              VECKOKÖRNING                               
001800*                                                                         
001900*        PARAMETERKORT LÄSES IN: DAGLIG KÖRNING / PERIODSLUT              
002000*                                                                         
002100*        PROGRAMMET LÄSER      WLXXBK (WDR2)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- PARAMETERKORT IN                                           
003600     SELECT PARMIN                     ASSIGN TO W22186D1.                
003700     SKIP2                                                                
003800*          --- LEVNR FÖR DAGENS PERIODKÖRNING                             
003900     SELECT W22186                     ASSIGN TO W22186D2.                
004000     SKIP2                                                                
004100*          --- PARAMETERKORT UT                                           
004200     SELECT PARMUT                     ASSIGN TO W22186D3.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  PARMIN                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200 01  FILLER                 PIC X(80).                                    
005300     SKIP3                                                                
005400 FD  W22186                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700     SKIP2                                                                
005800*01  POST -COPY W22186 -PRE  UT-  -L.                                     
005900     SKIP3                                                                
006000 FD  PARMUT                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300     SKIP2                                                                
006400 01  UT-PARM                PIC X(80).                                    
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700     SKIP2                                                                
006800*    -COPY WY2000W1                                                       
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(8)    VALUE 'W2218600'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300     SKIP2                                                                
007400 01  SW-UT-IDLEVNR               PIC X       VALUE 'N'.                   
007500 01  SW-PERIODSLUTKORNING        PIC X       VALUE 'N'.                   
007600     SKIP3                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200 01  DAGENS-AAVVD                PIC 9(5)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-AAVVD.                                       
008400     03  DAGENS-AA               PIC 9(2).                                
008500     03  DAGENS-VV               PIC 9(2).                                
008600     03  DAGENS-D                PIC 9.                                   
008700     SKIP2                                                                
008701*    --- AKTUELLT DATUM,EJ SAMMA SOM I DATKORT.                           
008710 01  DAGENS-DAGNR                PIC 9(1)    VALUE ZERO.                  
008720     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009600     SKIP2                                                                
009700*    --- PARAMETRAR TILL ABEND                                            
009800                                                                          
009900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010100     SKIP2                                                                
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL DATKORT                                          
010700*                                                                         
010800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22186'.              
010900     SKIP2                                                                
011000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011100     SKIP2                                                                
011200*01  -COPY WDATKORT                                                       
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL POSTSUM                                          
011500*                                                                         
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL WDATKONV                                         
011900*                                                                         
012000*01  -COPY WDATAREA                                                       
012100     EJECT                                                                
012200 01  UT-AREA-START               PIC X(24)   VALUE                        
012300                                 'UT-AREA-START  '.                       
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W22186     -PRE UT-                                       
012700     SKIP3                                                                
012800     SKIP3                                                                
012900 01  PARM-AREA.                                                           
013000     03  KORTYP             PIC X(3)   VALUE SPACE.                       
013100     03  FILLER             PIC X(77)  VALUE SPACE.                       
013200     EJECT                                                                
013300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     SKIP3                                                                
013700 01  NYCKLAR-TILL-DLI.                                                    
013800     03  W-WDGXKEY-X.                                                     
013900         05  FILLER              PIC X(4)     VALUE '2215'.               
014000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
014100     03  W-IDLEVNR-X.                                                     
014200         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
014300     SKIP2                                                                
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014900     SKIP2                                                                
015000 01  GODK-STATUSKODER.                                                    
015100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015200     SKIP3                                                                
015300 01  SSA1                        PIC X(64).                               
015400 01  SSA2                        PIC X(64).                               
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016100     SKIP3                                                                
016200 01  DLI-IO-AREA.                                                         
016300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
016400     SKIP3                                                                
016500     03  WLXXBK01 REDEFINES IO-AREA.                                      
016600*        05  -COPY WDGX01  -PRE XXBK-                                     
016700     EJECT                                                                
016800     03  WLXXBK11 REDEFINES IO-AREA.                                      
016900*        05  -COPY WDGX2216 -PRE XXBK-                                    
017000     EJECT                                                                
017100 LINKAGE SECTION.                                                         
017200                                                                          
017300                                                                          
017400*01  -COPY W0008  -PRE XXBK-                                              
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700 PROCEDURE DIVISION  USING XXBK-PCB.                                      
017800     ENTRY 'DLITCBL' USING XXBK-PCB.                                      
017900                                                                          
018000     SKIP2                                                                
018100     PERFORM A-INIT                                                       
018200                                                                          
018300     PERFORM IMS-GU-XXBK-ROT                                              
018400     PERFORM IMS-GNP-XXBK-2216                                            
018500                                                                          
018600     PERFORM UNTIL SEGMENT-SAKNAS                                         
018700        IF XXBK-2216-KDEDI NOT = 'T'                                      
018800*          (TEST-LEV SKALL EJ MED)                                        
018900*          KÖRTYP BESTÄMMER URVAL  (PERIOD /VECKO /DAGLIG)                
019000           IF KORTYP = 'PER' AND                                          
019100              XXBK-2216-FLLEVPLP  = JA                                    
019200              MOVE XXBK-2216-IDLEVNR TO UT-IDLEVNR                        
019300              MOVE JA                TO SW-UT-IDLEVNR                     
019400           END-IF                                                         
019500           IF KORTYP = 'VEC' AND                                          
019600              XXBK-2216-FLLEVVB  = JA                                     
019710              IF XXBK-2216-KDVECKOSL = 'D'                                
019711                IF DAGENS-DAGNR = 1                                       
019720*--               OM DET ÄR MÅNDAG OCH LEVERANTÖREN HAR FAST              
019721*--               DAGLIG SÄNDNING (KDVECKOSL)                             
019740                                                                          
019800                  MOVE XXBK-2216-IDLEVNR TO UT-IDLEVNR                    
019900                  MOVE JA                TO SW-UT-IDLEVNR                 
019901                END-IF                                                    
019902              ELSE                                                        
019903                IF DAGENS-DAGNR = 4                                       
019905*--               OM DET ÄR TORSDAG OCH LEVERANTÖREN HAR FAST             
019906*--               ONSDAGS SÄNDNING (KDVECKOSL = V)                        
019907                                                                          
019910                  MOVE XXBK-2216-IDLEVNR TO UT-IDLEVNR                    
019920                  MOVE JA                TO SW-UT-IDLEVNR                 
019940                END-IF                                                    
019950              END-IF                                                      
020000           END-IF                                                         
020100           MOVE XXBK-2216-TISEND-PER   TO TMP1-YYMMDD                     
020200           MOVE DAGENS-DATUM           TO TMP2-YYMMDD                     
020300           PERFORM WY2000P1                                               
020400           IF KORTYP = 'DAG' AND                                          
020500              TMP1-YYMMDD     > ZERO AND                                  
020600              TMP1-YYMMDD NOT > TMP2-YYMMDD                               
020700              MOVE XXBK-2216-IDLEVNR TO UT-IDLEVNR                        
020800              MOVE JA                TO SW-UT-IDLEVNR                     
020900           END-IF                                                         
021000           IF SW-UT-IDLEVNR = JA                                          
021100              PERFORM S11-SKRIV-W22186                                    
021200              MOVE NEJ TO SW-UT-IDLEVNR                                   
021300           END-IF                                                         
021400        END-IF                                                            
021500        PERFORM IMS-GNP-XXBK-2216                                         
021600     END-PERFORM                                                          
021700                                                                          
021800                                                                          
021900     PERFORM Z-FINIT                                                      
022000                                                                          
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     OPEN INPUT  PARMIN                                                   
022800     OPEN OUTPUT W22186                                                   
022900                 PARMUT                                                   
023000                                                                          
023100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023200                                                                          
023210*--  TAG REDA PÅ DAGENS DAGNUMMER, EJ FRÅN DATKORTET.                     
023220*--  W221V2 KÖRS PÅ MÅNDAG MED FREDAGENS DATUM I DATKORT.                 
023221*--  W221V2 KÖRS PÅ TORSDAG MED ONSDAGENS DATUM I DATKORT.                
023230                                                                          
023240     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
023250     CALL WDATKONV USING DAT-KDDATFORM                                    
023260                         DAT-I-TIDATUM                                    
023270                         DAT-O-TIDATUM                                    
023280                         DAT-KDSVAR                                       
023290                                                                          
023291     MOVE DAT-TID     TO DAGENS-DAGNR                                     
023292                                                                          
023293                                                                          
023300*--  HÄMTA DAGENS-DATUM FRÅN DATKORT                                      
023400                                                                          
023500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023600     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
023700                         DAGENS-AA                                        
023800                         DAT-TIAA-PPPER                                   
023900     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
024000     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
024100     MOVE D-VECKA     TO DAGENS-VV                                        
024200     MOVE D-DAGNR     TO DAGENS-D                                         
024300     MOVE D-PERIOD    TO DAT-TIPP                                         
024400                                                                          
024500     PERFORM S01-LAS-PARMIN                                               
024600                                                                          
024700     PERFORM S12-SKRIV-PARMUT                                             
024800     .                                                                    
024900     EJECT                                                                
025000 Z-FINIT SECTION.                                                         
025100                                                                          
025200     CLOSE PARMIN                                                         
025300           W22186                                                         
025400           PARMUT                                                         
025500     SKIP2                                                                
025600     MOVE 'S' TO POSTSUM-OPKOD                                            
025700     CALL POSTSUM USING POSTSUM-PARM                                      
025800     .                                                                    
025900     EJECT                                                                
026000 S01-LAS-PARMIN   SECTION.                                                
026100     SKIP2                                                                
026200*    LÄS DATAKORT FRÅN JCL OM DET ÄR DAGLIG ELLER PERIODKÖRNING           
026300                                                                          
026400     READ PARMIN INTO PARM-AREA                                           
026500                                                                          
026600     AT END                                                               
026700        DISPLAY '***   PARM SAKNAS   ***'                                 
026800        PERFORM S99-ABEND                                                 
026900     NOT AT END                                                           
027000        IF KORTYP NOT = 'DAG' AND                                         
027100           KORTYP NOT = 'PER' AND                                         
027200           KORTYP NOT = 'VEC'                                             
027300           DISPLAY '***   PARM FELAKTIG ***'                              
027400           PERFORM S99-ABEND                                              
027500        END-IF                                                            
027600                                                                          
027700        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
027800        MOVE 'W22186'   TO POSTSUM-FDNAMN                                 
027900        MOVE 'W22186D1' TO POSTSUM-DDNAMN2                                
028000        CALL POSTSUM USING POSTSUM-PARM                                   
028100     END-READ                                                             
028200     .                                                                    
028300     EJECT                                                                
028400 S11-SKRIV-W22186 SECTION.                                                
028500     SKIP2                                                                
028600*    HÄR SKRIVS DE LEVNR SOM BERÖRS AV DENNA LEVERANSPLANEKÖRNING         
028700                                                                          
028800     WRITE UT-POST FROM UT-AREA                                           
028900                                                                          
029000     MOVE 'LEV'      TO POSTSUM-TRANSTYP                                  
029100     MOVE 'W22186'   TO POSTSUM-FDNAMN                                    
029200     MOVE 'W22186D2' TO POSTSUM-DDNAMN2                                   
029300     CALL POSTSUM USING POSTSUM-PARM                                      
029400     .                                                                    
029500     EJECT                                                                
029600 S12-SKRIV-PARMUT SECTION.                                                
029700     SKIP2                                                                
029800*    HÄR SKRIVER VI UT PARAMETER TILL EFTERFÖLJANDE PGM                   
029900*    SÅ ATT MAN KAN SKILJA PÅ OM DET ÄR EN DAGLIG ELLER                   
030000*    EN PERIODSLUTS-KÖRNING                                               
030100*    (DAG KÖRS FÖRE PERIOD OM SAMMA DAG)                                  
030200                                                                          
030300     WRITE UT-PARM FROM PARM-AREA                                         
030400                                                                          
030500     MOVE 'PARM'     TO POSTSUM-TRANSTYP                                  
030600     MOVE 'W22186'   TO POSTSUM-FDNAMN                                    
030700     MOVE 'W22186D3' TO POSTSUM-DDNAMN2                                   
030800     CALL POSTSUM USING POSTSUM-PARM                                      
030900     .                                                                    
031000     EJECT                                                                
031100 S99-ABEND SECTION.                                                       
031200     SKIP2                                                                
031300     SKIP2                                                                
031400     MOVE 'S' TO POSTSUM-OPKOD                                            
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
031700     .                                                                    
031800     EJECT                                                                
031900* --- IMS SEKTIONER ---                                                   
032000     SKIP3                                                                
032100 IMS-GU-XXBK-ROT SECTION.                                                 
032200     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-X ')'                         
032300          DELIMITED BY SIZE INTO SSA1                                     
032400     MOVE '  GE' TO GODK-STATUSKODER                                      
032500     CALL CBLTDLI USING GU XXBK-PCB DLI-IO-AREA SSA1                      
032600     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     SKIP3                                                                
033000 IMS-GNP-XXBK-2216 SECTION.                                               
033100     STRING 'WLXXBK11    '                                                
033200          DELIMITED BY SIZE INTO SSA1                                     
033300     MOVE '  GE' TO GODK-STATUSKODER                                      
033400     CALL CBLTDLI USING GNP XXBK-PCB DLI-IO-AREA SSA1                     
033500     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
033600     PERFORM IMS-STATUSKONTROLL                                           
033700                                                                          
033800***  FIX                                                                  
033900     IF SEGMENT-FINNS                                                     
034000        IF XXBK-2216-TISEND-PER NOT NUMERIC                               
034100           MOVE ZERO TO XXBK-2216-TISEND-PER                              
034200        END-IF                                                            
034300     END-IF                                                               
034400***  FIX                                                                  
034500     .                                                                    
034600     EJECT                                                                
034700 IMS-STATUSKONTROLL SECTION.                                              
034800     SKIP2                                                                
034900     SET STATUS-IX TO 1                                                   
035000     SEARCH GODK-STATUS                                                   
035100       AT END                                                             
035200         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
035300         DISPLAY FELTEXT                                                  
035400         CALL FELLOG                                                      
035500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035600         CONTINUE                                                         
035700     END-SEARCH                                                           
035800     .                                                                    
035900     EJECT                                                                
036000*    -COPY WY2000P1                                                       
