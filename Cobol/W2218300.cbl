000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2218300.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   93/08/18.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                KOPIA PÅ PGM W22182 + RENSNING                           
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        LÄSER FIL W22182 MED ARTIKLAR                                    
001110*        (FRÅN PGM W22181 - BMP MED EV. ÅTERSTARTSPROBLEM                 
001120*        UTAN DENNA LÖSNING)                                              
001200*        SOM SKALL DELETAS PÅ WLXXBL                                      
001210*        OBS  NY ROT PÅ XXBL = 2217J !                                    
001600*                                                                         
001700*        I SAMBAND MED PERIODSLUTKÖRNING SÅ RENSAS BASEN (XXBL)           
001800*        PÅ ALLT UTOM DE ART SOM HAR LEVERANTÖR MED                       
001900*        TISEND-PER > DAGENS-DATUM                                        
002000*                                                                         
002100*        PROGRAMMET UPPDATERAR WLXXBL (WDR5)                              
002200*        PROGRAMMET LÄSER      WLXXBK (WDR2)                              
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- INFIL LEVNR-ARTNR                                          
003700     SELECT W22182                     ASSIGN TO W22183D1.                
003800*          --- INFIL PARAMETER DAG/PERIOD                                 
003900     SELECT PARMIN                     ASSIGN TO W22183D2.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W22182                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  -COPY W22182      -L.                                                
005000     SKIP2                                                                
005100 FD  PARMIN                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING F                                                          
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600 01  FILLER                     PIC X(80).                                
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
005901*    -COPY WY2000W1                                                       
005910     SKIP3                                                                
006000 77  IDPGM                       PIC X(8)    VALUE 'W2218300'.            
006100                                                                          
006200 01  CHKP-VAR.                                                            
006300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
006400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
006500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
006600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
006700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006800 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
006900                                                                          
007000 77  JA                          PIC X       VALUE 'J'.                   
007100 77  NEJ                         PIC X       VALUE 'N'.                   
007101                                                                          
007102 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007110 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007200     SKIP2                                                                
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700 77  W22182-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W22182                       VALUE 'J'.                   
007900     EJECT                                                                
008000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500     SKIP3                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009010     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
009020     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009401     EJECT                                                                
009402*    --- PARAMETRAR TILL DATKORT                                          
009403*                                                                         
009404 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22186'.              
009405     SKIP2                                                                
009406 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009407     SKIP2                                                                
009410*01  -COPY WDATKORT                                                       
009500     EJECT                                                                
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                             'IN-AREA-START'.             
009800     SKIP2                                                                
009900 01  PARM-AREA.                                                           
010000     03  KORTYP                  PIC X(3)  VALUE SPACE.                   
010100     03  FILLER                  PIC X(73) VALUE SPACE.                   
010200     SKIP3                                                                
010300                                                                          
010400*01  AREA -COPY W22182     -PRE IN-                                       
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000     03  W-WDGXKEY-ROT-BL.                                                
011100         05  FILLER              PIC X(05)    VALUE '2217J'.              
011200         05  FILLER              PIC X(25)    VALUE LOW-VALUE.            
011300     03  W-WDGXKEY-ROT-BK.                                                
011400         05  FILLER              PIC X(04)    VALUE '2215'.               
011500         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
011600     03  W-WDGXKEY-X.                                                     
011700         05  W-WDGXKEY-IDLEVNR   PIC X(5) VALUE SPACE.                    
011800         05  W-WDGXKEY-IDARTNR   PIC S9(9) COMP-3 VALUE ZERO.             
011900     03  W-IDLEVNR-X.                                                     
012000         05  W-IDLEVNR           PIC X(5) VALUE SPACE.                    
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     88  IMS-EJ-OK                           VALUE 'XD'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(64).                               
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014100     SKIP3                                                                
014200 01  DLI-IO-AREA.                                                         
014300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014400     SKIP3                                                                
014500     03  WLXXBL01 REDEFINES IO-AREA.                                      
014600*        05  -COPY WDGX2217  -PRE XXBL-                                   
014700     EJECT                                                                
014800     03  WLXXBL11 REDEFINES IO-AREA.                                      
014900*        05  -COPY WDGX2218  -PRE XXBL-                                   
015000     EJECT                                                                
015100*    ---  DLI INPUT-OUTPUT AREA                                           
015200 01  FILLER                      PIC X(16)                                
015300                             VALUE 'DLI-IO-AREA-2'.                       
015400     SKIP3                                                                
015500 01  DLI-IO-AREA-2.                                                       
015600     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
015700     SKIP3                                                                
015800     03  WLXXBK11 REDEFINES IO-AREA-2.                                    
015900*        05  -COPY WDGX2216  -PRE XXBK-                                   
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200                                                                          
016300*01  -COPY W0009   -PRE MSG-                                              
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE XXBL-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE XXBK-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100 PROCEDURE DIVISION  USING MSG-PCB XXBL-PCB XXBK-PCB.                     
017200     ENTRY 'DLITCBL' USING MSG-PCB XXBL-PCB XXBK-PCB.                     
017300                                                                          
017400     SKIP2                                                                
017500     PERFORM A-INIT                                                       
017510                                                                          
017600     PERFORM S01-LAES-W22182                                              
017610                                                                          
017700     PERFORM UNTIL END-OF-W22182                                          
017800       IF CHKP-ANT > CHKP-MAX                                             
017900         PERFORM X-TAG-CHECKPOINT                                         
018000       END-IF                                                             
018100                                                                          
018200       PERFORM B-DELETE-BL11                                              
018300                                                                          
018400       PERFORM S01-LAES-W22182                                            
018500     END-PERFORM                                                          
018600                                                                          
018700     IF KORTYP = 'PER'                                                    
018710*       VID PERIODKÖRNING SÅ RENSAR VI HÄNDELSEBASEN                      
018720*       PÅ SÅDANT SOM EJ LÄNGRE ÄR RELEVANT                               
018730                                                                          
018800        PERFORM C-RENSA-GAMMALT-XXBL                                      
018900     END-IF                                                               
019000                                                                          
019100     PERFORM Z-FINIT                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800     SKIP2                                                                
019900                                                                          
020000     PERFORM IMS-RESTART                                                  
020100                                                                          
020200     OPEN INPUT W22182                                                    
020300                PARMIN                                                    
020400                                                                          
020500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020501                                                                          
020502     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020503     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
020506     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
020510     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
020600                                                                          
020700     PERFORM S02-LAES-PARMIN                                              
020800     .                                                                    
020900     EJECT                                                                
021000 B-DELETE-BL11 SECTION.                                                   
021100     SKIP2                                                                
021200     MOVE IN-IDLEVNR TO W-WDGXKEY-IDLEVNR                                 
021300     MOVE IN-IDARTNR TO W-WDGXKEY-IDARTNR                                 
021400                                                                          
021500     PERFORM IMS-GHU-XXBL-BL11                                            
021600                                                                          
021700     IF SEGMENT-FINNS                                                     
021800        PERFORM IMS-DLET-XXBL                                             
021810        ADD +1 TO CHKP-ANT                                                
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 C-RENSA-GAMMALT-XXBL SECTION.                                            
022300     SKIP2                                                                
022400     PERFORM IMS-GU-XXBL-ROT                                              
022500     PERFORM IMS-GHNP-XXBL-BL11                                           
022600                                                                          
022700     PERFORM UNTIL SEGMENT-SAKNAS                                         
022800       IF CHKP-ANT > CHKP-MAX                                             
022810         MOVE XXBL-2218-IDLEVNR TO W-WDGXKEY-IDLEVNR                      
022820         MOVE XXBL-2218-IDARTNR TO W-WDGXKEY-IDARTNR                      
022900         PERFORM X-TAG-CHECKPOINT                                         
022910         PERFORM IMS-GU-XXBL-ROT                                          
022920         PERFORM IMS-GHNP-XXBL-BL11-KVAL                                  
023000       END-IF                                                             
023100                                                                          
023200       IF XXBL-2218-IDLEVNR NOT = W-IDLEVNR                               
023300          MOVE XXBL-2218-IDLEVNR  TO W-IDLEVNR                            
023400          PERFORM IMS-GU-XXBK-BK11                                        
023401          IF SEGMENT-SAKNAS                                               
023402             MOVE ZERO TO XXBK-2216-TISEND-PER                            
023403          END-IF                                                          
023410******    FIX                                                             
023420          IF XXBK-2216-TISEND-PER NOT NUMERIC                             
023430             MOVE ZERO TO XXBK-2216-TISEND-PER                            
023440          END-IF                                                          
023450******    FIX                                                             
023500       END-IF                                                             
024200                                                                          
024201       MOVE XXBK-2216-TISEND-PER   TO TMP1-YYMMDD                         
024202       MOVE DAGENS-DATUM           TO TMP2-YYMMDD                         
024210       PERFORM WY2000P1                                                   
024300       IF TMP1-YYMMDD <= TMP2-YYMMDD                                      
024400          PERFORM IMS-DLET-XXBL                                           
024410          ADD +1 TO CHKP-ANT                                              
024500       END-IF                                                             
024600                                                                          
024700       PERFORM IMS-GHNP-XXBL-BL11                                         
024800     END-PERFORM                                                          
024900     .                                                                    
025000     EJECT                                                                
025100 Z-FINIT SECTION.                                                         
025200                                                                          
025300                                                                          
025400     CLOSE W22182                                                         
025500           PARMIN                                                         
025600     SKIP2                                                                
025700     MOVE 'S' TO POSTSUM-OPKOD                                            
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     .                                                                    
026000     EJECT                                                                
026100 S01-LAES-W22182  SECTION.                                                
026200     SKIP2                                                                
026210*    ARTIKLAR FRÅN W22181 SOM SKALL RENSAS PÅ 2217J                       
026220                                                                          
026300     READ W22182 INTO IN-AREA                                             
026400     AT END                                                               
026500        SET END-OF-W22182 TO TRUE                                         
026600                                                                          
026700     NOT AT END                                                           
026800        MOVE 'W22182'   TO POSTSUM-FDNAMN                                 
026900        MOVE 'W22183D1' TO POSTSUM-DDNAMN2                                
027000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
027100        CALL POSTSUM USING POSTSUM-PARM                                   
027200                                                                          
027300*       ADD 1 TO W-W22183-KVPOST-IN                                       
027400     END-READ                                                             
027500     .                                                                    
027600     EJECT                                                                
027700 S02-LAES-PARMIN  SECTION.                                                
027800     SKIP2                                                                
027810*    HÄR LÄSER VI IN PARAMETER FÖR ATT SE OM DETTA ÄR EN                  
027820*    DAGLIG ELLER PERIODSLUTS-KÖRNING                                     
027830                                                                          
027900     READ PARMIN INTO PARM-AREA                                           
028000                                                                          
028100     AT END                                                               
028200        DISPLAY '***   PARM SAKNAS   ***'                                 
028300        PERFORM S99-ABEND                                                 
028400     NOT AT END                                                           
028500        IF KORTYP NOT = 'DAG' AND                                         
028600           KORTYP NOT = 'VEC' AND                                         
028610           KORTYP NOT = 'PER'                                             
028700           DISPLAY '***   PARM FELAKTIG ***'                              
028800           PERFORM S99-ABEND                                              
028900        END-IF                                                            
029000                                                                          
029100        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
029200        MOVE 'W22183'   TO POSTSUM-FDNAMN                                 
029300        MOVE 'W22183D2' TO POSTSUM-DDNAMN2                                
029400        CALL POSTSUM USING POSTSUM-PARM                                   
029500     END-READ                                                             
029501     .                                                                    
029502     EJECT                                                                
029503 S99-ABEND SECTION.                                                       
029505     SKIP2                                                                
029506     MOVE 'S' TO POSTSUM-OPKOD                                            
029507     CALL POSTSUM USING POSTSUM-PARM                                      
029510     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
029600     .                                                                    
029700     EJECT                                                                
029800 X-TAG-CHECKPOINT   SECTION.                                              
029900                                                                          
030000* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
030100* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
030110                                                                          
030200     PERFORM IMS-CHECKPOINT                                               
030300     MOVE ZERO TO CHKP-ANT                                                
030310                                                                          
030400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
030500     .                                                                    
030600     EJECT                                                                
030700* --- IMS SEKTIONER ---                                                   
030800     SKIP2                                                                
030900 IMS-GU-XXBL-ROT SECTION.                                                 
031000     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-BL ')'                    
031100          DELIMITED BY SIZE INTO SSA1                                     
031200     MOVE '  GE' TO GODK-STATUSKODER                                      
031300     CALL CBLTDLI USING GU XXBL-PCB DLI-IO-AREA SSA1                      
031400     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031700     SKIP2                                                                
031800 IMS-GHNP-XXBL-BL11 SECTION.                                              
031900     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-BL ')'                    
032000          DELIMITED BY SIZE INTO SSA1                                     
032100     STRING 'WLXXBL11  '                                                  
032200          DELIMITED BY SIZE INTO SSA2                                     
032300     MOVE '  GE' TO GODK-STATUSKODER                                      
032400     CALL CBLTDLI USING GHNP XXBL-PCB DLI-IO-AREA SSA1 SSA2               
032500     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
032600     PERFORM IMS-STATUSKONTROLL                                           
032610     .                                                                    
032620     SKIP2                                                                
032630 IMS-GHNP-XXBL-BL11-KVAL SECTION.                                         
032640     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-BL ')'                    
032650          DELIMITED BY SIZE INTO SSA1                                     
032651     STRING 'WLXXBL11(WDGXKEY  =' W-WDGXKEY-X ')'                         
032652          DELIMITED BY SIZE INTO SSA2                                     
032680     MOVE '    ' TO GODK-STATUSKODER                                      
032690     CALL CBLTDLI USING GHNP XXBL-PCB DLI-IO-AREA SSA1 SSA2               
032691     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
032692     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800     EJECT                                                                
032900 IMS-GHU-XXBL-BL11 SECTION.                                               
033000     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT-BL ')'                    
033100          DELIMITED BY SIZE INTO SSA1                                     
033200     STRING 'WLXXBL11(WDGXKEY  =' W-WDGXKEY-X ')'                         
033300          DELIMITED BY SIZE INTO SSA2                                     
033400     MOVE '  GE' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING GHU  XXBL-PCB DLI-IO-AREA SSA1 SSA2               
033600     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     SKIP3                                                                
034000 IMS-DLET-XXBL SECTION.                                                   
034100                                                                          
034200     MOVE '  ' TO GODK-STATUSKODER                                        
034300     CALL CBLTDLI USING DLET XXBL-PCB DLI-IO-AREA                         
034400     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
034500     PERFORM IMS-STATUSKONTROLL                                           
034600                                                                          
034700     IF SEGMENT-FINNS                                                     
034800        MOVE 'WDR5'     TO POSTSUM-FDNAMN                                 
034900        MOVE 'WLXXBL11' TO POSTSUM-DDNAMN2                                
035000        MOVE 'DLET'     TO POSTSUM-TRANSTYP                               
035100        CALL POSTSUM USING POSTSUM-PARM                                   
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 IMS-GU-XXBK-BK11 SECTION.                                                
035600     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT-BK ')'                    
035700          DELIMITED BY SIZE INTO SSA1                                     
035800     STRING 'WLXXBK11(IDLEVNR  =' W-IDLEVNR-X ')'                         
035900          DELIMITED BY SIZE INTO SSA2                                     
036000     MOVE '  GE' TO GODK-STATUSKODER                                      
036100     CALL CBLTDLI USING GU  XXBK-PCB DLI-IO-AREA-2 SSA1 SSA2              
036200     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     EJECT                                                                
036600 IMS-RESTART SECTION.                                                     
036700     SKIP2                                                                
036800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036900     MOVE '  ' TO GODK-STATUSKODER                                        
037000     CALL CBLTDLI USING XRST MSG-PCB                                      
037100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037200                        CHKP-AREA-LENGTH CHKP-AREA                        
037300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037400     PERFORM IMS-STATUSKONTROLL                                           
037500     .                                                                    
037600     EJECT                                                                
037700 IMS-CHECKPOINT SECTION.                                                  
037800     SKIP2                                                                
037900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038000     MOVE '  XD' TO GODK-STATUSKODER                                      
038100     CALL CBLTDLI USING CHKP MSG-PCB                                      
038200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038300                        CHKP-AREA-LENGTH CHKP-AREA                        
038400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038500     PERFORM IMS-STATUSKONTROLL                                           
038600                                                                          
038700     IF IMS-EJ-OK                                                         
038800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
038900       DISPLAY FELTEXT                                                    
039000       CALL FELLOG                                                        
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 IMS-STATUSKONTROLL SECTION.                                              
039500     SKIP2                                                                
039600     SET STATUS-IX TO 1                                                   
039700     SEARCH GODK-STATUS                                                   
039800       AT END                                                             
039900         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
040000         DISPLAY FELTEXT                                                  
040100         CALL FELLOG                                                      
040200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040300         CONTINUE                                                         
040400     END-SEARCH                                                           
040500     .                                                                    
040510     EJECT                                                                
040600*    -COPY WY2000P1                                                       
