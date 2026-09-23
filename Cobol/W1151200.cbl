000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1151200.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   92/11/19.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ERSÄTTNINGSBEVAKNING BASLAGER                                    
001100*                                                                         
001200*        PROGRAMMET LÄSTER OCH DELEATER HTR 1158                          
001300*        SOM LÄGGS UPP AV ERSÄTTNINGSBEVAKNINGEN W11120 VID NY            
001400*                      EK > 10 SAMT PÅ                                    
001500*                      BILD 1113 VID NY EK > 10                           
001600*                                                                         
001700*        ARTIKEL MED KDERS > 10 RENSAS BASLAGER                           
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WLARTG (WDD2)                              
002000*                              WLXXCW (WDG3)                              
002100*        PROGRAMMET LÄSER      WLXXAP (WDG2)                              
002200*                              WLARTC (WDK6)                              
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700*                                                                         
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(8)    VALUE 'W1151200'.            
004000 01  CHKP-VAR.                                                            
004100 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004200 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004300 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004400 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004500 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004600 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005000 77  WS-SPAR-IDPROJ              PIC X(4)    VALUE SPACE.                 
005100                                                                          
005200 01  SW-ARTIKEL-KLAR             PIC X       VALUE 'J'.                   
005300     88  ARTIKEL-EJ-KLAR         VALUE 'N'.                               
005400                                                                          
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900*************                                                             
006000*01  -COPY WWPRODSL                                                       
006100     EJECT                                                                
006200************* POSTSUM                                                     
006300*01  -COPY W0005                 -PRE POSTSUM-                            
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007200                                                                          
007300 01  NYCKLAR-TILL-DLI.                                                    
007400     03  W-IDARTNR-X.                                                     
007500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007600     03  W-1123-KEY-X.                                                    
007700         05  FILLER              PIC X(4)    VALUE '1123'.                
007800         05  W-1123-KDPRODSL     PIC S9(3)   VALUE ZERO COMP-3.           
007900         05  W-1123-IDPROJ       PIC X(4)    VALUE SPACE.                 
008000         05  FILLER              PIC X(20)   VALUE LOW-VALUE.             
008100     03  W-1157-KEY-X.                                                    
008200         05  FILLER              PIC X(4)    VALUE '1157'.                
008300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     88  IMS-EJ-OK                           VALUE 'XD'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
010500     SKIP3                                                                
010600     03  WLARTG01 REDEFINES IO-AREA.                                      
010700*        05  -COPY WDD201     -PRE ARTG01-                                
010800     EJECT                                                                
010900     03  WLARTG11 REDEFINES IO-AREA.                                      
011000*        05  -COPY WDD211     -PRE ARTG11-                                
011100     EJECT                                                                
011200     03  WLARTC01 REDEFINES IO-AREA.                                      
011300*        05  -COPY WDK601                                                 
011400     EJECT                                                                
011500     03  WLXXAP12 REDEFINES IO-AREA.                                      
011600*        05  -COPY WDGX1126   -PRE XXAP-                                  
011700     EJECT                                                                
011800 01  DLI-IO-AREA-2.                                                       
011900     03  IO-AREA-2               PIC X(100)  VALUE SPACE.                 
012000     SKIP3                                                                
012100     03  WLXXCW01 REDEFINES IO-AREA-2.                                    
012200*        05  -COPY WDGX1158   -PRE XXCW-                                  
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600*01  -COPY W0009   -PRE MSG-                                              
012700     EJECT                                                                
012800*01  -COPY W0008  -PRE ARTG-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100*01  -COPY W0008  -PRE ARTC-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400*01  -COPY W0008  -PRE XXAP-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE XXCW-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB ARTG-PCB ARTC-PCB                      
014100                           XXAP-PCB XXCW-PCB.                             
014200     ENTRY 'DLITCBL' USING MSG-PCB ARTG-PCB ARTC-PCB                      
014300                           XXAP-PCB XXCW-PCB.                             
014400                                                                          
014500     PERFORM A-INIT                                                       
014600                                                                          
014700     PERFORM IMS-GET-XXCW01                                               
014800     IF SEGMENT-FINNS                                                     
014900        PERFORM IMS-GET-XXCW11                                            
015000        PERFORM S01-POSTSUM                                               
015100        PERFORM UNTIL SEGMENT-SAKNAS                                      
015200                                                                          
015300           IF XXCW-1158-KDERS > 10                                        
015400              MOVE XXCW-1158-IDARTNR TO WS-IDARTNR                        
015500              PERFORM B-BEHANDLA-ERSATTNING                               
015600           END-IF                                                         
015700                                                                          
015800           PERFORM IMS-DLET-XXCW                                          
015900           ADD +1 TO CHKP-ANT                                             
016000                                                                          
016100           IF CHKP-ANT > CHKP-MAX                                         
016200              PERFORM X-TAG-CHECKPOINT                                    
016300           END-IF                                                         
016400                                                                          
016500           PERFORM IMS-GET-XXCW11                                         
016600           PERFORM S01-POSTSUM                                            
016700        END-PERFORM                                                       
016800     END-IF                                                               
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     PERFORM IMS-RESTART                                                  
017900     MOVE ZERO TO CHKP-ANT                                                
018000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018100     .                                                                    
018200     EJECT                                                                
018300 B-BEHANDLA-ERSATTNING SECTION.                                           
018400*****************************************************************         
018500* KONTROLL TIBASL  = 1 ARTIKEL PÅ KÖ TILL BASLAGER-BEREDARE     *         
018600*                  > 1 ARTIKEL ÄR MARKNADSKNUTEN                *         
018700*****************************************************************         
018800                                                                          
018900     MOVE WS-IDARTNR TO W-IDARTNR                                         
019000     PERFORM IMS-GET-ARTG01                                               
019100     IF SEGMENT-FINNS                                                     
019200        MOVE ARTG01-ART-IDPROJ TO WS-SPAR-IDPROJ                          
019300        IF ARTG01-ART-DABASL = +1                                         
019400          MOVE ZERO TO ARTG01-ART-DABASL                                  
019500          PERFORM IMS-REPL-ARTG                                           
019600          ADD +1 TO CHKP-ANT                                              
019700        ELSE                                                              
019800           IF ARTG01-ART-DABASL > +1                                      
019900              PERFORM BA-KOLLA-ORDERDATUM                                 
020000              IF ARTIKEL-EJ-KLAR                                          
020100                 PERFORM BB-RENSA-FRAN-BASLAGER                           
020200              END-IF                                                      
020300           END-IF                                                         
020400        END-IF                                                            
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 BA-KOLLA-ORDERDATUM SECTION.                                             
020900*****************************************************************         
021000* KONTROLL DATUM FÖR ORDERGENERING FÖR SAMTLIGA BASLAGER-MARKN. *         
021100* OM ORDER ÄR GENERAD FÖR SAMTLIGA MARKNADER BEHANDLAS INTE     *         
021200* ARTIKEL.                                                      *         
021300* TIBASORD SÄTTS TILL +1 BILD 1156 ORDERGENERING                *         
021400*          DATUM SÄTTS I W11540 NÄR ORDERFIL SKAPAS             *         
021500*****************************************************************         
021600                                                                          
021700     MOVE JA TO SW-ARTIKEL-KLAR                                           
021800     MOVE WS-IDARTNR TO W-IDARTNR                                         
021900     PERFORM IMS-GET-ARTC01                                               
022000     IF SEGMENT-FINNS                                                     
022100        MOVE ART-KDPRODSL TO TEST-KDPRODSL                                
022200        IF KDPRODSL-VOLVO-UTAN-EMB                                        
022300           MOVE +11            TO W-1123-KDPRODSL                         
022400           MOVE WS-SPAR-IDPROJ TO W-1123-IDPROJ                           
022500           PERFORM IMS-GET-XXAP01                                         
022600           IF SEGMENT-FINNS                                               
022700              PERFORM IMS-GET-XXAP12                                      
022800              PERFORM UNTIL SEGMENT-SAKNAS                                
022900                 IF XXAP-1126-TIBASORD > +1                               
023000                    CONTINUE                                              
023100                 ELSE                                                     
023200                    MOVE NEJ TO SW-ARTIKEL-KLAR                           
023300                 END-IF                                                   
023400                 PERFORM IMS-GET-XXAP12                                   
023500              END-PERFORM                                                 
023600           END-IF                                                         
023700        END-IF                                                            
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 BB-RENSA-FRAN-BASLAGER SECTION.                                          
024200*****************************************************************         
024300* - SAMTLIGA MARKNADSSEGMENT ARTG11 UNDER ARTIKEL RIVS          *         
024400* - ARTIKEL ARTG01 UPPDATERAS                                   *         
024500*****************************************************************         
024600                                                                          
024700     MOVE WS-IDARTNR TO W-IDARTNR                                         
024800     PERFORM IMS-GET-ARTG01                                               
024900     MOVE ZERO TO ARTG01-ART-DABASL                                       
025000                  ARTG01-ART-TISTOMREG                                    
025100                  ARTG01-ART-KVBASL                                       
025200     PERFORM IMS-REPL-ARTG                                                
025300     ADD +1 TO CHKP-ANT                                                   
025400     PERFORM IMS-GET-ARTG11                                               
025500     PERFORM UNTIL SEGMENT-SAKNAS                                         
025600        PERFORM IMS-DLET-ARTG                                             
025700        ADD +1 TO CHKP-ANT                                                
025800        PERFORM IMS-GET-ARTG11                                            
025900     END-PERFORM                                                          
026000     .                                                                    
026100     EJECT                                                                
026200 Z-FINIT SECTION.                                                         
026300                                                                          
026400     MOVE 'S' TO POSTSUM-OPKOD                                            
026500     CALL POSTSUM USING POSTSUM-PARM                                      
026600     .                                                                    
026700     EJECT                                                                
026800 X-TAG-CHECKPOINT SECTION.                                                
026900                                                                          
027000     PERFORM IMS-CHECKPOINT                                               
027100     MOVE ZERO TO CHKP-ANT                                                
027200     PERFORM IMS-GET-XXCW01                                               
027300     .                                                                    
027400     EJECT                                                                
027500 S01-POSTSUM SECTION.                                                     
027600                                                                          
027700     MOVE 'W1151200' TO POSTSUM-FDNAMN                                    
027800     MOVE '1158    ' TO POSTSUM-DDNAMN2                                   
027900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300* --- IMS SEKTIONER ---                                                   
028400                                                                          
028500 IMS-GET-ARTG01 SECTION.                                                  
028600     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
028700          DELIMITED BY SIZE INTO SSA1                                     
028800     MOVE '  GE' TO GODK-STATUSKODER                                      
028900     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA SSA1                     
029000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
029100     PERFORM IMS-STATUSKONTROLL                                           
029200     .                                                                    
029300     SKIP3                                                                
029400 IMS-GET-ARTG11 SECTION.                                                  
029500     MOVE 'WLARTG11 ' TO SSA1                                             
029600     MOVE '  GE' TO GODK-STATUSKODER                                      
029700     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA SSA1                    
029800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
029900     PERFORM IMS-STATUSKONTROLL                                           
030000     .                                                                    
030100     SKIP3                                                                
030200 IMS-REPL-ARTG SECTION.                                                   
030300     MOVE '  ' TO GODK-STATUSKODER                                        
030400     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA                         
030500     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     SKIP3                                                                
030900 IMS-DLET-ARTG SECTION.                                                   
031000     MOVE '  ' TO GODK-STATUSKODER                                        
031100     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA                         
031200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031500     EJECT                                                                
031600 IMS-GET-ARTC01 SECTION.                                                  
031700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031800          DELIMITED BY SIZE INTO SSA1                                     
031900     MOVE '  GE' TO GODK-STATUSKODER                                      
032000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
032100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032400     SKIP3                                                                
032500 IMS-GET-XXAP01 SECTION.                                                  
032600     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     MOVE '  GE' TO GODK-STATUSKODER                                      
032900     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA SSA1                      
033000     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     SKIP3                                                                
033400 IMS-GET-XXAP12 SECTION.                                                  
033500     MOVE 'WLXXAP12 ' TO SSA1                                             
033600     MOVE '  GE' TO GODK-STATUSKODER                                      
033700     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA SSA1                     
033800     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     EJECT                                                                
034200 IMS-GET-XXCW01 SECTION.                                                  
034300     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
034400          DELIMITED BY SIZE INTO SSA1                                     
034500     MOVE '  GE' TO GODK-STATUSKODER                                      
034600     CALL CBLTDLI USING GU XXCW-PCB DLI-IO-AREA-2 SSA1                    
034700     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
034800     PERFORM IMS-STATUSKONTROLL                                           
034900     .                                                                    
035000     SKIP3                                                                
035100 IMS-GET-XXCW11 SECTION.                                                  
035200     MOVE 'WLXXCW11 ' TO SSA1                                             
035300     MOVE '  GE' TO GODK-STATUSKODER                                      
035400     CALL CBLTDLI USING GHNP XXCW-PCB DLI-IO-AREA-2 SSA1                  
035500     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     SKIP3                                                                
035900 IMS-DLET-XXCW SECTION.                                                   
036000     MOVE '  ' TO GODK-STATUSKODER                                        
036100     CALL CBLTDLI USING DLET XXCW-PCB DLI-IO-AREA-2                       
036200     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
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
037600     SKIP3                                                                
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
039900         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
040000               DELIMITED BY SIZE INTO FELTEXT-STR                         
040100         DISPLAY FELTEXT                                                  
040200         CALL FELLOG                                                      
040300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040400         CONTINUE                                                         
040500     END-SEARCH                                                           
040600     .                                                                    
040700     EJECT                                                                
