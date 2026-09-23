000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1227200.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   91/03/04.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL W12251 MED ARTIKLAR SOM RENSAS ARTREG.                 
001100*        - OM ARTIKEL INGÅR SOM RAD PÅ WDJ1 UPPDATERAS RADEN.             
001200*        - KONTROLL OM ARTIKEL ÄR 52-MÄRKT. FINNS DEN SOM STRUKTUW        
001300*          RENSAS DEN FRÅN WDJ1 OCH SPARAS PÅ HISTORIKFIL.                
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ARTIKLAR SOM RENSAS ARTREG                                 
002600     SELECT INFIL                      ASSIGN TO W12272D1.                
002700     SKIP3                                                                
002800*          --- HISTORIKFIL RASA                                           
002900     SELECT UTFIL                      ASSIGN TO W12272D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  INFIL                                                                
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  -COPY W12251          -L.                                            
004100     EJECT                                                                
004200 FD  UTFIL                                                                
004300     LABEL RECORD    STANDARD                                             
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  POST  -COPY W11251       -PRE UT01-   -L.                            
004900     SKIP2                                                                
005000*01  POST  -COPY W11252       -PRE UT11-   -L.                            
005200     SKIP2                                                                
005300*01  POST  -COPY W11253       -PRE UT22-   -L.                            
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700     SKIP2                                                                
005701                                                                          
005710*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W1227200'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  W-DLET-WDJ101               PIC 9(7)    VALUE ZERO.                  
006000 77  W-REPL-WDJ111               PIC 9(7)    VALUE ZERO.                  
006100                                                                          
006200 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006300     88  END-OF-INFIL                        VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007100     SKIP2                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005      -PRE  POSTSUM-                                      
007600     EJECT                                                                
007700*01  -COPY WDATAREA                                                       
007900     EJECT                                                                
008000 01  IN-AREA-START             PIC X(24)   VALUE                          
008100                                 'IN-AREA-START  '.                       
008200     SKIP2                                                                
008300                                                                          
008400*01  AREA -COPY W12251         -PRE IN-                                   
008600     EJECT                                                                
008700 01  UT-AREA-START             PIC X(24)   VALUE                          
008800                                 'UT-AREA-START  '.                       
008900     SKIP2                                                                
009000 01  UT-AREA                   PIC X(225)  VALUE SPACE.                   
009100                                                                          
009200*01  AREA -COPY W11251         -PRE UT01- -RED UT-AREA.                   
009400     EJECT                                                                
009500*01  AREA -COPY W11252         -PRE UT11- -RED UT-AREA.                   
009700     EJECT                                                                
009800*01  AREA -COPY W11253         -PRE UT22- -RED UT-AREA.                   
010000     EJECT                                                                
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010600     03  W-WDJ1CSEQ-X.                                                    
010700         05  W-IDLEVNR-S         PIC X(5)    VALUE SPACE.                 
010800         05  W-BELEVART-S        PIC X(30)   VALUE SPACE.                 
010900         05  W-IDARTNR-S         PIC S9(9)   VALUE ZERO COMP-3.           
011000     03  W-IDARTNR-X.                                                     
011100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  W-WDJ111KY-X.                                                    
011300         05  W-KDSTRRAD          PIC X       VALUE SPACE.                 
011400         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
011500     SKIP2                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  BASEN-SLUT                          VALUE 'GB'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     SKIP2                                                                
012200 01  GODK-STATUSKODER.                                                    
012300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNKTIONSKODER                                               
012900*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013400     SKIP3                                                                
013500 01  DLI-IO-AREA.                                                         
013600     03  IO-AREA                 PIC X(350)  VALUE SPACE.                 
013700     EJECT                                                                
013800     03  WLSATB01 REDEFINES IO-AREA.                                      
013900*        05  -COPY WDJ101     -PRE SATB-                                  
014100     EJECT                                                                
014200     03  WLSATB11 REDEFINES IO-AREA.                                      
014300*        05  -COPY WDJ111     -PRE SATB-                                  
014500     EJECT                                                                
014600     03  WLSATB22 REDEFINES IO-AREA.                                      
014700*        05  -COPY WDJ122     -PRE SATB-                                  
014900     EJECT                                                                
015000     03  WDJ1CSEQ REDEFINES IO-AREA.                                      
015100*        05  -COPY WDJ111     -PRE SATE-                                  
015300*        05  -COPY WDJ101     -PRE SATE-                                  
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700                                                                          
015800     EJECT                                                                
015900*01  -COPY W0008      -PRE SATB-                                          
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008      -PRE SATE-                                          
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700 PROCEDURE DIVISION  USING SATB-PCB SATE-PCB.                             
016800     ENTRY 'DLITCBL' USING SATB-PCB SATE-PCB.                             
016900     SKIP2                                                                
017000     PERFORM A-INIT                                                       
017100                                                                          
017200     PERFORM S01-LAES-INFIL                                               
017300     PERFORM UNTIL END-OF-INFIL                                           
017400       PERFORM B-KOLLA-RADER                                              
017500       IF IN-KDERS-UTG = 52                                               
017600          PERFORM C-KOLLA-STRUKTUR                                        
017700       END-IF                                                             
017800       PERFORM S01-LAES-INFIL                                             
017900     END-PERFORM                                                          
018000                                                                          
018100     PERFORM Z-FINIT                                                      
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     OPEN INPUT  INFIL                                                    
019000     OPEN OUTPUT UTFIL                                                    
019100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019200     .                                                                    
019300     EJECT                                                                
019400 B-KOLLA-RADER SECTION.                                                   
019500*                                                                         
019600******************************************************************        
019700* OM RENSAD ARTIKEL INGÅR SOM RAD UPPDATERAS RADEN MED BENÄMNING          
019800* SORT OCH HOMONYMKOD. STRUKTURTYP BLANKAS.                               
019900******************************************************************        
020000*                                                                         
020100     MOVE IN-IDARTNR TO W-IDARTNR-S                                       
020200     MOVE SPACE      TO W-IDLEVNR-S                                       
020300     MOVE SPACE      TO W-BELEVART-S                                      
020400     PERFORM IMS-GU-SATB11-CSEQ                                           
020500                                                                          
020600     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
020700                                                                          
020800        MOVE SATE-STR-IDARTNR   TO W-IDARTNR                              
020900        MOVE SATE-RAD-IDRADNR   TO W-IDRADNR                              
021000        MOVE SATE-RAD-KDSTRRAD  TO W-KDSTRRAD                             
021100                                                                          
021200        PERFORM IMS-GHU-SATB11                                            
021300                                                                          
021400        IF SEGMENT-FINNS                                                  
021500           MOVE IN-BEART     TO SATB-RAD-BEART-SVE                        
021600           MOVE IN-KDHOMONYM TO SATB-RAD-KDBENHOM                         
021700           MOVE IN-KDSORT    TO SATB-RAD-KDSORT                           
021800           MOVE SPACE        TO SATB-RAD-IDSTRTYP                         
021900           PERFORM IMS-REPL-SATB                                          
                 ADD 1 TO W-REPL-WDJ111                                         
022000        END-IF                                                            
022100                                                                          
022200        PERFORM IMS-GN-SATB11-CSEQ                                        
022300     END-PERFORM                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 C-KOLLA-STRUKTUR SECTION.                                                
022700*                                                                         
022800******************************************************************        
022900* OM RENSAD ARTIKEL ÄR 52-MÄRKT OCH FINNS SOM STRUKTUR RENSAS DEN         
023000* FRÅN WDJ1 OCH SPARAS PÅ HISTORIKFIL.                                    
023100* 52-MÄRKT ARTIKEL RENSAS EFTER 6 MÅN FRÅN ARTREG OCH 'SKALAS' EJ         
023200******************************************************************        
023300*                                                                         
023400     MOVE IN-IDARTNR TO W-IDARTNR                                         
023500     PERFORM IMS-GET-SATB01                                               
023600     IF SEGMENT-FINNS                                                     
023700        MOVE '001'                 TO UT01-IDPTYP                         
023800        MOVE SATB-STR-IDARTNR      TO UT01-IDARTNR                        
023900        MOVE IN-BEART              TO UT01-BEART-SVE                      
024000        MOVE SATB-STR-FLEXFORP     TO UT01-FLEXFORP                       
024100        MOVE SATB-STR-FLFORPQ      TO UT01-FLFORPQ                        
024200        MOVE SATB-STR-IDLEVNR      TO UT01-IDLEVNR                        
024300        MOVE SATB-STR-IDSTRTYP     TO UT01-IDSTRTYP                       
024400        MOVE SATB-STR-IDFKNGRP     TO UT01-IDFKNGRP                       
024500        MOVE SATB-STR-IDTSPEC      TO UT01-IDTSPEC                        
024600        MOVE SATB-STR-IDUSER       TO UT01-IDUSER                         
024700        MOVE IN-KDHOMONYM          TO UT01-KDBENHOM                       
024800        MOVE SATB-STR-KDPRODSL     TO UT01-KDPRODSL                       
024900        MOVE SATB-STR-TIBORT       TO UT01-TIBORT                         
025000        MOVE SATB-STR-TIREGDAT     TO UT01-TIREGDAT                       
025100        MOVE SATB-STR-TIUPPDAT     TO UT01-TIUPPDAT                       
025200        MOVE SATB-STR-TESTRNOT(1)  TO UT01-TESTRNOT(1)                    
025300        MOVE SATB-STR-TESTRNOT(2)  TO UT01-TESTRNOT(2)                    
025400                                                                          
025500        PERFORM S02-SKRIV-UTPOST                                          
025600        PERFORM IMS-GET-SATB11                                            
025700                                                                          
025800        PERFORM UNTIL SEGMENT-SAKNAS                                      
025900           MOVE '011'              TO UT11-IDPTYP                         
026000           MOVE SATB-RAD-KDSTRRAD  TO UT11-KDSTRRAD                       
026100           MOVE SATB-RAD-IDRADNR   TO UT11-IDRADNR                        
026200           MOVE SATB-RAD-IDLEVNR   TO UT11-IDLEVNR                        
026300           MOVE SATB-RAD-BELEVART  TO UT11-BELEVART                       
026400           MOVE SATB-RAD-IDARTNR   TO UT11-IDARTNR                        
026500           MOVE SATB-RAD-BEART-SVE TO UT11-BEART-SVE                      
026600           MOVE SATB-RAD-IDAO-STA  TO UT11-IDAO-STA                       
026700           MOVE SATB-RAD-IDAO-STO  TO UT11-IDAO-STO                       
026800           MOVE SATB-RAD-IDSTRTYP  TO UT11-IDSTRTYP                       
026900           MOVE SATB-RAD-KDBENHOM  TO UT11-KDBENHOM                       
027000           MOVE SATB-RAD-KDISATS   TO UT11-KDISATS                        
027100           MOVE SATB-RAD-KDSORT    TO UT11-KDSORT                         
027200           MOVE SATB-RAD-REANTPSA  TO UT11-REANTPSA                       
027300           MOVE SATB-RAD-TIREGDAT  TO UT11-TIREGDAT                       
027400           MOVE SATB-RAD-TISTADAT  TO UT11-TISTADAT                       
027500           MOVE SATB-RAD-TISTODAT  TO UT11-TISTODAT                       
027600                                                                          
027700           PERFORM S02-SKRIV-UTPOST                                       
027800                                                                          
027900           MOVE SATB-RAD-KDSTRRAD  TO W-KDSTRRAD                          
028000           MOVE SATB-RAD-IDRADNR   TO W-IDRADNR                           
028100           PERFORM IMS-GET-SATB22                                         
028200           IF SEGMENT-FINNS                                               
028300              MOVE '022'                TO UT22-IDPTYP                    
028400              MOVE SATB-NOT-IDSTRNOT    TO UT22-IDSTRNOT                  
028500              MOVE SATB-NOT-TESTRNOT(1) TO UT22-TESTRNOT(1)               
028600              MOVE SATB-NOT-TESTRNOT(2) TO UT22-TESTRNOT(2)               
028700              PERFORM S02-SKRIV-UTPOST                                    
028800           END-IF                                                         
028900           PERFORM IMS-GET-SATB11                                         
029000        END-PERFORM                                                       
029100        PERFORM IMS-GET-SATB01                                            
029200        DISPLAY SATB-STR-IDARTNR ' 52-MÄRKT RENSAD FRÅN RASA'             
029300        PERFORM IMS-DLET-SATB                                             
              ADD 1 TO W-DLET-WDJ101                                            
029400     END-IF                                                               
029500                                                                          
029600     .                                                                    
029700     EJECT                                                                
029800 Z-FINIT SECTION.                                                         
           DISPLAY 'ANTAL BORTTAGNA WDJ101: ' W-DLET-WDJ101                     
           DISPLAY 'ANTAL ÄNDRADE WDJ111: ' W-REPL-WDJ111                       
029900     CLOSE INFIL                                                          
030000           UTFIL                                                          
030100     SKIP2                                                                
030200     MOVE 'S' TO POSTSUM-OPKOD                                            
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     EJECT                                                                
030600 S01-LAES-INFIL  SECTION.                                                 
030700     SKIP2                                                                
030800     READ INFIL INTO IN-AREA                                              
030900     AT END                                                               
031000        SET END-OF-INFIL TO TRUE                                          
031100                                                                          
031200     NOT AT END                                                           
031300        MOVE SPACE TO POSTSUM-TRANSTYP                                    
031400        MOVE 'W12251' TO POSTSUM-FDNAMN                                   
031500        MOVE 'W12272D1' TO POSTSUM-DDNAMN2                                
031600        CALL POSTSUM USING POSTSUM-PARM                                   
031700     END-READ                                                             
031800     .                                                                    
031900     EJECT                                                                
032000 S02-SKRIV-UTPOST SECTION.                                                
032100     SKIP2                                                                
032200     EVALUATE TRUE                                                        
032300       WHEN UT01-IDPTYP = '001'                                           
032400          WRITE UT01-POST FROM UT01-W11251                                
032500       WHEN UT01-IDPTYP = '011'                                           
032600          WRITE UT11-POST FROM UT11-W11252                                
032700       WHEN UT01-IDPTYP = '022'                                           
032800          WRITE UT22-POST FROM UT22-W11253                                
032900     END-EVALUATE                                                         
033000                                                                          
033100     MOVE UT01-IDPTYP   TO POSTSUM-TRANSTYP                               
033200     MOVE 'UTFIL'       TO POSTSUM-FDNAMN                                 
033300     MOVE 'W12272D2'    TO POSTSUM-DDNAMN2                                
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     .                                                                    
033600     EJECT                                                                
033700* --- IMS SEKTIONER ---                                                   
033800*                                                                         
033900 IMS-GET-SATB01 SECTION.                                                  
034000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
034100         DELIMITED BY SIZE INTO SSA1                                      
034200     MOVE '  GE' TO GODK-STATUSKODER                                      
034300     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
034400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
034500     PERFORM IMS-STATUSKONTROLL                                           
034600     .                                                                    
034700     SKIP3                                                                
034800 IMS-GET-SATB11 SECTION.                                                  
034900     MOVE 'WLSATB11 ' TO SSA1                                             
035000     MOVE '  GE' TO GODK-STATUSKODER                                      
035100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
035200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     SKIP3                                                                
035600 IMS-GET-SATB22 SECTION.                                                  
035700     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
035800        DELIMITED BY SIZE INTO SSA1                                       
035900     MOVE 'WLSATB22 ' TO SSA2                                             
036000     MOVE '  GE' TO GODK-STATUSKODER                                      
036100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
036200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     .                                                                    
036500     EJECT                                                                
036600 IMS-GU-SATB11-CSEQ SECTION.                                              
036700     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
036800        DELIMITED BY SIZE INTO SSA1                                       
036900     MOVE 'WLSATB01 ' TO SSA2                                             
037000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
037100     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
037200     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
037300     PERFORM IMS-STATUSKONTROLL                                           
037400     .                                                                    
037500     SKIP3                                                                
037600 IMS-GN-SATB11-CSEQ SECTION.                                              
037700     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
037800        DELIMITED BY SIZE INTO SSA1                                       
037900     MOVE 'WLSATB01 ' TO SSA2                                             
038000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
038100     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
038200     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     SKIP3                                                                
038600 IMS-GHU-SATB11 SECTION.                                                  
038700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
038800        DELIMITED BY SIZE INTO SSA1                                       
038900     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
039000        DELIMITED BY SIZE INTO SSA2                                       
039100     MOVE '  GE' TO GODK-STATUSKODER                                      
039200     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1 SSA2                
039300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     EJECT                                                                
039700 IMS-REPL-SATB SECTION.                                                   
039800     MOVE '  ' TO GODK-STATUSKODER                                        
039900     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
040000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
040100     PERFORM IMS-STATUSKONTROLL                                           
040200     .                                                                    
040300     SKIP3                                                                
040400 IMS-DLET-SATB SECTION.                                                   
040500     MOVE '  ' TO GODK-STATUSKODER                                        
040600     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
040700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000     SKIP3                                                                
041100 IMS-STATUSKONTROLL SECTION.                                              
041200     SKIP2                                                                
041300     SET STATUS-IX TO 1                                                   
041400     SEARCH GODK-STATUS                                                   
041500         AT END CALL FELLOG                                               
041600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
041700     END-SEARCH                                                           
041800     .                                                                    
