000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3713400.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   92/08/25.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER KVITTNINGSTABELLEN PÅ WDR2                             
001010*        TILL FIL W37181.                                                 
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLXXCP (WDR2)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- KVITTNINGSTABELL                                           
002900     SELECT W37181                     ASSIGN TO W37134D1.                
003000     SKIP2                                                                
003100*          --- SORTERINGSFIL                                              
003200     SELECT SORTFIL                    ASSIGN TO W37134DS.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W37181                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004200*01  POST -COPY W37181 -PRE  UT-  -L.                                     
004300     SKIP3                                                                
004400 SD  SORTFIL                                                              
004500     RECORDING      F                                                     
004600     SKIP2                                                                
004700 01  SORTERAD-POST.                                                       
004800*  03  POST -COPY W37181 -PRE SORT-.                                      
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100     SKIP2                                                                
005101                                                                          
005110*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W3713400'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  WS-TEST                     PIC S9(5)   VALUE +11000.                
005600                                                                          
005700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005800     88  END-OF-SORTFIL                      VALUE 'J'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     SKIP2                                                                
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007700     SKIP2                                                                
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  FILLER                       PIC X(16)  VALUE 'WS-SORT-A'.           
008700 01  WS-SORTERA-AREA.                                                     
008800*  03  AREA -COPY W37181  -PRE SORT81-.                                   
008900     EJECT                                                                
009000 01  UT-AREA-START               PIC X(24)   VALUE                        
009100                                 'UT-AREA-START  '.                       
009200     SKIP2                                                                
009300                                                                          
009400*01  AREA -COPY W37181     -PRE UT-                                       
009500 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010200 01  NYCKLAR-TILL-DLI.                                                    
010300     03  W-WDGXKEY-X.                                                     
010400         05   FILLER             PIC X(4)    VALUE '3139'.                
010500         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
010600     03  W-WDGX30-X.                                                      
010700         05  W-IDARTNR-KVITT     PIC S9(9)   VALUE ZERO COMP-3.           
010800         05  W-IDDISTR-KVITT     PIC S9(5)   VALUE ZERO COMP-3.           
010900         05  W-IDTABNR-KVITT     PIC S9(3)   VALUE ZERO COMP-3.           
011000                                                                          
011100     03  W-WDD3B1KY-X.                                                    
011200         05  W-WDD3B1KY          PIC S9(5)   VALUE ZERO COMP-3.           
011300     03  W-IDSKYLT-X.                                                     
011400         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
011500     03  W-IDARTNR-X.                                                     
011600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013400     SKIP3                                                                
013500 01  DLI-IO-AREA.                                                         
013600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013700     SKIP3                                                                
013800     03  WLXXCP11 REDEFINES IO-AREA.                                      
013900*        05  -COPY WDGX3140 -PRE XXCP-                                    
014000     SKIP3                                                                
014100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA1'.         
014200     SKIP3                                                                
014300 01  DLI-IO-AREA1.                                                        
014400     03  IO-AREA1                 PIC X(150)  VALUE SPACE.                
014500     SKIP3                                                                
014600     03  WLBENA11 REDEFINES IO-AREA1.                                     
014700*        05  -COPY WDD311  -PRE  BENA-                                    
014800     SKIP3                                                                
014900     03  WLARTC01 REDEFINES IO-AREA1.                                     
015000*        05  -COPY WDK601  -PRE  ARTC-                                    
015100     EJECT                                                                
015200 LINKAGE SECTION.                                                         
015300                                                                          
015400*01  -COPY W0008  -PRE XXCP-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700*01  -COPY W0008  -PRE BENA-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE ARTC-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING XXCP-PCB BENA-PCB ARTC-PCB.                    
016400     ENTRY 'DLITCBL' USING XXCP-PCB BENA-PCB ARTC-PCB.                    
016500                                                                          
016600     SKIP2                                                                
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     SORT SORTFIL ASCENDING KEY SORT-IDFKNGRP                             
017000                                SORT-IDTABNR                              
017100                                SORT-IDARTNR                              
017200                  INPUT PROCEDURE B-SORT-INPUT                            
017300                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
017400                                                                          
017500     IF SORT-RETURN NOT = 0                                               
017600       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017700       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017800           DELIMITED BY SIZE                                              
017900           INTO FELTEXT-STR                                               
018000       DISPLAY FELTEXT                                                    
018100       PERFORM S99-ABEND                                                  
018200     ELSE                                                                 
018300       PERFORM Z-FINIT                                                    
018400                                                                          
018500       MOVE ZERO TO RETURN-CODE                                           
018600       GOBACK                                                             
018700     END-IF                                                               
018800                                                                          
018900     .                                                                    
019000     EJECT                                                                
019100 A-INIT SECTION.                                                          
019200                                                                          
019300     OPEN OUTPUT W37181                                                   
019400     SKIP2                                                                
019500     ACCEPT DAGENS-DATUM  FROM DATE                                       
019600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019700     .                                                                    
019800     EJECT                                                                
019900 B-SORT-INPUT SECTION.                                                    
020000                                                                          
020100     MOVE SPACE TO SORT81-KDANDRING                                       
020200     PERFORM IMS-GU-ROT                                                   
020300     PERFORM IMS-GET-ARTIKEL-XXCP                                         
020400     PERFORM UNTIL SEGMENT-SAKNAS                                         
020510       MOVE XXCP-3140-IDTABNR     TO SORT81-IDTABNR                       
020600       MOVE XXCP-3140-IDARTNR-BYT TO SORT81-IDARTNR                       
020700       MOVE XXCP-3140-IDARTNR-BYT TO W-IDARTNR                            
020800       PERFORM BA-HAEMTA-BENAEMNING                                       
020900       PERFORM BC-HAEMTA-FUNKTIONSGRUPP                                   
021000       RELEASE SORTERAD-POST FROM WS-SORTERA-AREA                         
021100       PERFORM IMS-GET-ARTIKEL-XXCP                                       
021200     END-PERFORM                                                          
021300     .                                                                    
021400     EJECT                                                                
021500                                                                          
021600 BA-HAEMTA-BENAEMNING SECTION.                                            
021700     SKIP2                                                                
021800     MOVE 'S  ' TO W-IDSKYLT                                              
021900     PERFORM IMS-GET-BENA                                                 
022000     IF SEGMENT-FINNS                                                     
022100       MOVE BENA-TEXT-BEART TO SORT81-BEART-SVE                           
022200     ELSE                                                                 
022300       MOVE SPACE TO SORT81-BEART-SVE                                     
022400     END-IF                                                               
022500     MOVE 'GB ' TO W-IDSKYLT                                              
022600     PERFORM IMS-GET-BENA                                                 
022700     IF SEGMENT-FINNS                                                     
022800       MOVE BENA-TEXT-BEART TO SORT81-BEART-ENG                           
022900     ELSE                                                                 
023000       MOVE SPACE TO SORT81-BEART-ENG                                     
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 BC-HAEMTA-FUNKTIONSGRUPP SECTION.                                        
023600     SKIP2                                                                
023700     PERFORM IMS-GET-ARTNR-WDK6                                           
023800     IF SEGMENT-FINNS                                                     
023900       MOVE ARTC-ART-IDFKNGRP TO SORT81-IDFKNGRP                          
024000     ELSE                                                                 
024100       MOVE +9999         TO SORT81-IDFKNGRP                              
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700 C-SORT-OUTPUT SECTION.                                                   
024800     SKIP2                                                                
024900     PERFORM S32-SORT-RETURN                                              
025000     PERFORM UNTIL END-OF-SORTFIL                                         
025100       MOVE WS-SORTERA-AREA TO UT-AREA                                    
025200       PERFORM S11-SKRIV-W37181                                           
025300       PERFORM S32-SORT-RETURN                                            
025400     END-PERFORM                                                          
025500     .                                                                    
025600     EJECT                                                                
025700 Z-FINIT SECTION.                                                         
025800     CLOSE   W37181                                                       
025900     SKIP2                                                                
026000     MOVE 'S' TO POSTSUM-OPKOD                                            
026100     CALL POSTSUM USING POSTSUM-PARM                                      
026200     .                                                                    
026300     EJECT                                                                
026400 S11-SKRIV-W37181 SECTION.                                                
026500     SKIP2                                                                
026600     WRITE UT-POST FROM UT-AREA                                           
026700                                                                          
026800     MOVE 'KVIT'    TO POSTSUM-TRANSTYP                                   
026900     MOVE 'W37181' TO POSTSUM-FDNAMN                                      
027000     MOVE 'W37134D1' TO POSTSUM-DDNAMN2                                   
027100     CALL POSTSUM USING POSTSUM-PARM                                      
027200     .                                                                    
027300     EJECT                                                                
027400 S32-SORT-RETURN  SECTION.                                                
027500     SKIP2                                                                
027600     RETURN SORTFIL INTO WS-SORTERA-AREA                                  
027700     AT END                                                               
027800         SET END-OF-SORTFIL TO TRUE                                       
027900     .                                                                    
028000     EJECT                                                                
028100 S99-ABEND SECTION.                                                       
028200     SKIP2                                                                
028300     SKIP2                                                                
028400     MOVE 'S' TO POSTSUM-OPKOD                                            
028500     CALL POSTSUM USING POSTSUM-PARM                                      
028600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028700     .                                                                    
028800     EJECT                                                                
028900* --- IMS SEKTIONER ---                                                   
029000     SKIP3                                                                
029100 IMS-GU-ROT      SECTION.                                                 
029200     STRING 'WLXXCP01(WDGXKEY  =' W-WDGXKEY-X ')'                         
029300          DELIMITED BY SIZE INTO SSA1                                     
029400     MOVE '  ' TO GODK-STATUSKODER                                        
029500     CALL CBLTDLI USING GU XXCP-PCB DLI-IO-AREA SSA1                      
029600     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
029700     PERFORM IMS-STATUSKONTROLL                                           
029800     .                                                                    
029900                                                                          
030000 IMS-GET-ARTIKEL-XXCP SECTION.                                            
030300     MOVE 'WLXXCP11 ' TO SSA1                                             
030400                                                                          
030500     MOVE '  GE' TO GODK-STATUSKODER                                      
030600     CALL CBLTDLI USING GNP XXCP-PCB DLI-IO-AREA SSA1                     
030700     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000                                                                          
031100 IMS-GET-BENA SECTION.                                                    
031200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
031300          DELIMITED BY SIZE INTO SSA1                                     
031400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031500          DELIMITED BY SIZE INTO SSA2                                     
031600     MOVE '  GE' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
031800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     .                                                                    
032100     EJECT                                                                
032200 IMS-GET-ARTNR-WDK6 SECTION.                                              
032300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
032400          DELIMITED BY SIZE INTO SSA1                                     
032510     MOVE '  GE' TO GODK-STATUSKODER                                      
032600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
032700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000                                                                          
033100 IMS-STATUSKONTROLL SECTION.                                              
033200     SKIP2                                                                
033300     SET STATUS-IX TO 1                                                   
033400     SEARCH GODK-STATUS                                                   
033500       AT END                                                             
033600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
033700         DISPLAY FELTEXT                                                  
033800         CALL FELLOG                                                      
033900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034000         CONTINUE                                                         
034100     END-SEARCH                                                           
034200     .                                                                    
