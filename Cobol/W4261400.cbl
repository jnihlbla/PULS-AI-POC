000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4261400.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   93/10/21.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKRIVER NER ALLA ERSATTA ARTIKLAR PÅ EN FIL                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
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
002800*          --- FIL MED ERSATTA ARTIKLAR                                   
002900     SELECT W42615                     ASSIGN TO W42614D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W42615                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  POST -COPY W4261501 -PRE  UT-  -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4261400'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500                                                                          
005600*01  -COPY WWPRODSL                                                       
005700                                                                          
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     SKIP2                                                                
006500*    --- PARAMETRAR TILL ABEND                                            
006600                                                                          
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  UT-AREA-START               PIC X(24)   VALUE                        
007900                                 'UT-AREA-START  '.                       
008000     SKIP2                                                                
008100                                                                          
008200*01  AREA -COPY W4261501     -PRE UT-                                     
008300     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-IDARTNR-X.                                                     
009100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009200     03  W-IDSKYLT-KEY-X.                                                 
009300         05  W-IDSKYLT-KEY       PIC X(3)    VALUE SPACE.                 
009400     SKIP2                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700 01  SSA3                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011400     SKIP3                                                                
011500 01  DLI-IO-AREA.                                                         
011600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
011700     SKIP3                                                                
011800     03  WLARTC01 REDEFINES IO-AREA.                                      
011900*        05  -COPY WDK601  -PRE ARTC-                                     
012000     SKIP3                                                                
012100     03  WLARTC11 REDEFINES IO-AREA.                                      
012200*        05  -COPY WDK611  -PRE ARTC-                                     
012300     SKIP3                                                                
012400     03  WLBENA11 REDEFINES IO-AREA.                                      
012500*        05  -COPY WDD311  -PRE BENA11-                                   
012600     SKIP3                                                                
012700     03  W6KVAH01 REDEFINES IO-AREA.                                      
012800*        05  -COPY W6D201  -PRE KVAH01-                                   
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100                                                                          
013200     EJECT                                                                
013300*01  -COPY W0008  -PRE ARTC-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600*01  -COPY W0008  -PRE BENA-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE KVAH-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING ARTC-PCB BENA-PCB KVAH-PCB.                    
014300     ENTRY 'DLITCBL' USING ARTC-PCB BENA-PCB KVAH-PCB.                    
014400                                                                          
014500     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014700     PERFORM S01-NOLLA-UTAREA                                             
014800                                                                          
014900     PERFORM IMS-GN-ARTC-WDK601                                           
015000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
015100       MOVE ARTC-ART-IDARTNR TO W-IDARTNR                                 
015200       IF ARTC-ART-TIERSDAT > ZERO                                        
015300         MOVE ARTC-ART-IDARTNR    TO  UT-IDARTNR                          
015400         MOVE ARTC-ART-IDFKNGRP   TO  UT-IDFKNGRP                         
015500         MOVE ARTC-ART-TIERSDAT   TO  UT-TIERSDAT                         
015600         MOVE ARTC-ART-KDPRODSL   TO TEST-KDPRODSL                        
015700         IF KDPRODSL-VOLVO-BIMA                                           
015800           MOVE 'S  ' TO W-IDSKYLT-KEY                                    
015900         ELSE                                                             
016000           MOVE 'GB ' TO W-IDSKYLT-KEY                                    
016100         END-IF                                                           
016200         PERFORM IMS-GNP-ARTC-WDK611                                      
016300         IF SEGMENT-FINNS                                                 
016400           MOVE ARTC-CLAG-KDERS TO UT-KDERS                               
016500           PERFORM IMS-GU-BENA-WDD311                                     
016600           IF SEGMENT-FINNS                                               
016700             MOVE BENA11-TEXT-BEART TO UT-BEART                           
016800             PERFORM IMS-GU-KVAH-W6D201                                   
016900             IF SEGMENT-FINNS                                             
017000               MOVE KVAH01-ART-ADKVAULG TO UT-ADKVAULG                    
017100               MOVE KVAH01-ART-KDKVAKTL TO UT-KDKVAKTL                    
017200             END-IF                                                       
017300           END-IF                                                         
017400         END-IF                                                           
017500         PERFORM S11-SKRIV-W42615                                         
017600         PERFORM S01-NOLLA-UTAREA                                         
017700       END-IF                                                             
017800     PERFORM IMS-GN-ARTC-WDK601                                           
017900     END-PERFORM                                                          
018000                                                                          
018100                                                                          
018200     PERFORM Z-FINIT                                                      
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     OPEN OUTPUT W42615                                                   
019100     SKIP2                                                                
019200     ACCEPT DAGENS-DATUM  FROM DATE                                       
019300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019400     .                                                                    
019500     EJECT                                                                
019600 Z-FINIT SECTION.                                                         
019700     CLOSE W42615                                                         
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300 S01-NOLLA-UTAREA SECTION.                                                
020400                                                                          
020500     MOVE ZERO                  TO  UT-IDARTNR                            
020600     MOVE SPACE                 TO  UT-BEART                              
020700     MOVE ZERO                  TO  UT-IDFKNGRP                           
020800     MOVE SPACE                 TO  UT-KDKVAKTL                           
020900                                    UT-ADKVAULG                           
021000     MOVE ZERO                  TO  UT-TIERSDAT                           
021100                                    UT-KDERS                              
021200     .                                                                    
021300     EJECT                                                                
021400 S11-SKRIV-W42615 SECTION.                                                
021500     SKIP2                                                                
021600     WRITE UT-POST FROM UT-AREA                                           
021700                                                                          
021800     MOVE 'W42615' TO POSTSUM-FDNAMN                                      
021900     MOVE 'W42614D1' TO POSTSUM-DDNAMN2                                   
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300* --- IMS SEKTIONER ---                                                   
022400     SKIP3                                                                
022500     EJECT                                                                
022600 IMS-GN-ARTC-WDK601 SECTION.                                              
022700     MOVE 'WLARTC01  ' TO SSA1                                            
022800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
022900     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA SSA1                      
023000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
023100     PERFORM IMS-STATUSKONTROLL                                           
023200     .                                                                    
023300     SKIP2                                                                
023400 IMS-GNP-ARTC-WDK611 SECTION.                                             
023500     STRING 'WLARTC11(KDSEGKEY =1)'                                       
023600          DELIMITED BY SIZE INTO SSA1                                     
023700     MOVE '  GE' TO GODK-STATUSKODER                                      
023800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
023900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024000     PERFORM IMS-STATUSKONTROLL                                           
024100     .                                                                    
024200     EJECT                                                                
024300 IMS-GU-BENA-WDD311 SECTION.                                              
024400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
024500          DELIMITED BY SIZE INTO SSA1                                     
024600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
024700          DELIMITED BY SIZE INTO SSA2                                     
024800     MOVE '  GE' TO GODK-STATUSKODER                                      
024900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
025000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     .                                                                    
025300     EJECT                                                                
025400 IMS-GU-KVAH-W6D201 SECTION.                                              
025500     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
025600          DELIMITED BY SIZE INTO SSA1                                     
025700     MOVE '  GE' TO GODK-STATUSKODER                                      
025800     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1                      
025900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     .                                                                    
026200     EJECT                                                                
026300 IMS-STATUSKONTROLL SECTION.                                              
026400     SKIP2                                                                
026500     SET STATUS-IX TO 1                                                   
026600     SEARCH GODK-STATUS                                                   
026700       AT END                                                             
026800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
026900         DISPLAY FELTEXT                                                  
027000         CALL FELLOG                                                      
027100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027200         CONTINUE                                                         
027300     END-SEARCH                                                           
027400     .                                                                    
