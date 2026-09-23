000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1150100.                                    
000300 AUTHOR.                     GUNNEL ERIKSSON                              
000400 DATE-WRITTEN.               MAJ  1988.                                   
000500     SKIP2                                                                
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*    (BMP MED CHECK-POINT)                                                
001000*    LÄSER WLARTG                                                         
001100*    UPPDATERAR WLARTG11                                                  
001200*    OM STOPPDATUM < = DAGENS DATUM SÄTTS MARKNADENS UPPD.DATUM           
001300*    TILL 2, D.V.S. ARTIKELN TAS BORT FRÅN MARKNADENS KÖBILD              
001400*            -----------------------------------------------              
001500*    ÄNDRING: TILLAGT ATT ÄVEN KONTROLLERA MARKNADSSPECIFIKT              
001600*             STOPPDATUM PÅ ARTG11-SEGM. /C.E. 910821                     
001700*            -----------------------------------------------              
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300     SKIP2                                                                
002301*    -COPY WY2000W1                                                       
002310     SKIP3                                                                
002400 77  PROGRAM-NAMN            PIC X(8) VALUE 'W1150100'.                   
002500*- - - - - - - - - - - - - - - - - KONSTANTER.                            
002600 77  JA                      PIC X       VALUE 'J'.                       
002700 77  NEJ                     PIC X       VALUE 'N'.                       
002800 77  CHKP-RAK                PIC S9(9)   VALUE +0 COMP SYNC.              
002900*- - - - - - - - - - - - - - - - -CHECK-POINT.                            
003000 77  CHKP-ID                     PIC X(8)    VALUE 'W1150100'.            
003100 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003200 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
003300 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003400 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
003500     SKIP2                                                                
003600*- - - - - - - - - - - - - - - - - HJÄLP-FAELT                            
003700 77  WS-GENERELL-RENS         PIC X            VALUE 'N'.                 
003800 01  DAGENS-DATUM             PIC S9(7) COMP-3 VALUE ZERO.                
003900 01  WS-ARTG-TISTOMREG        PIC S9(7) COMP-3 VALUE ZERO.                
004000*- - - - - - - - - - - - - - - - - DYNAMISKA-SUB-PGM                      
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
004300     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
004400     EJECT                                                                
004500*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
004600*                                                                         
004700 01    IMS-WS.                                                            
004800   03     FILLER           PIC X(8)   VALUE 'IMS-WS  '.                   
004900*                                                                         
005000*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
005700   03    W-WDD2D1KY.                                                      
005800     05  W-IDPROJ       PIC   X(4)          VALUE SPACE.                  
005900     05  W-KDBASLM      PIC   X(6)          VALUE SPACE.                  
006000     05  W-IDFKNGRP     PIC   S9(5)  COMP-3 VALUE ZERO.                   
006100     05  W-IDARTNR-SEQD PIC   S9(9)  COMP-3 VALUE ZERO.                   
006200                                                                          
006300   03    W-IDARTNR-X.                                                     
006400     05  W-IDARTNR      PIC S9(9)  COMP-3 VALUE ZERO.                     
006500                                                                          
006600*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
006700   03    STATUS-WS      PIC XX.                                           
006800     88  SEGMENT-FINNS             VALUE '  '.                            
006900     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
007000     88  BASEN-SLUT                VALUE 'GB'.                            
007100     88  IMS-EJ-OK                 VALUE 'XD'.                            
007200*                                                                         
007300*                                                                         
007400   03    SSA1           PIC X(96).                                        
007500   03    SSA2           PIC X(96).                                        
007600*                                                                         
007700   03    GODK-STATUSKODER.                                                
007800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007900     EJECT                                                                
008000*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
008100*01      -COPY W0003.                                                     
008200     EJECT                                                                
008300*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
008400 01   FILLER                  PIC X(16) VALUE 'I-O-AREA1'.                
008500 01  DLI-IO-AREA1.                                                        
008600     03 IO-AREA1            PIC X(25).                                    
008700*                                                                         
008800*    03 AREA -COPY WDD2D1 -PRE   ARTK01- -RED IO-AREA1.                   
008900     EJECT                                                                
009000 01  FILLER                  PIC X(16) VALUE 'I-O-AREA2'.                 
009100 01  DLI-IO-AREA2.                                                        
009200     03 IO-AREA2            PIC X(550).                                   
009300*                                                                         
009400*    03 AREA  -COPY WDD201 -PRE   ARTG01- -RED IO-AREA2.                  
009500     EJECT                                                                
009600*    03 AREA  -COPY WDD211 -PRE   ARTG11- -RED IO-AREA2.                  
009700*                                                                         
009800     EJECT                                                                
009900 LINKAGE SECTION.                                                         
010000     SKIP2                                                                
010100*    -COPY W0008 -PRE MSG-.                                               
010200          05  FILLER         PIC XX.                                      
010300     SKIP2                                                                
010400     EJECT                                                                
010500*    -COPY W0008 -PRE ARTK-.                                              
010600          05  FILLER         PIC XX.                                      
010700     EJECT                                                                
010800*    -COPY W0008 -PRE ARTG-.                                              
010900          05  FILLER         PIC XX.                                      
011000     EJECT                                                                
011100 PROCEDURE DIVISION  USING MSG-PCB  ARTK-PCB ARTG-PCB.                    
011200     ENTRY 'DLITCBL' USING MSG-PCB  ARTK-PCB ARTG-PCB.                    
011300     SKIP2                                                                
011700     PERFORM  A-INIT                                                      
011800     MOVE +1              TO CHKP-RAK                                     
011900     PERFORM IMS-GN-ARTK01                                                
012000     PERFORM UNTIL BASEN-SLUT                                             
012100       MOVE ARTK01-SEQD-IDPROJ   TO  W-IDPROJ                             
012200       MOVE ARTK01-SEQD-KDBASLM  TO  W-KDBASLM                            
012300       MOVE ARTK01-SEQD-IDFKNGRP TO  W-IDFKNGRP                           
012400       MOVE ARTK01-SEQD-IDARTNR                                           
012410            TO  W-IDARTNR, W-IDARTNR-SEQD                                 
012500                                                                          
012600       MOVE NEJ TO WS-GENERELL-RENS                                       
012700                                                                          
012800       PERFORM IMS-GU-ARTG01                                              
012801       MOVE ARTG01-ART-TISTOMREG   TO TMP1-YYMMDD                         
012802       MOVE DAGENS-DATUM           TO TMP2-YYMMDD                         
012810       PERFORM WY2000P1                                                   
012900       IF TMP1-YYMMDD <= TMP2-YYMMDD                                      
013000         MOVE JA TO WS-GENERELL-RENS                                      
013100       END-IF                                                             
013200                                                                          
013300       PERFORM IMS-GHNP-ARTG11                                            
013400       PERFORM UNTIL SEGMENT-SAKNAS                                       
013410*                                     * FIX *                             
013500         IF ARTG11-ART-KDBASLM = '25ESP' OR '27PRT'                       
013501           CONTINUE                                                       
013502         ELSE                                                             
013503*                                     * FIX *                             
013510           IF  ARTG11-ART-TISTOMREG = ZERO                                
013600           AND WS-GENERELL-RENS = JA                                      
013610             IF ARTG11-ART-TIBASLM = 0                                    
013611******* - - - -  URSPRUNGLIGT ENDA TRÄFFALTERNATIV                        
013620               MOVE +2 TO ARTG11-ART-TIBASLM                              
013621               MOVE JA TO ARTG11-ART-FLBLMQ                               
013630               PERFORM IMS-REPL-ARTG11                                    
013640               ADD +1    TO CHKP-RAK                                      
013650             END-IF                                                       
013810           ELSE                                                           
013811             MOVE ARTG11-ART-TISTOMREG   TO TMP1-YYMMDD                   
013812             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
013813             PERFORM WY2000P1                                             
013820             IF  TMP1-YYMMDD          > ZERO                              
013900             AND TMP1-YYMMDD          <= TMP2-YYMMDD                      
014400               IF ARTG11-ART-TIBASLM = 0                                  
014500                 MOVE +2 TO ARTG11-ART-TIBASLM                            
014510                 MOVE JA TO ARTG11-ART-FLBLMQ                             
014600                 PERFORM IMS-REPL-ARTG11                                  
014700                 ADD +1    TO CHKP-RAK                                    
014800               END-IF                                                     
014900             ELSE                                                         
014910               CONTINUE                                                   
015000             END-IF                                                       
015020           END-IF                                                         
015021*                                     * FIX *                             
015023         END-IF                                                           
015024*                                     * FIX *                             
015030         PERFORM IMS-GHNP-ARTG11                                          
015100       END-PERFORM                                                        
015110                                                                          
015200       IF CHKP-RAK > 500                                                  
015300         PERFORM IMS-CHECKPOINT                                           
015400         MOVE +1 TO CHKP-RAK                                              
015500       END-IF                                                             
015600       PERFORM IMS-GN-ARTK01                                              
015610                                                                          
015700     END-PERFORM                                                          
015800     MOVE ZERO               TO RETURN-CODE                               
015900     GOBACK                                                               
016000     EJECT                                                                
016100     .                                                                    
016200 A-INIT SECTION.                                                          
016300     SKIP3                                                                
016400     MOVE SPACE TO    W-IDPROJ                                            
016500                      W-KDBASLM                                           
016600     MOVE ZERO TO     W-IDFKNGRP                                          
016700                      W-IDARTNR-SEQD                                      
016800     ACCEPT DAGENS-DATUM FROM DATE                                        
016900     PERFORM IMS-RESTART                                                  
017000     EJECT                                                                
017100     .                                                                    
017200*- - -  - - - IMS SEKTION                                                 
017300 IMS-RESTART SECTION.                                                     
017400     SKIP2                                                                
017500     MOVE SPACE TO MSG-IO-AREA                                            
017600     MOVE '  ' TO GODK-STATUSKODER                                        
017700     CALL CBLTDLI USING XRST MSG-PCB                                      
017800                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
017900                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
018000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
018100     PERFORM IMS-STATUSKONTROLL                                           
018200     SKIP3                                                                
018300     .                                                                    
018400 IMS-CHECKPOINT SECTION.                                                  
018500     SKIP2                                                                
018600     MOVE CHKP-ID TO MSG-IO-AREA                                          
018700     MOVE '  XD' TO GODK-STATUSKODER                                      
018800     CALL CBLTDLI USING CHKP MSG-PCB                                      
018900                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
019000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
019100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     IF IMS-EJ-OK                                                         
019400       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
019500       CALL FELLOG                                                        
019600     END-IF                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 IMS-GN-ARTK01  SECTION.                                                  
020000*                                                                         
020100     STRING 'WLARTK01(WDD2D1KY >' W-WDD2D1KY ')'                          
020200             DELIMITED BY SIZE INTO SSA1                                  
020300     MOVE '  GB' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING GN   ARTK-PCB DLI-IO-AREA1 SSA1                   
020500     MOVE ARTK-STATUS-CODE TO STATUS-WS                                   
020600     PERFORM IMS-STATUSKONTROLL                                           
020700     SKIP2                                                                
020800     EJECT                                                                
020900     .                                                                    
021000 IMS-GU-ARTG01 SECTION.                                                   
021100*                                                                         
021200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
021300             DELIMITED BY SIZE INTO SSA1                                  
021400     MOVE '  '   TO GODK-STATUSKODER                                      
021500     CALL CBLTDLI USING GU   ARTG-PCB DLI-IO-AREA2 SSA1                   
021600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
021700     PERFORM IMS-STATUSKONTROLL                                           
021800     SKIP2                                                                
021900     .                                                                    
022000 IMS-GHNP-ARTG11 SECTION.                                                 
022100*                                                                         
022200     MOVE   'WLARTG11 ' TO   SSA1                                         
022300                                                                          
022400     MOVE '  GE' TO GODK-STATUSKODER                                      
022500     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
022600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     SKIP2                                                                
022900     .                                                                    
023000 IMS-REPL-ARTG11 SECTION.                                                 
023100*                                                                         
023200     MOVE '  '   TO GODK-STATUSKODER                                      
023300     CALL  CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                       
023400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     SKIP2                                                                
023700     .                                                                    
023800 IMS-STATUSKONTROLL SECTION.                                              
023900*                                                                         
024000     SET STATUS-IX TO 1                                                   
024100     SEARCH GODK-STATUS AT END CALL FELLOG                                
024200         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
024300         CONTINUE                                                         
024400     END-SEARCH                                                           
024500     .                                                                    
024510     EJECT                                                                
024600*    -COPY WY2000P1                                                       
