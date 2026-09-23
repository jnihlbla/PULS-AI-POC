000100 ID DIVISION.                                                             
000200 PROGRAM-ID.              W2218010.                                       
000300 AUTHOR.                  KENT HELLQVIST                                  
000400 DATE-WRITTEN.            JANUARI 1986.                                   
000500     REMARKS.                                                             
000600*                                                                         
000700*        ANN J HAR ÄNDRAT PGM:ET FÖR DELNINGEN. KLAR 901204               
000800*        (GJORT COBMETA)                                                  
000900*                                                                         
001000*        NY COPYTEXT PÅ WLXXBK.                 KLAR 910322               
001100*        (GJORT COBCONV)                                                  
001200*                                                                         
001300*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2218000                       
001400*        OCH SKÖTER OM SAMTLIGA IMS-CALL ÅT DETSAMMA                      
001500*        MOT WDR2, LOGISKT (WLXXBK),                                      
001501*        MOT WDR5, LOGISKT (WLXXBL),                                      
001502*        MOT WDK6, LOGISKT (WLARTC)                                       
001700*    SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002201                                                                          
002210*    -- CHECKED BY WY2000                                                 
002300 77   PROGRAM-NAMN           VALUE 'W2218010'                             
002400                                 PIC X(8).                                
002500 77  JA                          PIC X       VALUE 'J'.                   
002600 77  NEJ                         PIC X       VALUE 'N'.                   
002700     SKIP3                                                                
002800 01  NYCKLAR-TILL-DLI.                                                    
002900   03  W-IDLEVNR-X.                                                       
003000     05  W-IDLEVNR               PIC X(5).                                
003100   03  W-IDARTNR-X.                                                       
003200     05  W-IDARTNR               PIC S9(9)  COMP-3.                       
003300                                                                          
003400   03  W-WDGXKEY-2215-X.                                                  
003500     05  FILLER                  PIC  X(4)  VALUE '2215'.                 
003600     05  FILLER                  PIC  X(26) VALUE LOW-VALUE.              
003700                                                                          
003800   03  W-WDGXKEY-2217-X.                                                  
003900     05  FILLER                  PIC  X(4)  VALUE '2217'.                 
004000     05  FILLER                  PIC  X(26) VALUE LOW-VALUE.              
004100     SKIP3                                                                
004200*                            *** GENERELLA SUBRUTINER                     
004300 01  SUBPROGRAM.                                                          
004400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004600     EJECT                                                                
004700*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
004800*                                                                         
004900 01  IMS-WS.                                                              
005000   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
005100     SKIP3                                                                
005200*                            *** STATUSKOD FRÅN IMS                       
005300   03  STATUS-WS                 PIC XX.                                  
005400     88  SEGMENT-FINNS                       VALUE '  '.                  
005500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005600     SKIP3                                                                
005700   03  GODK-STATUSKODER.                                                  
005800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005900     SKIP3                                                                
006000   03  SSA1                      PIC X(64).                               
006100     EJECT                                                                
006200*01  -COPY W0003                                                          
006400     EJECT                                                                
006500 01  DLI-IO-AREA.                                                         
006600   03 IO-AREA                    PIC X(200)  VALUE SPACE.                 
006700     SKIP3                                                                
006800*  03  WLXXBL01 -COPY WDGX01 -PRE   XXBL-     -RED IO-AREA.               
007000     EJECT                                                                
007100*  03  WLXXBL11 -COPY WDGX2218 -PRE   XXBL-     -RED IO-AREA.             
007300     EJECT                                                                
007400*  03  WLXXBK01 -COPY WDGX01 -PRE   XXBK-     -RED IO-AREA.               
007600     EJECT                                                                
007700*  03  WLXXBK11 -COPY WDGX2216 -PRE   XXBK-     -RED IO-AREA.             
007900     EJECT                                                                
008000 01  DLI-IO-AREA1.                                                        
008100   03 IO-AREA1                   PIC X(200)  VALUE SPACE.                 
008200*  03  WLARTC01 -COPY WDK601    -RED IO-AREA1.                            
008400     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600*01  0-AREA  -COPY W221L800 -PRE LINK                                     
008800     EJECT                                                                
008900 01  LINK-DATA-AREA.                                                      
009000   03  LINK-AREA                  PIC X(100).                             
009100*  03  1-AREA -COPY W221L801 -PRE LINK   -RED LINK-AREA                   
009300     EJECT                                                                
009400*  03  2-AREA -COPY W221L802 -PRE LINK   -RED LINK-AREA                   
009600     EJECT                                                                
009700*  03  3-AREA -COPY W221L803 -PRE LINK   -RED LINK-AREA                   
009900     EJECT                                                                
010000*01      -COPY W0008     -PRE WLXXBL-                                     
010200      05 FILLER          PIC X.                                           
010300     EJECT                                                                
010400*01      -COPY W0008     -PRE WLXXBK-                                     
010600      05 FILLER          PIC X.                                           
010700     EJECT                                                                
010800*01      -COPY W0008     -PRE WLARTC-                                     
011000      05 FILLER          PIC X.                                           
011100     EJECT                                                                
011200 PROCEDURE DIVISION USING LINK0-AREA LINK-DATA-AREA WLXXBL-PCB            
011300                                         WLXXBK-PCB WLARTC-PCB.           
011400     SKIP3                                                                
011500     EVALUATE LINK0-KDCALL                                                
011600       WHEN LINK0-LAES-WLXXBL01                                           
011700         PERFORM A-LAES-WLXXBL01                                          
011800                                                                          
011900       WHEN LINK0-LAES-WLXXBL11-KVAL                                      
012000         PERFORM B-LAES-WLXXBL11-KVAL                                     
012100                                                                          
012500       WHEN LINK0-LAES-WLXXBK01                                           
012600         PERFORM D-LAES-WLXXBK01                                          
012700                                                                          
012800       WHEN LINK0-LAES-WLXXBK11                                           
012900         PERFORM E-LAES-WLXXBK11                                          
013000                                                                          
013100       WHEN LINK0-REPL-WLXXBK11                                           
013200         PERFORM F-REPL-WLXXBK11                                          
013300                                                                          
013400       WHEN LINK0-LAES-WLARTC01                                           
013500         PERFORM G-LAES-WLARTC01                                          
013600                                                                          
013700       WHEN OTHER                                                         
013800         MOVE LINK0-KDSVAR-FEL TO LINK0-KDSVAR                            
013900     END-EVALUATE                                                         
014000*                                                                         
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-LAES-WLXXBL01 SECTION.                                                 
014600     SKIP3                                                                
014700*                                                                         
014800     PERFORM IMS-GET-WLXXBL01                                             
014900                                                                          
015000     MOVE LINK0-KDSVAR-OK     TO LINK0-KDSVAR                             
015100     .                                                                    
015200     EJECT                                                                
015300 B-LAES-WLXXBL11-KVAL SECTION.                                            
015400     SKIP3                                                                
015500                                                                          
015600     MOVE LINK0-IDLEVNR              TO W-IDLEVNR                         
015700     PERFORM IMS-GET-WLXXBL11-KVAL                                        
015800                                                                          
015900     IF SEGMENT-FINNS                                                     
016000        MOVE XXBL-2218-IDLEVNR       TO LINK2-IDLEVNR                     
016100        MOVE XXBL-2218-IDARTNR       TO LINK2-IDARTNR                     
016200        MOVE XXBL-2218-IDANSK        TO LINK2-IDANSK                      
016300        MOVE LINK0-KDSVAR-OK         TO LINK0-KDSVAR                      
016400     ELSE                                                                 
016500        MOVE LINK0-KDSVAR-FEL        TO LINK0-KDSVAR                      
016600     END-IF                                                               
016700     .                                                                    
016800     EJECT                                                                
017500 D-LAES-WLXXBK01 SECTION.                                                 
017600     SKIP3                                                                
017700     PERFORM IMS-GET-WLXXBK01                                             
017800     MOVE LINK0-KDSVAR-OK   TO LINK0-KDSVAR                               
017900     .                                                                    
018000     EJECT                                                                
018100 E-LAES-WLXXBK11 SECTION.                                                 
018200     SKIP3                                                                
018300     PERFORM IMS-GET-WLXXBK11                                             
018400                                                                          
018500     IF SEGMENT-FINNS                                                     
018600        MOVE XXBK-2216-IDLEVNR        TO LINK1-IDLEVNR                    
018700        MOVE XXBK-2216-IDOVERFNR      TO LINK1-IDOVERFNR                  
018800        MOVE XXBK-2216-TISEND-SEN     TO LINK1-TISEND-SEN                 
019000        MOVE XXBK-2216-KDVECKOSL      TO LINK1-KDVECKOSL                  
019010        MOVE XXBK-2216-FLLEVPLP       TO LINK1-FLLEVPLP                   
019020        MOVE XXBK-2216-FLLEVVB        TO LINK1-FLLEVVB                    
019100        MOVE XXBK-2216-KDEDI          TO LINK1-KDEDI                      
019200        MOVE XXBK-2216-FLAVIS         TO LINK1-FLAVIS                     
019300        MOVE XXBK-2216-IDOVERFNR-VV   TO LINK1-IDOVERFNR-VV               
019410        MOVE XXBK-2216-IDLEVKND       TO LINK1-IDLEVKND                   
019420        MOVE XXBK-2216-FLODETTE       TO LINK1-FLODETTE                   
019421        MOVE LINK0-KDSVAR-OK          TO LINK0-KDSVAR                     
019430        IF XXBK-2216-TISEND-PER NUMERIC                                   
019440          MOVE XXBK-2216-TISEND-PER   TO LINK1-TISEND-PER                 
019450        ELSE                                                              
019460          MOVE ZERO                   TO LINK1-TISEND-PER                 
019470        END-IF                                                            
019500                                                                          
019700     ELSE                                                                 
019800        MOVE LINK0-KDSVAR-FEL         TO LINK0-KDSVAR                     
019900     END-IF                                                               
020000                                                                          
020100     .                                                                    
020200     EJECT                                                                
020300 F-REPL-WLXXBK11 SECTION.                                                 
020400     SKIP3                                                                
020510     MOVE LINK1-IDLEVNR          TO XXBK-2216-IDLEVNR                     
020520     MOVE LINK1-IDOVERFNR        TO XXBK-2216-IDOVERFNR                   
020530     MOVE LINK1-TISEND-SEN       TO XXBK-2216-TISEND-SEN                  
020550     MOVE LINK1-KDVECKOSL        TO XXBK-2216-KDVECKOSL                   
020560     MOVE LINK1-FLLEVPLP         TO XXBK-2216-FLLEVPLP                    
020580     MOVE LINK1-KDEDI            TO XXBK-2216-KDEDI                       
020590     MOVE LINK1-FLAVIS           TO XXBK-2216-FLAVIS                      
020591     MOVE LINK1-IDOVERFNR-VV     TO XXBK-2216-IDOVERFNR-VV                
020593     MOVE LINK1-IDLEVKND         TO XXBK-2216-IDLEVKND                    
020594***  FIX                                                                  
020595     IF LINK1-FLLEVVB = 'J' OR 'N'                                        
020596        MOVE LINK1-FLLEVVB       TO XXBK-2216-FLLEVVB                     
020597     ELSE                                                                 
020598        MOVE 'N'                 TO XXBK-2216-FLLEVVB                     
020599     END-IF                                                               
020600     IF LINK1-FLODETTE = 'J' OR 'N'                                       
020601        MOVE LINK1-FLODETTE      TO XXBK-2216-FLODETTE                    
020602     ELSE                                                                 
020603        MOVE SPACE               TO XXBK-2216-FLODETTE                    
020604     END-IF                                                               
020605     IF LINK1-TISEND-PER NUMERIC                                          
020606        MOVE LINK1-TISEND-PER    TO XXBK-2216-TISEND-PER                  
020607     ELSE                                                                 
020608        MOVE ZERO                TO XXBK-2216-TISEND-PER                  
020609     END-IF                                                               
020610***  FIX                                                                  
020700     PERFORM IMS-REPL-WLXXBK11                                            
020800                                                                          
020900     MOVE LINK0-KDSVAR-OK         TO LINK0-KDSVAR                         
021000     .                                                                    
021100     EJECT                                                                
021200 G-LAES-WLARTC01 SECTION.                                                 
021300     SKIP3                                                                
021400     MOVE LINK0-IDARTNR TO W-IDARTNR                                      
021500     PERFORM IMS-GET-WLARTC01                                             
021600                                                                          
021700     IF SEGMENT-FINNS                                                     
021800        MOVE WLARTC01                 TO LINK3-W221L803                   
021900                                                                          
022000        MOVE LINK0-KDSVAR-OK          TO LINK0-KDSVAR                     
022100     ELSE                                                                 
022200        MOVE LINK0-KDSVAR-FEL         TO LINK0-KDSVAR                     
022300     END-IF                                                               
022400     .                                                                    
022500                                                                          
022600     EJECT                                                                
022700* IMS SEKTIONER                                                           
022800     SKIP3                                                                
022900 IMS-GET-WLXXBL01 SECTION.                                                
023000     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-2217-X ')'                    
023100             DELIMITED BY SIZE INTO SSA1                                  
023200     MOVE '  ' TO GODK-STATUSKODER                                        
023300     CALL CBLTDLI USING GU WLXXBL-PCB DLI-IO-AREA SSA1                    
023400     MOVE WLXXBL-STATUS-CODE TO STATUS-WS                                 
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     SKIP3                                                                
023700     .                                                                    
023800 IMS-GET-WLXXBL11-KVAL SECTION.                                           
023900     STRING 'WLXXBL11(IDLEVNR  =' W-IDLEVNR-X ')'                         
024000             DELIMITED BY SIZE INTO SSA1                                  
024100     MOVE '  GE' TO GODK-STATUSKODER                                      
024200     CALL CBLTDLI USING GHNP WLXXBL-PCB DLI-IO-AREA SSA1                  
024300     MOVE WLXXBL-STATUS-CODE TO STATUS-WS                                 
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     SKIP3                                                                
024600     .                                                                    
025300     EJECT                                                                
025400 IMS-GET-WLXXBK01 SECTION.                                                
025500     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-2215-X ')'                    
025600             DELIMITED BY SIZE INTO SSA1                                  
025700     MOVE '  ' TO GODK-STATUSKODER                                        
025800     CALL CBLTDLI USING GU WLXXBK-PCB DLI-IO-AREA SSA1                    
025900     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
026000     PERFORM IMS-STATUSKONTROLL                                           
026100     SKIP3                                                                
026200     .                                                                    
026300 IMS-GET-WLXXBK11 SECTION.                                                
026400     MOVE 'WLXXBK11 '  TO SSA1                                            
026500     MOVE '  GE' TO GODK-STATUSKODER                                      
026600     CALL CBLTDLI USING GHNP WLXXBK-PCB DLI-IO-AREA SSA1                  
026700     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
026800     PERFORM IMS-STATUSKONTROLL                                           
026900     SKIP3                                                                
027000     .                                                                    
027100 IMS-REPL-WLXXBK11 SECTION.                                               
027200     MOVE '  ' TO GODK-STATUSKODER                                        
027300     CALL CBLTDLI USING REPL WLXXBK-PCB DLI-IO-AREA                       
027400     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
027500     PERFORM IMS-STATUSKONTROLL                                           
027600     .                                                                    
027700     EJECT                                                                
027800 IMS-GET-WLARTC01 SECTION.                                                
027900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
028000             DELIMITED BY SIZE INTO SSA1                                  
028100     MOVE '  GE' TO GODK-STATUSKODER                                      
028200     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA1 SSA1                   
028300     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
028400     PERFORM IMS-STATUSKONTROLL                                           
028500     SKIP3                                                                
028600     .                                                                    
028700 IMS-STATUSKONTROLL SECTION.                                              
028800     SET STATUS-IX TO 1                                                   
028900     SEARCH GODK-STATUS AT END CALL FELLOG                                
029000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029100       CONTINUE                                                           
029200     END-SEARCH                                                           
029300     .                                                                    
