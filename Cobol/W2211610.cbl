000100*                  * CONVERTED BY VILMAII *                               
000200     SKIP3                                                                
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W2211610.                                    
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 18:50:30.                         
000800*AUTHOR.                     IDK, GÖTEBORG.                               
000900*DATE-WRITTEN.               NOV 1978.                                    
001000*    SKIP3                                                                
001100*REMARKS.                                                                 
001200*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2211600 OCH SKÖTER            
001300*        SAMTLIGA IMS-CALL MOT DATABASERNA WDK6 OCH WDG3.                 
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP3                                                                
001700 DATA DIVISION.                                                           
001800     EJECT                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100*    -- CHECKED BY WY2000                                                 
002200 01  SUBPROGRAM.                                                          
002300     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
002400     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
002500     SKIP1                                                                
002600 01  SWITCHAR.                                                            
002700     03  SW-FORSTA-WDG3-ANROP PIC X      VALUE 'J'.                       
002800     EJECT                                                                
002900 01  IMS-WS.                                                              
003000     03  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
003100     SKIP3                                                                
003200     03  STATUS-WS           PIC X(2).                                    
003300         88  SEGMENT-FINNS               VALUE '  '.                      
003400         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
003500         88  SEGMENT-SLUT                VALUE 'GB'.                      
003600     SKIP3                                                                
003700     03  SSA1                PIC X(60).                                   
003800     03  SSA2                PIC X(60).                                   
003900     03  SSA3                PIC X(60).                                   
004000     SKIP3                                                                
004100     03  KONSTANTER.                                                      
004200         05  JA              PIC X       VALUE 'J'.                       
004300         05  NEJ             PIC X       VALUE 'N'.                       
004400     SKIP3                                                                
004500 01  W-IDARTNR-X.                                                         
004600     03  W-IDARTNR           PIC S9(9)               COMP-3.              
004700 01  W-2213-KEY.                                                          
004800     03  FILLER              PIC X(4)    VALUE '2213'.                    
004900     03  W-IDDC-2213         PIC X(2)    VALUE '11'.                      
005000     03  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
005100 01  W-2203-KEY.                                                          
005200     03  FILLER              PIC X(4)    VALUE '2203'.                    
005300     03  W-IDDC-2203         PIC X(2)    VALUE '11'.                      
005400     03  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
005500 01  W-KDERS-0-X.                                                         
005600     03  FILLER              PIC S9(3)   VALUE ZERO  COMP-3.              
005700     SKIP3                                                                
005800 01  GODK-STATUSKODER.                                                    
005900     03  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC X(2).             
006000     EJECT                                                                
006100*    -COPY W0003.                                                         
006200     EJECT                                                                
006300 01  DLI-IO-AREA-01          PIC X(120).                                  
006400                                                                          
006500*01  WLARTC01 -COPY WDK601     -RED DLI-IO-AREA-01                        
006600     EJECT                                                                
006700 01  DLI-IO-AREA-11          PIC X(900).                                  
006800                                                                          
006900*01  WLARTC11 -COPY WDK611     -RED DLI-IO-AREA-11                        
007000     EJECT                                                                
007100 01  DLI-IO-AREA             PIC X(120).                                  
007200                                                                          
007300*01  WLXXBI01 -COPY WDG301     -PRE HAND01-  -RED DLI-IO-AREA             
007400     EJECT                                                                
007500*01  WLXXBI11 -COPY WDGX2214   -PRE HAND02- -RED DLI-IO-AREA              
007600     EJECT                                                                
007700 01  DLI-IO-2204.                                                         
007800*03  -COPY WDGX2204                                                       
007900     EJECT                                                                
008000 LINKAGE SECTION.                                                         
008100     SKIP1                                                                
008200*01  AREA -COPY W221L161   -PRE LINK-                                     
008300     EJECT                                                                
008400*01  AREA -COPY W221L162   -PRE LINK2- -RED LINK-AREA                     
008500     EJECT                                                                
008600*    -COPY W0008 -PRE ARTC-                                               
008700          05  FILLER                     PIC X.                           
008800     EJECT                                                                
008900*    -COPY W0008 -PRE XXBI-                                               
009000          05  FILLER                     PIC X.                           
009100*    -COPY W0008 -PRE XXBJ-                                               
009200          05  FILLER                     PIC X.                           
009300     EJECT                                                                
009400 PROCEDURE DIVISION  USING LINK-AREA ARTC-PCB                             
009500                          XXBI-PCB XXBJ-PCB.                              
009600     ENTRY 'DLITCBL' USING LINK-AREA ARTC-PCB                             
009700                          XXBI-PCB XXBJ-PCB.                              
009800     SKIP3                                                                
009900     MOVE JA TO LINK-FLJANEJ-ANROP                                        
010000                                                                          
010100     IF LINK2-KDCALL = ZERO                                               
010200       PERFORM IMS-GET-XXBI01                                             
010300       MOVE NEJ TO SW-FORSTA-WDG3-ANROP                                   
010400     END-IF                                                               
010500                                                                          
010600     IF LINK-LAS-ARTINFO                                                  
010700       PERFORM A-HAMTA-ARTIKELINFO                                        
010800     ELSE                                                                 
010900       IF LINK2-LAES-DLET-WDG3                                            
011000         PERFORM B-LAS-DLET-WDG3                                          
011100       ELSE                                                               
011200         IF LINK-REPL-ARTINFO                                             
011300           PERFORM C-REPL-ARTINFO                                         
011400         ELSE                                                             
011500           IF LINK2-ISRT-2204                                             
011600              PERFORM D-ISRT-2204                                         
011700           END-IF                                                         
011800         END-IF                                                           
011900       END-IF                                                             
012000     END-IF                                                               
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-HAMTA-ARTIKELINFO SECTION.                                             
012600     SKIP1                                                                
012700     MOVE LINK-IDARTNR TO W-IDARTNR                                       
012800     PERFORM IMS-GET-ARTC01                                               
012900     SKIP1                                                                
013000     IF SEGMENT-FINNS                                                     
013100       MOVE ART-IDLEVNR TO LINK-IDLEVNR                                   
013200       PERFORM IMS-GET-ARTC11                                             
013300       MOVE CLAG-KDHF          TO LINK-KDHF                               
013400       MOVE CLAG-IDINK         TO LINK-IDINK                              
013500       MOVE CLAG-KDAVT         TO LINK-KDAVT                              
013600       MOVE CLAG-KVDAGAR-INLEV TO LINK-KVDAGAR-INLEV                      
013700       MOVE CLAG-KVVECKOR-FT   TO LINK-KVVECKOR-FT                        
013800       MOVE CLAG-KVVECKOR-BT   TO LINK-KVVECKOR-BT                        
013900       MOVE CLAG-KVVECKOR-AT   TO LINK-KVVECKOR-AT                        
014000       MOVE CLAG-KVVECKOR-LT   TO LINK-KVVECKOR-LT                        
014100       MOVE CLAG-KVDAGAR-TT    TO LINK-KVDAGAR-TT                         
014200       MOVE CLAG-KVDAGAR-FFH   TO LINK-KVDAGAR-FFH                        
014300       MOVE CLAG-BEFT          TO LINK-BEFT                               
014400     ELSE                                                                 
014500       MOVE LOW-VALUE          TO LINK-AREA                               
014600       MOVE NEJ                TO LINK-FLJANEJ-ANROP                      
014700       MOVE W-IDARTNR          TO LINK-IDARTNR                            
014800     END-IF                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 B-LAS-DLET-WDG3 SECTION.                                                 
015200     SKIP1                                                                
015300     IF SW-FORSTA-WDG3-ANROP = JA                                         
015400       PERFORM IMS-GET-XXBI01                                             
015500       MOVE NEJ TO SW-FORSTA-WDG3-ANROP                                   
015600     END-IF                                                               
015700     SKIP1                                                                
015800     PERFORM IMS-GET-XXBI11                                               
015900     IF SEGMENT-FINNS                                                     
016000       MOVE HAND02-2214-IDARTNR TO LINK2-IDARTNR                          
016100       PERFORM IMS-DLET-XXBI11                                            
016200     ELSE                                                                 
016300       MOVE NEJ TO LINK2-FLJANEJ-ANROP                                    
016400     END-IF                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 C-REPL-ARTINFO SECTION.                                                  
016800     SKIP1                                                                
016900     MOVE LINK-KVDAGAR-TT    TO CLAG-KVDAGAR-TT                           
017000     MOVE LINK-KVDAGAR-INLEV TO CLAG-KVDAGAR-INLEV                        
017100     MOVE LINK-KVDAGAR-FFH   TO CLAG-KVDAGAR-FFH                          
017200     MOVE LINK-KVVECKOR-FT   TO CLAG-KVVECKOR-FT                          
017300     MOVE LINK-KVVECKOR-BT   TO CLAG-KVVECKOR-BT                          
017400     SKIP1                                                                
017500     PERFORM IMS-REPL-ARTC                                                
017600     .                                                                    
017700     SKIP1                                                                
017800     EJECT                                                                
017900 D-ISRT-2204 SECTION.                                                     
018000                                                                          
018100     MOVE LINK2-IOAREA TO 2204-WDGX2204                                   
018200     PERFORM IMS-ISRT-2204                                                
018300     .                                                                    
018400     EJECT                                                                
018500 IMS-GET-ARTC01 SECTION.                                                  
018600     SKIP1                                                                
018700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X                             
018800                    '&KDERS    =' W-KDERS-0-X ')'                         
018900     DELIMITED BY SIZE INTO SSA1                                          
019000     MOVE '  GE' TO GODK-STATUSKODER                                      
019100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
019200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
019300     PERFORM IMS-STATUSKONTROLL                                           
019400     .                                                                    
019500     EJECT                                                                
019600 IMS-GET-ARTC11 SECTION.                                                  
019700     SKIP1                                                                
019800     MOVE 'WLARTC11' TO SSA1                                              
019900     MOVE '  ' TO GODK-STATUSKODER                                        
020000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
020100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400     EJECT                                                                
020500 IMS-GU-ARTC11 SECTION.                                                   
020600     SKIP1                                                                
020700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
020800     DELIMITED BY SIZE INTO SSA1                                          
020900     MOVE 'WLARTC11' TO SSA2                                              
021000     MOVE '  ' TO GODK-STATUSKODER                                        
021100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2              
021200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
021300     PERFORM IMS-STATUSKONTROLL                                           
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-GET-XXBI01 SECTION.                                                  
021700     SKIP1                                                                
021800     STRING 'WLXXBI01(WDG3KEY  =' W-2213-KEY ')'                          
021900     DELIMITED BY SIZE INTO SSA1                                          
022000     MOVE '  ' TO GODK-STATUSKODER                                        
022100     CALL CBLTDLI USING GU XXBI-PCB DLI-IO-AREA SSA1                      
022200     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-GET-XXBI11 SECTION.                                                  
022700     SKIP1                                                                
022800     MOVE 'WLXXBI11' TO SSA1                                              
022900     MOVE '  GE' TO GODK-STATUSKODER                                      
023000     CALL CBLTDLI USING GHNP XXBI-PCB DLI-IO-AREA SSA1                    
023100     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
023200     PERFORM IMS-STATUSKONTROLL                                           
023300     .                                                                    
023400     EJECT                                                                
023500 IMS-DLET-XXBI11 SECTION.                                                 
023600     SKIP1                                                                
023700     MOVE '  ' TO GODK-STATUSKODER                                        
023800     CALL CBLTDLI USING DLET XXBI-PCB DLI-IO-AREA                         
023900     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
024000     PERFORM IMS-STATUSKONTROLL                                           
024100     .                                                                    
024200     EJECT                                                                
024300 IMS-REPL-ARTC SECTION.                                                   
024400     SKIP1                                                                
024500     MOVE '  ' TO GODK-STATUSKODER                                        
024600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-11                      
024700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-ISRT-2204 SECTION.                                                   
025200                                                                          
025300     STRING 'WLXXBJ01(WDG3KEY  =' W-2203-KEY ')'                          
025400             DELIMITED BY SIZE INTO SSA1                                  
025500     MOVE 'WLXXBJ11' TO SSA2                                              
025600     MOVE '  ' TO GODK-STATUSKODER                                        
025700     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-2204 SSA1 SSA2               
025800     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100                                                                          
026200     EJECT                                                                
026300 IMS-STATUSKONTROLL SECTION.                                              
026400     SKIP1                                                                
026500     SET STATUS-IX TO 1                                                   
026600     SEARCH GODK-STATUS                                                   
026700       AT END                                                             
026800          DISPLAY IMS-WS                                                  
026900          CALL FELLOG                                                     
027000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027100          CONTINUE                                                        
027200     END-SEARCH                                                           
027300     .                                                                    
