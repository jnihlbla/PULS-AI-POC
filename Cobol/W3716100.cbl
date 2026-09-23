000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3716100.                                                
000300 AUTHOR.         BO HAMMARIN, GDC-GROUP.                                  
000400 DATE-WRITTEN.   OKT 1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION: PGM UPPDATERAR SEGMENT PÅ WDK611                           
000800*                                                                         
001100*    INDATA:   W37164 UPPDATERINGSPOSTER BYTESPOÄNG                       
001300*                                                                         
001400*    RETURKODER:                                                          
001500*                 999 (DUMPKOD VID FELLOG ).                              
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000*- - - - - - - - - - - - - - - - - - - - - - INFILER.                     
002010     SELECT W37164     ASSIGN  TO  UT-S-W37161D1.                         
002020                                                                          
002030*- - - - - - - - - - - - - - - - - - - - - - UTFILER.                     
002040                                                                          
002050     EJECT                                                                
002060 DATA DIVISION.                                                           
002070 FILE SECTION.                                                            
002080     SKIP3                                                                
002090*  UPPDATERINGSTRANSAKTIONER WDK611                                       
002100*                                                                         
002200 FD  W37164                                                               
002300     RECORDING F                                                          
002400     BLOCK 0 RECORDS.                                                     
002500                                                                          
002600*01  W37164-IN      -COPY W37164  -L.                                     
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                   PIC X(8)            VALUE 'W3716100'.        
003300 77  JA                      PIC X               VALUE 'J'.               
003400 77  NEJ                     PIC X               VALUE 'N'.               
003500                                                                          
003600 01  CHKP-VAR.                                                            
003700   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
003800   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
003900   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004000   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
004100   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
004200   03  CHKP-MAX                  PIC S9(3)   VALUE +100.                  
004300                                                                          
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700                                                                          
004800 01  DIVERSE.                                                             
004900     03  W37164-EOF          PIC X               VALUE 'N'.               
005000     03  RETURKOD            PIC S9(4) COMP SYNC VALUE +0.                
005300     EJECT                                                                
007700                                                                          
007800 01  FILLER PIC X(16) VALUE '*****NYCKLAR****'.                           
007900                                                                          
008000 01  W-IDARTNR-X.                                                         
008100     03  W-IDARTNR               PIC S9(9)     COMP-3  VALUE ZERO.        
008170 01  W-KDSEGKEY-X.                                                        
008180     03  W-KDSEGKEY              PIC X(1)      VALUE '1'.                 
008800                                                                          
008900     EJECT                                                                
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
009200     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
009300     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM '.          
009500     03  ABEND                   PIC X(8)      VALUE 'ABEND   '.          
009700                                                                          
009800*---- PARAMETRAR TILL ABEND                                               
009900 01  RETURKODER.                                                          
010000     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
010100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
010200     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
010300                                                                          
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16) VALUE '*****W37164'.           
010600*01  -COPY W37164  -PRE BYT-.                                             
010700     EJECT                                                                
010800*-----------------------------------------PARAMETRAR TILL                 
010900*                                         SUBPROGRAM POSTSUM              
011000 01  FILLER             PIC X(7)   VALUE 'POSTSUM'.                       
011100*01  -COPY W0005   -PRE POSTSUM-.                                         
011200     EJECT                                                                
011300*-----------------------------------------PARAMETRAR TILL                 
011400*                                         SUBPROGRAM WDATKONV             
011500 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
011600*01  -COPY WDATAREA.                                                      
011700     EJECT                                                                
011800*    SPARAREOR FÖR DATABASSEGMENT                                         
011900*                                                                         
012000 01  FILLER             PIC X(16) VALUE  'WDK601  '.                      
012110 01  DLI-IO-WDK601.                                                       
012120*    03  -COPY WDK601                                                     
012130     EJECT                                                                
012131 01  FILLER             PIC X(16) VALUE  'WDK611  '.                      
012140 01  DLI-IO-WDK611.                                                       
012150*    03  -COPY WDK611                                                     
012200     EJECT                                                                
012600*****                                                                     
012700*****    IN-AREA TILL IMS-SEKTIONERNA                                     
012800*****                                                                     
012900 01  IMS-WORKAREOR.                                                       
013000     03  FILLER          PIC X(16)   VALUE '*-*-*IMS-WS*-*-*'.            
013100     03  STATUS-WS       PIC XX.                                          
013200         88  SEGMENT-FINNS       VALUE '  '.                              
013300         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
013400         88  BASEN-SLUT          VALUE 'GB'.                              
013500         88  IMS-EJ-OK           VALUE 'XD'.                              
013600     SKIP3                                                                
013700     03  GODK-STATUSKODER.                                                
013800         05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.            
013900     SKIP3                                                                
014000 01      SSA1            PIC X(64).                                       
014100 01      SSA2            PIC X(64).                                       
014200     EJECT                                                                
014300*                                                                         
014400*        IMS FUNKTIONSKODER                                               
014500*                                                                         
014600*01      -COPY W0003                                                      
014700     EJECT                                                                
014800****************************************                                  
014900                                                                          
015000 LINKAGE SECTION.                                                         
015100*  MSG                                                                    
015200*01  -COPY W0009     -PRE MSG-                                            
015300   EJECT                                                                  
015500*01  -COPY W0008     -PRE WDK6-                                           
015600     05  WDK6-KONKAT-KEY PIC X(6).                                        
015700   EJECT                                                                  
015800 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
016100                                                                          
016400     PERFORM A-INITIERING                                                 
016500                                                                          
016600     PERFORM B-BEARBETNING                                                
016700     PERFORM C-AVSLUTNING                                                 
016800                                                                          
016900     MOVE +0 TO RETURN-CODE                                               
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INITIERING SECTION.                                                    
017400                                                                          
017500     PERFORM IMS-RESTART                                                  
017600                                                                          
018400     MOVE 'W37164' TO POSTSUM-PROGNAMN                                    
018500                                                                          
018600     OPEN INPUT W37164                                                    
018700     .                                                                    
018800     EJECT                                                                
018900 B-BEARBETNING SECTION.                                                   
019000                                                                          
019100     PERFORM BA-READ-W37164                                               
019200                                                                          
019300     PERFORM UNTIL W37164-EOF = JA                                        
019400       MOVE BYT-IDARTNR       TO W-IDARTNR                                
019800       PERFORM IMS-GHU-WDK601                                             
020000       PERFORM IMS-GHNP-WDK611                                            
020400       IF BYT-KDBEH = 'D'                                                 
020500         MOVE ZERO            TO CLAG-DAXPOINT                            
020510                                 CLAG-KDEXCHA                             
020520                                 CLAG-KVPOINT                             
020530                                 CLAG-PRREF                               
020531         ADD +1    TO CHKP-ANT                                            
020532         PERFORM IMS-REPL-WDK611                                          
020540       ELSE                                                               
020550         IF BYT-KDBEH = 'C'                                               
020560           MOVE BYT-DAXPOINT  TO CLAG-DAXPOINT                            
020570           MOVE BYT-KDEXCHA   TO CLAG-KDEXCHA                             
020580           MOVE BYT-KVPOINT   TO CLAG-KVPOINT                             
020590           MOVE BYT-PRREF     TO CLAG-PRREF                               
020591           ADD +1    TO CHKP-ANT                                          
020592           PERFORM IMS-REPL-WDK611                                        
020593         END-IF                                                           
020594       END-IF                                                             
021700       IF CHKP-ANT > CHKP-MAX                                             
021800         PERFORM X-TAG-CHECKPOINT                                         
021900       END-IF                                                             
022000       PERFORM BA-READ-W37164                                             
022100     END-PERFORM                                                          
022200                                                                          
022300     .                                                                    
022400     EJECT                                                                
022500 BA-READ-W37164 SECTION.                                                  
022900                                                                          
023000     READ W37164 INTO BYT-W37164                                          
023100     AT END                                                               
023200        MOVE JA  TO W37164-EOF                                            
023300     NOT AT END                                                           
023400        MOVE 'W37164' TO POSTSUM-FDNAMN                                   
023500        MOVE 'W37161D1' TO POSTSUM-DDNAMN2                                
023600        CALL POSTSUM USING POSTSUM-PARM                                   
023700     END-READ                                                             
023800     .                                                                    
023900     EJECT                                                                
024000 C-AVSLUTNING SECTION.                                                    
024100                                                                          
024200     CLOSE W37164                                                         
024300                                                                          
024400     MOVE 'S' TO POSTSUM-OPKOD                                            
024500     CALL POSTSUM USING POSTSUM-PARM                                      
024600     .                                                                    
024700     EJECT                                                                
024800 X-TAG-CHECKPOINT   SECTION.                                              
024900                                                                          
025000     PERFORM IMS-CHECKPOINT                                               
025100     MOVE ZERO TO CHKP-ANT                                                
025200     .                                                                    
025300     EJECT                                                                
025400 IMS-RESTART SECTION.                                                     
025500                                                                          
025600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025700     MOVE '  ' TO GODK-STATUSKODER                                        
025800     CALL CBLTDLI USING XRST MSG-PCB                                      
025900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026000                        CHKP-AREA-LENGTH CHKP-AREA                        
026100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026200     PERFORM IMS-STATUSKONTROLL                                           
026300     .                                                                    
026400     SKIP3                                                                
026500 IMS-CHECKPOINT SECTION.                                                  
026600                                                                          
026700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026800     MOVE '  XD' TO GODK-STATUSKODER                                      
026900     CALL CBLTDLI USING CHKP MSG-PCB                                      
027000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027100                        CHKP-AREA-LENGTH CHKP-AREA                        
027200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027300     PERFORM IMS-STATUSKONTROLL                                           
027400                                                                          
027500     IF IMS-EJ-OK                                                         
027600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
027700       DISPLAY FELTEXT                                                    
027800       CALL FELLOG                                                        
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200* - - - - - - - - - - - - - *                                             
028300*    OPERATIONER MOT WDK6   *                                             
028400* - - - - - - - - - - - - - *                                             
028500 IMS-GHU-WDK601 SECTION.                                                  
028600                                                                          
028700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
028800             DELIMITED BY SIZE INTO SSA1                                  
029000     MOVE   '  '           TO GODK-STATUSKODER                            
029100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
029200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
029300     PERFORM IMS-STATUSKONTROLL                                           
029400     .                                                                    
029500     SKIP2                                                                
029600 IMS-GHNP-WDK611 SECTION.                                                 
029700                                                                          
029800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
029900     DELIMITED BY SIZE INTO SSA1                                          
030000     MOVE   '  '           TO GODK-STATUSKODER                            
030100     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
030200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
033000     SKIP2                                                                
033900 IMS-REPL-WDK611 SECTION.                                                 
034000                                                                          
034100     MOVE '  '             TO GODK-STATUSKODER                            
034200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
034300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
034400     PERFORM IMS-STATUSKONTROLL                                           
034500     .                                                                    
034800     EJECT                                                                
034900 IMS-STATUSKONTROLL SECTION.                                              
035000                                                                          
035100     SET STATUS-IX TO 1                                                   
035200     SEARCH GODK-STATUS AT END CALL FELLOG                                
035300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
035400     CONTINUE                                                             
035500     END-SEARCH                                                           
035600     .                                                                    
