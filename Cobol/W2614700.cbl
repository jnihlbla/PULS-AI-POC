000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2614700.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   09/02/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        KONTROLLERAR OM WLC EJ LÄNGRE GÄLLER                             
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK6                                       
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- ARTIKLAR EV. WARNING LAST CALL NY                          
002300     SELECT W26146I                    ASSIGN TO W26147D1.                
002400*          --- ARTIKLAR EV. WARNING LAST CALL GAMMAL                      
002500     SELECT W26146R                    ASSIGN TO W26147D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W26146I                                                              
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  I -COPY W26144   -L.                                                 
003600     SKIP3                                                                
003700 FD  W26146R                                                              
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  R -COPY W26144   -L.                                                 
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W2614700'.            
004600 01  CHKP-VAR.                                                            
004700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005200     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900 77  W26146I-EOF-SW              PIC X       VALUE 'N'.                   
006000     88  END-OF-W26146I                      VALUE 'J'.                   
006100 77  W26146R-EOF-SW              PIC X       VALUE 'N'.                   
006200     88  END-OF-W26146R                      VALUE 'J'.                   
006300     EJECT                                                                
006400*      --- VALID IDDC CODES                                               
006500*                                                                         
006600*01    -COPY WWDCKONS                                                     
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP3                                                                
007400                                                                          
007500 01  W-DATUM.                                                             
007600     05  W-DATUM-DATE    PIC X(6).                                        
007700     EJECT                                                                
007800*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
007900                                                                          
008000*01  -COPY WDATAREA                                                       
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  IN-AREA-START               PIC X(24)   VALUE                        
008700                                             'IN-AREA-START'.             
008800     SKIP2                                                                
008900                                                                          
009000*01  AREA -COPY W26144     -PRE IN-                                       
009100*                                                                         
009200     EJECT                                                                
009300 01  REG-AREA-START              PIC X(24)   VALUE                        
009400                                            'REG-AREA-START'.             
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W26144     -PRE REG-                                      
009800*                                                                         
009900     EJECT                                                                
010000                                                                          
010100*01  AREA -COPY W26144     -PRE UT-                                       
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010800     03  W-KDSEGKEY-X.                                                    
010900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011000                                                                          
011100 01  W-IDARTNR-STR-X.                                                     
011200     03  W-IDARTNR-STR        PIC S9(9) COMP-3 VALUE ZERO.                
011300                                                                          
011400     EJECT                                                                
011500     SKIP2                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012200     88  IMS-EJ-OK                           VALUE 'XD'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(128).                              
012800 01  SSA2                        PIC X(64).                               
012900 01  SSA3                        PIC X(64).                               
013000 01  SSA4                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013800 01  DLI-IO-WDK601.                                                       
013900*    03  -COPY WDK601                                                     
014000     EJECT                                                                
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014200 01  DLI-IO-WDK611.                                                       
014300*    03  -COPY WDK611                                                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800                                                                          
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING MSG-PCB                                        
015400           WDK6-PCB.                                                      
015500 MAIN SECTION.                                                            
015600     ENTRY 'DLITCBL' USING MSG-PCB                                        
015700           WDK6-PCB.                                                      
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W26146I                                             
016200     PERFORM S02-LAES-W26146R                                             
016300                                                                          
016400     PERFORM UNTIL END-OF-W26146R                                         
016500       IF CHKP-ANT > CHKP-MAX                                             
016600         PERFORM X-TAG-CHECKPOINT                                         
016700       END-IF                                                             
016800                                                                          
016900        IF IN-IDARTNR = REG-IDARTNR                                       
017000           PERFORM S01-LAES-W26146I                                       
017100           PERFORM S02-LAES-W26146R                                       
017200        ELSE                                                              
017300           IF IN-IDARTNR > REG-IDARTNR                                    
017310*****         SKROTPOLICY-ARTIKEL EJ LÄNGRE MED                           
017320*****         TAG BORT MÄRKNING TISKROT-BEV OCH WLC                       
017330              PERFORM D-UPPDATERA                                         
017400              PERFORM S02-LAES-W26146R                                    
017500           ELSE                                                           
017600*****         IN-IDARTNR < REG-IDARTNR                                    
018000              PERFORM S01-LAES-W26146I                                    
018100           END-IF                                                         
018200        END-IF                                                            
018300     END-PERFORM                                                          
018400                                                                          
018500     PERFORM Z-FINIT                                                      
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 A-INIT SECTION.                                                          
019200     SKIP2                                                                
019300                                                                          
019400     PERFORM IMS-RESTART                                                  
019500                                                                          
019600     OPEN INPUT W26146I                                                   
019700                W26146R                                                   
019800                                                                          
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100                                                                          
020200     .                                                                    
020300     EJECT                                                                
020400 D-UPPDATERA SECTION.                                                     
020500                                                                          
020600     MOVE REG-IDARTNR TO W-IDARTNR                                        
020700                                                                          
020800     PERFORM IMS-GET-WDK601                                               
020900     IF SEGMENT-FINNS                                                     
021000        PERFORM IMS-GET-WDK611                                            
021010       IF SEGMENT-FINNS                                                   
021100        MOVE NEJ         TO CLAG-FLSKROT-WLC                              
021200        MOVE NEJ         TO CLAG-FLSKROT-BEV                              
021300        MOVE ZERO        TO CLAG-TISKPREL                                 
021400                                                                          
021500        PERFORM IMS-REPL-WDK611                                           
021600       END-IF                                                             
021610     END-IF                                                               
021700                                                                          
021800     .                                                                    
021900     EJECT                                                                
022000 Z-FINIT SECTION.                                                         
022100                                                                          
022200                                                                          
022300     CLOSE W26146I                                                        
022400           W26146R                                                        
022500     SKIP2                                                                
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900     EJECT                                                                
023000 S01-LAES-W26146I SECTION.                                                
023100     SKIP2                                                                
023200     READ W26146I INTO IN-AREA                                            
023300     AT END                                                               
023400        SET END-OF-W26146I TO TRUE                                        
023410        MOVE 999999999     TO IN-IDARTNR                                  
023500                                                                          
023600     NOT AT END                                                           
023700        MOVE 'W26146I'  TO POSTSUM-FDNAMN                                 
023800        MOVE 'W26147D1' TO POSTSUM-DDNAMN2                                
023900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
024000        CALL POSTSUM USING POSTSUM-PARM                                   
024100                                                                          
024200     END-READ                                                             
024300     .                                                                    
024400     EJECT                                                                
024500 S02-LAES-W26146R SECTION.                                                
024600     SKIP2                                                                
024700     READ W26146R INTO REG-AREA                                           
024800     AT END                                                               
024900        SET END-OF-W26146R TO TRUE                                        
024910        MOVE 999999999     TO REG-IDARTNR                                 
025000                                                                          
025100     NOT AT END                                                           
025200        MOVE 'W26146R'  TO POSTSUM-FDNAMN                                 
025300        MOVE 'W26147D2' TO POSTSUM-DDNAMN2                                
025400        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
025500        CALL POSTSUM USING POSTSUM-PARM                                   
025600                                                                          
025700     END-READ                                                             
025800     .                                                                    
025900     EJECT                                                                
026000 X-TAG-CHECKPOINT   SECTION.                                              
026100                                                                          
026200     PERFORM IMS-CHECKPOINT                                               
026300     MOVE ZERO TO CHKP-ANT                                                
026400     .                                                                    
026500     EJECT                                                                
026600* --- IMS SEKTIONER ---                                                   
026700                                                                          
026800 IMS-GET-WDK601 SECTION.                                                  
026900                                                                          
027000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
027100          DELIMITED BY SIZE INTO SSA1                                     
027200     MOVE '  GE' TO GODK-STATUSKODER                                      
027300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
027400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027500     PERFORM IMS-STATUSKONTROLL                                           
027600     .                                                                    
027700     EJECT                                                                
027800 IMS-GET-WDK611 SECTION.                                                  
027900                                                                          
028000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
028100          DELIMITED BY SIZE INTO SSA1                                     
028200     MOVE '  GE' TO GODK-STATUSKODER                                      
028300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
028400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028500     PERFORM IMS-STATUSKONTROLL                                           
028600     .                                                                    
028700     SKIP3                                                                
028800 IMS-REPL-WDK611 SECTION.                                                 
028900                                                                          
029000     MOVE '  ' TO GODK-STATUSKODER                                        
029100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
029200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
029300     PERFORM IMS-STATUSKONTROLL                                           
029400     ADD +1  TO CHKP-ANT                                                  
029500     .                                                                    
029600     EJECT                                                                
029700 IMS-RESTART SECTION.                                                     
029800     SKIP2                                                                
029900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030000     MOVE '  ' TO GODK-STATUSKODER                                        
030100     CALL CBLTDLI USING XRST MSG-PCB                                      
030200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
030300                        CHKP-AREA-LENGTH CHKP-AREA                        
030400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030500     PERFORM IMS-STATUSKONTROLL                                           
030600     .                                                                    
030700     SKIP3                                                                
030800 IMS-CHECKPOINT SECTION.                                                  
030900     SKIP2                                                                
031000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031100     MOVE '  XD' TO GODK-STATUSKODER                                      
031200     CALL CBLTDLI USING CHKP MSG-PCB                                      
031300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031400                        CHKP-AREA-LENGTH CHKP-AREA                        
031500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031600     PERFORM IMS-STATUSKONTROLL                                           
031700                                                                          
031800     IF IMS-EJ-OK                                                         
031900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
032000       DISPLAY FELTEXT                                                    
032100       CALL FELLOG                                                        
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 IMS-STATUSKONTROLL SECTION.                                              
032600     SKIP2                                                                
032700     SET STATUS-IX TO 1                                                   
032800     SEARCH GODK-STATUS                                                   
032900       AT END                                                             
033000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033100           DELIMITED BY SIZE INTO FELTEXT                                 
033200         DISPLAY FELTEXT                                                  
033300         CALL FELLOG                                                      
033400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
