000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2242200.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   FEB-13.                                                  
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD ANSKAFFARE PÅ WDK7                          
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDK7                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- ANSKAFFARE SOM SKA FÖRÄNDRAS                               
003000     SELECT W22421                     ASSIGN TO W22422D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22421                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W22421      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2242200'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005000     SKIP2                                                                
005100 01  CHKP-VAR.                                                            
005200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005700 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005800                                                                          
005900                                                                          
006000                                                                          
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400                                                                          
006500 77  W22421-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W22421                       VALUE 'J'.                   
006700                                                                          
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800*                                                                         
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     EJECT                                                                
008210*01    -COPY WWDC99                                                       
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  IN-AREA-START               PIC X(24)   VALUE                        
008800                                             'IN-AREA-START'.             
008900     SKIP2                                                                
009000                                                                          
009100*01  AREA -COPY W22421     -PRE IN-                                       
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009900     03  W-IDDC-X.                                                        
010000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010100     03  W-KDSEGKEY-X.                                                    
010200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
010300                                                                          
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800 01  SSA3                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500     SKIP3                                                                
012600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
012700 01  DLI-IO-WDK701.                                                       
012800*    03  -COPY WDK701                                                     
012900     EJECT                                                                
013000                                                                          
013100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
013200 01  DLI-IO-WDK711.                                                       
013300*    03  -COPY WDK711                                                     
013400     EJECT                                                                
013500                                                                          
013600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722'.                      
013700 01  DLI-IO-WDK722.                                                       
013800*    03  -COPY WDK722                                                     
013900     EJECT                                                                
014000                                                                          
014010 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
014020 01  DLI-IO-WDK601.                                                       
014030*    03  -COPY WDK601                                                     
014040     EJECT                                                                
014050                                                                          
014060 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
014070 01  DLI-IO-WDK611.                                                       
014080*    03  -COPY WDK611                                                     
014090     EJECT                                                                
014091                                                                          
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300*01  -COPY W0009   -PRE MSG-                                              
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE WDK7-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014710*01  -COPY W0008  -PRE WDK6-                                              
014720     05  FILLER                  PIC X.                                   
014730     EJECT                                                                
014800 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDK6-PCB.                     
014900     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDK6-PCB.                     
015000                                                                          
015100     PERFORM A-INIT                                                       
015200     PERFORM S01-LAES-W22421                                              
015300     PERFORM UNTIL END-OF-W22421                                          
015400                                                                          
015500       PERFORM B-BEHANDLA-POSTER                                          
015600                                                                          
015700       PERFORM S01-LAES-W22421                                            
015800                                                                          
015900     END-PERFORM                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700     SKIP2                                                                
016800                                                                          
016900     ACCEPT DAGENS-DATUM FROM DATE                                        
017000                                                                          
017100     PERFORM IMS-RESTART                                                  
017200                                                                          
017300     OPEN INPUT W22421                                                    
017400                                                                          
017500                                                                          
017600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017700     .                                                                    
017800     EJECT                                                                
017900 B-BEHANDLA-POSTER SECTION.                                               
018000                                                                          
018600     IF CHKP-ANT > CHKP-MAX                                               
018700       PERFORM X-TAG-CHECKPOINT                                           
018800     END-IF                                                               
018900     MOVE IN-IDARTNR TO W-IDARTNR                                         
019000     MOVE IN-IDDC    TO W-IDDC                                            
019100                        WS-IDDC                                           
019300                                                                          
019400     IF CDC-SE                                                            
019410       PERFORM IMS-GHU-WDK611                                             
019420       IF SEGMENT-FINNS                                                   
019430         MOVE IN-IDANSK-NEW TO CLAG-IDANSK                                
019440         PERFORM IMS-REPL-WDK611                                          
019450       END-IF                                                             
019500     ELSE                                                                 
021000       PERFORM IMS-GHU-WDK722                                             
021100       IF SEGMENT-FINNS                                                   
021200         MOVE IN-IDANSK-NEW TO XLAG-IDANSK                                
021300         PERFORM IMS-REPL-WDK722                                          
021400       END-IF                                                             
021500     END-IF                                                               
022300                                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800     CLOSE W22421                                                         
022900     SKIP2                                                                
023000     MOVE 'S' TO POSTSUM-OPKOD                                            
023100     CALL POSTSUM USING POSTSUM-PARM                                      
023200                                                                          
023300     .                                                                    
023400     EJECT                                                                
023500 S01-LAES-W22421  SECTION.                                                
023600     SKIP2                                                                
023700     READ W22421 INTO IN-AREA                                             
023800     AT END                                                               
023900        SET END-OF-W22421 TO TRUE                                         
024000                                                                          
024100     NOT AT END                                                           
024200        MOVE 'W22421' TO POSTSUM-FDNAMN                                   
024300        MOVE 'W22422D1' TO POSTSUM-DDNAMN2                                
024400        CALL POSTSUM USING POSTSUM-PARM                                   
024500     END-READ                                                             
024600     .                                                                    
024700     EJECT                                                                
024800 X-TAG-CHECKPOINT   SECTION.                                              
024900                                                                          
025200     PERFORM IMS-CHECKPOINT                                               
025300     MOVE ZERO TO CHKP-ANT                                                
025500     .                                                                    
025600     EJECT                                                                
025700* --- IMS SEKTIONER ---                                                   
025800     SKIP3                                                                
025900     EJECT                                                                
026000 IMS-RESTART SECTION.                                                     
026100     SKIP2                                                                
026200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026300     MOVE '  ' TO GODK-STATUSKODER                                        
026400     CALL CBLTDLI USING XRST MSG-PCB                                      
026500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026600                        CHKP-AREA-LENGTH CHKP-AREA                        
026700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026800     PERFORM IMS-STATUSKONTROLL                                           
026900     .                                                                    
027000     EJECT                                                                
027100 IMS-CHECKPOINT SECTION.                                                  
027200     SKIP2                                                                
027300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027400     MOVE '  XD' TO GODK-STATUSKODER                                      
027500     CALL CBLTDLI USING CHKP MSG-PCB                                      
027600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027700                        CHKP-AREA-LENGTH CHKP-AREA                        
027800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027900     PERFORM IMS-STATUSKONTROLL                                           
028000                                                                          
028100     IF IMS-EJ-OK                                                         
028200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028300       DISPLAY FELTEXT                                                    
028400       CALL FELLOG                                                        
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
030800 IMS-GHU-WDK722 SECTION.                                                  
030900                                                                          
031000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
031100          DELIMITED BY SIZE INTO SSA1                                     
031200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
031300          DELIMITED BY SIZE INTO SSA2                                     
031400     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
031500          DELIMITED BY SIZE INTO SSA3                                     
031600     MOVE '  ' TO GODK-STATUSKODER                                        
031700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
031800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     .                                                                    
032100     EJECT                                                                
033000 IMS-REPL-WDK722 SECTION.                                                 
033100                                                                          
033200     MOVE '  ' TO GODK-STATUSKODER                                        
033300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
033400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
033500     PERFORM IMS-STATUSKONTROLL                                           
033510     ADD +1 TO CHKP-ANT                                                   
033600     .                                                                    
033700     EJECT                                                                
033710 IMS-GHU-WDK611 SECTION.                                                  
033720                                                                          
033730     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
033740          DELIMITED BY SIZE INTO SSA1                                     
033750     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
033760          DELIMITED BY SIZE INTO SSA2                                     
033790     MOVE '  ' TO GODK-STATUSKODER                                        
033791     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
033792     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033793     PERFORM IMS-STATUSKONTROLL                                           
033794     .                                                                    
033795     EJECT                                                                
033796 IMS-REPL-WDK611 SECTION.                                                 
033797                                                                          
033798     MOVE '  ' TO GODK-STATUSKODER                                        
033799     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
033800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033801     PERFORM IMS-STATUSKONTROLL                                           
033802     ADD +1 TO CHKP-ANT                                                   
033803     .                                                                    
033804     EJECT                                                                
033810 IMS-STATUSKONTROLL SECTION.                                              
033900     SKIP2                                                                
034000     SET STATUS-IX TO 1                                                   
034100     SEARCH GODK-STATUS                                                   
034200       AT END                                                             
034300         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
034400         DISPLAY FELTEXT                                                  
034500         CALL FELLOG                                                      
034600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034700         CONTINUE                                                         
034800     END-SEARCH                                                           
034900     .                                                                    
