000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1228200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   95/07/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER 2 INFILER SKAPADE I PGM W12204.                 
000900*           W12207 MED ARTIKLAR SOM SKA RENSAS/SKALAS WDK6                
001000*              PTYP = B = ARTIKEL TAS BORT WDL7                           
001100*              PTYP = S = ARTIKEL TAS BORT EFTER KONTROLL                 
001200*                         ATT ORDERINGGÅNG = 0                            
001300*           W12203 MED ARTIKLAR SOM REDAN ÄR SKALADE                      
001400*                         ARTIKEL TAS BORT EFTER KONTROLL                 
001500*                         ATT ORDERINGGÅNG = 0                            
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLOIGA (WDL7)                              
001800*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ARTIKLAR SOM SKA RENSAS/SKALAS WDK6                        
002700     SELECT W12207                     ASSIGN TO W12282D1.                
002800     SKIP2                                                                
002900*          --- REDAN SKALADE ARTIKLAR WDK6                                
003000     SELECT W12203                     ASSIGN TO W12282D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W12207                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  -COPY W12207      -L.                                                
004000     SKIP3                                                                
004100 FD  W12203                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400*01  -COPY W12203      -L.                                                
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W1228200'.            
005100 01  CHKP-VAR.                                                            
005200 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005500 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005600 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005700 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  SW-KVOI-TRAEFF              PIC X       VALUE 'N'.                   
006100 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  RULL-IX-MAX                 PIC S9(3)   VALUE 53   COMP-3.           
006300 77  INNEV-IX-MAX                PIC S9(3)   VALUE 5    COMP-3.           
006400 77  FOREG-IX-MAX                PIC S9(3)   VALUE 2    COMP-3.           
006500 77  W-DLET-WDL701               PIC 9(7)    VALUE ZERO.                  
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000                                                                          
007100 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-W12207                       VALUE 'J'.                   
007300                                                                          
007400 77  W12203-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W12203                       VALUE 'J'.                   
007600     SKIP3                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  IN07-AREA-START             PIC X(24)   VALUE                        
008700                                             '07-AREA-START'.             
008800                                                                          
008900*01  AREA -COPY W12207     -PRE IN07-                                     
009000     EJECT                                                                
009100 01  IN03-AREA-START             PIC X(24)   VALUE                        
009200                                             '03-AREA-START'.             
009300*01  AREA -COPY W12203     -PRE IN03-                                     
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  NYCKLAR-TILL-DLI.                                                    
009800     03  W-IDARTNR-X.                                                     
009900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010000     SKIP2                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010700     88  IMS-EJ-OK                           VALUE 'XD'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012000     SKIP3                                                                
012100 01  DLI-IO-AREA.                                                         
012200     03  IO-AREA                 PIC X(1500)  VALUE SPACE.                
012300     SKIP3                                                                
012400     03  WLOIGA01 REDEFINES IO-AREA.                                      
012500*        05  -COPY WDL701                                                 
012600     SKIP3                                                                
012700     03  WLOIGA11 REDEFINES IO-AREA.                                      
012800*        05  -COPY WDL711                                                 
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100*01  -COPY W0009   -PRE MSG-                                              
013200                                                                          
013300*01  -COPY W0008  -PRE OIGA-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB OIGA-PCB.                              
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB OIGA-PCB.                              
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-LAES-W12207                                              
014300     PERFORM UNTIL END-OF-W12207                                          
014400       PERFORM B-BEHANDLA-W12207                                          
014500       IF CHKP-ANT > CHKP-MAX                                             
014600         PERFORM X-TAG-CHECKPOINT                                         
014700       END-IF                                                             
014800       PERFORM S01-LAES-W12207                                            
014900     END-PERFORM                                                          
015000                                                                          
015100     PERFORM S02-LAES-W12203                                              
015200     PERFORM UNTIL END-OF-W12203                                          
015300       PERFORM C-BEHANDLA-W12203                                          
015400       IF CHKP-ANT > CHKP-MAX                                             
015500         PERFORM X-TAG-CHECKPOINT                                         
015600       END-IF                                                             
015700       PERFORM S02-LAES-W12203                                            
015800     END-PERFORM                                                          
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     PERFORM IMS-RESTART                                                  
016900                                                                          
017000     OPEN INPUT W12207                                                    
017100                W12203                                                    
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017300     .                                                                    
017400     EJECT                                                                
017500 B-BEHANDLA-W12207 SECTION.                                               
017600                                                                          
017700     MOVE IN07-IDARTNR TO W-IDARTNR                                       
017800     PERFORM IMS-GET-OIGA01                                               
017900     IF SEGMENT-FINNS                                                     
018000        IF IN07-UTFIL-TYP = 'B'                                           
018100           PERFORM IMS-DLET-OIGA                                          
018200           ADD 1 TO W-DLET-WDL701                                         
018300        ELSE                                                              
018400           IF IN07-UTFIL-TYP = 'S'                                        
018500              PERFORM S11-KOLLA-ORDERINGANG                               
018600              IF SW-KVOI-TRAEFF = NEJ                                     
018700                 PERFORM IMS-GET-OIGA01                                   
018800                 PERFORM IMS-DLET-OIGA                                    
018900                 ADD 1 TO W-DLET-WDL701                                   
019000              END-IF                                                      
019100           END-IF                                                         
019200        END-IF                                                            
019300     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 C-BEHANDLA-W12203 SECTION.                                               
019700                                                                          
019800     MOVE IN03-IDARTNR TO W-IDARTNR                                       
019900     PERFORM IMS-GET-OIGA01                                               
020000     IF SEGMENT-FINNS                                                     
020100        PERFORM S11-KOLLA-ORDERINGANG                                     
020200        IF SW-KVOI-TRAEFF = NEJ                                           
020300           PERFORM IMS-GET-OIGA01                                         
020400           PERFORM IMS-DLET-OIGA                                          
020500           ADD 1 TO W-DLET-WDL701                                         
020600        END-IF                                                            
020700     END-IF                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 Z-FINIT SECTION.                                                         
021100                                                                          
021200     DISPLAY 'ANTAL BORTTAGNA WDL701: ' W-DLET-WDL701                     
021300     CLOSE W12207                                                         
021400           W12203                                                         
021500     MOVE 'S' TO POSTSUM-OPKOD                                            
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
021800     EJECT                                                                
021900 S01-LAES-W12207 SECTION.                                                 
022000                                                                          
022100     READ W12207 INTO IN07-AREA                                           
022200     AT END                                                               
022300        SET END-OF-W12207 TO TRUE                                         
022400     NOT AT END                                                           
022500        MOVE 'W12207'   TO POSTSUM-FDNAMN                                 
022600        MOVE 'W12282D1' TO POSTSUM-DDNAMN2                                
022700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
022800        CALL POSTSUM USING POSTSUM-PARM                                   
022900     END-READ                                                             
023000     .                                                                    
023100     EJECT                                                                
023200 S02-LAES-W12203 SECTION.                                                 
023300                                                                          
023400     READ W12203 INTO IN03-AREA                                           
023500     AT END                                                               
023600        SET END-OF-W12203 TO TRUE                                         
023700     NOT AT END                                                           
023800        MOVE 'W12203'   TO POSTSUM-FDNAMN                                 
023900        MOVE 'W12282D2' TO POSTSUM-DDNAMN2                                
024000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
024100        CALL POSTSUM USING POSTSUM-PARM                                   
024200     END-READ                                                             
024300     .                                                                    
024400     EJECT                                                                
024500 S11-KOLLA-ORDERINGANG SECTION.                                           
024600                                                                          
024700     MOVE NEJ TO SW-KVOI-TRAEFF                                           
024800     PERFORM IMS-GET-OIGA11                                               
024900     PERFORM UNTIL SEGMENT-SAKNAS OR SW-KVOI-TRAEFF = JA                  
025000        MOVE +1 TO IX                                                     
025100        PERFORM UNTIL IX > RULL-IX-MAX                                    
025200           IF DC-KVOI-RULL(IX) = ZERO AND                                 
025300              DC-KVOI-CDC-RULL(IX) = ZERO                                 
025400             CONTINUE                                                     
025500           ELSE                                                           
025600             MOVE JA TO SW-KVOI-TRAEFF                                    
025700           END-IF                                                         
025800           ADD +1 TO IX                                                   
025900        END-PERFORM                                                       
026000                                                                          
026100        MOVE +1 TO IX                                                     
026200        PERFORM UNTIL IX > INNEV-IX-MAX                                   
026300           IF (DC-KVOI-INNEV(IX) + DC-KVOI-PP-INNEV(IX) = ZERO)           
026400          AND (DC-KVOI-CDC-INNEV(IX) = ZERO)                              
026500             CONTINUE                                                     
026600           ELSE                                                           
026700             MOVE JA TO SW-KVOI-TRAEFF                                    
026800           END-IF                                                         
026900           ADD +1 TO IX                                                   
027000        END-PERFORM                                                       
027100                                                                          
027200        PERFORM IMS-GET-OIGA11                                            
027300     END-PERFORM                                                          
027400     .                                                                    
027500     EJECT                                                                
027600 X-TAG-CHECKPOINT SECTION.                                                
027700                                                                          
027800     PERFORM IMS-CHECKPOINT                                               
027900     MOVE ZERO TO CHKP-ANT                                                
028000     .                                                                    
028100     EJECT                                                                
028200* --- IMS SEKTIONER ---                                                   
028300                                                                          
028400 IMS-GET-OIGA01 SECTION.                                                  
028500     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
028600          DELIMITED BY SIZE INTO SSA1                                     
028700     MOVE '  GE' TO GODK-STATUSKODER                                      
028800     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-AREA SSA1                     
028900     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     SKIP3                                                                
029300 IMS-GET-OIGA11 SECTION.                                                  
029400     MOVE 'WLOIGA11 ' TO SSA1                                             
029500     MOVE '  GE' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GHNP OIGA-PCB DLI-IO-AREA SSA1                    
029700     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     SKIP3                                                                
030100 IMS-DLET-OIGA SECTION.                                                   
030200     MOVE '  ' TO GODK-STATUSKODER                                        
030300     CALL CBLTDLI USING DLET OIGA-PCB DLI-IO-AREA                         
030400     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
030500     PERFORM IMS-STATUSKONTROLL                                           
030600     .                                                                    
030700     EJECT                                                                
030800 IMS-RESTART SECTION.                                                     
030900     SKIP2                                                                
031000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031100     MOVE '  ' TO GODK-STATUSKODER                                        
031200     CALL CBLTDLI USING XRST MSG-PCB                                      
031300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031400                        CHKP-AREA-LENGTH CHKP-AREA                        
031500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031600     PERFORM IMS-STATUSKONTROLL                                           
031700     .                                                                    
031800     EJECT                                                                
031900 IMS-CHECKPOINT SECTION.                                                  
032000     SKIP2                                                                
032100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
032200     MOVE '  XD' TO GODK-STATUSKODER                                      
032300     CALL CBLTDLI USING CHKP MSG-PCB                                      
032400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032500                        CHKP-AREA-LENGTH CHKP-AREA                        
032600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032700     PERFORM IMS-STATUSKONTROLL                                           
032800                                                                          
032900     IF IMS-EJ-OK                                                         
033000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
033100       DISPLAY FELTEXT                                                    
033200       CALL FELLOG                                                        
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 IMS-STATUSKONTROLL SECTION.                                              
033700     SKIP2                                                                
033800     SET STATUS-IX TO 1                                                   
033900     SEARCH GODK-STATUS                                                   
034000       AT END                                                             
034100         CALL FELLOG                                                      
034200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034300         CONTINUE                                                         
034400     END-SEARCH                                                           
034500     .                                                                    
