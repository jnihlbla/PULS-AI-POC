000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2218500.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   03/05/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ENGÅNGSKÖRNINGSDATUM SOM                                         
001000*        PASSERATS, NOLLAS                                                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDR2                                       
001300*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL NOLLA 2216-DATUM                                     
002400     SELECT W22184                     ASSIGN TO W22185D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W22184                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W22184      -L.                                                
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W2218500'.            
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 77  W22184-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W22184                       VALUE 'J'.                   
005500                                                                          
005600 01  ARBAREOR.                                                            
005700     03  W-W22184-KVPOST-IN      PIC S9(5)   VALUE ZERO COMP-3.           
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007400     EJECT                                                                
007500 01  IN-AREA-START               PIC X(24)   VALUE                        
007600                                             'IN-AREA-START'.             
007700     SKIP2                                                                
007800                                                                          
007900*01  AREA -COPY W22184     -PRE IN-                                       
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008500     03  W-WDGXKEY-X.                                                     
008600         05  FILLER              PIC  X(4)  VALUE '2215'.                 
008700         05  FILLER              PIC  X(26) VALUE LOW-VALUE.              
008800     03  W-IDLEVNR-X.                                                     
008900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009000     SKIP2                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009700     88  IMS-EJ-OK                           VALUE 'XD'.                  
009800     SKIP2                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900                                                                          
011000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
011100 01  DLI-IO-WDGX01.                                                       
011200*    03  -COPY WDGX01                                                     
011300     EJECT                                                                
011400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2216'.                    
011500 01  DLI-IO-WDGX2216.                                                     
011600*    03  -COPY WDGX2216                                                   
011700                                                                          
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000                                                                          
012100*01  -COPY W0009   -PRE MSG-                                              
012200                                                                          
012300*01  -COPY W0008  -PRE XXBK-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING MSG-PCB XXBK-PCB.                              
012700 MAIN SECTION.                                                            
012800     ENTRY 'DLITCBL' USING MSG-PCB XXBK-PCB.                              
012900                                                                          
013000     SKIP2                                                                
013100     PERFORM A-INIT                                                       
013200     PERFORM S01-LAES-W22184                                              
013300     PERFORM UNTIL END-OF-W22184                                          
013400       IF CHKP-ANT > CHKP-MAX                                             
013500         PERFORM X-TAG-CHECKPOINT                                         
013600       END-IF                                                             
013700       MOVE IN-IDLEVNR TO W-IDLEVNR                                       
013800       PERFORM IMS-GET-WDGX2216                                           
013900       IF SEGMENT-FINNS                                                   
014000          MOVE IN-IDOVERFNR    TO 2216-IDOVERFNR                          
014010          MOVE IN-TISEND-PER   TO 2216-TISEND-PER                         
014020          MOVE IN-FLLEVPLP     TO 2216-FLLEVPLP                           
014030          MOVE IN-FLLEVVB      TO 2216-FLLEVVB                            
014100          PERFORM IMS-REPL-WDGX2216                                       
014200       END-IF                                                             
014300       PERFORM S01-LAES-W22184                                            
014400     END-PERFORM                                                          
014500                                                                          
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-INIT SECTION.                                                          
015400     SKIP2                                                                
015500                                                                          
015600     PERFORM IMS-RESTART                                                  
015700                                                                          
015800     OPEN INPUT W22184                                                    
015900                                                                          
016000                                                                          
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016200     .                                                                    
016300     EJECT                                                                
016400 Z-FINIT SECTION.                                                         
016500                                                                          
016600                                                                          
016700     CLOSE W22184                                                         
016800     SKIP2                                                                
016900     MOVE 'S' TO POSTSUM-OPKOD                                            
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     .                                                                    
017200     EJECT                                                                
017300 S01-LAES-W22184  SECTION.                                                
017400     SKIP2                                                                
017500     READ W22184 INTO IN-AREA                                             
017600     AT END                                                               
017700        SET END-OF-W22184 TO TRUE                                         
017800                                                                          
017900     NOT AT END                                                           
018000        MOVE 'W22184'   TO POSTSUM-FDNAMN                                 
018100        MOVE 'W22185D1' TO POSTSUM-DDNAMN2                                
018200        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
018300        CALL POSTSUM USING POSTSUM-PARM                                   
018400                                                                          
018500        ADD 1 TO W-W22184-KVPOST-IN                                       
018600     END-READ                                                             
018700     .                                                                    
018800     EJECT                                                                
018900 X-TAG-CHECKPOINT   SECTION.                                              
019000                                                                          
019100     PERFORM IMS-CHECKPOINT                                               
019200     MOVE ZERO TO CHKP-ANT                                                
019300                                                                          
019900     .                                                                    
020000     EJECT                                                                
020100* --- IMS SEKTIONER ---                                                   
020200                                                                          
020300 IMS-GET-WDGX2216 SECTION.                                                
020400                                                                          
020500     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-X ')'                         
020600          DELIMITED BY SIZE INTO SSA1                                     
020700     STRING 'WLXXBK11(IDLEVNR  =' W-IDLEVNR-X ')'                         
020800          DELIMITED BY SIZE INTO SSA2                                     
020900     MOVE '  GE' TO GODK-STATUSKODER                                      
021000     CALL CBLTDLI USING GHU XXBK-PCB DLI-IO-WDGX2216 SSA1 SSA2            
021100     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
021200     PERFORM IMS-STATUSKONTROLL                                           
021300     .                                                                    
021400     SKIP3                                                                
021500 IMS-REPL-WDGX2216 SECTION.                                               
021600                                                                          
021700     MOVE '  ' TO GODK-STATUSKODER                                        
021800     CALL CBLTDLI USING REPL XXBK-PCB DLI-IO-WDGX2216                     
021900     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
022000     PERFORM IMS-STATUSKONTROLL                                           
022100                                                                          
022200     ADD +1 TO CHKP-ANT                                                   
022300     .                                                                    
022400     EJECT                                                                
022500 IMS-RESTART SECTION.                                                     
022600     SKIP2                                                                
022700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022800     MOVE '  ' TO GODK-STATUSKODER                                        
022900     CALL CBLTDLI USING XRST MSG-PCB                                      
023000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023100                        CHKP-AREA-LENGTH CHKP-AREA                        
023200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023300     PERFORM IMS-STATUSKONTROLL                                           
023400     .                                                                    
023500     SKIP3                                                                
023600 IMS-CHECKPOINT SECTION.                                                  
023700     SKIP2                                                                
023800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023900     MOVE '  XD' TO GODK-STATUSKODER                                      
024000     CALL CBLTDLI USING CHKP MSG-PCB                                      
024100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024200                        CHKP-AREA-LENGTH CHKP-AREA                        
024300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024400     PERFORM IMS-STATUSKONTROLL                                           
024500                                                                          
024600     IF IMS-EJ-OK                                                         
024700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024800       DISPLAY FELTEXT                                                    
024900       CALL FELLOG                                                        
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 IMS-STATUSKONTROLL SECTION.                                              
025400     SKIP2                                                                
025500     SET STATUS-IX TO 1                                                   
025600     SEARCH GODK-STATUS                                                   
025700       AT END                                                             
025800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025900           DELIMITED BY SIZE INTO FELTEXT                                 
026000         DISPLAY FELTEXT                                                  
026100         CALL FELLOG                                                      
026200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026300         CONTINUE                                                         
026400     END-SEARCH                                                           
026500     .                                                                    
