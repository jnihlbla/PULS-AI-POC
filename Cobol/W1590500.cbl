000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1590500.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   02/03/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDD301 MED JA I BEN-FLAENDR                
001100*        FÖR BENÄMNINGAR SOM HÖR TILL NYTILLKOMNA ARTIKLAR I NEVIS        
001200*                                                                         
001300*        ARTIKLAR SOM BYTT BENÄMNING FÅR FLAGGOR SATTA I 1131             
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- ALLA ARTIKLAR SOM ÄR NYA FÖR NEVIS                         
002300     SELECT W15905                     ASSIGN TO W15905D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W15905                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200 01  IN-POST                 PIC X(15).                                   
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W1590500'.            
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600     SKIP2                                                                
004700 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100 77  W15905-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W15905                       VALUE 'J'.                   
005300     EJECT                                                                
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900     EJECT                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  IN-AREA-START               PIC X(24)   VALUE                        
007100                                             'IN-AREA-START'.             
007200     SKIP2                                                                
007300                                                                          
007400 01  IN-AREA.                                                             
007500     03 IN-IDARTNR               PIC 9(9).                                
007600     03 FILLER                   PIC X(6).                                
007700                                                                          
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008400     03  W-IDARTNR-X.                                                     
008500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400                                                                          
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
010600 01  DLI-IO-WDD301.                                                       
010700*    03  -COPY WDD301                                                     
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009   -PRE MSG-                                              
011200                                                                          
011300*01  -COPY W0008  -PRE WDD3-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB WDD3-PCB.                              
011700 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING MSG-PCB WDD3-PCB.                              
011900                                                                          
012000     SKIP2                                                                
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     PERFORM S01-LAES-W15905                                              
012400     PERFORM UNTIL END-OF-W15905                                          
012500       MOVE IN-IDARTNR TO W-IDARTNR                                       
012600                                                                          
012700       PERFORM IMS-GHU-WDD301-BSEQ                                        
012800       IF SEGMENT-FINNS                                                   
013100         IF BEN-FLAENDR = NEJ                                             
013200           MOVE JA TO BEN-FLAENDR                                         
013300           PERFORM IMS-REPL-WDD301                                        
013400           ADD +1 TO CHKP-ANT                                             
013500         END-IF                                                           
013600       END-IF                                                             
013700                                                                          
013800       IF CHKP-ANT > CHKP-MAX                                             
013900         PERFORM X-TAG-CHECKPOINT                                         
014000       END-IF                                                             
014100                                                                          
014200       PERFORM S01-LAES-W15905                                            
014300     END-PERFORM                                                          
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200     SKIP2                                                                
015300                                                                          
015400     PERFORM IMS-RESTART                                                  
015500                                                                          
015600     OPEN INPUT W15905                                                    
015700                                                                          
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016200                                                                          
016300     CLOSE W15905                                                         
016400                                                                          
016500     SKIP2                                                                
016600     MOVE 'S' TO POSTSUM-OPKOD                                            
016700     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016900     EJECT                                                                
017000 S01-LAES-W15905  SECTION.                                                
017100     SKIP2                                                                
017200     READ W15905 INTO IN-AREA                                             
017300     AT END                                                               
017400        MOVE 999999999  TO IN-IDARTNR                                     
017500        SET END-OF-W15905 TO TRUE                                         
017600                                                                          
017700     NOT AT END                                                           
017800        MOVE 'W15905' TO POSTSUM-FDNAMN                                   
017900        MOVE 'W15905D1' TO POSTSUM-DDNAMN2                                
018000        MOVE SPACE     TO POSTSUM-TRANSTYP                                
018100        CALL POSTSUM USING POSTSUM-PARM                                   
018200                                                                          
018300     END-READ                                                             
018400     .                                                                    
018500     EJECT                                                                
018600 X-TAG-CHECKPOINT   SECTION.                                              
018700                                                                          
018800     PERFORM IMS-CHECKPOINT                                               
018900     MOVE ZERO TO CHKP-ANT                                                
019000     .                                                                    
019100     EJECT                                                                
019200* --- IMS SEKTIONER ---                                                   
019300                                                                          
019400     EJECT                                                                
019500 IMS-GHU-WDD301-BSEQ SECTION.                                             
019600                                                                          
019700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
019800          DELIMITED BY SIZE INTO SSA1                                     
019900     MOVE '  GE' TO GODK-STATUSKODER                                      
020000     CALL CBLTDLI USING GHU  WDD3-PCB DLI-IO-WDD301 SSA1                  
020100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400     SKIP3                                                                
020500 IMS-REPL-WDD301 SECTION.                                                 
020600                                                                          
020700     MOVE '  ' TO GODK-STATUSKODER                                        
020800     CALL CBLTDLI USING REPL WDD3-PCB DLI-IO-WDD301                       
020900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
021000     PERFORM IMS-STATUSKONTROLL                                           
021100     .                                                                    
021200     EJECT                                                                
022300 IMS-RESTART SECTION.                                                     
022400     SKIP2                                                                
022500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022600     MOVE '  ' TO GODK-STATUSKODER                                        
022700     CALL CBLTDLI USING XRST MSG-PCB                                      
022800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022900                        CHKP-AREA-LENGTH CHKP-AREA                        
023000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023100     PERFORM IMS-STATUSKONTROLL                                           
023200     .                                                                    
023300     SKIP3                                                                
023400 IMS-CHECKPOINT SECTION.                                                  
023500     SKIP2                                                                
023600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023700     MOVE '  XD' TO GODK-STATUSKODER                                      
023800     CALL CBLTDLI USING CHKP MSG-PCB                                      
023900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024000                        CHKP-AREA-LENGTH CHKP-AREA                        
024100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024200     PERFORM IMS-STATUSKONTROLL                                           
024300                                                                          
024400     IF IMS-EJ-OK                                                         
024500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024600       DISPLAY FELTEXT                                                    
024700       CALL FELLOG                                                        
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-STATUSKONTROLL SECTION.                                              
025200     SKIP2                                                                
025300     SET STATUS-IX TO 1                                                   
025400     SEARCH GODK-STATUS                                                   
025500       AT END                                                             
025600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025700           DELIMITED BY SIZE INTO FELTEXT                                 
025800         DISPLAY FELTEXT                                                  
025900         CALL FELLOG                                                      
026000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026100         CONTINUE                                                         
026200     END-SEARCH                                                           
026300     .                                                                    
