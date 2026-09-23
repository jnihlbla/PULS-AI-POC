000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2728200.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   OKT 2015.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD FLREFBEO PÅ WDK6 FÖR CDC                    
001100*        INGET ÅTERSTARSREGISTER BEHÖVS                                   
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK6                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FLREFBEO SOM SKA FÖRÄNDRAS                                 
002800     SELECT W27281                     ASSIGN TO W27282D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W27281                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W27281      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2728200'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004800     SKIP2                                                                
004900 01  CHKP-VAR.                                                            
005000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005400 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005500 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005600                                                                          
005700                                                                          
005800                                                                          
005900 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W27281-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W27281                       VALUE 'J'.                   
006500                                                                          
006600     EJECT                                                                
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                             'IN-AREA-START'.             
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W27281     -PRE IN-                                       
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300     03  W-IDARTNR-X.                                                     
009400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009500     03  W-KDSEGKEY-X.                                                    
009600         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
009700                                                                          
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FINNS                       VALUE '  '.                  
010200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010500     88  IMS-EJ-OK                           VALUE 'XD'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(128).                              
011100 01  SSA2                        PIC X(128).                              
011200 01  SSA3                        PIC X(128).                              
011300     EJECT                                                                
011400*    --- IMS FUNKTIONSKODER                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
011900 01  DLI-IO-WDK629.                                                       
012000*    03  -COPY WDK629                                                     
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012500     EJECT                                                                
012600*01  -COPY W0008  -PRE WDK6-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
013000     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013100                                                                          
013200     PERFORM A-INIT                                                       
013300     PERFORM S01-LAES-W27281                                              
013400     PERFORM UNTIL END-OF-W27281                                          
013500                                                                          
013600       PERFORM B-BEHANDLA-POSTER                                          
013700                                                                          
013800       PERFORM S01-LAES-W27281                                            
013900                                                                          
014000     END-PERFORM                                                          
014100     PERFORM Z-FINIT                                                      
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800     SKIP2                                                                
014900                                                                          
015000     ACCEPT DAGENS-DATUM FROM DATE                                        
015100                                                                          
015200     PERFORM IMS-RESTART                                                  
015300                                                                          
015400     OPEN INPUT W27281                                                    
015500                                                                          
015600                                                                          
015700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
015900     EJECT                                                                
016000 B-BEHANDLA-POSTER SECTION.                                               
016100                                                                          
016200     IF CHKP-ANT > CHKP-MAX                                               
016300       PERFORM X-TAG-CHECKPOINT                                           
016400     END-IF                                                               
016500     MOVE IN-IDARTNR TO W-IDARTNR                                         
016600     PERFORM IMS-GHU-K629                                                 
016700                                                                          
016800     IF SEGMENT-FINNS                                                     
016900       MOVE IN-FLREFBEO      TO CREF-FLREFBEO                             
017000       PERFORM IMS-REPL-K629                                              
017100       ADD +1 TO CHKP-ANT                                                 
017110     END-IF                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 Z-FINIT SECTION.                                                         
017500                                                                          
017600     CLOSE W27281                                                         
017700     SKIP2                                                                
017800     MOVE 'S' TO POSTSUM-OPKOD                                            
017900     CALL POSTSUM USING POSTSUM-PARM                                      
018000                                                                          
018100     .                                                                    
018200     EJECT                                                                
018300 S01-LAES-W27281  SECTION.                                                
018400     SKIP2                                                                
018500     READ W27281 INTO IN-AREA                                             
018600     AT END                                                               
018700        SET END-OF-W27281 TO TRUE                                         
018800                                                                          
018900     NOT AT END                                                           
019000        MOVE 'W27281' TO POSTSUM-FDNAMN                                   
019100        MOVE 'W27282D1' TO POSTSUM-DDNAMN2                                
019200        CALL POSTSUM USING POSTSUM-PARM                                   
019300     END-READ                                                             
019400     .                                                                    
019500     EJECT                                                                
019600 X-TAG-CHECKPOINT   SECTION.                                              
019700                                                                          
019800* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
019900* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
020000     PERFORM IMS-CHECKPOINT                                               
020100     MOVE ZERO TO CHKP-ANT                                                
020200* --- LÄS OM DATABAS OM DET BEHÖVS                                        
020300     .                                                                    
020400     EJECT                                                                
020500* --- IMS SEKTIONER ---                                                   
020600     SKIP3                                                                
020700     EJECT                                                                
020800 IMS-RESTART SECTION.                                                     
020900     SKIP2                                                                
021000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021100     MOVE '  ' TO GODK-STATUSKODER                                        
021200     CALL CBLTDLI USING XRST MSG-PCB                                      
021300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021400                        CHKP-AREA-LENGTH CHKP-AREA                        
021500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021600     PERFORM IMS-STATUSKONTROLL                                           
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-CHECKPOINT SECTION.                                                  
022000     SKIP2                                                                
022100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022200     MOVE '  XD' TO GODK-STATUSKODER                                      
022300     CALL CBLTDLI USING CHKP MSG-PCB                                      
022400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022500                        CHKP-AREA-LENGTH CHKP-AREA                        
022600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022700     PERFORM IMS-STATUSKONTROLL                                           
022800                                                                          
022900     IF IMS-EJ-OK                                                         
023000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
023100       DISPLAY FELTEXT                                                    
023200       CALL FELLOG                                                        
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 IMS-GHU-K629 SECTION.                                                    
023700                                                                          
023800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023900          DELIMITED BY SIZE INTO SSA1                                     
024000     MOVE 'WDK611  '          TO SSA2                                     
024100     MOVE 'WDK629  '          TO SSA3                                     
024200     MOVE '  GE' TO GODK-STATUSKODER                                      
024300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2              
024400                                                   SSA3                   
024500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     .                                                                    
024800     SKIP3                                                                
024900 IMS-REPL-K629 SECTION.                                                   
025000                                                                          
025100     MOVE '  ' TO GODK-STATUSKODER                                        
025200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
025300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     EJECT                                                                
025700 IMS-STATUSKONTROLL SECTION.                                              
025800     SKIP2                                                                
025900     SET STATUS-IX TO 1                                                   
026000     SEARCH GODK-STATUS                                                   
026100       AT END                                                             
026200         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
026300         DISPLAY FELTEXT                                                  
026400         CALL FELLOG                                                      
026500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026600         CONTINUE                                                         
026700     END-SEARCH                                                           
026800     .                                                                    
