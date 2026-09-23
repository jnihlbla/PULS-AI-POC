000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2615600.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   01/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOMPLETTERAR INFIL MED ANSKAFFARID OCH ANSKAFFARGRUPP            
000900*        SKRIVER ENDAST UT POSTER SOM TILLHÖR DISTRIKT 81                 
001000*                                         OCH KUNDERNA 11 OCH 111         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ALLA RADER PÅ EFR                                          
002700     SELECT W47953                     ASSIGN TO W26156D1.                
002800     SKIP2                                                                
002900*          --- KOMPLETTERAD EFR-FIL MED ANSK PÅ DISTR 81                  
003000     SELECT W26156                     ASSIGN TO W26156D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W47953                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W479053      -L.                                               
004100     SKIP3                                                                
004200 FD  W26156                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W26156 -PRE  UT-  -L.                                     
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W2615600'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  W47953-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W47953                       VALUE 'J'.                   
005600     EJECT                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     03  W200ANSK                PIC X(8)    VALUE 'W200ANSK'.            
007000     SKIP2                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600     SKIP2                                                                
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500*01  -COPY W009W42   -PRE L-                                              
008600     EJECT                                                                
008700 01  IN-AREA-START       PIC X(24)   VALUE 'IN-AREA-START  '.             
008800*01  AREA -COPY W479053     -PRE IN-                                      
008900     EJECT                                                                
009000                                                                          
009100                                                                          
009200 01  UT-AREA-START       PIC X(24)   VALUE 'UT-AREA-START  '.             
009300*01  AREA -COPY W26156     -PRE UT-                                       
009400                                                                          
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700*                                                                         
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-IDARTNR-X.                                                     
010300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012200 01  DLI-IO-WDK601.                                                       
012300*    03  -COPY WDK601                                                     
012400     EJECT                                                                
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012600 01  DLI-IO-WDK611.                                                       
012700*    03  -COPY WDK611                                                     
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100*01  -COPY W0008  -PRE WDK6-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING WDK6-PCB.                                      
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
013700*------------------------                                                 
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     PERFORM S01-LAES-W47953                                              
014100     PERFORM UNTIL END-OF-W47953                                          
014200                                                                          
014300       IF IN-IDDISTR = +81                                                
014400       AND ( IN-IDKUNDNR = +11 OR +111 )                                  
014500         MOVE IN-AREA TO UT-AREA                                          
014600         MOVE ZERO    TO UT-GRP-INT                                       
014700                         UT-IDANSK                                        
014800*      --- KOMPLETTERA POSTER MED DIST=81 FÖR KUND 11 OCH 111             
014900                                                                          
015000         MOVE IN-IDARTNR TO W-IDARTNR                                     
015100         PERFORM IMS-GET-WDK601                                           
015200         IF SEGMENT-FINNS                                                 
015300           PERFORM IMS-GET-WDK611                                         
015400           IF SEGMENT-FINNS                                               
015500             MOVE CLAG-FLSKROT-AUTO TO UT-FLSKROT-AUTO                    
015600             MOVE CLAG-IDANSK       TO L-IDANSK                           
015700                                                                          
015800             CALL W200ANSK USING L-W009W42                                
015900                                                                          
016000             MOVE L-IDANSK          TO UT-IDANSK                          
016100             MOVE L-GRUPP-INTERVALL TO UT-GRP-INT                         
016200           END-IF                                                         
016300         END-IF                                                           
016310         IF IN-IDKONTO = 481180                                           
016320            CONTINUE                                                      
016330         ELSE                                                             
016400            PERFORM S11-SKRIV-W26156                                      
016410         END-IF                                                           
016500       END-IF                                                             
016600                                                                          
016700       PERFORM S01-LAES-W47953                                            
016800     END-PERFORM                                                          
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     OPEN INPUT  W47953                                                   
017900                                                                          
018000     OPEN OUTPUT W26156                                                   
018100                                                                          
018200     ACCEPT DAGENS-DATUM  FROM DATE                                       
018300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018400     .                                                                    
018500     EJECT                                                                
018600 Z-FINIT SECTION.                                                         
018700     CLOSE W47953                                                         
018800           W26156                                                         
018900     SKIP2                                                                
019000     MOVE 'S' TO POSTSUM-OPKOD                                            
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400 S01-LAES-W47953  SECTION.                                                
019500     READ W47953 INTO IN-AREA                                             
019600     AT END                                                               
019700        MOVE HIGH-VALUE TO IN-AREA                                        
019800        SET END-OF-W47953 TO TRUE                                         
019900                                                                          
020000     NOT AT END                                                           
020100        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
020200        MOVE 'W47953' TO POSTSUM-FDNAMN                                   
020300        MOVE 'W26156D1' TO POSTSUM-DDNAMN2                                
020400        CALL POSTSUM USING POSTSUM-PARM                                   
020500     END-READ                                                             
020600     .                                                                    
020700     EJECT                                                                
020800 S11-SKRIV-W26156 SECTION.                                                
020900                                                                          
021000     WRITE UT-POST FROM UT-AREA                                           
021100                                                                          
021200     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
021300     MOVE 'W26156' TO POSTSUM-FDNAMN                                      
021400     MOVE 'W26156D2' TO POSTSUM-DDNAMN2                                   
021500     CALL POSTSUM USING POSTSUM-PARM                                      
021600     .                                                                    
021700     EJECT                                                                
021800* --- IMS SEKTIONER ---                                                   
021900                                                                          
022000     EJECT                                                                
022100 IMS-GET-WDK601 SECTION.                                                  
022200                                                                          
022300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022400          DELIMITED BY SIZE INTO SSA1                                     
022500     MOVE '  GE' TO GODK-STATUSKODER                                      
022600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
022700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022800     PERFORM IMS-STATUSKONTROLL                                           
022900     .                                                                    
023000     EJECT                                                                
023100 IMS-GET-WDK611 SECTION.                                                  
023200                                                                          
023300     MOVE   'WDK611  '        TO SSA1                                     
023400     MOVE '  GE' TO GODK-STATUSKODER                                      
023500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
023600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023700     PERFORM IMS-STATUSKONTROLL                                           
023800     .                                                                    
023900     EJECT                                                                
024000 IMS-STATUSKONTROLL SECTION.                                              
024100                                                                          
024200     SET STATUS-IX TO 1                                                   
024300     SEARCH GODK-STATUS                                                   
024400       AT END                                                             
024500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024600           DELIMITED BY SIZE INTO FELTEXT                                 
024700         DISPLAY FELTEXT                                                  
024800         CALL FELLOG                                                      
024900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025000         CONTINUE                                                         
025100     END-SEARCH                                                           
025200     .                                                                    
