000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2363300.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS / JOHAN LINDKVIST.                  
000400 DATE-WRITTEN.   01/04/17  /  06/10/31.                                   
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KOPIA PÅ W23632300  (ENDAST 1 IF-SATS SOM SKILJER)               
000801*                                                                         
000802*        GÖR ENDAST URVAL PÅ LEVERANTÖR MWAJB                             
000803*                                                                         
000810*        INLEVERANSER GÅGNA VECKAN                                        
000900*        KOMPLETTERAS MED WDK6-DATA                                       
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- VECKANS INLEV                                              
002600     SELECT W23631                     ASSIGN TO W23633D1.                
002700     SKIP2                                                                
002800*          --- VECKANS INLEV KOMPL                                        
002900     SELECT W23633                     ASSIGN TO W23633D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W23631                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W23631      -L.                                                
004000     SKIP3                                                                
004100 FD  W23633                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W23633 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2363300'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W23631-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W23631                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                 'IN-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W23631     -PRE IN-                                       
008800     EJECT                                                                
008900 01  UT-AREA-START               PIC X(24)   VALUE                        
009000                                 'UT-AREA-START  '.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W23633     -PRE UT-                                       
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010300     03  W-KDSEGKEY-X.                                                    
010400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010500     SKIP2                                                                
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                       VALUE '  '.                  
010900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011100     SKIP2                                                                
011200 01  GODK-STATUSKODER.                                                    
011300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(64).                               
011600 01  SSA2                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012300 01  DLI-IO-WDK601.                                                       
012400*    03  -COPY WDK601                                                     
012500     EJECT                                                                
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012700 01  DLI-IO-WDK611.                                                       
012800*    03  -COPY WDK611                                                     
012900     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100                                                                          
013200                                                                          
013300*01  -COPY W0008  -PRE WDK6-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING WDK6-PCB.                                      
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
013900                                                                          
014000                                                                          
014100     PERFORM A-INIT                                                       
014200                                                                          
014300     PERFORM S01-LAES-W23631                                              
014400     PERFORM UNTIL END-OF-W23631                                          
014510        IF IN-IDLEVNR = 'MWAJB'                                           
014520          MOVE IN-IDARTNR TO W-IDARTNR                                    
014600          PERFORM IMS-GET-WDK611                                          
014700          IF SEGMENT-FINNS                                                
014900                MOVE IN-AREA          TO UT-AREA                          
014910                MOVE CLAG-KDEFFMAN    TO UT-KDEFFMAN                      
015000                MOVE CLAG-BEFT        TO UT-BEFT                          
015010                MOVE CLAG-IDANSK      TO UT-IDANSK                        
015100                MOVE CLAG-KVDAGAR-TT  TO UT-KVDAGAR-TT                    
015200                MOVE CLAG-KVDAGAR-INLEV TO UT-KVDAGAR-INLEV               
015300                PERFORM S11-SKRIV-W23633                                  
015310          ELSE                                                            
015320            DISPLAY IN-IDARTNR                                            
015400          END-IF                                                          
015500        END-IF                                                            
015600                                                                          
015700        PERFORM S01-LAES-W23631                                           
015800     END-PERFORM                                                          
015900                                                                          
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     OPEN INPUT  W23631                                                   
017000                                                                          
017100     OPEN OUTPUT W23633                                                   
017200                                                                          
017300     ACCEPT DAGENS-DATUM  FROM DATE                                       
017400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017500     .                                                                    
017600     EJECT                                                                
017700 Z-FINIT SECTION.                                                         
017800     CLOSE W23631                                                         
017900           W23633                                                         
018000     SKIP2                                                                
018100     MOVE 'S' TO POSTSUM-OPKOD                                            
018200     CALL POSTSUM USING POSTSUM-PARM                                      
018300     .                                                                    
018400     EJECT                                                                
018500 S01-LAES-W23631  SECTION.                                                
018600     READ W23631 INTO IN-AREA                                             
018700     AT END                                                               
018800        SET END-OF-W23631 TO TRUE                                         
018900                                                                          
019000     NOT AT END                                                           
019100        MOVE 'W23631'   TO POSTSUM-FDNAMN                                 
019200        MOVE 'W23633D1' TO POSTSUM-DDNAMN2                                
019300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
019400        CALL POSTSUM USING POSTSUM-PARM                                   
019500     END-READ                                                             
019600     .                                                                    
019700     EJECT                                                                
019800 S11-SKRIV-W23633 SECTION.                                                
019900                                                                          
020000     WRITE UT-POST FROM UT-AREA                                           
020100                                                                          
020200     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
020300     MOVE 'W23633'   TO POSTSUM-FDNAMN                                    
020400     MOVE 'W23633D2' TO POSTSUM-DDNAMN2                                   
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 S99-ABEND SECTION.                                                       
020900                                                                          
021000     SKIP2                                                                
021100     MOVE 'S' TO POSTSUM-OPKOD                                            
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     CALL ABEND USING RKOD-ABEND                                          
021400     .                                                                    
021500     EJECT                                                                
021600* --- IMS SEKTIONER ---                                                   
021700                                                                          
021800 IMS-GET-WDK601 SECTION.                                                  
021900                                                                          
022000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
022100          DELIMITED BY SIZE INTO SSA1                                     
022200     MOVE '  GE' TO GODK-STATUSKODER                                      
022300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
022400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022500     PERFORM IMS-STATUSKONTROLL                                           
022600     .                                                                    
022700     EJECT                                                                
022800 IMS-GET-WDK611 SECTION.                                                  
022900                                                                          
023000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023100          DELIMITED BY SIZE INTO SSA1                                     
023200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
023300          DELIMITED BY SIZE INTO SSA2                                     
023400     MOVE '  GE' TO GODK-STATUSKODER                                      
023500     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
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
