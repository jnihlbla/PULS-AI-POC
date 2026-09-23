000100                                                                          
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4321000.                                                
000400 AUTHOR.         STEFAN ANDREASSON.                                       
000500 DATE-WRITTEN.   99/01/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        UPPDATERAR KUNDREGISTRET MED NAMN OCH ADRESS PÅ                  
001100*        JAPANSKA FÖR JAPANSKA KUNDER                                     
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDB2                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- JAPANSKA ADRESSER                                          
002400     SELECT W43210                     ASSIGN TO W43210D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W43210                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W460RHG      -L.                                               
003500                                                                          
003600*01  -COPY W460RHH      -L.                                               
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W4321000'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 77  W43210-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W43210                       VALUE 'J'.                   
005200     EJECT                                                                
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  IN-AREA-START               PIC X(24)   VALUE                        
007000                                             'IN-AREA-START'.             
007100     SKIP2                                                                
007200 01  IN-AREA.                                                             
007300                                                                          
007400     05  IN-IDPTYP               PIC X(3).                                
007500     05  IN-IDDISTR              PIC 9(4).                                
007600     05  FILLER                  PIC X(73).                               
007700*01  FILLER -COPY W460RHG  -PRE IN-  -RED  IN-AREA                        
007800*01  FILLER -COPY W460RHH  -PRE IN-  -RED  IN-AREA                        
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008400     03  W-IDGMT-X.                                                       
008500         05  W-IDGMT.                                                     
008600            07  W-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
008700            07  W-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
008800     SKIP2                                                                
008900*                                                                         
009000 01  FILLER                 PIC X(16) VALUE 'IDDISTRIKT '.                
009100 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
009200*01  FILLER  -COPY WWDIST34 -RED TEST-IDDISTR.                            
009300     EJECT                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200                                                                          
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
011400 01  DLI-IO-WDB201.                                                       
011500*    03  -COPY WDB201                                                     
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0009   -PRE MSG-                                              
012000                                                                          
012100*01  -COPY W0008  -PRE WDB2-                                              
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB WDB2-PCB.                              
012500 MAIN SECTION.                                                            
012600     ENTRY 'DLITCBL' USING MSG-PCB WDB2-PCB.                              
012700                                                                          
012800     SKIP2                                                                
012900     PERFORM A-INIT                                                       
013000     PERFORM S01-LAES-W43210                                              
013100     PERFORM UNTIL END-OF-W43210                                          
013200       MOVE IN-IDDISTR       TO TEST-IDDISTR                              
013300       IF DIST34-JAPAN-NDC                                                
013400       OR DIST34-KINA-NDC OR DIST34-KINA-LDC                              
013500          IF IN-IDPTYP = 'RHG'                                            
013600             MOVE IN-RHG-IDDISTR                                          
013700                             TO W-IDDISTR                                 
013800             MOVE IN-RHG-IDKUNDNR                                         
013900                             TO W-IDKUNDNR                                
014000             PERFORM IMS-GHU-WDB201                                       
014100             MOVE IN-RHG-LEV-NAMN                                         
014200                             TO GMT-BEGMT-OVR-RAD1                        
014300             MOVE IN-RHG-LEV-ADR1                                         
014400                             TO GMT-ADGMT-OVR-GATA                        
014500          END-IF                                                          
014600          IF IN-IDPTYP = 'RHH'                                            
014700             MOVE IN-RHH-LEV-ADR2                                         
014800                             TO GMT-ADGMT-OVR-LAND                        
014900             MOVE IN-RHH-LEV-PNR                                          
015000                             TO GMT-ADGMT-OVR-PADR (1:8)                  
015100             MOVE IN-RHH-LEV-PADR                                         
015200                             TO GMT-ADGMT-OVR-PADR (10:20)                
015300             IF SEGMENT-FINNS                                             
015400               PERFORM IMS-REPL-WDB201                                    
015500            END-IF                                                        
015600          END-IF                                                          
015700       END-IF                                                             
015800                                                                          
015900       PERFORM S01-LAES-W43210                                            
016000     END-PERFORM                                                          
016100                                                                          
016200                                                                          
016300     PERFORM Z-FINIT                                                      
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000     SKIP2                                                                
017100                                                                          
017200     OPEN INPUT W43210                                                    
017300                                                                          
017400                                                                          
017500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017600     .                                                                    
017700     EJECT                                                                
017800 Z-FINIT SECTION.                                                         
017900                                                                          
018000                                                                          
018100     CLOSE W43210                                                         
018200     SKIP2                                                                
018300     MOVE 'S' TO POSTSUM-OPKOD                                            
018400     CALL POSTSUM USING POSTSUM-PARM                                      
018500     .                                                                    
018600     EJECT                                                                
018700 S01-LAES-W43210  SECTION.                                                
018800     SKIP2                                                                
018900     READ W43210 INTO IN-AREA                                             
019000     AT END                                                               
019100        SET END-OF-W43210 TO TRUE                                         
019200                                                                          
019300     NOT AT END                                                           
019400        MOVE 'W43210' TO POSTSUM-FDNAMN                                   
019500        MOVE 'W43210D1' TO POSTSUM-DDNAMN2                                
019600        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
019700        CALL POSTSUM USING POSTSUM-PARM                                   
019800     END-READ                                                             
019900     .                                                                    
020000     EJECT                                                                
020100* --- IMS SEKTIONER ---                                                   
020200                                                                          
020300     EJECT                                                                
020400 IMS-GHU-WDB201   SECTION.                                                
020500                                                                          
020600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
020700          DELIMITED BY SIZE INTO SSA1                                     
020800     MOVE '  GE' TO GODK-STATUSKODER                                      
020900     CALL CBLTDLI USING GHU WDB2-PCB DLI-IO-WDB201 SSA1                   
021000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300     SKIP3                                                                
021400 IMS-REPL-WDB201   SECTION.                                               
021500                                                                          
021600     MOVE '  ' TO GODK-STATUSKODER                                        
021700     CALL CBLTDLI USING REPL WDB2-PCB DLI-IO-WDB201                       
021800     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
021900     PERFORM IMS-STATUSKONTROLL                                           
022000     .                                                                    
022100     EJECT                                                                
022200 IMS-STATUSKONTROLL SECTION.                                              
022300     SKIP2                                                                
022400     SET STATUS-IX TO 1                                                   
022500     SEARCH GODK-STATUS                                                   
022600       AT END                                                             
022700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022800           DELIMITED BY SIZE INTO FELTEXT                                 
022900         DISPLAY FELTEXT                                                  
023000         CALL FELLOG                                                      
023100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023200         CONTINUE                                                         
023300     END-SEARCH                                                           
023400     .                                                                    
