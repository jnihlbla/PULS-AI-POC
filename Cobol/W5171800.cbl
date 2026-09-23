000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W5171800.                                               
000400 AUTHOR.          KARL JOHAN HANSSON.                                     
000500 DATE-WRITTEN.    SEPTEMBER ÅR 2000.                                      
000600                                                                          
000700*    REMARKS.                                                             
000800*       PROGRAMET ÄR EN SB.                                               
000900*       PROGRAMET LÄSER WDL2 (INLEV.HISTORIK FÖR CDC) OCH                 
001000*       OCH TAR FRAM ALLA ÅF-RETURER (TYP KDRT = 7,77).                   
001100*       MED DISTRIKT/LEVNR = 8111 ELLER 8211, ÅF-RETUR USA/CANADA         
001200*       DESSA TRANSAR SKALL ÖKA AK-VÄRDET PÅ RSREDO FÖR DC 11             
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000     SELECT W51718                     ASSIGN TO W51718D1.                
002100                                                                          
002200 DATA DIVISION.                                                           
002300                                                                          
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W51718                                                               
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  POST -COPY W51712  -PRE UT-  -L.                                     
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77   IDPGM                      PIC X(8)    VALUE 'W5171800'.            
003600                                                                          
003700 01  JA                          PIC X       VALUE 'J'.                   
003800 01  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 01  W-DAVVREG                   PIC 9(6).                                
004100                                                                          
004200 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
004300*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
004400                                                                          
004500*    --- VALID IDDC CODES                                                 
004600*01  -COPY WWDCKONS                                                       
004700                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005000   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
005100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005300     EJECT                                                                
005400 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
005500 01  IMS-WS.                                                              
005600   03  STATUS-WS                 PIC X(2).                                
005700      88  SEGMENT-FINNS                      VALUE '  '.                  
005800      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
005810      88  SEGMENT-SLUT                       VALUE 'GB'.                  
005900                                                                          
006000   03 GODK-STATUSKODER.                                                   
006100      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
006200                                                                          
006300     03  SSA1                       PIC X(64)  VALUE SPACE.               
006400     EJECT                                                                
006500*01        -COPY W0003                                                    
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
006800                                                                          
006900*    -COPY W0005       -PRE POSTSUM-                                      
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)  VALUE 'UT-AREA-START'.        
007200                                                                          
007300*01  AREA  -COPY W51712   -PRE UT-                                        
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)  VALUE 'IO-AREA'.              
007600 01  IO-AREA.                                                             
007700   03  IO-AREA1                  PIC X(250).                              
007800                                                                          
007900*  03  ART-AREA   -COPY WDL201 -PRE L2- -RED IO-AREA1                     
008000                                                                          
008100*  03  MOT-AREA   -COPY WDL221          -RED IO-AREA1                     
008200     EJECT                                                                
008300 LINKAGE SECTION.                                                         
008400                                                                          
008500*01  -COPY W0008       -PRE WDL2-                                         
008600       05 FILLER                 PIC X(1).                                
008700     EJECT                                                                
008800 PROCEDURE DIVISION USING WDL2-PCB.                                       
008900     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
009000                                                                          
009100     PERFORM A-INIT                                                       
009200     PERFORM IMS-GET-WDL2                                                 
009300                                                                          
009400     PERFORM UNTIL SEGMENT-SLUT                                           
009500                                                                          
009600       EVALUATE WDL2-SEG-NAME-FB                                          
009700         WHEN  'WDL201'                                                   
009800           MOVE L2-ART-IDARTNR    TO UT-IDARTNR                           
009900         WHEN  'WDL221'                                                   
010000           PERFORM B-TESTA-SKAPA-UTPOST                                   
010100       END-EVALUATE                                                       
010200                                                                          
010300       PERFORM IMS-GET-WDL2                                               
010400     END-PERFORM                                                          
010500                                                                          
010600     PERFORM Z-FINIT                                                      
010700     MOVE ZERO TO RETURN-CODE                                             
010800     GOBACK                                                               
010900     .                                                                    
011000     SKIP3                                                                
011100 A-INIT SECTION.                                                          
011200                                                                          
011300     OPEN OUTPUT W51718                                                   
011400                                                                          
011500     MOVE WC-CDC-SE              TO UT-IDDC                               
011600     MOVE ZERO                   TO UT-KVLS                               
011700                                    UT-KVEFRS                             
011800                                    UT-KVRESS                             
011900                                    UT-KVOKS                              
012000                                                                          
012100     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
012200     .                                                                    
012300     EJECT                                                                
012400 B-TESTA-SKAPA-UTPOST SECTION.                                            
012500                                                                          
012600     MOVE ZERO           TO TALLY                                         
012700     INSPECT MOT-IDLEVNR TALLYING TALLY                                   
012800                         FOR CHARACTERS BEFORE INITIAL SPACE              
012900     IF TALLY = ZERO                                                      
013000       MOVE ZERO TO TEST-IDDISTR                                          
013100     ELSE                                                                 
013200       MOVE MOT-IDLEVNR(1:TALLY) TO TEST-IDDISTR                          
013300     END-IF                                                               
013400* OM DISTRIKT = LEVNR = 8111 OCH 8211, SKAPA TRANS                        
013500     IF DIST35-NA-CDC-RETURN                                              
013510     OR DIST35-CDC-RETURNS-NON-VCC                                        
013600*    OR DIST35-CN-CDC-RETURNS                                             
013610*    OR DIST35-IN-CDC-RETURNS                                             
013700       IF (MOT-IDPTYP = 'R31' OR '310') AND (MOT-KDRT = +7 OR +77)        
013800         IF MOT-KVAVIS NOT = +0                                           
013900           COMPUTE UT-KVAKS = MOT-KVAVIS                                  
014000           PERFORM S01-SKRIV-W51718                                       
014100         END-IF                                                           
014200       END-IF                                                             
014300     END-IF                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 Z-FINIT SECTION.                                                         
014700                                                                          
014800     CLOSE W51718                                                         
014900                                                                          
015000     MOVE 'S' TO POSTSUM-OPKOD                                            
015100     CALL POSTSUM USING POSTSUM-PARM                                      
015200     .                                                                    
015300     SKIP2                                                                
015400 S01-SKRIV-W51718       SECTION.                                          
015500                                                                          
015600     WRITE UT-POST FROM UT-AREA                                           
015700                                                                          
015800     MOVE 'W51718'     TO POSTSUM-FDNAMN                                  
015900     MOVE 'W51718D1'   TO POSTSUM-DDNAMN2                                 
016000     MOVE 'UT  '       TO POSTSUM-TRANSTYP                                
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 IMS-GET-WDL2 SECTION.                                                    
016500                                                                          
016600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016700     CALL CBLTDLI USING GN WDL2-PCB IO-AREA                               
016800     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
016900     PERFORM IMS-STATUSKONTROLL                                           
017000     .                                                                    
017100     SKIP2                                                                
017200 IMS-STATUSKONTROLL SECTION.                                              
017300                                                                          
017400     SET STATUS-IX TO 1                                                   
017500     SEARCH GODK-STATUS AT END CALL FELLOG                                
017600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
017700     END-SEARCH                                                           
017800     .                                                                    
