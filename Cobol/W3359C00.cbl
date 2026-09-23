000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3359C00.                                                
000300 AUTHOR.         STINA MOGREN                                             
000400 DATE-WRITTEN.   05/10/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BATCH FÖR ATT PLOCKA FRAM PRISFRÅGOR FÖR SVERIGE SOM INTE        
001100*        KRÄVER SVAR DIREKT I PGM W3039100. ADDISPABS = SPACE.            
001200*        FÅGORNA BUNTAS IHOP FÖR ATT VIPS INTE KLARAR AV ATT TA           
001300*        EMOT SÅ STORA MÄNGDER FRÅGOR SOM DET BLIR FÖR SVERIGE.           
001400*        PROGRAM W3359B SÄNDER PRISFRÅGOR OCH UPPDATERAR WDC7             
001500*        MED SÄND-DATUM                                                   
001600*                                                                         
001700*    E'TRACKER ID: 3901057 2006-10 ÄNDRA TILL SB.                         
001800*                                                                         
001900*    UTDATA.                                                              
002000*        FIL MED UTVALDA PRISFRÅGOR                                       
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600*       --- UTFIL FRÅN W3359C                                             
002700     SELECT W3359C                     ASSIGN TO W3359CD1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 FILE SECTION.                                                            
003200 FD  W3359C                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W3359C01 -PRE  UT-  -L.                                   
003700      SKIP2                                                               
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W3359C00'.            
004000*                                                                         
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  W-FIRST-TIME                PIC 9       VALUE ZERO.                  
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100     EJECT                                                                
005200                                                                          
005300*    --- PARAMETRAR TILL POSTSUM                                          
005400*                                                                         
005500*01  -COPY W0005   -PRE  POSTSUM-                                         
005600     EJECT                                                                
005700                                                                          
005800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005900 01  GENERELLA-SUBPROGRAM.                                                
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006300     EJECT                                                                
006400 01  MESSAGE-CODES.                                                       
006500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006600     EJECT                                                                
006700 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
006800     SKIP2                                                                
006900*01  FILLER -COPY W3359C01     -PRE UT-                                   
007000     EJECT                                                                
007100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007200*                                                                         
007300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007400     SKIP3                                                                
007500 01  NYCKLAR-TILL-DLI.                                                    
007600   03  W-IDBUNDLE-X.                                                      
007700     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
007800     05  W-PRQ-IDORDNR7-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
007900       07  W-PRQ-IDORDNR7        PIC 9(7).                                
008000       07  FILLER                PIC X(8).                                
008100     05  W-PRQ-IDRAPPNR-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
008200       07  W-PRQ-IDRAPPNR        PIC 9(7).                                
008300       07  FILLER                PIC X(8).                                
008400                                                                          
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(128).                              
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400                                                                          
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7  '.                      
010600 01  DLI-IO-WDC7.                                                         
010700*    03  -COPY WDC711 -PRE RR-                                            
010800                                                                          
010900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
011000 01  DLI-IO-WDC701.                                                       
011100*    03  -COPY WDC701                                                     
011200     EJECT                                                                
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
011400 01  DLI-IO-WDC711.                                                       
011500*    03  -COPY WDC711                                                     
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800*01  -COPY W0008  -PRE WDC7-                                              
011900     05  FILLER                  PIC X.                                   
012000     EJECT                                                                
012100 PROCEDURE DIVISION  USING  WDC7-PCB.                                     
012200 MAIN SECTION.                                                            
012300     ENTRY 'DLITCBL' USING  WDC7-PCB.                                     
012400                                                                          
012500     PERFORM A-INIT                                                       
012600     PERFORM C-CHECKBASE                                                  
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200                                                                          
013300 A-INIT SECTION.                                                          
013400                                                                          
013500     OPEN OUTPUT W3359C                                                   
013600                                                                          
013700     .                                                                    
013800     EJECT                                                                
013900                                                                          
014000 C-CHECKBASE SECTION.                                                     
014100                                                                          
014200     PERFORM IMS-GN-WDC7                                                  
014300     PERFORM UNTIL SEGMENT-SLUT                                           
014400                                                                          
014500       IF WDC7-SEG-LEVEL = '01'                                           
014600         MOVE DLI-IO-WDC7 TO DLI-IO-WDC701                                
014700         IF PRQ-IDDISTR = 0778                                            
014800         OR PRQ-IDDISTR = 8857                                            
014810         OR PRQ-IDDISTR = 8859                                            
014900            MOVE PRQ-IDDISTR   TO   UT-IDDISTR                            
015000            MOVE PRQ-IDKUNDNR  TO   UT-IDKUNDNR                           
015100            MOVE PRQ-IDBUNDLE  TO   UT-IDBUNDLE                           
015200         END-IF                                                           
015300       END-IF                                                             
015400                                                                          
015500       IF WDC7-SEG-LEVEL = '02'                                           
015600         IF PRQ-IDDISTR = 0778                                            
015700         OR PRQ-IDDISTR = 8857                                            
015800         OR PRQ-IDDISTR = 8859                                            
015900           MOVE DLI-IO-WDC7 TO DLI-IO-WDC711                              
016000           IF LPRQ-KDORDKL = 4 AND                                        
016100              LPRQ-DADATTID-SEND = 0 AND                                  
016200              LPRQ-DADATTID-OK NOT = 0 AND                                
016300              LPRQ-FLALL = 'N' AND                                        
016400              LPRQ-FILLER(1:1) = SPACE                                    
016500              MOVE ZERO     TO   LPRQ-DADATTID-OK                         
016600           END-IF                                                         
016700           IF (LPRQ-ADDISPABS = SPACE) AND                                
016800              (LPRQ-DADATTID-SEND = 0) AND                                
016900              (LPRQ-DADATTID-OK = 0)   AND                                
017000              (LPRQ-KDORDTYP = 'M')   AND                                 
017100              (LPRQ-KDPRSTA NOT = 'M')                                    
017200                                                                          
017300              MOVE LPRQ-IDPRQUES TO   UT-IDPRQUES                         
017400              MOVE LPRQ-IDARTNR  TO   UT-IDARTNR                          
017500              MOVE LPRQ-KDORDKL  TO   UT-KDORDKL                          
017600              MOVE LPRQ-KVBEART  TO   UT-KVBEART                          
017700                                                                          
017800              PERFORM S05-SKRIV-POST                                      
017900                                                                          
018000           END-IF                                                         
018100         END-IF                                                           
018200       END-IF                                                             
018300                                                                          
018400       PERFORM IMS-GN-WDC7                                                
018500                                                                          
018600     END-PERFORM                                                          
018700                                                                          
018800     MOVE 'S' TO POSTSUM-OPKOD                                            
018900     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
019000                                                                          
019100     CLOSE W3359C                                                         
019200     .                                                                    
019300     EJECT                                                                
019400 S05-SKRIV-POST SECTION.                                                  
019500                                                                          
019600     WRITE UT-POST    FROM UT-AREA                                        
019700                                                                          
019800     MOVE 'W3359C'    TO POSTSUM-FDNAMN                                   
019900     MOVE 'W3359CD1'  TO POSTSUM-DDNAMN2                                  
020000     CALL POSTSUM  USING POSTSUM-PARM                                     
020100     .                                                                    
020200     EJECT                                                                
020300* --- IMS SEKTIONER ---                                                   
020400     SKIP3                                                                
020500 IMS-GN-WDC7    SECTION.                                                  
020600                                                                          
020700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
020800     CALL CBLTDLI USING GN  WDC7-PCB DLI-IO-WDC7                          
020900     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
021000     PERFORM IMS-STATUSKONTROLL                                           
021100     .                                                                    
021200     SKIP3                                                                
021300 IMS-STATUSKONTROLL SECTION.                                              
021400                                                                          
021500     SET STATUS-IX TO 1                                                   
021600     SEARCH GODK-STATUS                                                   
021700       AT END                                                             
021800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021900         DELIMITED BY SIZE INTO FELTEXT                                   
022000         CALL FELLOG                                                      
022100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022200         CONTINUE                                                         
022300     END-SEARCH                                                           
022400     .                                                                    
