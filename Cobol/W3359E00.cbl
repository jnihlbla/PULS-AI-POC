000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3359E00.                                                
000300 AUTHOR.         ELEONOR ÖSTRÖM                                           
000400 DATE-WRITTEN.   07/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BMP FÖR ATT SÄNDA OM ALLA PRISFRÅGOR SOM INTE                    
000900*        HAR FÅTT NÅGOT SVAR FRÅN VIPS INOM 1 TIMME, OCH DE SOM           
001000*        INTE FICK SVAR MED PRIS.DENNA SB KÖRS IGÅNG AV EN                
001100*        TIDSHÅLLNINGSMODUL AV LASSI (W335S6)                             
001200*        PROGRAMMET SKAPAR FIL SOM SENARE BUNTAS OCH SKICKAS              
001300*        I PROGRAM  W3359D.                                               
001400*                                                                         
001501*        PROGRAMMET LÄSER WDC7 MED SB.                                    
001600*                                                                         
001700*    UTDATA.                                                              
001800*        FIL MED UTVALDA PRISFRÅGOR(OMFRÅGOR)                             
001900*                                                                         
002000*    CHANGE LOG: E'TRACKER 5978507 DATED 071127                           
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700 FILE-CONTROL.                                                            
002800*       --- UTFIL FRÅN W3359E                                             
002900     SELECT W3359E                     ASSIGN TO W3359ED1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 FILE SECTION.                                                            
003400 FD  W3359E                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W3359C01 -PRE  UT-  -L.                                   
003900      SKIP2                                                               
004000 WORKING-STORAGE SECTION.                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W3359E00'.            
004200*                                                                         
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300     EJECT                                                                
005400 01  W-DADATTID                 PIC 9(14)    VALUE ZERO.                  
005500 01  W-TIDDIFF                  PIC 9(14)    VALUE ZERO.                  
005600                                                                          
005700*    --- PARAMETRAR TILL POSTSUM                                          
005800*                                                                         
005900*01  -COPY W0005   -PRE  POSTSUM-                                         
006000     EJECT                                                                
006100                                                                          
006200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006300 01  GENERELLA-SUBPROGRAM.                                                
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006800     EJECT                                                                
006900 01  MESSAGE-CODES.                                                       
007000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007100     EJECT                                                                
007200 01  COUNTERS.                                                            
007300     03  TALLY-OK                PIC 9(4)    VALUE ZERO.                  
007400     03  TALLY-FEL               PIC 9(4)    VALUE ZERO.                  
007500     03  W-FIRST-TIME            PIC 9       VALUE ZERO.                  
007600*    --- UT-AREA                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
007800     SKIP2                                                                
007900*01  FILLER -COPY W3359C01     -PRE UT-                                   
008000     EJECT                                                                
008100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008600     03  W-WDC701KY-X.                                                    
008700         05  W-IDDISTR           PIC 9(4)     VALUE ZERO.                 
008800         05  W-IDKUNDNR          PIC 9(7)     VALUE ZERO.                 
008900         05  W-IDBUNDLE          PIC X(15)    VALUE SPACE.                
009000     03  W-WDC711KY-X.                                                    
009100         05  W-IDPRQUES          PIC 9(7)     VALUE ZERO.                 
009200     SKIP2                                                                
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     88  DB-SLUT                             VALUE 'GB'.                  
009900     SKIP2                                                                
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNKTIONSKODER                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000                                                                          
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC7  '.                      
011200 01  DLI-IO-WDC7.                                                         
011300*    03  -COPY WDC711 -PRE RR-                                            
011400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
011500 01  DLI-IO-WDC701.                                                       
011600*    03  -COPY WDC701                                                     
011700     EJECT                                                                
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
011900 01  DLI-IO-WDC711.                                                       
012000*    03  -COPY WDC711                                                     
012100     EJECT                                                                
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-PRFEL1'.                      
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500*01  -COPY W0008  -PRE WDC7-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING WDC7-PCB.                                      
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING WDC7-PCB.                                      
014000                                                                          
015000     PERFORM A-INIT                                                       
015100     PERFORM C-CHECKBASE                                                  
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700                                                                          
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     OPEN OUTPUT W3359E                                                   
016100                                                                          
016200     MOVE FUNCTION CURRENT-DATE(1:14) TO W-DADATTID                       
016300     CONTINUE                                                             
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 C-CHECKBASE SECTION.                                                     
016800                                                                          
016900     PERFORM IMS-GN-WDC7                                                  
017000     PERFORM UNTIL DB-SLUT                                                
017100****  CHECKS IF WDC701 SEGMENT***************                             
017200      IF WDC7-SEG-LEVEL = '01'                                            
017300        MOVE DLI-IO-WDC7 TO DLI-IO-WDC701                                 
017400        IF PRQ-IDDISTR = 0778                                             
017500*       OR PRQ-IDDISTR = 8857                                             
017510*       OR PRQ-IDDISTR = 8859                                             
017600          MOVE PRQ-IDDISTR   TO   UT-IDDISTR                              
017700          MOVE PRQ-IDKUNDNR  TO   UT-IDKUNDNR                             
017800          MOVE PRQ-IDBUNDLE  TO   UT-IDBUNDLE                             
017900        END-IF                                                            
018000      END-IF                                                              
018100****  CHECKS IF WDC711 SEGMENT***************                             
018200      IF WDC7-SEG-LEVEL = '02'                                            
018300        IF PRQ-IDDISTR = 0778                                             
018400*       OR PRQ-IDDISTR = 8857                                             
018410*       OR PRQ-IDDISTR = 8859                                             
018500          MOVE DLI-IO-WDC7 TO DLI-IO-WDC711                               
018600          IF  LPRQ-DADATTID-OK = 0 AND                                    
018700              LPRQ-DADATTID-SEND > 0                                      
018800                                                                          
018900            COMPUTE  W-TIDDIFF = W-DADATTID - LPRQ-DADATTID-SEND          
019000            IF W-TIDDIFF > 9900                                           
019100****CHECK THAT PRICE NOT MANUALLY UPDATED,                                
019200              IF LPRQ-KDPRSTA NOT = 'M'                                   
019300                MOVE LPRQ-IDPRQUES TO   UT-IDPRQUES                       
019400                MOVE LPRQ-IDARTNR  TO   UT-IDARTNR                        
019500                MOVE LPRQ-KDORDKL  TO   UT-KDORDKL                        
019600                MOVE LPRQ-KVBEART  TO   UT-KVBEART                        
019700                PERFORM S05-SKRIV-POST                                    
019800              END-IF                                                      
019900            END-IF                                                        
020000          END-IF                                                          
020100        END-IF                                                            
020200      END-IF                                                              
020300***********************************                                       
020400      PERFORM IMS-GN-WDC7                                                 
020500     END-PERFORM                                                          
020600                                                                          
020700     MOVE 'S' TO POSTSUM-OPKOD                                            
020800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
020900                                                                          
021000     CLOSE W3359E                                                         
021100     .                                                                    
021200     EJECT                                                                
021300 S05-SKRIV-POST SECTION.                                                  
021400                                                                          
021500     WRITE UT-POST    FROM UT-AREA                                        
021600                                                                          
021700     MOVE 'W3359E'    TO POSTSUM-FDNAMN                                   
021800     MOVE 'W3359ED1'  TO POSTSUM-DDNAMN2                                  
021900     CALL POSTSUM  USING POSTSUM-PARM                                     
022000     .                                                                    
022100     EJECT                                                                
022200* --- IMS SEKTIONER ---                                                   
022300     SKIP3                                                                
022400 IMS-GN-WDC7    SECTION.                                                  
022500                                                                          
022601     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022700     CALL CBLTDLI USING GN  WDC7-PCB DLI-IO-WDC7                          
022800     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
022900     PERFORM IMS-STATUSKONTROLL                                           
023000     .                                                                    
023100     SKIP3                                                                
023200 IMS-STATUSKONTROLL SECTION.                                              
024000                                                                          
024100     SET STATUS-IX TO 1                                                   
024200     SEARCH GODK-STATUS                                                   
024300       AT END                                                             
024400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024500         DELIMITED BY SIZE INTO FELTEXT                                   
024600         CALL FELLOG                                                      
024700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024800         CONTINUE                                                         
024900     END-SEARCH                                                           
025000     .                                                                    
