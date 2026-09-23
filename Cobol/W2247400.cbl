000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247400.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000810*        INPUT: PARAMETER FILE FROM SCREEN 2115.                          
000820*               IDLEVNR + KVVECKOR-LT                                     
000821*                                                                         
000830*        NDC:KINA LAGERBAND SORTERAT ARTIKELNR, DC (71, 72, 73)           
000831*        NDC:USA  LAGERBAND SORTERAT ARTIKELNR, DC (41,43 - 46)           
000832*        NDC'R MED LOKAL ANSKAFFNING                                      
000833*                                                                         
000840*        OUTPUT: FILE WITH ALL PARTS CONNECTED TO SUPPLIER                
000850*                FROM THE INPUT FILE                                      
000860*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- PARAMETER FROM SCREEN 2115                                 
002500     SELECT W22474-PARM                ASSIGN TO W22474D1.                
002510     SKIP2                                                                
002511*          --- NDC:ERS  LAGERBAND SORTERAT ARTIKELNR, DC (7X,4X)          
002512     SELECT W01184                     ASSIGN TO W22474D2.                
002520     SKIP2                                                                
002600*          --- OUTPUT FILE TO UPDATE WDK7                                 
002700     SELECT W22474                     ASSIGN TO W22474D3.                
002710     EJECT                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  W22474-PARM                                                          
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  IN-PARM            PIC X(80).                                        
003700     SKIP3                                                                
003701 FD  W01184                                                               
003702     RECORDING       F                                                    
003703     BLOCK CONTAINS  0.                                                   
003704                                                                          
003705*01  -COPY W01184      -L.                                                
003710     SKIP3                                                                
003800 FD  W22474                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  W2247401 -COPY W2247401 -PRE  OUT-  -L.                              
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2247400'.            
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005100                                                                          
005200 77  PARM-EOF-SW                 PIC X       VALUE 'N'.                   
005210     88  END-OF-PARM                         VALUE 'J'.                   
005211                                                                          
005212 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
005213     88  END-OF-W01184                       VALUE 'J'.                   
005214                                                                          
005215     EJECT                                                                
005800 01  GENERAL-SUBPROGRAMS.                                                 
005900*                                                                         
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     SKIP2                                                                
006500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006600                                                                          
006700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  ERROR-TEXT.                                                          
007200     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007802 01  FILLER                   PIC X(16) VALUE 'WWDC99          '.         
007803*    --- VALID IDDC CODES                                                 
007804*01   -COPY WWDC99.                                                       
007810     EJECT                                                                
007900 01  IN-PARM-START               PIC X(24)   VALUE                        
008000                                 'IN-PARM-START  '.                       
008610 01  PARM-AREA.                                                           
008672     03  PARM-IDLEVNR            PIC X(5).                                
008673     03  FILLER                  PIC X(1).                                
008674     03  PARM-IDDC               PIC X(2).                                
008675     03  FILLER                  PIC X(1).                                
008676     03  PARM-KVVECKOR-AT        PIC X(2).                                
008677     03  FILLER                  PIC X(1).                                
008678     03  PARM-KVVECKOR-LT        PIC X(2).                                
008679     03  FILLER                  PIC X(1).                                
008680     03  PARM-KVDAGAR-TT         PIC X(2).                                
008681     03  FILLER                  PIC X(66)   VALUE SPACE.                 
008690     EJECT                                                                
008691 01  IN-LB-AREA-START            PIC X(24)   VALUE                        
008692                                 'IN-LB-AREA-START  '.                    
008695                                                                          
008696*01  SLAG-AREA -COPY W01184                                               
008697     EJECT                                                                
008700                                                                          
008800 01  OUT-AREA-START              PIC X(24)   VALUE                        
008900                                 'OUT-AREA-START  '.                      
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W2247401     -PRE OUT-                                    
009300     EJECT                                                                
013600 PROCEDURE DIVISION.                                                      
013700 MAIN SECTION.                                                            
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-READ-PARM-FROM-SCREEN-2115                               
014300                                                                          
014400     PERFORM S02-READ-W01184                                              
014500     PERFORM UNTIL END-OF-W01184                                          
014510                                                                          
014520       MOVE SLAG-IDDC TO WS-IDDC                                          
014530                                                                          
014540       IF NDC-CN OR NDC-US                                                
014921         IF SLAG-IDLEVNR = PARM-IDLEVNR                                   
014930            MOVE SLAG-IDARTNR         TO OUT-IDARTNR                      
014940            MOVE SLAG-IDDC            TO OUT-IDDC                         
014960            MOVE PARM-KVVECKOR-LT     TO OUT-KVVECKOR-LT                  
014970            PERFORM S11-WRITE-W2247401                                    
014991         END-IF                                                           
014992       END-IF                                                             
015000                                                                          
015100       PERFORM S02-READ-W01184                                            
015200     END-PERFORM                                                          
015300                                                                          
015400     PERFORM Z-FINIT                                                      
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100                                                                          
016200     OPEN INPUT  W22474-PARM                                              
016210                 W01184                                                   
016300          OUTPUT W22474                                                   
016400                                                                          
016600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016700     .                                                                    
016800     EJECT                                                                
018000 Z-FINIT SECTION.                                                         
018100     CLOSE W22474-PARM                                                    
018110           W01184                                                         
018200           W22474                                                         
018300     SKIP2                                                                
018400     MOVE 'S' TO POSTSUM-OPKOD                                            
018500     CALL POSTSUM USING POSTSUM-PARM                                      
018600     .                                                                    
018700     EJECT                                                                
018800 S01-READ-PARM-FROM-SCREEN-2115 SECTION.                                  
018900     MOVE 'S01-READ-PARM-FROM-SCREEN-2115'  TO CURRENT-SECTION            
019000                                                                          
019100     READ W22474-PARM   INTO PARM-AREA                                    
019110     AT END                                                               
019111        MOVE HIGH-VALUE TO PARM-AREA                                      
019120        SET END-OF-PARM TO TRUE                                           
019130                                                                          
019200     NOT AT END                                                           
019300       MOVE 'W224PP  '    TO POSTSUM-FDNAMN                               
019400       MOVE 'W22474D1'    TO POSTSUM-DDNAMN2                              
019500       MOVE 'PARM'        TO POSTSUM-TRANSTYP                             
019600       CALL POSTSUM USING POSTSUM-PARM                                    
019610     END-READ                                                             
019700     .                                                                    
019800     EJECT                                                                
019810 S02-READ-W01184  SECTION.                                                
019811     MOVE 'S02-READ-W01184 '  TO CURRENT-SECTION                          
019812                                                                          
019820     READ W01184 INTO SLAG-AREA                                           
019830     AT END                                                               
019840        MOVE HIGH-VALUE   TO SLAG-AREA                                    
019850        SET END-OF-W01184 TO TRUE                                         
019860                                                                          
019870     NOT AT END                                                           
019880        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
019890        MOVE 'W22474D2' TO POSTSUM-DDNAMN2                                
019891*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
019892        MOVE 'SLAG'     TO POSTSUM-TRANSTYP                               
019893        CALL POSTSUM USING POSTSUM-PARM                                   
019894     END-READ                                                             
019895     .                                                                    
019896     EJECT                                                                
019900 S11-WRITE-W2247401      SECTION.                                         
019910     MOVE 'S11-WRITE-W2247401            '  TO CURRENT-SECTION            
020000                                                                          
020100     WRITE OUT-W2247401 FROM OUT-AREA                                     
020200                                                                          
020400     MOVE 'W22474'   TO POSTSUM-FDNAMN                                    
020500     MOVE 'W22474D3' TO POSTSUM-DDNAMN2                                   
020510     MOVE 'OUT '     TO POSTSUM-TRANSTYP                                  
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     EJECT                                                                
020900 S99-ABEND SECTION.                                                       
021000                                                                          
021100     SKIP2                                                                
021200     MOVE 'S' TO POSTSUM-OPKOD                                            
021300     CALL POSTSUM USING POSTSUM-PARM                                      
021400     CALL ABEND USING RKOD-ABEND                                          
021500     .                                                                    
