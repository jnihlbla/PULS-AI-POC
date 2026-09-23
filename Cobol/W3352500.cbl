000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3352500.                                                
000400 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000500 DATE-WRITTEN.   08/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SENDS CAMPAIGN INFO TO PRICE MC SYSTEM                           
001100*                                                                         
001110*        PROGRAMMET LÄSER DB2  TP1ARTK                                    
001120*                MED "JOIN" AV TP1KAMP                                    
001200*                                                                         
001300*    ABENDCODES:                                                          
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
002401     SKIP2                                                                
002402*          --- UTFIL                                                      
002410     SELECT W33525                     ASSIGN TO W33525D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W33525                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005     SKIP2                                                                
003010*01  POST -COPY W33525 -PRE  UT-  -L.                                     
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W3352500'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003900     EJECT                                                                
003910 01  WS.                                                                  
003920     03  WS-ANTAL-UTDATA         PIC 9(9)    VALUE ZERO.                  
003930     03  WS-ANTAL-INSERT         PIC 9(9)    VALUE ZERO.                  
003940     03  WS-ANTAL-UPDATE         PIC 9(9)    VALUE ZERO.                  
003941     03 FILLER                   PIC X(16)   VALUE                        
003942                                             'WS-DB2-SEKTION'.            
003943     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
003944     EJECT                                                                
003950                                                                          
003960 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
003970 01  FILLER REDEFINES DAGENS-DATUM.                                       
003981     03  DAGENS-DATUM-AAAA       PIC 9(4).                                
003990     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
003991     03  DAGENS-DATUM-DAG        PIC 9(2).                                
003992                                                                          
003993 01  WS-JMFR-AAAAMMDD            PIC 9(8).                                
003994 01  FILLER REDEFINES WS-JMFR-AAAAMMDD.                                   
003995     03  WS-JMFR-AA              PIC 9(2).                                
003996     03  WS-JMFR-AAMMDD          PIC 9(6).                                
003998     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004903     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
004904     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERRTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006102*01  -COPY ABEND                                                          
006103     EJECT                                                                
006104*01  -COPY CBLTDLI                                                        
006105     EJECT                                                                
006106*01  -COPY FELLOG                                                         
006107     EJECT                                                                
006110*01  -COPY POSTSUM                                                        
006201     EJECT                                                                
006202 01  UT-AREA-START             PIC X(24)   VALUE                          
006203                                 'UT-AREA-START'.                         
006204     SKIP2                                                                
006210*01  AREA -COPY W33525     -PRE UT-                                       
006310     EJECT                                                                
006320 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
006330       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006340                                                                          
006350 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
006360 01  DB2-WS.                                                              
006370     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
006380         88  CURSOR-OK                       VALUE 000.                   
006390         88  ROW-FOUND                       VALUE 000.                   
006391         88  ROW-NOTFOUND                    VALUE 100.                   
006392         88  RESOURCE-WRONG                  VALUE 904.                   
006393     03  GOOD-SQLCODECODES.                                               
006394         05  GOOD-SQLCODE OCCURS 5                                        
006395             INDEXED BY SQLCODE-IX PIC 9(3).                              
006396*                                                                         
006397*    ---  DB2 HOST-COPYTEXTER                                             
006398 01  FILLER                     PIC X(16)  VALUE 'TP1ARTK-AREA'.          
006399*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
006400     EJECT                                                                
006402 01   FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.        
006403*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
006404     EJECT                                                                
006405                                                                          
006406*    ---  DB2 DCL                                                         
006407 01   FILLER                     PIC X(16)   VALUE 'TP1ARTK DCL '.        
006408     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
006409 01   FILLER                     PIC X(16)   VALUE 'TP1KAMP DCL '.        
006410     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
006411     EJECT                                                                
006412                                                                          
006413     EJECT                                                                
006420 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006600     ENTRY 'DLITCBL'.                                                     
006800                                                                          
006900     PERFORM A-INIT                                                       
007000                                                                          
007010     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
007020                                                                          
007030     IF ROW-FOUND                                                         
007040       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
007050     END-IF                                                               
007060                                                                          
007070     PERFORM UNTIL ROW-NOTFOUND                                           
007080                                                                          
007090       IF TP1KAMP-TISTODAT-KAMP  > ZERO                                   
007091       DISPLAY 'TISTODAT, JMFR = ' TP1KAMP-TISTODAT-KAMP '/'              
007092                                  WS-JMFR-AAMMDD                          
007093         IF TP1KAMP-TISTODAT-KAMP >= WS-JMFR-AAMMDD                       
007094           MOVE TP1ARTK-IDARTNR       TO UT-IDARTNR                       
007095           MOVE TP1KAMP-IDKAMP        TO UT-IDKAMP                        
007096           MOVE TP1KAMP-IDKAMP-GRP    TO UT-IDKAMP-GRP                    
007097           MOVE TP1KAMP-KVKAMP-CARS   TO UT-KVKAMP-CARS                   
007098           MOVE TP1KAMP-TISTADAT-KAMP TO UT-TISTADAT-KAMP                 
007099           MOVE TP1KAMP-TISTODAT-KAMP TO UT-TISTODAT-KAMP                 
007101           MOVE TP1KAMP-KDKAMP        TO UT-KDKAMP                        
007102           PERFORM S11-WRITE-W33525                                       
007103         END-IF                                                           
007104       END-IF                                                             
007105                                                                          
007106       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
007107                                                                          
007108     END-PERFORM                                                          
007109                                                                          
007110     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
007120                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
009001                                                                          
009010     OPEN OUTPUT W33525                                                   
009100     SKIP2                                                                
009300     MOVE FUNCTION CURRENT-DATE(1:8)   TO DAGENS-DATUM                    
009301     MOVE DAGENS-DATUM TO WS-JMFR-AAAAMMDD                                
009302** STOPPDATUM FÅR VAR 1 ÅR GAMMALT                                        
009303     SUBTRACT 10000 FROM WS-JMFR-AAAAMMDD                                 
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009320                                                                          
009330     INITIALIZE GOOD-SQLCODECODES                                         
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009700                                                                          
009802     DISPLAY 'ANTAL UTDATA : ' WS-ANTAL-UTDATA                            
009803     CLOSE W33525                                                         
009804     SKIP2                                                                
009900     .                                                                    
010101     EJECT                                                                
010102 S11-WRITE-W33525 SECTION.                                                
010103                                                                          
010104     WRITE UT-POST FROM UT-AREA                                           
010105                                                                          
010106     ADD 1                   TO WS-ANTAL-UTDATA                           
010120     .                                                                    
010300     EJECT                                                                
010310 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
010320     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
010330                                                                          
010340     MOVE 000100  TO GOOD-SQLCODECODES                                    
010350                                                                          
010360     EXEC SQL                                                             
010370         DECLARE TP1ARTK-CRS CURSOR FOR                                   
010380           SELECT  A.IDARTNR                                              
010390                  ,B.IDKAMP                                               
010391                  ,B.IDKAMP_GRP                                           
010392                  ,B.KVKAMP_CARS                                          
010393                  ,B.TISTADAT_KAMP                                        
010394                  ,B.TISTODAT_KAMP                                        
010395                  ,B.KDKAMP                                               
010396                                                                          
010397           FROM    TP1ARTK A                                              
010398                  ,TP1KAMP B                                              
010399                                                                          
010400           WHERE   A.IDKAMP  =  B.IDKAMP                                  
010401                                                                          
010402           ORDER BY B.IDKAMP                                              
010403     END-EXEC                                                             
010404                                                                          
010405     MOVE 000100  TO GOOD-SQLCODECODES                                    
010406     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
010407     .                                                                    
010408     SKIP3                                                                
010409 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
010410     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
010411     SKIP2                                                                
010412     MOVE 000100  TO GOOD-SQLCODECODES                                    
010413     EXEC SQL                                                             
010414         FETCH TP1ARTK-CRS INTO                                           
010415                    :TP1ARTK-IDARTNR                                      
010416                   ,:TP1KAMP-IDKAMP                                       
010417                   ,:TP1KAMP-IDKAMP-GRP                                   
010418                   ,:TP1KAMP-KVKAMP-CARS                                  
010419                   ,:TP1KAMP-TISTADAT-KAMP                                
010420                   ,:TP1KAMP-TISTODAT-KAMP                                
010421                   ,:TP1KAMP-KDKAMP                                       
010422     END-EXEC                                                             
010423                                                                          
010424     MOVE SQLCODE TO SQLCODE-WS                                           
010425     PERFORM DB2-STATUS-CHECK                                             
010426     .                                                                    
010427     SKIP3                                                                
010428 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
010429     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
010430                                                                          
010431     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
010432     .                                                                    
010433     EJECT                                                                
010434                                                                          
010435 DB2-STATUS-CHECK  SECTION.                                               
010436                                                                          
010437     SET SQLCODE-IX TO 1                                                  
010438     SEARCH GOOD-SQLCODE                                                  
010439       AT END CALL FELLOG                                                 
010440       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
010441     END-SEARCH                                                           
010442     .                                                                    
010443                                                                          
010450 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
