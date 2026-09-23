000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3719B00.                                                
000500*AUTHOR.         RONNY STENHOLM.                                          
000600*DATE-WRITTEN.   92/04/09.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        ÖFGJL                                                            
001200*                                                                         
001210*                                                                         
001300*        PROGRAMMET UPPDATERAR TABELL BYART                               
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
002700*          --- KD                                                         
002800     SELECT W3719A                     ASSIGN TO W3719BD1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W3719A                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY W3719A      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W3719B00'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
004600     SKIP2                                                                
004700 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100 77  W3719A-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W3719A                       VALUE 'J'.                   
005300     EJECT                                                                
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900     EJECT                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
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
007200                                                                          
007300*01  AREA -COPY W3719A     -PRE IN-                                       
007400*                                                                         
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
007700     EXEC SQL INCLUDE SQLCA END-EXEC.                                     
007800                                                                          
007900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008000 01  DB2-WS.                                                              
008100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
008200         88  CURSOR-OK                       VALUE 000.                   
008300         88  RADER-FINNS                     VALUE 000.                   
008400         88  RADER-SAKNAS                    VALUE 100.                   
008500         88  ATKOMST-FEL                     VALUE 904.                   
008600     03  GODK-SQLCODEKODER.                                               
008700         05  GODK-SQLCODE OCCURS 5                                        
008800             INDEXED BY SQLCODE-IX PIC 9(3).                              
008900     EJECT                                                                
009000*    ---  DB2 INPUT-OUTPUT AREA                                           
009100 01  FILLER                      PIC X(16)   VALUE 'BYART-AREA'.          
009200*01  FILLER -COPY BYART -PRE BYART-                                       
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'BYART-DB2 '.          
009500     EXEC SQL INCLUDE BYART  END-EXEC.                                    
009600     EJECT                                                                
009700 PROCEDURE DIVISION.                                                      
009800                                                                          
009900     SKIP2                                                                
010000     PERFORM A-INIT                                                       
010100     PERFORM S01-LAES-W3719A                                              
010200     PERFORM UNTIL END-OF-W3719A                                          
010300        MOVE IN-IDARTNR TO W-IDARTNR-BYT                                  
010400        MOVE IN-IDDISTR-RENOV TO BYART-IDDISTR-RENOV                      
010410        MOVE IN-KVBYTPKO TO BYART-KVBYTPKO                                
010500        PERFORM DB2-UPDATE-BYART-TAB                                      
010501        IF RADER-SAKNAS                                                   
010510          DISPLAY 'ARTNR SAKNAS'                                          
010520          DISPLAY IN-IDARTNR ' ' IN-IDDISTR-RENOV                         
010530        END-IF                                                            
010600        PERFORM S01-LAES-W3719A                                           
010700     END-PERFORM                                                          
010800                                                                          
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100                                                                          
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600 A-INIT SECTION.                                                          
011700     SKIP2                                                                
011800                                                                          
011900     OPEN INPUT W3719A                                                    
012000                                                                          
012100     ACCEPT DAGENS-DATUM       FROM DATE                                  
012200                                                                          
012300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012400                                                                          
012500     INITIALIZE GODK-SQLCODEKODER                                         
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012900                                                                          
013000                                                                          
013100     CLOSE W3719A                                                         
013200     SKIP2                                                                
013300     MOVE 'S' TO POSTSUM-OPKOD                                            
013400     CALL POSTSUM USING POSTSUM-PARM                                      
013500     .                                                                    
013600     EJECT                                                                
013700 S01-LAES-W3719A  SECTION.                                                
013800     SKIP2                                                                
013900     READ W3719A INTO IN-AREA                                             
014000                                                                          
014100     AT END                                                               
014200       MOVE JA  TO W3719A-EOF-SW                                          
014300     NOT AT END                                                           
014400        MOVE 'W3719A' TO POSTSUM-FDNAMN                                   
014500        MOVE 'W3719BD1' TO POSTSUM-DDNAMN2                                
014600        MOVE 'IN' TO POSTSUM-TRANSTYP                                     
014700        CALL POSTSUM USING POSTSUM-PARM                                   
014800     END-READ                                                             
014900     .                                                                    
015000     EJECT                                                                
015100 DB2-UPDATE-BYART-TAB  SECTION.                                           
015200     SKIP2                                                                
015400     MOVE 000100  TO GODK-SQLCODEKODER                                    
015500     EXEC SQL                                                             
015600       UPDATE BYART                                                       
015700        SET IDDISTR_RENOV = :BYART-IDDISTR-RENOV,                         
015710            KVBYTPKO      = :BYART-KVBYTPKO                               
015800        WHERE IDARTNR_BYT = :W-IDARTNR-BYT                                
015900     END-EXEC                                                             
016000     MOVE SQLCODE           TO SQLCODE-WS                                 
016100     PERFORM DB2-STATUS-KONTROLL                                          
016200     .                                                                    
016300     EJECT                                                                
016400 DB2-STATUS-KONTROLL  SECTION.                                            
016500     SKIP2                                                                
016600     SET SQLCODE-IX TO 1                                                  
016700     SEARCH GODK-SQLCODE                                                  
016800       AT END CALL FELLOG                                                 
016900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
017000     END-SEARCH                                                           
017100     .                                                                    
