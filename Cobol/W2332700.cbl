000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2332700.                                                
000400 AUTHOR.         STENING INGER.                                           
000500 DATE-WRITTEN.   19/09/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CREATE EXTRACT FILE WITH DATA FROM HERKULES SYSTEM               
001100*                                                                         
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
002401                                                                          
002402*          --- HERCULES FILE                                              
002403     SELECT W23327                     ASSIGN TO W23327D1.                
002405*          --- HERCULES FILE - START RECORD REC.NO AND TOT REC NO         
002410     SELECT W23327A                    ASSIGN TO W23327D2.                
002420*          --- HERCULES FILE - EXTRACT DATA                               
002421     SELECT W23327B                    ASSIGN TO W23327D3.                
002422                                                                          
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W23327                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  RECORD  -COPY W23327A  -PRE IN-  -L.                                 
003007*01  RECORD1 -COPY W23327B  -PRE IN-  -L.                                 
003008     SKIP3                                                                
003009 FD  W23327A                                                              
003010     RECORDING       F                                                    
003011     BLOCK CONTAINS  0.                                                   
003012                                                                          
003020*01  RECORD -COPY W23327A -PRE  OUT-START-  -L.                           
003100     EJECT                                                                
003110 FD  W23327B                                                              
003120     RECORDING       F                                                    
003130     BLOCK CONTAINS  0.                                                   
003140                                                                          
003150*01  RECORD -COPY W23327B -PRE  OUT-LINE-   -L.                           
003160     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2332700'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003806 77  W23327-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W23327                      VALUE 'J'.                    
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
006204 01  IN-AREA                     PIC X(32).                               
006207*01  AREA       -COPY W23327A    -PRE IN-START- -RED IN-AREA.             
006209*01  AREA1      -COPY W23327B    -PRE IN-LINE-  -RED IN-AREA.             
006306     EJECT                                                                
006307                                                                          
006308 01  FILLER                      PIC X(24)   VALUE                        
006309                                'OUT-START-AREA'.                         
006311*01  AREA -COPY W23327A    -PRE OUT-START-                                
006320     EJECT                                                                
006330 01  FILLER                      PIC X(24)   VALUE                        
006340                                 'OUT-LINE-AREA'.                         
006350*01  AREA -COPY W23327B    -PRE OUT-LINE-                                 
006360     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006800                                                                          
006900     PERFORM A-INIT                                                       
007000                                                                          
007060     PERFORM S01-READ-W23327                                              
007100     PERFORM UNTIL END-OF-W23327                                          
007200       IF IN-START-IDPTYP = 'SV0'                                         
007201          MOVE IN-START-IDPTYP        TO OUT-START-IDPTYP                 
007202          MOVE IN-START-IDPOST        TO OUT-START-IDPOST                 
007224          MOVE IN-START-KVPOST-TOT    TO OUT-START-KVPOST-TOT             
007230          PERFORM S11A-WRITE-W23327A                                      
007240       ELSE                                                               
007250          IF IN-LINE-IDPTYP = 'SV1'                                       
007260             MOVE IN-LINE-IDPTYP      TO OUT-LINE-IDPTYP                  
007300             MOVE IN-LINE-KDPGMMRK    TO OUT-LINE-KDPGMMRK                
007400             MOVE IN-LINE-IDLANDX2    TO OUT-LINE-IDLANDX2                
007410             MOVE IN-LINE-IDFABRIK    TO OUT-LINE-IDFABRIK                
007500             MOVE IN-LINE-IDARTNR     TO OUT-LINE-IDARTNR                 
007600             IF IN-LINE-DAAAVV = SPACE                                    
007601                MOVE ZERO             TO OUT-LINE-DAAAVV                  
007602             ELSE                                                         
007610                MOVE IN-LINE-DAAAVV   TO OUT-LINE-DAAAVV                  
007620             END-IF                                                       
007700             MOVE IN-LINE-VLPART      TO OUT-LINE-VLPART                  
007801             PERFORM S11B-WRITE-W23327B                                   
007820          END-IF                                                          
007821       END-IF                                                             
007830       PERFORM S01-READ-W23327                                            
007900     END-PERFORM                                                          
008000                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W23327                                                   
009010          OUTPUT W23327A                                                  
009020                 W23327B                                                  
009100                                                                          
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W23327                                                         
009710           W23327A                                                        
009720           W23327B                                                        
009801                                                                          
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W23327  SECTION.                                                
010003                                                                          
010004     READ W23327 INTO IN-AREA                                             
010005     AT END                                                               
010006        MOVE HIGH-VALUE   TO IN-AREA                                      
010007        SET END-OF-W23327 TO TRUE                                         
010008                                                                          
010009     NOT AT END                                                           
010010        MOVE 'W23327'   TO POSTSUM-FDNAMN                                 
010011        MOVE 'W23327D1' TO POSTSUM-DDNAMN2                                
010013        MOVE SPACE      TO POSTSUM-TRANSTYP                               
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010105 S11A-WRITE-W23327A SECTION.                                              
010106                                                                          
010107     WRITE OUT-START-RECORD FROM OUT-START-AREA                           
010108                                                                          
010109     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010110     MOVE 'W23327A'  TO POSTSUM-FDNAMN                                    
010111     MOVE 'W23327D2' TO POSTSUM-DDNAMN2                                   
010112     CALL POSTSUM USING POSTSUM-PARM                                      
010120     .                                                                    
010300     EJECT                                                                
010310 S11B-WRITE-W23327B SECTION.                                              
010320                                                                          
010330     WRITE OUT-LINE-RECORD FROM OUT-LINE-AREA                             
010340                                                                          
010350     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010360     MOVE 'W23327B'  TO POSTSUM-FDNAMN                                    
010370     MOVE 'W23327D3' TO POSTSUM-DDNAMN2                                   
010380     CALL POSTSUM USING POSTSUM-PARM                                      
010390     .                                                                    
010391     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
