000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3300600.                                                 
000400 AUTHOR.        RONNY STENHOLM                                            
000500 DATE-WRITTEN.  AUG 1989.                                                 
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        FÅNGAR KREDITNOTOR.                                              
001000*        PROGRAMMET LÄSER EN FIL FRÅN W418 SYSTEMET.                      
001100*        BYTER DATUMTYP OCH POSTTYP. POSTERNA LÄGGS DÄREFTER              
001200*        I EN NY FIL (W33007).                                            
001300*        PERIODICITET : DAGLIGEN                                          
001400*                                                                         
001500*    SUBPROGRAM:                                                          
001600*        WDATKONV - UTFÖR KONVERTERING AV DATUM ÅÅMMDD TILL ÅÅVV          
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*    --- INFIL:                                                           
002600                                                                          
002700     SELECT W41839                       ASSIGN TO W33006D1.              
002800                                                                          
002900     SKIP2                                                                
003000*    --- UTFIL:                                                           
003100     SELECT W33007                       ASSIGN TO W33006D2.              
003200     SKIP2                                                                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W41839                                                               
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*    -COPY W330099   -L.                                                  
004500     SKIP2                                                                
004600     EJECT                                                                
004700 FD  W33007                                                               
004800     LABEL RECORD   STANDARD                                              
004900     RECORDING      V                                                     
005000     BLOCK CONTAINS 0.                                                    
005100     SKIP2                                                                
005200*01  POST -COPY W330100  -PRE UTREG-  -L.                                 
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005601                                                                          
005610*    -- CHECKED BY WY2000                                                 
005700 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3300600'.            
005800*    --- FLAGGOR                                                          
005900 77  W41839-EOF-SW               PIC X(1)    VALUE 'N'.                   
006000     88  END-OF-W41839                       VALUE 'J'.                   
006100     SKIP3                                                                
006200                                                                          
006300 01  WS-AAVVD.                                                            
006400*                                                                         
006500     03  WS-AAVV                 PIC 9(4).                                
006600     03  FILLER                  PIC 9(1).                                
006700     EJECT                                                                
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
007500*                                                                         
007600*01  -COPY W0005 -PRE  POSTSUM-                                           
007800*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
007900*                                                                         
008000*01  -COPY WDATAREA                                                       
008200     EJECT                                                                
008300 01  UT-AREA-START                PIC X(24)   VALUE                       
008400                                            'UT-AREA-START  '.            
008500     SKIP3                                                                
008600 01  UT-AREA.                                                             
008700     03  FILLER                   PIC X(25).                              
008800     SKIP2                                                                
008900*01  FILLER  -PRE UT-  -COPY W330100 -RED UT-AREA                         
009100     EJECT                                                                
009200 01  IN-AREA-START                PIC X(24) VALUE                         
009300                                            'IN-AREA-START  '.            
009400 01  IN-AREA.                                                             
009500     03  FILLER                   PIC X(26).                              
009600     SKIP2                                                                
009700*01  FILLER  -PRE  IN-  -COPY W330099 -RED IN-AREA                        
009900     EJECT                                                                
010000                                                                          
010100 PROCEDURE DIVISION.                                                      
010200     SKIP2                                                                
010300 STYR SECTION.                                                            
010400     PERFORM A-INIT                                                       
010500     PERFORM D-LAS-W418                                                   
010600     PERFORM UNTIL END-OF-W41839                                          
010700       PERFORM B-BEHANDLA-POST                                            
010800       PERFORM C-SKRIV-POST                                               
010900       PERFORM D-LAS-W418                                                 
011000     END-PERFORM                                                          
011100     PERFORM Z-FINIT                                                      
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600                                                                          
011700 A-INIT SECTION.                                                          
011800     SKIP2                                                                
011900     OPEN INPUT W41839                                                    
012000     SKIP2                                                                
012100     OPEN OUTPUT W33007                                                   
012200     SKIP2                                                                
012300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
012400     .                                                                    
012500     EJECT                                                                
012600                                                                          
012700 B-BEHANDLA-POST  SECTION.                                                
012800     SKIP2                                                                
012900     MOVE IN-TIFAKT TO DAT-I-TIDATUM                                      
013000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
013100     CALL WDATKONV USING DAT-KDDATFORM                                    
013200                         DAT-I-TIDATUM                                    
013300                         DAT-O-TIDATUM                                    
013400                         DAT-KDSVAR                                       
013500     MOVE DAT-TIAAVVD TO WS-AAVVD                                         
013601*--- --- FIX FIX VECKA 9253 LÄGGS TILL 9252 JP/RS                         
013602*    MOVE WS-AAVV     TO UT-TIFSGVV                                       
013603     IF WS-AAVV NOT = 9253                                                
013604       MOVE WS-AAVV     TO UT-TIFSGVV                                     
013605     ELSE                                                                 
013606       MOVE 9252        TO UT-TIFSGVV                                     
013607     END-IF                                                               
013610*--- --- FIX FIX VECKA 9253 LÄGGS TILL 9252 JP/RS                         
013700                                                                          
013800     MOVE 100         TO UT-IDPTYP                                        
013900     MOVE IN-IDARTNR  TO UT-IDARTNR                                       
014000     MOVE IN-IDDISTR  TO UT-IDDISTR                                       
014100     MOVE IN-KVLEVART  TO UT-KVLEVART                                     
014200     MOVE IN-PRARTNTO TO UT-PRARTNTO                                      
014300     MOVE IN-KDPRTYP  TO UT-KDPRTYP                                       
014400     MOVE IN-KDORDKL  TO UT-KDORDKL                                       
014500     .                                                                    
014600                                                                          
014700                                                                          
014800                                                                          
014900 C-SKRIV-POST  SECTION.                                                   
015000     SKIP2                                                                
015100     WRITE UTREG-POST FROM UT-AREA.                                       
015200     MOVE SPACE TO POSTSUM-TRANSTYP                                       
015300     MOVE 'W33007' TO POSTSUM-FDNAMN                                      
015400     MOVE 'W33006D2' TO POSTSUM-DDNAMN2                                   
015500     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015700     EJECT                                                                
015800                                                                          
015900 D-LAS-W418  SECTION.                                                     
016000     SKIP2                                                                
016100     READ W41839 INTO IN-AREA                                             
016200     AT END                                                               
016300         SET END-OF-W41839 TO TRUE                                        
016400     END-READ                                                             
016500     IF NOT END-OF-W41839                                                 
016600       MOVE SPACE TO POSTSUM-TRANSTYP                                     
016700       MOVE 'W41839' TO POSTSUM-FDNAMN                                    
016800       MOVE 'W33006D1' TO POSTSUM-DDNAMN2                                 
016900       CALL POSTSUM USING POSTSUM-PARM                                    
017000     END-IF                                                               
017100     .                                                                    
017200                                                                          
017300                                                                          
017400                                                                          
017500 Z-FINIT SECTION.                                                         
017600     SKIP2                                                                
017700     CLOSE W41839                                                         
017800           W33007                                                         
017900     MOVE 'S' TO POSTSUM-OPKOD                                            
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     SKIP2                                                                
018300     EJECT                                                                
