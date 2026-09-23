000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400     SKIP3                                                                
000500 PROGRAM-ID.     W9105000.                                                
000630*              PROGRAM CONVERTED BY                                       
000700*              COBOL CONVERSION AID PO 5785-ABJ                           
000800*              CONVERSION DATE 05/25/91 17:41:31.                         
000900*AUTHOR.         INGRID DANIELSSON.                                       
001000*DATE-WRITTEN.   MARS 1982.                                               
001100*REMARKS.                                                                 
001200*    FUNKTION:                                                            
001300*             EN FIL MED INFORMATION SOM  P V  ÖNSKAR                     
001400*             TILLVERKAS I DETTA PROGRAM                                  
001410*             ARTIKELFIL TILL STICS TILLAGT NOV 2000                      
001500                                                                          
001600*             INFILERNA ÄR EXTRAKT FRÅN ARTIKELREG                        
001700                                                                          
001800                                                                          
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500                                                                          
002600* -------------------------  EXTRAKT UR ARTIKELREG                        
002700     SELECT  W91042-ART        ASSIGN  TO  UT-S-W91050D1.                 
002800                                                                          
002900* -------------------------  FILE TILL  P V                               
003000     SELECT  W91050-PV         ASSIGN  TO  UT-S-W91050D2.                 
003001                                                                          
003010* -------------------------  FILE TILL  STICS                             
003020     SELECT  W91090-STICS      ASSIGN  TO  UT-S-W91050D3.                 
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500                                                                          
003600 FD  W91042-ART                                                           
003700     RECORDING MODE F                                                     
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  POST    -COPY W91042     -PRE ART- -L                                
004200     SKIP3                                                                
004300 FD  W91050-PV                                                            
004400     RECORDING MODE F                                                     
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*01  POST    -COPY W91050L1   -PRE PV-  -L                                
004710     SKIP3                                                                
004800 FD  W91090-STICS                                                         
004810     RECORDING MODE F                                                     
004820     BLOCK CONTAINS 0.                                                    
004830                                                                          
004840*01  POST    -COPY W9105001   -PRE STICS-  -L                             
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100*    -- CHECKED BY WY2000                                                 
005500                                                                          
005510*    -COPY WY2000W2                                                       
005600     SKIP3                                                                
005700 77  JA                      PIC X                   VALUE 'J'.           
005800 77  NEJ                     PIC X                   VALUE 'N'.           
005900 77  IX                      PIC S9(4)  COMP SYNC    VALUE ZERO.          
006000     SKIP3                                                                
006100 01  SWITCHAR.                                                            
006200     03  EOF-ART-SW          PIC X                   VALUE 'N'.           
006300         88  EOF-ART                                 VALUE 'J'.           
006400     03  PV-SW               PIC X                   VALUE 'N'.           
006500         88  SKRIV-PV                                VALUE 'J'.           
006600     EJECT                                                                
006700 01  GENERELLA-SUB-PROGRAM.                                               
006800     03  WDATKONV            PIC X(8)   VALUE 'WDATKONV'.                 
006900     03  POSTSUM             PIC X(8)   VALUE 'POSTSUM '.                 
007000     SKIP2                                                                
007100 01  DATUM-FAELT.                                                         
007200     03  KONV-DATUM          PIC 9(5).                                    
007300     03  WS-DATUM  REDEFINES KONV-DATUM.                                  
007400         05  WS-AAR          PIC 9(2).                                    
007500         05  WS-VECKA        PIC 9(2).                                    
007600         05  WS-DAG          PIC 9(1).                                    
007700     SKIP2                                                                
007800 01  W-AARSKIFTE-DATUM.                                                   
007900     03  W-AARSKIFTE-AA      PIC 9(2).                                    
008000     03  W-AARSKIFTE-MM      PIC 9(2)   VALUE 12.                         
008100     03  W-AARSKIFTE-DD      PIC 9(2)   VALUE 31.                         
008200     SKIP2                                                                
008300 01  W-VECKA                 PIC 9(2).                                    
008400     SKIP2                                                                
008500 01  KONV-DATUM-PLUS-FEM     PIC 9(5).                                    
008501     SKIP2                                                                
008510 01  WS-TIME.                                                             
008520     03  WS-TIME-HHMMSS          PIC 9(6).                                
008530     03  FILLER                  PIC 9(2).                                
008540                                                                          
008600     EJECT                                                                
008700 01  FILLER                  PIC X(16)  VALUE 'WDATAREA'.                 
008800*01  -COPY WDATAREA.                                                      
009000     EJECT                                                                
009100*01  -COPY W0005        -PRE POSTSUM-.                                    
009300     SKIP2                                                                
009400     03  REG-TRANSID.                                                     
009500         05  REG-FD-NAMN     PIC X(6)   VALUE 'W91050'.                   
009600         05  REG-DDNAMN      PIC X(8)   VALUE 'W91050D2'.                 
009700         05  REG-TRANSTYP    PIC X(4)   VALUE SPACE.                      
009800     EJECT                                                                
009900*01  AREA    -COPY W91042        -PRE WART-                               
010100     EJECT                                                                
010200*01  AREA    -COPY W91050L1      -PRE WPV-                                
010400     EJECT                                                                
010410*01  AREA    -COPY W9105001      -PRE WSTICS-                             
010420     EJECT                                                                
010500 PROCEDURE DIVISION.                                                      
010600     SKIP2                                                                
010700     PERFORM A-INIT                                                       
010800     SKIP3                                                                
010900     PERFORM S01-LAES-ART                                                 
011000                                                                          
011200     PERFORM UNTIL  EOF-ART                                               
011300       PERFORM B-BEHANDLA-PV-ART                                          
011400       IF SKRIV-PV                                                        
011500         PERFORM S02-SKRIV-PV                                             
011510         PERFORM S03-SKRIV-STICS                                          
011600       END-IF                                                             
011700       PERFORM S01-LAES-ART                                               
011800     END-PERFORM                                                          
011900     SKIP3                                                                
012000     PERFORM Z-SLUT                                                       
012100                                                                          
012200     MOVE 0 TO RETURN-CODE                                                
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600 A-INIT SECTION.                                                          
012700     SKIP2                                                                
012800     OPEN INPUT W91042-ART                                                
012900         OUTPUT W91050-PV                                                 
012910         OUTPUT W91090-STICS                                              
013000*                                                                         
013100     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
013200     CALL WDATKONV USING DAT-KDDATFORM                                    
013300                         DAT-I-TIDATUM                                    
013400                         DAT-O-TIDATUM                                    
013500                         DAT-KDSVAR                                       
013600                                                                          
013700     MOVE DAT-TIAAVVD TO KONV-DATUM                                       
013800     MOVE DAT-TIAA    TO W-AARSKIFTE-AA                                   
013900                                                                          
014000     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
014100     MOVE W-AARSKIFTE-DATUM  TO DAT-I-TIDATUM                             
014200                                                                          
014300     CALL WDATKONV USING DAT-KDDATFORM                                    
014400                         DAT-I-TIDATUM                                    
014500                         DAT-O-TIDATUM                                    
014600                         DAT-KDSVAR                                       
014700                                                                          
014800     MOVE DAT-TIVV    TO W-VECKA                                          
014900                                                                          
015000     ADD 2 TO WS-VECKA                                                    
015100     IF WS-VECKA > W-VECKA                                                
015200       ADD 1 TO WS-AAR                                                    
015300       SUBTRACT W-VECKA FROM WS-VECKA                                     
015400     END-IF                                                               
015500     MOVE KONV-DATUM TO KONV-DATUM-PLUS-FEM                               
015501                                                                          
015510     MOVE FUNCTION CURRENT-DATE(1:8) TO WSTICS-DADATUM                    
015520     ACCEPT WS-TIME FROM TIME                                             
015530     MOVE WS-TIME-HHMMSS        TO WSTICS-TIHHMMSS                        
015540     MOVE 'QP6'                 TO WSTICS-IDPTYP                          
015550     MOVE 'W111Z1SE '           TO WSTICS-PARTNER-ID                      
015600     .                                                                    
015700     EJECT                                                                
015800 B-BEHANDLA-PV-ART SECTION.                                               
015900     SKIP2                                                                
016000     MOVE NEJ TO PV-SW                                                    
016100                                                                          
016101     MOVE WART-TIFINLV          TO TMP1-YYWWD                             
016102     MOVE KONV-DATUM-PLUS-FEM   TO TMP2-YYWWD                             
016110     PERFORM WY2000P2                                                     
016200     IF TMP1-YYWWD > TMP2-YYWWD OR                                        
016300        WART-KDERS    = +52                   OR                          
016400        WART-KDERS-UTG    > 0                 OR                          
016500        WART-PRARTSTD     = +0                                            
016600*                  * DESSA ARTIKLAR SKALL EJ MED *                        
016700       CONTINUE                                                           
016800     ELSE                                                                 
017200         MOVE WART-IDARTNR   TO WPV-IDARTNR                               
017210                                WSTICS-IDARTNR                            
017300         MOVE WART-REKSIFFR  TO WPV-REKSIFFR                              
017310                                WSTICS-REKSIFFR                           
017400         MOVE WART-IDFKNGRP  TO WPV-IDFKNGRP                              
017410                                WSTICS-IDFKNGRP-005                       
017500         MOVE +1 TO IX                                                    
017600         PERFORM UNTIL                                                    
017700          NOT ( IX < +11 )                                                
017800           EVALUATE WART-IDSKYLT (IX)                                     
017900           WHEN 'GB '                                                     
018000             MOVE WART-BEART (IX) TO WPV-BEART-ENG                        
018010                                     WSTICS-BEART-ENG                     
018100           WHEN 'S  '                                                     
018200             MOVE WART-BEART (IX) TO WPV-BEART-SVE                        
018210                                     WSTICS-BEART-SVE                     
018300           END-EVALUATE                                                   
018400           ADD +1 TO IX                                                   
018500         END-PERFORM                                                      
018600         MOVE ZERO               TO WPV-PRARTBTO-EXP                      
018700         MOVE ZERO               TO WPV-REOMRTAL-MO                       
018800         MOVE WART-KDPRODSL      TO WPV-KDPRODSL                          
018810                                    WSTICS-KDPRODSL                       
018900                                                                          
019000         MOVE JA TO PV-SW                                                 
019200     END-IF                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 Z-SLUT SECTION.                                                          
019600     SKIP2                                                                
019700     CLOSE W91042-ART                                                     
019800           W91050-PV                                                      
019810           W91090-STICS                                                   
019900**********             SKRIV UT ANTAL SKRIVNA POSTER                      
020000     MOVE 'S' TO POSTSUM-OPKOD                                            
020100     MOVE REG-TRANSID TO POSTSUM-TRANSID                                  
020200     CALL POSTSUM USING POSTSUM-PARM                                      
020300     .                                                                    
020400     SKIP3                                                                
020500 S01-LAES-ART    SECTION.                                                 
020600     SKIP2                                                                
020700     READ W91042-ART INTO WART-AREA                                       
020800                     AT END MOVE JA TO EOF-ART-SW                         
020900     END-READ                                                             
021000     .                                                                    
021100     SKIP3                                                                
021200 S02-SKRIV-PV    SECTION.                                                 
021300     SKIP2                                                                
021400     WRITE PV-POST FROM WPV-AREA                                          
021500     MOVE REG-TRANSID TO POSTSUM-TRANSID                                  
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
021710     SKIP3                                                                
021720 S03-SKRIV-STICS SECTION.                                                 
021730                                                                          
021740     WRITE STICS-POST FROM WSTICS-AREA                                    
021750     MOVE REG-TRANSID TO POSTSUM-TRANSID                                  
021760     CALL POSTSUM USING POSTSUM-PARM                                      
021770     .                                                                    
021780     EJECT                                                                
021800*    -COPY WY2000P2                                                       
