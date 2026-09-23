000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3305800.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000810*    LÄSER FILEN W33057 SOM INNEHÅLLER RESULTAT (STORSÄLJARE)             
000820*    OCH FILEN W33058 MED SAMMA RESULTAT MEN ANNAN SORTERING              
000830*    MATCHAR MED W33031(URVAL), SORTERAR OCH SKRIVER FILEN W33055         
000840*    W33057 ÄR SORTERAD PÅ FSG-SUMMA.                                     
000850*    W33058 ÄR SORTERAD PÅ FSG-ANTAL.                                     
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*    --- INFILER:                                                         
001900     SELECT SORTFIL                      ASSIGN TO W33058DS.              
002000     SELECT W33031                       ASSIGN TO W33058D1.              
002100     SELECT W33057S                      ASSIGN TO W33058D2.              
002110     SELECT W33058S                      ASSIGN TO W33058D3.              
002200*    --- UTFILER:                                                         
002300     SELECT W33059                       ASSIGN TO W33058D4.              
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 SD  SORTFIL                                                              
003000     RECORDING      F                                                     
003100     SKIP2                                                                
003200 01  SORT-POST.                                                           
003300   03  IDUSER                       PIC X(8).                             
003400   03  DAREGDAT                     PIC  9(8).                            
003500   03  TIREGTID                     PIC S9(7) COMP-3 VALUE ZERO.          
003600   03  FILLER                       PIC X(146).                           
003700     EJECT                                                                
003800 FD  W33031                                                               
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      V                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*01  POST -COPY W3303103  -L -PRE I331-.                                  
004500     SKIP2                                                                
004600*01  POST -COPY W3303102  -L -PRE I231-.                                  
004800     SKIP2                                                                
004900*01  POST -COPY W3303104  -L -PRE I431-.                                  
005100     EJECT                                                                
005200 FD  W33057S                                                              
005300     LABEL RECORD   STANDARD                                              
005400     RECORDING      F                                                     
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700*01  POST -COPY W33057  -L -PRE I57-.                                     
005810 FD  W33058S                                                              
005820     LABEL RECORD   STANDARD                                              
005830     RECORDING      F                                                     
005840     BLOCK CONTAINS 0.                                                    
005850     SKIP2                                                                
005860*01  POST -COPY W33057  -L -PRE I58-.                                     
005900     EJECT                                                                
006000 FD  W33059                                                               
006100     LABEL RECORD   STANDARD                                              
006200     RECORDING      V                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*01  POST -COPY W33059  -L -PRE U059-.                                    
006700     SKIP2                                                                
006800*01  POST -COPY W3303104  -L -PRE U459-.                                  
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200     SKIP2                                                                
007201                                                                          
007210*    -- CHECKED BY WY2000                                                 
007300 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3305800'.            
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600 77  IN57-EOF                    PIC X(1)    VALUE 'N'.                   
007610 77  IN58-EOF                    PIC X(1)    VALUE 'N'.                   
007700 77  IN31-EOF                    PIC X(1)    VALUE 'N'.                   
007800 77  SORT-EOF                    PIC X(1)    VALUE 'N'.                   
007900 77  IX                          PIC S9(9)   VALUE +1  COMP SYNC.         
008000 77  WS-IDRADNR                  PIC S9(9)   VALUE +1  COMP SYNC.         
008100 77  WS-RELEVANT-RAAR         PIC S9(5)V9(1) VALUE ZERO.                  
008200 77  FELKOD                      PIC S9(9)   VALUE +16 COMP SYNC.         
008300                                                                          
008400     EJECT                                                                
008500                                                                          
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     SKIP2                                                                
009100*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
009200*                                                                         
009300 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
009400*01  -COPY W0005 -PRE  POSTSUM-                                           
009600     EJECT                                                                
009700 01  FILLER                       PIC X(16)  VALUE 'IN31-FILEN'.          
009800*                                                                         
009900*     FIL W33031                                                          
010000 01  I31-AREA.                                                            
010100   03  FILLER                         PIC X(28).                          
010200   03  I31-IDPTYP                     PIC X(3).                           
010300   03  I31-IDGTYP                     PIC S9(1) COMP-3.                   
010400   03  I31-IDTRANS                    PIC X(4).                           
010500   03  FILLER                         PIC X(2572).                        
010600*01  AREA  -COPY W3303104  -PRE SORT-.                                    
010800     EJECT                                                                
010900 01  FILLER                       PIC X(16)  VALUE 'IN57-FILEN'.          
011000*01  AREA  -COPY W33057  -PRE I57-.                                       
011200    EJECT                                                                 
011210 01  FILLER                       PIC X(16)  VALUE 'IN58-FILEN'.          
011220*01  AREA  -COPY W33057  -PRE I58-.                                       
011230    EJECT                                                                 
011300 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
011400*                                                                         
011500*     FIL W33059                                                          
011600*01  AREA  -COPY W33059  -PRE U059-.                                      
011800     EJECT                                                                
011900*01  AREA  -COPY W3303104  -PRE U459-.                                    
012100    EJECT                                                                 
012200 PROCEDURE DIVISION.                                                      
012300    SKIP2                                                                 
012400 STYR SECTION.                                                            
012500     PERFORM A-INIT                                                       
012600     SORT SORTFIL                                                         
012700        ASCENDING KEY IDUSER                                              
012800                      DAREGDAT                                            
012900                      TIREGTID                                            
013000        INPUT PROCEDURE  B-PLOCKA-S2-URVAL                                
013100        OUTPUT PROCEDURE C-MATCHA-OCH-SKRIV                               
013200     IF SORT-RETURN = ZERO                                                
013300        PERFORM Z-FINIT                                                   
013400        MOVE ZERO TO RETURN-CODE                                          
013500        GOBACK                                                            
013600     ELSE                                                                 
013700        DISPLAY 'FEL I SORTERINGEN'                                       
013800        CALL ABEND USING FELKOD                                           
013900     END-IF                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400     OPEN INPUT  W33031                                                   
014500                 W33057S                                                  
014510                 W33058S                                                  
014600     OPEN OUTPUT W33059                                                   
014700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014800     INITIALIZE SORT-AREA                                                 
014900                I57-AREA                                                  
015000                U059-AREA                                                 
015100     .                                                                    
015200     EJECT                                                                
015300 B-PLOCKA-S2-URVAL   SECTION.                                             
015400     SKIP2                                                                
015500     PERFORM S01-LAS-31-FIL                                               
015600     PERFORM UNTIL IN31-EOF = JA                                          
015700        IF I31-IDPTYP = 'S2 ' OR 'S2A'                                    
015800           RELEASE SORT-POST FROM I31-AREA                                
015900        END-IF                                                            
016000        PERFORM S01-LAS-31-FIL                                            
016100     END-PERFORM                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 C-MATCHA-OCH-SKRIV  SECTION.                                             
016500     SKIP2                                                                
016600     PERFORM S02-RETURN                                                   
016700     PERFORM S03-LAS-57-FIL                                               
016710     PERFORM S08-LAS-58-FIL                                               
016900     PERFORM UNTIL SORT-EOF = JA                                          
016910        IF SORT-IDPTYP = 'S2 '                                            
017000          IF IN57-EOF = NEJ                                               
017100             MOVE +1                          TO IX                       
017200             MOVE +1                          TO WS-IDRADNR               
017300             IF  SORT-IDUSER   = I57-IDUSER                               
017400             AND SORT-DAREGDAT = I57-DAREGDAT                             
017500             AND SORT-TIREGTID = I57-TIREGTID                             
017600                PERFORM S05-FLYTTA-OCH-BERAKN-59-FIL                      
017700                PERFORM S04-SKRIV-59-FIL-RESULTAT                         
017800                PERFORM S03-LAS-57-FIL                                    
017900                ADD  +1                       TO IX                       
018000                PERFORM UNTIL IN57-EOF = JA                               
018100                OR  SORT-IDUSER   NOT = I57-IDUSER                        
018200                OR  SORT-DAREGDAT NOT = I57-DAREGDAT                      
018300                OR  SORT-TIREGTID NOT = I57-TIREGTID                      
018600                   IF  IX > SORT-KVART                                    
018700                      CONTINUE                                            
018800                   ELSE                                                   
019000                      ADD +1            TO WS-IDRADNR                     
019400                      PERFORM S05-FLYTTA-OCH-BERAKN-59-FIL                
019500                      PERFORM S04-SKRIV-59-FIL-RESULTAT                   
019600                   END-IF                                                 
019700                   PERFORM S03-LAS-57-FIL                                 
019800                   ADD +1                     TO IX                       
019900                END-PERFORM                                               
020000             ELSE                                                         
020100                MOVE ZERO                     TO SORT-IDGTYP              
020200             END-IF                                                       
020300          ELSE                                                            
020400            MOVE ZERO                         TO SORT-IDGTYP              
020500          END-IF                                                          
020510        ELSE                                                              
020511          IF IN58-EOF = NEJ                                               
020512             MOVE +1                          TO IX                       
020513             MOVE +1                          TO WS-IDRADNR               
020514             IF  SORT-IDUSER   = I58-IDUSER                               
020515             AND SORT-DAREGDAT = I58-DAREGDAT                             
020516             AND SORT-TIREGTID = I58-TIREGTID                             
020518                                                                          
020519                PERFORM S07-FLYTTA-OCH-BERAKN-59-FIL                      
020520                PERFORM S04-SKRIV-59-FIL-RESULTAT                         
020521                PERFORM S08-LAS-58-FIL                                    
020522                ADD  +1                       TO IX                       
020523                PERFORM UNTIL IN58-EOF = JA                               
020524                OR  SORT-IDUSER   NOT = I58-IDUSER                        
020525                OR  SORT-DAREGDAT NOT = I58-DAREGDAT                      
020526                OR  SORT-TIREGTID NOT = I58-TIREGTID                      
020528                   IF  IX > SORT-KVART                                    
020529                      CONTINUE                                            
020530                   ELSE                                                   
020532                      ADD +1           TO WS-IDRADNR                      
020540                      PERFORM S07-FLYTTA-OCH-BERAKN-59-FIL                
020541                      PERFORM S04-SKRIV-59-FIL-RESULTAT                   
020542                   END-IF                                                 
020543                   PERFORM S08-LAS-58-FIL                                 
020544                   ADD +1                 TO IX                           
020545                END-PERFORM                                               
020546             ELSE                                                         
020547                MOVE ZERO               TO SORT-IDGTYP                    
020548             END-IF                                                       
020549          ELSE                                                            
020550            MOVE ZERO                 TO SORT-IDGTYP                      
020551          END-IF                                                          
020560        END-IF                                                            
020600        PERFORM S06-SKRIV-59-FIL-URVAL                                    
020700        PERFORM S02-RETURN                                                
020800     END-PERFORM                                                          
020900     .                                                                    
021000     EJECT                                                                
021100 S01-LAS-31-FIL   SECTION.                                                
021200     SKIP2                                                                
021300     READ W33031  INTO I31-AREA                                           
021400     AT END                                                               
021500       MOVE JA                    TO IN31-EOF                             
021600     END-READ                                                             
021700                                                                          
021800     IF IN31-EOF = NEJ                                                    
021900       MOVE '    '                TO POSTSUM-TRANSTYP                     
022000       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
022100       MOVE 'W33058D1'            TO POSTSUM-DDNAMN2                      
022200       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 S02-RETURN       SECTION.                                                
022700     SKIP2                                                                
022800     RETURN SORTFIL INTO SORT-AREA                                        
022900     AT END                                                               
023000       MOVE JA                    TO SORT-EOF                             
023100     END-RETURN                                                           
023200                                                                          
023300     IF SORT-EOF = NEJ                                                    
023400       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
023500       MOVE '      '              TO POSTSUM-FDNAMN                       
023600       MOVE '        '            TO POSTSUM-DDNAMN2                      
023700       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 S03-LAS-57-FIL    SECTION.                                               
024200     SKIP2                                                                
024300     READ W33057S INTO I57-AREA                                           
024400     AT END                                                               
024500       MOVE JA                    TO IN57-EOF                             
024600       MOVE +99999                TO I57-DAREGDAT                         
024700                                     I57-TIREGTID                         
024800     END-READ                                                             
024900                                                                          
025000     IF IN57-EOF = NEJ                                                    
025100       MOVE '    '                TO POSTSUM-TRANSTYP                     
025200       MOVE 'W33057'              TO POSTSUM-FDNAMN                       
025300       MOVE 'W33058D2'            TO POSTSUM-DDNAMN2                      
025400       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025800 S04-SKRIV-59-FIL-RESULTAT  SECTION.                                      
025900     SKIP2                                                                
026000     WRITE U059-POST FROM U059-AREA                                       
026100     MOVE 'UT  '                 TO POSTSUM-TRANSTYP                      
026200     MOVE 'W33059'               TO POSTSUM-FDNAMN                        
026300     MOVE 'W33058D4'             TO POSTSUM-DDNAMN2                       
026400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
026500     .                                                                    
026600     EJECT                                                                
026700 S05-FLYTTA-OCH-BERAKN-59-FIL   SECTION.                                  
026800     SKIP2                                                                
026900     MOVE WS-IDRADNR             TO U059-IDRADNR                          
027000     MOVE I57-001-GRUPP          TO U059-001-GRUPP                        
027100     MOVE I57-KDPRODSL           TO U059-KDPRODSL                         
027200     MOVE I57-IDARTNR            TO U059-IDARTNR                          
027300     MOVE I57-BEART-SVE          TO U059-BEART-SVE                        
027400     MOVE I57-IDFKNGRP           TO U059-IDFKNGRP                         
027500     MOVE I57-SUARTFSG-RAAR      TO U059-SUARTFSG-RAAR                    
027600     MOVE I57-SULEVANT-RAAR      TO U059-SULEVANT-RAAR                    
027700     MOVE I57-SULEVANT-FRAAR     TO U059-SULEVANT-FRAAR                   
027800     MOVE I57-SUARTSJK-RAAR      TO U059-SUARTSJK-RAAR                    
027900                                                                          
029429*--OM BÅDE SULEVANT-FRAAR OCH -RAAR ÄR LIKA MED 0 SÅ BLIR DET             
029430*--FEL, DET BLIR ALLTSÅ RELEVANT = +999.9                                 
029431     IF I57-SULEVANT-FRAAR = ZERO                                         
029432         MOVE +999.9               TO U059-RELEVANT-RAAR                  
029433     ELSE                                                                 
029434       IF I57-SULEVANT-RAAR = ZERO                                        
029435         MOVE -999.9               TO U059-RELEVANT-RAAR                  
029436       ELSE                                                               
029437         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
029438         ((I57-SULEVANT-RAAR - I57-SULEVANT-FRAAR) * 100) /               
029439         I57-SULEVANT-FRAAR                                               
029440         IF WS-RELEVANT-RAAR > +999.9                                     
029441           MOVE +999.9 TO U059-RELEVANT-RAAR                              
029442         ELSE                                                             
029443           IF WS-RELEVANT-RAAR < -999.9                                   
029444             MOVE -999.9 TO U059-RELEVANT-RAAR                            
029445           ELSE                                                           
029446             MOVE WS-RELEVANT-RAAR TO U059-RELEVANT-RAAR                  
029447           END-IF                                                         
029448         END-IF                                                           
029449       END-IF                                                             
029450     END-IF                                                               
029500     IF I57-SUARTFSG-RAAR = ZERO                                          
029600         MOVE +999999999.99               TO U059-RETOTBV-RAAR            
029700     ELSE                                                                 
029800         COMPUTE U059-RETOTBV-RAAR ROUNDED  =                             
029900         ((I57-SUARTFSG-RAAR - I57-SUARTSJK-RAAR) * 100) /                
030000         I57-SUARTFSG-RAAR                                                
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 S06-SKRIV-59-FIL-URVAL   SECTION.                                        
030500     SKIP2                                                                
030600     WRITE U459-POST FROM SORT-AREA                                       
030700     MOVE 'UT  '                 TO POSTSUM-TRANSTYP                      
030800     MOVE 'W33059'               TO POSTSUM-FDNAMN                        
030900     MOVE 'W33058D4'             TO POSTSUM-DDNAMN2                       
031000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
031100     .                                                                    
031200     EJECT                                                                
031201 S07-FLYTTA-OCH-BERAKN-59-FIL   SECTION.                                  
031202     SKIP2                                                                
031203     MOVE WS-IDRADNR             TO U059-IDRADNR                          
031204     MOVE I58-001-GRUPP          TO U059-001-GRUPP                        
031205     MOVE I58-KDPRODSL           TO U059-KDPRODSL                         
031207     MOVE I58-IDARTNR            TO U059-IDARTNR                          
031208     MOVE I58-BEART-SVE          TO U059-BEART-SVE                        
031209     MOVE I58-IDFKNGRP           TO U059-IDFKNGRP                         
031210     MOVE I58-SUARTFSG-RAAR      TO U059-SUARTFSG-RAAR                    
031211     MOVE I58-SULEVANT-RAAR      TO U059-SULEVANT-RAAR                    
031212     MOVE I58-SULEVANT-FRAAR     TO U059-SULEVANT-FRAAR                   
031213     MOVE I58-SUARTSJK-RAAR      TO U059-SUARTSJK-RAAR                    
031214                                                                          
031230*--OM BÅDE SULEVANT-FRAAR OCH -RAAR ÄR LIKA MED 0 SÅ BLIR DET             
031231*--FEL, DET BLIR ALLTSÅ RELEVANT = +999.9                                 
031232     IF I58-SULEVANT-FRAAR = ZERO                                         
031233         MOVE +999.9               TO U059-RELEVANT-RAAR                  
031234     ELSE                                                                 
031235       IF I58-SULEVANT-RAAR = ZERO                                        
031236         MOVE -999.9               TO U059-RELEVANT-RAAR                  
031237       ELSE                                                               
031238         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
031239         ((I58-SULEVANT-RAAR - I58-SULEVANT-FRAAR) * 100) /               
031240         I58-SULEVANT-FRAAR                                               
031241         IF WS-RELEVANT-RAAR > +999.9                                     
031242           MOVE +999.9 TO U059-RELEVANT-RAAR                              
031243         ELSE                                                             
031244           IF WS-RELEVANT-RAAR < -999.9                                   
031245             MOVE -999.9 TO U059-RELEVANT-RAAR                            
031246           ELSE                                                           
031247             MOVE WS-RELEVANT-RAAR TO U059-RELEVANT-RAAR                  
031248           END-IF                                                         
031249         END-IF                                                           
031250       END-IF                                                             
031251     END-IF                                                               
031252     IF I58-SUARTFSG-RAAR = ZERO                                          
031253         MOVE +999999999.99               TO U059-RETOTBV-RAAR            
031254     ELSE                                                                 
031255         COMPUTE U059-RETOTBV-RAAR ROUNDED  =                             
031256         ((I58-SUARTFSG-RAAR - I58-SUARTSJK-RAAR) * 100) /                
031257         I58-SUARTFSG-RAAR                                                
031258     END-IF                                                               
031259     .                                                                    
031260     EJECT                                                                
031261 S08-LAS-58-FIL    SECTION.                                               
031262     SKIP2                                                                
031263     READ W33058S INTO I58-AREA                                           
031264     AT END                                                               
031265       MOVE JA                    TO IN58-EOF                             
031266       MOVE +99999                TO I58-DAREGDAT                         
031270                                     I58-TIREGTID                         
031280     END-READ                                                             
031290                                                                          
031291     IF IN58-EOF = NEJ                                                    
031292       MOVE 'S2A '                TO POSTSUM-TRANSTYP                     
031293       MOVE 'W33058'              TO POSTSUM-FDNAMN                       
031294       MOVE 'W33058D3'            TO POSTSUM-DDNAMN2                      
031295       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
031296     END-IF                                                               
031297     .                                                                    
031298     EJECT                                                                
031300 Z-FINIT SECTION.                                                         
031400     SKIP2                                                                
031500     CLOSE W33031                                                         
031600           W33057S                                                        
031610           W33058S                                                        
031700           W33059                                                         
031800     MOVE 'S' TO POSTSUM-OPKOD                                            
031900     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
032000     .                                                                    
