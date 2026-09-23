000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3305400.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*    LÄSER FILEN W33053 SOM INNEHÅLLER RESULTAT (STORSÄLJARE)             
001000*    OCH FILEN W33054 MED SAMMA RESULTAT MEN ANNAN SORTERING              
001100*    MATCHAR MED W33031(URVAL), SORTERAR OCH SKRIVER FILEN W33055         
001200*    W33053 ÄR SORTERAD PÅ FSG-SUMMA.                                     
001300*    W33054 ÄR SORTERAD PÅ FSG-ANTAL.                                     
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*    --- INFILER:                                                         
002200     SELECT SORTFIL                      ASSIGN TO W33054DS.              
002300     SELECT W33031                       ASSIGN TO W33054D1.              
002400     SELECT W33053S                      ASSIGN TO W33054D2.              
002500     SELECT W33054S                      ASSIGN TO W33054D3.              
002600*    --- UTFILER:                                                         
002700     SELECT W33055                       ASSIGN TO W33054D4.              
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 SD  SORTFIL                                                              
003400     RECORDING      F                                                     
003500     SKIP2                                                                
003600 01  SORT-POST.                                                           
003700   03  IDUSER                       PIC X(8).                             
003800   03  DAREGDAT                     PIC 9(8).                             
003900   03  TIREGTID                     PIC S9(7) COMP-3.                     
004000   03  FILLER                       PIC X(146).                           
004100     EJECT                                                                
004200 FD  W33031                                                               
004300     LABEL RECORD   STANDARD                                              
004400     RECORDING      V                                                     
004500     BLOCK CONTAINS 0.                                                    
004600     SKIP2                                                                
004700*01  POST  -COPY W3303103  -L -PRE I331-.                                 
004800     SKIP2                                                                
004900*01  POST  -COPY W3303102  -L -PRE I231-.                                 
005000     SKIP2                                                                
005100*01  POST  -COPY W3303104  -L -PRE I431-.                                 
005200     EJECT                                                                
005300 FD  W33053S                                                              
005400     LABEL RECORD   STANDARD                                              
005500     RECORDING      F                                                     
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP2                                                                
005800*01  POST -COPY W33053  -L -PRE I53-.                                     
005900     SKIP2                                                                
006000 FD  W33054S                                                              
006100     LABEL RECORD   STANDARD                                              
006200     RECORDING      F                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*01  POST -COPY W33053  -L -PRE I54-.                                     
006600     EJECT                                                                
006700 FD  W33055                                                               
006800     LABEL RECORD   STANDARD                                              
006900     RECORDING      V                                                     
007000     BLOCK CONTAINS 0.                                                    
007100     SKIP2                                                                
007200*01  POST -COPY W3303104  -L -PRE U455-.                                  
007300     SKIP2                                                                
007400*01  POST -COPY W33055  -L -PRE U055-.                                    
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700     SKIP2                                                                
007701                                                                          
007710*    -- CHECKED BY WY2000                                                 
007800 77  PROGRAM-NAMN          PIC X(8)    VALUE 'W3305400'.                  
007900 77  JA                    PIC X(1)    VALUE 'J'.                         
008000 77  NEJ                   PIC X(1)    VALUE 'N'.                         
008100 77  IN53-EOF              PIC X(1)    VALUE 'N'.                         
008200 77  IN54-EOF              PIC X(1)    VALUE 'N'.                         
008300 77  IN31-EOF              PIC X(1)    VALUE 'N'.                         
008400 77  SORT-EOF              PIC X(1)    VALUE 'N'.                         
008500 77  IX                    PIC S9(9)   VALUE +1  COMP SYNC.               
008600 77  WS-ANTAL-URVAL        PIC S9(9)   VALUE +1  COMP SYNC.               
008700 77  WS-IDRADNR            PIC S9(9)   VALUE +1  COMP SYNC.               
008800 77  WS-RELEVANT-RAAR      PIC S9(4)V9(1) VALUE ZERO COMP-3.              
008900 77  FELKOD                PIC S9(9)   VALUE +16 COMP SYNC.               
009000                                                                          
009100     EJECT                                                                
009200                                                                          
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     SKIP2                                                                
009800*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
009900*                                                                         
010000 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
010100*01  -COPY W0005 -PRE  POSTSUM-                                           
010200     EJECT                                                                
010300 01  FILLER                       PIC X(16)  VALUE 'IN31-FILEN'.          
010400*                                                                         
010500*     FIL W33031                                                          
010600 01  I31-AREA.                                                            
010700   03  FILLER                         PIC X(28).                          
010800   03  I31-IDPTYP                     PIC X(3).                           
010900   03  I31-IDGTYP                     PIC S9(1) COMP-3.                   
011000   03  I31-IDTRANS                    PIC X(4).                           
011100   03  FILLER                         PIC X(2572).                        
011200*01  AREA  -COPY W3303104  -PRE SORT-.                                    
011300     EJECT                                                                
011400 01  FILLER                       PIC X(16)  VALUE 'IN53-FILEN'.          
011500*01  AREA  -COPY W33053  -PRE I53-.                                       
011600    EJECT                                                                 
011700 01  FILLER                       PIC X(16)  VALUE 'IN54-FILEN'.          
011800*01  AREA  -COPY W33053  -PRE I54-.                                       
011900    EJECT                                                                 
012000 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
012100*     FIL W33055                                                          
012200*01  AREA  -COPY W33055  -PRE U055-.                                      
012300     EJECT                                                                
012400*01  AREA  -COPY W3303104  -PRE U455-.                                    
012500    EJECT                                                                 
012600 PROCEDURE DIVISION.                                                      
012700    SKIP2                                                                 
012800 STYR SECTION.                                                            
012900     PERFORM A-INIT                                                       
013000     SORT SORTFIL                                                         
013100        ASCENDING KEY IDUSER                                              
013200                      DAREGDAT                                            
013300                      TIREGTID                                            
013400        INPUT PROCEDURE  B-PLOCKA-S1-URVAL                                
013500        OUTPUT PROCEDURE C-MATCHA-OCH-SKRIV                               
013600     IF SORT-RETURN = ZERO                                                
013700        PERFORM Z-FINIT                                                   
013800        MOVE ZERO TO RETURN-CODE                                          
013900        GOBACK                                                            
014000     ELSE                                                                 
014100        DISPLAY 'FEL I SORTERINGEN'                                       
014200        CALL ABEND USING FELKOD                                           
014300     END-IF                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700     SKIP2                                                                
014800     OPEN INPUT  W33031                                                   
014900                 W33053S                                                  
015000                 W33054S                                                  
015100     OPEN OUTPUT W33055                                                   
015200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015300     .                                                                    
015400     EJECT                                                                
015500 B-PLOCKA-S1-URVAL   SECTION.                                             
015600     SKIP2                                                                
015700     PERFORM S01-LAS-31-FIL                                               
015800     PERFORM UNTIL IN31-EOF = JA                                          
015900        IF I31-IDPTYP = 'S1 ' OR 'S1A'                                    
016000           RELEASE SORT-POST FROM I31-AREA                                
016100        END-IF                                                            
016200        PERFORM S01-LAS-31-FIL                                            
016300     END-PERFORM                                                          
016400     .                                                                    
016500     EJECT                                                                
016600 C-MATCHA-OCH-SKRIV  SECTION.                                             
016700     SKIP2                                                                
016800     PERFORM S02-RETURN                                                   
016900     PERFORM S03-LAS-53-FIL                                               
017000     PERFORM S08-LAS-54-FIL                                               
017100     PERFORM UNTIL SORT-EOF = JA                                          
017200       IF SORT-IDPTYP = 'S1 '                                             
017300         IF IN53-EOF = NEJ                                                
017400            MOVE +1                          TO WS-IDRADNR                
017500            MOVE +1                          TO IX                        
017600            IF  SORT-IDUSER   = I53-IDUSER                                
017700            AND SORT-DAREGDAT = I53-DAREGDAT                              
017800            AND SORT-TIREGTID = I53-TIREGTID                              
017900               PERFORM S05-FLYTTA-OCH-BERAKN-55-FIL                       
018000               PERFORM S04-SKRIV-55-FIL-RESULTAT                          
018100               PERFORM S03-LAS-53-FIL                                     
018200               ADD  +1                       TO IX                        
018300               PERFORM UNTIL IN53-EOF = JA                                
018400               OR  SORT-IDUSER   NOT = I53-IDUSER                         
018500               OR  SORT-DAREGDAT NOT = I53-DAREGDAT                       
018600               OR  SORT-TIREGTID NOT = I53-TIREGTID                       
018700                  IF  IX > SORT-KVART                                     
018800                     CONTINUE                                             
018900                  ELSE                                                    
019000                     ADD +1               TO WS-IDRADNR                   
019100                     PERFORM S05-FLYTTA-OCH-BERAKN-55-FIL                 
019200                     PERFORM S04-SKRIV-55-FIL-RESULTAT                    
019300                  END-IF                                                  
019400                  PERFORM S03-LAS-53-FIL                                  
019500                  ADD +1                     TO IX                        
019600               END-PERFORM                                                
019700            ELSE                                                          
019800               MOVE ZERO                     TO SORT-IDGTYP               
019900            END-IF                                                        
020000         ELSE                                                             
020100            MOVE ZERO                        TO SORT-IDGTYP               
020200         END-IF                                                           
020300       ELSE                                                               
020400         IF IN54-EOF = NEJ                                                
020500            MOVE +1                          TO WS-IDRADNR                
020600            MOVE +1                          TO IX                        
020700            IF  SORT-IDUSER   = I54-IDUSER                                
020800            AND SORT-DAREGDAT = I54-DAREGDAT                              
020900            AND SORT-TIREGTID = I54-TIREGTID                              
021000               PERFORM S07-FLYTTA-OCH-BERAKN-55-FIL                       
021100               PERFORM S04-SKRIV-55-FIL-RESULTAT                          
021200               PERFORM S08-LAS-54-FIL                                     
021300               ADD  +1                       TO IX                        
021400               PERFORM UNTIL IN54-EOF = JA                                
021500               OR  SORT-IDUSER   NOT = I54-IDUSER                         
021600               OR  SORT-DAREGDAT NOT = I54-DAREGDAT                       
021700               OR  SORT-TIREGTID NOT = I54-TIREGTID                       
021800                  IF  IX > SORT-KVART                                     
021900                     CONTINUE                                             
022000                  ELSE                                                    
022100                     ADD +1             TO WS-IDRADNR                     
022200                     PERFORM S07-FLYTTA-OCH-BERAKN-55-FIL                 
022300                     PERFORM S04-SKRIV-55-FIL-RESULTAT                    
022400                  END-IF                                                  
022500                  PERFORM S08-LAS-54-FIL                                  
022600                  ADD +1                     TO IX                        
022700               END-PERFORM                                                
022800            ELSE                                                          
022900               MOVE ZERO                     TO SORT-IDGTYP               
023000            END-IF                                                        
023100         ELSE                                                             
023200            MOVE ZERO                        TO SORT-IDGTYP               
023300         END-IF                                                           
023400       END-IF                                                             
023500       PERFORM S06-SKRIV-55-FIL-URVAL                                     
023600       PERFORM S02-RETURN                                                 
023700     END-PERFORM                                                          
023800     .                                                                    
023900     EJECT                                                                
024000 S01-LAS-31-FIL   SECTION.                                                
024100     SKIP2                                                                
024200     READ W33031  INTO I31-AREA                                           
024300     AT END                                                               
024400       MOVE JA                    TO IN31-EOF                             
024500     END-READ                                                             
024600                                                                          
024700     IF IN31-EOF = NEJ                                                    
024800       MOVE '    '                TO POSTSUM-TRANSTYP                     
024900       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
025000       MOVE 'W33054D1'            TO POSTSUM-DDNAMN2                      
025100       CALL POSTSUM USING POSTSUM-PARM                                    
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 S02-RETURN       SECTION.                                                
025600     SKIP2                                                                
025700     RETURN SORTFIL INTO SORT-AREA                                        
025800     AT END                                                               
025900       MOVE JA                    TO SORT-EOF                             
026000     END-RETURN                                                           
026100                                                                          
026200     IF SORT-EOF = NEJ                                                    
026300       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
026400       MOVE '      '              TO POSTSUM-FDNAMN                       
026500       MOVE '        '            TO POSTSUM-DDNAMN2                      
026600       CALL POSTSUM USING POSTSUM-PARM                                    
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 S03-LAS-53-FIL    SECTION.                                               
027100     SKIP2                                                                
027200     READ W33053S INTO I53-AREA                                           
027300     AT END                                                               
027400       MOVE JA                    TO IN53-EOF                             
027500       MOVE +99                   TO I53-DAREGDAT                         
027600                                     I53-TIREGTID                         
027700     END-READ                                                             
027800                                                                          
027900     IF IN53-EOF = NEJ                                                    
028000       MOVE '    '                TO POSTSUM-TRANSTYP                     
028100       MOVE 'W33053'              TO POSTSUM-FDNAMN                       
028200       MOVE 'W33054D2'            TO POSTSUM-DDNAMN2                      
028300       CALL POSTSUM USING POSTSUM-PARM                                    
028400     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 S04-SKRIV-55-FIL-RESULTAT  SECTION.                                      
028800     SKIP2                                                                
028900     WRITE U055-POST FROM U055-AREA                                       
029000     MOVE '    '                 TO POSTSUM-TRANSTYP                      
029100     MOVE 'W33055'               TO POSTSUM-FDNAMN                        
029200     MOVE 'W33054D4'             TO POSTSUM-DDNAMN2                       
029300     CALL POSTSUM USING POSTSUM-PARM                                      
029400     .                                                                    
029500     EJECT                                                                
029600 S05-FLYTTA-OCH-BERAKN-55-FIL   SECTION.                                  
029700     SKIP2                                                                
029800     MOVE WS-IDRADNR             TO U055-IDRADNR                          
029900     MOVE I53-001-GRUPP          TO U055-001-GRUPP                        
030000     MOVE I53-KDPRODSL           TO U055-KDPRODSL                         
030100     MOVE I53-IDARTNR            TO U055-IDARTNR                          
030200     MOVE I53-BEART-SVE          TO U055-BEART-SVE                        
030300     MOVE I53-IDFKNGRP           TO U055-IDFKNGRP                         
030400     MOVE I53-SUARTFSG-RAAR      TO U055-SUARTFSG-RAAR                    
030500     MOVE I53-SULEVANT-RAAR      TO U055-SULEVANT-RAAR                    
030600     MOVE I53-SULEVANT-FRAAR     TO U055-SULEVANT-FRAAR                   
030700     MOVE I53-SUARTSJK-RAAR      TO U055-SUARTSJK-RAAR                    
030701                                                                          
030710*--OM BÅDE SULEVANT-FRAAR OCH -RAAR ÄR LIKA MED 0 SÅ BLIR DET             
030720*--FEL, DET BLIR ALLTSÅ RELEVANT = +999.9                                 
030800     IF I53-SULEVANT-FRAAR = ZERO                                         
030900         MOVE +999.9               TO U055-RELEVANT-RAAR                  
031000     ELSE                                                                 
031010       IF I53-SULEVANT-RAAR = ZERO                                        
031020         MOVE -999.9               TO U055-RELEVANT-RAAR                  
031030       ELSE                                                               
031100         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
031200         ((I53-SULEVANT-RAAR - I53-SULEVANT-FRAAR) * 100) /               
031300         I53-SULEVANT-FRAAR                                               
031400         IF WS-RELEVANT-RAAR > +999.9                                     
031500           MOVE +999.9 TO U055-RELEVANT-RAAR                              
031600         ELSE                                                             
031700           IF WS-RELEVANT-RAAR < -999.9                                   
031800             MOVE -999.9 TO U055-RELEVANT-RAAR                            
031900           ELSE                                                           
032000             MOVE WS-RELEVANT-RAAR TO U055-RELEVANT-RAAR                  
032100           END-IF                                                         
032200         END-IF                                                           
032210       END-IF                                                             
032300     END-IF                                                               
032400     IF I53-SUARTFSG-RAAR = ZERO                                          
032500         MOVE +999999999.99               TO U055-RETOTBV-RAAR            
032600     ELSE                                                                 
032700         COMPUTE U055-RETOTBV-RAAR ROUNDED  =                             
032800         ((I53-SUARTFSG-RAAR - I53-SUARTSJK-RAAR) * 100) /                
032900         I53-SUARTFSG-RAAR                                                
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 S06-SKRIV-55-FIL-URVAL   SECTION.                                        
033400     SKIP2                                                                
033500     WRITE U455-POST FROM SORT-AREA                                       
033600     MOVE '    '                 TO POSTSUM-TRANSTYP                      
033700     MOVE 'W33055'               TO POSTSUM-FDNAMN                        
033800     MOVE 'W33054D4'             TO POSTSUM-DDNAMN2                       
033900     CALL POSTSUM USING POSTSUM-PARM                                      
034000     .                                                                    
034100     EJECT                                                                
034200 S07-FLYTTA-OCH-BERAKN-55-FIL   SECTION.                                  
034300     SKIP2                                                                
034400     MOVE WS-IDRADNR             TO U055-IDRADNR                          
034500     MOVE I54-001-GRUPP          TO U055-001-GRUPP                        
034600     MOVE I54-KDPRODSL           TO U055-KDPRODSL                         
034700     MOVE I54-IDARTNR            TO U055-IDARTNR                          
034800     MOVE I54-BEART-SVE          TO U055-BEART-SVE                        
034900     MOVE I54-IDFKNGRP           TO U055-IDFKNGRP                         
035000     MOVE I54-SUARTFSG-RAAR      TO U055-SUARTFSG-RAAR                    
035100     MOVE I54-SULEVANT-RAAR      TO U055-SULEVANT-RAAR                    
035200     MOVE I54-SULEVANT-FRAAR     TO U055-SULEVANT-FRAAR                   
035300     MOVE I54-SUARTSJK-RAAR      TO U055-SUARTSJK-RAAR                    
035301                                                                          
035400     IF I54-SULEVANT-FRAAR = ZERO                                         
035500         MOVE +999.9               TO U055-RELEVANT-RAAR                  
035600     ELSE                                                                 
035610       IF I54-SULEVANT-RAAR = ZERO                                        
035620         MOVE -999.9               TO U055-RELEVANT-RAAR                  
035630       ELSE                                                               
035700         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
035800         ((I54-SULEVANT-RAAR - I54-SULEVANT-FRAAR) * 100) /               
035900         I54-SULEVANT-FRAAR                                               
036000         IF WS-RELEVANT-RAAR > +999.9                                     
036100           MOVE +999.9 TO U055-RELEVANT-RAAR                              
036200         ELSE                                                             
036300           IF WS-RELEVANT-RAAR < -999.9                                   
036400             MOVE -999.9 TO U055-RELEVANT-RAAR                            
036500           ELSE                                                           
036600             MOVE WS-RELEVANT-RAAR TO U055-RELEVANT-RAAR                  
036700           END-IF                                                         
036800         END-IF                                                           
036810       END-IF                                                             
036900     END-IF                                                               
037000     IF I54-SUARTFSG-RAAR = ZERO                                          
037100         MOVE +999999999.99               TO U055-RETOTBV-RAAR            
037200     ELSE                                                                 
037300         COMPUTE U055-RETOTBV-RAAR ROUNDED  =                             
037400         ((I54-SUARTFSG-RAAR - I54-SUARTSJK-RAAR) * 100) /                
037500         I54-SUARTFSG-RAAR                                                
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 S08-LAS-54-FIL    SECTION.                                               
038000     SKIP2                                                                
038100     READ W33054S INTO I54-AREA                                           
038200     AT END                                                               
038300       MOVE JA                    TO IN54-EOF                             
038400       MOVE +99                   TO I54-DAREGDAT                         
038500                                     I54-TIREGTID                         
038600     END-READ                                                             
038700                                                                          
038800     IF IN54-EOF = NEJ                                                    
038900       MOVE 'S1A '                TO POSTSUM-TRANSTYP                     
039000       MOVE 'W33054'              TO POSTSUM-FDNAMN                       
039100       MOVE 'W33054D3'            TO POSTSUM-DDNAMN2                      
039200       CALL POSTSUM USING POSTSUM-PARM                                    
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 Z-FINIT SECTION.                                                         
039700     SKIP2                                                                
039800     CLOSE W33031                                                         
039900           W33053S                                                        
040000           W33054S                                                        
040100           W33055                                                         
040200     MOVE 'S' TO POSTSUM-OPKOD                                            
040300     CALL POSTSUM USING POSTSUM-PARM                                      
040400     .                                                                    
