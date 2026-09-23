000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3306200.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000810*    LÄSER FILEN W33061 SOM INNEHÅLLER RESULTAT (STORSÄLJARE)             
000820*    OCH FILEN W33062 MED SAMMA RESULTAT MEN ANNAN SORTERING              
000830*    MATCHAR MED W33031(URVAL), SORTERAR OCH SKRIVER FILEN W33063         
000840*    W33061 ÄR SORTERAD PÅ FSG-SUMMA.                                     
000850*    W33062 ÄR SORTERAD PÅ FSG-ANTAL.                                     
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*    --- INFILER:                                                         
001900     SELECT SORTFIL                      ASSIGN TO W33062DS.              
002000     SELECT W33031                       ASSIGN TO W33062D1.              
002100     SELECT W33061S                      ASSIGN TO W33062D2.              
002110     SELECT W33062S                      ASSIGN TO W33062D3.              
002200*    --- UTFILER:                                                         
002300     SELECT W33063                       ASSIGN TO W33062D4.              
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
003500   03  TIREGTID                     PIC S9(7) COMP-3.                     
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
005100 FD  W33061S                                                              
005200     LABEL RECORD   STANDARD                                              
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  POST -COPY W33061  -L -PRE I61-.                                     
005800     EJECT                                                                
005810 FD  W33062S                                                              
005820     LABEL RECORD   STANDARD                                              
005830     RECORDING      F                                                     
005840     BLOCK CONTAINS 0.                                                    
005850     SKIP2                                                                
005860*01  POST -COPY W33061  -L -PRE I62-.                                     
005870     EJECT                                                                
005900 FD  W33063                                                               
006000     LABEL RECORD   STANDARD                                              
006100     RECORDING      V                                                     
006200     BLOCK CONTAINS 0.                                                    
006300     SKIP2                                                                
006400*01  POST -COPY W3303104  -L -PRE U463-.                                  
006600     SKIP2                                                                
006700*01  POST -COPY W33063  -L -PRE U063-.                                    
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100     SKIP2                                                                
007101                                                                          
007110*    -- CHECKED BY WY2000                                                 
007200 77  PROGRAM-NAMN             PIC X(8)    VALUE 'W3306200'.               
007300 77  JA                       PIC X(1)    VALUE 'J'.                      
007400 77  NEJ                      PIC X(1)    VALUE 'N'.                      
007500 77  IN61-EOF                 PIC X(1)    VALUE 'N'.                      
007510 77  IN62-EOF                 PIC X(1)    VALUE 'N'.                      
007600 77  IN31-EOF                 PIC X(1)    VALUE 'N'.                      
007700 77  SORT-EOF                 PIC X(1)    VALUE 'N'.                      
007800 77  IX                       PIC S9(9)   VALUE +1  COMP SYNC.            
007900 77  WS-ANTAL-URVAL           PIC S9(9)   VALUE +1  COMP SYNC.            
008000 77  WS-IDRADNR               PIC S9(9)   VALUE +1  COMP SYNC.            
008100 77  WS-RELEVANT-RAAR         PIC S9(5)V9(1) VALUE ZERO.                  
008200 77  FELKOD                   PIC S9(9)   VALUE +16 COMP SYNC.            
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
010900 01  FILLER                       PIC X(16)  VALUE 'IN61-FILEN'.          
011000*01  AREA  -COPY W33061  -PRE I61-.                                       
011200    EJECT                                                                 
011210 01  FILLER                       PIC X(16)  VALUE 'IN62-FILEN'.          
011220*01  AREA  -COPY W33061  -PRE I62-.                                       
011230    EJECT                                                                 
011300 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
011400*                                                                         
011500*     FIL W33063                                                          
011600*01  AREA  -COPY W33063  -PRE U063-.                                      
011800     EJECT                                                                
011900*01  AREA  -COPY W3303104  -PRE U463-.                                    
012100    EJECT                                                                 
012200 PROCEDURE DIVISION.                                                      
012300    SKIP2                                                                 
012400 STYR SECTION.                                                            
012500     PERFORM A-INIT                                                       
012600     SORT SORTFIL                                                         
012700        ASCENDING KEY IDUSER                                              
012800                      DAREGDAT                                            
012900                      TIREGTID                                            
013000        INPUT PROCEDURE  B-PLOCKA-S3-URVAL                                
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
014500                 W33061S                                                  
014510                 W33062S                                                  
014600     OPEN OUTPUT W33063                                                   
014700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014800     .                                                                    
014900     EJECT                                                                
015000 B-PLOCKA-S3-URVAL   SECTION.                                             
015100     SKIP2                                                                
015200     PERFORM S01-LAS-31-FIL                                               
015300     PERFORM UNTIL IN31-EOF = JA                                          
015400        IF I31-IDPTYP = 'S3 ' OR 'S3A'                                    
015500           RELEASE SORT-POST FROM I31-AREA                                
015600        END-IF                                                            
015700        PERFORM S01-LAS-31-FIL                                            
015800     END-PERFORM                                                          
015900     .                                                                    
016000     EJECT                                                                
016100 C-MATCHA-OCH-SKRIV  SECTION.                                             
016200     SKIP2                                                                
016300     PERFORM S02-RETURN                                                   
016400     PERFORM S03-LAS-61-FIL                                               
016410     PERFORM S08-LAS-62-FIL                                               
016600     PERFORM UNTIL SORT-EOF = JA                                          
016700        IF SORT-IDPTYP = 'S3 '                                            
016710          IF IN61-EOF = NEJ                                               
016800             MOVE +1                          TO WS-IDRADNR               
016900             MOVE +1                          TO IX                       
017000             IF  SORT-IDUSER   = I61-IDUSER                               
017100             AND SORT-DAREGDAT = I61-DAREGDAT                             
017200             AND SORT-TIREGTID = I61-TIREGTID                             
017300                PERFORM S05-FLYTTA-OCH-BERAKN-63-FIL                      
017400                PERFORM S04-SKRIV-63-FIL-RESULTAT                         
017500                PERFORM S03-LAS-61-FIL                                    
017600                ADD  +1                          TO IX                    
017700                PERFORM UNTIL IN61-EOF = JA                               
017800                OR  SORT-IDUSER   NOT = I61-IDUSER                        
017900                OR  SORT-DAREGDAT NOT = I61-DAREGDAT                      
018000                OR  SORT-TIREGTID NOT = I61-TIREGTID                      
018100                   IF  IX > SORT-KVART                                    
018200                      CONTINUE                                            
018300                   ELSE                                                   
018500                      ADD +1               TO WS-IDRADNR                  
018900                      PERFORM S05-FLYTTA-OCH-BERAKN-63-FIL                
019000                      PERFORM S04-SKRIV-63-FIL-RESULTAT                   
019100                   END-IF                                                 
019200                   PERFORM S03-LAS-61-FIL                                 
019300                   ADD +1                     TO IX                       
019400                END-PERFORM                                               
019500             ELSE                                                         
019600                MOVE ZERO                      TO SORT-IDGTYP             
019700             END-IF                                                       
019800          ELSE                                                            
019900             MOVE ZERO                         TO SORT-IDGTYP             
020000          END-IF                                                          
020010        ELSE                                                              
020011          IF IN62-EOF = NEJ                                               
020012             MOVE +1                          TO WS-IDRADNR               
020013             MOVE +1                          TO IX                       
020014             IF  SORT-IDUSER   = I62-IDUSER                               
020015             AND SORT-DAREGDAT = I62-DAREGDAT                             
020016             AND SORT-TIREGTID = I62-TIREGTID                             
020017                PERFORM S07-FLYTTA-OCH-BERAKN-63-FIL                      
020018                PERFORM S04-SKRIV-63-FIL-RESULTAT                         
020019                PERFORM S08-LAS-62-FIL                                    
020020                ADD  +1                          TO IX                    
020021                PERFORM UNTIL IN62-EOF = JA                               
020022                OR  SORT-IDUSER   NOT = I62-IDUSER                        
020023                OR  SORT-DAREGDAT NOT = I62-DAREGDAT                      
020024                OR  SORT-TIREGTID NOT = I62-TIREGTID                      
020025                   IF  IX > SORT-KVART                                    
020026                      CONTINUE                                            
020027                   ELSE                                                   
020029                      ADD +1             TO WS-IDRADNR                    
020038                      PERFORM S07-FLYTTA-OCH-BERAKN-63-FIL                
020039                      PERFORM S04-SKRIV-63-FIL-RESULTAT                   
020040                   END-IF                                                 
020041                   PERFORM S08-LAS-62-FIL                                 
020042                   ADD +1                TO IX                            
020043                END-PERFORM                                               
020044             ELSE                                                         
020045                MOVE ZERO            TO SORT-IDGTYP                       
020046             END-IF                                                       
020047          ELSE                                                            
020048             MOVE ZERO            TO SORT-IDGTYP                          
020049          END-IF                                                          
020050        END-IF                                                            
020100        PERFORM S06-SKRIV-63-FIL-URVAL                                    
020200        PERFORM S02-RETURN                                                
020300     END-PERFORM                                                          
020400     .                                                                    
020500     EJECT                                                                
020600 S01-LAS-31-FIL   SECTION.                                                
020700     SKIP2                                                                
020800     READ W33031  INTO I31-AREA                                           
020900     AT END                                                               
021000       MOVE JA                    TO IN31-EOF                             
021100     END-READ                                                             
021200                                                                          
021300     IF IN31-EOF = NEJ                                                    
021400       MOVE 'URV '                TO POSTSUM-TRANSTYP                     
021500       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
021600       MOVE 'W33062D1'            TO POSTSUM-DDNAMN2                      
021700       CALL POSTSUM USING POSTSUM-PARM                                    
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 S02-RETURN       SECTION.                                                
022200     SKIP2                                                                
022300     RETURN SORTFIL INTO SORT-AREA                                        
022400     AT END                                                               
022500       MOVE JA                    TO SORT-EOF                             
022600     END-RETURN                                                           
022700                                                                          
022800     IF SORT-EOF = NEJ                                                    
022900       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
023000       MOVE '      '              TO POSTSUM-FDNAMN                       
023100       MOVE '        '            TO POSTSUM-DDNAMN2                      
023200       CALL POSTSUM USING POSTSUM-PARM                                    
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 S03-LAS-61-FIL    SECTION.                                               
023700     SKIP2                                                                
023800     READ W33061S INTO I61-AREA                                           
023900     AT END                                                               
024000       MOVE JA                    TO IN61-EOF                             
024100       MOVE +99                   TO I61-DAREGDAT                         
024200                                     I61-TIREGTID                         
024300     END-READ                                                             
024400                                                                          
024500     IF IN61-EOF = NEJ                                                    
024600       MOVE 'S3  '                TO POSTSUM-TRANSTYP                     
024700       MOVE 'W33061'              TO POSTSUM-FDNAMN                       
024800       MOVE 'W33062D2'            TO POSTSUM-DDNAMN2                      
024900       CALL POSTSUM USING POSTSUM-PARM                                    
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 S04-SKRIV-63-FIL-RESULTAT SECTION.                                       
025400     SKIP2                                                                
025500     WRITE U063-POST FROM U063-AREA                                       
025600     MOVE 'UT  '                 TO POSTSUM-TRANSTYP                      
025700     MOVE 'W33063'               TO POSTSUM-FDNAMN                        
025800     MOVE 'W33062D4'             TO POSTSUM-DDNAMN2                       
025900     CALL POSTSUM USING POSTSUM-PARM                                      
026000     .                                                                    
026100     EJECT                                                                
026200 S05-FLYTTA-OCH-BERAKN-63-FIL   SECTION.                                  
026300     SKIP2                                                                
026400     MOVE WS-IDRADNR             TO U063-IDRADNR                          
026500     MOVE I61-001-GRUPP          TO U063-001-GRUPP                        
026600     MOVE I61-KDPRODSL           TO U063-KDPRODSL                         
026700     MOVE I61-IDARTNR            TO U063-IDARTNR                          
026800     MOVE I61-BEART-SVE          TO U063-BEART-SVE                        
026900     MOVE I61-IDFKNGRP           TO U063-IDFKNGRP                         
027000     MOVE I61-SUARTFSG-RAAR      TO U063-SUARTFSG-RAAR                    
027100     MOVE I61-SULEVANT-RAAR      TO U063-SULEVANT-RAAR                    
027200     MOVE I61-SULEVANT-FRAAR     TO U063-SULEVANT-FRAAR                   
027300     MOVE I61-SUARTSJK-RAAR      TO U063-SUARTSJK-RAAR                    
028900                                                                          
028901*--OM BÅDE SULEVANT-FRAAR OCH -RAAR ÄR LIKA MED 0 SÅ BLIR DET             
028902*--FEL, DET BLIR ALLTSÅ RELEVANT = +999.9                                 
028903     IF I61-SULEVANT-FRAAR = ZERO                                         
028904         MOVE +999.9               TO U063-RELEVANT-RAAR                  
028905     ELSE                                                                 
028906       IF I61-SULEVANT-RAAR = ZERO                                        
028907         MOVE -999.9               TO U063-RELEVANT-RAAR                  
028908       ELSE                                                               
028909         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
028910         ((I61-SULEVANT-RAAR - I61-SULEVANT-FRAAR) * 100) /               
028911         I61-SULEVANT-FRAAR                                               
028912         IF WS-RELEVANT-RAAR > +999.9                                     
028913           MOVE +999.9 TO U063-RELEVANT-RAAR                              
028914         ELSE                                                             
028915           IF WS-RELEVANT-RAAR < -999.9                                   
028916             MOVE -999.9 TO U063-RELEVANT-RAAR                            
028917           ELSE                                                           
028918             MOVE WS-RELEVANT-RAAR TO U063-RELEVANT-RAAR                  
028919           END-IF                                                         
028920         END-IF                                                           
028921       END-IF                                                             
028930     END-IF                                                               
029000     IF I61-SUARTFSG-RAAR = ZERO                                          
029100         MOVE +999999999.99               TO U063-RETOTBV-RAAR            
029200     ELSE                                                                 
029300         COMPUTE U063-RETOTBV-RAAR ROUNDED  =                             
029400         ((I61-SUARTFSG-RAAR - I61-SUARTSJK-RAAR) * 100) /                
029500         I61-SUARTFSG-RAAR                                                
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 S06-SKRIV-63-FIL-URVAL    SECTION.                                       
030000     SKIP2                                                                
030100     WRITE U463-POST FROM SORT-AREA                                       
030200     MOVE 'UT  '                 TO POSTSUM-TRANSTYP                      
030300     MOVE 'W33063'               TO POSTSUM-FDNAMN                        
030400     MOVE 'W33062D4'             TO POSTSUM-DDNAMN2                       
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
030700     EJECT                                                                
030701 S07-FLYTTA-OCH-BERAKN-63-FIL   SECTION.                                  
030702     SKIP2                                                                
030703     MOVE WS-IDRADNR             TO U063-IDRADNR                          
030704     MOVE I62-001-GRUPP          TO U063-001-GRUPP                        
030705     MOVE I62-KDPRODSL           TO U063-KDPRODSL                         
030706     MOVE I62-IDARTNR            TO U063-IDARTNR                          
030707     MOVE I62-BEART-SVE          TO U063-BEART-SVE                        
030708     MOVE I62-IDFKNGRP           TO U063-IDFKNGRP                         
030709     MOVE I62-SUARTFSG-RAAR      TO U063-SUARTFSG-RAAR                    
030710     MOVE I62-SULEVANT-RAAR      TO U063-SULEVANT-RAAR                    
030711     MOVE I62-SULEVANT-FRAAR     TO U063-SULEVANT-FRAAR                   
030712     MOVE I62-SUARTSJK-RAAR      TO U063-SUARTSJK-RAAR                    
030713                                                                          
030729*--OM BÅDE SULEVANT-FRAAR OCH -RAAR ÄR LIKA MED 0 SÅ BLIR DET             
030730*--FEL, DET BLIR ALLTSÅ RELEVANT = +999.9                                 
030731     IF I62-SULEVANT-FRAAR = ZERO                                         
030732         MOVE +999.9               TO U063-RELEVANT-RAAR                  
030733     ELSE                                                                 
030734       IF I62-SULEVANT-RAAR = ZERO                                        
030735         MOVE -999.9               TO U063-RELEVANT-RAAR                  
030736       ELSE                                                               
030737         COMPUTE WS-RELEVANT-RAAR ROUNDED  =                              
030738         ((I62-SULEVANT-RAAR - I62-SULEVANT-FRAAR) * 100) /               
030739         I62-SULEVANT-FRAAR                                               
030740         IF WS-RELEVANT-RAAR > +999.9                                     
030741           MOVE +999.9 TO U063-RELEVANT-RAAR                              
030742         ELSE                                                             
030743           IF WS-RELEVANT-RAAR < -999.9                                   
030744             MOVE -999.9 TO U063-RELEVANT-RAAR                            
030745           ELSE                                                           
030746             MOVE WS-RELEVANT-RAAR TO U063-RELEVANT-RAAR                  
030747           END-IF                                                         
030748         END-IF                                                           
030749       END-IF                                                             
030750     END-IF                                                               
030751     IF I62-SUARTFSG-RAAR = ZERO                                          
030752         MOVE +999999999.99               TO U063-RETOTBV-RAAR            
030753     ELSE                                                                 
030754         COMPUTE U063-RETOTBV-RAAR ROUNDED  =                             
030755         ((I62-SUARTFSG-RAAR - I62-SUARTSJK-RAAR) * 100) /                
030756         I62-SUARTFSG-RAAR                                                
030757     END-IF                                                               
030758     .                                                                    
030759     EJECT                                                                
030760 S08-LAS-62-FIL    SECTION.                                               
030761     SKIP2                                                                
030762     READ W33062S INTO I62-AREA                                           
030763     AT END                                                               
030764       MOVE JA                    TO IN62-EOF                             
030765       MOVE +99                   TO I62-DAREGDAT                         
030770                                     I62-TIREGTID                         
030780     END-READ                                                             
030790                                                                          
030791     IF IN62-EOF = NEJ                                                    
030792       MOVE 'S2A '                TO POSTSUM-TRANSTYP                     
030793       MOVE 'W33062'              TO POSTSUM-FDNAMN                       
030794       MOVE 'W33062D3'            TO POSTSUM-DDNAMN2                      
030795       CALL POSTSUM USING POSTSUM-PARM                                    
030796     END-IF                                                               
030797     .                                                                    
030798     EJECT                                                                
030800 Z-FINIT SECTION.                                                         
030900     SKIP2                                                                
031000     CLOSE W33031                                                         
031100           W33061S                                                        
031200           W33063                                                         
031300     MOVE 'S' TO POSTSUM-OPKOD                                            
031400     CALL POSTSUM USING POSTSUM-PARM                                      
031500     .                                                                    
