000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300     SKIP2                                                                
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.         W2225000.                                            
000600*              PROGRAM CONVERTED BY                                       
000700*              COBOL CONVERSION AID PO 5785-ABJ                           
000800*              CONVERSION DATE 05/25/91 19:37:27.                         
000900*AUTHOR.             JANNE MELANDER.                                      
001000*DATE-WRITTEN.       OKTOBER 1986.                                        
001100*    SKIP2                                                                
001200*REMARKS.                                                                 
001300*    FUNKTION.                                                            
001400*            PROGRAMMET                                                   
001500*            KOMPLETTERAR PERIODBEHOVSPOSTER MED                          
001600*            FÖRPACKNINGSTYPER.                                           
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*                            ***  INPUT:  PERIODBEHOVSPOSTER              
002400     SELECT W22149 ASSIGN UT-S-W22250D1.                                  
002500*                                                                         
002600*                            ***  OUTPUT: PERIODBEHOVSPOSTER              
002700*                                         KOMPLETTERADE MED               
002800*                                         FÖRPACKNINGSTYPER               
002900     SELECT W22253 ASSIGN UT-S-W22250D2.                                  
003000*                                                                         
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400     SKIP2                                                                
003500 FD  W22149                                                               
003600     RECORDING F                                                          
003700     BLOCK 0                                                              
003800                          .                                               
003900*01  -COPY W22149     -L.                                                 
004000     EJECT                                                                
004100 FD  W22253                                                               
004200     RECORDING F                                                          
004300     BLOCK 0                                                              
004400                          .                                               
004500*01  W22253-POST   -COPY W22253 -L.                                       
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004710                                                                          
004800*    -- CHECKED BY WY2000                                                 
005200*                                                                         
005300     SKIP2                                                                
005400                                                                          
005500 77  IX                          PIC S9(9)  VALUE +0  COMP SYNC.          
005600 77  JA                          PIC X      VALUE 'J'.                    
005700 77  NEJ                         PIC X      VALUE 'N'.                    
005800 77  CL                          PIC S9(1)  VALUE +0  COMP SYNC.          
005900 77  W22149-EOF                  PIC X      VALUE 'N'.                    
006000                                                                          
006100 01  ARBETSFALT.                                                          
006200     03  WS-KVPB OCCURS 2.                                                
006300       05  WS-KVPB-TOTAL-NY      PIC S9(6)V9 VALUE ZERO COMP-3.           
006400       05  WS-KVPB-TOTAL-GAMMAL  PIC S9(6)V9 VALUE ZERO COMP-3.           
006500                                                                          
006600     03  WS-PROCTAL              PIC S9(4)V999 VALUE ZERO COMP-3.         
006700                                                                          
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007000     03  W2225010                PIC X(8)    VALUE 'W2225010'.            
007100     SKIP2                                                                
007200*                                *** PARAMETRAR TILL DATKORT   ***        
007300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22250'.              
007400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007500*01  -COPY WDATKORTC0.                                                    
007700     EJECT                                                                
007800*01  -COPY W0005 -PRE POSTSUM-                                            
008000     EJECT                                                                
008100*****************************************************************         
008200*    HÄR BÖRJAR INPUT-AREAN FÖR FIL W22215                      *         
008300*****************************************************************         
008400                                                                          
008500                                                                          
008600 01  FILLER              PIC X(15)  VALUE 'INPOST'.                       
008700                                                                          
008800 01  INPOST-WS.                                                           
008900*03  IN-AREA          -COPY W22149 -PRE IN-                               
009000     EJECT                                                                
009100 01  FILLER              PIC X(54)  VALUE 'UTPOST'.                       
009200                                                                          
009300*01  AREA1            -COPY W22253 -PRE UT-                               
009400     EJECT                                                                
009500**** - - - - - - - PARAMETER-AREOR TILL IMS-SUBPGM.                       
009600*                                                                         
009700 01  FILLER              PIC X(16)  VALUE                                 
009800                                    '0-AREA-START'.                       
009900                                                                          
010000*01  0-AREA   -COPY W222L500 -PRE CALL                                    
010100*                                                                         
010200 EJECT                                                                    
010300 01  FILLER              PIC X(16)  VALUE                                 
010400                                    '1-AREA-START'.                       
010500                                                                          
010600*01  1-AREA    -COPY W222L501 -PRE CALL                                   
010700*                                                                         
010800 EJECT                                                                    
010900 01  FILLER              PIC X(16)  VALUE                                 
011000                                    '2-AREA-START'.                       
011100* 01  2-AREA    -COPY W222L502 -PRE CALL                                  
011200*                                                                         
011300 EJECT                                                                    
011400 LINKAGE SECTION.                                                         
011800*01  -COPY W0008 -PRE ARTC-                                               
011900     05  FILLER                  PIC X(32).                               
012000     EJECT                                                                
012100 PROCEDURE DIVISION USING  ARTC-PCB.                                      
012200     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
012300                                                                          
012400     PERFORM A-INITIERING                                                 
012500                                                                          
012600     PERFORM S01-LAS-W22149                                               
012700     PERFORM UNTIL W22149-EOF = JA                                        
012900       PERFORM B-LAES-ROT-WDK601                                          
013000       IF CALL0-KDSVAR = CALL0-KDSVAR-OK                                  
013600                                                                          
014600         COMPUTE WS-KVPB-TOTAL-NY(1) =                                    
014700                  IN-KVPB-VESL-NY(1) + IN-KVPB-TPO-NY(1)                  
014800         COMPUTE WS-KVPB-TOTAL-GAMMAL(1) =                                
014900                  IN-KVPB-VESL-GAMMAL(1) + IN-KVPB-TPO-GAMMAL(1)          
015000                                                                          
015100         IF WS-KVPB-TOTAL-NY(1) = ZERO                                    
015200            MOVE +1 TO WS-KVPB-TOTAL-NY(1)                                
015300***         VI VILL UNDVIKA DIVISION MED NOLL                             
015400         END-IF                                                           
015500                                                                          
015600         IF WS-KVPB-TOTAL-GAMMAL(1) > 300 OR                              
015700          WS-KVPB-TOTAL-NY(1) > 300                                       
015800            COMPUTE WS-PROCTAL =                                          
015900               WS-KVPB-TOTAL-GAMMAL(1) / WS-KVPB-TOTAL-NY(1)              
016000                                                                          
016100           IF WS-PROCTAL > 0.8 AND < 1.2                                  
016200             CONTINUE                                                     
016300           ELSE                                                           
016400             PERFORM C-LAS-WDD660                                         
016500                                                                          
016600             IF CALL0-KDSVAR = CALL0-KDSVAR-OK                            
016700                                                                          
016800               IF CALL1-BEFT > 0                                          
016900                 PERFORM D-TEST-IDARTNR-EMBQ                              
017000                 MOVE IN-IDARTNR             TO UT-IDARTNR                
017100                 MOVE +1                     TO UT-KDCLAGER               
017200                 MOVE IN-KVPB-VESL-NY(1)     TO UT-KVPB-VESL-NY           
017300                 MOVE IN-KVPB-VESL-GAMMAL(1)TO                            
017400                                   UT-KVPB-VESL-GAMMAL                    
017500                 MOVE IN-KVPB-TPO-NY(1)     TO UT-KVPB-TPO-NY             
017600                 MOVE IN-KVPB-TPO-GAMMAL(1) TO                            
017700                                   UT-KVPB-TPO-GAMMAL                     
017800                 MOVE IN-KDGK                TO UT-KDGK                   
017900                 PERFORM S02-SKRIV-UTPOST-W22253                          
018000               END-IF                                                     
018100             END-IF                                                       
018200           END-IF                                                         
018300         END-IF                                                           
018400       END-IF                                                             
018500       PERFORM S01-LAS-W22149                                             
018600     END-PERFORM                                                          
018700     PERFORM G-AVSLUTA                                                    
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INITIERING SECTION.                                                    
019300************************************                                      
019400*    ÖPPNA SAMTLIGA FILER         *                                       
019500*    HÄMTA INFO FRÅN DATUMKORT    *                                       
019600************************************                                      
019700     SKIP2                                                                
019800     OPEN INPUT W22149                                                    
019900     OPEN OUTPUT W22253                                                   
020000                                                                          
020100     MOVE 'W22149' TO POSTSUM-PROGNAMN                                    
020200     .                                                                    
020300                                                                          
020400     EJECT                                                                
020500 B-LAES-ROT-WDK601 SECTION.                                               
020600**********************************************                            
020700*    LÄS WDK601                              *                            
020800*                                            *                            
020900**********************************************                            
021000                                                                          
021100     MOVE IN-IDARTNR       TO CALL0-IDARTNR                               
021200     MOVE CALL0-LAES-ROT-WDD601 TO CALL0-KDCALL                           
021300     CALL W2225010 USING CALL0-AREA                                       
021400                         CALL1-AREA                                       
021600                         ARTC-PCB                                         
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 C-LAS-WDD660      SECTION.                                               
022100**********************************************                            
022200*    LÄS WLARTC11                  BEFT      *                            
022300*                         IDARTNR-EMBQ0      *                            
022400*                                -EMBQ1      *                            
022500*                                -EMBQ2      *                            
022600**********************************************                            
022900                                                                          
023000     MOVE CALL0-LAES-WDD660 TO CALL0-KDCALL                               
023100     CALL W2225010 USING CALL0-AREA                                       
023200                         CALL1-AREA                                       
023400                         ARTC-PCB                                         
023500     .                                                                    
023600     EJECT                                                                
023700 D-TEST-IDARTNR-EMBQ  SECTION.                                            
023800*************************************************                         
023900* OM CALL1-IDARTNR-EMBQ0,1,2 = 0                *                         
024000*    -FLYTTA ZERO TILL UT-KVPB-SEP-C1-EMBQ0,1,2 *                         
024100*                      UT-KVPB-SEP-C2-EMBQ0,1,2 *                         
024200* ANNARS                                        *                         
024300*        FLYTTA RESP. KVPB-SEP FÖR BÅDE C1,C2   *                         
024400*************************************************                         
024500                                                                          
024600     MOVE ZERO               TO IX                                        
024700                                                                          
024800     IF CALL1-IDARTNR-EMBQ0 = 0                                           
024900       ADD +1               TO IX                                         
025000       MOVE ZERO            TO UT-KVPB-SEP-C1-EMBQ0                       
025100                               UT-KVPB-SEP-C2-EMBQ0                       
025200       MOVE CALL1-IDARTNR-EMBQ0 TO UT-IDARTNR-EMBQ0                       
025300     ELSE                                                                 
025400       MOVE CALL1-IDARTNR-EMBQ0 TO CALL0-IDARTNR                          
025500       MOVE CALL1-IDARTNR-EMBQ0 TO UT-IDARTNR-EMBQ0                       
025600       ADD +1 TO IX                                                       
025700       PERFORM DA-LAES-ROT-WDK601                                         
025800     END-IF                                                               
025900     IF CALL1-IDARTNR-EMBQ1 = 0                                           
026000       ADD +1               TO IX                                         
026100       MOVE ZERO            TO UT-KVPB-SEP-C1-EMBQ1                       
026200                               UT-KVPB-SEP-C2-EMBQ1                       
026300       MOVE CALL1-IDARTNR-EMBQ1 TO UT-IDARTNR-EMBQ1                       
026400     ELSE                                                                 
026500       MOVE CALL1-IDARTNR-EMBQ1 TO CALL0-IDARTNR                          
026600       MOVE CALL1-IDARTNR-EMBQ1 TO UT-IDARTNR-EMBQ1                       
026700       ADD +1 TO IX                                                       
026800       PERFORM DA-LAES-ROT-WDK601                                         
026900     END-IF                                                               
027000     IF CALL1-IDARTNR-EMBQ2 = 0                                           
027100       ADD +1               TO IX                                         
027200       MOVE ZERO            TO UT-KVPB-SEP-C1-EMBQ2                       
027300                               UT-KVPB-SEP-C2-EMBQ2                       
027400       MOVE CALL1-IDARTNR-EMBQ2 TO UT-IDARTNR-EMBQ2                       
027500     ELSE                                                                 
027600       MOVE CALL1-IDARTNR-EMBQ2 TO CALL0-IDARTNR                          
027700       MOVE CALL1-IDARTNR-EMBQ2 TO UT-IDARTNR-EMBQ2                       
027800       ADD +1 TO IX                                                       
027900       PERFORM DA-LAES-ROT-WDK601                                         
028000     END-IF                                                               
028100     .                                                                    
028200 DA-LAES-ROT-WDK601 SECTION.                                              
028300     MOVE CALL0-LAES-ROT-WDD601 TO CALL0-KDCALL                           
028400     CALL W2225010 USING CALL0-AREA                                       
028500                         CALL2-AREA                                       
028700                         ARTC-PCB                                         
028800     IF CALL0-KDSVAR = CALL0-KDSVAR-OK                                    
029000       MOVE CALL0-LAES-WDD631 TO CALL0-KDCALL                             
029100       CALL W2225010 USING CALL0-AREA                                     
029200                           CALL2-AREA                                     
029400                           ARTC-PCB                                       
029500       IF CALL0-KDSVAR = CALL0-KDSVAR-OK                                  
029600                                                                          
029700         IF IX = 1                                                        
029800           MOVE CALL2-KVPB-SEP   TO UT-KVPB-SEP-C1-EMBQ0                  
029900         END-IF                                                           
030000         IF IX = 2                                                        
030100           MOVE CALL2-KVPB-SEP   TO UT-KVPB-SEP-C1-EMBQ1                  
030200         END-IF                                                           
030300         IF IX = 3                                                        
030400           MOVE CALL2-KVPB-SEP   TO UT-KVPB-SEP-C1-EMBQ2                  
030500         END-IF                                                           
030600       ELSE                                                               
030700         IF IX = 1                                                        
030800           MOVE ZERO             TO UT-KVPB-SEP-C1-EMBQ0                  
030900         END-IF                                                           
031000         IF IX = 2                                                        
031100           MOVE ZERO             TO UT-KVPB-SEP-C1-EMBQ1                  
031200         END-IF                                                           
031300         IF IX = 3                                                        
031400           MOVE ZERO             TO UT-KVPB-SEP-C1-EMBQ2                  
031500         END-IF                                                           
031600       END-IF                                                             
032900       MOVE ZERO                 TO UT-KVPB-SEP-C2-EMBQ0                  
033000                                    UT-KVPB-SEP-C2-EMBQ1                  
033100                                    UT-KVPB-SEP-C2-EMBQ2                  
034500     ELSE                                                                 
034600       PERFORM DAA-TEST-ZERO-KVPB-SEP                                     
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 DAA-TEST-ZERO-KVPB-SEP  SECTION.                                         
035100     IF IX = 1                                                            
035200       MOVE ZERO        TO UT-KVPB-SEP-C1-EMBQ0                           
035300       MOVE ZERO        TO UT-KVPB-SEP-C2-EMBQ0                           
035400     ELSE                                                                 
035500       EVALUATE TRUE                                                      
035600       WHEN IX = 2                                                        
035700         MOVE ZERO        TO UT-KVPB-SEP-C1-EMBQ1                         
035800         MOVE ZERO        TO UT-KVPB-SEP-C2-EMBQ1                         
035900       WHEN IX = 3                                                        
036000         MOVE ZERO        TO UT-KVPB-SEP-C1-EMBQ2                         
036100         MOVE ZERO        TO UT-KVPB-SEP-C2-EMBQ2                         
036200       END-EVALUATE                                                       
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 S01-LAS-W22149 SECTION.                                                  
036700**********************************************                            
036800*    LÄS W22149 OCH ÖKA UPP POSTRÄKNAREN     *                            
036900**********************************************                            
037000     SKIP2                                                                
037100     READ W22149 INTO INPOST-WS                                           
037200     AT END MOVE JA TO W22149-EOF                                         
037300     END-READ                                                             
037400                                                                          
037500     IF W22149-EOF = NEJ                                                  
037600       MOVE 'W22149' TO POSTSUM-FDNAMN                                    
037700       MOVE 'W22250D1' TO POSTSUM-DDNAMN2                                 
037800       MOVE SPACE      TO POSTSUM-TRANSTYP                                
037900       CALL POSTSUM USING POSTSUM-PARM                                    
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 S02-SKRIV-UTPOST-W22253  SECTION.                                        
038400                                                                          
038500     WRITE W22253-POST FROM UT-AREA1                                      
038600     MOVE 'W22253'                 TO POSTSUM-FDNAMN                      
038700     MOVE 'W22250D2'               TO POSTSUM-DDNAMN2                     
038800     MOVE SPACE                    TO POSTSUM-TRANSTYP                    
038900     CALL POSTSUM USING POSTSUM-PARM                                      
039000     .                                                                    
039100     EJECT                                                                
039200 G-AVSLUTA SECTION.                                                       
039300     CLOSE W22149 W22253                                                  
039400     MOVE 'S' TO POSTSUM-OPKOD                                            
039500     CALL POSTSUM USING POSTSUM-PARM                                      
039600     .                                                                    
