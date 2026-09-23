000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3350200.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   97/01/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        JÄMFÖR STD OCH SJK PÅ DAGENS-LAGERB MOT                          
001100*        GÅRDAGENS.                                                       
001200*        ALLA PRISER SOM HAR ÄNDRATS MULTIPLICERAS MED 2                  
001300*        OCH LÄGGS PÅ UTFIL W33501                                        
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- LAGERBANDET                                                
002700     SELECT W01172                     ASSIGN TO W33502D1.                
002800     SKIP2                                                                
002900*          --- MATCHNINGSFIL                                              
003000     SELECT W33502-IN                  ASSIGN TO W33502D2.                
003100     SKIP2                                                                
003200*          --- FIL TILL W33501 PRISUPPDATERING                            
003300     SELECT W33501                     ASSIGN TO W33502D3.                
003400     SKIP2                                                                
003500*          --- REGISTRET UT                                               
003600     SELECT W33502-UT                  ASSIGN TO W33502D4.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W01172                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W011100      -L.                                               
004700     SKIP3                                                                
004800 FD  W33502-IN                                                            
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W33502      -L.                                                
005300     SKIP3                                                                
005400 FD  W33501                                                               
005500     RECORDING       V                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  POST -COPY W33505 -PRE  XY-  -L.                                     
005900     SKIP3                                                                
006000 FD  W33502-UT                                                            
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  POST -COPY W33502 -PRE  UTREG-  -L.                                  
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800                                                                          
006900*    -- CHECKED BY WY2000                                                 
007000 77  IDPGM                       PIC X(8)    VALUE 'W3350200'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  SKRIV-POST                  PIC X       VALUE 'N'.                   
007400 77  WS-IDPTYP                   PIC X       VALUE SPACE.                 
007500                                                                          
007600 77  W01172-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W01172                       VALUE 'J'.                   
007800                                                                          
007900 77  W33502-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W33502                       VALUE 'J'.                   
008100                                                                          
008200 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
008300                                                                          
008400     EJECT                                                                
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     SKIP2                                                                
009600*    --- PARAMETRAR TILL ABEND                                            
009700                                                                          
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000     SKIP2                                                                
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900 01  W011-AREA-START             PIC X(24)   VALUE                        
011000                                 'W011-AREA-START  '.                     
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W011100     -PRE W011-                                    
011400     EJECT                                                                
011500 01  INREG-AREA-START            PIC X(24)   VALUE                        
011600                                 'INREG-AREA-START  '.                    
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W33502     -PRE INREG-                                    
012000     EJECT                                                                
012100 01  XY-AREA-START               PIC X(24)   VALUE                        
012200                                 'XY-AREA-START  '.                       
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W33505     -PRE XY-                                       
012600     EJECT                                                                
012700 01  UTREG-AREA-START            PIC X(24)   VALUE                        
012800                                 'UTREG-AREA-START  '.                    
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W33502     -PRE UTREG-                                    
013200     EJECT                                                                
013300 PROCEDURE DIVISION.                                                      
013400 MAIN SECTION.                                                            
013500     SKIP2                                                                
013600                                                                          
013700     PERFORM A-INIT                                                       
013800     PERFORM S01-LAES-W01172                                              
013900     PERFORM S02-LAES-W33502                                              
014000     PERFORM UNTIL END-OF-W01172                                          
014100       IF W011-KDERS < +21                                                
014200         IF W011-IDARTNR = INREG-IDARTNR                                  
014300           PERFORM B-JMF-PRISER                                           
014400           PERFORM S01-LAES-W01172                                        
014500           PERFORM S02-LAES-W33502                                        
014600         ELSE                                                             
014700           IF W011-IDARTNR < INREG-IDARTNR                                
014800             PERFORM C-SKAPA-NY                                           
014900             PERFORM S01-LAES-W01172                                      
015000           ELSE                                                           
015100             IF W011-IDARTNR > INREG-IDARTNR                              
015200               PERFORM S02-LAES-W33502                                    
015300             END-IF                                                       
015400           END-IF                                                         
015500         END-IF                                                           
015600       ELSE                                                               
015700         PERFORM S01-LAES-W01172                                          
015800       END-IF                                                             
015900                                                                          
016000                                                                          
016100     END-PERFORM                                                          
016200                                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     OPEN INPUT  W01172                                                   
017300                 W33502-IN                                                
017400                                                                          
017500     OPEN OUTPUT W33501                                                   
017600                 W33502-UT                                                
017700     SKIP2                                                                
017800     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
017900     ACCEPT DAGENS-DATUM  FROM DATE                                       
018000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018100                                                                          
018200* INITIERAR VÄRDEN PÅ UTFIL DESSA SKALL VARA SAMMA PÅ ALLA REC            
018300                                                                          
018400     MOVE 'A'                  TO XY-IDVTYP                               
018500     MOVE 01                   TO XY-KDRABATT                             
018600     MOVE ZERO                 TO XY-KDARTKAM                             
018700     MOVE ZERO                 TO XY-KDARTRAB-ALT                         
018800     .                                                                    
018900     EJECT                                                                
019000 B-JMF-PRISER SECTION.                                                    
019100     SKIP2                                                                
019200     MOVE W011-IDARTNR         TO UTREG-IDARTNR                           
019300                                  XY-IDARTNR                              
019400     MOVE W011-PRARTSJK        TO UTREG-PRARTSJK                          
019500     MOVE W011-PRARTSTD        TO UTREG-PRARTSTD                          
019600     IF W011-PRARTSJK NOT = INREG-PRARTSJK                                
019700       PERFORM S03-PRARTSJK                                               
019800     END-IF                                                               
019900     IF W011-PRARTSTD NOT = INREG-PRARTSTD                                
020000       PERFORM S04-PRARTSTD                                               
020100     END-IF                                                               
020200     PERFORM S12-SKRIV-W33502                                             
020300     .                                                                    
020400     EJECT                                                                
020500 C-SKAPA-NY   SECTION.                                                    
020600     SKIP2                                                                
020700     MOVE W011-IDARTNR      TO UTREG-IDARTNR                              
020800                                  XY-IDARTNR                              
020900     MOVE W011-PRARTSJK     TO UTREG-PRARTSJK                             
021000     MOVE W011-PRARTSTD     TO UTREG-PRARTSTD                             
021100     MOVE NEJ TO SKRIV-POST                                               
021200     IF W011-PRARTSJK > +0                                                
021300       PERFORM S03-PRARTSJK                                               
021400       MOVE JA TO SKRIV-POST                                              
021500     END-IF                                                               
021600     IF W011-PRARTSTD > +0                                                
021700       PERFORM S04-PRARTSTD                                               
021800       MOVE JA TO SKRIV-POST                                              
021900     END-IF                                                               
022000     IF SKRIV-POST = JA                                                   
022100       PERFORM S12-SKRIV-W33502                                           
022200     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 Z-FINIT      SECTION.                                                    
022600     CLOSE W01172                                                         
022700           W33502-IN                                                      
022800           W33501                                                         
022900           W33502-UT                                                      
023000     SKIP2                                                                
023100     MOVE 'S' TO POSTSUM-OPKOD                                            
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023500 S01-LAES-W01172  SECTION.                                                
023600     READ W01172 INTO W011-AREA                                           
023700     AT END                                                               
023800        MOVE +999999999 TO W011-IDARTNR                                   
023900        SET END-OF-W01172 TO TRUE                                         
024000                                                                          
024100     NOT AT END                                                           
024200        MOVE 'W01172'   TO POSTSUM-FDNAMN                                 
024300        MOVE 'W33502D1' TO POSTSUM-DDNAMN2                                
024400        MOVE 'W011'     TO POSTSUM-TRANSTYP                               
024500        CALL POSTSUM    USING POSTSUM-PARM                                
024600     END-READ                                                             
024700     .                                                                    
024800     EJECT                                                                
024900 S02-LAES-W33502  SECTION.                                                
025000     READ W33502-IN INTO INREG-AREA                                       
025100     AT END                                                               
025200        MOVE +999999999 TO INREG-IDARTNR                                  
025300        SET END-OF-W33502 TO TRUE                                         
025400                                                                          
025500     NOT AT END                                                           
025600        MOVE 'W33502-IN' TO POSTSUM-FDNAMN                                
025700        MOVE 'W33502D2' TO POSTSUM-DDNAMN2                                
025800        MOVE 'INREG'    TO POSTSUM-TRANSTYP                               
025900        CALL POSTSUM    USING POSTSUM-PARM                                
026000     END-READ                                                             
026100     .                                                                    
026200     EJECT                                                                
026300 S03-PRARTSJK     SECTION.                                                
026400     SKIP2                                                                
026500     MOVE  'X'                 TO XY-IDMARKBO                             
026600                                  WS-IDPTYP                               
026700     MOVE WS-TTMMSSTH          TO XY-TIKLOCK                              
026800     COMPUTE XY-PRARTBTO-MARK = W011-PRARTSJK * 2                         
026900     PERFORM S11-SKRIV-W33501                                             
027000     .                                                                    
027100     EJECT                                                                
027200 S04-PRARTSTD     SECTION.                                                
027300     SKIP2                                                                
027400     MOVE  'Y'                 TO XY-IDMARKBO                             
027500                                  WS-IDPTYP                               
027600     COMPUTE XY-PRARTBTO-MARK = W011-PRARTSTD * 2                         
027700     PERFORM S11-SKRIV-W33501                                             
027800     .                                                                    
027900     EJECT                                                                
028000 S11-SKRIV-W33501 SECTION.                                                
028100                                                                          
028200     WRITE XY-POST FROM XY-AREA                                           
028300                                                                          
028400     MOVE WS-IDPTYP TO POSTSUM-TRANSTYP                                   
028500     MOVE 'W33501' TO POSTSUM-FDNAMN                                      
028600     MOVE 'W33502D3' TO POSTSUM-DDNAMN2                                   
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000 S12-SKRIV-W33502 SECTION.                                                
029100                                                                          
029200     WRITE UTREG-POST FROM UTREG-AREA                                     
029300                                                                          
029400     MOVE 'REG'       TO POSTSUM-TRANSTYP                                 
029500     MOVE 'W33502-UT' TO POSTSUM-FDNAMN                                   
029600     MOVE 'W33502D4'  TO POSTSUM-DDNAMN2                                  
029700     CALL POSTSUM     USING POSTSUM-PARM                                  
029800     .                                                                    
029900     EJECT                                                                
030000 S99-ABEND SECTION.                                                       
030100                                                                          
030200     SKIP2                                                                
030300     MOVE 'S' TO POSTSUM-OPKOD                                            
030400     CALL POSTSUM USING POSTSUM-PARM                                      
030500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
030600     .                                                                    
