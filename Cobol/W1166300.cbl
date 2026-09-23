000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1166300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   20/02/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*                LÄSER AKTUELL ARTIKELFIL FÖR NDC                         
001000*                SKAPAR TOTALFIL ARTIKELINFO TILL VIPS                    
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
002400     SKIP2                                                                
002500*          --- SYSIN COUNTRY CODE FROM VCOMPARM                           
002600     SELECT SYSIN                      ASSIGN TO SYSINPUT.                
002700     SKIP2                                                                
002800*          --- INPUT  PART REGISTER                                       
002900     SELECT W11690                     ASSIGN TO W11663D2.                
003000     SKIP2                                                                
003100*          --- OUTPUT PART INFO VIPS                                      
003200     SELECT W11663                     ASSIGN TO W11663D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  SYSIN                                                                
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  PARM-VCOM                   PIC X(80).                               
004300                                                                          
004400 FD  W11690                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W11616A        -PRE IN-   -L.                                  
004900     SKIP3                                                                
005000 FD  W11663                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  RECORD -COPY W11620A -PRE  UT-  -L.                                  
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W1166300'.            
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006200                                                                          
006500 01  WS-IDARTNR.                                                          
006600     05 WS-IDARTNR-BLANK         PIC X(11).                               
006700     05 WS-IDARTNR-ALFA          PIC X(09).                               
006800                                                                          
006900 01  SPAR-IDARTNR-NUM-X.                                                  
007000     03 SPAR-IDARTNR-NUM         PIC 9(09).                               
007100     03 SPAR-IDARTNR REDEFINES SPAR-IDARTNR-NUM.                          
007200       05 SPAR-IDARTNR1            PIC X(01).                             
007300       05 SPAR-IDARTNR2            PIC X(01).                             
007400       05 SPAR-IDARTNR3            PIC X(01).                             
007500       05 SPAR-IDARTNR4            PIC X(01).                             
007600       05 SPAR-IDARTNR5            PIC X(01).                             
007700       05 SPAR-IDARTNR6            PIC X(01).                             
007800       05 SPAR-IDARTNR7            PIC X(01).                             
007900       05 SPAR-IDARTNR8            PIC X(01).                             
008000       05 SPAR-IDARTNR9            PIC X(01).                             
008100                                                                          
008200 77  W11690-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W11690                       VALUE 'J'.                   
008400     EJECT                                                                
008500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES TODAYS-DATE.                                        
008700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008900     03  TODAYS-DATE-DAY         PIC 9(2).                                
009000     EJECT                                                                
009200 01  GENERAL-SUBPROGRAMS.                                                 
009300*                                                                         
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     SKIP2                                                                
009700*    --- PARAMETERS TO ABEND                                              
009800                                                                          
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'VCOM-PARM'.           
010400 01  PARM-SYSINPUT.                                                       
010500     05 PARM-BESTLAND            PIC X(2).                                
010600     05 FILLER                   PIC X(72).                               
010700                                                                          
010800 01  ERROR-TEXT.                                                          
010900     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
011000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL POSTSUM                                          
011300*                                                                         
011400*01  -COPY W0005   -PRE  POSTSUM-                                         
011500     EJECT                                                                
011600 01  IN-AREA-START               PIC X(24)   VALUE                        
011700                                 'IN-AREA-START  '.                       
011800     SKIP2                                                                
011900                                                                          
012000*01  AREA -COPY W11616A -PRE IN-                                          
012100     EJECT                                                                
012200 01  UT-AREA-START               PIC X(24)   VALUE                        
012300                                 'UT-AREA-START  '.                       
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W11620A -PRE UT-                                          
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900 MAIN SECTION.                                                            
013000                                                                          
013100     PERFORM A-INIT                                                       
013200     PERFORM S01-READ-VCOM-PARM                                           
013300                                                                          
013400     PERFORM S02-READ-W11690                                              
013500     PERFORM UNTIL END-OF-W11690                                          
013600       IF IN-IDLANDX2 = PARM-BESTLAND                                     
013700          PERFORM B-BEARBETA-ARTIKELNR                                    
013800          PERFORM C-WRITE-W11663                                          
013900       END-IF                                                             
014000       PERFORM S02-READ-W11690                                            
014100     END-PERFORM                                                          
014200                                                                          
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN INPUT  SYSIN                                                    
015300                 W11690                                                   
015400                                                                          
015500          OUTPUT W11663                                                   
015600                                                                          
015700     ACCEPT TODAYS-DATE  FROM DATE                                        
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     .                                                                    
016000     EJECT                                                                
016100 B-BEARBETA-ARTIKELNR SECTION.                                            
016200                                                                          
016300     MOVE IN-IDARTNR              TO SPAR-IDARTNR-NUM                     
016400     IF SPAR-IDARTNR1 = '0'                                               
016500       MOVE SPACE                 TO SPAR-IDARTNR1                        
016600       IF SPAR-IDARTNR2 = '0'                                             
016700         MOVE SPACE               TO SPAR-IDARTNR2                        
016800         IF SPAR-IDARTNR3 = '0'                                           
016900           MOVE SPACE             TO SPAR-IDARTNR3                        
017000           IF SPAR-IDARTNR4 = '0'                                         
017100             MOVE SPACE           TO SPAR-IDARTNR4                        
017200             IF SPAR-IDARTNR5 = '0'                                       
017300               MOVE SPACE         TO SPAR-IDARTNR5                        
017400               IF SPAR-IDARTNR6 = '0'                                     
017500                 MOVE SPACE       TO SPAR-IDARTNR6                        
017600                 IF SPAR-IDARTNR7 = '0'                                   
017700                   MOVE SPACE     TO SPAR-IDARTNR7                        
017800                   IF SPAR-IDARTNR8 = '0'                                 
017900                     MOVE SPACE   TO SPAR-IDARTNR8                        
018000                     IF SPAR-IDARTNR9 = '0'                               
018100                       MOVE SPACE TO SPAR-IDARTNR9                        
018200                     END-IF                                               
018300                   END-IF                                                 
018400                 END-IF                                                   
018500               END-IF                                                     
018600             END-IF                                                       
018700           END-IF                                                         
018800         END-IF                                                           
018900       END-IF                                                             
019000     END-IF                                                               
019100                                                                          
019200     .                                                                    
019300     EJECT                                                                
019400 C-WRITE-W11663 SECTION.                                                  
019500                                                                          
019600     MOVE 'WA1'                  TO UT-IDPTYP                             
019700     MOVE SPAR-IDARTNR           TO WS-IDARTNR-ALFA                       
019800     MOVE SPACE                  TO WS-IDARTNR-BLANK                      
019900     MOVE WS-IDARTNR             TO UT-IDARTNR20                          
020000     MOVE IN-IDLANDX2            TO UT-IDLANDX2                           
020100     MOVE IN-IDFKNGRP            TO UT-IDFKNGRP                           
020200     MOVE IN-KDSRA               TO UT-KDSRA                              
020300     MOVE IN-KVQPACK-0           TO UT-KVQPACK-0                          
020400     MOVE IN-KDARTURS-NUM        TO UT-KDARTURS-NUM                       
020500     MOVE IN-KDPRODSL            TO UT-KDPRODSL                           
020600     MOVE IN-VLARTNTO            TO UT-VLARTNTO                           
020700     MOVE IN-VKART               TO UT-VKART                              
020800     MOVE IN-KDVSOP              TO UT-KDVSOP                             
020900     MOVE IN-IDSTATNR            TO UT-IDSTATNR                           
021000     MOVE IN-KDSORT              TO UT-KDSORT                             
021100     MOVE IN-KDERS               TO UT-KDERS                              
021200     MOVE IN-KDBPSR              TO UT-KDBPSR                             
021300     MOVE IN-KDBBCL              TO UT-KDBBCL                             
021400     MOVE IN-IDLEVNR             TO UT-IDLEVNR                            
021500     MOVE IN-KDAGE               TO UT-KDAGE                              
021600     MOVE IN-IDPROJ              TO UT-IDPROJ                             
021700     MOVE IN-PRARTSJK            TO UT-PRARTSJK                           
021800     MOVE IN-PRARTSTD            TO UT-PRARTSTD                           
021900     MOVE IN-BEART-L1            TO UT-BEART-L1                           
022000     MOVE IN-BEART-L2            TO UT-BEART-L2                           
022100     MOVE IN-KDPSLLOC            TO UT-KDPSLLOC                           
022200     MOVE IN-IDLEVNR-LOC         TO UT-IDLEVNR-LOC                        
022300     MOVE IN-KDSTANAUTG          TO UT-KDSTANAUTG                         
022400     MOVE IN-FLOVRLAG            TO UT-FLOVRLAG                           
022500     MOVE IN-FLSOFTWARE          TO UT-FLSOFTWARE                         
022600     MOVE IN-DADATUM             TO UT-DADATUM                            
022700     MOVE IN-IDLEVNR-DUBLETT     TO UT-IDLEVNR-DUBLETT                    
022800     MOVE IN-IDLEVNR-LOC-DUBLETT TO UT-IDLEVNR-LOC-DUBLETT                
022900     MOVE IN-KDTIPPR             TO UT-KDTIPPR                            
023000     MOVE IN-IDKAT(1)            TO UT-IDKAT(1)                           
023100     MOVE IN-IDKAT(2)            TO UT-IDKAT(2)                           
023200     MOVE IN-IDKAT(3)            TO UT-IDKAT(3)                           
023300     MOVE IN-BELEVART            TO UT-BELEVART                           
023400     MOVE IN-FLGEMFMC            TO UT-FLGEMFMC                           
023500     MOVE IN-IDPROJUP            TO UT-IDPROJUP                           
023600     MOVE IN-BEARTEXT            TO UT-BEARTEXT                           
023700     MOVE IN-TIURPROD            TO UT-TIURPROD                           
023800                                                                          
023900     PERFORM S11-WRITE-W11663                                             
024000     .                                                                    
024100     EJECT                                                                
024200 Z-FINIT SECTION.                                                         
024300                                                                          
024400     CLOSE SYSIN                                                          
024500           W11690                                                         
024600           W11663                                                         
024700                                                                          
024800     MOVE 'S' TO POSTSUM-OPKOD                                            
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
025100     EJECT                                                                
025200 S01-READ-VCOM-PARM SECTION.                                              
025300                                                                          
025400     READ SYSIN                INTO PARM-SYSINPUT                         
025500     DISPLAY '     PARM-BESTLAND    =' PARM-BESTLAND                      
026700     .                                                                    
026800     EJECT                                                                
026900 S02-READ-W11690  SECTION.                                                
027000     READ W11690 INTO IN-AREA                                             
027100     AT END                                                               
027200        MOVE HIGH-VALUE TO IN-AREA                                        
027300        SET END-OF-W11690 TO TRUE                                         
027400                                                                          
027500     NOT AT END                                                           
027600        MOVE 'W11690' TO POSTSUM-FDNAMN                                   
027700        MOVE 'W11663D2' TO POSTSUM-DDNAMN2                                
027800        MOVE SPACE     TO POSTSUM-TRANSTYP                                
027900        CALL POSTSUM USING POSTSUM-PARM                                   
028000     END-READ                                                             
028100     .                                                                    
028200     EJECT                                                                
028300 S11-WRITE-W11663 SECTION.                                                
028400                                                                          
028500     WRITE UT-RECORD FROM UT-AREA                                         
028600                                                                          
028700     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
028800     MOVE 'W11663'   TO POSTSUM-FDNAMN                                    
028900     MOVE 'W11663D3' TO POSTSUM-DDNAMN2                                   
029000     MOVE PARM-BESTLAND TO POSTSUM-TRANSTYP                               
029100     CALL POSTSUM USING POSTSUM-PARM                                      
029200     .                                                                    
029300     EJECT                                                                
029400 S99-ABEND SECTION.                                                       
029500                                                                          
029600     SKIP2                                                                
029700     MOVE 'S' TO POSTSUM-OPKOD                                            
029800     CALL POSTSUM USING POSTSUM-PARM                                      
029900     CALL ABEND USING RKOD-ABEND                                          
030000     .                                                                    
