000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613400.                                                 
001000*AUTHOR.        CARINA VIKTORSSON.                                        
001100*DATE-WRITTEN.  NOV  1987.                                                
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (PORTUGAL)                          
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIS OCH RKA  SKRIVS                                           
002100*                                                                         
002500*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46134                       ASSIGN TO UT-S-W46134D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W46155                       ASSIGN TO UT-S-W46134D2.         
004000     SELECT W4615E                       ASSIGN TO UT-S-W46134D3.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46134                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004801     SKIP2                                                                
005216*01  FILLER -COPY W461RISN        -L.                                     
005217     SKIP2                                                                
005230*01  FILLER -COPY W461RKAN        -L.                                     
005247     EJECT                                                                
005250 FD  W46155                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700 FD  W4615E                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS 0.                                                    
006000 01  UTPOST2                      PIC X(80).                              
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613400'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46134-EOF                  PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008200                                                                          
008300 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008400 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008500                                                                          
008600*- - - - - - - - - - - - - -                                              
008700                                                                          
008800     EJECT                                                                
008900 01  DAGENS-DATUM.                                                        
009000   03  DAGENS-DATUM-AR           PIC 9(2).                                
009100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009300                                                                          
009400 01  DAGENS-TID.                                                          
009500   03  DAGENS-TID-TIM            PIC 9(2).                                
009600   03  DAGENS-TID-MIN            PIC 9(2).                                
009700   03  DAGENS-TID-SEK            PIC 9(2).                                
009800                                                                          
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010100   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010300     SKIP3                                                                
010400*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010500                                                                          
010600 01  RETURKODER.                                                          
010700   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010900   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011000     EJECT                                                                
011100*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011200                                                                          
011300******************************************************************        
011400                                                                          
011500 01  W-ARBAREA.                                                           
011600 03  W-ARBAREA-X                 PIC X(102).                              
011700     SKIP2                                                                
011800*                                                                         
012010*03  FILLER  -COPY W461RISN        -PRE W- -RED W-ARBAREA-X               
012011     EJECT                                                                
012013*03  FILLER  -COPY W461RKAN        -PRE W- -RED W-ARBAREA-X               
012014     EJECT                                                                
012031                                                                          
012032********************* GAMLA UTSEENDET ****************************        
012033**                                                                        
012034 01  W-OLDAREA.                                                           
012035 03  W-OLDAREA-X                 PIC X(80).                               
012036     SKIP2                                                                
012037*                                                                         
012076*03  FILLER  -COPY W461RIS0        -PRE O- -RED W-OLDAREA-X               
012077     EJECT                                                                
012088*03  FILLER  -COPY W461RKA0        -PRE O- -RED W-OLDAREA-X               
012089     EJECT                                                                
012106                                                                          
012200*                             STARTKORT                                   
012300*01  -COPY W461RI0                                                        
012500     EJECT                                                                
012600*                             SLUTKORT                                    
012700*01  -COPY W461RI9                                                        
012900     EJECT                                                                
013000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013100                                                                          
013200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013300     SKIP2                                                                
013400*01  -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013800                                                                          
013900*01  -COPY W0005       -PRE  POSTSUM-.                                    
014100     EJECT                                                                
014200 PROCEDURE DIVISION.                                                      
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500     PERFORM B-BEHANDLA                                                   
014600     PERFORM Z-FINIT                                                      
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000                                                                          
015100     SKIP3                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400     OPEN INPUT W46134 OUTPUT W46155 W4615E                               
015500     SKIP2                                                                
015600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015700     SKIP2                                                                
015800*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016000     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016100     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016200     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016300     SKIP2                                                                
016400*- - - - - - - - - - - - - - - - TID                                      
016500*                                                                         
016600     ACCEPT   DAGENS-TID FROM TIME                                        
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 B-BEHANDLA SECTION.                                                      
017100     SKIP2                                                                
017200     PERFORM BA-START-KORT                                                
017300     PERFORM S01-LAS-W46134                                               
017400     PERFORM UNTIL                                                        
017500      NOT ( W46134-EOF = NEJ )                                            
017600       EVALUATE W-RIS-IDPTYP                                              
018920                                                                          
019037                                                                          
019038         WHEN      'RIS'                                                  
019039              PERFORM B-RIS-AENDRA-TILL-OLD                               
019040                                                                          
019059         WHEN      'RKA'                                                  
019060              PERFORM B-RKA-AENDRA-TILL-OLD                               
019061                                                                          
019085                                                                          
019086       END-EVALUATE                                                       
019088         ADD         +1           TO W-ANT-POSTER                         
019089         WRITE       UTPOST   FROM W-OLDAREA                              
019090         WRITE       UTPOST2  FROM W-OLDAREA                              
019092       PERFORM S01-LAS-W46134                                             
019093     END-PERFORM                                                          
019094     PERFORM BB-SLUT-KORT                                                 
019095     .                                                                    
019096     EJECT                                                                
019097                                                                          
019375 B-RIS-AENDRA-TILL-OLD SECTION.                                           
019376                                                                          
019377     MOVE W-RIS-IDPTYP         TO O-RIS-IDPTYP                            
019378     MOVE W-RIS-IDARTNR        TO O-RIS-IDARTNR                           
019379     MOVE W-RIS-REKSIFFR       TO O-RIS-REKSIFFR                          
019380     MOVE W-RIS-IDFKNGRP       TO O-RIS-IDFKNGRP                          
019381     MOVE W-RIS-KDSRA          TO O-RIS-KDSRA                             
019382     MOVE W-RIS-KVQPACK-1      TO O-RIS-KVQPACK-1                         
019383     MOVE +1                   TO O-RIS-KDCLAGER                          
019384     MOVE W-RIS-KDARTURS       TO O-RIS-KDARTURS                          
019385     MOVE W-RIS-KDPRODSL       TO O-RIS-KDPRODSL                          
019386     MOVE W-RIS-VLARTNTO       TO O-RIS-VLARTNTO                          
019387     MOVE W-RIS-VKART          TO O-RIS-VKART                             
019388     MOVE W-RIS-KDVSOP         TO O-RIS-KDVSOP                            
019389     MOVE W-RIS-IDSTATNR       TO O-RIS-IDSTATNR                          
019390     MOVE W-RIS-PRARTBTO-EXP   TO O-RIS-PRARTBTO-EXP                      
019391     MOVE W-RIS-FLMILART       TO O-RIS-FLMILART                          
019392     MOVE W-RIS-KDSORT         TO O-RIS-KDSORT                            
019393     MOVE W-RIS-KDERS          TO O-RIS-KDERS                             
019394     MOVE W-RIS-KDBPSR         TO O-RIS-KDBPSR                            
019395     MOVE W-RIS-KDBBCL         TO O-RIS-KDBBCL                            
019396     MOVE W-RIS-IDLEVNR        TO O-RIS-IDLEVNR                           
019397     MOVE W-RIS-KDAGE          TO O-RIS-KDAGE                             
019398     .                                                                    
019399     EJECT                                                                
019400                                                                          
019457 B-RKA-AENDRA-TILL-OLD SECTION.                                           
019458                                                                          
019459     MOVE W-ARBAREA            TO W-OLDAREA                               
019460     .                                                                    
019461     EJECT                                                                
019462                                                                          
020000 BA-START-KORT SECTION.                                                   
020100     SKIP2                                                                
020200     MOVE     'RI0'          TO START-IDPTYP                              
020300     MOVE     1920           TO START-IDDISTR                             
020400     MOVE     1              TO START-KDCLAGER                            
020500     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020600*                                                                         
020700     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020800*                                                                         
020900     DISPLAY  'START-KORT  ' START-W461RI0                                
021000     WRITE    UTPOST         FROM  START-W461RI0                          
021100     WRITE    UTPOST2        FROM  START-W461RI0                          
021200     .                                                                    
021300     SKIP2                                                                
021400 BB-SLUT-KORT SECTION.                                                    
021500     SKIP2                                                                
021600     MOVE     'RI9'          TO SLUT-IDPTYP                               
021700     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021800*                                                                         
021900     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022000     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
022100     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022200     .                                                                    
022300     SKIP2                                                                
022400 S01-LAS-W46134 SECTION.                                                  
022500     SKIP2                                                                
022600     READ   W46134 INTO W-ARBAREA                                         
022700     AT END MOVE JA TO W46134-EOF                                         
022800     END-READ                                                             
022900                                                                          
023000     IF W46134-EOF = NEJ                                                  
023100                                                                          
023200       MOVE 'W46134'            TO POSTSUM-FDNAMN                         
023300       MOVE 'W46134D1'          TO POSTSUM-DDNAMN2                        
023400       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023500       CALL POSTSUM             USING POSTSUM-PARM                        
023600                                                                          
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 Z-FINIT SECTION.                                                         
024100     SKIP2                                                                
024200                                                                          
024300     CLOSE W46134 W46155 W4615E                                           
024400     SKIP2                                                                
024500*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024600*                                    SKRIVNA POSTER                       
024700                                                                          
024800     MOVE 'S' TO POSTSUM-OPKOD                                            
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
