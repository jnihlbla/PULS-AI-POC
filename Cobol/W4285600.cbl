000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4285600.                                                 
000300*AUTHOR.        JAN-ERIK FRANTZEN                                         
000400*DATE-WRITTEN.  NOVEMBER 1993.                                            
000500                                                                          
000600*REMARKS:                                                                 
000700                                                                          
000800*    FUNKTION:                                                            
000900                                                                          
001000*        PROGRAMMET LÄSER WDA2 MED SB.                                    
001100*        OCH SKAPAR EN FIL MED INFO TILL EPLUS KÖRNINGAR.                 
001200*        FILEN RENAMAS SENARE TILL W020.KRED.W47970.                      
001300*                                                                         
001400*        PROGRAMMET LÄSER    WDG2 VALUTAKURSER H-TYP 9305                 
001500*                            WDGX9308 VIA W510CURR PGM                    
001600*                                                                         
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200*                                                                         
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*                                                                         
002600     SELECT W42856    ASSIGN TO W42856D1.                                 
002700*                                                                         
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W42856                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700*01  A1-AREA  -COPY W4797001  -L                                          
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4285600'.               
004300 77  JA                          PIC X(1)    VALUE 'J'.                   
004400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004500 77  WS-REEMBHNT                 PIC S9(2)V9(1) VALUE +0 COMP-3.          
004600 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
004610 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004620 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
004700*                                                                         
004800 01  DAGENS-DATUM.                                                        
004900   03  DAGENS-AAR                PIC 9(2).                                
005000   03  DAGENS-VECKA              PIC 9(2).                                
005100                                                                          
005200 01  LEV-DATUM.                                                           
005300   03  LEV-AAR                   PIC 9(2).                                
005400   03  LEV-VECKA                 PIC 9(2).                                
005500                                                                          
005600     EJECT                                                                
005700 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
005800                                                                          
005900 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
006000*01  FILLER   -COPY WWDIST79   -RED TEST-IDDISTR.                         
006100     EJECT                                                                
006200                                                                          
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006810     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
006900                                                                          
007000     EJECT                                                                
007100 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
007200     SKIP2                                                                
007300*01          -COPY W4797001 -PRE A11-                                     
007400     EJECT                                                                
007500 01  FILLER                      PIC X(8)    VALUE 'DATUM   '.            
007600     SKIP2                                                                
007700*01  -COPY WDATAREA                                                       
007800     EJECT                                                                
007810*    --- PARAMETRAR TILL W510CURR                                         
007820*01  -COPY W510CURR                                                       
007900                                                                          
008000 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
008100 01  IMS-WS.                                                              
008200                                                                          
008300     03  STATUS-WS               PIC X(2).                                
008400        88  SEGMENT-FINNS                    VALUE '  '.                  
008500        88  BASEN-SLUT                       VALUE 'GB'.                  
008600                                                                          
008700     03  GODK-STATUSKODER.                                                
008800         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008900                                                                          
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400                                                                          
009500     EJECT                                                                
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*01  -COPY W0005    -PRE POSTSUM-                                         
009900     EJECT                                                                
010000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA             PIC X(1500).                                 
010300     SKIP3                                                                
010400*    03  WDA201  -COPY WDA201    -RED IO-AREA                             
010500     EJECT                                                                
010600*    03  WDA211  -COPY WDA211    -RED IO-AREA                             
010700     EJECT                                                                
010800*    03  WDA221  -COPY WDA221    -RED IO-AREA                             
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100     SKIP3                                                                
011200*01  -COPY W0008   -PRE WDA2-                                             
011300         05  FILLER          PIC X(1).                                    
011400     EJECT                                                                
011500*01  -COPY W0008  -PRE 9305-                                              
011600         05  FILLER          PIC X(1).                                    
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING WDA2-PCB 9305-PCB.                             
011900     ENTRY 'DLITCBL' USING WDA2-PCB 9305-PCB.                             
012000                                                                          
012100     PERFORM A-INIT                                                       
012200     PERFORM IMS-GET-WDA201                                               
012300                                                                          
012400     PERFORM UNTIL BASEN-SLUT                                             
012500                                                                          
012600                                                                          
012700       IF WDA2-SEG-NAME-FB = 'WDA201'                                     
012800          PERFORM B-LEVANM                                                
012900       ELSE                                                               
013000         PERFORM IMS-GET-WDA201                                           
013100       END-IF                                                             
013200                                                                          
013300     END-PERFORM                                                          
013400     PERFORM Z-FINIT                                                      
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 A-INIT SECTION.                                                          
014000                                                                          
014100     OPEN OUTPUT W42856                                                   
014200                                                                          
014300                                                                          
014400     MOVE 'IDAG  '         TO DAT-KDDATFORM                               
014500     CALL WDATKONV USING DAT-KDDATFORM                                    
014600                         DAT-I-TIDATUM                                    
014700                         DAT-O-TIDATUM                                    
014800                         DAT-KDSVAR                                       
014900                                                                          
015000     MOVE DAT-TIAA         TO DAGENS-AAR                                  
015100                              W-DATE-AAMM(1:2)                            
015200     MOVE DAT-TIVV         TO DAGENS-VECKA                                
015300     MOVE DAT-TIMM         TO W-DATE-AAMM(3:2)                            
015400                                                                          
015500     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
015600     .                                                                    
015700                                                                          
015800     EJECT                                                                
015900 B-LEVANM SECTION.                                                        
016000                                                                          
016100     PERFORM BA-FLYTTA-HUVUD                                              
016200     PERFORM IMS-GET-WDA201                                               
016300     PERFORM UNTIL WDA2-SEG-NAME-FB  = 'WDA201' OR BASEN-SLUT             
016400                                                                          
016500        IF WDA2-SEG-NAME-FB = 'WDA211' AND LEV-TIKNOTA > +0               
016600           MOVE LEV-TIKNOTA           TO DAT-I-TIDATUM                    
016700           PERFORM S01-CALL-WDATCONV-AAMMDD                               
016800                                                                          
016900           IF DAT-KDSVAR-OK                                               
017000             MOVE DAT-TIAA           TO LEV-AAR                           
017100             MOVE DAT-TIVV           TO LEV-VECKA                         
017200           ELSE                                                           
017300             MOVE '0000'             TO LEV-DATUM                         
017400           END-IF                                                         
017500           IF DAGENS-DATUM = LEV-DATUM AND                                
017600              LEV-FLANNULL NOT = 1                                        
017700                                                                          
017800              IF LEV-KDKREBEH (1:1) NOT = 'N'                             
017900                PERFORM BB-FLYTTA-POSTER                                  
018000                PERFORM BC-SKRIV-W42856                                   
018100              END-IF                                                      
018200           END-IF                                                         
018300        END-IF                                                            
018400        PERFORM IMS-GET-WDA201                                            
018500     END-PERFORM                                                          
018600     .                                                                    
018700     EJECT                                                                
018800 BA-FLYTTA-HUVUD SECTION.                                                 
018900     MOVE '211'                   TO A11-IDPTYP                           
019000     MOVE ANM-IDDISTR             TO A11-IDDISTR                          
019100     MOVE ANM-IDKUNDNR            TO A11-IDKUNDNR                         
019200     MOVE ANM-IDRAPPNR            TO A11-IDLEVANM                         
019300     MOVE ANM-DALEVANM (3:6)      TO A11-TIM-LEVANM                       
019400     MOVE +0                      TO A11-KDUPPTYP                         
019500     MOVE ANM-KDLEVANM            TO A11-KDTRSTAT                         
019600     MOVE ANM-RELANDCO            TO A11-RELANDCO                         
019700     MOVE ANM-REEMBHNT            TO WS-REEMBHNT                          
019800     MOVE ANM-PRFRAKT             TO A11-PRFRAKT                          
019900     MOVE ANM-PRLEGKST            TO A11-PRLEGKST                         
020000     MOVE ANM-PRFOERS             TO A11-PRFOERS                          
020100     MOVE +0                      TO A11-PREXPKST                         
020200     MOVE ANM-DARETILL (3:6)      TO A11-TIM-RETILL                       
020300     MOVE ANM-KDVALISO            TO WS-KDVALISO                          
020400     .                                                                    
020500     EJECT                                                                
020600 BB-FLYTTA-POSTER SECTION.                                                
020700                                                                          
020800     MOVE LEV-IDDC                TO A11-IDDC                             
020900     MOVE LEV-IDORDNR5            TO A11-IDORDNR                          
021000     MOVE LEV-IDARTNR             TO A11-IDARTNR                          
021100     MOVE LEV-KVLEVANM            TO A11-KVLEVANM                         
021200     MOVE LEV-KDANMORS            TO A11-KDANMORS                         
021300                                                                          
021400     MOVE A11-IDDISTR  TO TEST-IDDISTR                                    
021500     IF DIST79-DEALER-PRICE OR                                            
021520        DIST79-ECOM-PRICE                                                 
021600                                                                          
021700*- PRISET SKALL VARA I SEK.MAN HÄMTAR KURSEN OCH RÄKNAR OM.               
021800       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
021900                                                                          
022000**** LÄS PRKURS HTYP 9305               *****                             
022200       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
022210       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
022220       MOVE 'M'                   TO CURR-KDVALTYP                        
022230                                                                          
022240       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
022250       IF CURR-KDSVAR = ' '                                               
022260         CONTINUE                                                         
022270       ELSE                                                               
022280         MOVE 1                   TO CURR-PRKURS-NEW                      
022290       END-IF                                                             
022291                                                                          
022300       COMPUTE A11-PRARTBTO ROUNDED =                                     
022400          LEV-PRARTBTO-LOC  * CURR-PRKURS-NEW                             
022500                                                                          
022600       END-COMPUTE                                                        
022700     ELSE                                                                 
022800       MOVE LEV-PRARTBTO          TO A11-PRARTBTO                         
022900     END-IF                                                               
023000                                                                          
023100     MOVE +0                      TO A11-FLSKROT                          
023200     MOVE +0                      TO A11-IDRETILL                         
023300     MOVE LEV-KVRETINL            TO A11-KVRETINL                         
023400     MOVE LEV-IDKNOTNR            TO A11-IDKNOTNR                         
023500     MOVE LEV-TIKNOTA             TO A11-TIM-KN                           
023600     IF WS-REEMBHNT > +0                                                  
023700       COMPUTE A11-PREMBHNT ROUNDED =                                     
023800                (WS-REEMBHNT * A11-PRARTBTO) + A11-PRARTBTO               
023900     ELSE                                                                 
024000        MOVE +0                   TO A11-PREMBHNT                         
024100     END-IF                                                               
024200     MOVE LEV-TIINLINL            TO DAT-I-TIDATUM                        
024300     PERFORM S01-CALL-WDATCONV-AAMMDD                                     
024400                                                                          
024500     IF DAT-KDSVAR-OK                                                     
024600        MOVE DAT-TIAAVVD        TO A11-TIV-RETREG                         
024700     ELSE                                                                 
024800        MOVE +0                 TO A11-TIV-RETREG                         
024900     END-IF                                                               
025000                                                                          
025100     .                                                                    
025200     EJECT                                                                
025300 BC-SKRIV-W42856 SECTION.                                                 
025400                                                                          
025500     WRITE A1-AREA     FROM A11-W4797001                                  
025600                                                                          
025700     MOVE 'W42856'     TO POSTSUM-FDNAMN                                  
025800     MOVE 'W42856D1'   TO POSTSUM-DDNAMN2                                 
025900     MOVE '211'        TO POSTSUM-TRANSTYP                                
026000                                                                          
026100     CALL POSTSUM USING POSTSUM-PARM                                      
026200     .                                                                    
026300     EJECT                                                                
026400 Z-FINIT  SECTION.                                                        
026500                                                                          
026600     CLOSE W42856                                                         
026700                                                                          
026800     MOVE 'S'          TO POSTSUM-OPKOD                                   
026900     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027100     EJECT                                                                
027200 S01-CALL-WDATCONV-AAMMDD        SECTION.                                 
027300                                                                          
027400     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
027500                                                                          
027600     CALL WDATKONV USING DAT-KDDATFORM                                    
027700                         DAT-I-TIDATUM                                    
027800                         DAT-O-TIDATUM                                    
027900                         DAT-KDSVAR                                       
028000     .                                                                    
028100                                                                          
028200     EJECT                                                                
028300*         * I M S  S E C T I O N                                          
028400                                                                          
028500 IMS-GET-WDA201         SECTION.                                          
028600                                                                          
028700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
028800     CALL CBLTDLI USING GN WDA2-PCB IO-AREA                               
028900     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     SKIP2                                                                
030600 IMS-STATUSKONTROLL       SECTION.                                        
030700                                                                          
030800     SET STATUS-IX TO 1                                                   
030900     SEARCH GODK-STATUS AT END CALL FELLOG                                
031000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
031100           CONTINUE                                                       
031200     END-SEARCH                                                           
031300     .                                                                    
