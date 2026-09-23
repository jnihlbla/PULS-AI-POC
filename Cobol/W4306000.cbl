000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W4306000.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   NOVEMBER 2007.                                        
000600*                                                                         
000700*    FUNKTION:                                                            
000800*               SELECTERAR UT PACKADE ARTIKLAR FRÅN LDC                   
000900*               WXTR.PACKRAD                                              
001000*                                                                         
001100                                                                          
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700                                                                          
001800     SELECT PACKRAD ASSIGN       TO W43060D1.                             
001900                                                                          
002000     SELECT W43060  ASSIGN       TO W43060D2.                             
002100                                                                          
002200     SELECT W43061  ASSIGN       TO W43060D3.                             
002300                                                                          
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  PACKRAD                                                              
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0.                                                    
003100                                                                          
003200 01  INPOST.                                                              
003300*    03   -COPY WXTRA0    -L                                              
003400     SKIP2                                                                
003500                                                                          
003600 FD  W43060                                                               
003700     RECORDING F                                                          
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  UTPOST      -COPY W43060    -L                                       
004100     SKIP2                                                                
004200                                                                          
004300 FD  W43061                                                               
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700 01  UTPOST2                   PIC X(80).                                 
004800     SKIP2                                                                
004900                                                                          
005000 WORKING-STORAGE SECTION.                                                 
005100     SKIP3                                                                
005200 77  IDPGM                   PIC X(8)      VALUE 'W4306000'.              
005300 77  JA                      PIC X         VALUE 'J'.                     
005400 77  NEJ                     PIC X         VALUE 'N'.                     
005500 77  EOF-PACKRAD             PIC X         VALUE 'N'.                     
005600 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
005700 77  MAX-WEEK                PIC S9(3)   COMP-3 VALUE +53.                
005800                                                                          
005900 01  WS-DAREGDAT             PIC X(8)    VALUE SPACE.                     
006000 01  WS-IDLOPNR              PIC S9(3)   COMP-3 VALUE ZERO.               
006100                                                                          
006200 01  SUBPROGRAM.                                                          
006300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006310     03  WL10WBDC            PIC X(8)    VALUE 'WL10WBDC'.                
006400                                                                          
006500 01  PACKRA-TRANSID.                                                      
006600     03  FILLER              PIC X(6) VALUE 'PACKRA'.                     
006700     03  FILLER              PIC X(8) VALUE 'W43060D1'.                   
006800     03  FILLER              PIC X(4) VALUE '  IN'.                       
006900                                                                          
007000 01  W43060-TRANSID.                                                      
007100     03  FILLER              PIC X(6) VALUE 'W46030'.                     
007200     03  FILLER              PIC X(8) VALUE 'W43060D2'.                   
007300     03  FILLER              PIC X(4) VALUE '  UT'.                       
007400                                                                          
007500     EJECT                                                                
007510*    --- PARAMETRAR TILL WL10WBDC                                         
007520*01  -COPY WL10WBDC                                                       
007530     EJECT                                                                
007540*    --- PARAMETRAR TILL POSTSUM                                          
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800                                                                          
007900 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
008000     SKIP3                                                                
008100*    -COPY WZ20DAYS                                                       
008200     EJECT                                                                
008300                                                                          
008700 01  FILLER                  PIC X(16)   VALUE 'IN-AREA    '.             
008800 01  INAREA.                                                              
008900*    03  -COPY WXTRA0 -PRE IN-                                            
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                  PIC X(16)   VALUE 'UT-AREA     '.            
009300 01  UTAREA.                                                              
009400*    03  -COPY W43060 -PRE UT-                                            
009500     EJECT                                                                
009600                                                                          
009700 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
009800 01  WS-AREA.                                                             
009900     03 WS-FILLER1             PIC X(8)  VALUE SPACE.                     
010000     03 WS-DAEXDAT             PIC X(4) VALUE " < '".                     
010100     03 WS-DELDATUM            PIC X(8)  VALUE SPACE.                     
010200     03 WS-FILLER2             PIC X(60) VALUE "')".                      
010300                                                                          
010400 PROCEDURE DIVISION.                                                      
010500 MAIN SECTION.                                                            
010600                                                                          
010700     PERFORM A-INIT                                                       
010800                                                                          
010900     PERFORM S01-READ-PACKRAD-POST                                        
011000                                                                          
011100     PERFORM UNTIL  EOF-PACKRAD = JA                                      
011200       PERFORM B-READ-PACKRAD                                             
011300       PERFORM S01-READ-PACKRAD-POST                                      
011400     END-PERFORM                                                          
011500     PERFORM D-CREATE-DELETE-DATE                                         
011600                                                                          
011700     PERFORM Z-END                                                        
011800                                                                          
011900     MOVE ZERO TO RETURN-CODE                                             
012000     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400                                                                          
012500 A-INIT SECTION.                                                          
012600     OPEN INPUT  PACKRAD                                                  
012700          OUTPUT W43060                                                   
012800          OUTPUT W43061                                                   
012900                                                                          
013000     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013100                                                                          
013200     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
013300     MOVE ZERO TO WS-IDLOPNR                                              
013400     .                                                                    
013500     EJECT                                                                
013600                                                                          
013700 B-READ-PACKRAD SECTION.                                                  
013800     MOVE IN-IDDC TO WBDC-IDDC                                            
013810     CALL WL10WBDC USING WBDC-AREA                                        
013900     IF WBDC-FLWEBDC = JA                                                 
014000       ADD +1  TO WS-IDLOPNR                                              
014100       PERFORM C-CREATE-PACKRAD                                           
014200     ELSE                                                                 
014300       CONTINUE                                                           
014400     END-IF                                                               
014500     .                                                                    
014600     EJECT                                                                
014700                                                                          
014800 C-CREATE-PACKRAD SECTION.                                                
014900     MOVE IN-IDARTNR       TO UT-IDARTNR                                  
015000     MOVE WS-DAREGDAT      TO UT-DAREGDAT                                 
015100     MOVE WS-IDLOPNR       TO UT-IDLOPNR                                  
015200     MOVE IN-IDDISTR       TO UT-IDDISTR                                  
015300     MOVE IN-IDKUNDNR      TO UT-IDKUNDNR                                 
015400     MOVE IN-IDPRODNR      TO UT-IDPRODNR                                 
015500     MOVE IN-IDORDER       TO UT-IDORDER                                  
015600     MOVE IN-IDDC          TO UT-IDDC                                     
015700     MOVE IN-ADLAGOMR      TO UT-ADLAGOMR                                 
015800                                                                          
015900     WRITE UTPOST FROM UT-W43060                                          
016000     MOVE W43060-TRANSID TO POSTSUM-TRANSID                               
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400                                                                          
016500 D-CREATE-DELETE-DATE SECTION.                                            
016600     MOVE WS-DAREGDAT             TO DAYS-TIDATE2                         
016700     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT2                       
016800     MOVE '183'                   TO DAYS-KVDAYS                          
016900     MOVE 'WEEKDAYS'              TO DAYS-IDCALEND                        
017000     MOVE SPACE                   TO DAYS-TIDATE1                         
017100     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT1                       
017200     CALL WZ20DAYS USING                                                  
017300          DAYS-WZ20DAYS                                                   
017400     IF DAYS-KDRC = ZERO                                                  
017500       MOVE DAYS-TIDATE1(1:8)     TO WS-DELDATUM                          
017600     ELSE                                                                 
017700      DISPLAY ' ERROR IN WZ20DAYS ' DAYS-KDRC                             
017800     END-IF                                                               
017900                                                                          
018000     WRITE UTPOST2 FROM WS-AREA                                           
018100     .                                                                    
018200     EJECT                                                                
018300                                                                          
018400 Z-END SECTION.                                                           
018500     CLOSE PACKRAD                                                        
018600           W43060                                                         
018700           W43061                                                         
018800                                                                          
018900     MOVE 'S'        TO POSTSUM-OPKOD                                     
019000     CALL POSTSUM USING POSTSUM-PARM                                      
019100     .                                                                    
019200     EJECT                                                                
019300                                                                          
019400 S01-READ-PACKRAD-POST SECTION.                                           
019500     READ PACKRAD INTO INAREA                                             
019600     AT END                                                               
019700       MOVE JA TO EOF-PACKRAD                                             
019800     NOT AT END                                                           
019900       MOVE PACKRA-TRANSID TO POSTSUM-TRANSID                             
020000       CALL POSTSUM USING POSTSUM-PARM                                    
020100     END-READ                                                             
020200     .                                                                    
020300     SKIP2                                                                
