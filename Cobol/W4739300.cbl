000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4739300.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   13/12/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        TO CREATE XML FILE FOR PACKAGING                                 
001000*                                                                         
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
001800*                                                                         
001900*      13/12/24 - REDDY RAHUL     - ETRACKER 10221367                     
002000*                                   INITIAL VERSION.                      
002100                                                                          
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FILE FOR PACKAGING                                         
003100     SELECT W47392A                    ASSIGN TO W47393D1.                
003200     SKIP2                                                                
003300*          --- FILE FOR PACKAGING, IN XML FORMAT                          
003400     SELECT W47393                     ASSIGN TO W47393D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W47392A                                                              
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  IN-RECORD                   PIC X(56).                               
004500     SKIP3                                                                
004600 FD  W47393                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  UT-RECORD                   PIC X(163).                              
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W4739300'.            
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005610 77  HEA-SW                      PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W47392A-EOF-SW              PIC X       VALUE 'N'.                   
005900     88  END-OF-W47392A                      VALUE 'J'.                   
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 77  WS-IN-IDPTYP                PIC 9(01)   VALUE ZERO.                  
006800 77  WS-IDPTYP                   PIC 9(01)   VALUE ZERO.                  
006900 77  STA                         PIC 9(01)   VALUE 1.                     
007000 77  HEA                         PIC 9(01)   VALUE 2.                     
007100 77  DET                         PIC 9(01)   VALUE 3.                     
007200 01  WS-HEA-TAG-FLAG             PIC X(1)    VALUE 'C'.                   
007300     88  WS-HEA-TAG-OPENED                   VALUE 'O'.                   
007400     88  WS-HEA-TAG-CLOSED                   VALUE 'C'.                   
007500 01  WS-DET-TAG-FLAG             PIC X(1)    VALUE 'C'.                   
007600     88  WS-DET-TAG-OPENED                   VALUE 'O'.                   
007700     88  WS-DET-TAG-CLOSED                   VALUE 'C'.                   
007500 01  WS-STA-TAG-FLAG             PIC X(1)    VALUE 'C'.                   
007600     88  WS-STA-TAG-OPENED                   VALUE 'O'.                   
007700     88  WS-STA-TAG-CLOSED                   VALUE 'C'.                   
007800     EJECT                                                                
007900 01  GENERAL-SUBPROGRAMS.                                                 
008000*                                                                         
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
009200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800 01  IN-AREA-START                PIC X(24)  VALUE                        
009900                                 'IN-AREA-START  '.                       
010000     SKIP2                                                                
010100                                                                          
010200 01  INPUT-AREA.                                                          
010300     03  IN-AREA.                                                         
010400         05  IN-IDPTYP            PIC X(03).                              
010500         05  FILLER               PIC X(53).                              
010600*01  AREA -COPY WTMSSTA      -PRE IN-STA-                                 
010700     EJECT                                                                
010800*01  AREA -COPY WTMSHEA      -PRE IN-HEA-                                 
010900     EJECT                                                                
011000*01  AREA -COPY WTMSDET      -PRE IN-DET-                                 
011100     EJECT                                                                
011200                                                                          
011300 01  UT-AREA-START                PIC X(24)  VALUE                        
011400                                 'UT-AREA-START  '.                       
011500 01  UT-AREA                      PIC X(163) VALUE SPACE.                 
011600                                                                          
011700 01  XML-OPEN-TAG-DATA.                                                   
011800     03  STA-OPEN-TAG.                                                    
011900         05  FILLER               PIC X(54) VALUE                         
012000         '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'.        
012100         05  FILLER               PIC X(25) VALUE                         
012200         '<xml-import transaction="'.                                     
012300         05  STA-IDBATCH          PIC X(12).                              
012400         05  FILLER               PIC X(12) VALUE '" language="'.         
012500         05  STA-LANG             PIC X(02).                              
012600         05  FILLER               PIC X(02) VALUE '">'.                   
012700         05  FILLER               PIC X(07) VALUE '<msgid>'.              
012800         05  STA-IDMSG            PIC X(18).                              
012900         05  FILLER               PIC X(08) VALUE '</msgid>'.             
013000     03  HEA-OPEN-TAG.                                                    
013100         05  FILLER               PIC X(13) VALUE '<booking id="'.        
013200         05  HEA-IDLOPNR          PIC X(04).                              
013300         05  FILLER               PIC X(08) VALUE '" type="'.             
013400         05  HEA-TYPE             PIC X(03).                              
013500         05  FILLER               PIC X(02) VALUE '">'.                   
013600         05  FILLER               PIC X(06) VALUE '<ubid>'.               
013700         05  HEA-IDBOKN           PIC X(16).                              
013800         05  FILLER               PIC X(07) VALUE '</ubid>'.              
013900         05  FILLER               PIC X(07) VALUE '<idate>'.              
014000         05  HEA-DATUM-ISSUED     PIC X(10).                              
014100         05  FILLER               PIC X(08) VALUE '</idate>'.             
014200         05  FILLER               PIC X(07) VALUE '<bdate>'.              
014300         05  HEA-DATUM-BOOKED     PIC X(10).                              
014400         05  FILLER               PIC X(08) VALUE '</bdate>'.             
014500         05  FILLER               PIC X(08) VALUE '<source>'.             
014600         05  HEA-GSDB-SEND        PIC X(05).                              
014700         05  FILLER               PIC X(09) VALUE '</source>'.            
014800         05  FILLER               PIC X(13) VALUE '<destination>'.        
014900         05  HEA-GSDB-REC         PIC X(05).                              
015000         05  FILLER               PIC X(14) VALUE                         
015100                                                 '</destination>'.        
015200     03  DET-OPEN-TAG.                                                    
015300         05  FILLER               PIC X(07) VALUE '<items>'.              
015400     03  DET-DATA.                                                        
015500         05  FILLER               PIC X(08) VALUE '<lm id="'.             
015600         05  DET-EMBTYP           PIC X(05).                              
015700         05  FILLER               PIC X(08) VALUE '" type="'.             
015800         05  DET-TYPE             PIC X(06).                              
015900         05  FILLER               PIC X(07) VALUE '" ref="'.              
016000         05  DET-FSEDELNR         PIC X(07).                              
016100         05  FILLER               PIC X(08) VALUE '" ref2="'.             
016200         05  DET-REF2             PIC X(01) VALUE SPACE.                  
016300         05  FILLER               PIC X(08) VALUE '" note="'.             
016400         05  DET-NOTE             PIC X(01) VALUE SPACE.                  
016500         05  FILLER               PIC X(02) VALUE '">'.                   
016600         05  DET-EMBANTAL         PIC X(05).                              
016700         05  FILLER               PIC X(05) VALUE '</lm>'.                
016800                                                                          
016900 01  XML-CLOSE-TAG-DATA-TABLE.                                            
017000     03 XML-CLOSE-DATA.                                                   
017100        05 STA-CLOSE.                                                     
017200           07 FILLER             PIC X(13) VALUE '</xml-import>'.         
017300        05 HEA-CLOSE.                                                     
017400           07 FILLER             PIC X(10) VALUE '</booking>'.            
017500           07 FILLER             PIC X(03) VALUE SPACE.                   
017600        05 DET-CLOSE.                                                     
017700           07 FILLER             PIC X(08) VALUE '</items>'.              
017800           07 FILLER             PIC X(05) VALUE SPACE.                   
017900     03 XML-CLOSE-TAG REDEFINES XML-CLOSE-DATA OCCURS 3 TIMES             
018000                                 PIC X(13).                               
018100                                                                          
018200 PROCEDURE DIVISION.                                                      
018300 MAIN SECTION.                                                            
018400     SKIP2                                                                
018500                                                                          
018600     PERFORM A-INIT                                                       
018700     PERFORM S01-READ-W47392A                                             
018800     IF NOT END-OF-W47392A                                                
018900       MOVE ZERO                 TO WS-IDPTYP                             
019000     END-IF                                                               
019100     PERFORM UNTIL END-OF-W47392A                                         
019200       IF WS-IN-IDPTYP < WS-IDPTYP                                        
019300         PERFORM                                                          
019400           UNTIL WS-IN-IDPTYP > WS-IDPTYP                                 
019500           MOVE XML-CLOSE-TAG (WS-IDPTYP)                                 
019600                                 TO UT-AREA                               
019700           PERFORM S11-WRITE-W47393                                       
019800           IF WS-IDPTYP = HEA                                             
019900             SET WS-HEA-TAG-CLOSED                                        
020000                                 TO TRUE                                  
020100           END-IF                                                         
020200           IF WS-IDPTYP = DET                                             
020300             SET WS-DET-TAG-CLOSED                                        
020400                                 TO TRUE                                  
020500           END-IF                                                         
020600           COMPUTE WS-IDPTYP = WS-IDPTYP - 1                              
020700         END-PERFORM                                                      
020800       END-IF                                                             
020900       EVALUATE WS-IN-IDPTYP                                              
021000         WHEN STA                                                         
021100           MOVE IN-STA-TMS-IDBATCH                                        
021200                                 TO STA-IDBATCH                           
021300           MOVE IN-STA-FILLER-1  TO STA-LANG                              
021400           MOVE IN-STA-TMS-IDMSG TO STA-IDMSG                             
021500           MOVE STA-OPEN-TAG     TO UT-AREA                               
022300           SET  WS-STA-TAG-OPENED                                         
022520                                 TO TRUE                                  
021600           PERFORM S11-WRITE-W47393                                       
021700           PERFORM S21-INIT-STA-STA                                       
021800         WHEN HEA                                                         
021801           MOVE 'Y'              TO HEA-SW                                
021900           IF WS-HEA-TAG-OPENED                                           
022000             MOVE XML-CLOSE-TAG (HEA)                                     
022100                                 TO UT-AREA                               
022200             PERFORM S11-WRITE-W47393                                     
022300             SET WS-HEA-TAG-CLOSED                                        
022520                                 TO TRUE                                  
022520           END-IF                                                         
022600           MOVE IN-HEA-TMS-IDLOPNR                                        
022700                                 TO HEA-IDLOPNR                           
022800           MOVE IN-HEA-FILLER-1  TO HEA-TYPE                              
022900           MOVE IN-HEA-TMS-IDBOKN                                         
023000                                 TO HEA-IDBOKN                            
023100           MOVE IN-HEA-TMS-DATUM-ISSUED                                   
023200                                 TO HEA-DATUM-ISSUED                      
023300           MOVE IN-HEA-TMS-DATUM-BOOKED                                   
023400                                 TO HEA-DATUM-BOOKED                      
023500           MOVE IN-HEA-TMS-IDLEVNR-GSDB-SEND                              
023600                                 TO HEA-GSDB-SEND                         
023700           MOVE IN-HEA-TMS-IDLEVNR-GSDB-REC                               
023800                                 TO HEA-GSDB-REC                          
023900           MOVE HEA-OPEN-TAG     TO UT-AREA                               
024400         WHEN DET                                                         
024402           IF HEA-SW = 'Y'                                                
024410             PERFORM S11-WRITE-W47393                                     
024420             SET WS-HEA-TAG-OPENED                                        
024430                                 TO TRUE                                  
024440             PERFORM S22-INIT-HEA-HEA                                     
024441             MOVE 'N'            TO HEA-SW                                
024442           END-IF                                                         
024500           IF WS-DET-TAG-CLOSED                                           
024600             MOVE DET-OPEN-TAG   TO UT-AREA                               
024700             PERFORM S11-WRITE-W47393                                     
024800             SET WS-DET-TAG-OPENED                                        
024900                                 TO TRUE                                  
025000           END-IF                                                         
025100           MOVE IN-DET-TMS-EMBTYP                                         
025200                                 TO DET-EMBTYP                            
025300           MOVE IN-DET-FILLER-3  TO DET-TYPE                              
025400           MOVE IN-DET-TMS-FSEDELNR                                       
025500                                 TO DET-FSEDELNR                          
025600           MOVE IN-DET-FILLER-1  TO DET-REF2                              
025700           MOVE IN-DET-FILLER-2  TO DET-NOTE                              
025800           MOVE IN-DET-TMS-EMBANTAL                                       
025900                                 TO DET-EMBANTAL                          
026000           MOVE DET-DATA         TO UT-AREA                               
026100           PERFORM S11-WRITE-W47393                                       
026200           PERFORM S23-INIT-DET-DET                                       
026300       END-EVALUATE                                                       
026400       MOVE WS-IN-IDPTYP         TO WS-IDPTYP                             
026500                                                                          
026600       PERFORM S01-READ-W47392A                                           
026700     END-PERFORM                                                          
026800                                                                          
026800     IF  WS-DET-TAG-OPENED                                                
027100       MOVE XML-CLOSE-TAG (DET)                                           
027200                                 TO UT-AREA                               
027300       PERFORM S11-WRITE-W47393                                           
027310     END-IF                                                               
026800     IF WS-HEA-TAG-OPENED                                                 
027100       MOVE XML-CLOSE-TAG (HEA)                                           
027200                                 TO UT-AREA                               
027300       PERFORM S11-WRITE-W47393                                           
027310     END-IF                                                               
026800     IF WS-STA-TAG-OPENED                                                 
027100       MOVE XML-CLOSE-TAG (STA)                                           
027200                                 TO UT-AREA                               
027300       PERFORM S11-WRITE-W47393                                           
027310     END-IF                                                               
027600                                                                          
027700     PERFORM Z-FINIT                                                      
027800                                                                          
027900     MOVE ZERO                   TO RETURN-CODE                           
028000     GOBACK                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 A-INIT SECTION.                                                          
028400                                                                          
028500     OPEN INPUT  W47392A                                                  
028600                                                                          
028700     OPEN OUTPUT W47393                                                   
028800     SKIP2                                                                
028900     ACCEPT TODAYS-DATE        FROM DATE                                  
029000     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
029100     .                                                                    
029200     EJECT                                                                
029300 Z-FINIT SECTION.                                                         
029400     CLOSE W47392A                                                        
029500           W47393                                                         
029600     SKIP2                                                                
029700     MOVE 'S'                    TO POSTSUM-OPKOD                         
029800     CALL POSTSUM             USING POSTSUM-PARM                          
029900     .                                                                    
030000     EJECT                                                                
030100 S01-READ-W47392A SECTION.                                                
030200     READ W47392A              INTO IN-AREA                               
030300     AT END                                                               
030400       MOVE HIGH-VALUE           TO IN-AREA                               
030500       SET END-OF-W47392A        TO TRUE                                  
030600                                                                          
030700     NOT AT END                                                           
030800       MOVE 'W47392A'            TO POSTSUM-FDNAMN                        
030900       MOVE 'W47393D1'           TO POSTSUM-DDNAMN2                       
031000       MOVE IN-IDPTYP            TO POSTSUM-TRANSTYP                      
031100       CALL POSTSUM           USING POSTSUM-PARM                          
031200     END-READ                                                             
031300                                                                          
031410     EVALUATE IN-IDPTYP                                                   
031500       WHEN 'STA'                                                         
031600         MOVE STA                TO WS-IN-IDPTYP                          
031700         MOVE IN-AREA            TO IN-STA-AREA                           
031800       WHEN 'HEA'                                                         
031900         MOVE HEA                TO WS-IN-IDPTYP                          
032000         MOVE IN-AREA            TO IN-HEA-AREA                           
032100       WHEN 'DET'                                                         
032200         MOVE DET                TO WS-IN-IDPTYP                          
032300         MOVE IN-AREA            TO IN-DET-AREA                           
032400     END-EVALUATE                                                         
032500     .                                                                    
032600     EJECT                                                                
032700 S11-WRITE-W47393 SECTION.                                                
032800                                                                          
032900     WRITE UT-RECORD           FROM UT-AREA                               
033000                                                                          
033100     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
033200     MOVE 'W47393'               TO POSTSUM-FDNAMN                        
033300     MOVE 'W47393D2'             TO POSTSUM-DDNAMN2                       
033400     CALL POSTSUM             USING POSTSUM-PARM                          
033500                                                                          
033600     MOVE SPACES                 TO UT-AREA                               
033700     .                                                                    
033800     EJECT                                                                
033900 S21-INIT-STA-STA SECTION.                                                
034000                                                                          
034100     MOVE SPACES                 TO STA-IDBATCH                           
034200                                    STA-LANG                              
034300                                    STA-IDMSG                             
034400     .                                                                    
034500     EJECT                                                                
034600 S22-INIT-HEA-HEA SECTION.                                                
034700                                                                          
034800     MOVE SPACES                 TO HEA-IDLOPNR                           
034900                                    HEA-TYPE                              
035000                                    HEA-IDBOKN                            
035100                                    HEA-DATUM-ISSUED                      
035200                                    HEA-DATUM-BOOKED                      
035300                                    HEA-GSDB-SEND                         
035400                                    HEA-GSDB-REC                          
035500     .                                                                    
035600     EJECT                                                                
035700 S23-INIT-DET-DET SECTION.                                                
035800                                                                          
035900     MOVE SPACES                 TO DET-EMBTYP                            
036000                                    DET-TYPE                              
036100                                    DET-FSEDELNR                          
036200                                    DET-REF2                              
036300                                    DET-EMBANTAL                          
036400     .                                                                    
036500     EJECT                                                                
036600 S99-ABEND SECTION.                                                       
036700                                                                          
036800     SKIP2                                                                
036900     MOVE 'S'                    TO POSTSUM-OPKOD                         
037000     CALL POSTSUM             USING POSTSUM-PARM                          
037100     CALL ABEND               USING RKOD-ABEND                            
037200     .                                                                    
