000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2712900.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   JANUARI 2003.                                            
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*                                                                         
001100*        PROGRAMMET SKRIVER EN LISTA SOM SKICKAS VIA MAIL                 
001200*        TILL DEN SOM HAR BESTÄLLT URVALET FRÅN 2348                      
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          ---                                                            
003000     SELECT W27129                     ASSIGN TO W27129D1.                
003100     SKIP2                                                                
003200*          ---                                                            
003300     SELECT W271UT                     ASSIGN TO W27129D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W27129                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200*01  -COPY W27128      -L.                                                
004300                                                                          
004400                                                                          
004500 FD  W271UT                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  ORDER-REPORT-REC       PIC X(100).                                   
005000                                                                          
005100                                                                          
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -CHECKED BY WY2000                                                   
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(8)    VALUE 'W2712900'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W27129-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W27129                       VALUE 'J'.                   
006300                                                                          
006400     EJECT                                                                
006500 01  ARBETSAREOR.                                                         
006600                                                                          
006700     03 WS-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.            
006800     03 WS-TIME                  PIC  9(8)  VALUE ZERO.                   
007100     03 WS-RED-DATUM.                                                     
007200       05  FILLER                PIC X(2)   VALUE '20'.                   
007300       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
007400       05  FILLER                PIC X      VALUE '/'.                    
007500       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
007600       05  FILLER                PIC X      VALUE '/'.                    
007700       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
007800     03 WS-TIPRLIST              PIC 9(8)   VALUE ZERO.                   
007900     03 WS-DAGENS-DATUM          PIC 9(8)   VALUE ZERO.                   
008000     03 WS-HELTAL                PIC 9(8)   VALUE ZERO.                   
008100     03 WS-HELTAL-RED            PIC Z(5)9  VALUE ZERO.                   
008200     03 WS-DECIMAL               PIC 9(3)   VALUE ZERO.                   
008300     03 WS-VKART-RED.                                                     
008400       05  WS-VKART-TKN          PIC X      VALUE SPACE.                  
008500       05  FILLER                PIC X      VALUE SPACE.                  
008600       05  WS-VKART              PIC X(7)   VALUE SPACE.                  
008700     03 WS-VLARTNTO-RED.                                                  
008800       05  WS-VLARTNTO-TKN       PIC X      VALUE SPACE.                  
008900       05  FILLER                PIC X      VALUE SPACE.                  
009000       05  WS-VLARTNTO           PIC X(7)   VALUE SPACE.                  
009100     03 WS-PRARTSTD-RED.                                                  
009200       05  WS-PRARTSTD-TKN       PIC X      VALUE SPACE.                  
009300       05  FILLER                PIC X      VALUE SPACE.                  
009400       05  WS-PRARTSTD           PIC X(7)   VALUE SPACE.                  
009500     03 WS-KVLS-RED.                                                      
009600       05  WS-KVLS-TKN           PIC X      VALUE SPACE.                  
009700       05  FILLER                PIC X      VALUE SPACE.                  
009800       05  WS-KVLS               PIC X(7)   VALUE SPACE.                  
009900     03 WS-TIFINLV-RED.                                                   
010000       05  WS-TIFINLV-TKN        PIC X      VALUE SPACE.                  
010100       05  FILLER                PIC X      VALUE SPACE.                  
010200       05  WS-TIFINLV            PIC X(5)   VALUE SPACE.                  
010300     03 WS-TIREFEFT-RED.                                                  
010400       05  WS-TIREFEFT-TKN       PIC X      VALUE SPACE.                  
010500       05  FILLER                PIC X      VALUE SPACE.                  
010600       05  WS-TIREFEFT           PIC X(6)   VALUE SPACE.                  
010700     03 WS-SUPERWEEK-RED.                                                 
010800       05  WS-SUPERWEEK-TKN      PIC X      VALUE SPACE.                  
010900       05  FILLER                PIC X      VALUE SPACE.                  
011000       05  WS-SUPERWEEK          PIC X(7)   VALUE SPACE.                  
011100     03  W-KVLS                  PIC 9(7)   VALUE ZERO.                   
011200     03  W-PRARTSTD              PIC 9(7).9(2)   VALUE ZERO.              
011300     03  WS-TOTAL-VALUE          PIC 9(10)  VALUE ZERO.                   
011400     03  WS-VALUE                PIC 9(9)V9(2)  VALUE ZERO.               
011500     03  WS-VALUE-rounded        PIC 9(9)   VALUE ZERO.                   
011600     03  WS-ANTAL-TRAEFF         PIC 9(9)   VALUE ZERO.                   
011700     03  WS-ANTAL-TRAEFF-RED     PIC Z(8)9  VALUE ZERO.                   
011800                                                                          
011900     03  RAD-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012000     03  KOL-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012100     03  TAB-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012200     03  FIL-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
012300     03  IX                      PIC S9(9)  VALUE ZERO COMP-3.            
012400                                                                          
012500                                                                          
012600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012700 01  FILLER REDEFINES DAGENS-DATUM.                                       
012800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013100                                                                          
013200 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
013300                                                                          
013400     SKIP3                                                                
013500*    --- Nycklar som läses från sysin                                     
013600 01  INX-SYSIN                   PIC X(80).                               
013700 01  INX-BEST                    PIC X(4)    VALUE SPACE.                 
013800                                                                          
013900                                                                          
014000     SKIP3                                                                
014100 01  FELMEDDELANDE.                                                       
014200   03 FILLER                     PIC X(34)                                
014300          VALUE 'MORE THAN 50000 LINES WAS CREATED,'.                     
014400   03 FILLER                     PIC X(35)                                
014500          VALUE 'PLEASE DO A NEW SELECTION FROM 2348'.                    
014600                                                                          
014700     EJECT                                                                
014800                                                                          
014900******************************************************************        
015000*      TABELLER                                                           
015100******************************************************************        
015200                                                                          
015300                                                                          
015400 01  TABENTRY-PARM.                                                       
015500                                                                          
015600     03  STEGLANGD                 PIC S9(9) COMP.                        
015700     03  POST-ANTAL                PIC S9(9) COMP.                        
015800     03  NYCKELLANGD               PIC S9(9) COMP.                        
015900                                                                          
016000 01  TAB-MAX                     PIC S9(9) COMP  VALUE ZERO.              
016100                                                                          
016200 01  RETURANTALTABELL.                                                    
016300     03  RETURANTAL OCCURS 5.                                             
016400        05  TAB-IDDC                  PIC  X(2).                          
016500        05  TAB-ANTAL                 PIC S9(7)    COMP-3.                
016600                                                                          
016700 01  DYNAMISKA-SUBPROGRAM.                                                
016800*                                                                         
016900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
017300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
017600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
017700     SKIP2                                                                
017800*    --- PARAMETRAR TILL ABEND                                            
017900                                                                          
018000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018200     SKIP2                                                                
018300 01  FELTEXT.                                                             
018400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018600     EJECT                                                                
018700*                                                                         
018800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27129'.              
018900     SKIP2                                                                
019000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019100     EJECT                                                                
019200*******                      PARAMETRAR TILL WORKDAY                      
019300*                                                                         
019400*01  -COPY WORKAREA                                                       
019500     EJECT                                                                
019600*01  -COPY WDATAREA                                                       
019700     EJECT                                                                
019800*******                      PARAMETRAR TILL WDAGKONV                     
019900*                                                                         
020000*01  -COPY WDAGAREA                                                       
020100     EJECT                                                                
020200 01  FILLER                      PIC X(24)   VALUE                        
020300                                 'IN-AREA        '.                       
020400     SKIP2                                                                
020500                                                                          
020600*01  AREA -COPY W27128     -PRE IN-                                       
020700     EJECT                                                                
020800 01  W271UT-AREA-START           PIC X(24)   VALUE                        
020900                                 'W271UT-AREA-START  '.                   
021000     SKIP2                                                                
021100                                                                          
021200 01  UT-AREOR.                                                            
021300     03 UT-RPT-PRINT-LINES.                                               
021400       05 UT-HEADING-1.                                                   
021500         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
021600         10 FILLER               PIC X(10) VALUE ' PARTS   '.             
021610         10 FILLER               PIC X(01) VALUE X'05'.                   
021611         10 FILLER               PIC X(01) VALUE X'05'.                   
021612         10 FILLER               PIC X(01) VALUE X'05'.                   
021613         10 FILLER               PIC X(01) VALUE X'05'.                   
021614         10 FILLER               PIC X(01) VALUE X'05'.                   
021615         10 FILLER               PIC X(01) VALUE X'05'.                   
021616         10 FILLER               PIC X(05) VALUE 'DATE '.                 
021620         10 UT-REPORT-DATE       PIC X(14) VALUE SPACE.                   
022100       05 UT-HEADING-2.                                                   
022200         10 URVALS-RAD OCCURS 8.                                          
022300           15 URVALS-KOL OCCURS 4.                                        
022400             20 UT-URVAL-TXT     PIC X(13) VALUE SPACE.                   
022401             20 FILLER           PIC X(01) VALUE X'05'.                   
022600             20 UT-URVAL         PIC X(09) VALUE SPACE.                   
022700             20 FILLER           PIC X(01) VALUE X'05'.                   
022800       05 UT-HEADING-3.                                                   
022900         10 UT-URVAL-TXT2        PIC X(08) VALUE SPACE.                   
023000         10 FILLER               PIC X(01) VALUE X'05'.                   
023100         10 UT-URVAL-BEN         PIC X(25) VALUE SPACE.                   
023200       05 UT-HEADING-4.                                                   
023300         10 FILLER               PIC X(10) VALUE '  PARTID'.              
023310         10 FILLER               PIC X(01) VALUE X'05'.                   
023400         10 FILLER               PIC X(04) VALUE 'DC  '.                  
023410         10 FILLER               PIC X(01) VALUE X'05'.                   
023500         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
023510         10 FILLER               PIC X(01) VALUE X'05'.                   
023700         10 FILLER               PIC X(09) VALUE 'BALANCE'.               
023710         10 FILLER               PIC X(01) VALUE X'05'.                   
023900         10 FILLER               PIC X(11) VALUE 'STOCK VALUE'.           
023910         10 FILLER               PIC X(01) VALUE X'05'.                   
024100         10 FILLER               PIC X(08) VALUE 'FORECAST'.              
024110         10 FILLER               PIC X(01) VALUE X'05'.                   
024300         10 FILLER               PIC X(11) VALUE 'ADDRESS'.               
024400       05 UT-RAD.                                                         
024500         10 UT-IDARTNR           PIC X(08) VALUE SPACE.                   
024600         10 FILLER               PIC X(01) VALUE X'05'.                   
024700         10 UT-IDDC              PIC X(02) VALUE SPACE.                   
024800         10 FILLER               PIC X(01) VALUE X'05'.                   
025000         10 UT-BEART             PIC X(19) VALUE SPACE.                   
025010         10 FILLER               PIC X(01) VALUE X'05'.                   
025200         10 UT-KVLS              PIC X(08) VALUE SPACE.                   
025210         10 FILLER               PIC X(01) VALUE X'05'.                   
025400         10 UT-VALUE             PIC X(10) VALUE SPACE.                   
025410         10 FILLER               PIC X(01) VALUE X'05'.                   
025600         10 UT-KVPB              PIC X(08) VALUE SPACE.                   
025610         10 FILLER               PIC X(01) VALUE X'05'.                   
025800         10 UT-ADLAGOMR          PIC X(02) VALUE SPACE.                   
025810         10 FILLER               PIC X(01) VALUE X'05'.                   
026000         10 UT-ADGANG            PIC X(02) VALUE SPACE.                   
026010         10 FILLER               PIC X(01) VALUE X'05'.                   
026200         10 UT-ADPLATS           PIC X(05) VALUE SPACE.                   
026300       05 UT-AVSLUT.                                                      
026400         10 FILLER               PIC X(08) VALUE '  NO OF '.              
026500         10 FILLER               PIC X(08) VALUE 'LINES : '.              
026600         10 UT-NO-OF-LINES       PIC X(09) VALUE SPACE.                   
026700         10 FILLER               PIC X(12) VALUE ' TOTAL STOCK'.          
026800         10 FILLER               PIC X(09) VALUE ' VALUE : '.             
026900         10 UT-TOTAL-VALUE       PIC X(10) VALUE SPACE.                   
027000     EJECT                                                                
027100 PROCEDURE DIVISION.                                                      
027200                                                                          
027300                                                                          
027400     PERFORM A-INIT                                                       
027500     PERFORM B-SKAPA-POSTER                                               
027600     PERFORM Z-FINIT                                                      
027700                                                                          
027800     MOVE ZERO TO RETURN-CODE                                             
027900     GOBACK                                                               
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300                                                                          
028400 A-INIT SECTION.                                                          
028500                                                                          
028600     OPEN INPUT  W27129                                                   
028700                                                                          
028800     OPEN OUTPUT W271UT                                                   
028900                                                                          
029000     ACCEPT INX-SYSIN  FROM SYSIN                                         
029100     MOVE INX-SYSIN          TO INX-BEST                                  
029200*    UNSTRING IN-SYSIN DELIMITED BY ','                                   
029300*      INTO INX-IDCATNR INX-IDCATGRP INX-IDCATAVS INX-KDCATPUB-R          
029400*           INX-ROUTINE                                                   
029500                                                                          
029600                                                                          
029700     ACCEPT DAGENS-DATUM FROM DATE                                        
029800                                                                          
029900     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
030000     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
030100                                                                          
030200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
030300                     DAT-O-TIDATUM DAT-KDSVAR                             
030400                                                                          
030500     IF DAT-KDSVAR-OK                                                     
030600****             HÄMTA SEKELSIFFROR                                       
030700                                                                          
030800       MOVE DAT-TISEKEL       TO WS-DAGENS-DATUM(1:2)                     
030900       MOVE DAGENS-DATUM      TO WS-DAGENS-DATUM(3:6)                     
031000                                                                          
031100     ELSE                                                                 
031200         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
031300         DELIMITED BY SIZE INTO FELTEXT                                   
031400         CALL FELLOG                                                      
031500     END-IF                                                               
031600     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
031700     MOVE DAGENS-DATUM-MAANAD                                             
031800                             TO WS-RED-MM                                 
031900     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
032000     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
032100     ACCEPT WS-TIME FROM TIME                                             
032200     MOVE ZERO               TO WS-ANTAL-TRAEFF                           
032300     .                                                                    
032400     EJECT                                                                
032500 B-SKAPA-POSTER SECTION.                                                  
032600*   POSTTYP 1                                                             
032700     PERFORM S01-LAES-W27129                                              
032800     PERFORM BA-SKRIV-RUBRIKER                                            
032900                                                                          
033000     IF INX-BEST = 'FULL'                                                 
033100       MOVE FELMEDDELANDE    TO ORDER-REPORT-REC                          
033200       WRITE ORDER-REPORT-REC                                             
033300     End-if                                                               
033400                                                                          
033500*   POSTTYP 2 ARTNR + BEART-RES                                           
033600     PERFORM S01-LAES-W27129                                              
033700                                                                          
033800     PERFORM UNTIL END-OF-W27129                                          
033900     OR  WS-ANTAL-TRAEFF > 50000                                          
034000       ADD 1                 TO WS-ANTAL-TRAEFF                           
034100       PERFORM BB-SKRIV-RADER                                             
034200                                                                          
034300*   POSTTYP 2 ARTNR + BEART-RES                                           
034400       PERFORM S01-LAES-W27129                                            
034500     END-PERFORM                                                          
034600                                                                          
034700     PERFORM UNTIL END-OF-W27129                                          
034800       ADD 1                 TO WS-ANTAL-TRAEFF                           
034900       MOVE IN-KVLS-RES      TO W-KVLS                                    
035000       COMPUTE WS-VALUE ROUNDED = W-KVLS * IN-PRARTSTD-RES                
035100       compute ws-value-rounded rounded = ws-value * 1                    
035200       ADD ws-VALUE-rounded  TO WS-TOTAL-VALUE                            
035300       PERFORM S01-LAES-W27129                                            
035400     END-PERFORM                                                          
035500                                                                          
035600     MOVE WS-ANTAL-TRAEFF    TO WS-ANTAL-TRAEFF-RED                       
035700                                                                          
035900     MOVE WS-ANTAL-TRAEFF-RED TO UT-NO-OF-LINES                           
035920     MOVE WS-TOTAL-VALUE     TO UT-TOTAL-VALUE                            
035930     INSPECT UT-TOTAL-VALUE REPLACING LEADING ZERO BY SPACE               
035940     MOVE UT-AVSLUT          TO ORDER-REPORT-REC                          
035950     WRITE ORDER-REPORT-REC                                               
036000     .                                                                    
036100     EJECT                                                                
036200 BA-SKRIV-RUBRIKER SECTION.                                               
036400     MOVE 1                  TO RAD-IX                                    
036500     MOVE 1                  TO KOL-IX                                    
036700     MOVE UT-HEADING-1       TO ORDER-REPORT-REC                          
036800     WRITE ORDER-REPORT-REC                                               
036900     IF IN-IDDC           NOT = SPACE                                     
037000        MOVE 'DC'            TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
037100        MOVE IN-IDDC         TO UT-URVAL     (RAD-IX, KOL-IX)             
037200        PERFORM BAA-ADD-INDEX                                             
037300     END-IF                                                               
037400     IF IN-FLKVROS        NOT = SPACE                                     
037500        MOVE 'BACKORDER'     TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
037600        MOVE IN-FLKVROS      TO UT-URVAL     (RAD-IX, KOL-IX)             
037700        PERFORM BAA-ADD-INDEX                                             
037800     END-IF                                                               
037900     IF IN-IDPERSON-BUY-FOM   NOT = SPACE                                 
038000        MOVE 'BUYER fom'         TO UT-URVAL-TXT (RAD-IX, KOL-IX)         
038100        INSPECT IN-IDPERSON-BUY-FOM  REPLACING                            
038200                LEADING ZERO BY SPACE                                     
038300        MOVE IN-IDPERSON-BUY-FOM TO UT-URVAL     (RAD-IX, KOL-IX)         
038400        PERFORM BAA-ADD-INDEX                                             
038500     END-IF                                                               
038600     IF IN-IDPERSON-BUY-TOM   NOT = SPACE                                 
038700        MOVE 'BUYER tom'         TO UT-URVAL-TXT (RAD-IX, KOL-IX)         
038800        INSPECT IN-IDPERSON-BUY-TOM  REPLACING                            
038900                LEADING ZERO BY SPACE                                     
039000        MOVE IN-IDPERSON-BUY-TOM TO UT-URVAL     (RAD-IX, KOL-IX)         
039100        PERFORM BAA-ADD-INDEX                                             
039200     END-IF                                                               
039300     IF IN-IDPERSON-BUY2     NOT = SPACE                                  
039400        MOVE 'BUYER'             TO UT-URVAL-TXT (RAD-IX, KOL-IX)         
039500        INSPECT IN-IDPERSON-BUY2  REPLACING                               
039600                LEADING ZERO BY SPACE                                     
039700        MOVE IN-IDPERSON-BUY2   TO UT-URVAL     (RAD-IX, KOL-IX)          
039800        PERFORM BAA-ADD-INDEX                                             
039900     END-IF                                                               
040000     IF IN-IDPERSON-BUY3     NOT = SPACE                                  
040100        MOVE 'BUYER'             TO UT-URVAL-TXT (RAD-IX, KOL-IX)         
040200        INSPECT IN-IDPERSON-BUY3  REPLACING                               
040300                LEADING ZERO BY SPACE                                     
040400        MOVE IN-IDPERSON-BUY3   TO UT-URVAL     (RAD-IX, KOL-IX)          
040500        PERFORM BAA-ADD-INDEX                                             
040600     END-IF                                                               
040700     IF IN-IDPERSON-BUY4     NOT = SPACE                                  
040800        MOVE 'BUYER'             TO UT-URVAL-TXT (RAD-IX, KOL-IX)         
040900        INSPECT IN-IDPERSON-BUY4  REPLACING                               
041000                LEADING ZERO BY SPACE                                     
041100        MOVE IN-IDPERSON-BUY4   TO UT-URVAL     (RAD-IX, KOL-IX)          
041200        PERFORM BAA-ADD-INDEX                                             
041300     END-IF                                                               
041400     IF IN-FLONORDER      NOT = SPACE                                     
041500        MOVE 'ON ORDER'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
041600        MOVE IN-FLONORDER    TO UT-URVAL     (RAD-IX, KOL-IX)             
041700        PERFORM BAA-ADD-INDEX                                             
041800     END-IF                                                               
041900     IF IN-IDPROJ (1)     NOT = SPACE                                     
042000        MOVE 'PROJ   '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
042100        MOVE IN-IDPROJ (1)   TO UT-URVAL     (RAD-IX, KOL-IX)             
042200        PERFORM BAA-ADD-INDEX                                             
042300     END-IF                                                               
042400     IF IN-IDPROJ (2)     NOT = SPACE                                     
042500        MOVE 'PROJ   '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
042600        MOVE IN-IDPROJ (2)   TO UT-URVAL     (RAD-IX, KOL-IX)             
042700        PERFORM BAA-ADD-INDEX                                             
042800     END-IF                                                               
042900     IF IN-IDPROJ (3)     NOT = SPACE                                     
043000        MOVE 'PROJ   '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
043100        MOVE IN-IDPROJ (3)   TO UT-URVAL     (RAD-IX, KOL-IX)             
043200        PERFORM BAA-ADD-INDEX                                             
043300     END-IF                                                               
043400     IF IN-FLAK-DC        NOT = SPACE                                     
043500        MOVE 'AK in DC'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
043600        MOVE IN-FLAK-DC      TO UT-URVAL     (RAD-IX, KOL-IX)             
043700        PERFORM BAA-ADD-INDEX                                             
043800     END-IF                                                               
043900     IF IN-FLASEAS        NOT = SPACE                                     
044000        MOVE 'SEASON'        TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
044100        MOVE IN-FLASEAS      TO UT-URVAL     (RAD-IX, KOL-IX)             
044200        PERFORM BAA-ADD-INDEX                                             
044300     END-IF                                                               
044400     IF IN-IDLEVNR-CDC    NOT = SPACE                                     
044500        MOVE 'SUPP CDC'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
044600        MOVE IN-IDLEVNR-CDC  TO UT-URVAL      (RAD-IX, KOL-IX)            
044700        PERFORM BAA-ADD-INDEX                                             
044800     END-IF                                                               
044900     IF IN-KDPRODSL       NOT = SPACE                                     
045000        MOVE 'PRODUCT GROUP' TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
045100        MOVE IN-KDPRODSL     TO UT-URVAL     (RAD-IX, KOL-IX)             
045200        PERFORM BAA-ADD-INDEX                                             
045300     END-IF                                                               
045400     IF IN-IDLEVNR-DC     NOT = SPACE                                     
045500        MOVE 'SUPP DC '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
045600        MOVE IN-IDLEVNR-DC   TO UT-URVAL     (RAD-IX, KOL-IX)             
045700        PERFORM BAA-ADD-INDEX                                             
045800     END-IF                                                               
045900     IF IN-IDFKNGRP-FOM NOT = SPACE                                       
046000        MOVE 'FCNGRP '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
046100        MOVE IN-IDFKNGRP-FOM TO UT-URVAL     (RAD-IX, KOL-IX)             
046200        PERFORM BAA-ADD-INDEX                                             
046300     END-IF                                                               
046400     IF IN-IDFKNGRP-TOM NOT = SPACE                                       
046500        MOVE 'FCNGRP '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
046600        MOVE IN-IDFKNGRP-TOM TO UT-URVAL     (RAD-IX, KOL-IX)             
046700        PERFORM BAA-ADD-INDEX                                             
046800     END-IF                                                               
046900     IF IN-FLREFILL       NOT = SPACE                                     
047000        MOVE 'REF PART'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
047100        MOVE IN-FLREFILL     TO UT-URVAL     (RAD-IX, KOL-IX)             
047200        PERFORM BAA-ADD-INDEX                                             
047300     END-IF                                                               
047400     IF IN-PRISRAD        NOT = SPACE                                     
047500        MOVE 'PRICE  '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
047600        MOVE IN-PRISRAD      TO UT-URVAL     (RAD-IX, KOL-IX)             
047700        PERFORM BAA-ADD-INDEX                                             
047800     END-IF                                                               
047900     IF IN-PBRAD          NOT = SPACE                                     
048000        MOVE 'CLASS  '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
048100        MOVE IN-PBRAD        TO UT-URVAL     (RAD-IX, KOL-IX)             
048200        PERFORM BAA-ADD-INDEX                                             
048300     END-IF                                                               
048400     IF IN-FLREFBEO       NOT = SPACE                                     
048500        MOVE 'AUTOREF '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
048600        MOVE IN-FLREFBEO     TO UT-URVAL     (RAD-IX, KOL-IX)             
048700        PERFORM BAA-ADD-INDEX                                             
048800     END-IF                                                               
048900     IF IN-IDREFTAB       NOT = SPACE                                     
049000        MOVE 'TABLE  '       TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
049100        MOVE IN-IDREFTAB     TO UT-URVAL     (RAD-IX, KOL-IX)             
049200        PERFORM BAA-ADD-INDEX                                             
049300     END-IF                                                               
049400     IF IN-VKART-TKN      NOT = SPACE                                     
049500        MOVE 'WEIGHT  '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
049600        MOVE IN-VKART-TKN    TO WS-VKART-TKN                              
049700        MOVE IN-VKART        TO WS-VKART                                  
049800        MOVE WS-VKART-RED    TO UT-URVAL     (RAD-IX, KOL-IX)             
049900        PERFORM BAA-ADD-INDEX                                             
050000     END-IF                                                               
050100     IF IN-KVPB-REF-FOM NOT = SPACE                                       
050200        MOVE 'FORECAST'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
050300        MOVE IN-KVPB-REF-FOM TO UT-URVAL     (RAD-IX, KOL-IX)             
050400        PERFORM BAA-ADD-INDEX                                             
050500     END-IF                                                               
050600     IF IN-KVPB-REF-TOM NOT = SPACE                                       
050700        MOVE 'FORECAST'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
050800        MOVE IN-KVPB-REF-TOM TO UT-URVAL     (RAD-IX, KOL-IX)             
050900        PERFORM BAA-ADD-INDEX                                             
051000     END-IF                                                               
051100     IF IN-VLARTNTO-TKN   NOT = SPACE                                     
051200        MOVE 'VOLUME  '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
051300        MOVE IN-VLARTNTO-TKN TO WS-VLARTNTO-TKN                           
051400        MOVE IN-VLARTNTO (3:7)                                            
051500                             TO WS-VLARTNTO                               
051600        MOVE WS-VLARTNTO-RED TO UT-URVAL     (RAD-IX, KOL-IX)             
051700        PERFORM BAA-ADD-INDEX                                             
051800     END-IF                                                               
051900     IF IN-ADLAGOMR       NOT = SPACE                                     
052000        MOVE 'AREA    '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
052100        MOVE IN-ADLAGOMR     TO UT-URVAL     (RAD-IX, KOL-IX)             
052200        PERFORM BAA-ADD-INDEX                                             
052300     END-IF                                                               
052400     IF IN-ADGANG         NOT = SPACE                                     
052500        MOVE 'AISLE   '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
052600        MOVE IN-ADGANG       TO UT-URVAL     (RAD-IX, KOL-IX)             
052700        PERFORM BAA-ADD-INDEX                                             
052800     END-IF                                                               
052900     IF IN-ADPLATS-FOM    NOT = SPACE                                     
053000        MOVE 'LOCATION'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
053100        MOVE IN-ADPLATS-FOM  TO UT-URVAL     (RAD-IX, KOL-IX)             
053200        PERFORM BAA-ADD-INDEX                                             
053300     END-IF                                                               
053400     IF IN-ADPLATS-TOM    NOT = SPACE                                     
053500        MOVE 'LOCATION'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
053600        MOVE IN-ADPLATS-TOM  TO UT-URVAL     (RAD-IX, KOL-IX)             
053700        PERFORM BAA-ADD-INDEX                                             
053800     END-IF                                                               
053900     IF IN-KDERS          NOT = SPACE                                     
054000        MOVE 'SUPCODE '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
054100        MOVE IN-KDERS        TO UT-URVAL     (RAD-IX, KOL-IX)             
054200        PERFORM BAA-ADD-INDEX                                             
054300     END-IF                                                               
054400     IF IN-PRARTSTD-TKN   NOT = SPACE                                     
054500        MOVE 'STDPRICE'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
054600        MOVE IN-PRARTSTD-TKN TO WS-PRARTSTD-TKN                           
054700        MOVE IN-PRARTSTD (3:7)                                            
054800                             TO WS-PRARTSTD                               
054900        MOVE WS-PRARTSTD-RED TO UT-URVAL     (RAD-IX, KOL-IX)             
055000        PERFORM BAA-ADD-INDEX                                             
055100     END-IF                                                               
055200     IF IN-KVLS-TKN       NOT = SPACE                                     
055300        MOVE 'BALANCE '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
055400        MOVE IN-KVLS-TKN     TO WS-KVLS-TKN                               
055500        MOVE IN-KVLS         TO WS-KVLS                                   
055600        MOVE WS-KVLS-RED     TO UT-URVAL     (RAD-IX, KOL-IX)             
055700        PERFORM BAA-ADD-INDEX                                             
055800     END-IF                                                               
055900     IF IN-TIFINLV-TKN    NOT = SPACE                                     
056000        MOVE 'PUBLWEEK'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
056100        MOVE IN-TIFINLV-TKN  TO WS-TIFINLV-TKN                            
056200        MOVE IN-TIFINLV      TO WS-TIFINLV                                
056300        MOVE WS-TIFINLV-RED  TO UT-URVAL     (RAD-IX, KOL-IX)             
056400        PERFORM BAA-ADD-INDEX                                             
056500     END-IF                                                               
056600     IF IN-TIREFEFT-TKN NOT = SPACE                                       
056700        MOVE 'LASTSALE'      TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
056800        MOVE IN-TIREFEFT-TKN TO WS-TIREFEFT-TKN                           
056900        MOVE IN-TIREFEFT     TO WS-TIREFEFT                               
057000        MOVE WS-TIREFEFT-RED TO UT-URVAL     (RAD-IX, KOL-IX)             
057100        PERFORM BAA-ADD-INDEX                                             
057200     END-IF                                                               
057300     IF IN-BEART          NOT = SPACE                                     
057400        MOVE 'DESCR  '       TO UT-URVAL-TXT2                             
057500        MOVE IN-BEART        TO UT-URVAL-BEN                              
057600       MOVE UT-HEADING-3     TO ORDER-REPORT-REC                          
057700       WRITE ORDER-REPORT-REC                                             
057800     END-IF                                                               
057900     IF IN-BEMODELL       NOT = SPACE                                     
058000        MOVE 'MODEL '        TO UT-URVAL-TXT (RAD-IX, KOL-IX)             
058100        MOVE IN-BEMODELL     TO UT-URVAL     (RAD-IX, KOL-IX)             
058200        PERFORM BAA-ADD-INDEX                                             
058300     END-IF                                                               
058400     IF IN-KDREFSTA       NOT = SPACE                                     
058500        MOVE 'REF STAT '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)            
058600        MOVE IN-KDREFSTA      TO UT-URVAL     (RAD-IX, KOL-IX)            
058700        PERFORM BAA-ADD-INDEX                                             
058800     END-IF                                                               
058900     IF IN-SUPERWEEK-TKN       NOT = SPACE                                
059000        MOVE 'SUP WEEK '      TO UT-URVAL-TXT (RAD-IX, KOL-IX)            
059100        MOVE IN-SUPERWEEK-TKN TO WS-SUPERWEEK-TKN                         
059200        MOVE IN-SUPERWEEK     TO WS-SUPERWEEK                             
059300        MOVE WS-SUPERWEEK-RED TO UT-URVAL     (RAD-IX, KOL-IX)            
059400        PERFORM BAA-ADD-INDEX                                             
059500     END-IF                                                               
059600     IF IN-FLFLYG       NOT = SPACE                                       
059700        MOVE 'AIR '           TO UT-URVAL-TXT (RAD-IX, KOL-IX)            
059800        MOVE IN-FLFLYG        TO UT-URVAL     (RAD-IX, KOL-IX)            
059900        PERFORM BAA-ADD-INDEX                                             
060000     END-IF                                                               
060100     IF KOL-IX > 1                                                        
060200       MOVE SPACE             TO ORDER-REPORT-REC                         
060300       MOVE URVALS-RAD (RAD-IX)                                           
060400                              TO ORDER-REPORT-REC                         
060500       WRITE ORDER-REPORT-REC                                             
060600     END-IF                                                               
060700                                                                          
060800     MOVE UT-HEADING-4        TO ORDER-REPORT-REC                         
060900     WRITE ORDER-REPORT-REC                                               
061100     .                                                                    
061200     EJECT                                                                
061300 BAA-ADD-INDEX SECTION.                                                   
061400     ADD 1                   TO KOL-IX                                    
061500     IF KOL-IX > 4                                                        
061600       MOVE SPACE            TO ORDER-REPORT-REC                          
061700       MOVE URVALS-RAD (RAD-IX)                                           
061800                             TO ORDER-REPORT-REC                          
061900       WRITE ORDER-REPORT-REC                                             
062000       ADD 1                 TO RAD-IX                                    
062100       MOVE 1                TO KOL-IX                                    
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 BB-SKRIV-RADER SECTION.                                                  
063600                                                                          
063700     MOVE IN-IDARTNR-RES     TO UT-IDARTNR                                
063800     MOVE IN-IDDC-RES        TO UT-IDDC                                   
063900     MOVE IN-BEART-RES       TO UT-BEART                                  
064000     MOVE IN-KVLS-RES        TO UT-KVLS                                   
064100     MOVE IN-ADLAGOMR-RES    TO UT-ADLAGOMR                               
064200     MOVE IN-ADGANG-RES      TO UT-ADGANG                                 
064300     MOVE IN-ADPLATS-RES     TO UT-ADPLATS                                
064400     MOVE IN-KVPB-REF-RES    TO UT-KVPB                                   
064500     MOVE IN-KVLS-RES        TO W-KVLS                                    
064600     COMPUTE WS-VALUE ROUNDED = W-KVLS * IN-PRARTSTD-RES                  
064700     compute ws-value-rounded rounded = ws-value * 1                      
064800     move ws-value-rounded to ut-value                                    
064900     ADD ws-VALUE-rounded  TO WS-TOTAL-VALUE                              
065000     INSPECT UT-VALUE REPLACING LEADING ZERO BY SPACE                     
065100     MOVE UT-RAD             TO ORDER-REPORT-REC                          
065200     WRITE ORDER-REPORT-REC                                               
065400     .                                                                    
065500     EJECT                                                                
067800 Z-FINIT SECTION.                                                         
067900     CLOSE W27129                                                         
068000           W271UT                                                         
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400                                                                          
068500 S01-LAES-W27129  SECTION.                                                
068600*   POSTTYP 1                                                             
068700     READ W27129 INTO IN-AREA                                             
068800     AT END                                                               
068900        SET END-OF-W27129 TO TRUE                                         
069000                                                                          
069100     END-READ                                                             
069200     .                                                                    
