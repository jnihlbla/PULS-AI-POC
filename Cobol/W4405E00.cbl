000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4405E00.                                                
000400 AUTHOR.         BO SVENSSON.                                             
000500 DATE-WRITTEN.   03/03/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER TRANSAR FRÅN VORKÖ NY.                                     
001100*        OM SAMMA ARTIKEL FÖREKOMMER FLERA GÅNGER SLÅS DEN IHOP.          
001200*        UTPOSTEN ANPASSAD FÖR EXCEL.                                     
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002701     SKIP2                                                                
002702*          --- RADER FRÅN VORKÖ NY                                        
002703     SELECT W4405D                     ASSIGN TO W4405ED1.                
002704     SKIP2                                                                
002705*          --- POSTER TILL UPPF. EXCEL ANPASSAT                           
002710     SELECT W4405G                     ASSIGN TO W4405ED2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W4405D                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305                                                                          
003306*01  -COPY W4405D      -L.                                                
003307     SKIP3                                                                
003308 FD  W4405G                                                               
003309     RECORDING       V                                                    
003310     BLOCK CONTAINS  0.                                                   
003311                                                                          
003312 01  VOR-HEAD-POST PIC X(100).                                            
003320*01  POST -COPY W4405E -PRE  UT-  -L.                                     
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W4405E00'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
003901 77  HORIZTAB                    PIC X       VALUE X'05'.                 
003910 77  HEX-5E                      PIC X       VALUE ';'.                   
004000 77  IN-EJ-TOM-SW                PIC X       VALUE 'N'.                   
004100   88  IN-EJ-TOM                 VALUE 'J'.                               
004101                                                                          
004102 77  W4405E-EOF-SW               PIC X       VALUE 'N'.                   
004110     88  END-OF-W4405D                       VALUE 'J'.                   
004200     EJECT                                                                
004300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES DAGENS-DATUM.                                       
004500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004800     EJECT                                                                
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005300     SKIP2                                                                
005400*    --- PARAMETRAR TILL ABEND                                            
005500                                                                          
005600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006301     EJECT                                                                
006302*    --- PARAMETRAR TILL POSTSUM                                          
006303*                                                                         
006310*01  -COPY W0005   -PRE  POSTSUM-                                         
006501     EJECT                                                                
006502 01  IN-AREA-START               PIC X(24)   VALUE                        
006503                                 'IN-AREA-START  '.                       
006504     SKIP2                                                                
006505                                                                          
006506*01  AREA -COPY W4405D     -PRE IN-                                       
006507     EJECT                                                                
006508 01  UT-AREA-START               PIC X(24)   VALUE                        
006509                                 'UT-AREA-START  '.                       
006510     SKIP2                                                                
006511                                                                          
006520*01  AREA -COPY W4405E     -PRE UT-                                       
006530     EJECT                                                                
006540 01  JMF-AREA-START              PIC X(24)   VALUE                        
006550                                 'JMF-AREA-START '.                       
006560     SKIP2                                                                
006570                                                                          
006580*01  AREA -COPY W4405D     -PRE JMF-                                      
006581                                                                          
006590 01  VOR-HEAD-AREA                   PIC X(100).                          
006600     EJECT                                                                
006700 PROCEDURE DIVISION.                                                      
006800 MAIN SECTION.                                                            
007000     SKIP2                                                                
007100                                                                          
007200     PERFORM A-INIT                                                       
007310     PERFORM S01-LAES-W4405D                                              
007311                                                                          
007320     IF NOT END-OF-W4405D                                                 
007321       MOVE JA                   TO IN-EJ-TOM-SW                          
007322       MOVE IN-IDDISTR           TO JMF-IDDISTR                           
007323       MOVE IN-IDKUNDNR          TO JMF-IDKUNDNR                          
007324       MOVE IN-IDKUNDRF          TO JMF-IDKUNDRF                          
007325       MOVE IN-TIREGDAT-URSP TO JMF-TIREGDAT-URSP                         
007326       MOVE IN-IDARTNR           TO JMF-IDARTNR                           
007327       MOVE IN-KVWORKD           TO JMF-KVWORKD                           
007328       MOVE IN-KVRADER           TO JMF-KVRADER                           
007329       MOVE IN-KDVORATG          TO JMF-KDVORATG                          
007332     END-IF                                                               
007340                                                                          
007400     PERFORM UNTIL END-OF-W4405D                                          
007410       IF  IN-IDDISTR       = JMF-IDDISTR                                 
007420       AND IN-IDKUNDNR      = JMF-IDKUNDNR                                
007430       AND IN-IDKUNDRF      = JMF-IDKUNDRF                                
007440       AND IN-TIREGDAT-URSP = JMF-TIREGDAT-URSP                           
007450       AND IN-IDARTNR       = JMF-IDARTNR                                 
007500         IF  IN-KVWORKD > JMF-KVWORKD                                     
007600           MOVE IN-KVWORKD TO JMF-KVWORKD                                 
007610         END-IF                                                           
007611         IF  IN-KDVORATG < JMF-KDVORATG                                   
007612           MOVE IN-KDVORATG TO JMF-KDVORATG                               
007613         END-IF                                                           
007620                                                                          
007800       ELSE                                                               
007810*------------ OBS ' ' SKALL INNEHÅLLA TAB (HEX-05)                        
007900         MOVE ALL '	'            TO UT-AREA                               
007910         MOVE JMF-IDDISTR        TO UT-IDDISTR                            
007911         MOVE JMF-IDKUNDNR       TO UT-IDKUNDNR                           
007912         MOVE JMF-IDKUNDRF       TO UT-IDKUNDRF                           
007913         MOVE JMF-TIREGDAT-URSP  TO UT-TIREGDAT-URSP                      
007914         MOVE JMF-IDARTNR        TO UT-IDARTNR                            
007915         MOVE JMF-KVWORKD        TO UT-KVWORKD                            
007916         MOVE 1                  TO UT-KVRADER                            
007917                                                                          
007918         IF  JMF-KDVORATG = '0'                                           
007919         OR  JMF-KDVORATG = '1'                                           
007920         OR  JMF-KDVORATG = '2'                                           
007921             MOVE 'P'            TO UT-KDVORSTA                           
007922         ELSE                                                             
007923           IF JMF-KDVORATG = '3'                                          
007924              MOVE 'N'           TO UT-KDVORSTA                           
007925           ELSE                                                           
007926              MOVE 'A'           TO UT-KDVORSTA                           
007927           END-IF                                                         
007928         END-IF                                                           
007929                                                                          
007932         PERFORM S11-SKRIV-W4405G                                         
007933                                                                          
007934         MOVE IN-IDDISTR         TO JMF-IDDISTR                           
007935         MOVE IN-IDKUNDNR        TO JMF-IDKUNDNR                          
007936         MOVE IN-IDKUNDRF        TO JMF-IDKUNDRF                          
007937         MOVE IN-TIREGDAT-URSP   TO JMF-TIREGDAT-URSP                     
007938         MOVE IN-IDARTNR         TO JMF-IDARTNR                           
007939         MOVE IN-KVWORKD         TO JMF-KVWORKD                           
007940         MOVE IN-KVRADER         TO JMF-KVRADER                           
007950         MOVE IN-KDVORATG        TO JMF-KDVORATG                          
008106       END-IF                                                             
008107                                                                          
008110       PERFORM S01-LAES-W4405D                                            
008200     END-PERFORM                                                          
008300                                                                          
008310     IF IN-EJ-TOM                                                         
008311*------------ OBS ' ' SKALL INNEHÅLLA TAB (HEX-05)                        
008313       MOVE ALL '	'              TO UT-AREA                               
008314       MOVE JMF-IDDISTR          TO UT-IDDISTR                            
008315       MOVE JMF-IDKUNDNR         TO UT-IDKUNDNR                           
008316       MOVE JMF-IDKUNDRF         TO UT-IDKUNDRF                           
008317       MOVE JMF-TIREGDAT-URSP    TO UT-TIREGDAT-URSP                      
008318       MOVE JMF-IDARTNR          TO UT-IDARTNR                            
008319       MOVE JMF-KVWORKD          TO UT-KVWORKD                            
008320       MOVE 1                    TO UT-KVRADER                            
008321                                                                          
008322       IF    JMF-KDVORATG = '0'                                           
008323       OR    JMF-KDVORATG = '1'                                           
008324       OR    JMF-KDVORATG = '2'                                           
008325           MOVE 'P'              TO UT-KDVORSTA                           
008326       ELSE                                                               
008327         IF JMF-KDVORATG = '3'                                            
008328            MOVE 'N'             TO UT-KDVORSTA                           
008329         ELSE                                                             
008330            MOVE 'A'             TO UT-KDVORSTA                           
008331         END-IF                                                           
008332       END-IF                                                             
008333       PERFORM S11-SKRIV-W4405G                                           
008340     END-IF                                                               
008400                                                                          
008500     PERFORM Z-FINIT                                                      
008600                                                                          
008700     MOVE ZERO TO RETURN-CODE                                             
008800     GOBACK                                                               
008900     .                                                                    
009000     EJECT                                                                
009100 A-INIT SECTION.                                                          
009201                                                                          
009210     OPEN INPUT  W4405D                                                   
009301                                                                          
009310     OPEN OUTPUT W4405G                                                   
009400     SKIP2                                                                
009500     ACCEPT DAGENS-DATUM  FROM DATE                                       
009610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009620     PERFORM S02-TILL-VOR-HEAD                                            
009700     .                                                                    
009800     EJECT                                                                
009900 Z-FINIT SECTION.                                                         
010001     CLOSE W4405D                                                         
010010           W4405G                                                         
010101     SKIP2                                                                
010102     MOVE 'S' TO POSTSUM-OPKOD                                            
010110     CALL POSTSUM USING POSTSUM-PARM                                      
010200     .                                                                    
010301     EJECT                                                                
010302 S01-LAES-W4405D  SECTION.                                                
010303     READ W4405D INTO IN-AREA                                             
010304     AT END                                                               
010305        MOVE HIGH-VALUE TO IN-AREA                                        
010306        SET END-OF-W4405D TO TRUE                                         
010307                                                                          
010308     NOT AT END                                                           
010309        MOVE 'W4405D'   TO POSTSUM-FDNAMN                                 
010310        MOVE 'W4405ED1' TO POSTSUM-DDNAMN2                                
010313        MOVE SPACE      TO POSTSUM-TRANSTYP                               
010314        CALL POSTSUM USING POSTSUM-PARM                                   
010315     END-READ                                                             
010320     .                                                                    
010401     EJECT                                                                
010402                                                                          
010403**** RUBRIK TILL EXCEL-FIL ****                                           
010404 S02-TILL-VOR-HEAD  SECTION.                                              
010405                                                                          
010406     STRING 'DISTRICT', HORIZTAB,                                         
010407            'CUST NO', HORIZTAB,                                          
010408            'ORDER NO', HORIZTAB,                                         
010409            'REG DATE', HORIZTAB,                                         
010410            'PART NO', HORIZTAB,                                          
010411            'STATUS', HORIZTAB,                                           
010412            'WORKDAYS', HORIZTAB,                                         
010413            'LINES'                                                       
010415     DELIMITED BY SIZE INTO VOR-HEAD-AREA                                 
010416                                                                          
010417     WRITE VOR-HEAD-POST FROM VOR-HEAD-AREA                               
010418     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
010419     MOVE 'W4405F' TO POSTSUM-FDNAMN                                      
010420     MOVE 'W4405DD3' TO POSTSUM-DDNAMN2                                   
010421     CALL POSTSUM USING POSTSUM-PARM                                      
010422     .                                                                    
010423                                                                          
010424     EJECT                                                                
010425 S11-SKRIV-W4405G SECTION.                                                
010426                                                                          
010427     WRITE UT-POST FROM UT-AREA                                           
010428                                                                          
010429     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010430     MOVE 'W4405G'   TO POSTSUM-FDNAMN                                    
010431     MOVE 'W4405ED2' TO POSTSUM-DDNAMN2                                   
010432     CALL POSTSUM USING POSTSUM-PARM                                      
010440     .                                                                    
