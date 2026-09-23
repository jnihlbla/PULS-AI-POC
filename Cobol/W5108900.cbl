000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5108900.                                                
000301 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   2022/02/10.                                              
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*                                                                         
000801*    FUNCTION:                                                            
000901*        SALES REPORT FOR AU AND JP                                       
001001*                                                                         
001101*                                                                         
001201*    ABENDCODES:                                                          
001301*        U0016 -  . . . .                                                 
001401*        U1000 -  . . . .                                                 
001501*                                                                         
001601                                                                          
001701     SKIP3                                                                
001801 ENVIRONMENT DIVISION.                                                    
001901     SKIP2                                                                
002001 INPUT-OUTPUT SECTION.                                                    
002101                                                                          
002201 FILE-CONTROL.                                                            
002301     SKIP2                                                                
002401*          --- BOOKING EVENTS                                             
002501     SELECT W51066                     ASSIGN TO W51089D1.                
002601     SKIP2                                                                
002701*          --- SALES EVENTS FILE                                          
002801     SELECT W51089                     ASSIGN TO W51089D2.                
002901     EJECT                                                                
003001 DATA DIVISION.                                                           
003101     SKIP3                                                                
003201 FILE SECTION.                                                            
003301     SKIP3                                                                
003401 FD  W51066                                                               
003501     RECORDING       F                                                    
003601     BLOCK CONTAINS  0.                                                   
003701                                                                          
003702 01  IN-POST.                                                             
003703*    03   -COPY WDR901    -L.                                             
003704     03 FILLER                   PIC X(6).                                
003801     SKIP3                                                                
004001 FD  W51089                                                               
004101     RECORDING       V                                                    
004201     BLOCK CONTAINS  0.                                                   
004301 01  W51089-001              PIC X(999).                                  
004401     SKIP3                                                                
004501 WORKING-STORAGE SECTION.                                                 
004601                                                                          
004701 77  IDPGM                       PIC X(8)    VALUE 'W5108900'.            
004801 77  YES                         PIC X       VALUE 'J'.                   
004901 77  NOO                         PIC X       VALUE 'N'.                   
005001                                                                          
005002 77  FIRST-ROW-SW                PIC X       VALUE 'J'.                   
005003     88 FIRST-LINE                           VALUE 'J'.                   
005101 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005102 77  WS-PREV-IDDC                PIC X(2)    VALUE SPACE.                 
005201 77  W51066-EOF-SW               PIC X       VALUE 'N'.                   
005301     88  END-OF-W51066                       VALUE 'J'.                   
005401 01  TODAYS-DATE                 PIC 9(6)    VALUE 0.                     
005501 01  FILLER REDEFINES TODAYS-DATE.                                        
005601     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005701     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005801     03  TODAYS-DATE-DAY         PIC 9(2).                                
005901     EJECT                                                                
006001 01  GENERAL-SUBPROGRAMS.                                                 
006101*                                                                         
006201     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006301     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006401     SKIP2                                                                
006501*    --- PARAMETERS TO ABEND                                              
006601                                                                          
006701 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006801 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006901 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007001     SKIP2                                                                
007101 01  ERROR-TEXT.                                                          
007201     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007301     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007401     EJECT                                                                
007501*    --- PARAMETRAR TILL POSTSUM                                          
007601*                                                                         
007701*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007901 01  IN-AREA-START               PIC X(24)   VALUE                        
008001                                 'IN-AREA-START  '.                       
008101     SKIP2                                                                
008201                                                                          
008301 01  W001-DAP.                                                            
008401     03  FILLER              PIC X(165)  VALUE SPACE.                     
008501                                                                          
008601*01  AREA -COPY WDR901     -PRE IN-                                       
008701*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR901-DATA               
008801     EJECT                                                                
008802*01  -COPY WWDC99                                                         
008803     EJECT                                                                
008901 01  TEXT-AREA.                                                           
009001     03  HEAD-LINE.                                                       
009101         05  FILLER          PIC X(11)   VALUE                            
009201            'PART NUMBER'.                                                
009301         05  FILLER          PIC X(1)    VALUE ';'.                       
009401         05  FILLER          PIC X(8)    VALUE                            
009501            'PACKAGES'.                                                   
009601         05  FILLER          PIC X(1)    VALUE ';'.                       
009701         05  FILLER          PIC X(5)    VALUE                            
009801            'PRICE'.                                                      
009901         05  FILLER          PIC X(1)    VALUE ';'.                       
010001         05  FILLER          PIC X(8)    VALUE                            
010101            'QUANTITY'.                                                   
010201         05  FILLER          PIC X(1)    VALUE ';'.                       
010301         05  FILLER          PIC X(12)   VALUE                            
010401            'TOTAL AMOUNT'.                                               
010501         05  FILLER          PIC X(1)    VALUE ';'.                       
010601         05  FILLER          PIC X(21)   VALUE                            
010701            'INVOICE/CREDIT NUMBER'.                                      
010801         05  FILLER          PIC X(1)    VALUE ';'.                       
010901         05  FILLER          PIC X(10)   VALUE                            
011001            'MAIN EVENT'.                                                 
011101         05  FILLER          PIC X(1)    VALUE ';'.                       
011201         05  FILLER          PIC X(9)    VALUE                            
011301            'SUB EVENT'.                                                  
011401         05  FILLER          PIC X(1)    VALUE ';'.                       
011501         05  FILLER          PIC X(11)   VALUE                            
011601            'EVENT LEVEL'.                                                
011701         05  FILLER          PIC X(1)    VALUE ';'.                       
011801         05  FILLER          PIC X(11)   VALUE                            
011901            'POSTING KEY'.                                                
012000         05  FILLER          PIC X(1)    VALUE ';'.                       
012100         05  FILLER          PIC X(12)   VALUE                            
012200            'ORDER NUMBER'.                                               
012300         05  FILLER          PIC X(1)    VALUE ';'.                       
012400         05  FILLER          PIC X(11)   VALUE                            
012500            'CUSTOMER NO'.                                                
012600         05  FILLER          PIC X(1)    VALUE ';'.                       
012700         05  FILLER          PIC X(11)   VALUE                            
012800            'DISTRICT NO'.                                                
012900         05  FILLER          PIC X(1)    VALUE ';'.                       
013001         05  FILLER          PIC X(11)   VALUE                            
013101            'PRODUCT GRP'.                                                
013201         05  FILLER          PIC X(1)    VALUE ';'.                       
013301         05  FILLER          PIC X(13)   VALUE                            
013401            'DELIVERY DATE'.                                              
013501         05  FILLER          PIC X(1)    VALUE ';'.                       
013502         05  FILLER          PIC X(2)    VALUE                            
013503            'DC'.                                                         
013504         05  FILLER          PIC X(1)    VALUE ';'.                       
013600                                                                          
013700     03 ROW-LINE.                                                         
013801*        05   -COPY W51089                                                
013900     EJECT                                                                
014000 PROCEDURE DIVISION.                                                      
014100 MAIN SECTION.                                                            
014200     SKIP2                                                                
014300                                                                          
014400     PERFORM A-INIT                                                       
014501     PERFORM S01-READ-W51066                                              
014601     PERFORM UNTIL END-OF-W51066                                          
014602       MOVE IN-EKH-IDDC-SEND TO WS-IDDC                                   
014604       IF (NDC-JP OR NDC-AU)                                              
014700         PERFORM B-PROCESS-EVENTS                                         
014802       END-IF                                                             
014804       PERFORM S01-READ-W51066                                            
014805       IF IN-EKH-IDDC-SEND  =  WS-IDDC                                    
014806         CONTINUE                                                         
014807       ELSE                                                               
014808         MOVE YES TO FIRST-ROW-SW                                         
014809       END-IF                                                             
014900     END-PERFORM                                                          
015000                                                                          
015100                                                                          
015200     PERFORM Z-FINIT                                                      
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900                                                                          
016001     OPEN INPUT  W51066                                                   
016100                                                                          
016201     OPEN OUTPUT W51089                                                   
016300     SKIP2                                                                
016400     ACCEPT TODAYS-DATE  FROM DATE                                        
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600     .                                                                    
016700     EJECT                                                                
016800 B-PROCESS-EVENTS SECTION.                                                
016900                                                                          
017000     IF ((IN-EKH-KDEKHHT = '203'                                          
017100     AND  IN-EKH-KDEKSHT = '201')                                         
017200     OR  (IN-EKH-KDEKHHT = '204'                                          
017300     AND  IN-EKH-KDEKSHT = '201')                                         
017400     OR  (IN-EKH-KDEKHHT = '303'                                          
017500     AND  IN-EKH-KDEKSHT = '311')                                         
017600     OR  (IN-EKH-KDEKHHT = '303'                                          
017700     AND  IN-EKH-KDEKSHT = '391'))                                        
017800       EVALUATE IN-EKH-KDEKNIVA                                           
017801         WHEN 'DET'                                                       
017802           MOVE IN-EKH-PRARTSTD TO UT-PRARTSTD                            
017901           COMPUTE UT-SUBEL = IN-EKH-PRARTSTD * IN-EKH-KVANTAL            
018001           IF IN-EKH-KDEKHHT = '303'                                      
018101           AND IN-EKH-KDEKSHT = '391'                                     
018201             MOVE '-'          TO UT-KDTECKEN                             
018301           ELSE                                                           
018401             MOVE '+'          TO UT-KDTECKEN                             
018501           END-IF                                                         
018600           PERFORM BA-MOVE-FIELDS                                         
018610       END-EVALUATE                                                       
018700     END-IF                                                               
018800                                                                          
018901     IF IN-EKH-KDEKHHT = '302'                                            
019001     AND IN-EKH-KDEKSHT = '302'                                           
019101       MOVE IN-EKH-PRARTSTD    TO UT-PRARTSTD                             
019201       COMPUTE UT-SUBEL = IN-EKH-PRARTSTD * IN-EKH-KVANTAL                
019301       IF IN-FIL-IDPGM = 'W4079700'                                       
019401         MOVE '+'              TO UT-KDTECKEN                             
019501       ELSE                                                               
019601         MOVE '-'              TO UT-KDTECKEN                             
019701       END-IF                                                             
019801       PERFORM BA-MOVE-FIELDS                                             
019901     END-IF                                                               
019902                                                                          
019940                                                                          
021200     .                                                                    
021300     EJECT                                                                
021400 BA-MOVE-FIELDS SECTION.                                                  
021600     IF FIRST-LINE                                                        
021902       PERFORM S02-SKRIV-DAP1-S                                           
022001       PERFORM S03-SKRIV-DAP2-S                                           
022101       WRITE W51089-001 FROM HEAD-LINE                                    
022102       MOVE NOO TO FIRST-ROW-SW                                           
022103     ELSE                                                                 
022104       CONTINUE                                                           
022200     END-IF                                                               
022300     MOVE IN-EKH-IDARTNR   TO UT-IDARTNR                                  
022310     MOVE IN-EKH-IDDC-SEND TO UT-IDDC                                     
022400     MOVE IN-EKH-IDDISTR   TO UT-IDDISTR                                  
022500     MOVE IN-EKH-IDKUNDNR  TO UT-IDKUNDNR                                 
022600     MOVE IN-EKH-IDVERGL   TO UT-IDVERGL                                  
022700     MOVE IN-EKH-KDEKHHT   TO UT-KDEKHHT                                  
022800     MOVE IN-EKH-KDEKSHT   TO UT-KDEKSHT                                  
022900     MOVE IN-EKH-KDEKNIVA  TO UT-KDEKNIVA                                 
023000     MOVE IN-EKH-KVANTAL   TO UT-KVANTAL                                  
023100     MOVE IN-EKH-IDORDNR5  TO UT-IDORDNR5                                 
023201     MOVE IN-EKH-KDPRODSL  TO UT-KDPRODSL                                 
023301     MOVE IN-EKH-DAVERDAT  TO UT-DAVERDAT                                 
023400     MOVE ZERO             TO UT-IDKOLLI                                  
023500     MOVE ';'              TO UT-SEMICOLON-1                              
023600                              UT-SEMICOLON-2                              
023700                              UT-SEMICOLON-3                              
023800                              UT-SEMICOLON-4                              
023900                              UT-SEMICOLON-5                              
024000                              UT-SEMICOLON-6                              
024100                              UT-SEMICOLON-7                              
024200                              UT-SEMICOLON-8                              
024300                              UT-SEMICOLON-9                              
024400                              UT-SEMICOLON-10                             
024500                              UT-SEMICOLON-11                             
024600                              UT-SEMICOLON-12                             
024701                              UT-SEMICOLON-13                             
024801                              UT-SEMICOLON-14                             
024901                              UT-SEMICOLON-15                             
024902                              UT-SEMICOLON-16                             
025101     WRITE W51089-001 FROM ROW-LINE                                       
025200     .                                                                    
025300     EJECT                                                                
025400 Z-FINIT SECTION.                                                         
025501     CLOSE W51066                                                         
025601           W51089                                                         
025700     SKIP2                                                                
025800     MOVE 'S' TO POSTSUM-OPKOD                                            
025901     CALL   POSTSUM USING POSTSUM-PARM                                    
026000     .                                                                    
026100     EJECT                                                                
026201 S01-READ-W51066  SECTION.                                                
026301     READ W51066 INTO IN-AREA                                             
026400     AT END                                                               
026500        MOVE HIGH-VALUE TO IN-AREA                                        
026601        SET END-OF-W51066 TO TRUE                                         
026700                                                                          
026800     NOT AT END                                                           
026901        MOVE 'W51066' TO POSTSUM-FDNAMN                                   
027001        MOVE 'W51089D1' TO POSTSUM-DDNAMN2                                
027100        MOVE SPACES    TO POSTSUM-TRANSTYP                                
027201        CALL   POSTSUM USING POSTSUM-PARM                                 
027300     END-READ                                                             
027400     .                                                                    
027500     EJECT                                                                
027600 S02-SKRIV-DAP1-S SECTION.                                                
027700                                                                          
027801     MOVE ' ¤DAPW51089-001' TO W001-DAP                                   
027901     WRITE W51089-001    FROM W001-DAP                                    
028000                                                                          
028100     MOVE SPACE TO W001-DAP                                               
028200     .                                                                    
028300                                                                          
028400 S03-SKRIV-DAP2-S SECTION.                                                
028500                                                                          
028600     STRING ' ¤DAP' WS-IDDC                                               
028700            DELIMITED BY SIZE INTO W001-DAP                               
028801     WRITE W51089-001    FROM W001-DAP                                    
028900                                                                          
029000     MOVE SPACE TO W001-DAP                                               
030000     .                                                                    
