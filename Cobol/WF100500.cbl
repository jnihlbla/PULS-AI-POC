000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF100500.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   NOV 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -MATCHES TEMP. VAT-TABLE AGAINST PERMANENT VAT-TABLE                  
001910*   -CREATES FILE CONTAINING                                              
002000*    1 NEW VAT-CODES                                                      
002401*    2 DELETED VAT-CODES                                                  
002402*    3 CHANGED VAT-CODES                                                  
002403*    4 UNCHANGED VAT-CODES                                                
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003020*          --- LOADING-DATA                                               
003030     SELECT WF1010                     ASSIGN TO WF1005D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  WF1010                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY WF10M17 -PRE UT-   -L.                                    
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF100500'.            
004300 77  WS-COUNT                    PIC S9(9)   VALUE ZERO COMP-3.           
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01VAT                        
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004669 01  WS-IDLAND                   PIC X(2).                                
004670 01  WS-KDVAT                    PIC X(2).                                
004686     EJECT                                                                
004687                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006501     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006502                                                                          
006503*    --- PARAMETRAR TILL DATKORT                                          
006504*                                                                         
006506 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006507     EJECT                                                                
006508                                                                          
006509 01  WS-DATUM.                                                            
006510     03  WS-DAGENS-DATUM         PIC 9(8).                                
006511                                                                          
006512*    --- PARAMETRAR TILL DATKORT                                          
006513*                                                                         
006514 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006515                                                                          
006516*01  -COPY WDATKORT                                                       
006540     EJECT                                                                
006541                                                                          
006546 01  WF1010-AREA-START           PIC X(24)   VALUE                        
006547                                             'WF10-AREA-START'.           
006548*01  -COPY WF10M17              -PRE UT-                                  
006549*                                                                         
006550     EJECT                                                                
006560*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS-VAT '.        
010605*01  -COPY T01VAT     -PRE VAT-                                           
010607                                                                          
010608 01  FILLER                       PIC X(16)   VALUE 'DB2-WS-VATW'.        
010609*01  -COPY T01VAT     -PRE VATW-                                          
010610     EJECT                                                                
010611                                                                          
010612 01  FILLER                       PIC X(16)   VALUE 'VAT-AREA   '.        
010613       EXEC SQL INCLUDE T01VAT  END-EXEC.                                 
010614                                                                          
010615 01  FILLER                       PIC X(16)   VALUE 'VATW-AREA  '.        
010616       EXEC SQL INCLUDE T01VATW END-EXEC.                                 
010617                                                                          
010618 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
010619       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010620*                        **** STATUS-CODE FROM DB2                        
010621                                                                          
010622 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010623 01  DB2-WS.                                                              
010624   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010625     88  ROW-FOUND                            VALUE +000.                 
010626     88  ROW-MISSING                          VALUE +100.                 
010627   03  GOOD-SQLCODES.                                                     
010628     05  GOOD-SQLCODE OCCURS 5                                            
010629         INDEXED BY SQLCODE-IX    PIC 999.                                
010630     EJECT                                                                
010640                                                                          
010717 PROCEDURE DIVISION.                                                      
010718                                                                          
010719 MAIN SECTION.                                                            
010720     ENTRY 'DLITCBL'.                                                     
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     IF WS-COUNT > ZERO                                                   
011710       PERFORM B-EXECUTE                                                  
011800     END-IF                                                               
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
013000 A-INIT SECTION.                                                          
014040     OPEN OUTPUT WF1010                                                   
014041                                                                          
014042     CALL DATKORT USING IDPGM       DATUMKORT-ID DATUMKORT                
014043     MOVE 20           TO WS-DAGENS-DATUM(1:2)                            
014044     MOVE D-AAR        TO WS-DAGENS-DATUM(3:2)                            
014045     MOVE D-MAANAD     TO WS-DAGENS-DATUM(5:2)                            
014046     MOVE D-DAG        TO WS-DAGENS-DATUM(7:2)                            
014047                                                                          
014048     MOVE ZERO         TO WS-COUNT                                        
014049     PERFORM DB2-OPEN-CRS-EMPTY                                           
014050     PERFORM DB2-FETCH-CRS-EMPTY                                          
014060     PERFORM UNTIL ROW-MISSING                                            
014070       ADD 1           TO WS-COUNT                                        
014080       PERFORM DB2-FETCH-CRS-EMPTY                                        
014090     END-PERFORM                                                          
014091     PERFORM DB2-CLOSE-CRS-EMPTY                                          
014092                                                                          
014093     IF WS-COUNT = ZERO                                                   
014094       PERFORM DB2-OPEN-CRS-COPY                                          
014095       PERFORM DB2-FETCH-CRS-COPY                                         
014096       PERFORM UNTIL ROW-MISSING                                          
014097         PERFORM AA-BUILD-OUTPUT-COPY                                     
014098         PERFORM S11-WRITE-WF1010                                         
014099         PERFORM DB2-FETCH-CRS-COPY                                       
014100       END-PERFORM                                                        
014101       PERFORM DB2-CLOSE-CRS-COPY                                         
014102     END-IF                                                               
014110     .                                                                    
014200     EJECT                                                                
014210                                                                          
014220 AA-BUILD-OUTPUT-COPY SECTION.                                            
014230     MOVE VAT-IDLEGSEL            TO UT-IDLEGSEL                          
014240     MOVE VAT-IDLANDX2            TO UT-IDLAND                            
014250     MOVE VAT-KDVAT               TO UT-KDVAT                             
014260     MOVE VAT-REVAT               TO UT-REVAT                             
014270     MOVE VAT-BEVAT               TO UT-BEVAT                             
014280     MOVE VAT-DAREGDAT            TO UT-DAREGDAT                          
014290     MOVE VAT-DAUPPDAT            TO UT-DAUPPDAT                          
014300     MOVE VAT-DADELDAT            TO UT-DADELDAT                          
014400     .                                                                    
014500     EJECT                                                                
014600                                                                          
014985 B-EXECUTE SECTION.                                                       
015023     PERFORM DB2-OPEN-CRS-DEL                                             
015025     PERFORM DB2-FETCH-CRS-DEL                                            
015026     PERFORM UNTIL ROW-MISSING                                            
015027       PERFORM BB-BUILD-OUTPUT-DEL                                        
015028       PERFORM S11-WRITE-WF1010                                           
015030       PERFORM DB2-FETCH-CRS-DEL                                          
015031     END-PERFORM                                                          
015040     PERFORM DB2-CLOSE-CRS-DEL                                            
015041                                                                          
015050     PERFORM DB2-OPEN-CRS-CHA                                             
015060     PERFORM DB2-FETCH-CRS-CHA                                            
015070     PERFORM UNTIL ROW-MISSING                                            
015080       PERFORM BC-BUILD-OUTPUT-CHA                                        
015090       PERFORM S11-WRITE-WF1010                                           
015091       PERFORM DB2-FETCH-CRS-CHA                                          
015092     END-PERFORM                                                          
015093     PERFORM DB2-CLOSE-CRS-CHA                                            
015094                                                                          
015095     PERFORM DB2-OPEN-CRS-UNCHA                                           
015096     PERFORM DB2-FETCH-CRS-UNCHA                                          
015097     PERFORM UNTIL ROW-MISSING                                            
015098       PERFORM BD-BUILD-OUTPUT-UNCHA                                      
015099       PERFORM S11-WRITE-WF1010                                           
015100       PERFORM DB2-FETCH-CRS-UNCHA                                        
015101     END-PERFORM                                                          
015102     PERFORM DB2-CLOSE-CRS-UNCHA                                          
015103                                                                          
015104     PERFORM DB2-OPEN-CRS-NEW                                             
015105     PERFORM DB2-FETCH-CRS-NEW                                            
015106     PERFORM UNTIL ROW-MISSING                                            
015107       PERFORM BA-BUILD-OUTPUT-NEW                                        
015108       PERFORM S11-WRITE-WF1010                                           
015109       PERFORM DB2-FETCH-CRS-NEW                                          
015110     END-PERFORM                                                          
015111     PERFORM DB2-CLOSE-CRS-NEW                                            
015121     .                                                                    
015122     EJECT                                                                
015123                                                                          
015124 BA-BUILD-OUTPUT-NEW SECTION.                                             
015125     MOVE VATW-IDLEGSEL           TO UT-IDLEGSEL                          
015126     MOVE VATW-IDLANDX2           TO UT-IDLAND                            
015127     MOVE VATW-KDVAT              TO UT-KDVAT                             
015128     MOVE VATW-REVAT              TO UT-REVAT                             
015129     MOVE VATW-BEVAT              TO UT-BEVAT                             
015130     MOVE VATW-DAREGDAT           TO UT-DAREGDAT                          
015131     MOVE ZERO                    TO UT-DAUPPDAT                          
015140                                     UT-DADELDAT                          
015144     .                                                                    
015145     EJECT                                                                
015146                                                                          
015170 BB-BUILD-OUTPUT-DEL SECTION.                                             
015171* DELETE OF VATCODES CAN ONLY BE DONE IN BILL-IT WEB                      
015172     MOVE VAT-IDLEGSEL           TO UT-IDLEGSEL                           
015173     MOVE VAT-IDLANDX2           TO UT-IDLAND                             
015174     MOVE VAT-KDVAT              TO UT-KDVAT                              
015175     MOVE VAT-REVAT              TO UT-REVAT                              
015176     MOVE VAT-BEVAT              TO UT-BEVAT                              
015187     MOVE VAT-DAREGDAT           TO UT-DAREGDAT                           
015188     MOVE VAT-DAUPPDAT           TO UT-DAUPPDAT                           
015189     MOVE VAT-DADELDAT           TO UT-DADELDAT                           
015190     .                                                                    
015191     EJECT                                                                
015192                                                                          
015193 BC-BUILD-OUTPUT-CHA SECTION.                                             
015194     MOVE VAT-IDLEGSEL            TO UT-IDLEGSEL                          
015195     MOVE VAT-IDLANDX2            TO UT-IDLAND                            
015196     MOVE VAT-KDVAT               TO UT-KDVAT                             
015198     MOVE VATW-REVAT              TO UT-REVAT                             
015199     MOVE VATW-BEVAT              TO UT-BEVAT                             
015210     MOVE VAT-DAREGDAT            TO UT-DAREGDAT                          
015211     MOVE VATW-DAUPPDAT           TO UT-DAUPPDAT                          
015212     MOVE VAT-DADELDAT            TO UT-DADELDAT                          
015213     .                                                                    
015214     EJECT                                                                
015215                                                                          
015216 BD-BUILD-OUTPUT-UNCHA SECTION.                                           
015218     MOVE VAT-IDLEGSEL            TO UT-IDLEGSEL                          
015219     MOVE VAT-IDLANDX2            TO UT-IDLAND                            
015220     MOVE VAT-KDVAT               TO UT-KDVAT                             
015221     MOVE VAT-REVAT               TO UT-REVAT                             
015222     MOVE VAT-BEVAT               TO UT-BEVAT                             
015236     MOVE VAT-DAREGDAT            TO UT-DAREGDAT                          
015238     MOVE VAT-DAUPPDAT            TO UT-DAUPPDAT                          
015239     MOVE VAT-DADELDAT            TO UT-DADELDAT                          
015240     .                                                                    
015241     EJECT                                                                
015242                                                                          
015255 Z-FINISH SECTION.                                                        
015256     CLOSE WF1010                                                         
015257     .                                                                    
015258     EJECT                                                                
015259                                                                          
015260 S11-WRITE-WF1010 SECTION.                                                
015261     WRITE UT-POST   FROM UT-WF10M17                                      
015262     .                                                                    
015263     EJECT                                                                
015264                                                                          
015265* --- DB2 SECTIONS  ---                                                   
015266*                                                                         
015267 DB2-OPEN-CRS-EMPTY SECTION.                                              
015268     EXEC SQL DECLARE EMPTY-CRS CURSOR FOR                                
015269     SELECT   T01VATW.IDLEGSEL                                            
015280                                                                          
015300     FROM     T01VATW                                                     
016411                                                                          
016420     FOR FETCH ONLY                                                       
016500     END-EXEC                                                             
016600                                                                          
016700     MOVE 000            TO GOOD-SQLCODES                                 
016800     EXEC SQL OPEN EMPTY-CRS                                              
016810     END-EXEC                                                             
016820                                                                          
016900     MOVE SQLCODE        TO SQLCODE-WS                                    
017000     PERFORM DB2-STATUS-CHECK                                             
017100     .                                                                    
017200     EJECT                                                                
017210                                                                          
017220 DB2-OPEN-CRS-NEW SECTION.                                                
017230     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
017240     SELECT   T01VATW.IDLEGSEL,                                           
017250              T01VATW.IDLANDX2,                                           
017260              T01VATW.KDVAT,                                              
017270              T01VATW.REVAT,                                              
017280              T01VATW.BEVAT,                                              
017290              T01VATW.DAREGDAT                                            
017300                                                                          
017310     FROM     T01VATW                                                     
017311                                                                          
017312     WHERE    NOT EXISTS                                                  
017313                                                                          
017314             (SELECT *                                                    
017315              FROM   T01VAT                                               
017316              WHERE  T01VAT.IDLEGSEL = T01VATW.IDLEGSEL AND               
017317                     T01VAT.IDLANDX2 = T01VATW.IDLANDX2 AND               
017318                     T01VAT.KDVAT    = T01VATW.KDVAT)                     
017319                                                                          
017320     ORDER BY T01VATW.IDLEGSEL,                                           
017321              T01VATW.IDLANDX2,                                           
017322              T01VATW.KDVAT                                               
017323                                                                          
017324     FOR FETCH ONLY                                                       
017325     END-EXEC                                                             
017326                                                                          
017327     MOVE 000            TO GOOD-SQLCODES                                 
017328     EXEC SQL OPEN NEW-CRS                                                
017329     END-EXEC                                                             
017330     MOVE SQLCODE        TO SQLCODE-WS                                    
017331     PERFORM DB2-STATUS-CHECK                                             
017332     .                                                                    
017333     EJECT                                                                
017334                                                                          
017335 DB2-OPEN-CRS-DEL SECTION.                                                
017336     EXEC SQL DECLARE DEL-CRS CURSOR FOR                                  
017337     SELECT   T01VAT.IDLEGSEL,                                            
017338              T01VAT.IDLANDX2,                                            
017339              T01VAT.KDVAT,                                               
017340              T01VAT.REVAT,                                               
017341              T01VAT.BEVAT,                                               
017342              T01VAT.DAREGDAT,                                            
017343              T01VAT.DAUPPDAT,                                            
017344              T01VAT.DADELDAT                                             
017345                                                                          
017346     FROM     T01VAT                                                      
017347                                                                          
017348     WHERE    T01VAT.DADELDAT = '00000000'                                
017349     AND      NOT EXISTS                                                  
017350                                                                          
017351             (SELECT *                                                    
017352              FROM   T01VATW                                              
017353              WHERE  T01VATW.IDLEGSEL = T01VAT.IDLEGSEL AND               
017354                     T01VATW.IDLANDX2 = T01VAT.IDLANDX2 AND               
017355                     T01VATW.KDVAT    = T01VAT.KDVAT)                     
017356                                                                          
017357     ORDER BY T01VAT.IDLEGSEL,                                            
017358              T01VAT.IDLANDX2,                                            
017359              T01VAT.KDVAT                                                
017360                                                                          
017361     FOR FETCH ONLY                                                       
017362     END-EXEC                                                             
017363                                                                          
017364     MOVE 000            TO GOOD-SQLCODES                                 
017365     EXEC SQL OPEN DEL-CRS                                                
017366     END-EXEC                                                             
017367     MOVE SQLCODE        TO SQLCODE-WS                                    
017368     PERFORM DB2-STATUS-CHECK                                             
017369     .                                                                    
017370     EJECT                                                                
017371                                                                          
017380 DB2-OPEN-CRS-CHA SECTION.                                                
017382     EXEC SQL DECLARE CHA-CRS CURSOR FOR                                  
017383     SELECT   T01VAT.IDLEGSEL,                                            
017384              T01VAT.IDLANDX2,                                            
017385              T01VAT.KDVAT,                                               
017386              T01VATW.REVAT,                                              
017387              T01VATW.BEVAT,                                              
017388              T01VAT.DAREGDAT,                                            
017389              T01VATW.DAUPPDAT,                                           
017390              T01VAT.DADELDAT                                             
017391                                                                          
017392     FROM     T01VATW,                                                    
017393              T01VAT                                                      
017394                                                                          
017395     WHERE    T01VATW.IDLEGSEL     = T01VAT.IDLEGSEL    AND               
017396              T01VATW.IDLANDX2     = T01VAT.IDLANDX2    AND               
017397              T01VATW.KDVAT        = T01VAT.KDVAT       AND               
017398         (NOT T01VATW.REVAT        = T01VAT.REVAT   OR                    
017399          NOT T01VATW.BEVAT        = T01VAT.BEVAT)                        
017400                                                                          
017401     ORDER BY T01VAT.IDLEGSEL,                                            
017402              T01VAT.IDLANDX2,                                            
017403              T01VAT.KDVAT                                                
017404                                                                          
017405     FOR FETCH ONLY                                                       
017406     END-EXEC                                                             
017407                                                                          
017408     MOVE 000            TO GOOD-SQLCODES                                 
017409     EXEC SQL OPEN CHA-CRS                                                
017410     END-EXEC                                                             
017411     MOVE SQLCODE        TO SQLCODE-WS                                    
017412     PERFORM DB2-STATUS-CHECK                                             
017413     .                                                                    
017414     EJECT                                                                
017415                                                                          
017416 DB2-OPEN-CRS-UNCHA SECTION.                                              
017417     EXEC SQL DECLARE UNCHA-CRS CURSOR FOR                                
017418     SELECT   T01VAT.IDLEGSEL,                                            
017419              T01VAT.IDLANDX2,                                            
017420              T01VAT.KDVAT,                                               
017421              T01VAT.REVAT,                                               
017422              T01VAT.BEVAT,                                               
017423              T01VAT.DAREGDAT,                                            
017424              T01VAT.DAUPPDAT,                                            
017425              T01VAT.DADELDAT                                             
017426                                                                          
017427     FROM     T01VATW,                                                    
017428              T01VAT                                                      
017429                                                                          
017432     WHERE    T01VATW.IDLEGSEL     = T01VAT.IDLEGSEL    AND               
017433              T01VATW.IDLANDX2     = T01VAT.IDLANDX2    AND               
017434              T01VATW.KDVAT        = T01VAT.KDVAT       AND               
017435              T01VATW.REVAT        = T01VAT.REVAT       AND               
017436              T01VATW.BEVAT        = T01VAT.BEVAT                         
017441                                                                          
017442     ORDER BY T01VAT.IDLEGSEL,                                            
017443              T01VAT.IDLANDX2,                                            
017444              T01VAT.KDVAT                                                
017445                                                                          
017446     FOR FETCH ONLY                                                       
017447     END-EXEC                                                             
017448                                                                          
017449     MOVE 000            TO GOOD-SQLCODES                                 
017450     EXEC SQL OPEN UNCHA-CRS                                              
017451     END-EXEC                                                             
017452     MOVE SQLCODE        TO SQLCODE-WS                                    
017453     PERFORM DB2-STATUS-CHECK                                             
017454     .                                                                    
017455     EJECT                                                                
017456                                                                          
017457 DB2-OPEN-CRS-COPY SECTION.                                               
017458     EXEC SQL DECLARE COPY-CRS CURSOR FOR                                 
017459     SELECT   T01VAT.IDLEGSEL,                                            
017460              T01VAT.IDLANDX2,                                            
017461              T01VAT.KDVAT,                                               
017462              T01VAT.REVAT,                                               
017463              T01VAT.BEVAT,                                               
017464              T01VAT.DAREGDAT,                                            
017465              T01VAT.DAUPPDAT,                                            
017466              T01VAT.DADELDAT                                             
017467                                                                          
017469     FROM     T01VAT                                                      
017470                                                                          
017477     ORDER BY T01VAT.IDLEGSEL,                                            
017478              T01VAT.IDLANDX2,                                            
017479              T01VAT.KDVAT                                                
017480                                                                          
017481     FOR FETCH ONLY                                                       
017482     END-EXEC                                                             
017483                                                                          
017484     MOVE 000            TO GOOD-SQLCODES                                 
017485     EXEC SQL OPEN COPY-CRS                                               
017486     END-EXEC                                                             
017487     MOVE SQLCODE        TO SQLCODE-WS                                    
017488     PERFORM DB2-STATUS-CHECK                                             
017489     .                                                                    
017490     EJECT                                                                
017491                                                                          
017528 DB2-FETCH-CRS-EMPTY SECTION.                                             
017530     EXEC SQL FETCH EMPTY-CRS INTO                                        
017600            :VATW-IDLEGSEL                                                
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019012 DB2-FETCH-CRS-NEW SECTION.                                               
019013     EXEC SQL FETCH NEW-CRS INTO                                          
019014            :VATW-IDLEGSEL,                                               
019015            :VATW-IDLANDX2,                                               
019016            :VATW-KDVAT,                                                  
019017            :VATW-REVAT,                                                  
019018            :VATW-BEVAT,                                                  
019019            :VATW-DAREGDAT                                                
019020     END-EXEC                                                             
019021                                                                          
019022     MOVE 000100         TO GOOD-SQLCODES                                 
019023     MOVE SQLCODE        TO SQLCODE-WS                                    
019024     PERFORM DB2-STATUS-CHECK                                             
019025     .                                                                    
019026     EJECT                                                                
019027                                                                          
019030 DB2-FETCH-CRS-DEL SECTION.                                               
019032     EXEC SQL FETCH DEL-CRS INTO                                          
019033            :VAT-IDLEGSEL,                                                
019034            :VAT-IDLANDX2,                                                
019035            :VAT-KDVAT,                                                   
019036            :VAT-REVAT,                                                   
019037            :VAT-BEVAT,                                                   
019038            :VAT-DAREGDAT,                                                
019039            :VAT-DAUPPDAT,                                                
019040            :VAT-DADELDAT                                                 
019041     END-EXEC                                                             
019042                                                                          
019043     MOVE 000100         TO GOOD-SQLCODES                                 
019044     MOVE SQLCODE        TO SQLCODE-WS                                    
019045     PERFORM DB2-STATUS-CHECK                                             
019046     .                                                                    
019047     EJECT                                                                
019048                                                                          
019049 DB2-FETCH-CRS-CHA SECTION.                                               
019050     EXEC SQL FETCH CHA-CRS INTO                                          
019051            :VAT-IDLEGSEL,                                                
019052            :VAT-IDLANDX2,                                                
019053            :VAT-KDVAT,                                                   
019054            :VATW-REVAT,                                                  
019055            :VATW-BEVAT,                                                  
019056            :VAT-DAREGDAT,                                                
019057            :VATW-DAUPPDAT,                                               
019058            :VAT-DADELDAT                                                 
019059     END-EXEC                                                             
019060                                                                          
019061     MOVE 000100         TO GOOD-SQLCODES                                 
019062     MOVE SQLCODE        TO SQLCODE-WS                                    
019063     PERFORM DB2-STATUS-CHECK                                             
019064     .                                                                    
019065     EJECT                                                                
019066                                                                          
019067 DB2-FETCH-CRS-UNCHA SECTION.                                             
019068     EXEC SQL FETCH UNCHA-CRS INTO                                        
019069            :VAT-IDLEGSEL,                                                
019070            :VAT-IDLANDX2,                                                
019071            :VAT-KDVAT,                                                   
019072            :VAT-REVAT,                                                   
019073            :VAT-BEVAT,                                                   
019074            :VAT-DAREGDAT,                                                
019075            :VAT-DAUPPDAT,                                                
019076            :VAT-DADELDAT                                                 
019077     END-EXEC                                                             
019078                                                                          
019079     MOVE 000100         TO GOOD-SQLCODES                                 
019080     MOVE SQLCODE        TO SQLCODE-WS                                    
019081     PERFORM DB2-STATUS-CHECK                                             
019082     .                                                                    
019083     EJECT                                                                
019084                                                                          
019085 DB2-FETCH-CRS-COPY SECTION.                                              
019086     EXEC SQL FETCH COPY-CRS INTO                                         
019087            :VAT-IDLEGSEL,                                                
019088            :VAT-IDLANDX2,                                                
019089            :VAT-KDVAT,                                                   
019090            :VAT-REVAT,                                                   
019091            :VAT-BEVAT,                                                   
019092            :VAT-DAREGDAT,                                                
019093            :VAT-DAUPPDAT,                                                
019094            :VAT-DADELDAT                                                 
019095     END-EXEC                                                             
019096                                                                          
019097     MOVE 000100         TO GOOD-SQLCODES                                 
019098     MOVE SQLCODE        TO SQLCODE-WS                                    
019099     PERFORM DB2-STATUS-CHECK                                             
019100     .                                                                    
019101     EJECT                                                                
019102                                                                          
019121 DB2-CLOSE-CRS-EMPTY SECTION.                                             
019122     EXEC SQL CLOSE EMPTY-CRS                                             
019123     END-EXEC                                                             
019124     .                                                                    
019125     EJECT                                                                
019126                                                                          
019127 DB2-CLOSE-CRS-NEW SECTION.                                               
019128     EXEC SQL CLOSE NEW-CRS                                               
019129     END-EXEC                                                             
019130     .                                                                    
019131     EJECT                                                                
019132                                                                          
019133 DB2-CLOSE-CRS-DEL SECTION.                                               
019134     EXEC SQL CLOSE DEL-CRS                                               
019135     END-EXEC                                                             
019136     .                                                                    
019137     EJECT                                                                
019138                                                                          
019139 DB2-CLOSE-CRS-CHA SECTION.                                               
019140     EXEC SQL CLOSE CHA-CRS                                               
019141     END-EXEC                                                             
019142     .                                                                    
019143     EJECT                                                                
019144                                                                          
019145 DB2-CLOSE-CRS-UNCHA SECTION.                                             
019146     EXEC SQL CLOSE UNCHA-CRS                                             
019147     END-EXEC                                                             
019148     .                                                                    
019149     EJECT                                                                
019150                                                                          
019151 DB2-CLOSE-CRS-COPY SECTION.                                              
019152     EXEC SQL CLOSE COPY-CRS                                              
019153     END-EXEC                                                             
019154     .                                                                    
019155     EJECT                                                                
019156                                                                          
019163 DB2-STATUS-CHECK SECTION.                                                
019170     SET SQLCODE-IX         TO 1                                          
019200     SEARCH GOOD-SQLCODE AT END                                           
019210           CALL ABEND USING RKOD-ABEND-DB2                                
019300        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
