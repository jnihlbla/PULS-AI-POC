000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF100200.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   NOV 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -MATCHES TEMP. ACCOUNT-TABLE AGAINST PERMANENT ACCOUNT-TABLE          
001910*   -CREATES FILE CONTAINING                                              
002000*    1 NEW ACCOUNTS                                                       
002401*    2 DELETED ACCOUNTS                                                   
002402*    3 CHANGED ACCOUNTS                                                   
002403*    4 UNCHANGED ACCOUNTS                                                 
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003020*          --- LOADING-DATA                                               
003030     SELECT WF1007                     ASSIGN TO WF1002D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  WF1007                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY WF10M10 -PRE UT-   -L.                                    
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF100200'.            
004300 77  WS-COUNT                    PIC S9(9)   VALUE ZERO COMP-3.           
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01ACCT                       
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004670 01  WS-IDKONTO                  PIC X(10).                               
004680 01  WS-IDGL                     PIC X(4).                                
004686     EJECT                                                                
004687                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006506                                                                          
006542 01  WF1007-AREA-START           PIC X(24)   VALUE                        
006543                                             'WF10-AREA-START'.           
006544*01  -COPY WF10M10              -PRE UT-                                  
006545*                                                                         
006546     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS1    '.        
010605*01  -COPY T01ACCT    -PRE ACCT-                                          
010606                                                                          
010607 01  FILLER                       PIC X(16)   VALUE 'DB2-WS2    '.        
010608*01  -COPY T01ACCT    -PRE ACCW-                                          
010611                                                                          
010612 01  FILLER                       PIC X(16)   VALUE 'ACCT-AREA'.          
010613       EXEC SQL INCLUDE T01ACCT  END-EXEC.                                
010615                                                                          
010616 01  FILLER                       PIC X(16)   VALUE 'ACCW-AREA'.          
010617       EXEC SQL INCLUDE T01ACCW  END-EXEC.                                
010618     EJECT                                                                
010619                                                                          
010620 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
010621       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010622*                        **** STATUS-CODE FROM DB2                        
010623                                                                          
010624 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010625 01  DB2-WS.                                                              
010626   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010627     88  ROW-FOUND                            VALUE +000.                 
010628     88  ROW-MISSING                          VALUE +100.                 
010629   03  GOOD-SQLCODES.                                                     
010630     05  GOOD-SQLCODE OCCURS 5                                            
010631         INDEXED BY SQLCODE-IX    PIC 999.                                
010632     EJECT                                                                
010640                                                                          
010717 PROCEDURE DIVISION.                                                      
010718                                                                          
010719 MAIN SECTION.                                                            
010720     ENTRY 'DLITCBL'.                                                     
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     IF WS-COUNT > ZERO                                                   
011720       PERFORM B-EXECUTE                                                  
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
014040     OPEN OUTPUT WF1007                                                   
014041                                                                          
014050     MOVE ZERO         TO WS-COUNT                                        
014060     PERFORM DB2-OPEN-CRS-EMPTY                                           
014070     PERFORM DB2-FETCH-CRS-EMPTY                                          
014080     PERFORM UNTIL ROW-MISSING                                            
014090       ADD 1           TO WS-COUNT                                        
014091       PERFORM DB2-FETCH-CRS-EMPTY                                        
014092     END-PERFORM                                                          
014093     PERFORM DB2-CLOSE-CRS-EMPTY                                          
014094                                                                          
014095     IF WS-COUNT = ZERO                                                   
014096       PERFORM DB2-OPEN-CRS-COPY                                          
014097       PERFORM DB2-FETCH-CRS-COPY                                         
014098       PERFORM UNTIL ROW-MISSING                                          
014099         PERFORM AA-BUILD-OUTPUT-COPY                                     
014100         PERFORM S11-WRITE-WF1007                                         
014101         PERFORM DB2-FETCH-CRS-COPY                                       
014102       END-PERFORM                                                        
014103       PERFORM DB2-CLOSE-CRS-COPY                                         
014104     END-IF                                                               
014110     .                                                                    
014200     EJECT                                                                
014210                                                                          
014220 AA-BUILD-OUTPUT-COPY SECTION.                                            
014230     MOVE ACCT-IDLEGSEL           TO UT-IDLEGSEL                          
014240     MOVE ACCT-IDKONTO            TO UT-IDKONTO                           
014250     MOVE ACCT-IDGL               TO UT-IDGL                              
014260     MOVE ACCT-FLKST              TO UT-FLKST                             
014270     MOVE ACCT-FLANALYS           TO UT-FLANALYS                          
014280     MOVE ACCT-DAREGDAT           TO UT-DAREGDAT                          
014290     MOVE ACCT-DAUPPDAT           TO UT-DAUPPDAT                          
014300     MOVE ACCT-DADELDAT           TO UT-DADELDAT                          
014400     .                                                                    
014500     EJECT                                                                
014600                                                                          
014985 B-EXECUTE SECTION.                                                       
015011     PERFORM DB2-OPEN-CRS-DEL                                             
015025     PERFORM DB2-FETCH-CRS-DEL                                            
015026     PERFORM UNTIL ROW-MISSING                                            
015027       PERFORM BB-BUILD-OUTPUT-DEL                                        
015028       PERFORM S11-WRITE-WF1007                                           
015030       PERFORM DB2-FETCH-CRS-DEL                                          
015031     END-PERFORM                                                          
015040     PERFORM DB2-CLOSE-CRS-DEL                                            
015041                                                                          
015042     PERFORM DB2-OPEN-CRS-CHA                                             
015050     PERFORM DB2-FETCH-CRS-CHA                                            
015060     PERFORM UNTIL ROW-MISSING                                            
015070       PERFORM BC-BUILD-OUTPUT-CHA                                        
015080       PERFORM S11-WRITE-WF1007                                           
015090       PERFORM DB2-FETCH-CRS-CHA                                          
015091     END-PERFORM                                                          
015093     PERFORM DB2-CLOSE-CRS-CHA                                            
015094                                                                          
015095     PERFORM DB2-OPEN-CRS-UNCHA                                           
015096     PERFORM DB2-FETCH-CRS-UNCHA                                          
015097     PERFORM UNTIL ROW-MISSING                                            
015098       PERFORM BD-BUILD-OUTPUT-UNCHA                                      
015099       PERFORM S11-WRITE-WF1007                                           
015100       PERFORM DB2-FETCH-CRS-UNCHA                                        
015101     END-PERFORM                                                          
015102     PERFORM DB2-CLOSE-CRS-UNCHA                                          
015103                                                                          
015104     PERFORM DB2-OPEN-CRS-NEW                                             
015105     PERFORM DB2-FETCH-CRS-NEW                                            
015106     PERFORM UNTIL ROW-MISSING                                            
015107       PERFORM BA-BUILD-OUTPUT-NEW                                        
015108       PERFORM S11-WRITE-WF1007                                           
015109       PERFORM DB2-FETCH-CRS-NEW                                          
015110     END-PERFORM                                                          
015111     PERFORM DB2-CLOSE-CRS-NEW                                            
015113     .                                                                    
015114     EJECT                                                                
015115                                                                          
015116 BA-BUILD-OUTPUT-NEW SECTION.                                             
015117     MOVE ACCW-IDLEGSEL           TO UT-IDLEGSEL                          
015118     MOVE ACCW-IDKONTO            TO UT-IDKONTO                           
015119     MOVE ACCW-IDGL               TO UT-IDGL                              
015120     MOVE ACCW-FLKST              TO UT-FLKST                             
015121     MOVE ACCW-FLANALYS           TO UT-FLANALYS                          
015122     MOVE ACCW-DAREGDAT           TO UT-DAREGDAT                          
015123     MOVE ZERO                    TO UT-DAUPPDAT                          
015130                                     UT-DADELDAT                          
015144     .                                                                    
015145     EJECT                                                                
015146                                                                          
015160 BB-BUILD-OUTPUT-DEL SECTION.                                             
015161     MOVE ACCT-IDLEGSEL           TO UT-IDLEGSEL                          
015163     MOVE ACCT-IDKONTO            TO UT-IDKONTO                           
015164     MOVE ACCT-IDGL               TO UT-IDGL                              
015165     MOVE ACCT-FLKST              TO UT-FLKST                             
015166     MOVE ACCT-FLANALYS           TO UT-FLANALYS                          
015167     MOVE ACCT-DAREGDAT           TO UT-DAREGDAT                          
015168     MOVE ACCT-DAUPPDAT           TO UT-DAUPPDAT                          
015169     MOVE ACCW-DADELDAT           TO UT-DADELDAT                          
015170     .                                                                    
015171     EJECT                                                                
015172                                                                          
015173 BC-BUILD-OUTPUT-CHA SECTION.                                             
015174     MOVE ACCT-IDLEGSEL           TO UT-IDLEGSEL                          
015175     MOVE ACCT-IDKONTO            TO UT-IDKONTO                           
015176     MOVE ACCT-IDGL               TO UT-IDGL                              
015177     MOVE ACCW-FLKST              TO UT-FLKST                             
015178     MOVE ACCW-FLANALYS           TO UT-FLANALYS                          
015179     MOVE ACCT-DAREGDAT           TO UT-DAREGDAT                          
015180     MOVE ACCW-DAUPPDAT           TO UT-DAUPPDAT                          
015181     MOVE ACCT-DADELDAT           TO UT-DADELDAT                          
015182     .                                                                    
015183     EJECT                                                                
015184                                                                          
015185 BD-BUILD-OUTPUT-UNCHA SECTION.                                           
015186     MOVE ACCT-IDLEGSEL           TO UT-IDLEGSEL                          
015187     MOVE ACCT-IDKONTO            TO UT-IDKONTO                           
015188     MOVE ACCT-IDGL               TO UT-IDGL                              
015189     MOVE ACCT-FLKST              TO UT-FLKST                             
015190     MOVE ACCT-FLANALYS           TO UT-FLANALYS                          
015191     MOVE ACCT-DAREGDAT           TO UT-DAREGDAT                          
015192     MOVE ACCT-DAUPPDAT           TO UT-DAUPPDAT                          
015193     MOVE ACCT-DADELDAT           TO UT-DADELDAT                          
015194     .                                                                    
015195     EJECT                                                                
015196                                                                          
015197 Z-FINISH SECTION.                                                        
015198     CLOSE WF1007                                                         
015202     .                                                                    
015203     EJECT                                                                
015204                                                                          
015205 S11-WRITE-WF1007 SECTION.                                                
015206     WRITE UT-POST   FROM UT-WF10M10                                      
015212     .                                                                    
015213     EJECT                                                                
015214                                                                          
015215* --- DB2 SECTIONS  ---                                                   
015216*                                                                         
015217 DB2-OPEN-CRS-EMPTY SECTION.                                              
015218     EXEC SQL DECLARE EMPTY-CRS CURSOR FOR                                
015219     SELECT   T01ACCW.IDLEGSEL                                            
015220                                                                          
015221     FROM     T01ACCW                                                     
015222                                                                          
015223     FOR FETCH ONLY                                                       
015224     END-EXEC                                                             
015225                                                                          
015226     MOVE 000            TO GOOD-SQLCODES                                 
015227     EXEC SQL OPEN EMPTY-CRS                                              
015228     END-EXEC                                                             
015229                                                                          
015230     MOVE SQLCODE        TO SQLCODE-WS                                    
015231     PERFORM DB2-STATUS-CHECK                                             
015232     .                                                                    
015233     EJECT                                                                
015234                                                                          
015235 DB2-OPEN-CRS-NEW SECTION.                                                
015236     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
015237     SELECT   T01ACCW.IDLEGSEL,                                           
015238              T01ACCW.IDKONTO,                                            
015239              T01ACCW.IDGL,                                               
015240              T01ACCW.FLKST,                                              
015241              T01ACCW.FLANALYS,                                           
015242              T01ACCW.DAREGDAT                                            
015250                                                                          
015300     FROM     T01ACCW                                                     
015400                                                                          
015600     WHERE    NOT EXISTS                                                  
015800                                                                          
015900             (SELECT *                                                    
016000              FROM   T01ACCT                                              
016010              WHERE  T01ACCT.IDLEGSEL = T01ACCW.IDLEGSEL AND              
016020                     T01ACCT.IDKONTO  = T01ACCW.IDKONTO  AND              
016030                     T01ACCT.IDGL     = T01ACCW.IDGL)        AND          
016040                                                                          
016050              T01ACCW.DADELDAT = '00000000'                               
016100                                                                          
016300     ORDER BY T01ACCW.IDLEGSEL,                                           
016410              T01ACCW.IDKONTO,                                            
016411              T01ACCW.IDGL                                                
016412                                                                          
016420     FOR FETCH ONLY                                                       
016500     END-EXEC                                                             
016600                                                                          
016700     MOVE 000            TO GOOD-SQLCODES                                 
016800     EXEC SQL OPEN NEW-CRS                                                
016810     END-EXEC                                                             
016900     MOVE SQLCODE        TO SQLCODE-WS                                    
017000     PERFORM DB2-STATUS-CHECK                                             
017100     .                                                                    
017200     EJECT                                                                
017210                                                                          
017320 DB2-OPEN-CRS-DEL SECTION.                                                
017322     EXEC SQL DECLARE DEL-CRS CURSOR FOR                                  
017323     SELECT   T01ACCT.IDLEGSEL,                                           
017325              T01ACCT.IDKONTO,                                            
017326              T01ACCT.IDGL,                                               
017327              T01ACCT.FLKST,                                              
017328              T01ACCT.FLANALYS,                                           
017329              T01ACCT.DAREGDAT,                                           
017330              T01ACCT.DAUPPDAT,                                           
017331              T01ACCW.DADELDAT                                            
017332                                                                          
017333     FROM     T01ACCW,                                                    
017334              T01ACCT                                                     
017335                                                                          
017336     WHERE    T01ACCW.IDLEGSEL =  T01ACCT.IDLEGSEL AND                    
017338              T01ACCW.IDKONTO  =  T01ACCT.IDKONTO  AND                    
017339              T01ACCW.IDGL     =  T01ACCT.IDGL     AND                    
017340             (T01ACCW.DADELDAT > '00000000'        AND                    
017341              T01ACCT.DADELDAT = '00000000')                              
017342                                                                          
017343     ORDER BY T01ACCT.IDLEGSEL,                                           
017345              T01ACCT.IDKONTO,                                            
017346              T01ACCT.IDGL                                                
017347                                                                          
017348     FOR FETCH ONLY                                                       
017349     END-EXEC                                                             
017350                                                                          
017351     MOVE 000            TO GOOD-SQLCODES                                 
017352     EXEC SQL OPEN DEL-CRS                                                
017353     END-EXEC                                                             
017354     MOVE SQLCODE        TO SQLCODE-WS                                    
017355     PERFORM DB2-STATUS-CHECK                                             
017356     .                                                                    
017357     EJECT                                                                
017360                                                                          
017370 DB2-OPEN-CRS-CHA SECTION.                                                
017381     EXEC SQL DECLARE CHA-CRS CURSOR FOR                                  
017382     SELECT   T01ACCT.IDLEGSEL,                                           
017383              T01ACCT.IDKONTO,                                            
017384              T01ACCT.IDGL,                                               
017385              T01ACCW.FLKST,                                              
017386              T01ACCW.FLANALYS,                                           
017387              T01ACCT.DAREGDAT,                                           
017388              T01ACCW.DAUPPDAT,                                           
017389              T01ACCT.DADELDAT                                            
017390                                                                          
017391     FROM     T01ACCW,                                                    
017392              T01ACCT                                                     
017393                                                                          
017394     WHERE    T01ACCW.IDLEGSEL = T01ACCT.IDLEGSEL    AND                  
017395              T01ACCW.IDKONTO  = T01ACCT.IDKONTO     AND                  
017396              T01ACCW.IDGL     = T01ACCT.IDGL        AND                  
017397         (NOT T01ACCW.FLKST    = T01ACCT.FLKST    OR                      
017398          NOT T01ACCW.FLANALYS = T01ACCT.FLANALYS)                        
017399                                                                          
017400     ORDER BY T01ACCT.IDLEGSEL,                                           
017401              T01ACCT.IDKONTO,                                            
017402              T01ACCT.IDGL                                                
017403                                                                          
017404     FOR FETCH ONLY                                                       
017405     END-EXEC                                                             
017406                                                                          
017407     MOVE 000            TO GOOD-SQLCODES                                 
017408     EXEC SQL OPEN CHA-CRS                                                
017409     END-EXEC                                                             
017410     MOVE SQLCODE        TO SQLCODE-WS                                    
017411     PERFORM DB2-STATUS-CHECK                                             
017412     .                                                                    
017413     EJECT                                                                
017414                                                                          
017415 DB2-OPEN-CRS-UNCHA SECTION.                                              
017417     EXEC SQL DECLARE UNCHA-CRS CURSOR FOR                                
017418     SELECT   T01ACCT.IDLEGSEL,                                           
017419              T01ACCT.IDKONTO,                                            
017420              T01ACCT.IDGL,                                               
017421              T01ACCT.FLKST,                                              
017422              T01ACCT.FLANALYS,                                           
017423              T01ACCT.DAREGDAT,                                           
017424              T01ACCT.DAUPPDAT,                                           
017425              T01ACCT.DADELDAT                                            
017426                                                                          
017427     FROM     T01ACCW,                                                    
017428              T01ACCT                                                     
017429                                                                          
017430     WHERE    T01ACCW.IDLEGSEL = T01ACCT.IDLEGSEL    AND                  
017431              T01ACCW.IDKONTO  = T01ACCT.IDKONTO     AND                  
017432              T01ACCW.IDGL     = T01ACCT.IDGL        AND                  
017433              T01ACCW.FLKST    = T01ACCT.FLKST       AND                  
017434              T01ACCW.FLANALYS = T01ACCT.FLANALYS    AND                  
017435              T01ACCW.DADELDAT = '00000000'                               
017438                                                                          
017439     ORDER BY T01ACCT.IDLEGSEL,                                           
017440              T01ACCT.IDKONTO,                                            
017441              T01ACCT.IDGL                                                
017442                                                                          
017443     FOR FETCH ONLY                                                       
017444     END-EXEC                                                             
017445                                                                          
017446     MOVE 000            TO GOOD-SQLCODES                                 
017447     EXEC SQL OPEN UNCHA-CRS                                              
017448     END-EXEC                                                             
017449     MOVE SQLCODE        TO SQLCODE-WS                                    
017450     PERFORM DB2-STATUS-CHECK                                             
017451     .                                                                    
017452     EJECT                                                                
017453                                                                          
017454 DB2-OPEN-CRS-COPY SECTION.                                               
017455     EXEC SQL DECLARE COPY-CRS CURSOR FOR                                 
017456     SELECT   T01ACCT.IDLEGSEL,                                           
017457              T01ACCT.IDKONTO,                                            
017458              T01ACCT.IDGL,                                               
017459              T01ACCT.FLKST,                                              
017460              T01ACCT.FLANALYS,                                           
017461              T01ACCT.DAREGDAT,                                           
017462              T01ACCT.DAUPPDAT,                                           
017463              T01ACCT.DADELDAT                                            
017464                                                                          
017466     FROM     T01ACCT                                                     
017467                                                                          
017475     ORDER BY T01ACCT.IDLEGSEL,                                           
017476              T01ACCT.IDKONTO,                                            
017477              T01ACCT.IDGL                                                
017478                                                                          
017479     FOR FETCH ONLY                                                       
017480     END-EXEC                                                             
017481                                                                          
017482     MOVE 000            TO GOOD-SQLCODES                                 
017483     EXEC SQL OPEN COPY-CRS                                               
017484     END-EXEC                                                             
017485     MOVE SQLCODE        TO SQLCODE-WS                                    
017486     PERFORM DB2-STATUS-CHECK                                             
017487     .                                                                    
017488     EJECT                                                                
017489                                                                          
017490 DB2-FETCH-CRS-EMPTY SECTION.                                             
017491     EXEC SQL FETCH EMPTY-CRS INTO                                        
017492            :ACCW-IDLEGSEL                                                
017493     END-EXEC                                                             
017494                                                                          
017495     MOVE 000100         TO GOOD-SQLCODES                                 
017496     MOVE SQLCODE        TO SQLCODE-WS                                    
017497     PERFORM DB2-STATUS-CHECK                                             
017498     .                                                                    
017499     EJECT                                                                
017500                                                                          
017501 DB2-FETCH-CRS-NEW SECTION.                                               
017510     EXEC SQL FETCH NEW-CRS INTO                                          
017600            :ACCW-IDLEGSEL,                                               
017701            :ACCW-IDKONTO,                                                
017702            :ACCW-IDGL,                                                   
017710            :ACCW-FLKST,                                                  
017720            :ACCW-FLANALYS,                                               
017800            :ACCW-DAREGDAT                                                
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019030 DB2-FETCH-CRS-DEL SECTION.                                               
019032     EXEC SQL FETCH DEL-CRS INTO                                          
019033            :ACCT-IDLEGSEL,                                               
019035            :ACCT-IDKONTO,                                                
019036            :ACCT-IDGL,                                                   
019037            :ACCT-FLKST,                                                  
019038            :ACCT-FLANALYS,                                               
019041            :ACCT-DAREGDAT,                                               
019042            :ACCT-DAUPPDAT,                                               
019043            :ACCW-DADELDAT                                                
019044     END-EXEC                                                             
019045                                                                          
019046     MOVE 000100         TO GOOD-SQLCODES                                 
019047     MOVE SQLCODE        TO SQLCODE-WS                                    
019048     PERFORM DB2-STATUS-CHECK                                             
019049     .                                                                    
019050     EJECT                                                                
019051                                                                          
019052 DB2-FETCH-CRS-CHA SECTION.                                               
019054     EXEC SQL FETCH CHA-CRS INTO                                          
019055            :ACCT-IDLEGSEL,                                               
019056            :ACCT-IDKONTO,                                                
019057            :ACCT-IDGL,                                                   
019058            :ACCW-FLKST,                                                  
019059            :ACCW-FLANALYS,                                               
019060            :ACCT-DAREGDAT,                                               
019061            :ACCW-DAUPPDAT,                                               
019062            :ACCT-DADELDAT                                                
019063     END-EXEC                                                             
019064                                                                          
019065     MOVE 000100         TO GOOD-SQLCODES                                 
019066     MOVE SQLCODE        TO SQLCODE-WS                                    
019067     PERFORM DB2-STATUS-CHECK                                             
019068     .                                                                    
019069     EJECT                                                                
019070                                                                          
019071 DB2-FETCH-CRS-UNCHA SECTION.                                             
019073     EXEC SQL FETCH UNCHA-CRS INTO                                        
019074            :ACCT-IDLEGSEL,                                               
019075            :ACCT-IDKONTO,                                                
019076            :ACCT-IDGL,                                                   
019077            :ACCT-FLKST,                                                  
019078            :ACCT-FLANALYS,                                               
019079            :ACCT-DAREGDAT,                                               
019080            :ACCT-DAUPPDAT,                                               
019081            :ACCT-DADELDAT                                                
019082     END-EXEC                                                             
019083                                                                          
019084     MOVE 000100         TO GOOD-SQLCODES                                 
019085     MOVE SQLCODE        TO SQLCODE-WS                                    
019086     PERFORM DB2-STATUS-CHECK                                             
019087     .                                                                    
019088     EJECT                                                                
019089                                                                          
019090 DB2-FETCH-CRS-COPY SECTION.                                              
019091     EXEC SQL FETCH COPY-CRS INTO                                         
019092            :ACCT-IDLEGSEL,                                               
019093            :ACCT-IDKONTO,                                                
019094            :ACCT-IDGL,                                                   
019095            :ACCT-FLKST,                                                  
019096            :ACCT-FLANALYS,                                               
019097            :ACCT-DAREGDAT,                                               
019098            :ACCT-DAUPPDAT,                                               
019099            :ACCT-DADELDAT                                                
019100     END-EXEC                                                             
019101                                                                          
019102     MOVE 000100         TO GOOD-SQLCODES                                 
019103     MOVE SQLCODE        TO SQLCODE-WS                                    
019104     PERFORM DB2-STATUS-CHECK                                             
019105     .                                                                    
019106     EJECT                                                                
019107                                                                          
019108 DB2-CLOSE-CRS-EMPTY SECTION.                                             
019109     EXEC SQL CLOSE EMPTY-CRS                                             
019110     END-EXEC                                                             
019111     .                                                                    
019112     EJECT                                                                
019113                                                                          
019114 DB2-CLOSE-CRS-NEW SECTION.                                               
019115     EXEC SQL CLOSE NEW-CRS                                               
019116     END-EXEC                                                             
019117     .                                                                    
019118     EJECT                                                                
019119                                                                          
019120 DB2-CLOSE-CRS-DEL SECTION.                                               
019121     EXEC SQL CLOSE DEL-CRS                                               
019122     END-EXEC                                                             
019123     .                                                                    
019124     EJECT                                                                
019125                                                                          
019126 DB2-CLOSE-CRS-CHA SECTION.                                               
019127     EXEC SQL CLOSE CHA-CRS                                               
019128     END-EXEC                                                             
019129     .                                                                    
019130     EJECT                                                                
019131                                                                          
019132 DB2-CLOSE-CRS-UNCHA SECTION.                                             
019133     EXEC SQL CLOSE UNCHA-CRS                                             
019134     END-EXEC                                                             
019135     .                                                                    
019136     EJECT                                                                
019137                                                                          
019138 DB2-CLOSE-CRS-COPY SECTION.                                              
019139     EXEC SQL CLOSE COPY-CRS                                              
019140     END-EXEC                                                             
019141     .                                                                    
019142     EJECT                                                                
019143                                                                          
019144 DB2-STATUS-CHECK SECTION.                                                
019150     SET SQLCODE-IX         TO 1                                          
019200     SEARCH GOOD-SQLCODE AT END                                           
019210           CALL ABEND USING RKOD-ABEND-DB2                                
019300        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
