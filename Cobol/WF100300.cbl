000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF100300.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   NOV 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -MATCHES TEMP. COSTCTR-TABLE AGAINST PERMANENT COSTCTR-TABLE          
001910*   -CREATES FILE CONTAINING                                              
002000*    1 NEW COST CENTERS                                                   
002401*    2 DELETED COST CENTERS                                               
002402*    3 UNCHANGED COST CENTERS                                             
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003020*          --- LOADING-DATA                                               
003030     SELECT WF1008                     ASSIGN TO WF1003D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  WF1008                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY WF10M11 -PRE UT-   -L.                                    
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF100300'.            
004300 77  WS-COUNT                    PIC S9(9)   VALUE ZERO COMP-3.           
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01CCTR                       
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004670 01  WS-IDKST                    PIC X(10).                               
004680 01  WS-IDGL                     PIC X(4).                                
004686     EJECT                                                                
004687                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006502                                                                          
006542 01  WF1008-AREA-START           PIC X(24)   VALUE                        
006543                                             'WF10-AREA-START'.           
006544*01  -COPY WF10M11              -PRE UT-                                  
006545*                                                                         
006546     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
010605*01  -COPY T01CCTR    -PRE CCTR-                                          
010607                                                                          
010608 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
010609*01  -COPY T01CCTW    -PRE CCTW-                                          
010611                                                                          
010612 01  FILLER                       PIC X(16)   VALUE 'CST-AREA   '.        
010613       EXEC SQL INCLUDE T01CCTR END-EXEC.                                 
010614                                                                          
010615 01  FILLER                       PIC X(16)   VALUE 'CSTW-AREA  '.        
010616       EXEC SQL INCLUDE T01CCTW END-EXEC.                                 
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
014040     OPEN OUTPUT WF1008                                                   
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
014100         PERFORM AA-BUILD-OUTPUT-COPY                                     
014101         PERFORM S11-WRITE-WF1008                                         
014102         PERFORM DB2-FETCH-CRS-COPY                                       
014103       END-PERFORM                                                        
014104       PERFORM DB2-CLOSE-CRS-COPY                                         
014105     END-IF                                                               
014110     .                                                                    
014200     EJECT                                                                
014210                                                                          
014220 AA-BUILD-OUTPUT-COPY SECTION.                                            
014230     MOVE CCTR-IDLEGSEL           TO UT-IDLEGSEL                          
014240     MOVE CCTR-IDKST              TO UT-IDKST                             
014250     MOVE CCTR-IDGL               TO UT-IDGL                              
014260     MOVE CCTR-DAREGDAT           TO UT-DAREGDAT                          
014270     MOVE CCTR-DADELDAT           TO UT-DADELDAT                          
014280     .                                                                    
014290     EJECT                                                                
014300                                                                          
014985 B-EXECUTE SECTION.                                                       
015023     PERFORM DB2-OPEN-CRS-DEL                                             
015025     PERFORM DB2-FETCH-CRS-DEL                                            
015026     PERFORM UNTIL ROW-MISSING                                            
015027       PERFORM BB-BUILD-OUTPUT-DEL                                        
015028       PERFORM S11-WRITE-WF1008                                           
015030       PERFORM DB2-FETCH-CRS-DEL                                          
015031     END-PERFORM                                                          
015040     PERFORM DB2-CLOSE-CRS-DEL                                            
015050                                                                          
015051     PERFORM DB2-OPEN-CRS-UNCHA                                           
015060     PERFORM DB2-FETCH-CRS-UNCHA                                          
015070     PERFORM UNTIL ROW-MISSING                                            
015080       PERFORM BC-BUILD-OUTPUT-UNCHA                                      
015090       PERFORM S11-WRITE-WF1008                                           
015092       PERFORM DB2-FETCH-CRS-UNCHA                                        
015093     END-PERFORM                                                          
015095     PERFORM DB2-CLOSE-CRS-UNCHA                                          
015096                                                                          
015097     PERFORM DB2-OPEN-CRS-NEW                                             
015098     PERFORM DB2-FETCH-CRS-NEW                                            
015099     PERFORM UNTIL ROW-MISSING                                            
015100       PERFORM BA-BUILD-OUTPUT-NEW                                        
015101       PERFORM S11-WRITE-WF1008                                           
015102       PERFORM DB2-FETCH-CRS-NEW                                          
015103     END-PERFORM                                                          
015104     PERFORM DB2-CLOSE-CRS-NEW                                            
015106     .                                                                    
015107     EJECT                                                                
015108                                                                          
015109 BA-BUILD-OUTPUT-NEW SECTION.                                             
015110     MOVE CCTW-IDLEGSEL         TO UT-IDLEGSEL                            
015111     MOVE CCTW-IDKST            TO UT-IDKST                               
015112     MOVE CCTW-IDGL             TO UT-IDGL                                
015113     MOVE CCTW-DAREGDAT         TO UT-DAREGDAT                            
015115     MOVE ZERO                  TO UT-DADELDAT                            
015144     .                                                                    
015145     EJECT                                                                
015146                                                                          
015147 BB-BUILD-OUTPUT-DEL SECTION.                                             
015148     MOVE CCTR-IDLEGSEL          TO UT-IDLEGSEL                           
015149     MOVE CCTR-IDKST             TO UT-IDKST                              
015150     MOVE CCTR-IDGL              TO UT-IDGL                               
015151     MOVE CCTR-DAREGDAT          TO UT-DAREGDAT                           
015152     MOVE CCTW-DADELDAT          TO UT-DADELDAT                           
015153     .                                                                    
015154     EJECT                                                                
015155                                                                          
015156 BC-BUILD-OUTPUT-UNCHA SECTION.                                           
015157     MOVE CCTR-IDLEGSEL           TO UT-IDLEGSEL                          
015158     MOVE CCTR-IDKST              TO UT-IDKST                             
015159     MOVE CCTR-IDGL               TO UT-IDGL                              
015160     MOVE CCTR-DAREGDAT           TO UT-DAREGDAT                          
015161     MOVE CCTR-DADELDAT           TO UT-DADELDAT                          
015162     .                                                                    
015163     EJECT                                                                
015164                                                                          
015165 Z-FINISH SECTION.                                                        
015166     CLOSE WF1008                                                         
015170     .                                                                    
015171     EJECT                                                                
015172                                                                          
015173 S11-WRITE-WF1008 SECTION.                                                
015174     WRITE UT-POST   FROM UT-WF10M11                                      
015180     .                                                                    
015181     EJECT                                                                
015182                                                                          
015183* --- DB2 SECTIONS  ---                                                   
015184*                                                                         
015185 DB2-OPEN-CRS-EMPTY SECTION.                                              
015186     EXEC SQL DECLARE EMPTY-CRS CURSOR FOR                                
015187     SELECT   T01CCTW.IDLEGSEL                                            
015188                                                                          
015189     FROM     T01CCTW                                                     
015190                                                                          
015191     FOR FETCH ONLY                                                       
015192     END-EXEC                                                             
015193                                                                          
015194     MOVE 000            TO GOOD-SQLCODES                                 
015195     EXEC SQL OPEN EMPTY-CRS                                              
015196     END-EXEC                                                             
015197                                                                          
015198     MOVE SQLCODE        TO SQLCODE-WS                                    
015199     PERFORM DB2-STATUS-CHECK                                             
015200     .                                                                    
015201     EJECT                                                                
015202                                                                          
015203 DB2-OPEN-CRS-NEW SECTION.                                                
015204     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
015205     SELECT   T01CCTW.IDLEGSEL,                                           
015206              T01CCTW.IDKST,                                              
015207              T01CCTW.IDGL,                                               
015208              T01CCTW.DAREGDAT                                            
015210                                                                          
015300     FROM     T01CCTW                                                     
015400                                                                          
015600     WHERE    NOT EXISTS                                                  
015800                                                                          
015900             (SELECT *                                                    
016000              FROM   T01CCTR                                              
016010              WHERE  T01CCTR.IDLEGSEL = T01CCTW.IDLEGSEL AND              
016020                     T01CCTR.IDKST    = T01CCTW.IDKST    AND              
016030                     T01CCTR.IDGL     = T01CCTW.IDGL)        AND          
016040                                                                          
016100              T01CCTW.DADELDAT        = '00000000'                        
016200                                                                          
016300     ORDER BY T01CCTW.IDLEGSEL,                                           
016410              T01CCTW.IDKST,                                              
016411              T01CCTW.IDGL                                                
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
017319 DB2-OPEN-CRS-DEL SECTION.                                                
017321     EXEC SQL DECLARE DEL-CRS CURSOR FOR                                  
017322     SELECT   T01CCTR.IDLEGSEL,                                           
017324              T01CCTR.IDKST,                                              
017325              T01CCTR.IDGL,                                               
017326              T01CCTR.DAREGDAT,                                           
017327              T01CCTW.DADELDAT                                            
017328                                                                          
017329     FROM     T01CCTW,                                                    
017330              T01CCTR                                                     
017331                                                                          
017332     WHERE    T01CCTW.IDLEGSEL  =  T01CCTR.IDLEGSEL     AND               
017333              T01CCTW.IDGL      =  T01CCTR.IDGL         AND               
017334              T01CCTW.IDKST     =  T01CCTR.IDKST        AND               
017335             (T01CCTW.DADELDAT  > '00000000'     AND                      
017336              T01CCTR.DADELDAT  = '00000000')                             
017337                                                                          
017338     ORDER BY T01CCTR.IDLEGSEL,                                           
017340              T01CCTR.IDKST,                                              
017341              T01CCTR.IDGL                                                
017342                                                                          
017343     FOR FETCH ONLY                                                       
017344     END-EXEC                                                             
017345                                                                          
017346     MOVE 000            TO GOOD-SQLCODES                                 
017347     EXEC SQL OPEN DEL-CRS                                                
017348     END-EXEC                                                             
017349     MOVE SQLCODE        TO SQLCODE-WS                                    
017350     PERFORM DB2-STATUS-CHECK                                             
017351     .                                                                    
017352     EJECT                                                                
017360                                                                          
017370 DB2-OPEN-CRS-UNCHA SECTION.                                              
017381     EXEC SQL DECLARE UNCHA-CRS CURSOR FOR                                
017382     SELECT   T01CCTR.IDLEGSEL,                                           
017383              T01CCTR.IDKST,                                              
017384              T01CCTR.IDGL,                                               
017387              T01CCTR.DAREGDAT,                                           
017389              T01CCTR.DADELDAT                                            
017390                                                                          
017391     FROM     T01CCTW,                                                    
017392              T01CCTR                                                     
017393                                                                          
017394     WHERE    T01CCTW.IDLEGSEL  = T01CCTR.IDLEGSEL    AND                 
017395              T01CCTW.IDKST     = T01CCTR.IDKST       AND                 
017396              T01CCTW.IDGL      = T01CCTR.IDGL        AND                 
017398              T01CCTW.DADELDAT  = '00000000'                              
017399                                                                          
017400     ORDER BY T01CCTR.IDLEGSEL,                                           
017401              T01CCTR.IDKST,                                              
017402              T01CCTR.IDGL                                                
017403                                                                          
017404     FOR FETCH ONLY                                                       
017405     END-EXEC                                                             
017406                                                                          
017407     MOVE 000            TO GOOD-SQLCODES                                 
017408     EXEC SQL OPEN UNCHA-CRS                                              
017409     END-EXEC                                                             
017410     MOVE SQLCODE        TO SQLCODE-WS                                    
017411     PERFORM DB2-STATUS-CHECK                                             
017412     .                                                                    
017413     EJECT                                                                
017414                                                                          
017415 DB2-OPEN-CRS-COPY SECTION.                                               
017416     EXEC SQL DECLARE COPY-CRS CURSOR FOR                                 
017417     SELECT   T01CCTR.IDLEGSEL,                                           
017418              T01CCTR.IDKST,                                              
017419              T01CCTR.IDGL,                                               
017420              T01CCTR.DAREGDAT,                                           
017421              T01CCTR.DADELDAT                                            
017422                                                                          
017424     FROM     T01CCTR                                                     
017425                                                                          
017431     ORDER BY T01CCTR.IDLEGSEL,                                           
017432              T01CCTR.IDKST,                                              
017433              T01CCTR.IDGL                                                
017434                                                                          
017435     FOR FETCH ONLY                                                       
017436     END-EXEC                                                             
017437                                                                          
017438     MOVE 000            TO GOOD-SQLCODES                                 
017439     EXEC SQL OPEN COPY-CRS                                               
017440     END-EXEC                                                             
017441     MOVE SQLCODE        TO SQLCODE-WS                                    
017442     PERFORM DB2-STATUS-CHECK                                             
017443     .                                                                    
017444     EJECT                                                                
017445                                                                          
017446 DB2-FETCH-CRS-EMPTY SECTION.                                             
017447     EXEC SQL FETCH EMPTY-CRS INTO                                        
017448            :CCTW-IDLEGSEL                                                
017449     END-EXEC                                                             
017450                                                                          
017451     MOVE 000100         TO GOOD-SQLCODES                                 
017452     MOVE SQLCODE        TO SQLCODE-WS                                    
017453     PERFORM DB2-STATUS-CHECK                                             
017454     .                                                                    
017455     EJECT                                                                
017456                                                                          
017460 DB2-FETCH-CRS-NEW SECTION.                                               
017500     EXEC SQL FETCH NEW-CRS INTO                                          
017600            :CCTW-IDLEGSEL,                                               
017700            :CCTW-IDKST,                                                  
017710            :CCTW-IDGL,                                                   
017800            :CCTW-DAREGDAT                                                
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019028 DB2-FETCH-CRS-DEL SECTION.                                               
019030     EXEC SQL FETCH DEL-CRS INTO                                          
019031            :CCTR-IDLEGSEL,                                               
019033            :CCTR-IDKST,                                                  
019034            :CCTR-IDGL,                                                   
019035            :CCTR-DAREGDAT,                                               
019036            :CCTW-DADELDAT                                                
019037     END-EXEC                                                             
019038                                                                          
019039     MOVE 000100         TO GOOD-SQLCODES                                 
019040     MOVE SQLCODE        TO SQLCODE-WS                                    
019041     PERFORM DB2-STATUS-CHECK                                             
019042     .                                                                    
019043     EJECT                                                                
019044                                                                          
019045 DB2-FETCH-CRS-UNCHA SECTION.                                             
019047     EXEC SQL FETCH UNCHA-CRS INTO                                        
019048            :CCTR-IDLEGSEL,                                               
019049            :CCTR-IDKST,                                                  
019050            :CCTR-IDGL,                                                   
019051            :CCTR-DAREGDAT,                                               
019052            :CCTR-DADELDAT                                                
019053     END-EXEC                                                             
019054                                                                          
019055     MOVE 000100         TO GOOD-SQLCODES                                 
019056     MOVE SQLCODE        TO SQLCODE-WS                                    
019057     PERFORM DB2-STATUS-CHECK                                             
019058     .                                                                    
019059     EJECT                                                                
019060                                                                          
019061 DB2-FETCH-CRS-COPY SECTION.                                              
019062     EXEC SQL FETCH COPY-CRS INTO                                         
019063            :CCTR-IDLEGSEL,                                               
019064            :CCTR-IDKST,                                                  
019065            :CCTR-IDGL,                                                   
019066            :CCTR-DAREGDAT,                                               
019067            :CCTR-DADELDAT                                                
019068     END-EXEC                                                             
019069                                                                          
019070     MOVE 000100         TO GOOD-SQLCODES                                 
019071     MOVE SQLCODE        TO SQLCODE-WS                                    
019072     PERFORM DB2-STATUS-CHECK                                             
019073     .                                                                    
019074     EJECT                                                                
019075                                                                          
019076 DB2-CLOSE-CRS-EMPTY SECTION.                                             
019077     EXEC SQL CLOSE EMPTY-CRS                                             
019078     END-EXEC                                                             
019079     .                                                                    
019080     EJECT                                                                
019081                                                                          
019082 DB2-CLOSE-CRS-NEW SECTION.                                               
019083     EXEC SQL CLOSE NEW-CRS                                               
019084     END-EXEC                                                             
019085     .                                                                    
019086     EJECT                                                                
019087                                                                          
019088 DB2-CLOSE-CRS-DEL SECTION.                                               
019089     EXEC SQL CLOSE DEL-CRS                                               
019090     END-EXEC                                                             
019091     .                                                                    
019092     EJECT                                                                
019093                                                                          
019094 DB2-CLOSE-CRS-UNCHA SECTION.                                             
019095     EXEC SQL CLOSE UNCHA-CRS                                             
019096     END-EXEC                                                             
019097     .                                                                    
019098     EJECT                                                                
019099                                                                          
019100 DB2-CLOSE-CRS-COPY SECTION.                                              
019101     EXEC SQL CLOSE COPY-CRS                                              
019102     END-EXEC                                                             
019103     .                                                                    
019104     EJECT                                                                
019105                                                                          
019106 DB2-STATUS-CHECK SECTION.                                                
019110     SET SQLCODE-IX         TO 1                                          
019200     SEARCH GOOD-SQLCODE AT END                                           
019210           CALL ABEND USING RKOD-ABEND-DB2                                
019300        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
