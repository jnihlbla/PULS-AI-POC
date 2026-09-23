000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF100400.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   NOV 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -MATCHES TEMP. ORDERNR-TABLE AGAINST PERMANENT ORDERNR-TABLE          
001910*   -CREATES FILE CONTAINING                                              
002000*    1 NEW ORDER NUMBERS                                                  
002401*    2 DELETED ORDER NUMBERS                                              
002402*    3 UNCHANGED ORDER NUMBERS                                            
002410*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003020*          --- LOADING-DATA                                               
003030     SELECT WF1009                     ASSIGN TO WF1004D1.                
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800 FD  WF1009                                                               
003810     RECORDING       F                                                    
003820     BLOCK CONTAINS  0.                                                   
003830                                                                          
003840*01  POST -COPY WF10M13 -PRE UT-   -L.                                    
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF100400'.            
004300 77  WS-COUNT                    PIC S9(9)   VALUE ZERO COMP-3.           
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01ONUM                       
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004670 01  WS-IDANALYS                 PIC X(12).                               
004680 01  WS-IDGL                     PIC X(4).                                
004686     EJECT                                                                
004687                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006540     EJECT                                                                
006541                                                                          
006542 01  WF1009-AREA-START           PIC X(24)   VALUE                        
006543                                             'WF10-AREA-START'.           
006544*01  -COPY WF10M13              -PRE UT-                                  
006545*                                                                         
006546     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
010605*01  -COPY T01ONUM    -PRE ONUM-                                          
010607                                                                          
010608*01  -COPY T01ONUW    -PRE ONUW-                                          
010609     EJECT                                                                
010610                                                                          
010611 01  FILLER                       PIC X(16)   VALUE 'ORDER-AREA'.         
010612       EXEC SQL INCLUDE T01ONUM END-EXEC.                                 
010613                                                                          
010614 01  FILLER                       PIC X(16)   VALUE 'ORDERW-AREA'.        
010615       EXEC SQL INCLUDE T01ONUW END-EXEC.                                 
010616     EJECT                                                                
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
014040     OPEN OUTPUT WF1009                                                   
014050                                                                          
014060     MOVE ZERO         TO WS-COUNT                                        
014070     PERFORM DB2-OPEN-CRS-EMPTY                                           
014080     PERFORM DB2-FETCH-CRS-EMPTY                                          
014090     PERFORM UNTIL ROW-MISSING                                            
014091       ADD 1           TO WS-COUNT                                        
014092       PERFORM DB2-FETCH-CRS-EMPTY                                        
014093     END-PERFORM                                                          
014094     PERFORM DB2-CLOSE-CRS-EMPTY                                          
014095                                                                          
014096     IF WS-COUNT = ZERO                                                   
014097       PERFORM DB2-OPEN-CRS-COPY                                          
014098       PERFORM DB2-FETCH-CRS-COPY                                         
014099       PERFORM UNTIL ROW-MISSING                                          
014100         PERFORM AA-BUILD-OUTPUT-COPY                                     
014101         PERFORM S11-WRITE-WF1009                                         
014102         PERFORM DB2-FETCH-CRS-COPY                                       
014103       END-PERFORM                                                        
014104       PERFORM DB2-CLOSE-CRS-COPY                                         
014105     END-IF                                                               
014110     .                                                                    
014200     EJECT                                                                
014210                                                                          
014220 AA-BUILD-OUTPUT-COPY SECTION.                                            
014230     MOVE ONUM-IDLEGSEL           TO UT-IDLEGSEL                          
014240     MOVE ONUM-IDANALYS           TO UT-IDANALYS                          
014250     MOVE ONUM-IDGL               TO UT-IDGL                              
014260     MOVE ONUM-DAREGDAT           TO UT-DAREGDAT                          
014270     MOVE ONUM-DADELDAT           TO UT-DADELDAT                          
014280     .                                                                    
014290     EJECT                                                                
014300                                                                          
014985 B-EXECUTE SECTION.                                                       
015023     PERFORM DB2-OPEN-CRS-DEL                                             
015025     PERFORM DB2-FETCH-CRS-DEL                                            
015026     PERFORM UNTIL ROW-MISSING                                            
015027       PERFORM BB-BUILD-OUTPUT-DEL                                        
015028       PERFORM S11-WRITE-WF1009                                           
015030       PERFORM DB2-FETCH-CRS-DEL                                          
015031     END-PERFORM                                                          
015040     PERFORM DB2-CLOSE-CRS-DEL                                            
015050                                                                          
015060     PERFORM DB2-OPEN-CRS-UNCHA                                           
015070     PERFORM DB2-FETCH-CRS-UNCHA                                          
015080     PERFORM UNTIL ROW-MISSING                                            
015090       PERFORM BC-BUILD-OUTPUT-UNCHA                                      
015091       PERFORM S11-WRITE-WF1009                                           
015092       PERFORM DB2-FETCH-CRS-UNCHA                                        
015093     END-PERFORM                                                          
015094     PERFORM DB2-CLOSE-CRS-UNCHA                                          
015095                                                                          
015096     PERFORM DB2-OPEN-CRS-NEW                                             
015097     PERFORM DB2-FETCH-CRS-NEW                                            
015098     PERFORM UNTIL ROW-MISSING                                            
015099       PERFORM BA-BUILD-OUTPUT-NEW                                        
015100       PERFORM S11-WRITE-WF1009                                           
015101       PERFORM DB2-FETCH-CRS-NEW                                          
015102     END-PERFORM                                                          
015103     PERFORM DB2-CLOSE-CRS-NEW                                            
015105     .                                                                    
015106     EJECT                                                                
015107                                                                          
015108 BA-BUILD-OUTPUT-NEW SECTION.                                             
015109     MOVE ONUW-IDLEGSEL           TO UT-IDLEGSEL                          
015110     MOVE ONUW-IDANALYS           TO UT-IDANALYS                          
015111     MOVE ONUW-IDGL               TO UT-IDGL                              
015112     MOVE ONUW-DAREGDAT           TO UT-DAREGDAT                          
015113     MOVE ZERO                    TO UT-DADELDAT                          
015114     .                                                                    
015115     EJECT                                                                
015116                                                                          
015117 BB-BUILD-OUTPUT-DEL SECTION.                                             
015118     MOVE ONUM-IDLEGSEL           TO UT-IDLEGSEL                          
015119     MOVE ONUM-IDANALYS           TO UT-IDANALYS                          
015120     MOVE ONUM-IDGL               TO UT-IDGL                              
015121     MOVE ONUM-DAREGDAT           TO UT-DAREGDAT                          
015122     MOVE ONUW-DADELDAT           TO UT-DADELDAT                          
015123     .                                                                    
015124     EJECT                                                                
015125                                                                          
015126 BC-BUILD-OUTPUT-UNCHA SECTION.                                           
015127     MOVE ONUM-IDLEGSEL           TO UT-IDLEGSEL                          
015128     MOVE ONUM-IDANALYS           TO UT-IDANALYS                          
015129     MOVE ONUM-IDGL               TO UT-IDGL                              
015130     MOVE ONUM-DAREGDAT           TO UT-DAREGDAT                          
015131     MOVE ONUM-DADELDAT           TO UT-DADELDAT                          
015132     .                                                                    
015133     EJECT                                                                
015140                                                                          
015147 Z-FINISH SECTION.                                                        
015148     CLOSE WF1009                                                         
015152     .                                                                    
015153     EJECT                                                                
015154                                                                          
015155 S11-WRITE-WF1009 SECTION.                                                
015156     WRITE UT-POST   FROM UT-WF10M13                                      
015162     .                                                                    
015163     EJECT                                                                
015164                                                                          
015165* --- DB2 SECTIONS  ---                                                   
015166*                                                                         
015167 DB2-OPEN-CRS-EMPTY SECTION.                                              
015168     EXEC SQL DECLARE EMPTY-CRS CURSOR FOR                                
015169     SELECT   T01ONUW.IDLEGSEL                                            
015170                                                                          
015171     FROM     T01ONUW                                                     
015172                                                                          
015173     FOR FETCH ONLY                                                       
015174     END-EXEC                                                             
015175                                                                          
015176     MOVE 000            TO GOOD-SQLCODES                                 
015177     EXEC SQL OPEN EMPTY-CRS                                              
015178     END-EXEC                                                             
015179                                                                          
015180     MOVE SQLCODE        TO SQLCODE-WS                                    
015181     PERFORM DB2-STATUS-CHECK                                             
015182     .                                                                    
015183     EJECT                                                                
015184                                                                          
015185 DB2-OPEN-CRS-NEW SECTION.                                                
015186     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
015187     SELECT   T01ONUW.IDLEGSEL,                                           
015188              T01ONUW.IDANALYS,                                           
015189              T01ONUW.IDGL,                                               
015190              T01ONUW.DAREGDAT                                            
015200                                                                          
015300     FROM     T01ONUW                                                     
015400                                                                          
015600     WHERE    NOT EXISTS                                                  
015800                                                                          
015900             (SELECT *                                                    
016000              FROM   T01ONUM                                              
016010              WHERE  T01ONUM.IDLEGSEL = T01ONUW.IDLEGSEL AND              
016020                     T01ONUM.IDANALYS = T01ONUW.IDANALYS AND              
016030                     T01ONUM.IDGL     = T01ONUW.IDGL)        AND          
016031                                                                          
016040              T01ONUW.DADELDAT        = '00000000'                        
016100                                                                          
016300     ORDER BY T01ONUW.IDLEGSEL,                                           
016410              T01ONUW.IDANALYS,                                           
016411              T01ONUW.IDGL                                                
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
017322     SELECT   T01ONUM.IDLEGSEL,                                           
017324              T01ONUM.IDANALYS,                                           
017325              T01ONUM.IDGL,                                               
017326              T01ONUM.DAREGDAT,                                           
017327              T01ONUW.DADELDAT                                            
017328                                                                          
017329     FROM     T01ONUW,                                                    
017330              T01ONUM                                                     
017331                                                                          
017332     WHERE    T01ONUW.IDLEGSEL     =  T01ONUM.IDLEGSEL AND                
017333              T01ONUW.IDANALYS     =  T01ONUM.IDANALYS AND                
017334              T01ONUW.IDGL         =  T01ONUM.IDGL     AND                
017335             (T01ONUW.DADELDAT     > '00000000'   AND                     
017336              T01ONUM.DADELDAT     = '00000000')                          
017337                                                                          
017338     ORDER BY T01ONUM.IDLEGSEL,                                           
017340              T01ONUM.IDANALYS,                                           
017341              T01ONUM.IDGL                                                
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
017382     SELECT   T01ONUM.IDLEGSEL,                                           
017383              T01ONUM.IDANALYS,                                           
017384              T01ONUM.IDGL,                                               
017385              T01ONUM.DAREGDAT,                                           
017386              T01ONUM.DADELDAT                                            
017387                                                                          
017388     FROM     T01ONUW,                                                    
017389              T01ONUM                                                     
017390                                                                          
017391     WHERE    T01ONUW.IDLEGSEL   = T01ONUM.IDLEGSEL    AND                
017392              T01ONUW.IDANALYS   = T01ONUM.IDANALYS    AND                
017393              T01ONUW.IDGL       = T01ONUM.IDGL        AND                
017395              T01ONUW.DADELDAT   = '00000000'                             
017396                                                                          
017397     ORDER BY T01ONUM.IDLEGSEL,                                           
017398              T01ONUM.IDANALYS,                                           
017399              T01ONUM.IDGL                                                
017400                                                                          
017401     FOR FETCH ONLY                                                       
017402     END-EXEC                                                             
017403                                                                          
017404     MOVE 000            TO GOOD-SQLCODES                                 
017405     EXEC SQL OPEN UNCHA-CRS                                              
017406     END-EXEC                                                             
017407     MOVE SQLCODE        TO SQLCODE-WS                                    
017408     PERFORM DB2-STATUS-CHECK                                             
017409     .                                                                    
017410     EJECT                                                                
017411                                                                          
017412 DB2-OPEN-CRS-COPY SECTION.                                               
017413     EXEC SQL DECLARE COPY-CRS CURSOR FOR                                 
017414     SELECT   T01ONUM.IDLEGSEL,                                           
017415              T01ONUM.IDANALYS,                                           
017416              T01ONUM.IDGL,                                               
017417              T01ONUM.DAREGDAT,                                           
017418              T01ONUM.DADELDAT                                            
017419                                                                          
017421     FROM     T01ONUM                                                     
017422                                                                          
017428     ORDER BY T01ONUM.IDLEGSEL,                                           
017429              T01ONUM.IDANALYS,                                           
017430              T01ONUM.IDGL                                                
017431                                                                          
017432     FOR FETCH ONLY                                                       
017433     END-EXEC                                                             
017434                                                                          
017435     MOVE 000            TO GOOD-SQLCODES                                 
017436     EXEC SQL OPEN COPY-CRS                                               
017437     END-EXEC                                                             
017438     MOVE SQLCODE        TO SQLCODE-WS                                    
017439     PERFORM DB2-STATUS-CHECK                                             
017440     .                                                                    
017441     EJECT                                                                
017442                                                                          
017443 DB2-FETCH-CRS-EMPTY SECTION.                                             
017444     EXEC SQL FETCH EMPTY-CRS INTO                                        
017445            :ONUW-IDLEGSEL                                                
017446     END-EXEC                                                             
017447                                                                          
017448     MOVE 000100         TO GOOD-SQLCODES                                 
017449     MOVE SQLCODE        TO SQLCODE-WS                                    
017450     PERFORM DB2-STATUS-CHECK                                             
017451     .                                                                    
017452     EJECT                                                                
017453                                                                          
017460 DB2-FETCH-CRS-NEW SECTION.                                               
017500     EXEC SQL FETCH NEW-CRS INTO                                          
017600            :ONUW-IDLEGSEL,                                               
017700            :ONUW-IDANALYS,                                               
017710            :ONUW-IDGL,                                                   
017800            :ONUW-DAREGDAT                                                
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
019031            :ONUM-IDLEGSEL,                                               
019033            :ONUM-IDANALYS,                                               
019034            :ONUM-IDGL,                                                   
019035            :ONUM-DAREGDAT,                                               
019036            :ONUW-DADELDAT                                                
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
019048            :ONUM-IDLEGSEL,                                               
019049            :ONUM-IDANALYS,                                               
019050            :ONUM-IDGL,                                                   
019051            :ONUM-DAREGDAT,                                               
019052            :ONUM-DADELDAT                                                
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
019063            :ONUM-IDLEGSEL,                                               
019064            :ONUM-IDANALYS,                                               
019065            :ONUM-IDGL,                                                   
019066            :ONUM-DAREGDAT,                                               
019067            :ONUM-DADELDAT                                                
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
