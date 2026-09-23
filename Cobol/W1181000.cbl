000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.             W1181000.                                        
000400 AUTHOR.                 ARUP DATTA.                                      
000500 DATE-WRITTEN.           24/09/10.                                        
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RETRIEVE PARTS INFORMATION FROM PRINS/TC-PLM                     
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300                                                                          
001400 FILE-CONTROL.                                                            
001500***  CONSTANT WITH PRINS URL AND AUTH KEY                                 
001600     SELECT  W1181001                 ASSIGN  W11810D1.                   
001700***  INPUT FILE WITH PREVIOUS RUN TIME STAMP                              
001800     SELECT  W1181002                 ASSIGN  W11810D2.                   
001900***  OUTPUT JSON PARSED PART INFO FILE                                    
002000     SELECT  W1181003                 ASSIGN  W11810D3.                   
002100***  OUTPUT FILE WITH CURRENT RUN TIME STAMP                              
002200     SELECT  W1181004                 ASSIGN  W11810D4.                   
002300 DATA DIVISION.                                                           
002400                                                                          
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W1181001                                                             
002800     RECORDING      F                                                     
002900     BLOCK CONTAINS 0.                                                    
003000     SKIP3                                                                
003100 01  IN1-POST        PIC X(80).                                           
003200                                                                          
003300 FD  W1181002                                                             
003400     RECORDING      F                                                     
003500     BLOCK CONTAINS 0.                                                    
003600     SKIP3                                                                
003700 01  IN2-POST        PIC X(80).                                           
003800                                                                          
003900 FD  W1181003                                                             
004000     RECORDING   F                                                        
004100     BLOCK CONTAINS 0.                                                    
004200*01  POST -COPY W11810 -PRE  UT1-  -L.                                    
004300                                                                          
004400 FD  W1181004                                                             
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP3                                                                
004800 01  UT2-POST        PIC X(80).                                           
004900                                                                          
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W1181000'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  YES                         PIC X       VALUE 'Y'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600*                                                                         
005700 77  W1181001-EOF-SW             PIC X       VALUE 'N'.                   
005800     88  END-OF-W1181001                     VALUE 'J'.                   
005900                                                                          
006000                                                                          
006100 01 W-TISTAMP-CURR.                                                       
006200   03  FILLER                    PIC X(2)    VALUE '\"'.                  
006300   03  W-CURRDATE-UTC            PIC X(24)   VALUE SPACES.                
006400   03  FILLER                    PIC X(2)    VALUE '\"'.                  
006500                                                                          
006600 01 W-TISTAMP-SPAR.                                                       
006700   03  FILLER                    PIC X(2)    VALUE SPACES.                
006800   03  W-TISTAMP-SPAR-UTC        PIC X(24)   VALUE SPACES.                
006900   03  FILLER                    PIC X(2)    VALUE SPACES.                
007000*                                                                         
007100 01 WS-REFORMAT-GRP.                                                      
007200    03 WS-IDARTNR                PIC X(9)    VALUE SPACES.                
007300    03 WS-IDARTNR-BYT            PIC X(9)    VALUE SPACES.                
007400    03 WS-IDARTNR-COR            PIC X(9)    VALUE SPACES.                
007500    03 WS-IDARTNR-NUM            PIC X(9)    VALUE SPACES.                
007600    03 WS-IDARTNR-SS             PIC X(9)    VALUE SPACES.                
007700    03 WS-IDARTGRP               OCCURS 10.                               
007800       05 WS-IDARTNR-BYT-NUM     PIC X(9)    VALUE SPACES.                
007900       05 WS-IDARTNR-COR-NUM     PIC X(9)    VALUE SPACES.                
008000    03 WS-FLBYTES-MAN            PIC X(1)    VALUE SPACES.                
008100    03 WS-IDPROENH               PIC X(8)    VALUE SPACES.                
008200    03 WS-SPARE-PART             PIC X(3)    VALUE SPACES.                
008300    03 WS-KDERS                  PIC X(3)    VALUE SPACES.                
008400    03 WS-IDBERED                PIC X(2)    VALUE SPACES.                
008500    03 WS-KDPRODSL               PIC X(2)    VALUE SPACES.                
008600    03 WS-KDYTBEH                PIC X(2)    VALUE SPACES.                
008700    03 WS-KDFARLIG               PIC X(1)    VALUE SPACES.                
008800    03 WS-KDBPSR                 PIC X(1)    VALUE SPACES.                
008900    03 WS-TISOP                  PIC X(6)    VALUE SPACES.                
009000    03 WS-IDFKNGRP               PIC X(4)    VALUE SPACES.                
009100    03 WS-IDFKNGRP-BYT           PIC X(4)    VALUE SPACES.                
009200    03 WS-IDPSN                  PIC X(3)    VALUE SPACES.                
009300    03 WS-KDARTHNT               PIC X(6)    VALUE SPACES.                
009400    03 WS-KDSORT                 PIC X(5)    VALUE SPACES.                
009500    03 WS-VKART-NTO              PIC X(8)    VALUE SPACES.                
009600    03 WS-VLFG                   PIC X(8)    VALUE SPACES.                
009700    03 WS-KDSORT-VLFG            PIC X(5)    VALUE SPACES.                
009800    03 WS-IDARTNR-MOTSV          PIC X(9)    VALUE SPACES.                
009900    03 WS-IDARTNR-INSTD          PIC X(9)    VALUE SPACES.                
010000    03 WS-IDPROJ-SPAR            PIC X(4)    VALUE SPACES.                
010100    03 WS-KDERS-SPAR             PIC 9(3)    VALUE ZERO.                  
010200    03 WS-PRARTSTD               PIC X(10)   VALUE SPACES.                
010300    03 WS-DIERS                  PIC X(7)    VALUE SPACES.                
010400    03 WS-PLANNER-NOTE.                                                   
010500       05 WS-TEARTNOT-2          PIC X(40)   VALUE SPACES.                
010600       05 WS-TEARTNOT-7          PIC X(40)   VALUE SPACES.                
010700    03 WS-EXCH-PLAN-NOTE           OCCURS 10.                             
010800       05 WS-EXCH-TEARTNOT-2     PIC X(40)   VALUE SPACES.                
010900       05 WS-EXCH-TEARTNOT-7     PIC X(40)   VALUE SPACES.                
011000    03 WS-AVSL-MOT               PIC X(40)   VALUE SPACES.                
011100    03 FILLER REDEFINES WS-AVSL-MOT.                                      
011200       05 WS-AVSL-MOT-PART-1     PIC X(10).                               
011300       05 WS-AVSL-MOT-PART-2     PIC X(10).                               
011400       05 WS-AVSL-MOT-PART-3     PIC X(10).                               
011500       05 WS-AVSL-MOT-PART-4     PIC X(10).                               
011600                                                                          
011700 01 WS-SUPERSESSION-GRP-SPAR.                                             
011800    03 WS-FLSSCHG-SPAR           PIC X       VALUE SPACES.                
011900    03 WS-DIERS-ERS-SPAR         PIC X(7)    VALUE SPACES.                
012000    03 WS-TIERSDAT-UTC-SPAR      PIC X(24)   VALUE SPACES.                
012100    03 WS-KVRADER-SPAR           PIC 9(5)    VALUE ZERO.                  
012200    03 WS-SS-RAD-SPAR            OCCURS 99 TIMES.                         
012300       05 WS-IDARTNR-TILLK-SPAR  PIC X(9)    VALUE SPACES.                
012400       05 WS-DIERS-TILLK-SPAR    PIC X(7)    VALUE SPACES.                
012500       05 WS-BEERS-GRP-SPAR.                                              
012600          09 WS-BEERS-SPAR       OCCURS 10 TIMES                          
012700                                 PIC X(20)   VALUE SPACES.                
012800*                                                                         
012900 01 WS-JUST-RIGHT-GRP.                                                    
013000    03 WS-IDARTNR-R              PIC X(9)    VALUE SPACES                 
013100                                                   JUST RIGHT.            
013200    03 WS-IDARTNR-BYT-R          PIC X(9)    VALUE SPACES                 
013300                                                   JUST RIGHT.            
013400    03 WS-IDARTNR-COR-R          PIC X(9)    VALUE SPACES                 
013500                                                   JUST RIGHT.            
013600    03 WS-IDARTNR-SS-R           PIC X(9)    VALUE SPACES                 
013700                                                   JUST RIGHT.            
013800    03 WS-IDBERED-R              PIC X(2)    VALUE SPACES                 
013900                                                   JUST RIGHT.            
014000    03 WS-KDPRODSL-R             PIC X(2)    VALUE SPACES                 
014100                                                   JUST RIGHT.            
014200    03 WS-KDYTBEH-R              PIC X(2)    VALUE SPACES                 
014300                                                   JUST RIGHT.            
014400    03 WS-KDBPSR-R               PIC X(1)    VALUE SPACES                 
014500                                                   JUST RIGHT.            
014600    03 WS-TISOP-R                PIC X(6)    VALUE SPACES                 
014700                                                   JUST RIGHT.            
014800    03 WS-IDFKNGRP-R             PIC X(4)    VALUE SPACES                 
014900                                                   JUST RIGHT.            
015000    03 WS-IDPSN-R                PIC X(3)    VALUE SPACES                 
015100                                                   JUST RIGHT.            
015200    03 WS-KDERS-R                PIC X(3)    VALUE SPACES                 
015300                                                   JUST RIGHT.            
015400    03 WS-KDARTHNT-R             PIC X(6)    VALUE SPACES                 
015500                                                   JUST RIGHT.            
015600    03 WS-VKART-NTO-R            PIC X(8)    VALUE SPACES                 
015700                                                   JUST RIGHT.            
015800    03 WS-IDARTNR-MOTSV-R        PIC X(9)    VALUE SPACES                 
015900                                                   JUST RIGHT.            
016000    03 WS-PRARTSTD-R             PIC X(10)   VALUE SPACES                 
016100                                                   JUST RIGHT.            
016200    03 WS-DIERS-R                PIC X(7)    VALUE SPACES                 
016300                                                   JUST RIGHT.            
016400*                                                                         
016500 01 WS-IDFKNGRP-COR              PIC 9(4)    VALUE 0009.                  
016600 01 FILLER REDEFINES WS-IDFKNGRP-COR.                                     
016700    03 WS-IDFKNGRP-COR-F3        PIC 9(3).                                
016800    03 WS-IDFKNGRP-COR-L         PIC 9(1).                                
016900*                                                                         
017000 01 SW-PARTS-PRINS               PIC X       VALUE 'J'.                   
017100    88 MORE-PARTS-PRINS                      VALUE 'J'.                   
015100    88 NO-MORE-PARTS-PRINS                   VALUE 'N'.                   
017300*                                                                         
017400 01 SW-PRINS-PARSE               PIC X       VALUE 'N'.                   
017500    88 PRINS-PARSE-NOT-OK                    VALUE 'N'.                   
017600    88 PRINS-PARSE-OK                        VALUE 'J'.                   
017700*                                                                         
017800 01 LNK-VALUES.                                                           
017900    05 LNK-PRINS-URL             PIC X(80).                               
018000    05 LNK-PRINS-AUTH            PIC X(80).                               
018100*                                                                         
016100 01 IX-TAB-MAX                   PIC 9(3)    VALUE  999.                  
018300 01 IX-PARTS                     PIC 9(4)    VALUE ZERO.                  
018400 01 IX-PARTS-L                   PIC 9(4)    VALUE ZERO.                  
018500*                                                                         
018600 01 IX-EXCH                      PIC 9(2)    VALUE ZERO.                  
018700 01 IX-EXCH-L                    PIC 9(2)    VALUE ZERO.                  
018800 01 IX-EXCH-MAX                  PIC 9(2)    VALUE   10.                  
018900*                                                                         
019000 01 IX-SS                        PIC 9(3)    VALUE ZERO.                  
019100 01 IX-SS-MAX                    PIC 9(2)    VALUE   99.                  
019200*                                                                         
019300 01 PRINS-RESPONSE-AREA.                                                  
019400    03 PRN-RESP-LIST  OCCURS 999 INDEXED BY IX-PARTS-PR.                  
019500       05 PRN-IDBERED            PIC X(11).                               
019600       05 PRN-KDPRODSL           PIC X(11).                               
019700       05 PRN-KDPRODSL-SW        PIC X(11).                               
019800       05 PRN-KDSORT             PIC X(11).                               
019900       05 PRN-KDYTBEH            PIC X(11).                               
020000       05 PRN-DANG-GOODS         PIC X(11).                               
020100       05 PRN-KDBPSR             PIC X(11).                               
020200       05 PRN-IDKAT-1            PIC X(11).                               
020300       05 PRN-IDKAT-2            PIC X(11).                               
020400       05 PRN-IDKAT-3            PIC X(11).                               
020500       05 PRN-KDUART             PIC X(11).                               
020600       05 PRN-SPARE-PART-FL      PIC X(03).                               
020700       05 PRN-KDERS              PIC X(03).                               
020800       05 PRN-SCRAPCODE          PIC X(02).                               
020900       05 PRN-TIERSDAT-UTC       PIC X(24).                               
021000       05 PRN-SSDATE-UPD         PIC X(24).                               
021100       05 PRN-SUPERSESSION-DATA.                                          
021200          07 PRN-SUPERSEDE-TAB              OCCURS 99.                    
021300             09 PRN-SUPERSEDE-NODE.                                       
021400                11 PRN-SUPERSEDE-IDARTNR                                  
021500                                 PIC X(11).                               
021600             09 PRN-SUPERSEDE-PROPERTY.                                   
021700                11 PRN-SUPERSEDE-BEERS                                    
021800                                 PIC X(200).                              
021900                11 PRN-SUPERSEDE-QUANT                                    
022000                                 PIC X(11).                               
022100       05 PRN-FLLSRDEL           PIC X.                                   
022200          88 PRN-YES                         VALUE  'J'.                  
022300          88 PRN-NO                          VALUE  'N'.                  
022400       05 PRN-IDPROJUP           PIC X(11).                               
022500       05 PRN-IDFKNGRP           PIC X(11).                               
022600       05 PRN-TEORSAK            PIC X(50).                               
022700       05 PRN-PLANNER-NOTE       PIC X(80).                               
022800       05 PRN-TISOP-NEW          PIC X(11).                               
022900       05 PRN-PRARTSTD           PIC X(11).                               
023000       05 PRN-TEARTNOT-4         PIC X(40).                               
023100       05 PRN-KDEMBKOD-TEXT      PIC X(20).                               
023200       05 PRN-TIPRINS-UTC        PIC X(24).                               
023300       05 PRN-IDCDS              PIC X(11).                               
023400       05 PRN-BEART              PIC X(25).                               
023500       05 PRN-EXCH-PART-TAB                  OCCURS 10.                   
023600          07 PRN-IDARTNR-COR     PIC X(11).                               
023700          07 PRN-IDARTNR-BYT     PIC X(11).                               
023800          07 PRN-BEART-COR       PIC X(25).                               
023900          07 PRN-BEART-BYT       PIC X(25).                               
024000          07 PRN-EXCH-PLAN-NOTE  PIC X(80).                               
024100       05 PRN-CPART-TAB.                                                  
024200          07 PRN-IDARTNR-MOTSV   PIC X(11).                               
024300       05 PRN-PART-USE-TAB                   OCCURS 4.                    
024400          07 PRN-IDARTNR-INSTD   PIC X(11).                               
024500       05 PRN-SOFTWARE-PROD-TAB.                                          
024600          07 PRN-IDARTNR-SW      PIC X(11).                               
024700          07 PRN-BEART-SW        PIC X(25).                               
024800          07 PRN-IDPROJ-SW       PIC X(11).                               
024900       05 PRN-PART-TAB.                                                   
025000          07 PRN-IDARTNR         PIC X(11).                               
025100          07 PRN-IDPROJ          PIC X(11).                               
025200          07 PRN-MASTEREDBY      PIC X(11).                               
025300          07 PRN-A-WT-PARTS      PIC X(11).                               
025400          07 PRN-KDSORT-VLFG     PIC X(11).                               
025500          07 PRN-PART-USAGE-TAB              OCCURS 1.                    
025600             09 PRN-IDAO         PIC X(11).                               
025700             09 PRN-TISOP        PIC X(11).                               
025800          07 PRN-USED-ASMBLY-TAB             OCCURS 1.                    
025900             09 PRN-HW-SW-ASMBLY-TAB.                                     
026000                11 PRN-IDPROENH  PIC X(11).                               
026100          07 PRN-LATEST-RLSE-VER.                                         
026200             09 PRN-PART-FOLDER-ISSUE        OCCURS 1.                    
026300                11 PRN-AUX-DOCS              OCCURS 1.                    
026400                   13 PRN-AUX-IDRITN                                      
026500                                 PIC X(11).                               
026600                11 PRN-PRI-DOCS              OCCURS 1.                    
026700                   13 PRN-PRI-IDRITN                                      
026800                                 PIC X(11).                               
026900                                                                          
027000*EXCHANGE PART DETAILS FOR SUPERSEDING PARTS.                             
027100 01 PRINS-EXCH-RESPONSE-AREA.                                             
027200    03 PRN1-RESP-LIST OCCURS 99 TIMES.                                    
027300       05 PRN1-EXCH-INFO-TAB OCCURS 99 TIMES.                             
027400          07 PRN1-EXCH-SS-IDARTNR                                         
027500                                 PIC X(11).                               
027600          07 PRN1-CORE-SS-IDARTNR                                         
027700                                 PIC X(11).                               
027800***************************************                                   
027900*    DB2 VARIABLES FOR HTTP REQUESTS                                      
028000***************************************                                   
028100*                                                                         
028200 01 MY-CLOB USAGE IS SQL TYPE IS CLOB(60M).                               
028300 77 DB2-URL                      PIC X(500).                              
028400 77 DB2-HEADER                   PIC X(5000).                             
028500 77 WS-OFFSET-Z                  PIC 9(6).                                
028600 77 WS-OFFSET-R                  PIC Z(5)9.                               
028700 77 WS-OFFSET                    PIC X(6).                                
028800                                                                          
028900***********************                                                   
029000*    HTTP VARIABLES                                                       
029100***********************                                                   
029200 77 W-HEADER-BEGIN            PIC X(44) VALUE                             
029300                   '<httpHeader responseMsgFormat="errorTagged">'.        
029400                                                                          
029500 77 W-HEADER-AUTH-BEGIN       PIC X(43) VALUE                             
029600                   '<header name="Authorization" value="AppKey '.         
029700 77 W-HEADER-AUTH-END         PIC X(03) VALUE  '"/>'.                     
029800 77 W-HEADER-CONTENT-TYPE1    PIC X(28) VALUE                             
029900                   '<header name="Content-Type" '.                        
030000 77 W-HEADER-CONTENT-TYPE2J   PIC X(26) VALUE                             
030100                   'value="application/json"/>'.                          
030200 77 W-HEADER-END              PIC X(13) VALUE '</httpHeader>'.            
030300                                                                          
030400 77 W-QUERY-BEGIN             PIC X(12) VALUE '<httpHeader>'.             
030500                                                                          
030600*************************                                                 
030700* JSON SUPPORT FIELDS                                                     
030800*************************                                                 
030900                                                                          
031000 77 W-QUERY-PARTS                PIC X(15000).                            
031100 77 W-JSON-UTF8-EBCDIC           PIC X(1000).                             
031200 77 W-JSON-UTF8                  PIC U BYTE-LENGTH 80000000.              
031300*                                                                         
031400***************************************                                   
031500* DYNAMIC PROGRAMS AND AREAS                                              
031600***************************************                                   
031700*                                                                         
031800 01  DYNAMISKA-SUBPROGRAM.                                                
031900   03  ABEND                     PIC X(8)    VALUE 'ABEND '.              
032000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
032100                                                                          
032200 01  RETURKODER.                                                          
032300   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
032400   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
032500   SKIP2                                                                  
032600                                                                          
032700*                                                                         
032800*01  -COPY W0005       -PRE POSTSUM-.                                     
032900     EJECT                                                                
033000 01  IN1-AREA-START               PIC X(24)   VALUE                       
033100                                               'IN1-AREA-START  '.        
033200 01  IN1-AREA.                                                            
033300     03 FILLER                    PIC X(01).                              
033400     03 IN1-TYP                   PIC X(03).                              
033500     03 FILLER                    PIC X(02).                              
033600     03 IN1-BODY                  PIC X(75).                              
033700     EJECT                                                                
033800 01  IN2-AREA-START               PIC X(24)   VALUE                       
033900                                               'IN2-AREA-START  '.        
034000 01  IN2-AREA                     PIC X(80).                              
034100     EJECT                                                                
034200 01  UT1-AREA-START              PIC X(24)    VALUE                       
034300                                               'UT1-AREA-START  '.        
034400*01  AREA   -COPY W11810      -PRE UT1-                                   
034500     EJECT                                                                
034600 01  UT2-AREA-START              PIC X(24)    VALUE                       
034700                                               'UT2-AREA-START  '.        
034800 01  UT2-AREA                    PIC X(80).                               
034900     EJECT                                                                
035000******************************************************************        
035100*                                                                         
035200*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
035300******************************************************************        
035400 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
035500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
035600                                                                          
035700 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
035800 01  DB2-WS.                                                              
035900     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
036000         88  LINES-FOUND                    VALUE ZERO.                   
036100         88  LINES-MISSING                  VALUE 100.                    
036200     03  GOOD-SQLCODES.                                                   
036300         05  GOOD-SQLCODE OCCURS 5                                        
036400             INDEXED BY SQLCODE-IX PIC 9(3).                              
036500     EJECT                                                                
036600                                                                          
036700 PROCEDURE DIVISION.                                                      
036800                                                                          
036900     PERFORM A-INIT                                                       
037000     PERFORM B-PROCESS-INPUT                                              
037100                                                                          
037200     PERFORM C-EXTRACT-TC-PLM-DATA                                        
037300                                                                          
037400     MOVE W-TISTAMP-CURR       TO UT2-AREA                                
037500     PERFORM S12-WRITE-W1181004                                           
037600                                                                          
037700     PERFORM Z-FINIT                                                      
037800     MOVE ZERO TO RETURN-CODE                                             
037900     GOBACK                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     OPEN INPUT  W1181001                                                 
038500                 W1181002                                                 
038600          OUTPUT W1181003                                                 
038700                 W1181004                                                 
038800                                                                          
038900     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
039000     MOVE SPACES               TO LNK-VALUES                              
039100     INITIALIZE                   W-CURRDATE-UTC                          
039200                                  W-TISTAMP-CURR                          
039300                                  W-TISTAMP-SPAR                          
039400                                  UT2-AREA                                
039500     MOVE LOW-VALUES           TO UT1-AREA                                
039600*                                                                         
039700     MOVE FUNCTION                                                        
039800              FORMATTED-CURRENT-DATE('YYYY-MM-DDThh:mm:ss.sssZ')          
039900                               TO W-CURRDATE-UTC                          
040000     .                                                                    
040100     EJECT                                                                
040200 B-PROCESS-INPUT SECTION.                                                 
040300                                                                          
040400***                                                                       
040500*    GET URL AND AUTH KEY FROM CONSTANT FILE                              
040600***                                                                       
040700     PERFORM S01-READ-W1181001                                            
040800     PERFORM UNTIL END-OF-W1181001                                        
040900       IF IN1-TYP = 'URL'                                                 
041000          MOVE IN1-BODY         TO LNK-PRINS-URL                          
041100       ELSE                                                               
041200          IF IN1-TYP = 'AUT'                                              
041300             MOVE IN1-BODY      TO LNK-PRINS-AUTH                         
041400          END-IF                                                          
041500       END-IF                                                             
041600                                                                          
041700       PERFORM S01-READ-W1181001                                          
041800     END-PERFORM                                                          
041900                                                                          
042000     IF LNK-PRINS-URL NOT > SPACE                                         
042100        DISPLAY 'PRINS-URL MISSING'                                       
042200        PERFORM S99-ABEND                                                 
042300     END-IF                                                               
042400     IF LNK-PRINS-AUTH NOT > SPACE                                        
042500        DISPLAY 'PRINS-AUTH KEY MISSING'                                  
042600        PERFORM S99-ABEND                                                 
042700     END-IF                                                               
042800***                                                                       
042900*    GET PREVIOUS RUN TIMESTAMP                                           
043000***                                                                       
043100     PERFORM S02-READ-W1181002                                            
043200     MOVE IN2-AREA              TO W-TISTAMP-SPAR                         
043300     .                                                                    
043400     EJECT                                                                
043500 C-EXTRACT-TC-PLM-DATA SECTION.                                           
043600                                                                          
043700     MOVE ZERO                 TO WS-OFFSET-Z                             
043800                                  IX-PARTS-L                              
043900     MOVE JA                   TO SW-PARTS-PRINS                          
044000*                                                                         
044100     PERFORM S90-TRIM-OFFSET                                              
044200*                                                                         
044300     PERFORM UNTIL NO-MORE-PARTS-PRINS                                    
044400       PERFORM CA-GET-PART-INFO-PRINS                                     
044500       IF MY-CLOB-LENGTH > 0                                              
044600          PERFORM CB-PARSE-JSON                                           
044700          IF PRINS-PARSE-OK                                               
044800             PERFORM CC-SORT-PRN-TAB                                      
044900             PERFORM CD-WRITE-PARTS-INFO                                  
045000          ELSE                                                            
045100             MOVE ZERO         TO IX-PARTS-L                              
045200          END-IF                                                          
045300       END-IF                                                             
045400       IF  IX-PARTS-L  = IX-TAB-MAX                                       
045500           MOVE ZERO           TO IX-PARTS-L                              
045600       ELSE                                                               
045700           MOVE NEJ            TO SW-PARTS-PRINS                          
045800       END-IF                                                             
045900     END-PERFORM                                                          
046000     .                                                                    
046100     EJECT                                                                
046200 CA-GET-PART-INFO-PRINS SECTION.                                          
046300                                                                          
046400     MOVE SPACES               TO W-QUERY-PARTS                           
046500                                                                          
046600     STRING '{"query": '               DELIMITED BY SIZE                  
046700            '"query {sparePartInfos'   DELIMITED BY SIZE                  
046800            '(where:'                  DELIMITED BY SIZE                  
046900            '{lastModified_GT:'        DELIMITED BY SIZE                  
047000              W-TISTAMP-SPAR           DELIMITED BY SIZE                  
047100            '}, '                      DELIMITED BY SIZE                  
043800            'options:{limit: 999,offset:'                                 
047600                                       DELIMITED BY SIZE                  
047700              WS-OFFSET                DELIMITED BY SIZE                  
047800            '}'                        DELIMITED BY SIZE                  
047900            ')'                        DELIMITED BY SIZE                  
048000            '{'                        DELIMITED BY SIZE                  
048100            'plannerId '               DELIMITED BY SIZE                  
048200            'productGroup '            DELIMITED BY SIZE                  
048300            'productType '             DELIMITED BY SIZE                  
048400            'sparePartUnit '           DELIMITED BY SIZE                  
048500            'surface '                 DELIMITED BY SIZE                  
048600            'dangerousGoods '          DELIMITED BY SIZE                  
048700            'bpsr '                    DELIMITED BY SIZE                  
048800            'carLimit1 '               DELIMITED BY SIZE                  
048900            'carLimit2 '               DELIMITED BY SIZE                  
049000            'carLimit3 '               DELIMITED BY SIZE                  
049100            'exceptPart '              DELIMITED BY SIZE                  
049200            'sparePart '               DELIMITED BY SIZE                  
049300            'replacementCode '         DELIMITED BY SIZE                  
049400            'scrapCode '               DELIMITED BY SIZE                  
049500            'scrapDate '               DELIMITED BY SIZE                  
049600            'lastModifiedSupersession ' DELIMITED BY SIZE                 
049700            'supersededByConnection '  DELIMITED BY SIZE                  
049800            '{'                        DELIMITED BY SIZE                  
049900            'edges '                   DELIMITED BY SIZE                  
050000            '{'                        DELIMITED BY SIZE                  
050100            'node '                    DELIMITED BY SIZE                  
050200            '{'                        DELIMITED BY SIZE                  
050300            'partNumber '              DELIMITED BY SIZE                  
050400            '}'                        DELIMITED BY SIZE                  
050500            'properties '              DELIMITED BY SIZE                  
050600            '{'                        DELIMITED BY SIZE                  
050700            'comments '                DELIMITED BY SIZE                  
050800            'quantity '                DELIMITED BY SIZE                  
050900            '}'                        DELIMITED BY SIZE                  
051000            '}'                        DELIMITED BY SIZE                  
051100            '}'                        DELIMITED BY SIZE                  
051200            'isSoldSeparately '        DELIMITED BY SIZE                  
051300            'subProject '              DELIMITED BY SIZE                  
051400            'altName '                 DELIMITED BY SIZE                  
051500            'funcGroup '               DELIMITED BY SIZE                  
051600            'infoToSupplyPlanner '     DELIMITED BY SIZE                  
051700            'packingComp '             DELIMITED BY SIZE                  
051800            'plannerNote '             DELIMITED BY SIZE                  
051900            'sparePartProductionStart ' DELIMITED BY SIZE                 
052000            'pricingInfo '             DELIMITED BY SIZE                  
052100            'lastModified '            DELIMITED BY SIZE                  
052200            'owningUser '              DELIMITED BY SIZE                  
052300            'exchangePartInfos '       DELIMITED BY SIZE                  
052400            '{'                        DELIMITED BY SIZE                  
052500            'corePartNumber '          DELIMITED BY SIZE                  
052600            'exchPartNumber '          DELIMITED BY SIZE                  
052700            'corePartName '            DELIMITED BY SIZE                  
052800            'exchPartName '            DELIMITED BY SIZE                  
052900            'exchPlannerNotes '        DELIMITED BY SIZE                  
053000            '}'                        DELIMITED BY SIZE                  
053100            'comparablePart '          DELIMITED BY SIZE                  
053200            '{'                        DELIMITED BY SIZE                  
053300            'partNumber '              DELIMITED BY SIZE                  
053400            '}'                        DELIMITED BY SIZE                  
053500            'partUseInstead '          DELIMITED BY SIZE                  
053600            '{'                        DELIMITED BY SIZE                  
053700            'partNumber '              DELIMITED BY SIZE                  
053800            '}'                        DELIMITED BY SIZE                  
053900            'softwareProduct '         DELIMITED BY SIZE                  
054000            '{'                        DELIMITED BY SIZE                  
054100            'partNumber '              DELIMITED BY SIZE                  
054200            'name '                    DELIMITED BY SIZE                  
054300            'project '                 DELIMITED BY SIZE                  
054400            '}'                        DELIMITED BY SIZE                  
054500            'definitionOf '            DELIMITED BY SIZE                  
054600            '{'                        DELIMITED BY SIZE                  
054700            'partNumber '              DELIMITED BY SIZE                  
054800            'project '                 DELIMITED BY SIZE                  
054900            'masteredBy '              DELIMITED BY SIZE                  
055000            'aWeight '                 DELIMITED BY SIZE                  
055100            'unit '                    DELIMITED BY SIZE                  
055200            'rootPartUsages '          DELIMITED BY SIZE                  
055300            '('                        DELIMITED BY SIZE                  
055400            'where:'                   DELIMITED BY SIZE                  
055500            '{isRevoked:false}'        DELIMITED BY SIZE                  
055600            'options:'                 DELIMITED BY SIZE                  
055700            '{sort:{effectiveInWeek:ASC},limit:1} '                       
055800                                       DELIMITED BY SIZE                  
055900            ')'                        DELIMITED BY SIZE                  
056000            '{'                        DELIMITED BY SIZE                  
056100            'effectiveInWeek '         DELIMITED BY SIZE                  
056200            'coiNumberAdd '            DELIMITED BY SIZE                  
056300            '}'                        DELIMITED BY SIZE                  
056400            'usedInAssemblies '        DELIMITED BY SIZE                  
056500            '('                        DELIMITED BY SIZE                  
056600            'where:'                   DELIMITED BY SIZE                  
056700            '{isRevoked: false }, options:{limit:1} '                     
056800                                       DELIMITED BY SIZE                  
056900            ')'                        DELIMITED BY SIZE                  
057000            '{'                        DELIMITED BY SIZE                  
057100            'hardwareOrSoftwareAssembly '                                 
057200                                       DELIMITED BY SIZE                  
057300            '{'                        DELIMITED BY SIZE                  
057400            'partNumber '              DELIMITED BY SIZE                  
057500            '}'                        DELIMITED BY SIZE                  
057600            '}'                        DELIMITED BY SIZE                  
057700            'latestReleasedVersion '   DELIMITED BY SIZE                  
057800            '{'                        DELIMITED BY SIZE                  
057900            'partFolderIssue '         DELIMITED BY SIZE                  
058000            '(options:{sort: [{issue: DESC }], limit: 1 })'               
058100                                       DELIMITED BY SIZE                  
058200            '{'                        DELIMITED BY SIZE                  
058300            'auxDocs '                 DELIMITED BY SIZE                  
058400            '(where: { docType: \"DRAW\", subType: \"PROD\" })'           
058500                                       DELIMITED BY SIZE                  
058600            '{'                        DELIMITED BY SIZE                  
058700            'number '                  DELIMITED BY SIZE                  
058800            '}'                        DELIMITED BY SIZE                  
058900            'primaryDocs '             DELIMITED BY SIZE                  
059000            '(where: { docType: \"DRAW\", subType: \"PROD\" })'           
059100                                       DELIMITED BY SIZE                  
059200            '{'                        DELIMITED BY SIZE                  
059300            'number '                  DELIMITED BY SIZE                  
059400            '}'                        DELIMITED BY SIZE                  
059500            '}'                        DELIMITED BY SIZE                  
059600            '}'                        DELIMITED BY SIZE                  
059700            '}}}"}'                    DELIMITED BY SIZE                  
059800                             INTO W-QUERY-PARTS                           
059900*                                                                         
060000     MOVE SPACE                TO DB2-URL                                 
060100     MOVE LNK-PRINS-URL        TO DB2-URL                                 
060200     MOVE SPACE                TO DB2-HEADER                              
060300     STRING W-HEADER-BEGIN             DELIMITED BY SIZE                  
060400            W-HEADER-AUTH-BEGIN        DELIMITED BY SIZE                  
060500            LNK-PRINS-AUTH             DELIMITED BY SPACE                 
060600            W-HEADER-AUTH-END          DELIMITED BY SIZE                  
060700            W-HEADER-CONTENT-TYPE1     DELIMITED BY SIZE                  
060800            W-HEADER-CONTENT-TYPE2J    DELIMITED BY SIZE                  
060900            W-HEADER-END               DELIMITED BY SIZE                  
061000                             INTO DB2-HEADER                              
061100*                                                                         
061200     MOVE SPACE                TO MY-CLOB-DATA                            
061300     MOVE 0                    TO MY-CLOB-LENGTH                          
061400     PERFORM DB2-HTTPPOST-SYSDUMMY                                        
061500*                                                                         
061600     IF LINES-FOUND                                                       
061700        IF MY-CLOB-LENGTH > 0                                             
061800           MOVE SPACE          TO W-JSON-UTF8                             
061900           MOVE MY-CLOB-DATA   TO W-JSON-UTF8                             
062000        ELSE                                                              
062100           DISPLAY 'NO TOKEN RETURNED :' DB2-HEADER                       
062200        END-IF                                                            
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 CB-PARSE-JSON  SECTION.                                                  
062700                                                                          
062800     MOVE NEJ                  TO SW-PRINS-PARSE                          
062900     MOVE LOW-VALUES           TO PRINS-RESPONSE-AREA                     
063000     JSON PARSE W-JSON-UTF8                                               
063100                             INTO PRINS-RESPONSE-AREA                     
063200       WITH DETAIL                                                        
063300       NAME OF                                                            
063400          PRINS-RESPONSE-AREA     IS 'data'                               
063500          PRN-RESP-LIST           IS 'sparePartInfos'                     
063600          PRN-IDBERED             IS 'plannerId'                          
063700          PRN-KDPRODSL            IS 'productGroup'                       
063800          PRN-KDPRODSL-SW         IS 'productType'                        
063900          PRN-KDSORT              IS 'sparePartUnit'                      
064000          PRN-KDYTBEH             IS 'surface'                            
064100          PRN-DANG-GOODS          IS 'dangerousGoods'                     
064200          PRN-KDBPSR              IS 'bpsr'                               
064300          PRN-IDKAT-1             IS 'carLimit1'                          
064400          PRN-IDKAT-2             IS 'carLimit2'                          
064500          PRN-IDKAT-3             IS 'carLimit3'                          
064600          PRN-KDUART              IS 'exceptPart'                         
064700          PRN-SPARE-PART-FL       IS 'sparePart'                          
064800          PRN-KDERS               IS 'replacementCode'                    
064900          PRN-SCRAPCODE           IS 'scrapCode'                          
065000          PRN-TIERSDAT-UTC        IS 'scrapDate'                          
065100          PRN-SSDATE-UPD          IS 'lastModifiedSupersession'           
065200          PRN-SUPERSESSION-DATA   IS 'supersededByConnection'             
065300          PRN-SUPERSEDE-TAB       IS 'edges'                              
065400          PRN-SUPERSEDE-NODE      IS 'node'                               
065500          PRN-SUPERSEDE-IDARTNR   IS 'partNumber'                         
065600          PRN-SUPERSEDE-PROPERTY  IS 'properties'                         
065700          PRN-SUPERSEDE-BEERS     IS 'comments'                           
065800          PRN-SUPERSEDE-QUANT     IS 'quantity'                           
065900          PRN-FLLSRDEL            IS 'isSoldSeparately'                   
066000          PRN-IDPROJUP            IS 'subProject'                         
066100          PRN-BEART               IS 'altName'                            
066200          PRN-IDFKNGRP            IS 'funcGroup'                          
066300          PRN-PLANNER-NOTE        IS 'plannerNote'                        
066400          PRN-TISOP-NEW           IS 'sparePartProductionStart'           
066500          PRN-PRARTSTD            IS 'pricingInfo'                        
066600          PRN-TEARTNOT-4          IS 'infoToSupplyPlanner'                
066700          PRN-KDEMBKOD-TEXT       IS 'packingComp'                        
066800          PRN-TIPRINS-UTC         IS 'lastModified'                       
066900          PRN-IDCDS               IS 'owningUser'                         
067000          PRN-EXCH-PART-TAB       IS 'exchangePartInfos'                  
067100          PRN-IDARTNR-COR         IS 'corePartNumber'                     
067200          PRN-IDARTNR-BYT         IS 'exchPartNumber'                     
067300          PRN-BEART-COR           IS 'corePartName'                       
067400          PRN-BEART-BYT           IS 'exchPartName'                       
067500          PRN-EXCH-PLAN-NOTE      IS 'exchPlannerNotes'                   
067600          PRN-CPART-TAB           IS 'comparablePart'                     
067700          PRN-IDARTNR-MOTSV       IS 'partNumber'                         
067800          PRN-PART-USE-TAB        IS 'partUseInstead'                     
067900          PRN-IDARTNR-INSTD       IS 'partNumber'                         
068000          PRN-SOFTWARE-PROD-TAB   IS 'softwareProduct'                    
068100          PRN-IDARTNR-SW          IS 'partNumber'                         
068200          PRN-BEART-SW            IS 'name'                               
068300          PRN-IDPROJ-SW           IS 'project'                            
068400          PRN-PART-TAB            IS 'definitionOf'                       
068500          PRN-IDARTNR             IS 'partNumber'                         
068600          PRN-IDPROJ              IS 'project'                            
068700          PRN-MASTEREDBY          IS 'masteredBy'                         
068800          PRN-A-WT-PARTS          IS 'aWeight'                            
068900          PRN-KDSORT-VLFG         IS 'unit'                               
069000          PRN-PART-USAGE-TAB      IS 'rootPartUsages'                     
069100          PRN-IDAO                IS 'coiNumberAdd'                       
069200          PRN-TISOP               IS 'effectiveInWeek'                    
069300          PRN-USED-ASMBLY-TAB     IS 'usedInAssemblies'                   
069400          PRN-HW-SW-ASMBLY-TAB    IS 'hardwareOrSoftwareAssembly'         
069500          PRN-IDPROENH            IS 'partNumber'                         
069600          PRN-LATEST-RLSE-VER     IS 'latestReleasedVersion'              
069700          PRN-PART-FOLDER-ISSUE   IS 'partFolderIssue'                    
069800          PRN-AUX-DOCS            IS 'auxDocs'                            
069900          PRN-AUX-IDRITN          IS 'number'                             
070000          PRN-PRI-DOCS            IS 'primaryDocs'                        
070100          PRN-PRI-IDRITN          IS 'number'                             
070200           CONVERTING PRN-FLLSRDEL FROM  BOOLEAN                          
070300                                   USING PRN-YES AND PRN-NO               
070400       ON EXCEPTION                                                       
070500          MOVE NEJ                 TO SW-PRINS-PARSE                      
070600          EVALUATE JSON-CODE                                              
070700            WHEN 101                                                      
070800              DISPLAY                                                     
070900                     'JSON text was zero length or all whitespace'        
071000            WHEN 106                                                      
071100              DISPLAY 'No JSON name/value pair matched any data it        
071200-                     'em'                                                
071300            WHEN OTHER                                                    
071400              MOVE FUNCTION DISPLAY-OF  (                                 
071500                   FUNCTION NATIONAL-OF (W-JSON-UTF8, 1208)               
071600                                         , 278)                           
071700                                   TO W-JSON-UTF8-EBCDIC                  
071800              DISPLAY W-JSON-UTF8-EBCDIC                                  
071900              PERFORM S99-ABEND                                           
072000          END-EVALUATE                                                    
072100       NOT ON EXCEPTION                                                   
072200          MOVE JA                  TO SW-PRINS-PARSE                      
072300     END-JSON                                                             
072400     .                                                                    
072500     EJECT                                                                
072600 CC-SORT-PRN-TAB     SECTION.                                             
072700                                                                          
072800*   SORT WORKING TABLE BASED ON IDARTNR-SW AND THEN ON IDARTNR            
072900*   BOTH ARE MUTUALLY EXCLUSIVE.                                          
073000*   OBJECTIVE IS WHEN BOTH THESE FIELDS HAVE LOW-VALUES WE WILL           
073100*   TERMINATE THE LOOP WHILE PROCESSING DATA                              
073200*                                                                         
073300     SORT PRN-RESP-LIST                                                   
073400         ON DESCENDING KEY PRN-IDARTNR-SW                                 
073500         ON DESCENDING KEY PRN-IDARTNR                                    
073600     .                                                                    
073700     EJECT                                                                
073800 CD-WRITE-PARTS-INFO SECTION.                                             
073900                                                                          
074000     MOVE 1                  TO IX-PARTS                                  
074100     MOVE ZERO               TO IX-EXCH-L                                 
074200     PERFORM UNTIL (IX-PARTS  > IX-TAB-MAX)   OR                          
074300                   (PRN-IDARTNR    (IX-PARTS) NOT > LOW-VALUES            
074400               AND  PRN-IDARTNR-SW (IX-PARTS) NOT > LOW-VALUES )          
074500                                                                          
074600       MOVE ALL SPACES       TO WS-REFORMAT-GRP                           
074700                                WS-JUST-RIGHT-GRP                         
074800       MOVE ZERO             TO WS-IDFKNGRP-COR-F3                        
074900       MOVE NEJ              TO WS-FLBYTES-MAN                            
075000                                                                          
075100       PERFORM CDA-INITIALIZE-PRINS-UT                                    
075200*                                                                         
075300       IF PRN-IDARTNR-SW (IX-PARTS)  > LOW-VALUES                         
075400          PERFORM CDB-POPULATE-SOFTWARE                                   
075500          PERFORM CDE-WRITE-FILE                                          
075600       ELSE                                                               
075700          IF PRN-IDARTNR (IX-PARTS)  > LOW-VALUES                         
075800             PERFORM CDC-POPULATE-HARDWARE                                
075900             PERFORM CDD-POPULATE-SSINFO                                  
076000             PERFORM CDE-WRITE-FILE                                       
076100          END-IF                                                          
076200       END-IF                                                             
076300                                                                          
076400       ADD 1                   TO IX-PARTS                                
076500     END-PERFORM                                                          
076600                                                                          
076700     COMPUTE IX-PARTS-L = IX-PARTS - 1                                    
076800     COMPUTE WS-OFFSET-Z      = WS-OFFSET-Z + (IX-PARTS - 1)              
076900     PERFORM S90-TRIM-OFFSET                                              
077000     .                                                                    
077100 CDA-INITIALIZE-PRINS-UT SECTION.                                         
077200                                                                          
077300     INITIALIZE                 UT1-IDARTNR                               
077400                                UT1-IDPRODNR                              
077500                                UT1-FLBYTES-MAN                           
077600                                UT1-IDBERED                               
077700                                UT1-KDPRODSL                              
077800                                UT1-KDSORT                                
077900                                UT1-IDPROENH                              
078000                                UT1-KDYTBEH                               
078100                                UT1-IDPROJ                                
078200                                UT1-KDFARLIG                              
078300                                UT1-KDBPSR                                
078400                                UT1-KDUART                                
078500                                UT1-IDAO                                  
078600                                UT1-TISOP-PRINS                           
078700                                UT1-FLLSRDEL                              
078800                                UT1-IDPROJUP                              
078900                                UT1-BEART                                 
079000                                UT1-IDFKNGRP                              
079100                                UT1-TEORSAK-1                             
079200                                UT1-TEARTNOT-2                            
079300                                UT1-IDRITN                                
079400                                UT1-TEARTNOT-7                            
079500                                UT1-TEARTNOT-4                            
079600                                UT1-IDARTNR-MOTSV                         
079700                                UT1-IDPSN                                 
079800                                UT1-KDARTHNT                              
079900                                UT1-KDEMBKOD-TEXT                         
080000                                UT1-TIPRINS-UTC                           
080100                                UT1-IDCDS                                 
080200                                UT1-KDARTSYS                              
080300                                UT1-FLSPARE                               
080400                                UT1-KDERS                                 
080500                                UT1-VLFG                                  
080600                                UT1-KDSORT-VLFG                           
080700                                UT1-VKART-NTO                             
080800                                UT1-IDKAT-1                               
080900                                UT1-IDKAT-2                               
081000                                UT1-IDKAT-3                               
081100                                UT1-IDLEVNR                               
081200                                UT1-SUPERSESSION-GRP                      
081300                                UT1-KVRADER                               
081400*                                                                         
081500***  BELOW FIELD NOT IN PRINS BUT EXISTS IN TC-PLM                        
081600***  ADJUST IN NEXT PHASE                                                 
081700*                                                                         
081800*                                                                         
081900     MOVE SPACES            TO  UT1-TEORSAK-1                             
082000*    BELOW FIELDS NOT IN PRINS                                            
082100     MOVE ALL ZEROES        TO  UT1-IDPLANGR-AG                           
082200                                UT1-IDANSK                                
082300     MOVE '000000.0'        TO  UT1-KVPB-C1                               
082400***  BELOW FIELDS ARE HARDCODED/CUSTOMIZED AS PER NEED                    
082500     MOVE 'GB '             TO  UT1-IDSKYLT                               
082600     MOVE 'N'               TO  UT1-FLSSCHG                               
082700     MOVE SPACES            TO  UT1-FLRSBEART                             
082800                                UT1-FLPISK                                
082900                                UT1-IDPROJK                               
083000     MOVE ALL ZEROES        TO  UT1-PRARTSTD                              
083100     .                                                                    
083200     EJECT                                                                
083300 CDB-POPULATE-SOFTWARE SECTION.                                           
083400*                                                                         
083500     STRING PRN-IDARTNR-SW (IX-PARTS) DELIMITED BY SPACES                 
083600                              INTO WS-IDARTNR                             
083700*                                                                         
083800     IF PRN-BEART-SW (IX-PARTS) > LOW-VALUES                              
083900        MOVE PRN-BEART-SW (IX-PARTS)                                      
084000                                TO UT1-BEART                              
084100     END-IF                                                               
084200                                                                          
084300*                                                                         
084400     IF PRN-IDPROJ-SW  (IX-PARTS) > LOW-VALUES                            
084500        STRING PRN-IDPROJ-SW (IX-PARTS) DELIMITED BY '-'                  
084600                              INTO WS-IDPROJ-SPAR                         
084700        MOVE WS-IDPROJ-SPAR     TO UT1-IDPROJ                             
084800     END-IF                                                               
084900*                                                                         
085000     IF PRN-PRARTSTD (IX-PARTS)> LOW-VALUES                               
085100        STRING PRN-PRARTSTD (IX-PARTS)  DELIMITED BY ' '                  
085200                              INTO WS-PRARTSTD                            
085300        MOVE FUNCTION TRIM(WS-PRARTSTD)                                   
085400                                TO WS-PRARTSTD-R                          
085500        INSPECT WS-PRARTSTD-R  REPLACING LEADING SPACES BY ZERO           
085600        MOVE WS-PRARTSTD-R      TO UT1-PRARTSTD                           
085700     END-IF                                                               
085800*                                                                         
085900     IF PRN-KDPRODSL-SW(IX-PARTS) > LOW-VALUES                            
086000        STRING PRN-KDPRODSL-SW   (IX-PARTS) DELIMITED BY '-'              
086100                         INTO WS-KDPRODSL                                 
086200        MOVE FUNCTION TRIM(WS-KDPRODSL)                                   
086300                           TO WS-KDPRODSL-R                               
086400        INSPECT WS-KDPRODSL-R REPLACING LEADING SPACES BY ZERO            
086500        MOVE WS-KDPRODSL-R TO UT1-KDPRODSL                                
086600     ELSE                                                                 
086700        MOVE ALL ZEROES    TO UT1-KDPRODSL                                
086800     END-IF                                                               
086900*                                                                         
087000*                                                                         
087100*    MOVING HARDCODED VALUES                                              
087200     MOVE ALL ZEROES            TO UT1-IDAO                               
087300                                   UT1-IDPSN                              
087400                                   UT1-KDARTHNT                           
087500*                                                                         
087600     MOVE '3'                   TO UT1-KDFARLIG                           
087700     MOVE '5'                   TO UT1-KDBPSR                             
087800     MOVE 'PIE'                 TO UT1-IDRITN                             
087900     MOVE '1441'                TO UT1-IDLEVNR                            
088000     MOVE 9                     TO UT1-IDPLANGR-AG                        
088100     MOVE 3                     TO UT1-IDANSK                             
088200     MOVE 'SOFTWARE PRODUCT'    TO UT1-TEARTNOT-4                         
088300     MOVE 'TC'                  TO UT1-KDARTSYS                           
088400     .                                                                    
088500 CDC-POPULATE-HARDWARE SECTION.                                           
088600*                                                                         
088700     STRING PRN-IDARTNR (IX-PARTS)  DELIMITED BY SPACES                   
088800                              INTO WS-IDARTNR                             
088900*                                                                         
089000     IF PRN-BEART (IX-PARTS) > LOW-VALUES                                 
089100        MOVE PRN-BEART (IX-PARTS)                                         
089200                                TO UT1-BEART                              
089300     END-IF                                                               
089400                                                                          
089500*                                                                         
089600     IF PRN-IDPROJ  (IX-PARTS) > LOW-VALUES                               
089700        STRING PRN-IDPROJ (IX-PARTS) DELIMITED BY '-'                     
089800                              INTO WS-IDPROJ-SPAR                         
089900        MOVE   WS-IDPROJ-SPAR   TO UT1-IDPROJ                             
090000     END-IF                                                               
090100*                                                                         
090200     IF PRN-IDAO (IX-PARTS,1)     > LOW-VALUES                            
090300        STRING PRN-IDAO (IX-PARTS,1) DELIMITED BY ' '                     
090400                              INTO UT1-IDAO                               
090500     END-IF                                                               
090600*                                                                         
090700     IF PRN-KDPRODSL(IX-PARTS) > LOW-VALUES                               
090800        STRING PRN-KDPRODSL      (IX-PARTS) DELIMITED BY '-'              
090900                         INTO WS-KDPRODSL                                 
091000        MOVE FUNCTION TRIM(WS-KDPRODSL)                                   
091100                           TO WS-KDPRODSL-R                               
091200        INSPECT WS-KDPRODSL-R REPLACING LEADING SPACES BY ZERO            
091300        MOVE WS-KDPRODSL-R TO UT1-KDPRODSL                                
091400     ELSE                                                                 
091500        MOVE ALL ZEROES    TO UT1-KDPRODSL                                
091600     END-IF                                                               
091700*                                                                         
091800***    THE DANGEROUS GOODS FIELD IN PRINS IS A                            
091900***    COMBINATION OF KDFARLIG + IDPSN + DESC                             
092000***    NEEDS TO BE SEGREGATED ACCORDINGLY                                 
092100*                                                                         
092200     IF PRN-DANG-GOODS (IX-PARTS) > LOW-VALUES                            
092300        UNSTRING PRN-DANG-GOODS (IX-PARTS) DELIMITED BY ' '               
092400                         INTO WS-KDFARLIG                                 
092500                              WS-IDPSN                                    
092600        MOVE WS-IDPSN      TO WS-KDARTHNT                                 
092700        INSPECT WS-KDFARLIG   REPLACING LEADING SPACES BY ZERO            
092800        MOVE WS-KDFARLIG   TO UT1-KDFARLIG                                
092900        IF WS-KDFARLIG = '4' OR '6'                                       
093000           MOVE FUNCTION TRIM(WS-IDPSN)                                   
093100                           TO WS-IDPSN-R                                  
093200           INSPECT WS-IDPSN-R  REPLACING LEADING SPACES BY ZERO           
093300           MOVE WS-IDPSN-R TO UT1-IDPSN                                   
093400*                                                                         
093500***        HANDLING CODE SAME AS IDPSN FOR DANGEROUS GOODS                
093600*                                                                         
093700           MOVE FUNCTION TRIM(WS-KDARTHNT)                                
093800                           TO WS-KDARTHNT-R                               
093900           INSPECT WS-KDARTHNT-R                                          
094000                               REPLACING LEADING SPACES BY ZERO           
094100           MOVE WS-KDARTHNT-R                                             
094200                           TO UT1-KDARTHNT                                
094300        ELSE                                                              
094400           MOVE ALL ZEROES TO UT1-IDPSN                                   
094500                              UT1-KDARTHNT                                
094600        END-IF                                                            
094700     ELSE                                                                 
094800        MOVE ALL ZEROES    TO UT1-KDFARLIG                                
094900                              UT1-IDPSN                                   
095000                              UT1-KDARTHNT                                
095100     END-IF                                                               
095200*                                                                         
095300     IF PRN-KDBPSR  (IX-PARTS) > LOW-VALUES                               
095400        STRING PRN-KDBPSR     (IX-PARTS) DELIMITED BY '-'                 
095500                         INTO WS-KDBPSR                                   
095600        MOVE FUNCTION TRIM(WS-KDBPSR)                                     
095700                           TO WS-KDBPSR-R                                 
095800        INSPECT WS-KDBPSR-R   REPLACING LEADING SPACES BY ZERO            
095900        MOVE WS-KDBPSR-R   TO UT1-KDBPSR                                  
096000     ELSE                                                                 
096100        MOVE ALL ZEROES    TO UT1-KDBPSR                                  
096200     END-IF                                                               
096300*                                                                         
096400     IF PRN-AUX-IDRITN  (IX-PARTS,1,1) > LOW-VALUES                       
096500        STRING PRN-AUX-IDRITN (IX-PARTS,1,1) DELIMITED BY ' '             
096600                              INTO UT1-IDRITN                             
096700     ELSE                                                                 
096800        IF PRN-PRI-IDRITN  (IX-PARTS,1,1) > LOW-VALUES                    
096900           STRING PRN-PRI-IDRITN (IX-PARTS,1,1) DELIMITED BY ' '          
097000                              INTO UT1-IDRITN                             
097100        END-IF                                                            
097200     END-IF                                                               
097300*                                                                         
097400     IF PRN-TEARTNOT-4(IX-PARTS) > LOW-VALUES                             
097500        STRING PRN-TEARTNOT-4 (IX-PARTS) DELIMITED BY '-'                 
097600                         INTO UT1-TEARTNOT-4                              
097700     END-IF                                                               
097800*                                                                         
097900     IF PRN-MASTEREDBY(IX-PARTS) = 'Teamcenter'                           
098000        MOVE 'TC'          TO UT1-KDARTSYS                                
098100     END-IF                                                               
098200     .                                                                    
098300                                                                          
098400 CDD-POPULATE-SSINFO   SECTION.                                           
098500                                                                          
098600                                                                          
098700     IF PRN-SSDATE-UPD(IX-PARTS) > W-TISTAMP-SPAR-UTC                     
098800        MOVE JA            TO UT1-FLSSCHG                                 
098900     END-IF                                                               
099000                                                                          
099100     IF PRN-TIERSDAT-UTC (IX-PARTS)                                       
099200                                 > LOW-VALUES                             
099300        MOVE PRN-TIERSDAT-UTC (IX-PARTS)                                  
099400                           TO UT1-TIERSDAT-UTC                            
099500     END-IF                                                               
099600                                                                          
099700     MOVE 1                TO IX-SS                                       
099800     PERFORM UNTIL (IX-SS  > IX-SS-MAX)  OR                               
099900        (PRN-SUPERSEDE-IDARTNR (IX-PARTS,IX-SS)                           
100000                      NOT  > LOW-VALUES)                                  
100100*                                                                         
100200        STRING PRN-SUPERSEDE-IDARTNR (IX-PARTS,IX-SS)                     
100300                           DELIMITED BY SPACES                            
100400                         INTO WS-IDARTNR-SS                               
100500        MOVE FUNCTION TRIM(WS-IDARTNR-SS)                                 
100600                           TO WS-IDARTNR-SS-R                             
100700        INSPECT WS-IDARTNR-SS-R                                           
100800                           REPLACING LEADING SPACES BY ZERO               
100900        MOVE WS-IDARTNR-SS-R                                              
101000                           TO UT1-IDARTNR-TILLK(IX-SS)                    
101100*       replace with corresponding exchange part number if exists         
101200        PERFORM D-EXTRACT-TCPLM-EXCH-DATA                                 
101300*                                                                         
101400        STRING PRN-SUPERSEDE-QUANT (IX-PARTS,IX-SS)                       
101500                           DELIMITED BY SPACES                            
101600                         INTO WS-DIERS                                    
101700        MOVE FUNCTION TRIM(WS-DIERS)                                      
101800                           TO WS-DIERS-R                                  
101900        INSPECT WS-DIERS-R                                                
102000                           REPLACING LEADING SPACES BY ZERO               
102100        MOVE WS-DIERS-R                                                   
102200                           TO UT1-DIERS-TILLK(IX-SS)                      
102300*                                                                         
102400        IF PRN-SUPERSEDE-BEERS(IX-PARTS,IX-SS)                            
102500                           > LOW-VALUES                                   
102600           MOVE PRN-SUPERSEDE-BEERS(IX-PARTS,IX-SS)                       
102700                              TO UT1-BEERS-GRP(IX-SS)                     
102800        END-IF                                                            
102900*                                                                         
103000        MOVE IX-SS         TO UT1-KVRADER                                 
103100        ADD +1             TO IX-SS                                       
103200     END-PERFORM                                                          
103300*    To be used for Exchange parts,                                       
103400*    This group variable must be updated when there is a change           
103500*    in copybook                                                          
103600     MOVE UT1-SUPERSESSION-GRP                                            
103700                           TO WS-SUPERSESSION-GRP-SPAR                    
103800                                                                          
103900     .                                                                    
104000 CDE-WRITE-FILE  SECTION.                                                 
104100                                                                          
104200     MOVE FUNCTION TRIM( WS-IDARTNR )                                     
104300                              TO WS-IDARTNR-R                             
104400     INSPECT WS-IDARTNR-R  REPLACING LEADING SPACES BY ZERO               
104500     MOVE WS-IDARTNR-R        TO WS-IDARTNR-NUM                           
104600*                                                                         
104700     MOVE 1                   TO IX-EXCH                                  
104800     PERFORM UNTIL (IX-EXCH  > IX-EXCH-MAX)  OR                           
104900        (PRN-IDARTNR-BYT (IX-PARTS,IX-EXCH) NOT  > LOW-VALUES)            
105000*                                                                         
105100        IF PRN-IDARTNR-BYT (IX-PARTS,IX-EXCH) > LOW-VALUES                
105200           STRING PRN-IDARTNR-BYT (IX-PARTS,IX-EXCH)                      
105300                              DELIMITED BY SPACES                         
105400                              INTO WS-IDARTNR-BYT                         
105500           MOVE FUNCTION TRIM(WS-IDARTNR-BYT)                             
105600                              TO WS-IDARTNR-BYT-R                         
105700           INSPECT WS-IDARTNR-BYT-R                                       
105800                              REPLACING LEADING SPACES BY ZERO            
105900           MOVE WS-IDARTNR-BYT-R                                          
106000                              TO WS-IDARTNR-BYT-NUM(IX-EXCH)              
106100           MOVE JA            TO WS-FLBYTES-MAN                           
106200        ELSE                                                              
106300           MOVE ALL ZEROES    TO WS-IDARTNR-BYT-NUM(IX-EXCH)              
106400        END-IF                                                            
106500*                                                                         
106600        IF PRN-IDARTNR-COR (IX-PARTS,IX-EXCH) > LOW-VALUES                
106700           STRING PRN-IDARTNR-COR (IX-PARTS,IX-EXCH)                      
106800                              DELIMITED BY SPACES                         
106900                              INTO WS-IDARTNR-COR                         
107000           MOVE FUNCTION TRIM(WS-IDARTNR-COR)                             
107100                              TO WS-IDARTNR-COR-R                         
107200           INSPECT WS-IDARTNR-COR-R                                       
107300                              REPLACING LEADING SPACES BY ZERO            
107400           MOVE WS-IDARTNR-COR-R                                          
107500                              TO WS-IDARTNR-COR-NUM(IX-EXCH)              
107600           MOVE JA            TO WS-FLBYTES-MAN                           
107700        ELSE                                                              
107800           MOVE ALL ZEROES    TO WS-IDARTNR-COR-NUM(IX-EXCH)              
107900        END-IF                                                            
108000*                                                                         
108100        IF PRN-EXCH-PLAN-NOTE (IX-PARTS,IX-EXCH)                          
108200                               > LOW-VALUES                               
108300           MOVE PRN-EXCH-PLAN-NOTE (IX-PARTS,IX-EXCH)                     
108400                              TO WS-EXCH-PLAN-NOTE(IX-EXCH)               
108500           INSPECT WS-EXCH-PLAN-NOTE(IX-EXCH)                             
108600                              CONVERTING X'25' TO ' '                     
108700        END-IF                                                            
108800        MOVE IX-EXCH          TO IX-EXCH-L                                
108900*                                                                         
109000        ADD 1                 TO IX-EXCH                                  
109100     END-PERFORM                                                          
109200*                                                                         
109300     IF PRN-IDARTNR-INSTD (IX-PARTS,1) > LOW-VALUES                       
109400        STRING PRN-IDARTNR-INSTD (IX-PARTS,1)                             
109500                              DELIMITED BY SPACES                         
109600                            INTO WS-IDARTNR-INSTD                         
109700        MOVE WS-IDARTNR-INSTD                                             
109800                              TO WS-AVSL-MOT-PART-1                       
109900     END-IF                                                               
110000*                                                                         
110100     IF PRN-IDARTNR-INSTD (IX-PARTS,2) > LOW-VALUES                       
110200        STRING PRN-IDARTNR-INSTD (IX-PARTS,2)                             
110300                              DELIMITED BY SPACES                         
110400                            INTO WS-IDARTNR-INSTD                         
110500        MOVE WS-IDARTNR-INSTD                                             
110600                              TO WS-AVSL-MOT-PART-2                       
110700     END-IF                                                               
110800*                                                                         
110900     IF PRN-IDARTNR-INSTD (IX-PARTS,3) > LOW-VALUES                       
111000        STRING PRN-IDARTNR-INSTD (IX-PARTS,3)                             
111100                              DELIMITED BY SPACES                         
111200                            INTO WS-IDARTNR-INSTD                         
111300        MOVE WS-IDARTNR-INSTD                                             
111400                              TO WS-AVSL-MOT-PART-3                       
111500     END-IF                                                               
111600*                                                                         
111700     IF PRN-IDARTNR-INSTD (IX-PARTS,4) > LOW-VALUES                       
111800        STRING PRN-IDARTNR-INSTD (IX-PARTS,4)                             
111900                              DELIMITED BY SPACES                         
112000                            INTO WS-IDARTNR-INSTD                         
112100        MOVE WS-IDARTNR-INSTD                                             
112200                              TO WS-AVSL-MOT-PART-4                       
112300     END-IF                                                               
112400*                                                                         
112500     IF WS-AVSL-MOT > SPACES                                              
112600        MOVE WS-AVSL-MOT      TO UT1-AVSL-MOT                             
112700     ELSE                                                                 
112800        MOVE SPACES           TO UT1-AVSL-MOT                             
112900     END-IF                                                               
113000*                                                                         
113100     IF PRN-IDKAT-1 (IX-PARTS) > LOW-VALUES                               
113200        MOVE PRN-IDKAT-1 (IX-PARTS)                                       
113300                           TO UT1-IDKAT-1                                 
113400     END-IF                                                               
113500*                                                                         
113600     IF PRN-IDKAT-2 (IX-PARTS) > LOW-VALUES                               
113700        MOVE PRN-IDKAT-2 (IX-PARTS)                                       
113800                           TO UT1-IDKAT-2                                 
113900     END-IF                                                               
114000*                                                                         
114100     IF PRN-IDKAT-3 (IX-PARTS) > LOW-VALUES                               
114200        MOVE PRN-IDKAT-3 (IX-PARTS)                                       
114300                           TO UT1-IDKAT-3                                 
114400     END-IF                                                               
114500*                                                                         
114600     IF PRN-IDBERED (IX-PARTS) > LOW-VALUES                               
114700        STRING PRN-IDBERED    (IX-PARTS) DELIMITED BY '-'                 
114800                         INTO WS-IDBERED                                  
114900        MOVE FUNCTION TRIM(WS-IDBERED)                                    
115000                           TO WS-IDBERED-R                                
115100        INSPECT WS-IDBERED-R  REPLACING LEADING SPACES BY ZERO            
115200        MOVE WS-IDBERED-R  TO UT1-IDBERED                                 
115300     ELSE                                                                 
115400        MOVE ALL ZEROES    TO UT1-IDBERED                                 
115500     END-IF                                                               
115600*                                                                         
115700     IF PRN-KDSORT  (IX-PARTS) > LOW-VALUES                               
115800        STRING PRN-KDSORT     (IX-PARTS) DELIMITED BY '-'                 
115900                         INTO WS-KDSORT                                   
116000        IF WS-KDSORT  = 'LITRE'                                           
116100           MOVE 'L '       TO UT1-KDSORT                                  
116200        ELSE                                                              
116300           MOVE WS-KDSORT  TO UT1-KDSORT                                  
116400        END-IF                                                            
116500     END-IF                                                               
116600*                                                                         
116700     IF PRN-IDPROENH(IX-PARTS,1) > LOW-VALUES                             
116800        STRING PRN-IDPROENH (IX-PARTS,1) DELIMITED BY ' '                 
116900                         INTO WS-IDPROENH                                 
117000        MOVE   WS-IDPROENH TO UT1-IDPROENH                                
117100     END-IF                                                               
117200*                                                                         
117300     IF PRN-KDYTBEH (IX-PARTS) > LOW-VALUES                               
117400        STRING PRN-KDYTBEH    (IX-PARTS) DELIMITED BY '-'                 
117500                         INTO WS-KDYTBEH                                  
117600        MOVE FUNCTION TRIM(WS-KDYTBEH)                                    
117700                           TO WS-KDYTBEH-R                                
117800        INSPECT WS-KDYTBEH-R  REPLACING LEADING SPACES BY ZERO            
117900        MOVE WS-KDYTBEH-R  TO UT1-KDYTBEH                                 
118000     ELSE                                                                 
118100        MOVE ALL ZEROES    TO UT1-KDYTBEH                                 
118200     END-IF                                                               
118300***  DATE IN YYYYWW FORMAT.                                               
118400***  IN PGM W11811, CHANGE FORMAT TO YYWWD(D=1)                           
118500*                                                                         
118600     IF PRN-TISOP-NEW(IX-PARTS) > ZERO                                    
118700        STRING PRN-TISOP-NEW(IX-PARTS) DELIMITED BY ' '                   
118800                            INTO WS-TISOP                                 
118900        MOVE FUNCTION TRIM(WS-TISOP)                                      
119000                              TO WS-TISOP-R                               
119100        INSPECT WS-TISOP-R REPLACING LEADING SPACES BY ZERO               
119200        MOVE WS-TISOP-R       TO UT1-TISOP-PRINS                          
119300     ELSE                                                                 
119400        IF PRN-TISOP (IX-PARTS,1) > LOW-VALUES                            
119500           STRING PRN-TISOP    (IX-PARTS,1) DELIMITED BY ' '              
119600                            INTO WS-TISOP                                 
119700           MOVE FUNCTION TRIM(WS-TISOP)                                   
119800                              TO WS-TISOP-R                               
119900           INSPECT WS-TISOP-R REPLACING LEADING SPACES BY ZERO            
120000           MOVE WS-TISOP-R    TO UT1-TISOP-PRINS                          
120100        ELSE                                                              
120200           MOVE ALL ZEROES    TO UT1-TISOP-PRINS                          
120300        END-IF                                                            
120400     END-IF                                                               
120500*                                                                         
120600     IF PRN-FLLSRDEL(IX-PARTS) > LOW-VALUES                               
120700        MOVE PRN-FLLSRDEL   (IX-PARTS)                                    
120800                           TO UT1-FLLSRDEL                                
120900     END-IF                                                               
121000*                                                                         
121100     IF PRN-IDPROJUP (IX-PARTS) > LOW-VALUES                              
121200        STRING PRN-IDPROJUP   (IX-PARTS) DELIMITED BY ' '                 
121300                         INTO UT1-IDPROJUP                                
121400     END-IF                                                               
121500*                                                                         
121600     IF PRN-IDFKNGRP(IX-PARTS) > LOW-VALUES                               
121700        STRING PRN-IDFKNGRP   (IX-PARTS) DELIMITED BY '-'                 
121800                         INTO WS-IDFKNGRP                                 
121900        MOVE FUNCTION TRIM(WS-IDFKNGRP)                                   
122000                           TO WS-IDFKNGRP-R                               
122100        INSPECT WS-IDFKNGRP-R REPLACING LEADING SPACES BY ZERO            
122200        MOVE WS-IDFKNGRP-R TO UT1-IDFKNGRP                                
122300                              WS-IDFKNGRP-BYT                             
122400     ELSE                                                                 
122500        MOVE ALL ZEROES    TO UT1-IDFKNGRP                                
122600     END-IF                                                               
122700*                                                                         
122800***  WE RECIEVE NEW LINE CHARACTER '/N' FROM PRINS                        
122900***  THIS IS CONVERTED TO X'25' (HEX VALUES) DURING PARSING               
123000***  BELOW CODE REPLACES THE NEW LINE CHARACTER WITH '.'                  
123100*                                                                         
123200     IF PRN-PLANNER-NOTE (IX-PARTS) > LOW-VALUES                          
123300        MOVE PRN-PLANNER-NOTE (IX-PARTS)                                  
123400                           TO WS-PLANNER-NOTE                             
123500        INSPECT WS-PLANNER-NOTE                                           
123600                              CONVERTING X'25' TO ' '                     
123700        IF WS-TEARTNOT-2    > SPACES                                      
123800           MOVE WS-TEARTNOT-2                                             
123900                           TO UT1-TEARTNOT-2                              
124000        END-IF                                                            
124100        IF WS-TEARTNOT-7    > SPACES                                      
124200           MOVE WS-TEARTNOT-7                                             
124300                           TO UT1-TEARTNOT-7                              
124400        END-IF                                                            
124500     END-IF                                                               
124600*                                                                         
124700     IF PRN-IDARTNR-MOTSV(IX-PARTS) > LOW-VALUES                          
124800        STRING PRN-IDARTNR-MOTSV (IX-PARTS) DELIMITED BY ' '              
124900                         INTO WS-IDARTNR-MOTSV                            
125000        MOVE FUNCTION TRIM(WS-IDARTNR-MOTSV)                              
125100                           TO WS-IDARTNR-MOTSV-R                          
125200        INSPECT WS-IDARTNR-MOTSV-R                                        
125300                              REPLACING LEADING SPACES BY ZERO            
125400        MOVE WS-IDARTNR-MOTSV-R                                           
125500                           TO UT1-IDARTNR-MOTSV                           
125600     ELSE                                                                 
125700        MOVE ALL ZEROES    TO UT1-IDARTNR-MOTSV                           
125800     END-IF                                                               
125900*                                                                         
126000     IF PRN-KDEMBKOD-TEXT(IX-PARTS) > LOW-VALUES                          
126100        MOVE PRN-KDEMBKOD-TEXT (IX-PARTS)                                 
126200                           TO UT1-KDEMBKOD-TEXT                           
126300     END-IF                                                               
126400*                                                                         
126500     IF PRN-KDUART  (IX-PARTS) > LOW-VALUES                               
126600        STRING PRN-KDUART     (IX-PARTS) DELIMITED BY '-'                 
126700                              INTO UT1-KDUART                             
126800     END-IF                                                               
126900*                                                                         
127000     IF PRN-A-WT-PARTS (IX-PARTS) > LOW-VALUES                            
127100        STRING PRN-A-WT-PARTS (IX-PARTS) DELIMITED BY ' '                 
127200                          INTO WS-VKART-NTO                               
127300        MOVE FUNCTION TRIM(WS-VKART-NTO)                                  
127400                            TO WS-VKART-NTO-R                             
127500        INSPECT WS-VKART-NTO-R  REPLACING LEADING SPACES BY ZERO          
127600        MOVE WS-VKART-NTO-R TO UT1-VKART-NTO                              
127700                               UT1-VLFG                                   
127800     ELSE                                                                 
127900        MOVE ALL ZEROES     TO UT1-VKART-NTO                              
128000                               UT1-VLFG                                   
128100     END-IF                                                               
128200*                                                                         
128300     IF PRN-KDSORT-VLFG(IX-PARTS) > LOW-VALUES                            
128400        STRING PRN-KDSORT-VLFG(IX-PARTS) DELIMITED BY '-'                 
128500                          INTO WS-KDSORT-VLFG                             
128600        MOVE WS-KDSORT-VLFG TO UT1-KDSORT-VLFG                            
128700     END-IF                                                               
128800*                                                                         
128900     IF PRN-TIPRINS-UTC  (IX-PARTS) > SPACES                              
129000        MOVE PRN-TIPRINS-UTC     (IX-PARTS)                               
129100                           TO UT1-TIPRINS-UTC                             
129200     ELSE                                                                 
129300        MOVE ALL ZEROES    TO UT1-TIPRINS-UTC                             
129400     END-IF                                                               
129500*                                                                         
129600     IF PRN-IDCDS (IX-PARTS)   > LOW-VALUES                               
129700        STRING PRN-IDCDS      (IX-PARTS) DELIMITED BY ' '                 
129800                         INTO UT1-IDCDS                                   
129900     END-IF                                                               
130000*                                                                         
130100     IF PRN-SPARE-PART-FL  (IX-PARTS) > LOW-VALUES                        
130200        IF PRN-SPARE-PART-FL (IX-PARTS) = 'Yes'                           
130300           MOVE JA         TO UT1-FLSPARE                                 
130400        ELSE                                                              
130500           MOVE NEJ        TO UT1-FLSPARE                                 
130600        END-IF                                                            
130700     ELSE                                                                 
130800        MOVE NEJ           TO UT1-FLSPARE                                 
130900     END-IF                                                               
131000*                                                                         
131100     IF PRN-KDERS   (IX-PARTS) > LOW-VALUES                               
131200        STRING PRN-KDERS      (IX-PARTS) DELIMITED BY ' '                 
131300                         INTO WS-KDERS                                    
131400        MOVE FUNCTION TRIM(WS-KDERS)                                      
131500                           TO UT1-KDERS                                   
131600                              WS-KDERS-SPAR                               
131700        INSPECT WS-KDERS-R    REPLACING LEADING SPACES BY ZERO            
131800     ELSE                                                                 
131900        MOVE ALL ZEROES    TO UT1-KDERS                                   
132000                              WS-KDERS-SPAR                               
132100     END-IF                                                               
132200*                                                                         
132300     IF WS-KDERS-SPAR > 0                                                 
132400        MOVE '1'           TO UT1-DIERS-ERS                               
132500                              WS-DIERS-ERS-SPAR                           
132600     ELSE                                                                 
132700        MOVE SPACES        TO UT1-DIERS-ERS                               
132800                              WS-DIERS-ERS-SPAR                           
132900     END-IF                                                               
133000*                                                                         
133100     MOVE NEJ              TO UT1-FLBYTES-MAN                             
133200                                                                          
133300     IF UT1-FLSPARE     = NEJ                                             
133400*    Initialize supersession info for PROD part                           
133500        INITIALIZE            UT1-SUPERSESSION-GRP                        
133600        MOVE 'N'           TO  UT1-FLSSCHG                                
133700     END-IF                                                               
133800*                                                                         
133900***  FOR EXCHANGE AND CORE PARTS, LINKED PROD NO                          
134000***  SHOULD BE REGISTED AS NON SPARE PART - AS SEEN ON 1115               
134100***  THIS LINKED PROD PART SHOULD BE POPULATED TO PROD UNIT               
134200***  THIS IS DIFFERENT FROM NORMAL SPARE PART                             
134300*                                                                         
134400*                                                                         
134500     IF WS-FLBYTES-MAN = JA                                               
134600*                                                                         
134700***     THE PROD PART NUM SHOULD BE REGISTERED AS NON SPARE PART          
134800***     FLSPARE FROM PRINS SHOULD BE 'N' ALREADY                          
134900*                                                                         
135000        MOVE WS-IDARTNR-NUM                                               
135100                           TO UT1-IDARTNR                                 
135200        MOVE ALL ZERO      TO UT1-IDPRODNR                                
135300        MOVE WS-IDPROENH   TO UT1-IDPROENH                                
135400        PERFORM S11-WRITE-W1181003                                        
135500*                                                                         
135600***     WRITING BELOW EXCH AND CORE PART                                  
135700*                                                                         
135800        MOVE  1               TO IX-EXCH                                  
135900        PERFORM UNTIL IX-EXCH > IX-EXCH-L                                 
136000           IF WS-IDARTNR-BYT-NUM(IX-EXCH) > ZERO                          
136100              MOVE ALL SPACES TO UT1-BEART                                
136200                                                                          
136300              MOVE WS-IDARTNR-BYT-NUM(IX-EXCH)                            
136400                              TO UT1-IDARTNR                              
136500              MOVE WS-IDARTNR-NUM                                         
136600                              TO UT1-IDPRODNR                             
136700              MOVE WS-IDARTNR-NUM (2: )                                   
136800                              TO UT1-IDPROENH                             
136900              MOVE PRN-BEART-BYT (IX-PARTS,IX-EXCH)                       
137000                              TO UT1-BEART                                
137100              MOVE WS-FLBYTES-MAN                                         
137200                              TO UT1-FLBYTES-MAN                          
137300              MOVE WS-IDFKNGRP-BYT                                        
137400                              TO UT1-IDFKNGRP                             
137500              MOVE WS-IDPROJ-SPAR                                         
137600                              TO UT1-IDPROJ                               
137700              MOVE WS-KDERS-SPAR                                          
137800                              TO UT1-KDERS                                
137900              MOVE WS-SUPERSESSION-GRP-SPAR                               
138000                              TO UT1-SUPERSESSION-GRP                     
138100              IF WS-EXCH-TEARTNOT-2(IX-EXCH) > SPACES                     
138200                 MOVE SPACES  TO UT1-TEARTNOT-2                           
138300                                 UT1-TEARTNOT-7                           
138400                 MOVE WS-EXCH-TEARTNOT-2(IX-EXCH)                         
138500                              TO UT1-TEARTNOT-2                           
138600                 IF WS-EXCH-TEARTNOT-7(IX-EXCH) > SPACES                  
138700                    MOVE WS-EXCH-TEARTNOT-7(IX-EXCH)                      
138800                              TO UT1-TEARTNOT-7                           
138900                 END-IF                                                   
139000              END-IF                                                      
139100              PERFORM S11-WRITE-W1181003                                  
139200           END-IF                                                         
139300*                                                                         
139400           IF WS-IDARTNR-COR-NUM(IX-EXCH) > ZERO                          
139500              MOVE ALL SPACES TO UT1-BEART                                
139600              MOVE WS-IDARTNR-COR-NUM(IX-EXCH)                            
139700                              TO UT1-IDARTNR                              
139800              MOVE WS-IDARTNR-NUM                                         
139900                              TO UT1-IDPRODNR                             
140000              MOVE WS-IDARTNR-NUM (2: )                                   
140100                              TO UT1-IDPROENH                             
140200              MOVE PRN-BEART-COR (IX-PARTS,IX-EXCH)                       
140300                              TO UT1-BEART                                
140400              MOVE 'OBJ '     TO UT1-IDPROJ                               
140500*          ALWAYS MOVE ZERO TO KDERS FOR CORE PARTS                       
140600              MOVE ZERO       TO UT1-KDERS                                
140700              INITIALIZE         UT1-SUPERSESSION-GRP                     
140800              MOVE 'N'        TO UT1-FLSSCHG                              
140900***                                                                       
141000***        FUNCTION GRP ALSWAYS END WITH 9 FOR CORE PARTS                 
141100*                                                                         
141200              MOVE WS-IDFKNGRP-R (1:3)                                    
141300                              TO WS-IDFKNGRP-COR-F3                       
141400              IF WS-IDFKNGRP-COR-F3 > ZERO                                
141500                 MOVE WS-IDFKNGRP-COR                                     
141600                              TO UT1-IDFKNGRP                             
141700              ELSE                                                        
141800                 MOVE ALL ZERO                                            
141900                              TO UT1-IDFKNGRP                             
142000              END-IF                                                      
142100              MOVE WS-FLBYTES-MAN                                         
142200                              TO UT1-FLBYTES-MAN                          
142300              IF WS-EXCH-TEARTNOT-2(IX-EXCH) > SPACES                     
142400                 MOVE SPACES  TO UT1-TEARTNOT-2                           
142500                                 UT1-TEARTNOT-7                           
142600                 MOVE WS-EXCH-TEARTNOT-2(IX-EXCH)                         
142700                              TO UT1-TEARTNOT-2                           
142800                 IF WS-EXCH-TEARTNOT-7(IX-EXCH) > SPACES                  
142900                    MOVE WS-EXCH-TEARTNOT-7(IX-EXCH)                      
143000                              TO UT1-TEARTNOT-7                           
143100                 END-IF                                                   
143200              END-IF                                                      
143300              PERFORM S11-WRITE-W1181003                                  
143400           END-IF                                                         
143500           ADD 1              TO IX-EXCH                                  
143600        END-PERFORM                                                       
143700     ELSE                                                                 
143800        MOVE WS-IDARTNR-NUM                                               
143900                              TO UT1-IDARTNR                              
144000        MOVE ALL ZERO         TO UT1-IDPRODNR                             
144100        PERFORM S11-WRITE-W1181003                                        
144200     END-IF                                                               
144300     .                                                                    
144400     EJECT                                                                
144500     .                                                                    
144600 D-EXTRACT-TCPLM-EXCH-DATA SECTION.                                       
144700                                                                          
144800     PERFORM DA-GET-EXCH-INFO-PRINS                                       
144900     IF MY-CLOB-LENGTH > 0                                                
145000        PERFORM DB-PARSE-JSON                                             
145100        IF PRN1-EXCH-SS-IDARTNR(1,1) > LOW-VALUES                         
145200           STRING PRN1-EXCH-SS-IDARTNR(1,1)                               
145300                        DELIMITED BY SPACES                               
145400                             INTO WS-IDARTNR-SS                           
145500           MOVE FUNCTION TRIM(WS-IDARTNR-SS)                              
145600                               TO WS-IDARTNR-SS-R                         
145700           INSPECT WS-IDARTNR-SS-R                                        
145800                        REPLACING LEADING SPACES BY ZERO                  
145900           MOVE WS-IDARTNR-SS-R                                           
146000                               TO UT1-IDARTNR-TILLK(IX-SS)                
146100        END-IF                                                            
146200     END-IF                                                               
146300     .                                                                    
146400     EJECT                                                                
146500 DA-GET-EXCH-INFO-PRINS SECTION.                                          
146600                                                                          
146700     MOVE SPACES               TO W-QUERY-PARTS                           
146800                                                                          
146900     STRING '{"query":"'               DELIMITED BY SIZE                  
147000            'query PartsInfo {'        DELIMITED BY SIZE                  
147100            'parts(where:{partNumber:\"' DELIMITED BY SIZE                
147200             FUNCTION TRIM(WS-IDARTNR-SS)                                 
147300            '\"}){'                    DELIMITED BY SIZE                  
147400            'exchangeParts{'           DELIMITED BY SIZE                  
147500            'exchPartNumber '          DELIMITED BY SIZE                  
147600            'corePartNumber '          DELIMITED BY SIZE                  
147700            '}'                        DELIMITED BY SIZE                  
147800            '}'                        DELIMITED BY SIZE                  
147900            '}'                        DELIMITED BY SIZE                  
148000            '"}'                       DELIMITED BY SIZE                  
148100                             INTO W-QUERY-PARTS                           
148200                                                                          
148300     MOVE SPACE                TO DB2-URL                                 
148400     MOVE LNK-PRINS-URL        TO DB2-URL                                 
148500     MOVE SPACE                TO DB2-HEADER                              
148600     STRING W-HEADER-BEGIN             DELIMITED BY SIZE                  
148700            W-HEADER-AUTH-BEGIN        DELIMITED BY SIZE                  
148800            LNK-PRINS-AUTH             DELIMITED BY SPACE                 
148900            W-HEADER-AUTH-END          DELIMITED BY SIZE                  
149000            W-HEADER-CONTENT-TYPE1     DELIMITED BY SIZE                  
149100            W-HEADER-CONTENT-TYPE2J    DELIMITED BY SIZE                  
149200            W-HEADER-END               DELIMITED BY SIZE                  
149300                             INTO DB2-HEADER                              
149400*                                                                         
149500     MOVE SPACE                TO MY-CLOB-DATA                            
149600     MOVE 0                    TO MY-CLOB-LENGTH                          
149700     PERFORM DB2-HTTPPOST-SYSDUMMY                                        
149800*                                                                         
149900     IF LINES-FOUND                                                       
150000        IF MY-CLOB-LENGTH > 0                                             
150100           MOVE SPACE          TO W-JSON-UTF8                             
150200           MOVE MY-CLOB-DATA   TO W-JSON-UTF8                             
150300        ELSE                                                              
150400           DISPLAY 'DA- NO TOKEN RETURNED :' DB2-HEADER                   
150500        END-IF                                                            
150600     END-IF                                                               
150700     .                                                                    
150800     EJECT                                                                
150900 DB-PARSE-JSON  SECTION.                                                  
151000                                                                          
151100     MOVE LOW-VALUES           TO PRINS-EXCH-RESPONSE-AREA                
151200     JSON PARSE W-JSON-UTF8                                               
151300                             INTO PRINS-EXCH-RESPONSE-AREA                
151400       WITH DETAIL                                                        
151500       NAME OF                                                            
151600          PRINS-EXCH-RESPONSE-AREA                                        
151700                                  IS 'data'                               
151800          PRN1-RESP-LIST          IS 'parts'                              
151900          PRN1-EXCH-INFO-TAB      IS 'exchangeParts'                      
152000          PRN1-EXCH-SS-IDARTNR    IS 'exchPartNumber'                     
152100          PRN1-CORE-SS-IDARTNR    IS 'corePartNumber'                     
152200       ON EXCEPTION                                                       
152300          EVALUATE JSON-CODE                                              
152400            WHEN 101                                                      
152500              CONTINUE                                                    
152600*             DISPLAY                                                     
152700*                    'DB-JSON text was zero length or all whitesp         
152800*                    'ace'                                                
152900            WHEN 106                                                      
153000              CONTINUE                                                    
153100*             DISPLAY 'DB-No JSON name/value pair matched any data        
153200*                     'item'                                              
153300            WHEN OTHER                                                    
153400              MOVE FUNCTION DISPLAY-OF  (                                 
153500                   FUNCTION NATIONAL-OF (W-JSON-UTF8, 1208)               
153600                                         , 278)                           
153700                                   TO W-JSON-UTF8-EBCDIC                  
153800              DISPLAY W-JSON-UTF8-EBCDIC                                  
153900              PERFORM S99-ABEND                                           
154000          END-EVALUATE                                                    
154100*      NOT ON EXCEPTION                                                   
154200     END-JSON                                                             
154300     .                                                                    
154400     EJECT                                                                
154500 S01-READ-W1181001 SECTION.                                               
154600                                                                          
154700     READ W1181001        INTO IN1-AREA                                   
154800     AT END                                                               
154900        MOVE HIGH-VALUE     TO IN1-AREA                                   
155000        SET END-OF-W1181001 TO TRUE                                       
155100                                                                          
155200     NOT AT END                                                           
155300        MOVE 'W1181001'     TO POSTSUM-FDNAMN                             
155400        MOVE 'W11810D1'     TO POSTSUM-DDNAMN2                            
155500        MOVE 'IN1'          TO POSTSUM-TRANSTYP                           
155600        CALL POSTSUM     USING POSTSUM-PARM                               
155700     END-READ                                                             
155800     .                                                                    
155900     EJECT                                                                
156000 S02-READ-W1181002 SECTION.                                               
156100                                                                          
156200     READ W1181002        INTO IN2-AREA                                   
156300     AT END                                                               
156400        MOVE HIGH-VALUE     TO IN2-AREA                                   
156500                                                                          
156600     NOT AT END                                                           
156700        MOVE 'W1181002'     TO POSTSUM-FDNAMN                             
156800        MOVE 'W11810D2'     TO POSTSUM-DDNAMN2                            
156900        MOVE 'IN2'          TO POSTSUM-TRANSTYP                           
157000        CALL POSTSUM     USING POSTSUM-PARM                               
157100     END-READ                                                             
157200     .                                                                    
157300     EJECT                                                                
157400 S11-WRITE-W1181003 SECTION.                                              
157500                                                                          
157600     WRITE UT1-POST       FROM UT1-AREA                                   
157700                                                                          
157800     MOVE 'UT1'             TO POSTSUM-TRANSTYP                           
157900     MOVE 'W1181003'        TO POSTSUM-FDNAMN                             
158000     MOVE 'W11810D3'        TO POSTSUM-DDNAMN2                            
158100     CALL POSTSUM USING POSTSUM-PARM                                      
158200     .                                                                    
158300     EJECT                                                                
158400 S12-WRITE-W1181004 SECTION.                                              
158500                                                                          
158600     WRITE UT2-POST       FROM UT2-AREA                                   
158700                                                                          
158800     MOVE 'UT2'             TO POSTSUM-TRANSTYP                           
158900     MOVE 'W1181004'        TO POSTSUM-FDNAMN                             
159000     MOVE 'W11810D4'        TO POSTSUM-DDNAMN2                            
159100     CALL POSTSUM        USING POSTSUM-PARM                               
159200     .                                                                    
159300     EJECT                                                                
159400 S90-TRIM-OFFSET SECTION.                                                 
159500                                                                          
159600     MOVE WS-OFFSET-Z          TO WS-OFFSET-R                             
159700     MOVE FUNCTION TRIM(WS-OFFSET-R)                                      
159800                             TO WS-OFFSET                                 
160200     .                                                                    
160300     EJECT                                                                
160400 S99-ABEND SECTION.                                                       
160500     SKIP3                                                                
160600     MOVE 'S' TO POSTSUM-OPKOD                                            
160700     CALL POSTSUM USING POSTSUM-PARM                                      
160800     MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD                                   
160900     CALL ABEND USING RKOD                                                
161000     .                                                                    
161100     EJECT                                                                
161200 Z-FINIT   SECTION.                                                       
161300                                                                          
161400     CLOSE  W1181001                                                      
161500            W1181002                                                      
161600            W1181003                                                      
161700            W1181004                                                      
161800                                                                          
161900     MOVE 'S' TO POSTSUM-OPKOD                                            
162000     CALL POSTSUM USING POSTSUM-PARM                                      
162100     .                                                                    
162200     EJECT                                                                
162300 DB2-HTTPPOST-SYSDUMMY   SECTION.                                         
162400                                                                          
162500     MOVE 000100              TO GOOD-SQLCODES                            
162600     EXEC SQL                                                             
162700       SELECT DB2XML.HTTPPOSTCLOB                                         
162800         (CAST(TRIM(:DB2-URL) AS VARCHAR(255)),                           
162900          CAST(TRIM(:DB2-HEADER) AS CLOB(5K)),                            
163000          CAST(TRIM(:W-QUERY-PARTS) AS VARCHAR(15000))                    
163100         )                                                                
163200       INTO                                                               
163300          :MY-CLOB                                                        
163400        FROM   sysibm.sysdummy1                                           
163500     END-EXEC                                                             
163600                                                                          
163700     MOVE SQLCODE                TO SQLCODE-WS                            
163800     PERFORM DB2-STATUS-CHECK                                             
163900*                                                                         
164000     IF SQLCODE NOT = ZERO                                                
164100        DISPLAY 'SQLCODE      :' SQLCODE                                  
164200        DISPLAY 'DB2-URL      :' DB2-URL                                  
164300        DISPLAY 'DB2-HEADER   :' DB2-HEADER                               
164400        DISPLAY 'W-QUERY-PARTS:' W-QUERY-PARTS                            
164500     END-IF                                                               
164600     .                                                                    
164700     EJECT                                                                
164800 DB2-STATUS-CHECK  SECTION.                                               
164900                                                                          
165000     SET SQLCODE-IX TO 1                                                  
165100     SEARCH GOOD-SQLCODE                                                  
165200       AT END                                                             
165300         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD                                
165400         PERFORM S99-ABEND                                                
165500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
165600     END-SEARCH                                                           
165700     .                                                                    
165800     EJECT                                                                
