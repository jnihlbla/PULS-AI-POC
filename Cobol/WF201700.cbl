000100 PROCESS DYNAM                                                            
000110*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000130*                                                                         
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF201700.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/04/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000710*   PGM                                                                   
000720*   CREATES FEEDBACK DATA FILE FROM INVOICING DB2-TABLES.                 
000730*                                                                         
000740*   PGM READS                                                             
000750*   - ROWS IN TABLE T01PROC                                               
000760*   - ROWS IN TABLE T01DHEA                                               
000770*   - ROWS IN TABLE T01DLIN                                               
000780*   - ROWS IN TABLE T01FCUS                                               
000790*   - ROWS IN TABLE T01CURR                                               
000791*   - ROWS IN TABLE T01LSEL                                               
000792*   - ROWS IN TABLE T01SECO                                               
000793*                                                                         
000794*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
000795*                                                                         
000800*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- FEEDBACK DATA OUTPUT FILE VCCS                             
002410     SELECT WF2017                     ASSIGN TO WF2017D1.                
002420     SKIP2                                                                
002460*          --- FEEDBACK DATA OUTPUT FILE NON VCCS                         
002470     SELECT WF2037                     ASSIGN TO WF2017D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  WF2017                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003010*01  POST-WF2017 -COPY WF2017   -L.                                       
003100     EJECT                                                                
003170 FD  WF2037                                                               
003180     RECORDING       F                                                    
003190     BLOCK CONTAINS  0.                                                   
003191                                                                          
003192*01  POST-WF2037 -COPY WF2017   -L.                                       
003193     EJECT                                                                
003220 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003310*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003320 77  ERROR-TEXT                   PIC X(80)  VALUE SPACE.                 
003330                                                                          
003340* CONSTANTS.                                                              
003350 77  IDPGM                        PIC X(8)   VALUE 'WF201700'.            
003380 77  WS-CURRENT-VERSION           PIC S9(3)  VALUE +001 COMP-3.           
003390 77  WS-ACTIVE                    PIC X(8)   VALUE '00000000'.            
003391 77  WS-IDSYSTEM                  PIC X(4)   VALUE 'WF02'.                
003392 77  WS-IDLEGSEL-CRS              PIC X(4).                               
003393 77  WS-IDSYSTEM-REC              PIC X(4)   VALUE SPACE.                 
003394 77  WS-DAGENS-DATUM              PIC X(8)   VALUE '00000000'.            
003395 77  WS-DAGENS-DATUM-NUM          PIC 9(8).                               
003396 77  WS-DASTADAT-CREDIT           PIC X(8).                               
003397 77  WS2-DASTADAT-CREDIT          PIC X(8).                               
003398                                                                          
003399* WORKING-FIELDS.                                                         
003400 01  WS-MISC-MULTIFETCH.                                                  
003401     03 WS-DATUM                  PIC X(8)   VALUE SPACE.                 
003402     03 WS-KLOCKAN                PIC 9(10)  VALUE ZERO.                  
003403     03 WS-MX                     PIC S9(3)  COMP-3.                      
003404     03 WS-MULTIFETCH             PIC S9(3)  COMP-3.                      
003405     03 WS-IDLEGSEL               PIC X(4).                               
003406     03 WS-KDTRADP                PIC X(4).                               
003407     03 WS-KDVALISO-BET           PIC X(3).                               
003408     03 WS-KDVALISO               PIC X(3).                               
003409     03 WS-KDVALISO-SND           PIC X(3).                               
003410     03 WS-RECO-IDVAT             PIC X(17)  VALUE SPACE.                 
003411     03 WS-IDBREAK-B              PIC X(8).                               
003412                                                                          
003413 01  WS-MISC-TABLES.                                                      
003414     03 WS-DAFINDOC   OCCURS 100  PIC X(8).                               
003415     03 WS-FLRATE     OCCURS 100  PIC X(1).                               
003416     03 WS-DAREFDAT   OCCURS 100  PIC X(8).                               
003417     03 WS-DAFAKREF   OCCURS 100  PIC X(8).                               
003418     03 WS-IDREFRAD   OCCURS 100  PIC S9(5) COMP-3.                       
003419     03 WS-IDEXCUST-1 OCCURS 100  PIC X(15).                              
003420     03 WS-IDEXCUST-2 OCCURS 100  PIC X(15).                              
003421     03 WS-IDEXCUST-3 OCCURS 100  PIC X(15).                              
003422     03 WS-IDOPTION-1 OCCURS 100  PIC X(15).                              
003423     03 WS-IDOPTION-2 OCCURS 100  PIC X(15).                              
003424     03 WS-IDOPTION-3 OCCURS 100  PIC X(15).                              
003425     03 WS-IDOPTION-4 OCCURS 100  PIC X(15).                              
003426     03 WS-IDOPTION-5 OCCURS 100  PIC X(15).                              
003427     03 WS-IDACCNT-1  OCCURS 100  PIC X(15).                              
003428     03 WS-IDACCNT-2  OCCURS 100  PIC X(15).                              
003429     03 WS-IDACCNT-3  OCCURS 100  PIC X(15).                              
003430     03 WS-IDACCNT-4  OCCURS 100  PIC X(15).                              
003431     03 WS-FLDIRVAT   OCCURS 100  PIC X(1).                               
003432     03 WS-IDBREAK-2  OCCURS 100  PIC X(8).                               
003433     03 WS-FLLOCCUR   OCCURS 100  PIC X(1).                               
003435                                                                          
003436 *01  -COPY WWLANDX2                                                      
003437                                                                          
003440 01  WS-DASTADAT-KEY              PIC X(8)   VALUE SPACE.                 
004410 01  WS-REVALUTA                  PIC S9(5)  VALUE ZERO COMP-3.           
004420 01  WS-REVALUTA-SND              PIC S9(5)  VALUE ZERO COMP-3.           
004500                                                                          
004600 01  GENERAL-SUBPROGRAMS.                                                 
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005000                                                                          
005100*    --- PARAMETERS TO ABEND                                              
005300 01  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 01  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005510 01  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006201     EJECT                                                                
006202                                                                          
006203*    --- WORK-AREAS FOR OUTPUT-FILE                                       
006209 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
006210                                 'OUTPUT-TAB   '.                         
006211*01  -COPY WF2017T                                                        
006212     EJECT                                                                
006213 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
006214                                 'OUTPUT-AREA  '.                         
006215*01  -COPY WF2017  -PRE WS-                                               
006216     EJECT                                                                
006217                                                                          
006218*    --- WORK-AREAS FOR DB2-SECTIONS                                      
006220 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
006230*01  -COPY T01PROC    -PRE PROC-                                          
006240                                                                          
006250 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
006260*01  -COPY T01DHEA    -PRE DHEA-                                          
006270                                                                          
006280 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
006290*01  -COPY T01DLIN    -PRE DLIN-                                          
006291                                                                          
006292 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
006293*01  -COPY T01FCUS    -PRE FCUS-                                          
006294                                                                          
006295 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
006296*01  -COPY T01CURR    -PRE CURR-                                          
006297                                                                          
006298 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
006299*01  -COPY T01LSEL    -PRE LSEL-                                          
006301                                                                          
006302 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
006303*01  -COPY T01SECO    -PRE SECO-                                          
006304                                                                          
006305 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
006306*01  -COPY T01RECO    -PRE RECO-                                          
006307     EJECT                                                                
006308                                                                          
006309 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
006310       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
006311                                                                          
006312 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
006313       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
006314                                                                          
006315 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
006316       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
006317                                                                          
006318 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
006319       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
006320                                                                          
006321 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
006322       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
006323                                                                          
006324 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
006325       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
006327                                                                          
006328 01  FILLER                       PIC X(16)  VALUE 'SECO-AREA'.           
006329       EXEC SQL INCLUDE T01SECO  END-EXEC.                                
006331                                                                          
006332 01  FILLER                       PIC X(16)  VALUE 'RECO-AREA'.           
006333       EXEC SQL INCLUDE T01RECO  END-EXEC.                                
006334                                                                          
006335 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
006336       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006337*                        **** STATUS-CODE FROM DB2                        
006338 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
006339 01  DB2-WS.                                                              
006340   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
006341     88  LINES-FOUND                         VALUE 000.                   
006342     88  LINES-MISSING                       VALUE 100.                   
006343     88  RESOURCE-WRONG                      VALUE 904.                   
006344   03  GOOD-SQLCODES.                                                     
006345     05  GOOD-SQLCODE OCCURS 5                                            
006346         INDEXED BY SQLCODE-IX    PIC 999.                                
006347     EJECT                                                                
006350                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006900     PERFORM A-INIT                                                       
007000                                                                          
007110     PERFORM DB2-OPEN-CRS-LSEL                                            
007120     PERFORM DB2-FETCH-CRS-LSEL                                           
007130     PERFORM UNTIL LINES-MISSING                                          
007140       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                
007150       MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM            
007160       PERFORM DB2-SELECT-T01PROC-TAB                                     
007170       IF PROC-KDBEH = 'P'                                                
007180         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
007190         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
007191       END-IF                                                             
007192                                                                          
007200       PERFORM DB2-DCL-OPN-CRS1                                           
007300       PERFORM DB2-FETCH-CRS1                                             
007310       IF SQLERRD(3) > 0                                                  
007320         MOVE 000     TO SQLCODE-WS                                       
007330       END-IF                                                             
007400                                                                          
007500       PERFORM UNTIL LINES-MISSING                                        
007510         MOVE SQLERRD(3)                  TO WS-MULTIFETCH                
007520         MOVE ZERO                        TO WS-MX                        
007530         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
007540           ADD +1                         TO WS-MX                        
007541**** INT2 SHOULD NOT BE IN FILE                                           
007542           IF FEED-KDFINDOC(WS-MX) = 'INT2'                               
007543             CONTINUE                                                     
007544           ELSE                                                           
007550             MOVE FEED-IDLEGSEL   (WS-MX) TO WS-IDLEGSEL                  
007560             MOVE FEED-KDVALISO-BET (WS-MX) TO WS-KDVALISO-BET            
007570             MOVE FEED-KDVALISO   (WS-MX) TO WS-KDVALISO                  
007600             PERFORM S11-WRITE-WF20X7                                     
007601           END-IF                                                         
007610         END-PERFORM                                                      
007700         PERFORM DB2-FETCH-CRS1                                           
007710         IF SQLERRD(3) > 0                                                
007720           MOVE 000                       TO SQLCODE-WS                   
007721         END-IF                                                           
007800       END-PERFORM                                                        
008000       PERFORM DB2-CLOSE-CRS1                                             
008010                                                                          
008100       PERFORM DB2-FETCH-CRS-LSEL                                         
008110     END-PERFORM                                                          
008120     PERFORM DB2-CLOSE-CRS-LSEL                                           
008130                                                                          
008200     PERFORM Z-FINIT                                                      
008400     MOVE ZERO                        TO RETURN-CODE                      
008500     GOBACK                                                               
008600     .                                                                    
008700                                                                          
008800 A-INIT SECTION.                                                          
008900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
009000     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
009006                                                                          
009010     OPEN OUTPUT WF2017                                                   
009012                 WF2037                                                   
009017                                                                          
009020     INITIALIZE FEED-WF2017T                                              
009400     .                                                                    
009500                                                                          
009600 Z-FINIT SECTION.                                                         
009710     CLOSE WF2017                                                         
009730     CLOSE WF2037                                                         
009900     .                                                                    
010000                                                                          
010102 S11-WRITE-WF20X7 SECTION.                                                
010104     MOVE PROC-DAEXDAT                 TO WS-FEED-DAEXDAT                 
010105     MOVE PROC-TIEXTID                 TO WS-FEED-TIEXTID                 
010106     MOVE WS-KDTRADP                   TO WS-FEED-KDTRADP-SC              
010107     COMPUTE WS-FEED-TIEXTID = WS-FEED-TIEXTID / 10                       
010108     END-COMPUTE                                                          
010109     MOVE FEED-IDLEGSEL        (WS-MX) TO WS-FEED-IDLEGSEL                
010110     MOVE FEED-KDVALISO        (WS-MX) TO WS-FEED-KDVALISO                
010112     MOVE FEED-IDLANDX3-SEND   (WS-MX) TO WS-FEED-IDLANDX3-SEND           
010113     MOVE FEED-IDLANDX3-REC    (WS-MX) TO WS-FEED-IDLANDX3-REC            
010114     MOVE FEED-IDLEVNR         (WS-MX) TO WS-FEED-IDLEVNR                 
010115     MOVE FEED-IDPARTNR        (WS-MX) TO WS-FEED-IDPARTNR                
010116     MOVE FEED-KDFINDOC        (WS-MX) TO WS-FEED-KDFINDOC                
010117     MOVE FEED-FLSOFT          (WS-MX) TO WS-FEED-FLSOFT                  
010118     MOVE FEED-FLFREE          (WS-MX) TO WS-FEED-FLFREE                  
010119     MOVE FEED-IDFINDOC        (WS-MX) TO WS-FEED-IDFINDOC                
010120     MOVE FEED-IDSYSTEM-SEND   (WS-MX) TO WS-FEED-IDSYSTEM-SEND           
010121     MOVE FEED-IDSYSTEM-REC    (WS-MX) TO WS-FEED-IDSYSTEM-REC            
010122     MOVE FEED-IDSPRAK         (WS-MX) TO WS-FEED-IDSPRAK                 
010123     MOVE FEED-KDBETALV        (WS-MX) TO WS-FEED-KDBETALV                
010124     MOVE FEED-BEBETVIL        (WS-MX) TO WS-FEED-BEBETVIL                
010125     MOVE FEED-BELEGRAD-1      (WS-MX) TO WS-FEED-BELEGRAD-1              
010126     MOVE FEED-BELEGRAD-2      (WS-MX) TO WS-FEED-BELEGRAD-2              
010127     MOVE FEED-ADLEG-STREET    (WS-MX) TO WS-FEED-ADLEG-STREET            
010128     MOVE FEED-ADLEG-BOX       (WS-MX) TO WS-FEED-ADLEG-BOX               
010129     MOVE FEED-ADLEG-CITY      (WS-MX) TO WS-FEED-ADLEG-CITY              
010130     MOVE FEED-ADLEG-PCODE     (WS-MX) TO WS-FEED-ADLEG-PCODE             
010131     MOVE FEED-IDLANDX3-LEG    (WS-MX) TO WS-FEED-IDLANDX3-LEG            
010132     MOVE FEED-IDVAT-LEG       (WS-MX) TO WS-FEED-IDVAT-LEG               
010133     MOVE FEED-IDTFN-LEG       (WS-MX) TO WS-FEED-IDTFN-LEG               
010134     MOVE FEED-IDTFX-LEG       (WS-MX) TO WS-FEED-IDTFX-LEG               
010135     MOVE FEED-IDMAIL-LEG      (WS-MX) TO WS-FEED-IDMAIL-LEG              
010136     MOVE FEED-BECONT-LEG      (WS-MX) TO WS-FEED-BECONT-LEG              
010137     MOVE FEED-IDBG-LEG        (WS-MX) TO WS-FEED-IDBG-LEG                
010138     MOVE FEED-IDPG-LEG        (WS-MX) TO WS-FEED-IDPG-LEG                
010139     MOVE FEED-BERESPRA-1      (WS-MX) TO WS-FEED-BERESPRA-1              
010140     MOVE FEED-BERESPRA-2      (WS-MX) TO WS-FEED-BERESPRA-2              
010141     MOVE FEED-ADRESP-STREET   (WS-MX) TO WS-FEED-ADRESP-STREET           
010142     MOVE FEED-ADRESP-BOX      (WS-MX) TO WS-FEED-ADRESP-BOX              
010143     MOVE FEED-ADRESP-CITY     (WS-MX) TO WS-FEED-ADRESP-CITY             
010144     MOVE FEED-ADRESP-PCODE    (WS-MX) TO WS-FEED-ADRESP-PCODE            
010145     MOVE FEED-IDLANDX3-RESP   (WS-MX) TO WS-FEED-IDLANDX3-RESP           
010146     MOVE FEED-IDVAT-RESP      (WS-MX) TO WS-FEED-IDVAT-RESP              
010147     MOVE FEED-IDTFN-RESP      (WS-MX) TO WS-FEED-IDTFN-RESP              
010148     MOVE FEED-IDTFX-RESP      (WS-MX) TO WS-FEED-IDTFX-RESP              
010149     MOVE FEED-IDMAIL-RESP     (WS-MX) TO WS-FEED-IDMAIL-RESP             
010150     MOVE FEED-BECONT-RESP     (WS-MX) TO WS-FEED-BECONT-RESP             
010151     MOVE FEED-IDBG-RESP       (WS-MX) TO WS-FEED-IDBG-RESP               
010152     MOVE FEED-IDPG-RESP       (WS-MX) TO WS-FEED-IDPG-RESP               
010153     MOVE FEED-BEBET-NAME1     (WS-MX) TO WS-FEED-BEBET-NAME1             
010154     MOVE FEED-BEBET-NAME2     (WS-MX) TO WS-FEED-BEBET-NAME2             
010155     MOVE FEED-ADBET-STREET    (WS-MX) TO WS-FEED-ADBET-STREET            
010156     MOVE FEED-ADBET-BOX       (WS-MX) TO WS-FEED-ADBET-BOX               
010157     MOVE FEED-ADBET-CITY      (WS-MX) TO WS-FEED-ADBET-CITY              
010158     MOVE FEED-ADBET-PCODE     (WS-MX) TO WS-FEED-ADBET-PCODE             
010159     MOVE FEED-IDVAT-BET       (WS-MX) TO WS-FEED-IDVAT-BET               
010160     MOVE FEED-IDLANDX3-BET    (WS-MX) TO WS-FEED-IDLANDX3-BET            
010161     MOVE FEED-KDTRADP         (WS-MX) TO WS-FEED-KDTRADP                 
010163     MOVE FEED-SUNTO-SERV      (WS-MX) TO WS-FEED-SUNTO-SERV              
010164     MOVE FEED-SUNTO-PART      (WS-MX) TO WS-FEED-SUNTO-PART              
010165     MOVE FEED-SUBTO-SERV      (WS-MX) TO WS-FEED-SUBTO-SERV              
010166     MOVE FEED-SUBTO-PART      (WS-MX) TO WS-FEED-SUBTO-PART              
010167     MOVE FEED-SUNTO-TOT       (WS-MX) TO WS-FEED-SUNTO-TOT               
010168     MOVE FEED-SUBTO-TOT       (WS-MX) TO WS-FEED-SUBTO-TOT               
010169     MOVE FEED-SUVAT-BILLIT-TOT (WS-MX)                                   
010170                                       TO WS-FEED-SUVAT-BILLIT-TOT        
010171     MOVE FEED-IDBUNDLE        (WS-MX) TO WS-FEED-IDBUNDLE                
010172     MOVE FEED-BEVOLREF        (WS-MX) TO WS-FEED-BEVOLREF                
010173     MOVE FEED-IDREF           (WS-MX) TO WS-FEED-IDREF                   
010175     MOVE FEED-IDAPPEND        (WS-MX) TO WS-FEED-IDAPPEND                
010176     MOVE FEED-IDARTNR-FINANCE (WS-MX) TO WS-FEED-IDARTNR-FINANCE         
010178     MOVE FEED-BEART           (WS-MX) TO WS-FEED-BEART                   
010179     MOVE FEED-IDSTATNR        (WS-MX) TO WS-FEED-IDSTATNR                
010180     MOVE FEED-VKORDBTO-KOLLI  (WS-MX) TO WS-FEED-VKORDBTO-KOLLI          
010182     MOVE FEED-VKARTNTO        (WS-MX) TO WS-FEED-VKARTNTO                
010183     MOVE FEED-PRARTBTO        (WS-MX) TO WS-FEED-PRARTBTO                
010184     MOVE FEED-PRARTNTO        (WS-MX) TO WS-FEED-PRARTNTO                
010185     MOVE FEED-REARTRAB        (WS-MX) TO WS-FEED-REARTRAB                
010186     MOVE FEED-KVBEART         (WS-MX) TO WS-FEED-KVBEART                 
010187     MOVE FEED-KVLEVART        (WS-MX) TO WS-FEED-KVLEVART                
010188     MOVE FEED-FLSPECPR        (WS-MX) TO WS-FEED-FLSPECPR                
010189     MOVE FEED-REVAT           (WS-MX) TO WS-FEED-REVAT                   
010190     MOVE FEED-KDVAT           (WS-MX) TO WS-FEED-KDVAT                   
010191     MOVE FEED-KDARTURS        (WS-MX) TO WS-FEED-KDARTURS                
010192     MOVE FEED-KDANMORS        (WS-MX) TO WS-FEED-KDANMORS                
010193     MOVE FEED-IDFAKREF        (WS-MX) TO WS-FEED-IDFAKREF                
010194     MOVE FEED-IDDC            (WS-MX) TO WS-FEED-IDDC                    
010195     MOVE FEED-KDFRAKT         (WS-MX) TO WS-FEED-KDFRAKT                 
010196     MOVE FEED-BELEVVIL        (WS-MX) TO WS-FEED-BELEVVIL                
010197     MOVE FEED-SUNTO           (WS-MX) TO WS-FEED-SUNTO                   
010198     MOVE FEED-SUBTO           (WS-MX) TO WS-FEED-SUBTO                   
010199     MOVE FEED-SUVAT-BILLIT    (WS-MX) TO WS-FEED-SUVAT-BILLIT            
010200     MOVE FEED-BETEXT          (WS-MX) TO WS-FEED-BETEXT                  
010201     MOVE FEED-KDVALISO-BET    (WS-MX) TO WS-FEED-KDVALISO-BET            
010203     MOVE FEED-BEANST          (WS-MX) TO WS-FEED-BEANST                  
010204     MOVE FEED-KDPARTTY        (WS-MX) TO WS-FEED-KDPARTTY                
010205     MOVE FEED-KDPARTGR        (WS-MX) TO WS-FEED-KDPARTGR                
010207     MOVE WS-DAFINDOC          (WS-MX) TO WS-FEED-DAFINDOC                
010208     MOVE WS-DAREFDAT          (WS-MX) TO WS-FEED-DAREFDAT                
010209     MOVE WS-DAFAKREF          (WS-MX) TO WS-FEED-DAFAKREF                
010210     MOVE WS-IDEXCUST-1        (WS-MX) TO WS-FEED-IDEXCUST (1)            
010211     MOVE WS-IDEXCUST-2        (WS-MX) TO WS-FEED-IDEXCUST (2)            
010212     MOVE WS-IDEXCUST-3        (WS-MX) TO WS-FEED-IDEXCUST (3)            
010213     MOVE WS-IDOPTION-1        (WS-MX) TO WS-FEED-IDOPTION (1)            
010214     MOVE WS-IDOPTION-2        (WS-MX) TO WS-FEED-IDOPTION (2)            
010215     MOVE WS-IDOPTION-3        (WS-MX) TO WS-FEED-IDOPTION (3)            
010216     MOVE WS-IDOPTION-4        (WS-MX) TO WS-FEED-IDOPTION (4)            
010217     MOVE WS-IDOPTION-5        (WS-MX) TO WS-FEED-IDOPTION (5)            
010218     MOVE WS-IDACCNT-1         (WS-MX) TO WS-FEED-IDACCNT  (1)            
010219     MOVE WS-IDACCNT-2         (WS-MX) TO WS-FEED-IDACCNT  (2)            
010220     MOVE WS-IDACCNT-3         (WS-MX) TO WS-FEED-IDACCNT  (3)            
010221     MOVE WS-IDACCNT-4         (WS-MX) TO WS-FEED-IDACCNT  (4)            
010222     MOVE WS-IDREFRAD          (WS-MX) TO WS-FEED-IDREFRAD                
010223     MOVE FEED-IDLEVNR-ART     (WS-MX) TO WS-FEED-IDLEVNR-ART             
010224     MOVE FEED-IDVAT-AGENT     (WS-MX) TO WS-FEED-IDVAT-AGENT             
010225     MOVE FEED-FLPCOO          (WS-MX) TO WS-FEED-FLPCOO                  
010226     MOVE FEED-KDPRMOD         (WS-MX) TO WS-FEED-KDPRMOD                 
010227                                                                          
010228*CHANGES DUE TO CENTRAL PRICING                                           
010229     IF FEED-KDFINDOC(WS-MX) = 'CR'                                       
010230       IF FEED-IDLEVNR(WS-MX)(1:4) = '0000'                               
010231       OR FEED-IDLEVNR(WS-MX)(1:4) = '    '                               
010232         PERFORM DB2-SELECT-MAX-T01CURR                                   
010233         PERFORM DB2-SELECT-T01CURR                                       
010234       ELSE                                                               
010235         MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                             
010236     MOVE FEED-IDLEVNR(WS-MX)(1:4) TO WS-DASTADAT-CREDIT(3:4)             
010237         MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                             
010238         PERFORM DB2-SELECT-MAX-T01CURR-CREDIT                            
010239         PERFORM DB2-SELECT-T01CURR-CREDIT                                
010240       END-IF                                                             
010241     ELSE                                                                 
010242       PERFORM DB2-SELECT-MAX-T01CURR                                     
010243       PERFORM DB2-SELECT-T01CURR                                         
010244     END-IF                                                               
010245     COMPUTE WS-FEED-PRKURS-BET ROUNDED =                                 
010246             CURR-PRKURS / WS-REVALUTA                                    
010247     END-COMPUTE                                                          
010248                                                                          
010249     IF FEED-KDFINDOC(WS-MX) = 'CR'                                       
010250       IF FEED-IDLEVNR(WS-MX)(1:4) = '0000'                               
010251       OR FEED-IDLEVNR(WS-MX)(1:4) = '    '                               
010252         PERFORM DB2-SELECT-T01CURR-2                                     
010253       ELSE                                                               
010254         PERFORM DB2-SELECT-T01CURR-2-CRE                                 
010255       END-IF                                                             
010256     ELSE                                                                 
010257       PERFORM DB2-SELECT-T01CURR-2                                       
010258     END-IF                                                               
010259     COMPUTE WS-FEED-PRKURS ROUNDED =                                     
010260             CURR-PRKURS / WS-REVALUTA                                    
010261     END-COMPUTE                                                          
010262     COMPUTE WS-FEED-PRKURS-FAKBET ROUNDED =                              
010263             WS-FEED-PRKURS-BET / WS-FEED-PRKURS                          
010264     END-COMPUTE                                                          
010265*****                                                                     
010266                                                                          
010267*CHANGES DUE TO CURRENCY RATE ON SENDING COUNTRY TO CUSTOM                
010268     PERFORM DB2-SELECT-T01SECO-SND                                       
010269     IF FEED-KDFINDOC(WS-MX) = 'CR'                                       
010270       IF FEED-IDLEVNR(WS-MX)(1:4) = '0000'                               
010271       OR FEED-IDLEVNR(WS-MX)(1:4) = '    '                               
010272         PERFORM DB2-SELECT-MAX-T01CURR-SND                               
010273         PERFORM DB2-SELECT-T01CURR-SND                                   
010274       ELSE                                                               
010275         PERFORM DB2-SELECT-MAX-T01CURR-SND-CRE                           
010276         PERFORM DB2-SELECT-T01CURR-SND-CRE                               
010277       END-IF                                                             
010278     ELSE                                                                 
010279       PERFORM DB2-SELECT-MAX-T01CURR-SND                                 
010280       PERFORM DB2-SELECT-T01CURR-SND                                     
010281     END-IF                                                               
010282     COMPUTE WS-FEED-PRKURS-SND ROUNDED =                                 
010283             (CURR-PRKURS     / WS-REVALUTA-SND) /                        
010284             WS-FEED-PRKURS                                               
010285     END-COMPUTE                                                          
010286     MOVE WS-KDVALISO-SND TO WS-FEED-KDVALISO-SND                         
010287******                                                                    
010288                                                                          
010289**** IDVAT-DDGS-RESP                                                      
010290     MOVE SPACE          TO WS-FEED-IDVAT-DDGS-RESP                       
010291     MOVE FEED-IDLANDX3-RESP(WS-MX) TO LANDX2-IDLANDX2                    
010292     IF LANDX2-EU-IDLANDX2                                                
010293       MOVE FEED-IDLANDX3-SEND(WS-MX) TO LANDX2-IDLANDX2                  
010294       IF LANDX2-EU-IDLANDX2                                              
010295         IF WS-FLDIRVAT(WS-MX) = 'J'                                      
010296           IF  FEED-IDLEVNR(WS-MX) > ' '                                  
010297           AND FEED-KDFINDOC(WS-MX) = 'INV'                               
010298             MOVE WS-IDBREAK-2(WS-MX) TO WS-IDBREAK-B                     
010299             MOVE WS-IDBREAK-B(6:2)   TO LANDX2-IDLANDX2                  
010300             IF LANDX2-EU-IDLANDX2                                        
010301               MOVE SPACE          TO WS-RECO-IDVAT                       
010302               PERFORM DB2-SELECT-T01RECO                                 
010303               MOVE WS-RECO-IDVAT  TO WS-FEED-IDVAT-DDGS-RESP             
010304             ELSE                                                         
010305               CONTINUE                                                   
010306             END-IF                                                       
010307           END-IF                                                         
010308         ELSE                                                             
010309           CONTINUE                                                       
010310         END-IF                                                           
010311       ELSE                                                               
010312         CONTINUE                                                         
010313       END-IF                                                             
010314     ELSE                                                                 
010315       CONTINUE                                                           
010316     END-IF                                                               
010317****                                                                      
010318                                                                          
010319**** AVERAGE COST                                                         
010322     MOVE SPACE            TO WS-FEED-KDVALISO-AVC                        
010323     MOVE ZERO             TO WS-FEED-PRAVCOST                            
010324     IF FEED-KDPRMOD(WS-MX) = '04'                                        
010325       MOVE FEED-KDVALISO(WS-MX)  TO WS-FEED-KDVALISO-AVC                 
010326       MOVE FEED-PRARTNTO(WS-MX)  TO WS-FEED-PRAVCOST                     
010327     END-IF                                                               
010328****                                                                      
010329                                                                          
010330**** KDVALISO RECALCULATED ON THE INVOICE                                 
010331     IF WS-FLRATE(WS-MX) = 'N'                                            
010332       PERFORM DB2-SELECT-MAX-T01CURR                                     
010333       PERFORM DB2-SELECT-T01CURR                                         
010334     ELSE                                                                 
010335       MOVE FEED-PRKURS(WS-MX) TO CURR-PRKURS                             
010336       MOVE 1                  TO WS-REVALUTA                             
010337     END-IF                                                               
010338     MOVE SPACE TO WS-FEED-KDVALISO-RECALC                                
010339     MOVE ZERO TO WS-FEED-SUNTO-TOT-RECALC                                
010340     MOVE ZERO TO WS-FEED-SUVAT-BILLIT-TOT-REC                            
010341     MOVE ZERO TO WS-FEED-SUBTO-TOT-RECALC                                
010342     IF WS-FLLOCCUR(WS-MX) = 'J'                                          
010343     AND FEED-KDVALISO(WS-MX) = 'SEK'                                     
010344       IF FEED-KDVALISO(WS-MX) = FEED-KDVALISO-BET(WS-MX)                 
010345         CONTINUE                                                         
010346       ELSE                                                               
010347         MOVE FEED-KDVALISO-BET(WS-MX) TO WS-FEED-KDVALISO-RECALC         
010348         COMPUTE WS-FEED-SUNTO-TOT-RECALC ROUNDED =                       
010349                (FEED-SUNTO-TOT(WS-MX) * WS-REVALUTA) /                   
010350                CURR-PRKURS                                               
010351         COMPUTE WS-FEED-SUVAT-BILLIT-TOT-REC    ROUNDED =                
010352                (FEED-SUVAT-BILLIT-TOT(WS-MX) * WS-REVALUTA) /            
010353                CURR-PRKURS                                               
010354         COMPUTE WS-FEED-SUBTO-TOT-RECALC ROUNDED =                       
010355                (FEED-SUBTO-TOT(WS-MX) * WS-REVALUTA) /                   
010356                CURR-PRKURS                                               
010357       END-IF                                                             
010358     END-IF                                                               
010359****                                                                      
010360                                                                          
010361     IF FEED-IDLEGSEL (WS-MX) = 'VCCS'                                    
010362       WRITE POST-WF2017   FROM WS-FEED-WF2017                            
010363     ELSE                                                                 
010364       WRITE POST-WF2037   FROM WS-FEED-WF2017                            
010365     END-IF                                                               
010366     .                                                                    
010367                                                                          
010368* --- DB2 SECTIONS  ---                                                   
010369*                                                                         
010370 DB2-SELECT-T01PROC-TAB   SECTION.                                        
010371     MOVE 000    TO GOOD-SQLCODES                                         
010372                                                                          
010373     EXEC SQL                                                             
010374           SELECT  IDLEGSEL                                               
010380                ,  DAEXDAT                                                
010400                ,  TIEXTID                                                
010410                ,  KDBEH                                                  
010500                                                                          
010600           INTO   :PROC-IDLEGSEL                                          
010610                , :PROC-DAEXDAT                                           
010700                , :PROC-TIEXTID                                           
010710                , :PROC-KDBEH                                             
010800                                                                          
010900           FROM    T01PROC                                                
011000                                                                          
011100           WHERE   IDSYSTEM = :WS-IDSYSTEM     AND                        
011110                   IDLEGSEL = :WS-IDLEGSEL-CRS                            
011200     END-EXEC                                                             
011300                                                                          
011400     MOVE SQLCODE TO SQLCODE-WS                                           
011500     PERFORM DB2-STATUS-CHECK                                             
011600     .                                                                    
011700                                                                          
011710 DB2-SELECT-MAX-T01CURR SECTION.                                          
011730     MOVE 000            TO GOOD-SQLCODES                                 
011740                                                                          
011750     EXEC SQL                                                             
011760     SELECT   MAX(T01CURR.DASTADAT)                                       
011770                                                                          
011780     INTO     :WS-DASTADAT-KEY                                            
011790                                                                          
011791     FROM     T01CURR                                                     
011792                                                                          
011793     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
011794       AND   (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
011795        OR    T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
011796     END-EXEC                                                             
011797                                                                          
011798     MOVE SQLCODE        TO SQLCODE-WS                                    
011799     PERFORM DB2-STATUS-CHECK                                             
011800     .                                                                    
011801                                                                          
011802 DB2-SELECT-T01CURR SECTION.                                              
011803     MOVE 000            TO GOOD-SQLCODES                                 
011804                                                                          
011805     EXEC SQL                                                             
011806     SELECT   KDVALISO                                                    
011807             ,PRKURS                                                      
011808             ,REVALUTA                                                    
011809                                                                          
011810     INTO     :CURR-KDVALISO                                              
011811             ,:CURR-PRKURS                                                
011812             ,:WS-REVALUTA                                                
011813                                                                          
011814     FROM     T01CURR                                                     
011815                                                                          
011816     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
011817       AND    KDVALISO = :WS-KDVALISO-BET                                 
011818       AND    DASTADAT = :WS-DASTADAT-KEY                                 
011819     END-EXEC                                                             
011820                                                                          
011821     MOVE SQLCODE        TO SQLCODE-WS                                    
011822     PERFORM DB2-STATUS-CHECK                                             
011823     .                                                                    
011824                                                                          
011844 DB2-SELECT-T01CURR-2 SECTION.                                            
011845     MOVE 000            TO GOOD-SQLCODES                                 
011846                                                                          
011847     EXEC SQL                                                             
011848     SELECT   KDVALISO                                                    
011849             ,PRKURS                                                      
011850             ,REVALUTA                                                    
011851                                                                          
011852     INTO     :CURR-KDVALISO                                              
011853             ,:CURR-PRKURS                                                
011854             ,:WS-REVALUTA                                                
011855                                                                          
011856     FROM     T01CURR                                                     
011857                                                                          
011858     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
011859       AND    KDVALISO = :WS-KDVALISO                                     
011860       AND    DASTADAT = :WS-DASTADAT-KEY                                 
011861     END-EXEC                                                             
011862                                                                          
011863     MOVE SQLCODE        TO SQLCODE-WS                                    
011864     PERFORM DB2-STATUS-CHECK                                             
011865     .                                                                    
011866                                                                          
011867 DB2-SELECT-MAX-T01CURR-CREDIT SECTION.                                   
011868     MOVE 000            TO GOOD-SQLCODES                                 
011869                                                                          
011870     EXEC SQL                                                             
011871     SELECT   MAX(T01CURR.DASTADAT)                                       
011872                                                                          
011873     INTO     :WS2-DASTADAT-CREDIT                                        
011874                                                                          
011875     FROM     T01CURR                                                     
011876                                                                          
011877     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
011878       AND   (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
011879        OR    T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
011880     END-EXEC                                                             
011881                                                                          
011882     MOVE SQLCODE        TO SQLCODE-WS                                    
011883     PERFORM DB2-STATUS-CHECK                                             
011884     .                                                                    
011885                                                                          
011886 DB2-SELECT-T01CURR-CREDIT SECTION.                                       
011887     MOVE 000            TO GOOD-SQLCODES                                 
011888                                                                          
011889     EXEC SQL                                                             
011890     SELECT   KDVALISO                                                    
011891             ,PRKURS                                                      
011892             ,REVALUTA                                                    
011893                                                                          
011894     INTO     :CURR-KDVALISO                                              
011895             ,:CURR-PRKURS                                                
011896             ,:WS-REVALUTA                                                
011897                                                                          
011898     FROM     T01CURR                                                     
011899                                                                          
011900     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
011901       AND    KDVALISO = :WS-KDVALISO-BET                                 
011902       AND    DASTADAT = :WS2-DASTADAT-CREDIT                             
011903     END-EXEC                                                             
011904                                                                          
011905     MOVE SQLCODE        TO SQLCODE-WS                                    
011906     PERFORM DB2-STATUS-CHECK                                             
011907     .                                                                    
011908                                                                          
011909 DB2-SELECT-T01CURR-2-CRE SECTION.                                        
011910     MOVE 000            TO GOOD-SQLCODES                                 
011911                                                                          
011912     EXEC SQL                                                             
011913     SELECT   KDVALISO                                                    
011914             ,PRKURS                                                      
011915             ,REVALUTA                                                    
011916                                                                          
011917     INTO     :CURR-KDVALISO                                              
011918             ,:CURR-PRKURS                                                
011919             ,:WS-REVALUTA                                                
011920                                                                          
011921     FROM     T01CURR                                                     
011922                                                                          
011923     WHERE    IDLEGSEL = :WS-IDLEGSEL                                     
011924       AND    KDVALISO = :WS-KDVALISO                                     
011925       AND    DASTADAT = :WS2-DASTADAT-CREDIT                             
011926     END-EXEC                                                             
011927                                                                          
011928     MOVE SQLCODE        TO SQLCODE-WS                                    
011929     PERFORM DB2-STATUS-CHECK                                             
011930     .                                                                    
011931                                                                          
011940 DB2-DCL-OPN-CRS1 SECTION.                                                
012000     MOVE 000100 TO GOOD-SQLCODES                                         
012100                                                                          
012200     EXEC SQL                                                             
012300        DECLARE CRS1 CURSOR WITH ROWSET POSITIONING FOR                   
012400        SELECT A.IDLEGSEL                                                 
012500             , A.IDLANDX3_SEND                                            
012600             , A.KDVALISO                                                 
012610             , A.IDLEVNR                                                  
012700             , A.IDPARTNR                                                 
012800             , A.KDFINDOC                                                 
012810             , A.FLSOFT                                                   
012900             , A.FLFREE                                                   
013000             , A.DAFINDOC                                                 
013100             , A.IDFINDOC                                                 
013200             , A.IDSYSTEM_SEND                                            
013300             , A.IDSYSTEM_REC                                             
013400             , A.IDSPRAK                                                  
013500             , A.KDBETALV                                                 
013600             , A.BEBETVIL                                                 
013700             , A.BELEGRAD_1                                               
013800             , A.BELEGRAD_2                                               
013900             , A.ADLEG_STREET                                             
014000             , A.ADLEG_BOX                                                
014100             , A.ADLEG_CITY                                               
014200             , A.ADLEG_PCODE                                              
014300             , A.IDLANDX3_LEG                                             
014400             , A.IDTFN_LEG                                                
014500             , A.IDTFX_LEG                                                
014600             , A.IDMAIL_LEG                                               
014700             , A.BECONT_LEG                                               
014800             , A.IDVAT_LEG                                                
014900             , A.IDBG_LEG                                                 
015000             , A.IDPG_LEG                                                 
015100             , A.BERESPRA_1                                               
015200             , A.BERESPRA_2                                               
015300             , A.ADRESP_STREET                                            
015400             , A.ADRESP_BOX                                               
015500             , A.ADRESP_CITY                                              
015600             , A.ADRESP_PCODE                                             
015700             , A.IDLANDX3_RESP                                            
015800             , A.IDTFN_RESP                                               
015900             , A.IDTFX_RESP                                               
016000             , A.IDMAIL_RESP                                              
016100             , A.BECONT_RESP                                              
016200             , A.IDVAT_RESP                                               
016300             , A.IDBG_RESP                                                
016400             , A.IDPG_RESP                                                
016500             , A.BEBET_NAME1                                              
016600             , A.BEBET_NAME2                                              
016700             , A.ADBET_STREET                                             
016800             , A.ADBET_BOX                                                
016900             , A.ADBET_CITY                                               
017000             , A.ADBET_PCODE                                              
017100             , A.IDLANDX3_BET                                             
017200             , A.IDVAT_BET                                                
017300             , A.KDTRADP                                                  
017400             , A.SUNTO_SERV                                               
017500             , A.SUBTO_SERV                                               
017600             , A.SUNTO_PART                                               
017700             , A.SUBTO_PART                                               
017800             , A.SUNTO_TOT                                                
017900             , A.SUBTO_TOT                                                
018000             , A.SUVAT_BILLIT_TOT                                         
018100             , A.IDVAT_AGENT                                              
018110             , A.IDBREAK_2                                                
018200             , B.IDLANDX3_REC                                             
018300             , B.IDEXCUST_1                                               
018400             , B.IDEXCUST_2                                               
018500             , B.IDEXCUST_3                                               
018600             , B.IDBUNDLE                                                 
018700             , B.IDREF                                                    
018800             , B.DAREFDAT                                                 
018900             , B.BEVOLREF                                                 
019000             , B.IDOPTION_1                                               
019100             , B.IDOPTION_2                                               
019200             , B.IDOPTION_3                                               
019300             , B.IDOPTION_4                                               
019400             , B.IDOPTION_5                                               
019500             , B.IDARTNR_FINANCE                                          
019600             , B.BEART                                                    
019700             , B.IDSTATNR                                                 
019800             , B.VKORDBTO_KOLLI                                           
019810             , B.VKARTNTO                                                 
019900             , B.KDARTURS                                                 
020000             , B.KVBEART                                                  
020100             , B.KVLEVART                                                 
020200             , B.PRARTBTO                                                 
020300             , B.PRARTNTO                                                 
020400             , B.REARTRAB                                                 
020500             , B.FLSPECPR                                                 
020700             , B.KDANMORS                                                 
020800             , B.IDFAKREF                                                 
020900             , B.DAFAKREF                                                 
021000             , B.IDDC                                                     
021011             , B.KDFRAKT                                                  
021020             , B.BELEVVIL                                                 
021100             , B.IDACCNT_1                                                
021200             , B.IDACCNT_2                                                
021300             , B.IDACCNT_3                                                
021400             , B.IDACCNT_4                                                
021500             , B.KDVAT                                                    
021600             , B.REVAT                                                    
021700             , B.SUNTO                                                    
021800             , B.SUVAT_BILLIT                                             
021900             , B.SUBTO                                                    
021910             , B.IDREFRAD                                                 
022000             , B.FILLER                                                   
022001             , B.IDLEVNR_ART                                              
022002             , B.FLPCOO                                                   
022010             , C.KDVALISO                                                 
022020             , C.FLRATE                                                   
022030             , C.KDPARTTY                                                 
022040             , C.KDPARTGR                                                 
022050             , C.FLDIRVAT                                                 
022060             , C.FLLOCCUR                                                 
022070             , B.KDPRMOD                                                  
022100                                                                          
022200        FROM   T01DHEA A                                                  
022300             , T01DLIN B                                                  
022310             , T01FCUS C                                                  
022400                                                                          
022410        WHERE  A.IDLEGSEL      = :PROC-IDLEGSEL                           
022411        AND    A.DAEXDAT       = :PROC-DAEXDAT                            
022420        AND    A.TIEXTID       = :PROC-TIEXTID                            
022700        AND    A.IDSYSTEM_REC  > :WS-IDSYSTEM-REC                         
022710        AND    C.IDLEGSEL      = A.IDLEGSEL                               
022720        AND    C.IDPARTNR      = A.IDPARTNR                               
022730        AND    C.KDSTATUS      = 1                                        
022740        AND    C.DADELDAT      = '00000000'                               
022800        AND    A.IDLEGSEL      = B.IDLEGSEL                               
022810        AND    A.DAEXDAT       = B.DAEXDAT                                
022900        AND    A.TIEXTID       = B.TIEXTID                                
022910        AND    A.KDVALISO      = B.KDVALISO                               
022920        AND    A.IDLANDX3_SEND = B.IDLANDX3_SEND                          
022930        AND    A.IDLEVNR       = B.IDLEVNR                                
022940        AND    A.IDPARTNR      = B.IDPARTNR                               
022950        AND    A.KDFINDOC      = B.KDFINDOC                               
022960        AND    A.FLSOFT        = B.FLSOFT                                 
022970        AND    A.FLFREE        = B.FLFREE                                 
022971        AND    A.FLPRIV        = B.FLPRIV                                 
022980        AND    A.IDBREAK_1     = B.IDBREAK_1                              
022990        AND    A.IDBREAK_2     = B.IDBREAK_2                              
023000                                                                          
023100        ORDER BY A.IDLEGSEL                                               
023300               , A.DAEXDAT                                                
023500               , A.TIEXTID                                                
023700               , A.KDVALISO                                               
023900               , A.IDLANDX3_SEND                                          
024000               , A.IDLEVNR                                                
024100               , A.IDPARTNR                                               
024300               , A.KDFINDOC                                               
024500               , A.FLFREE                                                 
024600               , A.FLPRIV                                                 
024700               , A.IDBREAK_1                                              
024900               , A.IDBREAK_2                                              
025100               , B.IDLOPNR                                                
025200     END-EXEC                                                             
025300                                                                          
025400     EXEC SQL                                                             
025500        OPEN CRS1                                                         
025600     END-EXEC                                                             
025700                                                                          
025800     MOVE SQLCODE TO SQLCODE-WS                                           
025900     PERFORM DB2-STATUS-CHECK                                             
026000     .                                                                    
026100                                                                          
026200 DB2-FETCH-CRS1 SECTION.                                                  
026400     MOVE 000100         TO GOOD-SQLCODES                                 
026500                                                                          
026600     EXEC SQL                                                             
026710       FETCH NEXT ROWSET FROM CRS1 FOR 100 ROWS                           
026800       INTO  :FEED-IDLEGSEL                                               
026900           , :FEED-IDLANDX3-SEND                                          
027000           , :FEED-KDVALISO                                               
027010           , :FEED-IDLEVNR                                                
027100           , :FEED-IDPARTNR                                               
027200           , :FEED-KDFINDOC                                               
027210           , :FEED-FLSOFT                                                 
027300           , :FEED-FLFREE                                                 
027400           , :WS-DAFINDOC                                                 
027500           , :FEED-IDFINDOC                                               
027600           , :FEED-IDSYSTEM-SEND                                          
027700           , :FEED-IDSYSTEM-REC                                           
027800           , :FEED-IDSPRAK                                                
027900           , :FEED-KDBETALV                                               
028000           , :FEED-BEBETVIL                                               
028100           , :FEED-BELEGRAD-1                                             
028200           , :FEED-BELEGRAD-2                                             
028300           , :FEED-ADLEG-STREET                                           
028400           , :FEED-ADLEG-BOX                                              
028500           , :FEED-ADLEG-CITY                                             
028600           , :FEED-ADLEG-PCODE                                            
028700           , :FEED-IDLANDX3-LEG                                           
028800           , :FEED-IDTFN-LEG                                              
028900           , :FEED-IDTFX-LEG                                              
029000           , :FEED-IDMAIL-LEG                                             
029100           , :FEED-BECONT-LEG                                             
029200           , :FEED-IDVAT-LEG                                              
029300           , :FEED-IDBG-LEG                                               
029400           , :FEED-IDPG-LEG                                               
029500           , :FEED-BERESPRA-1                                             
029600           , :FEED-BERESPRA-2                                             
029700           , :FEED-ADRESP-STREET                                          
029800           , :FEED-ADRESP-BOX                                             
029900           , :FEED-ADRESP-CITY                                            
030000           , :FEED-ADRESP-PCODE                                           
030100           , :FEED-IDLANDX3-RESP                                          
030200           , :FEED-IDTFN-RESP                                             
030300           , :FEED-IDTFX-RESP                                             
030400           , :FEED-IDMAIL-RESP                                            
030500           , :FEED-BECONT-RESP                                            
030600           , :FEED-IDVAT-RESP                                             
030700           , :FEED-IDBG-RESP                                              
030800           , :FEED-IDPG-RESP                                              
030900           , :FEED-BEBET-NAME1                                            
031000           , :FEED-BEBET-NAME2                                            
031100           , :FEED-ADBET-STREET                                           
031200           , :FEED-ADBET-BOX                                              
031300           , :FEED-ADBET-CITY                                             
031400           , :FEED-ADBET-PCODE                                            
031500           , :FEED-IDLANDX3-BET                                           
031600           , :FEED-IDVAT-BET                                              
031700           , :FEED-KDTRADP                                                
031800           , :FEED-SUNTO-SERV                                             
031900           , :FEED-SUBTO-SERV                                             
032000           , :FEED-SUNTO-PART                                             
032100           , :FEED-SUBTO-PART                                             
032200           , :FEED-SUNTO-TOT                                              
032300           , :FEED-SUBTO-TOT                                              
032400           , :FEED-SUVAT-BILLIT-TOT                                       
032500           , :FEED-IDVAT-AGENT                                            
032510           , :WS-IDBREAK-2                                                
032600           , :FEED-IDLANDX3-REC                                           
032700           , :WS-IDEXCUST-1                                               
032800           , :WS-IDEXCUST-2                                               
032900           , :WS-IDEXCUST-3                                               
033000           , :FEED-IDBUNDLE                                               
033100           , :FEED-IDREF                                                  
033200           , :WS-DAREFDAT                                                 
033300           , :FEED-BEVOLREF                                               
033400           , :WS-IDOPTION-1                                               
033500           , :WS-IDOPTION-2                                               
033600           , :WS-IDOPTION-3                                               
033700           , :WS-IDOPTION-4                                               
033800           , :WS-IDOPTION-5                                               
033900           , :FEED-IDARTNR-FINANCE                                        
034000           , :FEED-BEART                                                  
034100           , :FEED-IDSTATNR                                               
034200           , :FEED-VKORDBTO-KOLLI                                         
034210           , :FEED-VKARTNTO                                               
034300           , :FEED-KDARTURS                                               
034400           , :FEED-KVBEART                                                
034500           , :FEED-KVLEVART                                               
034600           , :FEED-PRARTBTO                                               
034700           , :FEED-PRARTNTO                                               
034800           , :FEED-REARTRAB                                               
034900           , :FEED-FLSPECPR                                               
035100           , :FEED-KDANMORS                                               
035200           , :FEED-IDFAKREF                                               
035300           , :WS-DAFAKREF                                                 
035400           , :FEED-IDDC                                                   
035411           , :FEED-KDFRAKT                                                
035420           , :FEED-BELEVVIL                                               
035500           , :WS-IDACCNT-1                                                
035600           , :WS-IDACCNT-2                                                
035700           , :WS-IDACCNT-3                                                
035800           , :WS-IDACCNT-4                                                
035900           , :FEED-KDVAT                                                  
036000           , :FEED-REVAT                                                  
036100           , :FEED-SUNTO                                                  
036200           , :FEED-SUVAT-BILLIT                                           
036300           , :FEED-SUBTO                                                  
036400           , :WS-IDREFRAD                                                 
036410           , :FEED-BETEXT                                                 
036411           , :FEED-IDLEVNR-ART                                            
036412           , :FEED-FLPCOO                                                 
036420           , :FEED-KDVALISO-BET                                           
036430           , :WS-FLRATE                                                   
036440           , :FEED-KDPARTTY                                               
036450           , :FEED-KDPARTGR                                               
036460           , :WS-FLDIRVAT                                                 
036470           , :WS-FLLOCCUR                                                 
036480           , :FEED-KDPRMOD                                                
036500     END-EXEC                                                             
036600                                                                          
036700     MOVE SQLCODE TO SQLCODE-WS                                           
036800     PERFORM DB2-STATUS-CHECK                                             
036900     .                                                                    
037000                                                                          
037200 DB2-CLOSE-CRS1 SECTION.                                                  
037400     EXEC SQL                                                             
037500        CLOSE CRS1                                                        
037600     END-EXEC                                                             
037700     .                                                                    
037800                                                                          
037900 DB2-OPEN-CRS-LSEL SECTION.                                               
037910     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
037920     SELECT   T01LSEL.IDLEGSEL                                            
037921             ,T01LSEL.KDTRADP                                             
037930                                                                          
037940     FROM     T01LSEL                                                     
037950                                                                          
037960     WHERE    KDSTATUS = 1                                                
037970     END-EXEC                                                             
037980                                                                          
037990     EXEC SQL OPEN T01LSEL-CRS                                            
037991     END-EXEC                                                             
037992                                                                          
037993     MOVE 000            TO GOOD-SQLCODES                                 
037994     MOVE SQLCODE        TO SQLCODE-WS                                    
037995     PERFORM DB2-STATUS-CHECK                                             
037996     .                                                                    
037997                                                                          
037998 DB2-FETCH-CRS-LSEL SECTION.                                              
037999     EXEC SQL FETCH T01LSEL-CRS INTO                                      
038000            :WS-IDLEGSEL-CRS                                              
038001           ,:WS-KDTRADP                                                   
038010     END-EXEC                                                             
038020                                                                          
038030     MOVE 000100         TO GOOD-SQLCODES                                 
038040     MOVE SQLCODE        TO SQLCODE-WS                                    
038050     PERFORM DB2-STATUS-CHECK                                             
038060     .                                                                    
038070                                                                          
038080 DB2-CLOSE-CRS-LSEL SECTION.                                              
038090     EXEC SQL CLOSE T01LSEL-CRS                                           
038091     END-EXEC                                                             
038092     .                                                                    
038093     EJECT                                                                
038094                                                                          
038095 DB2-SELECT-T01SECO-SND SECTION.                                          
038096     MOVE 000  TO GOOD-SQLCODES                                           
038097                                                                          
038098     EXEC SQL                                                             
038099           SELECT  KDVALISO                                               
038100                                                                          
038101           INTO   :WS-KDVALISO-SND                                        
038102                                                                          
038103           FROM    T01SECO                                                
038104                                                                          
038105           WHERE   IDLANDX3 = :FEED-IDLANDX3-SEND                         
038106           AND     IDLEGSEL = :FEED-IDLEGSEL                              
038107           AND     KDSTATUS = 001                                         
038108     END-EXEC                                                             
038109                                                                          
038110     MOVE SQLCODE TO SQLCODE-WS                                           
038111     PERFORM DB2-STATUS-CHECK                                             
038112     .                                                                    
038113                                                                          
038114 DB2-SELECT-T01RECO     SECTION.                                          
038115     MOVE 000  TO GOOD-SQLCODES                                           
038116                                                                          
038117     EXEC SQL                                                             
038118           SELECT  IDVAT                                                  
038119                                                                          
038120           INTO   :WS-RECO-IDVAT                                          
038121                                                                          
038122           FROM    T01RECO                                                
038123                                                                          
038124           WHERE   IDLANDX3 = :FEED-IDLANDX3-BET                          
038125           AND     IDLEGSEL = :FEED-IDLEGSEL                              
038126           AND     KDSTATUS = 001                                         
038127           AND     DADELDAT = :WS-ACTIVE                                  
038128     END-EXEC                                                             
038129                                                                          
038130     MOVE SQLCODE TO SQLCODE-WS                                           
038131     PERFORM DB2-STATUS-CHECK                                             
038132     .                                                                    
038133                                                                          
038134 DB2-SELECT-MAX-T01CURR-SND SECTION.                                      
038135     MOVE 000            TO GOOD-SQLCODES                                 
038136                                                                          
038137     EXEC SQL                                                             
038138     SELECT   MAX(T01CURR.DASTADAT)                                       
038139                                                                          
038140     INTO     :WS-DASTADAT-KEY                                            
038141                                                                          
038142     FROM     T01CURR                                                     
038143                                                                          
038144     WHERE    T01CURR.IDLEGSEL = :FEED-IDLEGSEL                           
038145     AND     (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
038146     OR       T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
038147     END-EXEC                                                             
038148                                                                          
038149     MOVE SQLCODE        TO SQLCODE-WS                                    
038150     PERFORM DB2-STATUS-CHECK                                             
038151     .                                                                    
038152                                                                          
038153 DB2-SELECT-T01CURR-SND SECTION.                                          
038154     MOVE 000            TO GOOD-SQLCODES                                 
038155                                                                          
038156     EXEC SQL                                                             
038157     SELECT   PRKURS                                                      
038158             ,REVALUTA                                                    
038159                                                                          
038160     INTO     :CURR-PRKURS                                                
038161             ,:WS-REVALUTA-SND                                            
038162                                                                          
038163     FROM     T01CURR                                                     
038164                                                                          
038165     WHERE    IDLEGSEL = :FEED-IDLEGSEL                                   
038166     AND      KDVALISO = :WS-KDVALISO-SND                                 
038167     AND      DASTADAT = :WS-DASTADAT-KEY                                 
038168     END-EXEC                                                             
038169                                                                          
038170     MOVE SQLCODE        TO SQLCODE-WS                                    
038171     PERFORM DB2-STATUS-CHECK                                             
038172     .                                                                    
038173                                                                          
038174 DB2-SELECT-MAX-T01CURR-SND-CRE SECTION.                                  
038175     MOVE 000            TO GOOD-SQLCODES                                 
038176                                                                          
038177     EXEC SQL                                                             
038178     SELECT   MAX(T01CURR.DASTADAT)                                       
038179                                                                          
038180     INTO     :WS2-DASTADAT-CREDIT                                        
038181                                                                          
038182     FROM     T01CURR                                                     
038183                                                                          
038184     WHERE    T01CURR.IDLEGSEL = :FEED-IDLEGSEL                           
038185     AND     (T01CURR.DASTADAT < :WS-DASTADAT-CREDIT                      
038186     OR       T01CURR.DASTADAT = :WS-DASTADAT-CREDIT)                     
038187     END-EXEC                                                             
038188                                                                          
038189     MOVE SQLCODE        TO SQLCODE-WS                                    
038190     PERFORM DB2-STATUS-CHECK                                             
038191     .                                                                    
038192                                                                          
038193 DB2-SELECT-T01CURR-SND-CRE SECTION.                                      
038194     MOVE 000            TO GOOD-SQLCODES                                 
038195                                                                          
038196     EXEC SQL                                                             
038197     SELECT   PRKURS                                                      
038198             ,REVALUTA                                                    
038199                                                                          
038200     INTO     :CURR-PRKURS                                                
038201             ,:WS-REVALUTA-SND                                            
038202                                                                          
038203     FROM     T01CURR                                                     
038204                                                                          
038205     WHERE    IDLEGSEL = :FEED-IDLEGSEL                                   
038206     AND      KDVALISO = :WS-KDVALISO-SND                                 
038207     AND      DASTADAT = :WS2-DASTADAT-CREDIT                             
038208     END-EXEC                                                             
038209                                                                          
038210     MOVE SQLCODE        TO SQLCODE-WS                                    
038211     PERFORM DB2-STATUS-CHECK                                             
038212     .                                                                    
038213                                                                          
038214 DB2-STATUS-CHECK  SECTION.                                               
038220     SET SQLCODE-IX TO 1                                                  
038300     SEARCH GOOD-SQLCODE                                                  
038400       AT END                                                             
038500          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
038600          DELIMITED BY SIZE INTO ERROR-TEXT                               
038700          CALL ABEND USING RKOD-ABEND-DB2                                 
038800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
038900          CONTINUE                                                        
039000     END-SEARCH                                                           
039100     .                                                                    
