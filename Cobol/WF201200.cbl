000100 PROCESS DYNAM                                                            
000220*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000230*                                                                         
000240 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF201200.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/04/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001010*        CREATES INTRASTAT DATA FILE FROM INVOICING.                      
001020*                                                                         
001030*        PGM READS                                                        
001040*        - DB2-TABLE T01PROC                                              
001050*        - DB2-TABLE T01DHEA                                              
001060*        - DB2-TABLE T01DLIN                                              
001070*        - DB2-TABLE T01DOTY                                              
001080*        - DB2-TABLE T01INRE                                              
001090*        - DB2-TABLE T01SECO                                              
001091*        - DB2-TABLE T01RECO                                              
001100*        - DB2-TABLE T01CURR                                              
001110*        - DB2-TABLE T01LSEL                                              
001200*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- INTRASTAT DATA OUTPUT FILE                                 
002410     SELECT WF2012                     ASSIGN TO WF2012D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  WF2012                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003010*01  POST-WF2012 -COPY WF2012   -L.                                       
003100     EJECT                                                                
003110                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003310*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003320 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003330                                                                          
003340* CONSTANTS.                                                              
003350 77  IDPGM                       PIC X(8)    VALUE 'WF201200'.            
003360 77  JA                          PIC X       VALUE 'J'.                   
003370 77  NEJ                         PIC X       VALUE 'N'.                   
003380 77  WS-CURRENT-VERSION          PIC S9(3)   VALUE +001 COMP-3.           
003390 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
003391 77  WS-IDSYSTEM                 PIC X(4)    VALUE 'WF02'.                
003392 77  WS-IDLEGSEL-CRS             PIC X(4)           VALUE SPACE.          
003393 77  WS-IDARTNR-FINANCE          PIC X(50)   VALUE SPACE.                 
003394 77  WS-DAGENS-DATUM             PIC X(8)    VALUE '00000000'.            
003395 77  WS-DAGENS-DATUM-NUM         PIC 9(8).                                
003396                                                                          
003397* WORKING-FIELDS.                                                         
003398 01  WS-MISC-MULTIFETCH.                                                  
003399     03 WS-DATUM                 PIC X(8)   VALUE SPACE.                  
003401     03 WS-KLOCKAN               PIC 9(10)  VALUE ZERO.                   
003410     03 WS-MX                    PIC S9(3)  COMP-3.                       
003420     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
003430     03 WS-IDLEGSEL              PIC X(4).                                
003440     03 WS-IDLANDX3-BET          PIC X(3).                                
003450     03 WS-IDLANDX3-REC          PIC X(3).                                
003451     03 WS-IDLANDX3-SEND         PIC X(3).                                
003452     03 WS-KDVALISO              PIC X(3).                                
003453     03 WS-IDDISTR               PIC X(4).                                
003454     03 WS-DASTADAT-CREDIT       PIC X(8).                                
003455     03 WS2-DASTADAT-CREDIT      PIC X(8).                                
003460                                                                          
003480 01  WS-MISC-TABLES.                                                      
003490     03 WS-DAFINDOC   OCCURS 100 PIC X(8).                                
003491     03 WS-IDLEVNR    OCCURS 100 PIC X(5).                                
003492     03 WS-IDDC       OCCURS 100 PIC X(2).                                
003493                                                                          
004700 01  GENERAL-SUBPROGRAMS.                                                 
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005000                                                                          
005100*    --- PARAMETERS TO ABEND                                              
005300 01  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 01  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005510 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
005600                                                                          
005700 01  ERRTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006201                                                                          
006202*    --- WORK-AREAS FOR OUTPUT-FILE                                       
006205 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
006206                                 'OUTPUT-TAB   '.                         
006207*01  -COPY WF2012T                                                        
006208     EJECT                                                                
006209 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
006210                                 'OUTPUT-AREA  '.                         
006211*01  -COPY WF2012  -PRE WS-                                               
006212     EJECT                                                                
006213                                                                          
006214 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
006215                                                                          
006216*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
006217                                                                          
006218*01  -COPY WWLANDX2                                                       
006219                                                                          
006310*    --- WORK-AREAS FOR DB2-SECTIONS                                      
006330 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
006340*01  -COPY T01PROC    -PRE PROC-                                          
006350                                                                          
006360 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
006370*01  -COPY T01DHEA    -PRE DHEA-                                          
006380                                                                          
006390 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
006391*01  -COPY T01DLIN    -PRE DLIN-                                          
006392                                                                          
006393 01  FILLER                       PIC X(16)  VALUE 'DOTY-TAB   '.         
006394*01  -COPY T01DOTY    -PRE DOTY-                                          
006395                                                                          
006396 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
006397*01  -COPY T01INRE    -PRE INRE-                                          
006398                                                                          
006399 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
006400*01  -COPY T01SECO    -PRE SECO-                                          
006401                                                                          
006402 01  FILLER                       PIC X(16)  VALUE 'RECO-TAB   '.         
006403*01  -COPY T01RECO    -PRE RECO-                                          
006404                                                                          
006405 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
006406*01  -COPY T01CURR    -PRE CURR-                                          
006407                                                                          
006408 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
006409*01  -COPY T01LSEL    -PRE LSEL-                                          
006410     EJECT                                                                
006411                                                                          
006412 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
006413*01  -COPY T01FCUS    -PRE FCUS-                                          
006414     EJECT                                                                
006415                                                                          
006416 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
006417       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
006418                                                                          
006419 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
006420       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
006421                                                                          
006422 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
006423       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
006424                                                                          
006425 01  FILLER                       PIC X(16)  VALUE 'CUST-AREA'.           
006426       EXEC SQL INCLUDE T01DOTY  END-EXEC.                                
006427                                                                          
006428 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
006429       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
006430                                                                          
006431 01  FILLER                       PIC X(16)  VALUE 'SECO-AREA'.           
006432       EXEC SQL INCLUDE T01SECO  END-EXEC.                                
006433                                                                          
006434 01  FILLER                       PIC X(16)  VALUE 'RECO-AREA'.           
006435       EXEC SQL INCLUDE T01RECO  END-EXEC.                                
006436                                                                          
006437 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
006438       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
006439                                                                          
006440 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
006441       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
006442     EJECT                                                                
006443                                                                          
006444 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
006445       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
006446     EJECT                                                                
006447                                                                          
006448 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
006449       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006450*                        **** STATUS-CODE FROM DB2                        
006451                                                                          
006452 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
006453 01  DB2-WS.                                                              
006454   03  SQLCODE-WS                 PIC 9(3)   VALUE ZERO.                  
006455     88  LINES-FOUND                         VALUE 000.                   
006456     88  LINES-MISSING                       VALUE 100.                   
006457     88  RESOURCE-WRONG                      VALUE 904.                   
006458   03  GOOD-SQLCODES.                                                     
006459     05  GOOD-SQLCODE OCCURS 5                                            
006460         INDEXED BY SQLCODE-IX    PIC 999.                                
006461                                                                          
006470 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006900     PERFORM A-INIT                                                       
006910                                                                          
007001     PERFORM DB2-OPEN-CRS-LSEL                                            
007002     PERFORM DB2-FETCH-CRS-LSEL                                           
007003     PERFORM UNTIL LINES-MISSING                                          
007004       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                
007005       MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM            
007006       PERFORM DB2-SELECT-T01PROC-TAB                                     
007007       IF PROC-KDBEH = 'P'                                                
007008         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
007009         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
007010       END-IF                                                             
007011                                                                          
007012       PERFORM DB2-DCL-OPN-CRS1                                           
007020       PERFORM DB2-FETCH-CRS1                                             
007021       IF SQLERRD(3) > 0                                                  
007022         MOVE 000     TO SQLCODE-WS                                       
007023       END-IF                                                             
007030                                                                          
007040       PERFORM UNTIL LINES-MISSING                                        
007042         MOVE SQLERRD(3)                  TO WS-MULTIFETCH                
007044         MOVE ZERO                        TO WS-MX                        
007045         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
007047           ADD +1                         TO WS-MX                        
007048**** INT2 SHOULD NOT BE IN FILE                                           
007049           IF INT-KDFINDOC(WS-MX) = 'INT2'                                
007050             CONTINUE                                                     
007051           ELSE                                                           
007052             MOVE INT-IDLEGSEL      (WS-MX) TO WS-IDLEGSEL                
007053             MOVE INT-IDLANDX3-SEND (WS-MX) TO WS-IDLANDX3-SEND           
007054             MOVE INT-IDLANDX3-BET  (WS-MX) TO WS-IDLANDX3-BET            
007055             MOVE INT-IDLANDX3-REC  (WS-MX) TO WS-IDLANDX3-REC            
007056             MOVE INT-KDVALISO      (WS-MX) TO WS-KDVALISO                
007058             IF INT-IDLANDX3-REC (WS-MX) > SPACE                          
007060               PERFORM DB2-SELECT-T01INRE-RETURN                          
007066               IF LINES-FOUND                                             
007067                 IF INT-IDLANDX3-REC (WS-MX) NOT =                        
007068                    INT-IDLANDX3-SEND (WS-MX)                             
007069                   PERFORM DB2-SELECT-T01RECO                             
007070                   MOVE RECO-IDVAT      TO INT-IDVAT-RESP (WS-MX)         
007071                 END-IF                                                   
007072                 PERFORM DB2-SELECT-T01SECO                               
007073                 IF INT-KDFINDOC(WS-MX) = 'CR'                            
007074                   IF WS-IDLEVNR(WS-MX)(1:4) = '0000'                     
007075                   OR WS-IDLEVNR(WS-MX)(1:4) = '    '                     
007076                     PERFORM DB2-SELECT-T01CURR-MAX                       
007077                     PERFORM DB2-SELECT-T01CURR                           
007078                   ELSE                                                   
007079                     MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                 
007080          MOVE WS-IDLEVNR(WS-MX)(1:4) TO WS-DASTADAT-CREDIT(3:4)          
007081                     MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                 
007082                     PERFORM DB2-SELECT-T01CURR-MAX-CREDIT                
007083                     PERFORM DB2-SELECT-T01CURR-CREDIT                    
007084                   END-IF                                                 
007085                 ELSE                                                     
007086                   PERFORM DB2-SELECT-T01CURR-MAX                         
007087                   PERFORM DB2-SELECT-T01CURR                             
007088                 END-IF                                                   
007089                 PERFORM S11-WRITE-WF20X2                                 
007090               END-IF                                                     
007091             ELSE                                                         
007092               PERFORM DB2-SELECT-T01INRE                                 
007093               IF LINES-FOUND                                             
007100                 PERFORM DB2-SELECT-T01SECO                               
007103                 PERFORM DB2-SELECT-T01CURR-MAX                           
007104                 PERFORM DB2-SELECT-T01CURR                               
007106                 PERFORM S11-WRITE-WF20X2                                 
007107               END-IF                                                     
007108             END-IF                                                       
007130           END-IF                                                         
007201         END-PERFORM                                                      
007202         IF WS-MULTIFETCH = 100                                           
007210           PERFORM DB2-FETCH-CRS1                                         
007220           IF SQLERRD(3) > 0                                              
007230             MOVE 000                       TO SQLCODE-WS                 
007231           END-IF                                                         
007232         ELSE                                                             
007233           MOVE 100     TO SQLCODE-WS                                     
007240         END-IF                                                           
007300       END-PERFORM                                                        
007400       PERFORM DB2-CLOSE-CRS1                                             
008110                                                                          
008120       PERFORM DB2-FETCH-CRS-LSEL                                         
008130     END-PERFORM                                                          
008140     PERFORM DB2-CLOSE-CRS-LSEL                                           
008150                                                                          
008200     PERFORM Z-FINIT                                                      
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008710                                                                          
008800 A-INIT SECTION.                                                          
009010     OPEN OUTPUT WF2012                                                   
009021                                                                          
009030     INITIALIZE INT-WF2012T                                               
009031                                                                          
009040     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
009050     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
009400     .                                                                    
009500                                                                          
009600 Z-FINIT SECTION.                                                         
009710     CLOSE WF2012                                                         
009900     .                                                                    
010000                                                                          
010102 S11-WRITE-WF20X2 SECTION.                                                
010104     MOVE PROC-DAEXDAT                TO WS-INT-DAEXDAT                   
010105     MOVE PROC-TIEXTID                TO WS-INT-TIEXTID                   
010107     MOVE WS-DAFINDOC         (WS-MX) TO WS-INT-DAFINDOC                  
010108     MOVE SECO-KDVALISO               TO WS-INT-KDVALISO-SEND             
010109     MOVE INT-IDLEGSEL        (WS-MX) TO WS-INT-IDLEGSEL                  
010110     MOVE INT-IDLANDX3-SEND   (WS-MX) TO WS-INT-IDLANDX3-SEND             
010111     MOVE INT-IDLANDX3-BET    (WS-MX) TO WS-INT-IDLANDX3-BET              
010112     MOVE INT-IDLANDX3-REC    (WS-MX) TO WS-INT-IDLANDX3-REC              
010113     MOVE INT-KDVALISO        (WS-MX) TO WS-INT-KDVALISO                  
010114     MOVE INT-PRKURS          (WS-MX) TO WS-INT-PRKURS                    
010115     MOVE INT-KDFINDOC        (WS-MX) TO WS-INT-KDFINDOC                  
010116     MOVE INT-IDFINDOC        (WS-MX) TO WS-INT-IDFINDOC                  
010117     MOVE INT-IDPARTNR        (WS-MX) TO WS-INT-IDPARTNR                  
010118     MOVE INT-IDEXCUST-1      (WS-MX) TO WS-INT-IDEXCUST-1                
010119     MOVE INT-IDEXCUST-2      (WS-MX) TO WS-INT-IDEXCUST-2                
010120**** HERE WE HAVE A FIX FOR TIS                                           
010121     IF INT-KDFINDOC(WS-MX) = 'INV2'                                      
010122       MOVE SPACE TO INT-IDEXCUST-3(WS-MX)                                
010123     END-IF                                                               
010124     MOVE INT-IDEXCUST-3      (WS-MX) TO WS-INT-IDEXCUST-3                
010125     MOVE INT-IDARTNR-FINANCE (WS-MX) TO WS-INT-IDARTNR-FINANCE           
010126     MOVE INT-BEART           (WS-MX) TO WS-INT-BEART                     
010127     MOVE INT-IDSTATNR        (WS-MX) TO WS-INT-IDSTATNR                  
010128     MOVE INT-VKARTNTO        (WS-MX) TO WS-INT-VKARTNTO                  
010129     MOVE INT-KVLEVART        (WS-MX) TO WS-INT-KVLEVART                  
010130     MOVE INT-KDARTURS        (WS-MX) TO WS-INT-KDARTURS                  
010131     MOVE INT-KDFRAKT         (WS-MX) TO WS-INT-KDFRAKT                   
010132     MOVE INT-BELEVVIL        (WS-MX) TO WS-INT-BELEVVIL                  
010133     MOVE INT-SUNTO           (WS-MX) TO WS-INT-SUNTO                     
010134     MOVE INT-IDVAT-LEG       (WS-MX) TO WS-INT-IDVAT-LEG                 
010135     MOVE INT-IDVAT-BET       (WS-MX) TO WS-INT-IDVAT-BET                 
010136     MOVE INT-IDVAT-RESP      (WS-MX) TO WS-INT-IDVAT-RESP                
010137     MOVE INT-IDVAT-AGENT     (WS-MX) TO WS-INT-IDVAT-AGENT               
010138     COMPUTE WS-INT-PRKURS-SEND ROUNDED =                                 
010139             CURR-PRKURS / CURR-REVALUTA                                  
010140     END-COMPUTE                                                          
010141                                                                          
010142     IF INT-IDLEGSEL (WS-MX) = 'VCCS'                                     
010143**** DDGS TO WHERE WE ARE VAT REGISTERED SHOULD BE IN                     
010144**** INTRASTAT FILE BUT WITH RECIEVING COUNTRY AS SENDING                 
010145**** COUNTRY                                                              
010146       MOVE WS-IDDC(WS-MX)       TO LANDX2-IDLANDX2                       
010147       IF LANDX2-EU-IDLANDX2                                              
010148         MOVE INT-IDLANDX3-BET (WS-MX) TO LANDX2-IDLANDX2                 
010149         IF LANDX2-EU-IDLANDX2                                            
010150           PERFORM DB2-SELECT-T01FCUS                                     
010151           IF FCUS-FLDIRVAT = 'J'                                         
010152             MOVE INT-IDLANDX3-BET(WS-MX) TO WS-INT-IDLANDX3-SEND         
010153             MOVE INT-IDLANDX3-BET(WS-MX) TO WS-INT-IDLANDX3-REC          
010154             MOVE INT-IDLANDX3-BET(WS-MX) TO WS-IDLANDX3-REC              
010155             PERFORM DB2-SELECT-T01RECO                                   
010156             MOVE RECO-IDVAT              TO WS-INT-IDVAT-RESP            
010158             WRITE POST-WF2012 FROM WS-INT-WF2012                         
010159           ELSE                                                           
010160             IF WS-INT-IDEXCUST-1(1:4) NUMERIC                            
010161****       THE REFILL BOUNCE FLOW SHOULD NOT CREATE INTRASTAT             
010162               MOVE WS-INT-IDEXCUST-1(1:4) TO TEST-IDDISTR                
010163               IF DIST35-NONVCC-NONVCC-REFILL                             
010164               OR DIST35-NONVCC-NONVCC-TRANSFER                           
010165                 CONTINUE                                                 
010166               ELSE                                                       
010167                 WRITE POST-WF2012 FROM WS-INT-WF2012                     
010168               END-IF                                                     
010169             ELSE                                                         
010170               WRITE POST-WF2012 FROM WS-INT-WF2012                       
010171             END-IF                                                       
010172           END-IF                                                         
010173         ELSE                                                             
010174           IF WS-INT-IDEXCUST-1(1:4) NUMERIC                              
010175****     THE REFILL BOUNCE FLOW SHOULD NOT CREATE INTRASTAT               
010176             MOVE WS-INT-IDEXCUST-1(1:4) TO TEST-IDDISTR                  
010177             IF DIST35-NONVCC-NONVCC-REFILL                               
010178             OR DIST35-NONVCC-NONVCC-TRANSFER                             
010179               CONTINUE                                                   
010180             ELSE                                                         
010181               WRITE POST-WF2012 FROM WS-INT-WF2012                       
010182             END-IF                                                       
010183           ELSE                                                           
010184             WRITE POST-WF2012 FROM WS-INT-WF2012                         
010185           END-IF                                                         
010186         END-IF                                                           
010187       ELSE                                                               
010188         IF WS-INT-IDEXCUST-1(1:4) NUMERIC                                
010189****   THE REFILL BOUNCE FLOW SHOULD NOT CREATE INTRASTAT                 
010190           MOVE WS-INT-IDEXCUST-1(1:4) TO TEST-IDDISTR                    
010191           IF DIST35-NONVCC-NONVCC-REFILL                                 
010192           OR DIST35-NONVCC-NONVCC-TRANSFER                               
010193             CONTINUE                                                     
010194           ELSE                                                           
010195             WRITE POST-WF2012 FROM WS-INT-WF2012                         
010196           END-IF                                                         
010197         ELSE                                                             
010198           WRITE POST-WF2012 FROM WS-INT-WF2012                           
010199         END-IF                                                           
010200       END-IF                                                             
010201     END-IF                                                               
010202     .                                                                    
010203                                                                          
010204* --- DB2 SECTIONS  ---                                                   
010205*                                                                         
010206 DB2-SELECT-T01PROC-TAB   SECTION.                                        
010207     MOVE 000    TO GOOD-SQLCODES                                         
010208                                                                          
010209     EXEC SQL                                                             
010210           SELECT  IDLEGSEL                                               
010220                ,  DAEXDAT                                                
010300                ,  TIEXTID                                                
010310                ,  KDBEH                                                  
010400                                                                          
010500           INTO   :PROC-IDLEGSEL                                          
010510                , :PROC-DAEXDAT                                           
010600                , :PROC-TIEXTID                                           
010610                , :PROC-KDBEH                                             
010700                                                                          
010800           FROM    T01PROC                                                
010900                                                                          
011000           WHERE   IDSYSTEM = :WS-IDSYSTEM     AND                        
011010                   IDLEGSEL = :WS-IDLEGSEL-CRS                            
011100     END-EXEC                                                             
011200                                                                          
011300     MOVE SQLCODE TO SQLCODE-WS                                           
011400     PERFORM DB2-STATUS-CHECK                                             
011500     .                                                                    
011600                                                                          
011700 DB2-SELECT-T01INRE-RETURN SECTION.                                       
011900     MOVE 000100 TO GOOD-SQLCODES                                         
012000                                                                          
012100     EXEC SQL                                                             
012200           SELECT  FLINTREP                                               
012300                                                                          
012400           INTO   :INRE-FLINTREP                                          
012500                                                                          
012600           FROM    T01INRE                                                
012700                                                                          
012800           WHERE   IDLEGSEL      = :WS-IDLEGSEL                           
012900           AND     IDLANDX3_SEND = :WS-IDLANDX3-BET                       
013000           AND     IDLANDX3_REC  = :WS-IDLANDX3-REC                       
013010           AND     FLINTREP      = :JA                                    
013100           AND     KDSTATUS      = :WS-CURRENT-VERSION                    
013200           AND     DADELDAT      = :WS-ACTIVE                             
013300     END-EXEC                                                             
013400                                                                          
013500     MOVE SQLCODE TO SQLCODE-WS                                           
013600     PERFORM DB2-STATUS-CHECK                                             
013700     .                                                                    
013800                                                                          
013900 DB2-SELECT-T01INRE SECTION.                                              
014100     MOVE 000100 TO GOOD-SQLCODES                                         
014200                                                                          
014300     EXEC SQL                                                             
014400           SELECT  FLINTREP                                               
014500                                                                          
014600           INTO   :INRE-FLINTREP                                          
014700                                                                          
014800           FROM    T01INRE                                                
014900                                                                          
015000           WHERE   IDLEGSEL      = :WS-IDLEGSEL                           
015100           AND     IDLANDX3_SEND = :WS-IDLANDX3-SEND                      
015200           AND     IDLANDX3_REC  = :WS-IDLANDX3-BET                       
015210           AND     FLINTREP      = :JA                                    
015300           AND     KDSTATUS      = :WS-CURRENT-VERSION                    
015400           AND     DADELDAT      = :WS-ACTIVE                             
015500     END-EXEC                                                             
015600                                                                          
015700     MOVE SQLCODE TO SQLCODE-WS                                           
015800     PERFORM DB2-STATUS-CHECK                                             
015900     .                                                                    
016000                                                                          
016010 DB2-SELECT-T01SECO SECTION.                                              
016030     MOVE 000    TO GOOD-SQLCODES                                         
016040                                                                          
016050     EXEC SQL                                                             
016060           SELECT  KDVALISO                                               
016070                                                                          
016080           INTO   :SECO-KDVALISO                                          
016090                                                                          
016091           FROM    T01SECO                                                
016092                                                                          
016093           WHERE   IDLEGSEL = :WS-IDLEGSEL                                
016094           AND     IDLANDX3 = :WS-IDLANDX3-SEND                           
016096           AND     KDSTATUS = :WS-CURRENT-VERSION                         
016097           AND     DADELDAT = :WS-ACTIVE                                  
016098     END-EXEC                                                             
016099                                                                          
016100     MOVE SQLCODE TO SQLCODE-WS                                           
016101     PERFORM DB2-STATUS-CHECK                                             
016102     .                                                                    
016103                                                                          
016104 DB2-SELECT-T01RECO SECTION.                                              
016105     MOVE 000    TO GOOD-SQLCODES                                         
016106                                                                          
016107     EXEC SQL                                                             
016108           SELECT  IDVAT                                                  
016109                                                                          
016110           INTO   :RECO-IDVAT                                             
016111                                                                          
016112           FROM    T01RECO                                                
016113                                                                          
016114           WHERE   IDLEGSEL = :WS-IDLEGSEL                                
016115           AND     IDLANDX3 = :WS-IDLANDX3-REC                            
016116           AND     KDSTATUS = :WS-CURRENT-VERSION                         
016117           AND     DADELDAT = :WS-ACTIVE                                  
016118     END-EXEC                                                             
016119                                                                          
016120     MOVE SQLCODE TO SQLCODE-WS                                           
016121     PERFORM DB2-STATUS-CHECK                                             
016122     .                                                                    
016123                                                                          
016124 DB2-SELECT-T01FCUS SECTION.                                              
016125     MOVE 000    TO GOOD-SQLCODES                                         
016126                                                                          
016127     EXEC SQL                                                             
016128           SELECT  FLDIRVAT                                               
016129                                                                          
016130           INTO   :FCUS-FLDIRVAT                                          
016131                                                                          
016132           FROM    T01FCUS                                                
016133                                                                          
016134           WHERE   IDLEGSEL = :WS-IDLEGSEL                                
016135           AND     IDPARTNR = :WS-INT-IDPARTNR                            
016136           AND     KDSTATUS = :WS-CURRENT-VERSION                         
016138     END-EXEC                                                             
016139                                                                          
016140     MOVE SQLCODE TO SQLCODE-WS                                           
016141     PERFORM DB2-STATUS-CHECK                                             
016142     .                                                                    
016143                                                                          
016144 DB2-SELECT-T01CURR-MAX SECTION.                                          
016145     MOVE 000    TO GOOD-SQLCODES                                         
016146                                                                          
016147     EXEC SQL                                                             
016148           SELECT  MAX(DASTADAT)                                          
016149                                                                          
016150           INTO   :CURR-DASTADAT                                          
016151                                                                          
016152           FROM    T01CURR                                                
016153                                                                          
016154           WHERE   IDLEGSEL  = :WS-IDLEGSEL                               
016155           AND     KDVALISO  = :WS-KDVALISO                               
016156           AND     DASTADAT <= :WS-DAGENS-DATUM                           
016157     END-EXEC                                                             
016158                                                                          
016159     MOVE SQLCODE TO SQLCODE-WS                                           
016160     PERFORM DB2-STATUS-CHECK                                             
016161     .                                                                    
016162                                                                          
016163 DB2-SELECT-T01CURR SECTION.                                              
016164     MOVE 000    TO GOOD-SQLCODES                                         
016165                                                                          
016166     EXEC SQL                                                             
016167           SELECT  PRKURS                                                 
016168                  ,REVALUTA                                               
016169                                                                          
016170           INTO   :CURR-PRKURS                                            
016171                 ,:CURR-REVALUTA                                          
016172                                                                          
016173           FROM    T01CURR                                                
016174                                                                          
016175           WHERE   IDLEGSEL    = :WS-IDLEGSEL                             
016176           AND     KDVALISO    = :WS-KDVALISO                             
016177           AND     DASTADAT    = :CURR-DASTADAT                           
016178     END-EXEC                                                             
016179                                                                          
016180     MOVE SQLCODE TO SQLCODE-WS                                           
016181     PERFORM DB2-STATUS-CHECK                                             
016182     .                                                                    
016183                                                                          
016184 DB2-SELECT-T01CURR-MAX-CREDIT SECTION.                                   
016185     MOVE 000    TO GOOD-SQLCODES                                         
016186                                                                          
016187     EXEC SQL                                                             
016188           SELECT  MAX(DASTADAT)                                          
016189                                                                          
016190           INTO   :WS2-DASTADAT-CREDIT                                    
016191                                                                          
016192           FROM    T01CURR                                                
016193                                                                          
016194           WHERE   IDLEGSEL  = :WS-IDLEGSEL                               
016195           AND     KDVALISO  = :WS-KDVALISO                               
016196           AND     DASTADAT <= :WS-DASTADAT-CREDIT                        
016197     END-EXEC                                                             
016198                                                                          
016199     MOVE SQLCODE TO SQLCODE-WS                                           
016200     PERFORM DB2-STATUS-CHECK                                             
016201     .                                                                    
016202                                                                          
016203 DB2-SELECT-T01CURR-CREDIT SECTION.                                       
016204     MOVE 000    TO GOOD-SQLCODES                                         
016205                                                                          
016206     EXEC SQL                                                             
016207           SELECT  PRKURS                                                 
016208                  ,REVALUTA                                               
016209                                                                          
016210           INTO   :CURR-PRKURS                                            
016211                 ,:CURR-REVALUTA                                          
016212                                                                          
016213           FROM    T01CURR                                                
016214                                                                          
016215           WHERE   IDLEGSEL    = :WS-IDLEGSEL                             
016216           AND     KDVALISO    = :WS-KDVALISO                             
016217           AND     DASTADAT    = :WS2-DASTADAT-CREDIT                     
016218     END-EXEC                                                             
016219                                                                          
016220     MOVE SQLCODE TO SQLCODE-WS                                           
016221     PERFORM DB2-STATUS-CHECK                                             
016222     .                                                                    
016223                                                                          
016230 DB2-DCL-OPN-CRS1 SECTION.                                                
016300     MOVE 000100 TO GOOD-SQLCODES                                         
016400                                                                          
016500     EXEC SQL                                                             
016610        DECLARE CRS1 CURSOR WITH ROWSET POSITIONING FOR                   
016700        SELECT A.IDLEGSEL                                                 
016800             , A.IDLANDX3_SEND                                            
016900             , A.IDLANDX3_BET                                             
017000             , B.IDLANDX3_REC                                             
017100             , A.KDVALISO                                                 
017200             , A.PRKURS                                                   
017300             , A.IDPARTNR                                                 
017400             , A.KDFINDOC                                                 
017500             , A.DAFINDOC                                                 
017600             , A.IDFINDOC                                                 
017700             , B.IDEXCUST_1                                               
017701             , B.IDEXCUST_2                                               
017702             , B.IDEXCUST_3                                               
017710             , B.IDARTNR_FINANCE                                          
017800             , B.BEART                                                    
017900             , B.IDSTATNR                                                 
018000             , B.VKARTNTO                                                 
018100             , B.KVLEVART                                                 
018200             , B.SUNTO                                                    
018210             , B.KDARTURS                                                 
018220             , B.KDFRAKT                                                  
018230             , B.BELEVVIL                                                 
018240             , B.IDLEVNR                                                  
018241             , B.IDDC                                                     
018250             , A.IDVAT_LEG                                                
018260             , A.IDVAT_RESP                                               
018270             , A.IDVAT_BET                                                
018280             , A.IDVAT_AGENT                                              
018300                                                                          
018400        FROM   T01DHEA A                                                  
018500             , T01DLIN B                                                  
018600             , T01DOTY C                                                  
018700                                                                          
018800        WHERE  A.IDLEGSEL        = :PROC-IDLEGSEL                         
018810        AND    A.DAEXDAT         = :PROC-DAEXDAT                          
018900        AND    A.TIEXTID         = :PROC-TIEXTID                          
019000        AND    A.IDLEGSEL        = B.IDLEGSEL                             
019010        AND    A.DAEXDAT         = B.DAEXDAT                              
019100        AND    A.TIEXTID         = B.TIEXTID                              
019110        AND    A.KDVALISO        = B.KDVALISO                             
019120        AND    A.IDLANDX3_SEND   = B.IDLANDX3_SEND                        
019130        AND    A.IDLEVNR         = B.IDLEVNR                              
019140        AND    A.IDPARTNR        = B.IDPARTNR                             
019150        AND    A.KDFINDOC        = B.KDFINDOC                             
019160        AND    A.FLSOFT          = B.FLSOFT                               
019170        AND    A.FLFREE          = B.FLFREE                               
019171        AND    A.FLPRIV          = B.FLPRIV                               
019180        AND    A.IDBREAK_1       = B.IDBREAK_1                            
019190        AND    A.IDBREAK_2       = B.IDBREAK_2                            
019200        AND    A.FLSOFT          = :NEJ                                   
019210        AND    B.IDARTNR_FINANCE > :WS-IDARTNR-FINANCE                    
019300        AND    C.IDLEGSEL        = A.IDLEGSEL                             
019400        AND    C.KDFINDOC        = A.KDFINDOC                             
019600        AND    C.FLINTREP        = :JA                                    
019610        AND    C.KDSTATUS        = :WS-CURRENT-VERSION                    
019700        AND    C.DADELDAT        = :WS-ACTIVE                             
019800                                                                          
019900        ORDER BY A.IDLEGSEL                                               
020100               , A.DAEXDAT                                                
020300               , A.TIEXTID                                                
020500               , A.KDVALISO                                               
020700               , A.IDLANDX3_SEND                                          
020900               , A.IDLEVNR                                                
021000               , A.IDPARTNR                                               
021100               , A.KDFINDOC                                               
021200               , A.FLSOFT                                                 
021300               , A.FLFREE                                                 
021310               , A.FLPRIV                                                 
021500               , A.IDBREAK_1                                              
021700               , A.IDBREAK_2                                              
021900               , B.IDLOPNR                                                
022000     END-EXEC                                                             
022100                                                                          
022200     EXEC SQL                                                             
022300        OPEN CRS1                                                         
022400     END-EXEC                                                             
022500                                                                          
022600     MOVE SQLCODE TO SQLCODE-WS                                           
022700     PERFORM DB2-STATUS-CHECK                                             
022800     .                                                                    
022900                                                                          
023000 DB2-FETCH-CRS1 SECTION.                                                  
023200     MOVE 000100         TO GOOD-SQLCODES                                 
023300                                                                          
023400     EXEC SQL                                                             
023510       FETCH NEXT ROWSET FROM CRS1 FOR 100 ROWS                           
023600       INTO  :INT-IDLEGSEL                                                
023700           , :INT-IDLANDX3-SEND                                           
023800           , :INT-IDLANDX3-BET                                            
023900           , :INT-IDLANDX3-REC                                            
024000           , :INT-KDVALISO                                                
024100           , :INT-PRKURS                                                  
024200           , :INT-IDPARTNR                                                
024300           , :INT-KDFINDOC                                                
024400           , :WS-DAFINDOC                                                 
024500           , :INT-IDFINDOC                                                
024510           , :INT-IDEXCUST-1                                              
024520           , :INT-IDEXCUST-2                                              
024530           , :INT-IDEXCUST-3                                              
024600           , :INT-IDARTNR-FINANCE                                         
024700           , :INT-BEART                                                   
024800           , :INT-IDSTATNR                                                
024900           , :INT-VKARTNTO                                                
025000           , :INT-KVLEVART                                                
025100           , :INT-SUNTO                                                   
025110           , :INT-KDARTURS                                                
025120           , :INT-KDFRAKT                                                 
025130           , :INT-BELEVVIL                                                
025140           , :WS-IDLEVNR                                                  
025141           , :WS-IDDC                                                     
025150           , :INT-IDVAT-LEG                                               
025151           , :INT-IDVAT-RESP                                              
025152           , :INT-IDVAT-BET                                               
025153           , :INT-IDVAT-AGENT                                             
025200     END-EXEC                                                             
025300                                                                          
025400     MOVE SQLCODE TO SQLCODE-WS                                           
025500     PERFORM DB2-STATUS-CHECK                                             
025600     .                                                                    
025700                                                                          
025900 DB2-CLOSE-CRS1 SECTION.                                                  
026100     EXEC SQL                                                             
026200        CLOSE CRS1                                                        
026300     END-EXEC                                                             
026400     .                                                                    
026500                                                                          
026510 DB2-OPEN-CRS-LSEL SECTION.                                               
026520     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
026530     SELECT   T01LSEL.IDLEGSEL                                            
026540                                                                          
026550     FROM     T01LSEL                                                     
026560                                                                          
026570     WHERE    KDSTATUS = 1                                                
026580     END-EXEC                                                             
026590                                                                          
026591     EXEC SQL OPEN T01LSEL-CRS                                            
026592     END-EXEC                                                             
026593                                                                          
026594     MOVE 000            TO GOOD-SQLCODES                                 
026595     MOVE SQLCODE        TO SQLCODE-WS                                    
026596     PERFORM DB2-STATUS-CHECK                                             
026597     .                                                                    
026598     EJECT                                                                
026599                                                                          
026600 DB2-FETCH-CRS-LSEL SECTION.                                              
026610     EXEC SQL FETCH T01LSEL-CRS INTO                                      
026620            :WS-IDLEGSEL-CRS                                              
026630     END-EXEC                                                             
026640                                                                          
026650     MOVE 000100         TO GOOD-SQLCODES                                 
026660     MOVE SQLCODE        TO SQLCODE-WS                                    
026670     PERFORM DB2-STATUS-CHECK                                             
026680     .                                                                    
026690     EJECT                                                                
026691                                                                          
026692 DB2-CLOSE-CRS-LSEL SECTION.                                              
026693     EXEC SQL CLOSE T01LSEL-CRS                                           
026694     END-EXEC                                                             
026695     .                                                                    
026696     EJECT                                                                
026697                                                                          
026700 DB2-STATUS-CHECK  SECTION.                                               
026800     SET SQLCODE-IX TO 1                                                  
026900     SEARCH GOOD-SQLCODE                                                  
027000       AT END                                                             
027100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
027200          DELIMITED BY SIZE INTO ERROR-TEXT                               
027310          CALL ABEND USING RKOD-ABEND-DB2                                 
027400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
027500          CONTINUE                                                        
027600     END-SEARCH                                                           
027700     .                                                                    
