000100 PROCESS DYNAM                                                            
000220*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000230*                                                                         
000240 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF201800.                                                
000400 AUTHOR.         HAMMARIN BO.                                             
000500 DATE-WRITTEN.   NOV 2003.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001010*        CREATES CUSTOMS DATA FILE FROM INVOICING.                        
001020*                                                                         
001030*        PGM READS                                                        
001040*        - DB2-TABLE T01PROC                                              
001050*        - DB2-TABLE T01DHEA                                              
001060*        - DB2-TABLE T01DLIN                                              
001070*        - DB2-TABLE T01DOTY                                              
001080*        - DB2-TABLE T01INRE                                              
001090*        - DB2-TABLE T01SECO                                              
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
002402*          --- CUSTOMS DATA OUTPUT FILE                                   
002410     SELECT WF2018                     ASSIGN TO WF2018D1.                
002420                                                                          
002430     SELECT WF2028                     ASSIGN TO WF2018D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  WF2018                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003010*01  POST-WF2018 -COPY WF2018   -L.                                       
003011                                                                          
003020 FD  WF2028                                                               
003030     RECORDING       F                                                    
003040     BLOCK CONTAINS  0.                                                   
003050                                                                          
003060*01  POST-WF2028 -COPY WF2018   -L.                                       
003100     EJECT                                                                
003110                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003310*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003320 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003330                                                                          
003340* CONSTANTS.                                                              
003350 77  IDPGM                       PIC X(8)    VALUE 'WF201800'.            
003360 77  JA                          PIC X       VALUE 'J'.                   
003370 77  NEJ                         PIC X       VALUE 'N'.                   
003380 77  WS-CURRENT-VERSION          PIC S9(3)   VALUE +001 COMP-3.           
003390 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
003391 77  WS-IDSYSTEM                 PIC X(4)    VALUE 'WF02'.                
003392 77  WS-IDLEGSEL-CRS             PIC X(4).                                
003393 77  WS-IDARTNR-FINANCE          PIC X(50)   VALUE SPACE.                 
003394 77  WS-IDLEVNR2                 PIC X(5)    VALUE SPACE.                 
003396 77  WS-DAGENS-DATUM             PIC X(8)    VALUE '00000000'.            
003397 77  WS-DAGENS-DATUM-NUM         PIC 9(8).                                
003398                                                                          
003399 01  WS-MISC-MULTIFETCH.                                                  
003400     03 WS-DATUM                 PIC X(8)   VALUE SPACE.                  
003401     03 WS-KLOCKAN               PIC 9(10)  VALUE ZERO.                   
003402     03 WS-MX                    PIC S9(3)  COMP-3.                       
003403     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
003404     03 WS-IDLEGSEL              PIC X(4).                                
003405     03 WS-IDLANDX3-SEND         PIC X(3).                                
003406     03 WS-IDLANDX3-BET          PIC X(3).                                
003407     03 WS-IDLANDX3-REC          PIC X(3).                                
003408     03 WS-KDVALISO              PIC X(3).                                
003409     03 WS-DASTADAT-CREDIT       PIC X(8).                                
003410     03 WS2-DASTADAT-CREDIT      PIC X(8).                                
003411     03 WS-IDLEVNR-CREDIT        PIC X(5).                                
003420                                                                          
003500 01  WS-MISC-TABLES.                                                      
003600     03  WS-DAFINDOC OCCURS 100  PIC X(8).                                
003700     03  WS-IDLEVNR  OCCURS 100  PIC X(5).                                
004600                                                                          
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
006203 01  OUTPUT-AREA-T               PIC X(24)   VALUE                        
006204                                 'OUTPUT-TAB   '.                         
006220*01  -COPY WF2018T                                                        
006300     EJECT                                                                
006301 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
006302                                 'OUTPUT-AREA  '.                         
006303*01  -COPY WF2018  -PRE WS-                                               
006304     EJECT                                                                
006310*    --- WORK-AREAS FOR DB2-SECTIONS                                      
006330 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
006340*01  -COPY T01PROC -PRE PROC-                                             
006350                                                                          
006360 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
006372*01  -COPY T01DHEA -PRE DHEA-                                             
006380                                                                          
006390 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
006393*01  -COPY T01DLIN -PRE DLIN-                                             
006394                                                                          
006395 01  FILLER                       PIC X(16)  VALUE 'DOTY-TAB   '.         
006398*01  -COPY T01DOTY -PRE DOTY-                                             
006399                                                                          
006400 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
006401*01  -COPY T01INRE -PRE INRE-                                             
006402                                                                          
006403 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
006404*01  -COPY T01SECO -PRE SECO-                                             
006405                                                                          
006406 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
006407*01  -COPY T01CURR -PRE CURR-                                             
006408                                                                          
006409 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
006410*01  -COPY T01LSEL -PRE LSEL-                                             
006411     EJECT                                                                
006412                                                                          
006413 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
006414       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
006415                                                                          
006416 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
006417       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
006418                                                                          
006419 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
006420       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
006421                                                                          
006422 01  FILLER                       PIC X(16)  VALUE 'CUST-AREA'.           
006423       EXEC SQL INCLUDE T01DOTY  END-EXEC.                                
006424                                                                          
006425 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
006426       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
006427                                                                          
006428 01  FILLER                       PIC X(16)  VALUE 'SECO-AREA'.           
006429       EXEC SQL INCLUDE T01SECO  END-EXEC.                                
006430                                                                          
006431 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
006432       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
006433                                                                          
006434 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
006435       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
006436     EJECT                                                                
006437                                                                          
006438 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
006439       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006440*                        **** STATUS-CODE FROM DB2                        
006441                                                                          
006442 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
006443 01  DB2-WS.                                                              
006444   03  SQLCODE-WS                 PIC  9(3)  VALUE ZERO.                  
006445     88  LINES-FOUND                         VALUE 000.                   
006446     88  LINES-MISSING                       VALUE 100.                   
006447     88  RESOURCE-WRONG                      VALUE 904.                   
006448   03  GOOD-SQLCODES.                                                     
006449     05  GOOD-SQLCODE OCCURS 5                                            
006450         INDEXED BY SQLCODE-IX    PIC 999.                                
006451                                                                          
006460 PROCEDURE DIVISION.                                                      
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
007041         MOVE SQLERRD(3) TO WS-MULTIFETCH                                 
007042         MOVE ZERO       TO WS-MX                                         
007043         PERFORM UNTIL WS-MX = WS-MULTIFETCH                              
007044           ADD +1        TO WS-MX                                         
007045**** INT2 SHOULD NOT BE IN FILE                                           
007046           IF CUS-KDFINDOC(WS-MX) = 'INT2'                                
007047             CONTINUE                                                     
007048           ELSE                                                           
007049             MOVE CUS-IDLEGSEL    (WS-MX) TO WS-IDLEGSEL                  
007050             MOVE CUS-IDLANDX3-SEND (WS-MX) TO WS-IDLANDX3-SEND           
007051             MOVE CUS-IDLANDX3-BET (WS-MX) TO WS-IDLANDX3-BET             
007052             MOVE CUS-IDLANDX3-REC (WS-MX) TO WS-IDLANDX3-REC             
007053             MOVE CUS-KDVALISO    (WS-MX) TO WS-KDVALISO                  
007054             IF CUS-IDLANDX3-REC  (WS-MX) > SPACE                         
007060               PERFORM DB2-SELECT-T01INRE-RETURN                          
007070             ELSE                                                         
007080               PERFORM DB2-SELECT-T01INRE                                 
007090             END-IF                                                       
007091             IF LINES-FOUND                                               
007092               PERFORM DB2-SELECT-T01SECO                                 
007093               IF CUS-KDFINDOC (WS-MX) = 'CR'                             
007094                 IF WS-IDLEVNR (WS-MX)(1:4) = '0000'                      
007095                 OR WS-IDLEVNR (WS-MX)(1:4) = '    '                      
007099                   PERFORM DB2-SELECT-T01CURR-MAX                         
007100                   PERFORM DB2-SELECT-T01CURR                             
007101                 ELSE                                                     
007102                   MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                   
007103         MOVE WS-IDLEVNR (WS-MX)(1:4) TO WS-DASTADAT-CREDIT(3:4)          
007104                   MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                   
007105                   PERFORM DB2-SELECT-T01CURR-MAX-CREDIT                  
007106                   PERFORM DB2-SELECT-T01CURR-CREDIT                      
007107                 END-IF                                                   
007108               ELSE                                                       
007109                 PERFORM DB2-SELECT-T01CURR-MAX                           
007110                 PERFORM DB2-SELECT-T01CURR                               
007111               END-IF                                                     
007112               PERFORM S11-WRITE-WF20X8                                   
007120             END-IF                                                       
007130           END-IF                                                         
007210         END-PERFORM                                                      
007211         IF WS-MULTIFETCH = 100                                           
007220           PERFORM DB2-FETCH-CRS1                                         
007230           IF SQLERRD(3) > 0                                              
007240             MOVE 000     TO SQLCODE-WS                                   
007241           END-IF                                                         
007242         ELSE                                                             
007243           MOVE 100     TO SQLCODE-WS                                     
007250         END-IF                                                           
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
009010     OPEN OUTPUT WF2018                                                   
009020                 WF2028                                                   
009021                                                                          
009030     INITIALIZE CUS-WF2018T                                               
009031                                                                          
009040     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
009050     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
009400     .                                                                    
009500                                                                          
009600 Z-FINIT SECTION.                                                         
009710     CLOSE WF2018                                                         
009720     CLOSE WF2028                                                         
009900     .                                                                    
010000                                                                          
010102 S11-WRITE-WF20X8 SECTION.                                                
010104     MOVE PROC-DAEXDAT                TO WS-CUS-DAEXDAT                   
010105     MOVE PROC-TIEXTID                TO WS-CUS-TIEXTID                   
010107     MOVE WS-DAFINDOC (WS-MX)         TO WS-CUS-DAFINDOC                  
010108     MOVE SECO-KDVALISO               TO WS-CUS-KDVALISO-SEND             
010110     COMPUTE WS-CUS-PRKURS-SEND ROUNDED =                                 
010111             CURR-PRKURS / CURR-REVALUTA                                  
010112     END-COMPUTE                                                          
010113                                                                          
010114     MOVE CUS-IDLEGSEL        (WS-MX) TO WS-CUS-IDLEGSEL                  
010117     MOVE CUS-IDLANDX3-SEND   (WS-MX) TO WS-CUS-IDLANDX3-SEND             
010118     MOVE CUS-IDLANDX3-BET    (WS-MX) TO WS-CUS-IDLANDX3-BET              
010119     MOVE CUS-IDLANDX3-REC    (WS-MX) TO WS-CUS-IDLANDX3-REC              
010120     MOVE CUS-KDVALISO        (WS-MX) TO WS-CUS-KDVALISO                  
010121     MOVE CUS-PRKURS          (WS-MX) TO WS-CUS-PRKURS                    
010122     MOVE CUS-KDFINDOC        (WS-MX) TO WS-CUS-KDFINDOC                  
010124     MOVE CUS-IDFINDOC        (WS-MX) TO WS-CUS-IDFINDOC                  
010125     MOVE CUS-IDPARTNR        (WS-MX) TO WS-CUS-IDPARTNR                  
010126     MOVE CUS-IDEXCUST-1      (WS-MX) TO WS-CUS-IDEXCUST-1                
010127     MOVE CUS-IDEXCUST-2      (WS-MX) TO WS-CUS-IDEXCUST-2                
010128**** FIX TIS                                                              
010129     IF CUS-KDFINDOC(WS-MX) = 'INV2'                                      
010130       MOVE SPACE TO CUS-IDEXCUST-3(WS-MX)                                
010131     END-IF                                                               
010132     MOVE CUS-IDEXCUST-3      (WS-MX) TO WS-CUS-IDEXCUST-3                
010133     MOVE CUS-IDARTNR-FINANCE (WS-MX) TO WS-CUS-IDARTNR-FINANCE           
010134     MOVE CUS-BEART           (WS-MX) TO WS-CUS-BEART                     
010135     MOVE CUS-IDSTATNR        (WS-MX) TO WS-CUS-IDSTATNR                  
010136     MOVE CUS-VKARTNTO        (WS-MX) TO WS-CUS-VKARTNTO                  
010137     MOVE CUS-KVLEVART        (WS-MX) TO WS-CUS-KVLEVART                  
010138     MOVE CUS-KDARTURS        (WS-MX) TO WS-CUS-KDARTURS                  
010139     MOVE CUS-KDFRAKT         (WS-MX) TO WS-CUS-KDFRAKT                   
010140     MOVE CUS-BELEVVIL        (WS-MX) TO WS-CUS-BELEVVIL                  
010141     MOVE CUS-SUNTO           (WS-MX) TO WS-CUS-SUNTO                     
010142                                                                          
010143     IF CUS-IDLEGSEL (WS-MX) = 'VCCS'                                     
010144       WRITE POST-WF2018   FROM WS-CUS-WF2018                             
010145     ELSE                                                                 
010146       MOVE CUS-IDLEGSEL (WS-MX)      TO WS-IDLEGSEL                      
010147       IF WS-IDLEGSEL(1:2) = 'SC'                                         
010148         WRITE POST-WF2028 FROM WS-CUS-WF2018                             
010149       END-IF                                                             
010150     END-IF                                                               
010151     .                                                                    
010152                                                                          
010153* --- DB2 SECTIONS  ---                                                   
010154*                                                                         
010160 DB2-SELECT-T01PROC-TAB   SECTION.                                        
010170     MOVE 000    TO GOOD-SQLCODES                                         
010180                                                                          
010190     EXEC SQL                                                             
010200           SELECT  DAEXDAT                                                
010300                ,  TIEXTID                                                
010310                ,  KDBEH                                                  
010320                ,  IDLEGSEL                                               
010400                                                                          
010500           INTO   :PROC-DAEXDAT                                           
010600                , :PROC-TIEXTID                                           
010610                , :PROC-KDBEH                                             
010620                , :PROC-IDLEGSEL                                          
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
012200           SELECT  FLCUSREP                                               
012300                                                                          
012400           INTO   :INRE-FLCUSREP                                          
012500                                                                          
012600           FROM    T01INRE                                                
012700                                                                          
012800           WHERE   IDLEGSEL      = :WS-IDLEGSEL                           
012900           AND     IDLANDX3_SEND = :WS-IDLANDX3-BET                       
013000           AND     IDLANDX3_REC  = :WS-IDLANDX3-REC                       
013010           AND     FLCUSREP      = :JA                                    
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
014400           SELECT  FLCUSREP                                               
014500                                                                          
014600           INTO   :INRE-FLCUSREP                                          
014700                                                                          
014800           FROM    T01INRE                                                
014900                                                                          
015000           WHERE   IDLEGSEL      = :WS-IDLEGSEL                           
015100           AND     IDLANDX3_SEND = :WS-IDLANDX3-SEND                      
015200           AND     IDLANDX3_REC  = :WS-IDLANDX3-BET                       
015210           AND     FLCUSREP      = :JA                                    
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
016104 DB2-SELECT-T01CURR-MAX SECTION.                                          
016106     MOVE 000    TO GOOD-SQLCODES                                         
016107                                                                          
016108     EXEC SQL                                                             
016109           SELECT  MAX(DASTADAT)                                          
016110                                                                          
016111           INTO   :CURR-DASTADAT                                          
016112                                                                          
016113           FROM    T01CURR                                                
016114                                                                          
016115           WHERE   IDLEGSEL  = :WS-IDLEGSEL                               
016116           AND     KDVALISO  = :WS-KDVALISO                               
016117           AND     DASTADAT <= :WS-DAGENS-DATUM                           
016119     END-EXEC                                                             
016120                                                                          
016121     MOVE SQLCODE TO SQLCODE-WS                                           
016122     PERFORM DB2-STATUS-CHECK                                             
016123     .                                                                    
016124                                                                          
016125 DB2-SELECT-T01CURR SECTION.                                              
016127     MOVE 000    TO GOOD-SQLCODES                                         
016128                                                                          
016129     EXEC SQL                                                             
016130           SELECT  PRKURS                                                 
016131                  ,REVALUTA                                               
016132                                                                          
016133           INTO   :CURR-PRKURS                                            
016134                 ,:CURR-REVALUTA                                          
016135                                                                          
016136           FROM    T01CURR                                                
016137                                                                          
016138           WHERE   IDLEGSEL    = :WS-IDLEGSEL                             
016139           AND     KDVALISO    = :WS-KDVALISO                             
016140           AND     DASTADAT    = :CURR-DASTADAT                           
016141     END-EXEC                                                             
016142                                                                          
016143     MOVE SQLCODE TO SQLCODE-WS                                           
016144     PERFORM DB2-STATUS-CHECK                                             
016145     .                                                                    
016146                                                                          
016147 DB2-SELECT-T01CURR-MAX-CREDIT SECTION.                                   
016148     MOVE 000    TO GOOD-SQLCODES                                         
016149                                                                          
016150     EXEC SQL                                                             
016151           SELECT  MAX(DASTADAT)                                          
016152                                                                          
016153           INTO   :WS2-DASTADAT-CREDIT                                    
016154                                                                          
016155           FROM    T01CURR                                                
016156                                                                          
016157           WHERE   IDLEGSEL  = :WS-IDLEGSEL                               
016158           AND     KDVALISO  = :WS-KDVALISO                               
016159           AND     DASTADAT <= :WS-DASTADAT-CREDIT                        
016160     END-EXEC                                                             
016161                                                                          
016162     MOVE SQLCODE TO SQLCODE-WS                                           
016163     PERFORM DB2-STATUS-CHECK                                             
016164     .                                                                    
016165                                                                          
016166 DB2-SELECT-T01CURR-CREDIT SECTION.                                       
016167     MOVE 000    TO GOOD-SQLCODES                                         
016168                                                                          
016169     EXEC SQL                                                             
016170           SELECT  PRKURS                                                 
016171                  ,REVALUTA                                               
016172                                                                          
016173           INTO   :CURR-PRKURS                                            
016174                 ,:CURR-REVALUTA                                          
016175                                                                          
016176           FROM    T01CURR                                                
016177                                                                          
016178           WHERE   IDLEGSEL    = :WS-IDLEGSEL                             
016179           AND     KDVALISO    = :WS-KDVALISO                             
016180           AND     DASTADAT    = :WS2-DASTADAT-CREDIT                     
016181     END-EXEC                                                             
016182                                                                          
016183     MOVE SQLCODE TO SQLCODE-WS                                           
016184     PERFORM DB2-STATUS-CHECK                                             
016185     .                                                                    
016186                                                                          
016190 DB2-DCL-OPN-CRS1 SECTION.                                                
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
019201        AND   (A.IDLEVNR         = :WS-IDLEVNR2                           
019202        OR     A.KDFINDOC        = 'CR'                                   
019203        AND    A.IDLEVNR         > :WS-IDLEVNR2)                          
019210        AND    B.IDARTNR_FINANCE > :WS-IDARTNR-FINANCE                    
019300        AND    C.IDLEGSEL        = A.IDLEGSEL                             
019400        AND    C.KDFINDOC        = A.KDFINDOC                             
019600        AND    C.FLCUSREP        = :JA                                    
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
023600       INTO  :CUS-IDLEGSEL                                                
023700           , :CUS-IDLANDX3-SEND                                           
023800           , :CUS-IDLANDX3-BET                                            
023900           , :CUS-IDLANDX3-REC                                            
024000           , :CUS-KDVALISO                                                
024100           , :CUS-PRKURS                                                  
024200           , :CUS-IDPARTNR                                                
024300           , :CUS-KDFINDOC                                                
024400           , :WS-DAFINDOC                                                 
024500           , :CUS-IDFINDOC                                                
024510           , :CUS-IDEXCUST-1                                              
024520           , :CUS-IDEXCUST-2                                              
024530           , :CUS-IDEXCUST-3                                              
024600           , :CUS-IDARTNR-FINANCE                                         
024700           , :CUS-BEART                                                   
024800           , :CUS-IDSTATNR                                                
024900           , :CUS-VKARTNTO                                                
025000           , :CUS-KVLEVART                                                
025100           , :CUS-SUNTO                                                   
025110           , :CUS-KDARTURS                                                
025120           , :CUS-KDFRAKT                                                 
025130           , :CUS-BELEVVIL                                                
025140           , :WS-IDLEVNR                                                  
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
026510     EJECT                                                                
026520 DB2-OPEN-CRS-LSEL SECTION.                                               
026530     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
026540     SELECT   T01LSEL.IDLEGSEL                                            
026550                                                                          
026560     FROM     T01LSEL                                                     
026570                                                                          
026580     WHERE    KDSTATUS = 1                                                
026590     END-EXEC                                                             
026591                                                                          
026592     EXEC SQL OPEN T01LSEL-CRS                                            
026593     END-EXEC                                                             
026594                                                                          
026595     MOVE 000            TO GOOD-SQLCODES                                 
026596     MOVE SQLCODE        TO SQLCODE-WS                                    
026597     PERFORM DB2-STATUS-CHECK                                             
026598     .                                                                    
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
026690                                                                          
026691 DB2-CLOSE-CRS-LSEL SECTION.                                              
026692     EXEC SQL CLOSE T01LSEL-CRS                                           
026693     END-EXEC                                                             
026694     .                                                                    
026695     EJECT                                                                
026696                                                                          
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
