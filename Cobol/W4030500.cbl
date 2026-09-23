000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4030500.                                                
000400 AUTHOR.         LOTTA LANDSTEN.                                          
000500     DATE-WRITTEN.   JUNI -86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4305.                                                       
001100*        BÖRJA-OM.                                                        
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T305                                              
001600*        .            W4T305U                                             
001700*        MID:         W4I30501-MID.                                       
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O30201-MOD                                        
002100*                     W4O39301-MOD                                        
002200*                     W4O39401-MOD                                        
002300*                     W4O39501-MOD                                        
002400*                     W4O39601-MOD                                        
002500*                     W4O30101-MOD                                        
002600*                     W4O39101-MOD                                        
002700*                     RXOXXX01-MOD.                                       
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 WORKING-STORAGE SECTION.                                                 
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77   PROGRAM-NAMN           VALUE 'W4030500'                             
003500                                 PIC X(8).                                
003600 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003700 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +247 COMP SYNC.        
003800 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +48  COMP SYNC.        
003900 77    M430X-MOD-LAENGD          PIC S9(4)   VALUE +48  COMP SYNC.        
004000 77    M439X-MOD-LAENGD          PIC S9(4)   VALUE +82  COMP SYNC.        
004100 77    4316-IND                  PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77    MAX-4316-IND              PIC S9(9)   VALUE +12  COMP SYNC.        
004300 77    MAX-4316-IND-004          PIC S9(9)   VALUE +26  COMP SYNC.        
004400 77    RAETT                     PIC X       VALUE 'R'.                   
004500 77    FEL                       PIC X       VALUE 'F'.                   
004600 77    JA                        PIC X       VALUE 'J'.                   
004700 77    NEJ                       PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01    DYNAMISKA-SUBPROGRAM.                                              
005000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005200     EJECT                                                                
005300 01    FILLER                    PIC X(16)                                
005400                                 VALUE 'WS-MODNAMN'.                      
005500 01    WS-MODNAMN.                                                        
005600   03    FILLER                  PIC X       VALUE 'W'.                   
005700   03    WS-MOD-IDTRANS-POS-1    PIC X       VALUE SPACE.                 
005800   03    FILLER                  PIC X       VALUE 'O'.                   
005900   03    WS-MOD-IDTRANS-POS-2-4  PIC X(3)    VALUE SPACE.                 
006000   03    FILLER                  PIC XX      VALUE '01'.                  
006100                                                                          
006200 01    FILLER                    PIC X(16)                                
006300                                 VALUE 'WS-IDTRANS-MOD'.                  
006400 01    WS-IDTRANS-MOD.                                                    
006500   03    WS-IDTRANS-POS-1-MOD    PIC X       VALUE SPACE.                 
006600   03    WS-IDTRANS-POS-2-4-MOD  PIC X(3)    VALUE SPACE.                 
006700                                                                          
006800 01    FILLER                    PIC X(16)   VALUE 'DIVERSE'.             
006900 01    DIVERSE.                                                           
007000   03    W-IDPRODNR              PIC 9(7)    VALUE ZERO.                  
007100   03    W-IDDISTR               PIC 9(4)    VALUE ZERO.                  
007200   03    W-IDKUNDNR              PIC 9(6)    VALUE ZERO.                  
007300   03    W-KDFRAKT               PIC 9(2)    VALUE ZERO.                  
007400   03    W-IDORDNR               PIC 9(5)    VALUE ZERO.                  
007500   03    W-KDORDKL               PIC 9       VALUE ZERO.                  
007600                                                                          
007900   03    WS-INDATA-TEST          PIC X       VALUE SPACE.                 
008000         88  WS-INDATA-FEL                   VALUE 'F'.                   
008100         88  WS-INDATA-RAETT                 VALUE 'R'.                   
008200                                                                          
008300   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
008400         88  WS-EGEN-BILD        VALUE '4305'.                            
008500         88  WS-TILLATEN-BILD    VALUE  '4391' THRU '4396'.               
008600         88  WS-GODKAEND-BILD    VALUE  '4301' '4302'                     
008700                                        '4303' '4305' '4306'              
008800                   '4391' '4392' '4393' '4394' '4395' '4396'.             
008900   03    WS-FLREPL-TEST          PIC X       VALUE SPACE.                 
009000         88  WS-FLREPL                       VALUE 'J'.                   
009100                                                                          
009200   03    WS-FLTRAEFF-TEST        PIC X       VALUE SPACE.                 
009300         88  WS-FLTRAEFF                     VALUE 'J'.                   
009400                                                                          
009500   03    WS-IDKOLLI              PIC S9(5)   VALUE ZERO.                  
009600   03    WS-IDTRANS-4312         PIC X(4)    VALUE SPACE.                 
009700                                                                          
009800   03    WS-PLUS.                                                         
009900         05  FILLER              PIC X(10)    VALUE '++++++++++'.         
010000                                                                          
010100   03    WS-IDRADNR              PIC 9(4)    VALUE ZERO.                  
010200   03    WS-IDRADNR-X REDEFINES WS-IDRADNR PIC X(4).                      
010300                                                                          
010400   03    WS-IDKOLLI-TRAEFF       PIC S9(5)   VALUE ZERO  COMP-3.          
010500                                                                          
010600   03    WS-TESKALLEJ-X.                                                  
010700         05 FILLER               PIC X(12)   VALUE 'BEARBETAS EJ'.        
010800         05 FILLER               PIC X(12)   VALUE 'NOT TO PROC.'.        
010900   03    FILLER REDEFINES WS-TESKALLEJ-X.                                 
011000         05 WS-TESKALLEJ         PIC X(12) OCCURS 2.                      
011100                                                                          
011200   03    WS-TEEJBEARB-X.                                                  
011300         05 FILLER               PIC X(12)   VALUE 'EJ BEARBETAD'.        
011400         05 FILLER               PIC X(12)   VALUE 'NOT PROCESS.'.        
011500   03    FILLER REDEFINES WS-TEEJBEARB-X.                                 
011600         05 WS-TEEJBEARB         PIC X(12)   OCCURS 2.                    
011700                                                                          
011800   03    WS-TEBEARB-X.                                                    
011900         05 FILLER               PIC X(12)   VALUE 'BEARBETAD   '.        
012000         05 FILLER               PIC X(12)   VALUE 'PROCESSED   '.        
012100   03    FILLER REDEFINES WS-TEBEARB-X.                                   
012200         05 WS-TEBEARB           PIC X(12)   OCCURS 2.                    
012300                                                                          
012400   03    WS-TEPAGAENDE-X.                                                 
012500         05 FILLER               PIC X(12)   VALUE 'PÅBÖRJAD    '.        
012600         05 FILLER               PIC X(12)   VALUE 'STARTED     '.        
012700   03    FILLER REDEFINES WS-TEPAGAENDE-X.                                
012800         05 WS-TEPAGAENDE        PIC X(12)   OCCURS 2.                    
012900                                                                          
013000   03    WS-TEBACKA-4301         PIC X(6)    VALUE '4301'.                
013100   03    WS-TEBACKA-4302         PIC X(6)    VALUE '4302'.                
013200   03    WS-TEBACKA-4303         PIC X(6)    VALUE '4303'.                
013300   03    WS-TEBACKA-4306         PIC X(6)    VALUE '4306'.                
013400   03    WS-TEBACKA-4391         PIC X(6)    VALUE '4391'.                
013500   03    WS-TEBACKA-4393         PIC X(6)    VALUE '4393'.                
013600   03    WS-TEBACKA-4394         PIC X(6)    VALUE '4394'.                
013700   03    WS-TEBACKA-4395         PIC X(6)    VALUE '4395'.                
013800   03    WS-TEBACKA-4396         PIC X(6)    VALUE '4396'.                
013900                                                                          
013910*      --- VALID IDDD CODES                                               
013920*                                                                         
013930*01    -COPY WWDC99                                                       
013940       EJECT                                                              
014000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014100     88  NYCKLAR-OK                          VALUE 'J'.                   
014200     88  NYCKLAR-FEL                         VALUE 'N'.                   
014300                                                                          
014400                                                                          
014500     EJECT                                                                
014600 01    FILLER                    PIC X(16)                                
014700                                 VALUE 'NYCKLAR-TILL-DLI'.                
014800 01    NYCKLAR-TILL-DLI.                                                  
014900                                                                          
015000   03    W-WDGXKEY-4305-X.                                                
015100     05    FILLER                PIC X(4)    VALUE '4305'.                
015200     05    W-IDDC-4305           PIC X(2).                                
015300     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
015400                                                                          
015500   03    W-WDGXKEY-4306-X.                                                
015600     05    W-IDPRODNR-4306       PIC S9(7)   VALUE ZERO  COMP-3.          
015700     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
015800                                                                          
015900   03    W-WDGXKEY-4311-X.                                                
016000     05    FILLER                PIC X(4)    VALUE '4311'.                
016100     05    W-IDDC-4311           PIC X(2).                                
016200     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
016300                                                                          
016400   03    W-WDGXKEY-4312-X.                                                
016500     05    W-IDPRODNR-4312       PIC S9(7)   VALUE ZERO  COMP-3.          
016600     05    FILLER                PIC X(6)    VALUE LOW-VALUE.             
016700                                                                          
016800   03    W-WDGXKEY-4314-X.                                                
016900     05    W-IDRADNR-4314        PIC S9(5)   VALUE ZERO  COMP-3.          
017000     05    FILLER                PIC X(7)    VALUE LOW-VALUE.             
017100                                                                          
017200   03    W-WDGXKEY-4315-X.                                                
017300     05    FILLER                PIC X(4)    VALUE '4315'.                
017400     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
017500                                                                          
017600   03    W-WDGXKEY-4316-X.                                                
017700     05    W-IDPRODNR-4316       PIC S9(7)   VALUE ZERO  COMP-3.          
017800     05    W-IDPTYP-4316         PIC X(3)    VALUE SPACE.                 
017900     05    W-IDKOLLI-4316        PIC S9(5)   VALUE ZERO  COMP-3.          
018000     05    FILLER                PIC X(10)   VALUE LOW-VALUE.             
018100                                                                          
018200   03    W-WDGXKEY-4316-X-KOL-MIN.                                        
018300     05    W-IDPRODNR-4316-KOL-MIN PIC S9(7)   VALUE ZERO  COMP-3.        
018400     05    W-IDPTYP-4316-KOL-MIN   PIC X(3)    VALUE '002'.               
018500     05    W-IDKOLLI-4316-KOL-MIN  PIC S9(5)   VALUE ZERO  COMP-3.        
018600     05    FILLER                  PIC X(10)   VALUE LOW-VALUE.           
018700                                                                          
018800   03    W-WDGXKEY-4316-X-MIN.                                            
018900     05    W-IDPRODNR-4316-MIN   PIC S9(7)   VALUE ZERO  COMP-3.          
019000     05    W-IDPTYP-4316-MIN     PIC X(3)    VALUE '002'.                 
019100     05    FILLER                PIC X(13)   VALUE LOW-VALUE.             
019200                                                                          
019300   03    W-WDGXKEY-4316-X-MAX.                                            
019400     05    W-IDPRODNR-4316-MAX   PIC S9(7)   VALUE ZERO  COMP-3.          
019500     05    W-IDPTYP-4316-MAX     PIC X(3)    VALUE SPACE.                 
019600     05    FILLER                PIC X(13)   VALUE HIGH-VALUE.            
019610                                                                          
019620   03  W-IDDC-B6-X.                                                       
019630     05 W-IDDC-B6                PIC X(2).                                
019700                                                                          
019800     EJECT                                                                
019900 01    FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
020000 01    MEDDELANDE.                                                        
020100                                                                          
020200   03    FEL1.                                                            
020300      05    FILLER               PIC X(40)   VALUE                        
020400           '748. UPPLYSTA FÄLT FEL'.                                      
020500      05    FILLER               PIC X(40)   VALUE                        
020600           '748. HIGHLIT FIEDS WRONG.  '.                                 
020700   03    FILLER  REDEFINES FEL1.                                          
020800      05    FEL-1                PIC X(40)   OCCURS 2.                    
020900                                                                          
021000   03    FEL2.                                                            
021100      05    FILLER               PIC X(40)   VALUE                        
021200           '749. FEL NYCKEL'.                                             
021300      05    FILLER               PIC X(40)   VALUE                        
021400           '749. WRONG KEYS.                 '.                           
021500   03    FILLER  REDEFINES FEL2.                                          
021600      05    FEL-2                PIC X(40)   OCCURS 2.                    
021700                                                                          
021800   03    FEL3.                                                            
021900      05    FILLER               PIC X(40)   VALUE                        
022000           '815. FELAKTIGT USERID'.                                       
022100      05    FILLER               PIC X(40)   VALUE                        
022200           '815. WRONG USER-ID '.                                         
022300   03    FILLER  REDEFINES FEL3.                                          
022400      05    FEL-3                PIC X(40)   OCCURS 2.                    
022500                                                                          
022600   03    FEL4.                                                            
022700      05    FILLER               PIC X(40)   VALUE                        
022800           '708. ORDERN SAKNAS ELLER KLAR'.                               
022900      05    FILLER               PIC X(40)   VALUE                        
023000           '708. ORDER MISSING OR READY        '.                         
023100   03    FILLER  REDEFINES FEL4.                                          
023200      05    FEL-4                PIC X(40)   OCCURS 2.                    
023300                                                                          
023400   03    FEL5.                                                            
023500      05    FILLER               PIC X(40)   VALUE                        
023600           '812. TRYCK PF11 FÖR UPPDATERING'.                             
023700      05    FILLER               PIC X(40)   VALUE                        
023800           '812. PRESS PF11 TO UPDATE. '.                                 
023900   03    FILLER  REDEFINES FEL5.                                          
024000      05    FEL-5                PIC X(40)   OCCURS 2.                    
024100                                                                          
024200     EJECT                                                                
024300******************************************************************        
024400*                                                                         
024500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024600*                                                                         
024700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
024800     SKIP3                                                                
024900 01    FILLER                    PIC X(16)                                
025000                                 VALUE 'MID W4I30501 MID'.                
025100*01    -COPY W4I30501                                                     
025200     EJECT                                                                
025300*01    -COPY WMSGAREA                                                     
025400     EJECT                                                                
025500*  03  MOD -COPY W4O30501   -RED MSG-AREA                                 
025600     EJECT                                                                
025700*  03  MOD -COPY W4O30101   -RED MSG-AREA -PRE M4301-                     
025800     EJECT                                                                
025900*  03  MOD -COPY W4O30201   -RED MSG-AREA -PRE M4302-                     
026000     EJECT                                                                
026100*  03  MOD -COPY W4O30301   -RED MSG-AREA -PRE M4303-                     
026200     EJECT                                                                
026300*  03  MOD -COPY W4O39101   -RED MSG-AREA -PRE M4391-                     
026400     EJECT                                                                
026500*  03  MOD -COPY W4O39301   -RED MSG-AREA -PRE M4393-                     
026600     EJECT                                                                
026700*  03  MOD -COPY W4O39401   -RED MSG-AREA -PRE M4394-                     
026800     EJECT                                                                
026900*  03  MOD -COPY W4O39501   -RED MSG-AREA -PRE M4395-                     
027000     EJECT                                                                
027100*  03  MOD -COPY W4O39601   -RED MSG-AREA -PRE M4396-                     
027200     EJECT                                                                
027300*01    -COPY WMFSAREA                                                     
027400     EJECT                                                                
027500******************************************************************        
027600*                                                                         
027700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027800*                                                                         
027900 01    IMS-WS.                                                            
028000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
028100     SKIP3                                                                
028200*                        **** STATUS-KOD FRÅN IMS                         
028300   03    STATUS-WS               PIC XX.                                  
028400     88    SEGMENT-FINNS                     VALUE '  '.                  
028500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
028600     SKIP3                                                                
028700   03    GODK-STATUSKODER.                                                
028800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
028900     SKIP3                                                                
029000 01    SSA1                      PIC X(96).                               
029100 01    SSA2                      PIC X(64).                               
029200 01    SSA3                      PIC X(64).                               
029300     EJECT                                                                
029400*                            IMS FUNKTIONSKODER                           
029500*01    -COPY W0003                                                        
029600     EJECT                                                                
029700*                            DLI INPUT-OUTPUT AREOR                       
029800 01    FILLER                    PIC X(16)   VALUE                        
029900                                 'DLI-IOAREA-1'.                          
030000 01    DLI-IO-AREA-1.                                                     
030100   03    IO-AREA-1               PIC X(370)  VALUE SPACE.                 
030200     SKIP3                                                                
030300*  03    XXDJ01 -COPY WDGX4305   -RED IO-AREA-1                           
030400     EJECT                                                                
030500*  03    XXDJ11 -COPY WDGX4306   -RED IO-AREA-1                           
030600     EJECT                                                                
030700*  03    XXDL11 -COPY WDGX4316   -RED IO-AREA-1                           
030800     EJECT                                                                
030900*    08  AREA   -COPY W4I31501    -RED 4316-FILLER -PRE 4316-A-           
031000     EJECT                                                                
031100*    08  AREA   -COPY W4I31401    -RED 4316-FILLER -PRE 4316-B-           
031200     EJECT                                                                
031300*    08  AREA   -COPY W4I39801    -RED 4316-FILLER -PRE 4316-C-           
031400     EJECT                                                                
031500 01    FILLER                    PIC X(16)   VALUE                        
031600                                 'DLI-IOAREA-2'.                          
031700 01    DLI-IO-AREA-2.                                                     
031800   03    IO-AREA-2               PIC X(100)  VALUE SPACE.                 
031900     SKIP3                                                                
032000*  03    XXDK11 -COPY WDGX4312   -RED IO-AREA-2                           
032100     EJECT                                                                
032200*  03    XXDK21 -COPY WDGX4314   -RED IO-AREA-2                           
032210                                                                          
032220 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032230 01   DLI-IO-AREA-B6.                                                     
032240*     03  -COPY WDB601                                                    
032250                                                                          
032300     EJECT                                                                
032400 LINKAGE SECTION.                                                         
032500*01    -COPY W0009     -PRE MSG-                                          
032600     EJECT                                                                
032700*01    -COPY W0008     -PRE XXDJ-                                         
032800        05 FILLER                PIC X.                                   
032900     EJECT                                                                
033000*01    -COPY W0008     -PRE XXDK-                                         
033100        05 FILLER                PIC X.                                   
033200     EJECT                                                                
033300*01    -COPY W0008     -PRE XXDL-                                         
033400        05 FILLER                PIC X.                                   
033500     EJECT                                                                
033510*01    -COPY W0008     -PRE WDB6-                                         
033520        05 FILLER                PIC X.                                   
033530     EJECT                                                                
033600 PROCEDURE DIVISION  USING MSG-PCB                                        
033700                     XXDJ-PCB XXDK-PCB XXDL-PCB WDB6-PCB.                 
033800                                                                          
033900     ENTRY 'DLITCBL' USING MSG-PCB                                        
034000                     XXDJ-PCB XXDK-PCB XXDL-PCB WDB6-PCB.                 
034100                                                                          
034200     PERFORM IMS-GET-MSG                                                  
034300                                                                          
034400     IF SEGMENT-FINNS                                                     
034500       PERFORM A-INIT-SPARA-INPUT                                         
034600                                                                          
034700       IF NYCKLAR-OK                                                      
034800           PERFORM B-FLYTTA-NYCKLAR                                       
034900                                                                          
035000           IF WS-EGEN-BILD                                                
035100              PERFORM E-KOLLA-NYCKLAR                                     
035200              IF NYCKLAR-OK                                               
035300                                                                          
035400                 IF MFS-UPDATE                                            
035500                    PERFORM C-PF11                                        
035600                                                                          
035700                    IF WS-INDATA-FEL                                      
035800                       PERFORM S10-ROER-EJ-FAELT                          
035900                    END-IF                                                
036000                                                                          
036100                 ELSE                                                     
036200                    PERFORM D-ENTER                                       
036300                 END-IF                                                   
036400                                                                          
036500                 IF WS-INDATA-RAETT                                       
036600                    PERFORM S99-NAESTA-TRANS                              
036700                 END-IF                                                   
036800              ELSE                                                        
036900                MOVE MAX-MOD-LAENGD TO MSG-KVLL                           
037000              END-IF                                                      
037100                                                                          
037200           ELSE                                                           
037210             IF NYCKLAR-OK                                                
037300                 PERFORM S02-KOLL-4312                                    
037500                 IF WS-INDATA-RAETT                                       
037600                    PERFORM S09-VISA-4305                                 
037700                 END-IF                                                   
037800             END-IF                                                       
037900          END-IF                                                          
038000       ELSE                                                               
038100           MOVE FEL-2 (INDX) TO MOD-TEMFSFEL                              
038200       END-IF                                                             
038300       IF NOT WS-GODKAEND-BILD                                            
038400           PERFORM E-RENSA-NYCKLAR                                        
038500       END-IF                                                             
038600                                                                          
038700       PERFORM IMS-INSERT-MSG                                             
038800     END-IF                                                               
038900                                                                          
039000     MOVE ZERO TO RETURN-CODE                                             
039100     GOBACK                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 A-INIT-SPARA-INPUT SECTION.                                              
039500     IF MSG-DUBBLA-TRANSKODER                                             
039600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I30501                 
039700       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
039800       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
039900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
040000       MOVE MSG-IDPFK            TO MFS-IDPFK                             
040100     ELSE                                                                 
040200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I30501                  
040300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
040400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040500       MOVE ' '            TO MFS-KDTRTYP                                 
040600                              MFS-IDPFK                                   
040700     END-IF                                                               
040800     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
040900                                                                          
041000                                                                          
041100     IF  MID-IDPRODNR-IN NOT = ALL '+'                                    
041200         MOVE SPACE      TO MFS-KDTRTYP                                   
041300     END-IF                                                               
041400                                                                          
041700     MOVE LOW-VALUE TO MSG-AREA                                           
041800     MOVE 'W4O305N1' TO MFS-IDMOD                                         
041900     MOVE '4305' TO MOD-IDTRANS                                           
042000     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
042100                                                                          
042200     IF SWEDISH-TEXT                                                      
042300       MOVE +1 TO INDX                                                    
042400     ELSE                                                                 
042500       MOVE +2 TO INDX                                                    
042600     END-IF                                                               
042700                                                                          
042800     PERFORM AB-REDIGERA-NYCKLAR                                          
042900                                                                          
043000     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
043100                               MOD-IDPRODNR-IN                            
043200                               MOD-IDUSER                                 
043300                               MOD-FLTILLBAKA                             
043400                               MOD-KDBEHAND-GRUND                         
043500                               MOD-TEGRUND                                
043600                               MOD-KDBEHAND-RAD                           
043700                               MOD-TERAD                                  
043800                               MOD-KDBEHAND-DEL                           
043900                               MOD-TEDEL                                  
044000                               MOD-KDBEHAND-KOL                           
044100                               MOD-TEKOL                                  
044200                               MOD-KDBEHAND-AVVIK                         
044300                               MOD-TEAVVIK                                
044400                               MOD-KDBEHAND-URS                           
044500                               MOD-TEURS                                  
044600                               MOD-FLBORJA-OM                             
044700                               MOD-TEBACKA-PGM                            
044800                               MOD-FLBACKA                                
044900                               MOD-TEMFSINF                               
045000                                                                          
045100     MOVE RAETT             TO WS-INDATA-TEST                             
045200     MOVE MFS-IDTRANS       TO WS-IDTRANS                                 
045300                                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 AB-REDIGERA-NYCKLAR SECTION.                                             
045700                                                                          
045800     MOVE JA TO NYCKLAR-SW                                                
045900                                                                          
046000     IF MID-IDPRODNR-IN = ALL '+'                                         
046100        INSPECT MID-IDPRODNR-UT REPLACING LEADING SPACE BY ZERO           
046200        MOVE MID-IDPRODNR-UT    TO W-IDPRODNR                             
046300        MOVE MID-IDDISTR-UT     TO MOD-IDDISTR-UT                         
046400        MOVE MID-IDKUNDNR-UT    TO MOD-IDKUNDNR-UT                        
046500        MOVE MID-KDFRAKT-UT     TO MOD-KDFRAKT-UT                         
046600        MOVE MID-IDORDNR-UT     TO MOD-IDORDNR-UT                         
046700        MOVE MID-KDORDKL-UT     TO MOD-KDORDKL-UT                         
046800     ELSE                                                                 
046900        MOVE MID-IDPRODNR-IN    TO W-IDPRODNR                             
047000        MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-UT                         
047100                                   MOD-IDKUNDNR-UT                        
047200                                   MOD-KDFRAKT-UT                         
047300                                   MOD-IDORDNR-UT                         
047400                                   MOD-KDORDKL-UT                         
047410                                   MOD-IDDC-UT                            
047500     END-IF                                                               
047600                                                                          
047700     MOVE W-IDPRODNR            TO MOD-IDPRODNR-UT                        
047710     IF W-IDPRODNR NUMERIC                                                
047711        IF W-IDPRODNR > ZERO                                              
047720           CONTINUE                                                       
047730        ELSE                                                              
047740           MOVE NEJ             TO NYCKLAR-SW                             
047750        END-IF                                                            
047760     ELSE                                                                 
047770        MOVE NEJ                TO NYCKLAR-SW                             
047780     END-IF                                                               
047800                                                                          
047900     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
048000                                                                          
048030                                                                          
048100     IF MID-IDDC-IN = ALL '+'                                             
048200       IF MID-IDDC-UT = SPACE                                             
048300         MOVE NEJ               TO NYCKLAR-SW                             
048400       ELSE                                                               
048500         MOVE MID-IDDC-UT       TO WS-IDDC                                
048600       END-IF                                                             
048700     ELSE                                                                 
048800       MOVE MID-IDDC-IN         TO WS-IDDC                                
049100     END-IF                                                               
049200                                                                          
049210     IF NYCKLAR-OK                                                        
049220        MOVE WS-IDDC TO W-IDDC-B6                                         
049230        PERFORM IMS-GU-WDB6                                               
049240        IF SEGMENT-FINNS                                                  
049320           IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC                            
049400             MOVE WS-IDDC       TO W-IDDC-4305                            
049500                                   W-IDDC-4311                            
049700           ELSE                                                           
049800             MOVE NEJ           TO NYCKLAR-SW                             
049900           END-IF                                                         
049901        ELSE                                                              
049902          MOVE NEJ              TO NYCKLAR-SW                             
049904        END-IF                                                            
049905     END-IF                                                               
049906                                                                          
049910     MOVE WS-IDDC               TO MOD-IDDC-UT                            
050000     .                                                                    
050100     EJECT                                                                
050200 B-FLYTTA-NYCKLAR SECTION.                                                
050300                                                                          
050400           MOVE W-IDPRODNR      TO W-IDPRODNR-4306                        
050500                                   W-IDPRODNR-4312                        
050600                                   W-IDPRODNR-4316                        
050700                                   W-IDPRODNR-4316-KOL-MIN                
050800                                   W-IDPRODNR-4316-MIN                    
050900                                   W-IDPRODNR-4316-MAX                    
051000                                                                          
051100     .                                                                    
051200     EJECT                                                                
051300 E-KOLLA-NYCKLAR         SECTION.                                         
051400                                                                          
051500                                                                          
051600     IF NYCKLAR-FEL                                                       
051700        MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                                 
051800        PERFORM MFS-RENSA-FAELT-MOD-INFAELT                               
051900     END-IF                                                               
052000     .                                                                    
052100     SKIP2                                                                
052200 C-PF11  SECTION.                                                         
052300                                                                          
052400     PERFORM CA-INDATA-KOLL                                               
052500                                                                          
052600     IF WS-INDATA-RAETT                                                   
052700        PERFORM S02-KOLL-4312                                             
052800                                                                          
052900        IF WS-INDATA-RAETT                                                
053000                                                                          
053100           IF MID-FLBACKA = 'J' OR 'Y'                                    
053200           OR MID-FLBORJA-OM = 'J' OR 'Y'                                 
053300              PERFORM CB-BACKA-WDG2                                       
053400           END-IF                                                         
053500                                                                          
053600           IF MID-FLTILLBAKA = 'J' OR 'Y'                                 
053700              PERFORM IMS-GET-XXDK-4312                                   
053800              IF SEGMENT-FINNS                                            
053900                 IF 4312-KDBEHAND-RAD = +3                                
054000                 OR 4312-KDBEHAND-AVVIK = +3                              
054100                 OR 4312-KDBEHAND-URS = +3                                
054200                 OR 4312-KDBEHAND-DEL = +3                                
054300                 OR 4312-KDBEHAND-KOL = +3                                
054400                    PERFORM IMS-GET-XXDL-4315                             
054500                    EVALUATE TRUE                                         
054600                    WHEN 4312-KDBEHAND-KOL = +3                           
054700                       PERFORM S03-BACKA-KOL                              
054800                    WHEN 4312-KDBEHAND-DEL = +3                           
054900                       PERFORM S04-BACKA-DEL                              
055000                    WHEN 4312-KDBEHAND-URS = +3                           
055100                       PERFORM S05-BACKA-URS                              
055200                    WHEN 4312-KDBEHAND-AVVIK = +3                         
055300                       PERFORM S06-BACKA-AVVIK                            
055400                    WHEN 4312-KDBEHAND-RAD = +3                           
055500                       PERFORM S07-BACKA-RAD                              
055600                    END-EVALUATE                                          
055700                                                                          
055800                 END-IF                                                   
055900              END-IF                                                      
056000           END-IF                                                         
056100        END-IF                                                            
056200                                                                          
056300     ELSE                                                                 
056400        MOVE FEL-1 (INDX)    TO MOD-TEMFSFEL                              
056500     END-IF                                                               
056600                                                                          
056700                                                                          
056800     .                                                                    
056900     EJECT                                                                
057000 CA-INDATA-KOLL SECTION.                                                  
057100                                                                          
057200     MOVE MFS-ALFA-FAELT-RAETT TO                                         
057300                         MOD-IDUSER-ATTR                                  
057400                                                                          
057500     IF  MID-FLTILLBAKA  = '+'                                            
057600     AND MID-FLBORJA-OM  = '+'                                            
057700     AND MID-FLBACKA     = '+'                                            
057800        MOVE FEL                   TO WS-INDATA-TEST                      
057900     ELSE                                                                 
058000                                                                          
058100        IF MID-FLTILLBAKA = '+' OR 'J' OR 'Y'                             
058200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
058300                                MOD-FLTILLBAKA-ATTR                       
058400        ELSE                                                              
058500           MOVE FEL                   TO WS-INDATA-TEST                   
058600           MOVE MFS-ALFA-FAELT-FEL TO                                     
058700                                MOD-FLTILLBAKA-ATTR                       
058800        END-IF                                                            
058900                                                                          
059000        IF MID-FLBACKA = '+' OR 'J' OR 'Y'                                
059100           MOVE MFS-ALFA-FAELT-RAETT TO                                   
059200                                MOD-FLBACKA-ATTR                          
059300        ELSE                                                              
059400           MOVE FEL                   TO WS-INDATA-TEST                   
059500           MOVE MFS-ALFA-FAELT-FEL TO                                     
059600                                MOD-FLBACKA-ATTR                          
059700        END-IF                                                            
059800                                                                          
059900        IF MID-FLBORJA-OM = '+' OR 'J' OR 'Y'                             
060000           MOVE MFS-ALFA-FAELT-RAETT TO                                   
060100                                MOD-FLBORJA-OM-ATTR                       
060200        ELSE                                                              
060300           MOVE FEL                   TO WS-INDATA-TEST                   
060400           MOVE MFS-ALFA-FAELT-FEL TO                                     
060500                                MOD-FLBORJA-OM-ATTR                       
060600        END-IF                                                            
060700                                                                          
060800        IF WS-INDATA-RAETT                                                
060900                                                                          
061000           IF MID-FLBORJA-OM NOT = ALL '+'                                
061100              IF MID-FLBACKA NOT = ALL '+'                                
061200                 MOVE FEL                   TO WS-INDATA-TEST             
061300                 MOVE MFS-ALFA-FAELT-FEL TO                               
061400                                      MOD-FLBORJA-OM-ATTR                 
061500              END-IF                                                      
061600           END-IF                                                         
061700                                                                          
061800        END-IF                                                            
061900     END-IF                                                               
062000                                                                          
062100     .                                                                    
062200     EJECT                                                                
062300 CB-BACKA-WDG2 SECTION.                                                   
062400                                                                          
062500     PERFORM IMS-GET-XXDL-4315                                            
062600                                                                          
062700     EVALUATE TRUE                                                        
062800     WHEN MID-FLBORJA-OM = 'J' OR 'Y'                                     
062900        PERFORM CBA-BACKA-ALLT                                            
063000     WHEN MID-FLBACKA = 'J' OR 'Y'                                        
063100                                                                          
063200        EVALUATE TRUE                                                     
063300        WHEN 4312-KDBEHAND-KOL   = 2 OR 3                                 
063400           PERFORM S03-BACKA-KOL                                          
063500                                                                          
063600        WHEN 4312-KDBEHAND-DEL   = 2 OR 3                                 
063700           PERFORM S04-BACKA-DEL                                          
063800                                                                          
063900        WHEN 4312-KDBEHAND-URS   = 2 OR 3                                 
064000           PERFORM S05-BACKA-URS                                          
064100                                                                          
064200        WHEN 4312-KDBEHAND-AVVIK = 2 OR 3                                 
064300           PERFORM S06-BACKA-AVVIK                                        
064400                                                                          
064500        WHEN 4312-KDBEHAND-RAD   = 2 OR 3                                 
064600           PERFORM S07-BACKA-RAD                                          
064700                                                                          
064800        WHEN 4312-KDBEHAND-GRUND = 2 OR 3                                 
064900           PERFORM S08-BACKA-GRUND                                        
065000                                                                          
065100        END-EVALUATE                                                      
065200                                                                          
065300     END-EVALUATE                                                         
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700 CBA-BACKA-ALLT SECTION.                                                  
065800                                                                          
065900***** ALLA 4316-SEGMENTET TAS BORT: PTYP 001 - 004                        
066000     MOVE '001'       TO W-IDPTYP-4316-MIN                                
066100     MOVE '004'       TO W-IDPTYP-4316-MAX                                
066200     PERFORM IMS-GET-XXDL-4316-INTERV                                     
066300                                                                          
066400     PERFORM UNTIL SEGMENT-SAKNAS                                         
066500        PERFORM IMS-DLET-XXDL                                             
066600        PERFORM IMS-GET-XXDL-4316-INTERV                                  
066700     END-PERFORM                                                          
066800                                                                          
066900***** 4312-SEGMENTET TAS BORT                                             
067000     PERFORM IMS-DLET-XXDK                                                
067100                                                                          
067200***** 4306-SEGMENTET TAS BORT EL.                                         
067300***** OM ANNULATIONER ÄNDRAS LÅSNINGSK                                    
067400     PERFORM IMS-GET-XXDJ-4305                                            
067500     PERFORM IMS-GET-XXDJ-4306                                            
067600                                                                          
067700     IF 4306-FLANNULL = JA                                                
067800        MOVE +0             TO 4306-KDPACLAS                              
067900        PERFORM IMS-REPL-XXDJ                                             
068000     ELSE                                                                 
068100        PERFORM IMS-DLET-XXDJ                                             
068200     END-IF                                                               
068300                                                                          
068400     .                                                                    
068500     EJECT                                                                
068600 D-ENTER SECTION.                                                         
068700                                                                          
068800                                                                          
068900     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
069000        PERFORM S02-KOLL-4312                                             
069100        MOVE '+'         TO MID-FLTILLBAKA                                
069200                            MID-FLBORJA-OM                                
069300                            MID-FLBACKA                                   
069400     ELSE                                                                 
069500                                                                          
069600        IF MID-FLBORJA-OM = ALL '+'                                       
069700        AND MID-FLBACKA = ALL '+'                                         
069800           PERFORM S02-KOLL-4312                                          
069900                                                                          
070000           IF MID-FLTILLBAKA NOT = ALL '+'                                
070100           AND WS-INDATA-RAETT                                            
070200              IF 4312-KDBEHAND-RAD = +3                                   
070300              OR 4312-KDBEHAND-AVVIK = +3                                 
070400              OR 4312-KDBEHAND-URS = +3                                   
070500              OR 4312-KDBEHAND-DEL = +3                                   
070600              OR 4312-KDBEHAND-KOL = +3                                   
070700                 MOVE FEL          TO WS-INDATA-TEST                      
070800                 MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                        
070900                 PERFORM S10-ROER-EJ-FAELT                                
071000                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-ATTR             
071100                                              MOD-FLTILLBAKA-ATTR         
071200                                              MOD-FLBORJA-OM-ATTR         
071300                                              MOD-FLBACKA-ATTR            
071400              END-IF                                                      
071500           END-IF                                                         
071600        ELSE                                                              
071700           MOVE FEL          TO WS-INDATA-TEST                            
071800           MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                              
071900           PERFORM S10-ROER-EJ-FAELT                                      
072000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-ATTR                   
072100                                        MOD-FLTILLBAKA-ATTR               
072200                                        MOD-FLBORJA-OM-ATTR               
072300                                        MOD-FLBACKA-ATTR                  
072400        END-IF                                                            
072500                                                                          
072600     END-IF                                                               
072700                                                                          
072800     .                                                                    
072900     EJECT                                                                
073000 E-RENSA-NYCKLAR SECTION.                                                 
073100     MOVE MFS-RENSA-FAELT            TO MOD-IDPRODNR-UT                   
073200                                        MOD-IDDISTR-UT                    
073300                                        MOD-IDKUNDNR-UT                   
073400                                        MOD-KDFRAKT-UT                    
073500                                        MOD-IDORDNR-UT                    
073600                                        MOD-KDORDKL-UT                    
073700     .                                                                    
073800     EJECT                                                                
073900 S02-KOLL-4312 SECTION.                                                   
074000                                                                          
074100     PERFORM IMS-GET-XXDK-4311                                            
074200     PERFORM IMS-GET-XXDK-4312                                            
074300                                                                          
074400     IF SEGMENT-FINNS                                                     
074500        MOVE 4312-IDTRANS    TO WS-IDTRANS-4312                           
074600       IF WS-EGEN-BILD                                                    
074700          IF MID-IDUSER  = ALL '+'                                        
074800                                                                          
074900             IF MSG-SIGNON-USERID NOT = 4312-IDUSER                       
075000                MOVE FEL          TO WS-INDATA-TEST                       
075100                MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                         
075200             END-IF                                                       
075300          ELSE                                                            
075400             IF MID-IDUSER NOT = 4312-IDUSER                              
075500                MOVE FEL          TO WS-INDATA-TEST                       
075600                MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                         
075700                MOVE MFS-ALFA-FAELT-FEL TO                                
075800                               MOD-IDUSER-ATTR                            
075900             END-IF                                                       
076000          END-IF                                                          
076100       ELSE                                                               
076200          IF MSG-SIGNON-USERID NOT = 4312-IDUSER                          
076300             MOVE FEL          TO WS-INDATA-TEST                          
076400             MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                            
076500          END-IF                                                          
076600        END-IF                                                            
076700                                                                          
076800     ELSE                                                                 
076900        MOVE FEL          TO WS-INDATA-TEST                               
077000        MOVE FEL-4 (INDX) TO MOD-TEMFSFEL                                 
077100     END-IF                                                               
077200                                                                          
077300     .                                                                    
077400     EJECT                                                                
077500 S03-BACKA-KOL  SECTION.                                                  
077600                                                                          
077700***** BACKAR UT KOLLI-INF FRÅN PTYP 002 I 4316-SEGMENTEN                  
077800     MOVE '002'       TO W-IDPTYP-4316-MIN                                
077900                         W-IDPTYP-4316-MAX                                
078000     PERFORM IMS-GET-XXDL-4316-INTERV                                     
078100                                                                          
078200     PERFORM UNTIL SEGMENT-SAKNAS                                         
078300        MOVE WS-PLUS     TO 4316-A-MID-KDKOLLI                            
078400                            4316-A-MID-VKORDBTO-KOLLI                     
078500                            4316-A-MID-KDEMBTYP                           
078600                            4316-A-MID-DIKOLLIL                           
078700                            4316-A-MID-DIKOLLIB                           
078800                            4316-A-MID-DIKOLLIH                           
078900                            4316-A-MID-ADFLGEO                            
079000                            4316-A-MID-ADFLOMR                            
079100                            4316-A-MID-ADRUTNIV                           
079200        PERFORM IMS-REPL-XXDL                                             
079300        PERFORM IMS-GET-XXDL-4316-INTERV                                  
079400     END-PERFORM                                                          
079500                                                                          
079600     MOVE +1             TO 4312-KDBEHAND-KOL                             
079700     PERFORM IMS-REPL-XXDK                                                
079800                                                                          
079900     .                                                                    
080000     EJECT                                                                
080100 S04-BACKA-DEL  SECTION.                                                  
080200                                                                          
080300     PERFORM IMS-GET-XXDK-4314                                            
080400                                                                          
080500     PERFORM UNTIL SEGMENT-SAKNAS                                         
080600        MOVE 4314-IDRADNR    TO WS-IDRADNR                                
080700        MOVE '002'       TO W-IDPTYP-4316-MIN                             
080800                            W-IDPTYP-4316-MAX                             
080900        PERFORM IMS-GET-XXDL-4316-INTERV-F                                
081000                                                                          
081100        PERFORM UNTIL SEGMENT-SAKNAS                                      
081200           MOVE 4316-IDKOLLI  TO WS-IDKOLLI-TRAEFF                        
081300           PERFORM S04A-SOEK-TAB-002                                      
081400           MOVE '003'       TO W-IDPTYP-4316                              
081500           MOVE WS-IDKOLLI-TRAEFF TO W-IDKOLLI-4316                       
081600           PERFORM IMS-GET-XXDL-4316                                      
081700                                                                          
081800           PERFORM UNTIL SEGMENT-SAKNAS                                   
081900              PERFORM S04B-SOEK-TAB-003                                   
082000              PERFORM IMS-GET-XXDL-4316                                   
082100           END-PERFORM                                                    
082200                                                                          
082300           MOVE '002'       TO W-IDPTYP-4316-KOL-MIN                      
082400                               W-IDPTYP-4316-MAX                          
082500           MOVE WS-IDKOLLI-TRAEFF TO W-IDKOLLI-4316-KOL-MIN               
082600           PERFORM IMS-GET-XXDL-4316-INTERV-F-KOL                         
082700        END-PERFORM                                                       
082800                                                                          
082900        MOVE +1             TO 4314-KDBEHAND-RAD                          
083000        PERFORM IMS-REPL-XXDK                                             
083100                                                                          
083200        PERFORM IMS-GET-XXDK-4314                                         
083300     END-PERFORM                                                          
083400                                                                          
083500     PERFORM IMS-GET-XXDK-4312                                            
083600     MOVE +1         TO 4312-KDBEHAND-DEL                                 
083700     PERFORM IMS-REPL-XXDK                                                
083800                                                                          
083900     .                                                                    
084000     EJECT                                                                
084100 S04A-SOEK-TAB-002 SECTION.                                               
084200                                                                          
084300     MOVE NEJ         TO WS-FLTRAEFF-TEST                                 
084400     MOVE +1          TO 4316-IND                                         
084500                                                                          
084600     PERFORM UNTIL 4316-IND NOT < MAX-4316-IND                            
084700               AND 4316-IND NOT = MAX-4316-IND                            
084800                                                                          
084900        IF WS-IDRADNR-X = 4316-A-MID-IDRADNR-FOM (4316-IND)               
085000           MOVE ALL '+'   TO 4316-A-MID-RAD (4316-IND)                    
085100           MOVE JA        TO WS-FLTRAEFF-TEST                             
085200        END-IF                                                            
085300                                                                          
085400        ADD +1           TO 4316-IND                                      
085500     END-PERFORM                                                          
085600                                                                          
085700     IF WS-FLTRAEFF                                                       
085800        IF 4316-A-MID-RAD (1) = ALL '+'                                   
085900           PERFORM IMS-DLET-XXDL                                          
086000        ELSE                                                              
086100           PERFORM IMS-REPL-XXDL                                          
086200        END-IF                                                            
086300     END-IF                                                               
086400                                                                          
086500     .                                                                    
086600     EJECT                                                                
086700 S04B-SOEK-TAB-003 SECTION.                                               
086800                                                                          
086900     MOVE NEJ         TO WS-FLTRAEFF-TEST                                 
087000     MOVE +1          TO 4316-IND                                         
087100                                                                          
087200     PERFORM UNTIL 4316-IND NOT < MAX-4316-IND                            
087300               AND 4316-IND NOT = MAX-4316-IND                            
087400                                                                          
087500        IF WS-IDRADNR-X = 4316-B-MID-IDRADNR-FOM (4316-IND)               
087600           MOVE ALL '+'   TO 4316-B-MID-RAD (4316-IND)                    
087700           MOVE JA        TO WS-FLTRAEFF-TEST                             
087800        END-IF                                                            
087900                                                                          
088000        ADD +1           TO 4316-IND                                      
088100     END-PERFORM                                                          
088200                                                                          
088300     IF WS-FLTRAEFF                                                       
088400        IF 4316-B-MID-RAD (1) = ALL '+'                                   
088500           PERFORM IMS-DLET-XXDL                                          
088600        ELSE                                                              
088700           PERFORM IMS-REPL-XXDL                                          
088800        END-IF                                                            
088900     END-IF                                                               
089000                                                                          
089100     .                                                                    
089200     EJECT                                                                
089300 S05-BACKA-URS  SECTION.                                                  
089400                                                                          
089500***** 4316-SEGMENTET TAS BORT: PTYP 001                                   
089600     MOVE '001'       TO W-IDPTYP-4316                                    
089700     MOVE +0          TO W-IDKOLLI-4316                                   
089800     PERFORM IMS-GET-XXDL-4316                                            
089900                                                                          
090000     PERFORM UNTIL SEGMENT-SAKNAS                                         
090100        PERFORM IMS-DLET-XXDL                                             
090200        PERFORM IMS-GET-XXDL-4316                                         
090300     END-PERFORM                                                          
090400                                                                          
090500     MOVE +1             TO 4312-KDBEHAND-URS                             
090600     PERFORM IMS-REPL-XXDK                                                
090700                                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 S06-BACKA-AVVIK SECTION.                                                 
091100                                                                          
091200*****  PTYP 002 :    KAN SAKNAS OM INTE                                   
091300*****  PTYP 002 :    FINNS BARA EN,                                       
091400*****                INF. I HUVUDET SPARAS                                
091500*****                TABELLEN FYLLS MED PLUS                              
091600*****  PTYP 003 :    TAS BORT                                             
091700*****  PTYP 004 :    EN SPARAS ALLTID MED IFYLLD IDENTITET                
091800*****                TABELLEN FYLLS MED PLUS                              
091900*****                ÖVRIGA TAS BORT                                      
092000                                                                          
092100     MOVE '002'       TO W-IDPTYP-4316-MIN                                
092200                         W-IDPTYP-4316-MAX                                
092300     PERFORM IMS-GET-XXDL-4316-INTERV                                     
092400                                                                          
092500     IF SEGMENT-FINNS                                                     
092600        MOVE 4316-IDKOLLI   TO WS-IDKOLLI-TRAEFF                          
092700                                                                          
092800        PERFORM S06-PLUS-002                                              
092900        PERFORM IMS-REPL-XXDL                                             
093000                                                                          
093100        MOVE '003'             TO W-IDPTYP-4316                           
093200        MOVE WS-IDKOLLI-TRAEFF TO W-IDKOLLI-4316                          
093300        PERFORM IMS-GET-XXDL-4316                                         
093400                                                                          
093500        PERFORM UNTIL SEGMENT-SAKNAS                                      
093600           PERFORM IMS-DLET-XXDL                                          
093700           PERFORM IMS-GET-XXDL-4316                                      
093800        END-PERFORM                                                       
093900     END-IF                                                               
094000                                                                          
094100                                                                          
094200     MOVE '004'             TO W-IDPTYP-4316                              
094300     MOVE +0                TO W-IDKOLLI-4316                             
094400     PERFORM IMS-GET-XXDL-4316-BLANK                                      
094500     PERFORM S06-S07-PLUS-004                                             
094600     PERFORM IMS-REPL-XXDL                                                
094700     PERFORM IMS-GET-XXDL-4316                                            
094800                                                                          
094900     PERFORM UNTIL SEGMENT-SAKNAS                                         
095000        PERFORM IMS-DLET-XXDL                                             
095100        PERFORM IMS-GET-XXDL-4316                                         
095200     END-PERFORM                                                          
095300                                                                          
095400     MOVE +1        TO 4312-KDBEHAND-AVVIK                                
095500                                                                          
095600     PERFORM IMS-REPL-XXDK                                                
095700                                                                          
095800     .                                                                    
095900     EJECT                                                                
096000 S06-PLUS-002 SECTION.                                                    
096100                                                                          
096200     MOVE +1          TO 4316-IND                                         
096300                                                                          
096400     PERFORM UNTIL 4316-IND NOT < MAX-4316-IND                            
096500               AND 4316-IND NOT = MAX-4316-IND                            
096600        MOVE ALL '+'   TO 4316-A-MID-RAD (4316-IND)                       
096700        ADD +1           TO 4316-IND                                      
096800     END-PERFORM                                                          
096900                                                                          
097000     .                                                                    
097100     EJECT                                                                
097200 S06-S07-PLUS-004 SECTION.                                                
097300                                                                          
097400     MOVE +1          TO 4316-IND                                         
097500                                                                          
097600     PERFORM UNTIL 4316-IND NOT < MAX-4316-IND-004                        
097700               AND 4316-IND NOT = MAX-4316-IND-004                        
097800        MOVE ALL '+'   TO 4316-C-MID-RAD (4316-IND)                       
097900        ADD +1           TO 4316-IND                                      
098000     END-PERFORM                                                          
098100                                                                          
098200     .                                                                    
098300     EJECT                                                                
098400 S07-BACKA-RAD   SECTION.                                                 
098500                                                                          
098600*****  PTYP 002 :   -TAS BORT (FLERA KOLLI)                               
098700*****  PTYP 003 :   -TAS BORT                                             
098800*****  PTYP 004 :   -EN SPARAS ALLTID MED IFYLLD IDENTITET                
098900*****                TABELLEN FYLLS MED PLUS                              
099000*****                ÖVRIGA TAS BORT                                      
099100                                                                          
099200     MOVE '002'       TO W-IDPTYP-4316-MIN                                
099300                         W-IDPTYP-4316-MAX                                
099400     PERFORM IMS-GET-XXDL-4316-INTERV                                     
099500                                                                          
099600     PERFORM UNTIL SEGMENT-SAKNAS                                         
099700        MOVE 4316-IDKOLLI   TO WS-IDKOLLI-TRAEFF                          
099800        PERFORM IMS-DLET-XXDL                                             
099900                                                                          
100000        MOVE '003'             TO W-IDPTYP-4316                           
100100        MOVE WS-IDKOLLI-TRAEFF TO W-IDKOLLI-4316                          
100200        PERFORM IMS-GET-XXDL-4316                                         
100300                                                                          
100400        PERFORM UNTIL SEGMENT-SAKNAS                                      
100500           PERFORM IMS-DLET-XXDL                                          
100600           PERFORM IMS-GET-XXDL-4316                                      
100700        END-PERFORM                                                       
100800                                                                          
100900        MOVE '002'             TO W-IDPTYP-4316-KOL-MIN                   
101000                                  W-IDPTYP-4316-MAX                       
101100        PERFORM IMS-GET-XXDL-4316-INTERV-F                                
101200     END-PERFORM                                                          
101300                                                                          
101400     MOVE '004'             TO W-IDPTYP-4316                              
101500     MOVE +0                TO W-IDKOLLI-4316                             
101600     PERFORM IMS-GET-XXDL-4316-BLANK                                      
101700     PERFORM S06-S07-PLUS-004                                             
101800     PERFORM IMS-REPL-XXDL                                                
101900     PERFORM IMS-GET-XXDL-4316                                            
102000                                                                          
102100     PERFORM UNTIL SEGMENT-SAKNAS                                         
102200        PERFORM IMS-DLET-XXDL                                             
102300        PERFORM IMS-GET-XXDL-4316                                         
102400     END-PERFORM                                                          
102500                                                                          
102600     PERFORM IMS-GET-XXDK-4314                                            
102700                                                                          
102800     PERFORM UNTIL SEGMENT-SAKNAS                                         
102900        PERFORM IMS-DLET-XXDK                                             
103000        PERFORM IMS-GET-XXDK-4314                                         
103100     END-PERFORM                                                          
103200                                                                          
103300     PERFORM IMS-GET-XXDK-4312                                            
103400                                                                          
103500     MOVE +1        TO 4312-KDBEHAND-RAD                                  
103600     MOVE +0        TO 4312-KDBEHAND-DEL                                  
103700                                                                          
103800     PERFORM IMS-REPL-XXDK                                                
103900                                                                          
104000     .                                                                    
104100     EJECT                                                                
104200 S08-BACKA-GRUND SECTION.                                                 
104300                                                                          
104400***** ALLA 4316-SEGMENTET TAS BORT: PTYP 002 - 004                        
104500     MOVE '002'       TO W-IDPTYP-4316-MIN                                
104600     MOVE '004'       TO W-IDPTYP-4316-MAX                                
104700     PERFORM IMS-GET-XXDL-4316-INTERV                                     
104800                                                                          
104900     PERFORM UNTIL SEGMENT-SAKNAS                                         
105000        PERFORM IMS-DLET-XXDL                                             
105100        PERFORM IMS-GET-XXDL-4316-INTERV                                  
105200     END-PERFORM                                                          
105300                                                                          
105400***** 4312-SEGMENTET TAS BORT                                             
105500     PERFORM IMS-DLET-XXDK                                                
105600                                                                          
105700***** 4306-SEGMENTET TAS BORT EL. OM ANNULLATIONERÄNDRAS LÅSNINGSK        
105800     PERFORM IMS-GET-XXDJ-4305                                            
105900     PERFORM IMS-GET-XXDJ-4306                                            
106000                                                                          
106100     IF 4306-FLANNULL = JA                                                
106200        MOVE +0             TO 4306-KDPACLAS                              
106300        PERFORM IMS-REPL-XXDJ                                             
106400     ELSE                                                                 
106500        PERFORM IMS-DLET-XXDJ                                             
106600     END-IF                                                               
106700                                                                          
106800     .                                                                    
106900     EJECT                                                                
107000 S09-VISA-4305 SECTION.                                                   
107100                                                                          
107200     MOVE 4312-IDDISTR          TO W-IDDISTR                              
107300     MOVE W-IDDISTR             TO MOD-IDDISTR-UT                         
107400     MOVE 4312-IDKUNDNR         TO W-IDKUNDNR                             
107500     MOVE W-IDKUNDNR            TO MOD-IDKUNDNR-UT                        
107600     MOVE 4312-KDFRAKT          TO W-KDFRAKT                              
107700     MOVE W-KDFRAKT             TO MOD-KDFRAKT-UT                         
107800     MOVE 4312-IDKUNDRF         TO MOD-IDORDNR-UT                         
107900     MOVE 4312-KDORDKL          TO W-KDORDKL                              
108000     MOVE W-KDORDKL             TO MOD-KDORDKL-UT                         
108010     MOVE WS-IDDC               TO MOD-IDDC-UT                            
108100     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
108200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
108300     INSPECT MOD-KDFRAKT-UT  REPLACING LEADING ZERO BY SPACE              
108400     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
108500     MOVE 4312-KDBEHAND-GRUND   TO MOD-KDBEHAND-GRUND                     
108600     MOVE 4312-KDBEHAND-RAD     TO MOD-KDBEHAND-RAD                       
108700     MOVE 4312-KDBEHAND-DEL     TO MOD-KDBEHAND-DEL                       
108800     MOVE 4312-KDBEHAND-KOL     TO MOD-KDBEHAND-KOL                       
108900     MOVE 4312-KDBEHAND-AVVIK   TO MOD-KDBEHAND-AVVIK                     
109000     MOVE 4312-KDBEHAND-URS     TO MOD-KDBEHAND-URS                       
109100                                                                          
109200*************************************************************             
109300************ RÖR EJ ORDNINGEN AV TEST PÅ KDBEHAND ***********             
109400                                                                          
109500     EVALUATE TRUE                                                        
109600     WHEN 4312-KDBEHAND-GRUND = 0                                         
109700        MOVE WS-TESKALLEJ (INDX)   TO MOD-TEGRUND                         
109800     WHEN 4312-KDBEHAND-GRUND = 1                                         
109900        MOVE WS-TEEJBEARB (INDX)   TO MOD-TEGRUND                         
110000     WHEN 4312-KDBEHAND-GRUND = 2                                         
110100        MOVE WS-TEBEARB (INDX)     TO MOD-TEGRUND                         
110200                                                                          
110300        EVALUATE TRUE                                                     
110400        WHEN 4312-IDTRANS = '4301'                                        
110500           MOVE WS-TEBACKA-4301 TO MOD-TEBACKA-PGM                        
110600        WHEN 4312-IDTRANS = '4302'                                        
110700           MOVE WS-TEBACKA-4302 TO MOD-TEBACKA-PGM                        
110800        WHEN 4312-IDTRANS = '4303'                                        
110900           MOVE WS-TEBACKA-4303 TO MOD-TEBACKA-PGM                        
111000        WHEN 4312-IDTRANS = '4306'                                        
111100           MOVE WS-TEBACKA-4306 TO MOD-TEBACKA-PGM                        
111200        END-EVALUATE                                                      
111300                                                                          
111400     WHEN 4312-KDBEHAND-GRUND = 3                                         
111500        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TEGRUND                         
111600                                                                          
111700        EVALUATE TRUE                                                     
111800        WHEN 4312-IDTRANS = '4301'                                        
111900           MOVE WS-TEBACKA-4301 TO MOD-TEBACKA-PGM                        
112000        WHEN 4312-IDTRANS = '4302'                                        
112100           MOVE WS-TEBACKA-4302 TO MOD-TEBACKA-PGM                        
112200        WHEN 4312-IDTRANS = '4303'                                        
112300           MOVE WS-TEBACKA-4303 TO MOD-TEBACKA-PGM                        
112400        WHEN 4312-IDTRANS = '4306'                                        
112500           MOVE WS-TEBACKA-4306 TO MOD-TEBACKA-PGM                        
112600        END-EVALUATE                                                      
112700                                                                          
112800     END-EVALUATE                                                         
112900                                                                          
113000     EVALUATE TRUE                                                        
113100     WHEN 4312-KDBEHAND-RAD = 0                                           
113200        MOVE WS-TESKALLEJ (INDX)   TO MOD-TERAD                           
113300     WHEN 4312-KDBEHAND-RAD = 1                                           
113400        MOVE WS-TEEJBEARB (INDX)   TO MOD-TERAD                           
113500     WHEN 4312-KDBEHAND-RAD = 2                                           
113600        MOVE WS-TEBEARB (INDX)     TO MOD-TERAD                           
113700        MOVE WS-TEBACKA-4393 TO MOD-TEBACKA-PGM                           
113800     WHEN 4312-KDBEHAND-RAD = 3                                           
113900        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TERAD                           
114000        MOVE WS-TEBACKA-4393 TO MOD-TEBACKA-PGM                           
114100     END-EVALUATE                                                         
114200                                                                          
114300     EVALUATE TRUE                                                        
114400     WHEN 4312-KDBEHAND-AVVIK = 0                                         
114500        MOVE WS-TESKALLEJ (INDX)   TO MOD-TEAVVIK                         
114600     WHEN 4312-KDBEHAND-AVVIK = 1                                         
114700        MOVE WS-TEEJBEARB (INDX)   TO MOD-TEAVVIK                         
114800     WHEN 4312-KDBEHAND-AVVIK = 2                                         
114900        MOVE WS-TEBEARB (INDX)     TO MOD-TEAVVIK                         
115000        MOVE WS-TEBACKA-4391 TO MOD-TEBACKA-PGM                           
115100     WHEN 4312-KDBEHAND-AVVIK = 3                                         
115200        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TEAVVIK                         
115300        MOVE WS-TEBACKA-4391 TO MOD-TEBACKA-PGM                           
115400     END-EVALUATE                                                         
115500                                                                          
115600     EVALUATE TRUE                                                        
115700     WHEN 4312-KDBEHAND-URS = 0                                           
115800        MOVE WS-TESKALLEJ (INDX)   TO MOD-TEURS                           
115900     WHEN 4312-KDBEHAND-URS = 1                                           
116000        MOVE WS-TEEJBEARB (INDX)   TO MOD-TEURS                           
116100     WHEN 4312-KDBEHAND-URS = 2                                           
116200        MOVE WS-TEBEARB (INDX)     TO MOD-TEURS                           
116300        MOVE WS-TEBACKA-4394 TO MOD-TEBACKA-PGM                           
116400     WHEN 4312-KDBEHAND-URS = 3                                           
116500        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TEURS                           
116600        MOVE WS-TEBACKA-4394 TO MOD-TEBACKA-PGM                           
116700     END-EVALUATE                                                         
116800                                                                          
116900     EVALUATE TRUE                                                        
117000     WHEN 4312-KDBEHAND-DEL = 0                                           
117100        MOVE WS-TESKALLEJ (INDX)   TO MOD-TEDEL                           
117200     WHEN 4312-KDBEHAND-DEL = 1                                           
117300        MOVE WS-TEEJBEARB (INDX)   TO MOD-TEDEL                           
117400     WHEN 4312-KDBEHAND-DEL = 2                                           
117500        MOVE WS-TEBEARB (INDX)     TO MOD-TEDEL                           
117600        MOVE WS-TEBACKA-4395 TO MOD-TEBACKA-PGM                           
117700     WHEN 4312-KDBEHAND-DEL = 3                                           
117800        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TEDEL                           
117900        MOVE WS-TEBACKA-4395 TO MOD-TEBACKA-PGM                           
118000     END-EVALUATE                                                         
118100                                                                          
118200     EVALUATE TRUE                                                        
118300     WHEN 4312-KDBEHAND-KOL = 0                                           
118400        MOVE WS-TESKALLEJ (INDX)   TO MOD-TEKOL                           
118500     WHEN 4312-KDBEHAND-KOL = 1                                           
118600        MOVE WS-TEEJBEARB (INDX)   TO MOD-TEKOL                           
118700     WHEN 4312-KDBEHAND-KOL = 2                                           
118800        MOVE WS-TEBEARB (INDX)     TO MOD-TEKOL                           
118900        MOVE WS-TEBACKA-4396 TO MOD-TEBACKA-PGM                           
119000     WHEN 4312-KDBEHAND-KOL = 3                                           
119100        MOVE WS-TEPAGAENDE (INDX)  TO MOD-TEKOL                           
119200        MOVE WS-TEBACKA-4396 TO MOD-TEBACKA-PGM                           
119300     END-EVALUATE                                                         
119400                                                                          
119500     .                                                                    
119600     EJECT                                                                
119700 S10-ROER-EJ-FAELT SECTION.                                               
119800                                                                          
119900      MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER                                
120000                                MOD-FLTILLBAKA                            
120100                                MOD-KDBEHAND-GRUND                        
120200                                MOD-TEGRUND                               
120300                                MOD-KDBEHAND-RAD                          
120400                                MOD-TERAD                                 
120500                                MOD-KDBEHAND-DEL                          
120600                                MOD-TEDEL                                 
120700                                MOD-KDBEHAND-KOL                          
120800                                MOD-TEKOL                                 
120900                                MOD-KDBEHAND-AVVIK                        
121000                                MOD-TEAVVIK                               
121100                                MOD-KDBEHAND-URS                          
121200                                MOD-TEURS                                 
121300                                MOD-FLBORJA-OM                            
121400                                MOD-TEBACKA-PGM                           
121500                                MOD-FLBACKA                               
121600                                                                          
121700     .                                                                    
121800     EJECT                                                                
121900 S11-FORMATETS-ATTR SECTION.                                              
122000                                                                          
122100       MOVE MFS-FORMATETS-ATTR TO MOD-FLTILLBAKA-ATTR                     
122200                                  MOD-FLBORJA-OM-ATTR                     
122300                                  MOD-FLBACKA-ATTR                        
122400                                                                          
122500     .                                                                    
122600     EJECT                                                                
122700 S99-NAESTA-TRANS SECTION.                                                
122800                                                                          
122900     PERFORM IMS-GET-XXDK-4312                                            
123000                                                                          
123100     IF SEGMENT-FINNS                                                     
123200        IF MID-FLTILLBAKA = 'J' OR 'Y'                                    
123300           MOVE 4312-IDDISTR       TO W-IDDISTR                           
123400           MOVE 4312-IDKUNDNR      TO W-IDKUNDNR                          
123500           MOVE 4312-KDFRAKT       TO W-KDFRAKT                           
123600           MOVE 4312-KDORDKL       TO W-KDORDKL                           
123700                                                                          
123800           EVALUATE TRUE                                                  
123900           WHEN 4312-KDBEHAND-RAD = +1                                    
124000              MOVE 'W4O39301'    TO MFS-IDMOD                             
124100              MOVE M439X-MOD-LAENGD TO MSG-KVLL                           
124200              MOVE '4393'             TO M4393-MOD-IDTRANS                
124300              MOVE W-IDPRODNR         TO M4393-MOD-IDPRODNR-UT            
124400              MOVE W-IDDISTR          TO M4393-MOD-IDDISTR-UT             
124500              MOVE W-IDKUNDNR         TO M4393-MOD-IDKUNDNR-UT            
124600              MOVE W-KDFRAKT          TO M4393-MOD-KDFRAKT-UT             
124700              MOVE 4312-IDKUNDRF      TO M4393-MOD-IDORDNR-UT             
124800              MOVE W-KDORDKL          TO M4393-MOD-KDORDKL-UT             
124900              MOVE WS-IDDC            TO M4393-MOD-IDDC-UT                
125000                                                                          
125100              INSPECT M4393-MOD-IDPRODNR-UT REPLACING                     
125200                                      LEADING ZERO BY SPACE               
125300              INSPECT M4393-MOD-IDDISTR-UT REPLACING                      
125400                                      LEADING ZERO BY SPACE               
125500              INSPECT M4393-MOD-IDKUNDNR-UT REPLACING                     
125600                                      LEADING ZERO BY SPACE               
125700              INSPECT M4393-MOD-KDFRAKT-UT REPLACING                      
125800                                      LEADING ZERO BY SPACE               
125900              INSPECT M4393-MOD-IDORDNR-UT REPLACING                      
126000                                      LEADING ZERO BY SPACE               
126100                                                                          
126200           WHEN 4312-KDBEHAND-AVVIK = +1                                  
126300              MOVE 'W4O39101'    TO MFS-IDMOD                             
126400              MOVE M439X-MOD-LAENGD TO MSG-KVLL                           
126500              MOVE '4391'             TO M4391-MOD-IDTRANS                
126600              MOVE W-IDPRODNR         TO M4391-MOD-IDPRODNR-UT            
126700              MOVE W-IDDISTR          TO M4391-MOD-IDDISTR-UT             
126800              MOVE W-IDKUNDNR         TO M4391-MOD-IDKUNDNR-UT            
126900              MOVE W-KDFRAKT          TO M4391-MOD-KDFRAKT-UT             
127000              MOVE 4312-IDKUNDRF      TO M4391-MOD-IDORDNR-UT             
127100              MOVE W-KDORDKL          TO M4391-MOD-KDORDKL-UT             
127200              MOVE WS-IDDC            TO M4391-MOD-IDDC-UT                
127300                                                                          
127400              INSPECT M4391-MOD-IDPRODNR-UT REPLACING                     
127500                                      LEADING ZERO BY SPACE               
127600              INSPECT M4391-MOD-IDDISTR-UT REPLACING                      
127700                                      LEADING ZERO BY SPACE               
127800              INSPECT M4391-MOD-IDKUNDNR-UT REPLACING                     
127900                                      LEADING ZERO BY SPACE               
128000              INSPECT M4391-MOD-KDFRAKT-UT REPLACING                      
128100                                      LEADING ZERO BY SPACE               
128200              INSPECT M4391-MOD-IDORDNR-UT REPLACING                      
128300                                      LEADING ZERO BY SPACE               
128400                                                                          
128500           WHEN 4312-KDBEHAND-URS = +1                                    
128600                                                                          
128700              IF WS-IDTRANS-4312 = '4302' OR '4306'                       
128800                 MOVE 'W4O39401'    TO MFS-IDMOD                          
128900                 MOVE M439X-MOD-LAENGD TO MSG-KVLL                        
129000                 MOVE '4394'             TO M4394-MOD-IDTRANS             
129100                 MOVE W-IDPRODNR         TO M4394-MOD-IDPRODNR-UT         
129200                 MOVE W-IDDISTR          TO M4394-MOD-IDDISTR-UT          
129300                 MOVE W-IDKUNDNR         TO M4394-MOD-IDKUNDNR-UT         
129400                 MOVE W-KDFRAKT          TO M4394-MOD-KDFRAKT-UT          
129500                 MOVE 4312-IDKUNDRF      TO M4394-MOD-IDORDNR-UT          
129600                 MOVE W-KDORDKL          TO M4394-MOD-KDORDKL-UT          
129700                 MOVE WS-IDDC            TO M4394-MOD-IDDC-UT             
129800                                                                          
129900                 INSPECT M4394-MOD-IDPRODNR-UT REPLACING                  
130000                                         LEADING ZERO BY SPACE            
130100                 INSPECT M4394-MOD-IDDISTR-UT REPLACING                   
130200                                         LEADING ZERO BY SPACE            
130300                 INSPECT M4394-MOD-IDKUNDNR-UT REPLACING                  
130400                                         LEADING ZERO BY SPACE            
130500                 INSPECT M4394-MOD-KDFRAKT-UT REPLACING                   
130600                                         LEADING ZERO BY SPACE            
130700                 INSPECT M4394-MOD-IDORDNR-UT REPLACING                   
130800                                         LEADING ZERO BY SPACE            
130900                                                                          
131000              END-IF                                                      
131100                                                                          
131200           WHEN 4312-KDBEHAND-DEL = +1                                    
131300                                                                          
131400              IF WS-IDTRANS-4312 = '4302' OR '4306'                       
131500                 MOVE 'W4O39501'    TO MFS-IDMOD                          
131600                 MOVE M439X-MOD-LAENGD TO MSG-KVLL                        
131700                 MOVE '4395'             TO M4395-MOD-IDTRANS             
131800                 MOVE W-IDPRODNR         TO M4395-MOD-IDPRODNR-UT         
131900                 MOVE W-IDDISTR          TO M4395-MOD-IDDISTR-UT          
132000                 MOVE W-IDKUNDNR         TO M4395-MOD-IDKUNDNR-UT         
132100                 MOVE W-KDFRAKT          TO M4395-MOD-KDFRAKT-UT          
132200                 MOVE 4312-IDKUNDRF      TO M4395-MOD-IDORDNR-UT          
132300                 MOVE W-KDORDKL          TO M4395-MOD-KDORDKL-UT          
132310                 MOVE WS-IDDC            TO M4395-MOD-IDDC-UT             
132400                                                                          
132500                 INSPECT M4395-MOD-IDPRODNR-UT REPLACING                  
132600                                         LEADING ZERO BY SPACE            
132700                 INSPECT M4395-MOD-IDDISTR-UT REPLACING                   
132800                                         LEADING ZERO BY SPACE            
132900                 INSPECT M4395-MOD-IDKUNDNR-UT REPLACING                  
133000                                         LEADING ZERO BY SPACE            
133100                 INSPECT M4395-MOD-KDFRAKT-UT REPLACING                   
133200                                         LEADING ZERO BY SPACE            
133300                 INSPECT M4395-MOD-IDORDNR-UT REPLACING                   
133400                                         LEADING ZERO BY SPACE            
133500              END-IF                                                      
133600                                                                          
133700           WHEN 4312-KDBEHAND-KOL = +1                                    
133800                                                                          
133900              IF WS-IDTRANS-4312 = '4302' OR '4306'                       
134000                 MOVE 'W4O39601'    TO MFS-IDMOD                          
134100                 MOVE M439X-MOD-LAENGD TO MSG-KVLL                        
134200                 MOVE '4396'             TO M4396-MOD-IDTRANS             
134300                 MOVE W-IDPRODNR         TO M4396-MOD-IDPRODNR-UT         
134400                 MOVE W-IDDISTR          TO M4396-MOD-IDDISTR-UT          
134500                 MOVE W-IDKUNDNR         TO M4396-MOD-IDKUNDNR-UT         
134600                 MOVE W-KDFRAKT          TO M4396-MOD-KDFRAKT-UT          
134700                 MOVE 4312-IDKUNDRF      TO M4396-MOD-IDORDNR-UT          
134800                 MOVE W-KDORDKL          TO M4396-MOD-KDORDKL-UT          
134900                 MOVE WS-IDDC            TO M4396-MOD-IDDC-UT             
135000                                                                          
135100                 INSPECT M4396-MOD-IDPRODNR-UT REPLACING                  
135200                                         LEADING ZERO BY SPACE            
135300                 INSPECT M4396-MOD-IDDISTR-UT REPLACING                   
135400                                         LEADING ZERO BY SPACE            
135500                 INSPECT M4396-MOD-IDKUNDNR-UT REPLACING                  
135600                                         LEADING ZERO BY SPACE            
135700                 INSPECT M4396-MOD-KDFRAKT-UT REPLACING                   
135800                                         LEADING ZERO BY SPACE            
135900                 INSPECT M4396-MOD-IDORDNR-UT REPLACING                   
136000                                         LEADING ZERO BY SPACE            
136100              END-IF                                                      
136200                                                                          
136300           END-EVALUATE                                                   
136400                                                                          
136500        ELSE                                                              
136600           PERFORM S09-VISA-4305                                          
136700        END-IF                                                            
136800                                                                          
136900     ELSE                                                                 
137000                                                                          
137100        IF MID-FLTILLBAKA = 'J' OR 'Y'                                    
137200           MOVE WS-IDTRANS-4312        TO  WS-IDTRANS-MOD                 
137300           MOVE WS-IDTRANS-POS-1-MOD   TO WS-MOD-IDTRANS-POS-1            
137400           MOVE WS-IDTRANS-POS-2-4-MOD TO WS-MOD-IDTRANS-POS-2-4          
137500           MOVE WS-MODNAMN             TO MFS-IDMOD                       
137600           MOVE M430X-MOD-LAENGD       TO MSG-KVLL                        
137700           MOVE WS-IDTRANS-MOD         TO M4301-MOD-IDTRANS               
137800                                          M4302-MOD-IDTRANS               
137900                                          M4303-MOD-IDTRANS               
137910           MOVE WS-IDDC                TO M4301-MOD-IDDC-UT               
137920                                          M4302-MOD-IDDC-UT               
137930                                          M4303-MOD-IDDC-UT               
138000        ELSE                                                              
138100**** LÄGG UT EN TOM 4305                                                  
138200           PERFORM S11-FORMATETS-ATTR                                     
138300        END-IF                                                            
138400     END-IF                                                               
138500     .                                                                    
138600     SKIP3                                                                
138700 MFS-RENSA-FAELT-MOD-INFAELT SECTION.                                     
138800                                                                          
138900     MOVE MFS-RENSA-FAELT   TO MOD-FLTILLBAKA                             
139000                               MOD-KDBEHAND-GRUND                         
139100                               MOD-TEGRUND                                
139200                               MOD-KDBEHAND-RAD                           
139300                               MOD-TERAD                                  
139400                               MOD-KDBEHAND-DEL                           
139500                               MOD-TEDEL                                  
139600                               MOD-KDBEHAND-KOL                           
139700                               MOD-TEKOL                                  
139800                               MOD-KDBEHAND-AVVIK                         
139900                               MOD-TEAVVIK                                
140000                               MOD-KDBEHAND-URS                           
140100                               MOD-TEURS                                  
140200                               MOD-FLBORJA-OM                             
140300                               MOD-TEBACKA-PGM                            
140400                               MOD-FLBACKA                                
140500     .                                                                    
140600     SKIP3                                                                
140700* IMS SEKTIONER                                                           
140800     SKIP3                                                                
140900 IMS-GET-MSG SECTION.                                                     
141000     MOVE '  QC' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
141200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     SKIP3                                                                
141500     .                                                                    
141600 IMS-INSERT-MSG SECTION.                                                  
141700     IF NOT ENGLISH-TEXT                                                  
141800       MOVE '0' TO MFS-KDHUVOMR                                           
141900     END-IF                                                               
142000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
142100     MOVE SPACE TO GODK-STATUSKODER                                       
142200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
142300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
142400     PERFORM IMS-STATUSKONTROLL                                           
142500     SKIP3                                                                
142600     .                                                                    
142700     EJECT                                                                
142800 IMS-GET-XXDJ-4305 SECTION.                                               
142900                                                                          
143000     STRING 'WLXXDJ01(WDGXKEY  =' W-WDGXKEY-4305-X ')'                    
143100            DELIMITED BY SIZE INTO SSA1                                   
143200     MOVE '  '   TO GODK-STATUSKODER                                      
143300     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA-1 SSA1                    
143400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     SKIP3                                                                
143700     .                                                                    
143800 IMS-GET-XXDJ-4306 SECTION.                                               
143900                                                                          
144000     STRING 'WLXXDJ11(WDGXKEY  =' W-WDGXKEY-4306-X ')'                    
144100            DELIMITED BY SIZE INTO SSA1                                   
144200     MOVE '  '   TO GODK-STATUSKODER                                      
144300     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA-1 SSA1                  
144400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
144500     PERFORM IMS-STATUSKONTROLL                                           
144600     SKIP3                                                                
144700     .                                                                    
144800 IMS-REPL-XXDJ SECTION.                                                   
144900     MOVE '  '   TO GODK-STATUSKODER                                      
145000     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA-1                       
145100     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
145200     PERFORM IMS-STATUSKONTROLL                                           
145300                                                                          
145400                                                                          
145500                                                                          
145600     .                                                                    
145700 IMS-DLET-XXDJ SECTION.                                                   
145800     MOVE '  '   TO GODK-STATUSKODER                                      
145900     CALL CBLTDLI USING DLET XXDJ-PCB DLI-IO-AREA-1                       
146000     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200                                                                          
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-GET-XXDK-4311 SECTION.                                               
146600                                                                          
146700     STRING 'WLXXDK01(WDGXKEY  =' W-WDGXKEY-4311-X ')'                    
146800            DELIMITED BY SIZE INTO SSA1                                   
146900     MOVE '  '     TO GODK-STATUSKODER                                    
147000     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA-2 SSA1                    
147100     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     SKIP3                                                                
147400     .                                                                    
147500 IMS-GET-XXDK-4312 SECTION.                                               
147600                                                                          
147700     STRING 'WLXXDK11*F(WDGXKEY  =' W-WDGXKEY-4312-X ')'                  
147800            DELIMITED BY SIZE INTO SSA1                                   
147900     MOVE '  GE'     TO GODK-STATUSKODER                                  
148000     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA-2 SSA1                  
148100     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
148200     PERFORM IMS-STATUSKONTROLL                                           
148300     SKIP3                                                                
148400     .                                                                    
148500 IMS-GET-XXDK-4314 SECTION.                                               
148600                                                                          
148700     MOVE 'WLXXDK21 '     TO SSA1                                         
148800     MOVE '  GE'   TO GODK-STATUSKODER                                    
148900     CALL CBLTDLI USING GHNP XXDK-PCB DLI-IO-AREA-2 SSA1                  
149000     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     SKIP3                                                                
149300     .                                                                    
149400 IMS-DLET-XXDK SECTION.                                                   
149500     MOVE '  '   TO GODK-STATUSKODER                                      
149600     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA-2                       
149700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
149800     PERFORM IMS-STATUSKONTROLL                                           
149900                                                                          
150000                                                                          
150100                                                                          
150200     .                                                                    
150300 IMS-REPL-XXDK SECTION.                                                   
150400     MOVE '  '   TO GODK-STATUSKODER                                      
150500     CALL CBLTDLI USING REPL XXDK-PCB DLI-IO-AREA-2                       
150600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
150700     PERFORM IMS-STATUSKONTROLL                                           
150800                                                                          
150900     .                                                                    
151000     EJECT                                                                
151100 IMS-GET-XXDL-4315 SECTION.                                               
151200                                                                          
151300     STRING 'WLXXDL01(WDGXKEY  =' W-WDGXKEY-4315-X ')'                    
151400            DELIMITED BY SIZE INTO SSA1                                   
151500     MOVE '  '     TO GODK-STATUSKODER                                    
151600     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-AREA-1 SSA1                    
151700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     SKIP3                                                                
152000     .                                                                    
152100 IMS-GET-XXDL-4316 SECTION.                                               
152200                                                                          
152300     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
152400            DELIMITED BY SIZE INTO SSA1                                   
152500     MOVE '  GE'     TO GODK-STATUSKODER                                  
152600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
152700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     SKIP3                                                                
153000     .                                                                    
153100 IMS-GET-XXDL-4316-BLANK SECTION.                                         
153200                                                                          
153300     STRING 'WLXXDL11(WDGXKEY  =' W-WDGXKEY-4316-X ')'                    
153400            DELIMITED BY SIZE INTO SSA1                                   
153500     MOVE '  '     TO GODK-STATUSKODER                                    
153600     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
153700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
153800     PERFORM IMS-STATUSKONTROLL                                           
153900     SKIP3                                                                
154000     .                                                                    
154100 IMS-GET-XXDL-4316-INTERV SECTION.                                        
154200                                                                          
154300     STRING 'WLXXDL11(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                    
154400                    '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'                
154500            DELIMITED BY SIZE INTO SSA1                                   
154600     MOVE '  GE'     TO GODK-STATUSKODER                                  
154700     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
154800     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
154900     PERFORM IMS-STATUSKONTROLL                                           
155000                                                                          
155100                                                                          
155200                                                                          
155300     .                                                                    
155400 IMS-GET-XXDL-4316-INTERV-F SECTION.                                      
155500                                                                          
155600     STRING 'WLXXDL11*F(WDGXKEY  >' W-WDGXKEY-4316-X-MIN                  
155700                      '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'              
155800            DELIMITED BY SIZE INTO SSA1                                   
155900     MOVE '  GE'     TO GODK-STATUSKODER                                  
156000     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
156100     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     SKIP3                                                                
156400     .                                                                    
156500 IMS-GET-XXDL-4316-INTERV-F-KOL SECTION.                                  
156600                                                                          
156700     STRING 'WLXXDL11*F(WDGXKEY  >' W-WDGXKEY-4316-X-KOL-MIN              
156800                      '&WDGXKEY  <' W-WDGXKEY-4316-X-MAX ')'              
156900            DELIMITED BY SIZE INTO SSA1                                   
157000     MOVE '  GE'     TO GODK-STATUSKODER                                  
157100     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA-1 SSA1                  
157200     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
157300     PERFORM IMS-STATUSKONTROLL                                           
157400     SKIP3                                                                
157500     .                                                                    
157600 IMS-REPL-XXDL SECTION.                                                   
157700                                                                          
157800     MOVE '  '   TO GODK-STATUSKODER                                      
157900     CALL CBLTDLI USING REPL XXDL-PCB DLI-IO-AREA-1                       
158000     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
158100     PERFORM IMS-STATUSKONTROLL                                           
158200     SKIP3                                                                
158300     .                                                                    
158400 IMS-DLET-XXDL SECTION.                                                   
158500                                                                          
158600     MOVE '  '   TO GODK-STATUSKODER                                      
158700     CALL CBLTDLI USING DLET XXDL-PCB DLI-IO-AREA-1                       
158800     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
158900     PERFORM IMS-STATUSKONTROLL                                           
159000     SKIP3                                                                
159100     .                                                                    
159110                                                                          
159120 IMS-GU-WDB6      SECTION.                                                
159130     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
159140          DELIMITED BY SIZE INTO SSA1                                     
159150     MOVE '  GE' TO GODK-STATUSKODER                                      
159160     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B6 SSA1                   
159170     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
159180     PERFORM IMS-STATUSKONTROLL                                           
159190     .                                                                    
159200     EJECT                                                                
159300 IMS-STATUSKONTROLL SECTION.                                              
159400     SET STATUS-IX TO 1                                                   
159500     SEARCH GODK-STATUS AT END CALL FELLOG                                
159600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
159700     END-SEARCH                                                           
159800     CONTINUE                                                             
159900     .                                                                    
