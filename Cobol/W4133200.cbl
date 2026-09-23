000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4133200.                                                
000400 AUTHOR.         P-A HELGEGREN.                                           
000500 DATE-WRITTEN.   12/02/07.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*    KOPIA AV PGM W40387                                                  
000900*                                                                         
001000*    DISPLAY  WDGX4471-72                                                 
001100*                                                                         
001200*    LOOP (ON KDPRCGRP / IDPRC)                                           
001300*    READ SEQUENTIALLY WDGX4447-48 FOR 4448-IDPRCBAS                      
001400*    READ  WDGX4471-72 WITH KEY = 4448-IDPRCBAS                           
001500*    READ DATA FROM WDQ3 WITH SECONDARY INDEX WDQ2C                       
001600*    HANDLE DATA FOR DISPLAY/LIST                                         
001700*    END-LOOP                                                             
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- RADER    LISTA FÖR MAIL 57518                              
002800     SELECT W41333                     ASSIGN TO W41332D1.                
002900     SKIP2                                                                
003000*          --- RADER    LISTA FÖR MAIL 57512                              
003100     SELECT W41334                     ASSIGN TO W41332D2.                
003200     SKIP2                                                                
003300*          --- RADER    LISTA FÖR MAIL 57515                              
003400     SELECT W41335                     ASSIGN TO W41332D3.                
003500     SKIP2                                                                
003600*          --- RADER    LISTA FÖR MAIL 57523                              
003700     SELECT W41336                     ASSIGN TO W41332D4.                
003800     SKIP2                                                                
003900*          --- RADER    LISTA FÖR MAIL 57524                              
004000     SELECT W41337                     ASSIGN TO W41332D5.                
004100     SKIP2                                                                
004200*          --- RADER    LISTA FÖR MAIL 57525                              
004300     SELECT W41338                     ASSIGN TO W41332D6.                
004400     SKIP2                                                                
004500*          --- RADER    LISTA FÖR MAIL 57520                              
004600     SELECT W41339                     ASSIGN TO W41332D7.                
004700     SKIP2                                                                
004800*          --- RADER    LISTA FÖR MAIL 57522                              
004900     SELECT W41340                     ASSIGN TO W41332D8.                
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200     SKIP3                                                                
005300 FILE SECTION.                                                            
005400     SKIP3                                                                
005500 FD  W41333                                                               
005600     RECORDING       V                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST  -COPY W41333  -PRE  UT1-  -L.                                  
006000     SKIP3                                                                
006100 FD  W41334                                                               
006200     RECORDING       V                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  POST  -COPY W41333  -PRE  UT2-  -L.                                  
006600     SKIP3                                                                
006700 FD  W41335                                                               
006800     RECORDING       V                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST  -COPY W41333  -PRE  UT3-  -L.                                  
007200     SKIP3                                                                
007300 FD  W41336                                                               
007400     RECORDING       V                                                    
007500     BLOCK CONTAINS  0.                                                   
007600                                                                          
007700*01  POST  -COPY W41333  -PRE  UT4-  -L.                                  
007800     SKIP3                                                                
007900 FD  W41337                                                               
008000     RECORDING       V                                                    
008100     BLOCK CONTAINS  0.                                                   
008200                                                                          
008300*01  POST  -COPY W41333  -PRE  UT5-  -L.                                  
008400     SKIP3                                                                
008500 FD  W41338                                                               
008600     RECORDING       V                                                    
008700     BLOCK CONTAINS  0.                                                   
008800                                                                          
008900*01  POST  -COPY W41333  -PRE  UT6-  -L.                                  
009000     SKIP3                                                                
009100 FD  W41339                                                               
009200     RECORDING       V                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500*01  POST  -COPY W41333  -PRE  UT7-  -L.                                  
009600     SKIP3                                                                
009700 FD  W41340                                                               
009800     RECORDING       V                                                    
009900     BLOCK CONTAINS  0.                                                   
010000                                                                          
010100*01  POST  -COPY W41333  -PRE  UT8-  -L.                                  
010200     EJECT                                                                
010300 WORKING-STORAGE SECTION.                                                 
010400*    -COPY WY2000WB                                                       
010500     SKIP3                                                                
010600*    -COPY WY2000W1                                                       
010700     SKIP3                                                                
010800 77  IDPGM                       PIC X(08)   VALUE 'W4133200'.            
010900                                                                          
011000 77  JA                          PIC X       VALUE 'J'.                   
011100 77  NEJ                         PIC X       VALUE 'N'.                   
011200                                                                          
011300*    ---                                                                  
011400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
011500 77  MAX-INDX                    PIC S9(4)  VALUE +50   COMP SYNC.        
011600 77  ANT-INDX                    PIC S9(4)  VALUE ZERO  COMP SYNC.        
011700 77  PGRP-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
011800                                                                          
011900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
012000                                                                          
012100 77  TOT-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
012200                                                                          
012300 01      WS-TIRFS                PIC 9(11).                               
012400 01      FILLER REDEFINES WS-TIRFS.                                       
012500   03    FILLER                  PIC X(1).                                
012600   03    WS-RFS-DATE             PIC 9(6).                                
012700   03    WS-RFS-TIME             PIC X(4).                                
012800                                                                          
012900*                                                                         
013000*          DATE FROM DC-LOCAL                                             
013100*                                                                         
013200 01  FILLER.                                                              
013300     05  WS-LOCAL-DATE         PIC  9(06) VALUE 0.                        
013400*    ---                                                                  
013500     05  WS-ANTAL-RAD            PIC S9(3) COMP-3.                        
013600                                                                          
013700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013800     88  INDATA-OK                           VALUE 'J'.                   
013900     88  INDATA-FEL                          VALUE 'N'.                   
014000                                                                          
014100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014200     88  NYCKLAR-OK                          VALUE 'J'.                   
014300     88  NYCKLAR-FEL                         VALUE 'N'.                   
014400                                                                          
014500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
014600     88  ALLT-OK                             VALUE 'J'.                   
014700                                                                          
014800 77  RAD-INFO-SW                 PIC X       VALUE 'J'.                   
014900     88  RAD-INFO-OK                         VALUE 'J'.                   
015000                                                                          
015100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015200 01  FILLER REDEFINES DAGENS-DATUM.                                       
015300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015600 01  DAGENS-DATUM-AAVV           PIC 9(4)    VALUE ZERO.                  
015700 01  FILLER REDEFINES DAGENS-DATUM-AAVV.                                  
015800     03  DAGENS-DATUM-AA         PIC 9(2).                                
015900     03  DAGENS-DATUM-VV         PIC 9(2).                                
016000                                                                          
016100 01  W-KDPRCGRP-X.                                                        
016200     03 FILLER                   PIC X(5) VALUE '57518'.                  
016300     03 FILLER                   PIC X(5) VALUE '57512'.                  
016400     03 FILLER                   PIC X(5) VALUE '57515'.                  
016500     03 FILLER                   PIC X(5) VALUE '57523'.                  
016600     03 FILLER                   PIC X(5) VALUE '57524'.                  
016700     03 FILLER                   PIC X(5) VALUE '57525'.                  
016800     03 FILLER                   PIC X(5) VALUE '57520'.                  
016900     03 FILLER                   PIC X(5) VALUE '57522'.                  
017000 01  FILLER        REDEFINES W-KDPRCGRP-X.                                
017100     03  FILLER   OCCURS 8.                                               
017200         05  W-KDPRCGRP          PIC X(5).                                
017300 01  MAX-IX                      PIC S9(4)  COMP VALUE +8.                
017400*                                                                         
017500 01  WS-DCUSER.                                                           
017600     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
017700     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
017800     03 FILLER                   PIC X(1)   VALUE SPACE.                  
017900*      --- VALID IDDC CODES                                               
018000*                                                                         
018100*01    -COPY WWDC99                                                       
018200       EJECT                                                              
018300 01  MID-INPUT.                                                           
018400     05  WS-PFI-KDPRCGRP             PIC X(5) VALUE SPACE.                
018500     05  WS-PFI-KDPRODKL-01          PIC X(1) VALUE SPACE.                
018600         88 GOOD-PFI-KDPRODKL-01       VALUE ' ' 'B' 'C'.                 
018700     05  WS-PFI-KDPRODKL-02          PIC X(1) VALUE SPACE.                
018800         88 GOOD-PFI-KDPRODKL-02       VALUE ' ' 'B' 'C'.                 
018900     05  WS-PFI-IDPRC.                                                    
019000         10  WS-PFI-IDPRCBAS         PIC X(3) VALUE SPACES.               
019100         10  WS-PFI-IDPRCVAR         PIC X(1) VALUE SPACE.                
019200     05  WS-PFI-TIRFS-01             PIC 9(6) VALUE 0.                    
019300     05  WS-PFI-TIRFS-02             PIC 9(6) VALUE 0.                    
019400     05  WS-PFI-KDKALK               PIC 9(1) VALUE 0.                    
019500         88 GOOD-PFI-KDKALK          VALUE 1 2.                           
019600*                                                                         
019700*                 DATA FOR REFERENCES                                     
019800*                                                                         
019900 01  REF-DATA.                                                            
020000     05  WS-KVRADER-REG-RAD      PIC S9(7)      COMP-3 VALUE +0.          
020100     05  WS-KVARBTID             PIC S9(4)V9(1) COMP-3 VALUE +0.          
020200     05  WS-KVBEMAN              PIC S9(4)V9(1) COMP-3 VALUE +0.          
020300     05  WS-TIRFS01              PIC S9(11)     COMP-3 VALUE +0.          
020400     05  WS-TIRFS02              PIC S9(11)     COMP-3 VALUE +0.          
020500                                                                          
020600     05  WS-IDPRCVAR             PIC X(1) VALUE SPACES.                   
020700     05  WS-FIRST-IDPRC          PIC X(4) VALUE SPACES.                   
020800*                                                                         
020900 01  FILLER.                                                              
021000     05  WS-DB-TIRFS            PIC 9(11) VALUE 0.                        
021100     05  FILLER REDEFINES WS-DB-TIRFS.                                    
021200         10  WS-DB-TIRFS-AAMMDD PIC 9(7).                                 
021300         10  WS-DB-TIRFS-HHMM   PIC 9(4).                                 
021400*                                                                         
021500 01  TOTAL-TABLE.                                                         
021600     05  WS-KVRADER-TOT OCCURS 11 PIC S9(7) COMP-3 VALUE +0.              
021700     EJECT                                                                
021800*                                                                         
021900 01  XX00-4472.                                                           
022000         03  XX00-4472-KVRADER-DAG      PIC S9(7) COMP-3.                 
022100         03  XX00-4472-KVRADER-RFS      PIC S9(7) COMP-3.                 
022200         03  XX00-4472-KVRADER-PRAPP    PIC S9(7) COMP-3.                 
022300         03  FILLER OCCURS 3.                                             
022400             07  XX00-4472-KVRADER-SHFT-ALL PIC S9(7) COMP-3.             
022500         03  FILLER OCCURS 30.                                            
022600             07  XX00-4472-TIRFS        PIC S9(11) COMP-3.                
022700             07  XX00-4472-KVRADER      PIC S9(7)  COMP-3.                
022800             07  FILLER OCCURS 3.                                         
022900                 11  XX00-4472-KVRADER-SHFT PIC S9(7) COMP-3.             
023000*                                                                         
023100 01  KVRADER-SHFT-TIRFS-00.                                               
023200     05  FILLER                  PIC S9(7) COMP-3 VALUE +0.               
023300     05  FILLER.                                                          
023400         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
023500         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
023600         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
023700*                                                                         
023800 01  KVRADER-SHFT-TIRFS.                                                  
023900     05  WU-KVRADER              PIC S9(7) COMP-3.                        
024000     05  FILLER OCCURS 3.                                                 
024100         10  WU-KVRADER-SHFT     PIC S9(7) COMP-3.                        
024200*                                                                         
024300 01  FILLER.                                                              
024400     05  XX00-ORQC-KVRADER-U         PIC S9(7) COMP-3 VALUE +0.           
024500     05  XX00-ORQC-KVRADER-TIRFS-U   PIC S9(7) COMP-3 VALUE +0.           
024600*                                                                         
024700 01  FILLER.                                                              
024800     05  XX00-ORQC-KVRADER-R         PIC S9(7) COMP-3 VALUE +0.           
024900     05  XX00-ORQC-KVRADER-TIRFS-R   PIC S9(7) COMP-3 VALUE +0.           
025000*                                                                         
025100 01  FILLER.                                                              
025200     05  WS-KVRADER-CL4  PIC S9(7)   COMP-3 VALUE +0.                     
025300     05  WS-KVRADER-CL7  PIC S9(7)   COMP-3 VALUE +0.                     
025400     05  WS-KVRADER-CL8  PIC S9(7)   COMP-3 VALUE +0.                     
025500*                                                                         
025600     EJECT                                                                
025700 01  FILLER-INDX.                                                         
025800     05  RS-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
025900     05  RT-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
026000     05  RW-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
026100     05  RS-INDX-MAX      PIC S9(3)  COMP-3 VALUE +30.                    
026200*                                                                         
026300     EJECT                                                                
026400 01  W-UT-RAD.                                                            
026500     05 W-IDPRC-RAD.                                                      
026600        07 W-IDPRCBAS         PIC X(3)         VALUE ZERO.                
026700        07 W-IDPRCVAR         PIC X            VALUE ZERO.                
026800     05 W-KVRADER-RAD         PIC Z(4)9        VALUE ZERO.                
026900     05 W-KVRADER-DAG-RAD     PIC Z(4)9        VALUE ZERO.                
027000     05 W-KVRADER-RST-RAD     PIC Z(4)9        VALUE ZERO.                
027100     05 W-KVRADER-REG-RAD     PIC Z(4)9        VALUE ZERO.                
027200     05 W-KVRADER-UT-RAD      PIC Z(4)9        VALUE ZERO.                
027300     05 W-KVRADER-KAN-RAD     PIC Z(4)9        VALUE ZERO.                
027400     05 W-KVRADER-PLMI-RAD    PIC -Z(3)9.9     VALUE ZERO.                
027500     05 W-KVRADER-META-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
027600     05 W-KVRADER-METB-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
027700     05 W-KVRADER-METC-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
027800     EJECT                                                                
027900 01  RUBRIKER.                                                            
028000     03  RUBRIK1.                                                         
028100         05  FILLER           PIC X(7)                                    
028200             VALUE 'W41332-'.                                             
028300         05  RUB1-LISTNR      PIC 9(3).                                   
028400         05  FILLER           PIC X(62)                                   
028500             VALUE '   PLANERING BULKORDER-RADER'.                        
028600     03  RUBRIK2.                                                         
028700         05  FILLER            PIC X(9)  VALUE 'PROD-GRP '.               
028800         05  RUB2-KDPRCGRP     PIC X(5).                                  
028900         05  FILLER            PIC X(9)  VALUE ' P-KLASS '.               
029000         05  RUB2-KDPRODKL-01  PIC X     VALUE 'B'.                       
029100         05  FILLER            PIC X(3)  VALUE ' - '.                     
029200         05  RUB2-KDPRODKL-02  PIC X     VALUE 'C'.                       
029300         05  FILLER            PIC X(5)  VALUE ' PRC '.                   
029400         05  RUB2-IDPRC        PIC X(4)  VALUE SPACE.                     
029500         05  FILLER            PIC X(5)  VALUE ' RFS '.                   
029600         05  RUB2-TIRFS-01     PIC 9(6).                                  
029700         05  FILLER            PIC X(3)  VALUE ' - '.                     
029800         05  RUB2-TIRFS-02     PIC 9(6).                                  
029900         05  FILLER            PIC X(5)  VALUE ' BER '.                   
030000         05  RUB2-KDKALK       PIC X     VALUE '2'.                       
030100         05  FILLER            PIC X(4)  VALUE ' DC '.                    
030200         05  RUB2-IDDC         PIC X(2)  VALUE '11'.                      
030300     03  TOMRAD                PIC X(72) VALUE SPACE.                     
030400     03  RUBRIK4.                                                         
030500         05  FILLER            PIC X(25)                                  
030600             VALUE '  O R D E R K Ö       '.                              
030700         05  FILLER            PIC X(40)                                  
030800             VALUE ' KAPACITET DAGENS PRODUKTION '.                       
030900     03  RUBRIK5.                                                         
031000         05  FILLER            PIC X(25)                                  
031100             VALUE 'PRC  TOTAL  IDAG ID-KV  '.                            
031200         05  FILLER            PIC X(47)                                  
031300         VALUE 'REG UTSKR   KAN     + -      A      B      C '.           
031400 01  FILLER.                                                              
031500   02  RAD   OCCURS 50.                                                   
031600     03  RAD-IDPRC             PIC X(4).                                  
031700     03  FILLER                PIC X      VALUE SPACE.                    
031800     03  RAD-KVRADER           PIC Z(4)9.                                 
031900     03  FILLER                PIC X      VALUE SPACE.                    
032000     03  RAD-KVRADER-DAG       PIC Z(4)9.                                 
032100     03  FILLER                PIC X      VALUE SPACE.                    
032200     03  RAD-KVRADER-RST       PIC Z(4)9.                                 
032300     03  FILLER                PIC X      VALUE SPACE.                    
032400     03  RAD-KVRADER-REG       PIC Z(4)9.                                 
032500     03  FILLER                PIC X      VALUE SPACE.                    
032600     03  RAD-KVRADER-UT        PIC Z(4)9.                                 
032700     03  FILLER                PIC X      VALUE SPACE.                    
032800     03  RAD-KVRADER-KAN       PIC Z(4)9.                                 
032900     03  FILLER                PIC X      VALUE SPACE.                    
033000     03  RAD-KVRADER-PLMI      PIC -(4)9.9.                               
033100     03  FILLER                PIC X      VALUE SPACE.                    
033200     03  RAD-KVRADER-META      PIC Z(6).                                  
033300     03  FILLER                PIC X      VALUE SPACE.                    
033400     03  RAD-KVRADER-METB      PIC Z(6).                                  
033500     03  FILLER                PIC X      VALUE SPACE.                    
033600     03  RAD-KVRADER-METC      PIC Z(6).                                  
033700 01  TOTRAD.                                                              
033800     03  FILLER                PIC X(5)   VALUE 'TOT: '.                  
033900     03  TOT-KVRADER           PIC Z(4)9.                                 
034000     03  FILLER                PIC X      VALUE SPACE.                    
034100     03  TOT-KVRADER-DAG       PIC Z(4)9.                                 
034200     03  FILLER                PIC X      VALUE SPACE.                    
034300     03  TOT-KVRADER-RST       PIC Z(4)9.                                 
034400     03  FILLER                PIC X      VALUE SPACE.                    
034500     03  TOT-KVRADER-REG       PIC Z(4)9.                                 
034600     03  FILLER                PIC X      VALUE SPACE.                    
034700     03  TOT-KVRADER-UT        PIC Z(4)9.                                 
034800     03  FILLER                PIC X      VALUE SPACE.                    
034900     03  TOT-KVRADER-KAN       PIC Z(4)9.                                 
035000     03  FILLER                PIC X      VALUE SPACE.                    
035100     03  TOT-KVRADER-PLMI      PIC -(4)9.9.                               
035200     03  FILLER                PIC X      VALUE SPACE.                    
035300     03  TOT-KVRADER-META      PIC Z(6).                                  
035400     03  FILLER                PIC X      VALUE SPACE.                    
035500     03  TOT-KVRADER-METB      PIC Z(6).                                  
035600     03  FILLER                PIC X      VALUE SPACE.                    
035700     03  TOT-KVRADER-METC      PIC Z(6).                                  
035800     EJECT                                                                
035900 01  GENERELLA-SUBPROGRAM.                                                
036000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
036100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
036200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
036300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
036400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
036500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
036600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
036700     03  DATKONV                 PIC X(8)    VALUE 'DATKONV '.            
036800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
036900     EJECT                                                                
037000*--------------------------------------- PARAMETRAR TILL DATKORT          
037100 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
037200 01  PROGRAM-NAMN            PIC X(8)    VALUE 'W41332'.                  
037300                                                                          
037400*01  -COPY WDATKORT                                                       
037500     SKIP2                                                                
037600*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
037700                                                                          
037800*01  -COPY WDATAREA                                                       
037900     SKIP3                                                                
038000*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
038100*   -COPY WORKAREA                                                        
038200     EJECT                                                                
038300*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
038400*   -COPY WDECAREA                                                        
038500     EJECT                                                                
038600*    --- PARAMETRAR TILL POSTSUM                                          
038700*                                                                         
038800*01  -COPY W0005   -PRE  POSTSUM-                                         
038900     EJECT                                                                
039000*                                                                         
039100*                                                                         
039200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039300     SKIP3                                                                
039400*                                                                         
039500 01  NYCKLAR-TILL-DLI.                                                    
039600*                                                                         
039700     03  W-4447-WDGXKEY.                                                  
039800         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
039900         07 W-4447-IDDC          PIC X(02).                               
040000         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
040100*                                                                         
040200     03  W-4448-WDGXKEY.                                                  
040300         07 W-4448-IDPRC.                                                 
040400            11 W-4448-IDPRCBAS   PIC X(3).                                
040500            11 W-4448-IDPRCVAR   PIC X(1).                                
040600         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
040700*                                                                         
040800*                                                                         
040900     03  W-4471-WDGXKEY.                                                  
041000         07 FILLER               PIC X(4)    VALUE '4471'.                
041100         07 W-4471-IDDC          PIC X(02).                               
041200         07 W-4471-IDPRC.                                                 
041300            11 W-4471-IDPRCBAS   PIC X(3).                                
041400            11 W-4471-IDPRCVAR   PIC X(1).                                
041500         07 FILLER               PIC X(20)   VALUE LOW-VALUE.             
041600*                                                                         
041700     03  W-4472-KDSEGKEY.                                                 
041800         07 FILLER               PIC X(1)    VALUE '1'.                   
041900*                                                                         
042000     03  W-WDQ3C1KY-MIN.                                                  
042100         07 W-WDQ3C1KY-MIN-IDDC      PIC X(02).                           
042200         07 W-WDQ3C1KY-MIN-IDPRCBAS  PIC X(3).                            
042300         07 W-WDQ3C1KY-MIN-IDPRCVAR  PIC X(1).                            
042400         07 W-WDQ3C1KY-MIN-FILLER    PIC X(34) VALUE LOW-VALUE.           
042500*                                                                         
042600     03  W-WDQ3C1KY-MAX.                                                  
042700         07 W-WDQ3C1KY-MAX-IDDC      PIC X(02).                           
042800         07 W-WDQ3C1KY-MAX-IDPRCBAS  PIC X(3).                            
042900         07 W-WDQ3C1KY-MAX-IDPRCVAR  PIC X(1).                            
043000         07 W-WDQ3C1KY-MAX-FILLER    PIC X(34) VALUE HIGH-VALUE.          
043100*                                                                         
043200*    03  W-WDQ3C1KY-KDODELST-U       PIC X VALUE 'U'.                     
043300*                                                                         
043400 01  STATUS-WS                   PIC XX.                                  
043500     88  SEGMENT-OK                          VALUE '  '.                  
043600     88  SEGMENT-II                          VALUE 'II'.                  
043700     88  SEGMENT-GE                          VALUE 'GE'.                  
043800     88  SEGMENT-GB                          VALUE 'GB'.                  
043900     SKIP2                                                                
044000 01  4448-STATUS-WS              PIC XX.                                  
044100     88  4448-SEGMENT-OK                     VALUE '  '.                  
044200     88  4448-SEGMENT-II                     VALUE 'II'.                  
044300     88  4448-SEGMENT-GE                     VALUE 'GE'.                  
044400     88  4448-SEGMENT-GB                     VALUE 'GB'.                  
044500     SKIP2                                                                
044600 01  GODK-STATUSKODER.                                                    
044700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044800     SKIP3                                                                
044900 01  SSA1                        PIC X(128).                              
045000 01  SSA2                        PIC X(128).                              
045100     EJECT                                                                
045200*    --- IMS FUNKTIONSKODER                                               
045300*01  -COPY W0003                                                          
045400     EJECT                                                                
045500*    ---  DLI INPUT-OUTPUT AREA                                           
045600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
045700     SKIP3                                                                
045800 01  DLI-IO-AREA1.                                                        
045900     03  IO-AREA1                PIC X(1200)  VALUE SPACE.                
046000     SKIP3                                                                
046100     03  WLXXKH01 REDEFINES IO-AREA1.                                     
046200*        05  -COPY WDGX4447   -PRE XXKH-                                  
046300*                                                                         
046400     03  WLXXKH11 REDEFINES IO-AREA1.                                     
046500*        05  -COPY WDGX4448   -PRE XXKH-                                  
046600     EJECT                                                                
046700 01  DLI-IO-AREA2.                                                        
046800     03  IO-AREA2                PIC X(1200)  VALUE SPACE.                
046900     SKIP3                                                                
047000     03  WLXXKW01 REDEFINES IO-AREA2.                                     
047100*        05  -COPY WDGX4471   -PRE XXKW-                                  
047200*                                                                         
047300     03  WLXXKW11 REDEFINES IO-AREA2.                                     
047400*        05  -COPY WDGX4472   -PRE XXKW-                                  
047500*                                                                         
047600     03  WLORQD01 REDEFINES IO-AREA2.                                     
047700*        05  -COPY WDQ3C1     -PRE ORQD-                                  
047800*                                                                         
047900     EJECT                                                                
048000 LINKAGE SECTION.                                                         
048100                                                                          
048200*01  -COPY W0008      -PRE XXKH-                                          
048300     05  FILLER                  PIC X.                                   
048400     EJECT                                                                
048500*01  -COPY W0008      -PRE XXKW-                                          
048600     05  FILLER                  PIC X.                                   
048700     EJECT                                                                
048800*01  -COPY W0008      -PRE ORQD-                                          
048900     05  FILLER                  PIC X.                                   
049000     EJECT                                                                
049100     EJECT                                                                
049200 PROCEDURE DIVISION  USING XXKH-PCB XXKW-PCB                              
049300                                    ORQD-PCB.                             
049400 W41332 SECTION.                                                          
049500     ENTRY 'DLITCBL' USING XXKH-PCB XXKW-PCB                              
049600                                    ORQD-PCB.                             
049700                                                                          
049800     PERFORM A-INIT                                                       
049900                                                                          
050000     PERFORM F-LAES-VISA-INFO                                             
050100                                                                          
050200     CLOSE W41333                                                         
050300           W41334                                                         
050400           W41335                                                         
050500           W41336                                                         
050600           W41337                                                         
050700           W41338                                                         
050800           W41339                                                         
050900           W41340                                                         
051000     MOVE ZERO TO RETURN-CODE                                             
051100     GOBACK                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 A-INIT SECTION.                                                          
051500                                                                          
051600                                                                          
051700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
051800                                                                          
051900     MOVE D-AAR           TO DAGENS-DATUM-AAR                             
052000     MOVE D-MAANAD        TO DAGENS-DATUM-MAANAD                          
052100     MOVE D-DAG           TO DAGENS-DATUM-DAG                             
052200                                                                          
052300     MOVE DAGENS-DATUM    TO WS-PFI-TIRFS-01                              
052400                             WS-PFI-TIRFS-02                              
052500                                                                          
052600                                                                          
052700     MOVE 11             TO WS-IDDC                                       
052800     MOVE WS-IDDC        TO WS-DCUSER-IDDC                                
052900                                                                          
053000     MOVE 2              TO WS-PFI-KDKALK                                 
053100     MOVE 'B'            TO WS-PFI-KDPRODKL-01                            
053200     MOVE 'C'            TO WS-PFI-KDPRODKL-02                            
053300                                                                          
053400       MOVE WS-IDDC            TO RUB2-IDDC                               
053500                                                                          
053600       MOVE WS-PFI-KDPRCGRP    TO RUB2-KDPRCGRP                           
053700       MOVE WS-PFI-KDPRODKL-01 TO RUB2-KDPRODKL-01                        
053800       MOVE WS-PFI-KDPRODKL-02 TO RUB2-KDPRODKL-02                        
053900       MOVE WS-PFI-IDPRC       TO RUB2-IDPRC                              
054000       MOVE WS-PFI-TIRFS-01    TO RUB2-TIRFS-01                           
054100       MOVE WS-PFI-TIRFS-02    TO RUB2-TIRFS-02                           
054200       MOVE WS-PFI-KDKALK      TO RUB2-KDKALK                             
054300                                                                          
054400     OPEN OUTPUT W41333                                                   
054500                 W41334                                                   
054600                 W41335                                                   
054700                 W41336                                                   
054800                 W41337                                                   
054900                 W41338                                                   
055000                 W41339                                                   
055100                 W41340                                                   
055200     .                                                                    
055300     EJECT                                                                
055400 F-LAES-VISA-INFO SECTION.                                                
055500                                                                          
055600     MOVE +1  TO PGRP-IX                                                  
055700     PERFORM UNTIL PGRP-IX > MAX-IX                                       
055800        MOVE W-KDPRCGRP (PGRP-IX) TO RUB2-KDPRCGRP                        
055900                                     WS-PFI-KDPRCGRP                      
056000        MOVE PGRP-IX TO RUB1-LISTNR                                       
056100        MOVE ZERO    TO WS-ANTAL-RAD                                      
056200                                                                          
056300        PERFORM FA-INIT-WSDATA                                            
056400        PERFORM FB-LOC-WDGX4448                                           
056500        MOVE STATUS-WS            TO 4448-STATUS-WS                       
056600        IF SEGMENT-GE                                                     
056700           PERFORM RAD-RENSA-FAELT-UT                                     
056800           PERFORM FZ-TOTAL-LINE                                          
056900           MOVE +1 TO ANT-INDX                                            
057000        ELSE                                                              
057100          MOVE +1                 TO INDX                                 
057200          PERFORM UNTIL (INDX > MAX-INDX) OR                              
057300                        (NOT 4448-SEGMENT-OK)                             
057400            PERFORM FY-CMPT-SCRNLINE                                      
057500          END-PERFORM                                                     
057600          PERFORM UNTIL (NOT 4448-SEGMENT-OK)                             
057700            PERFORM FY-CMPT-SCRNLINE                                      
057800          END-PERFORM                                                     
057900          MOVE INDX TO ANT-INDX                                           
058000          SUBTRACT 1 FROM ANT-INDX                                        
058100          PERFORM FZ-TOTAL-LINE                                           
058200          MOVE +1 TO TOT-IX                                               
058300          PERFORM UNTIL TOT-IX > 11                                       
058400             MOVE ZERO TO WS-KVRADER-TOT (TOT-IX)                         
058500             ADD +1 TO TOT-IX                                             
058600          END-PERFORM                                                     
058700        END-IF                                                            
058800        PERFORM UNTIL (INDX > MAX-INDX)                                   
058900******    PERFORM RAD-ERASE-LINE                                          
059000          MOVE SPACE    TO RAD(INDX)                                      
059100          ADD +1        TO INDX                                           
059200        END-PERFORM                                                       
059300                                                                          
059400        MOVE +1         TO INDX                                           
059500        PERFORM UNTIL (INDX > ANT-INDX)                                   
059600          PERFORM S10-SKRIV-UTFIL                                         
059700          ADD +1        TO INDX                                           
059800        END-PERFORM                                                       
059900                                                                          
060000        MOVE TOMRAD TO RAD(INDX)                                          
060100        PERFORM S10-SKRIV-UTFIL                                           
060200        ADD +1 TO INDX                                                    
060300        MOVE TOTRAD TO RAD(INDX)                                          
060400        PERFORM S10-SKRIV-UTFIL                                           
060500        ADD +1 TO INDX                                                    
060600                                                                          
060700        ADD +1          TO PGRP-IX                                        
060800     END-PERFORM                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 FA-INIT-WSDATA SECTION.                                                  
061200                                                                          
061300     MOVE WS-IDDC         TO W-4447-IDDC                                  
061400                             W-4471-IDDC                                  
061500                             W-WDQ3C1KY-MIN-IDDC                          
061600                             W-WDQ3C1KY-MAX-IDDC                          
061700     MOVE WS-PFI-TIRFS-01 TO WS-DB-TIRFS-AAMMDD                           
061800     MOVE 0000            TO WS-DB-TIRFS-HHMM                             
061900     MOVE WS-DB-TIRFS     TO WS-TIRFS01                                   
062000     MOVE WS-PFI-TIRFS-02 TO WS-DB-TIRFS-AAMMDD                           
062100     MOVE 2359            TO WS-DB-TIRFS-HHMM                             
062200     MOVE WS-DB-TIRFS     TO WS-TIRFS02                                   
062300     .                                                                    
062400     EJECT                                                                
062500 FB-LOC-WDGX4448 SECTION.                                                 
062600     IF WS-PFI-IDPRC NOT = SPACE                                          
062700        PERFORM IMS-GU-XXKH-WDGX4448                                      
062800        IF SEGMENT-OK                                                     
062900           IF XXKH-4448-KDPRODKL = 'B' OR 'C'                             
063000              CONTINUE                                                    
063100           ELSE                                                           
063200              MOVE 'GE' TO STATUS-WS                                      
063300           END-IF                                                         
063400        END-IF                                                            
063500     ELSE                                                                 
063600        PERFORM IMS-GU-XXKH-WDGX4447                                      
063700        IF SEGMENT-OK                                                     
063800***        IF MFS-FIRST                                                   
063900              PERFORM IMS-GNP-XXKH-WDGX4448                               
064000***        ELSE                                                           
064100******        PERFORM IMS-LST-XXKH-WDGX4448                               
064200***        END-IF                                                         
064300           PERFORM S00-GET-WDGX4448                                       
064400        END-IF                                                            
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 FY-CMPT-SCRNLINE SECTION.                                                
064900                                                                          
065000     MOVE +0                    TO WS-KVARBTID                            
065100                                   WS-KVBEMAN                             
065200     MOVE XXKH-4448-IDPRC       TO WS-FIRST-IDPRC                         
065300     MOVE NEJ                   TO RAD-INFO-SW                            
065400     MOVE KVRADER-SHFT-TIRFS-00 TO KVRADER-SHFT-TIRFS                     
065500     MOVE +0                    TO XX00-ORQC-KVRADER-U                    
065600                                   XX00-ORQC-KVRADER-TIRFS-U              
065700                                   XX00-ORQC-KVRADER-R                    
065800                                   XX00-ORQC-KVRADER-TIRFS-R              
065900                                                                          
066000     PERFORM FYA-INIT-XX00-4472                                           
066100                                                                          
066200     PERFORM FYB-SUM-XXKW-WDGX4472                                        
066300     IF WS-PFI-IDPRC           NOT = SPACES                               
066400         MOVE 'GE'             TO  STATUS-WS                              
066500      ELSE                                                                
066600         PERFORM IMS-GNP-XXKH-WDGX4448                                    
066700         PERFORM S00-GET-WDGX4448                                         
066800     END-IF                                                               
066900     MOVE STATUS-WS            TO 4448-STATUS-WS                          
067000                                                                          
067100     IF RAD-INFO-OK                                                       
067200         PERFORM FYC-KVRADER-RFS                                          
067300         PERFORM FYD-SUM-ORQC-WDQ3C1                                      
067400         PERFORM FYE-DSPL-SCRN-LINE                                       
067500         IF INDX > MAX-INDX                                               
067600             CONTINUE                                                     
067700          ELSE                                                            
067800             PERFORM FYF-MOVE-W-RAD                                       
067900             ADD +1            TO INDX                                    
068000             PERFORM FYG-INIT-W-RAD                                       
068100         END-IF                                                           
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 FYA-INIT-XX00-4472 SECTION.                                              
068600                                                                          
068700     MOVE +0   TO  XX00-4472-KVRADER-DAG                                  
068800     MOVE +0   TO  XX00-4472-KVRADER-RFS                                  
068900     MOVE +0   TO  XX00-4472-KVRADER-PRAPP                                
069000     MOVE +1 TO RS-INDX                                                   
069100     PERFORM UNTIL RS-INDX > 3                                            
069200       MOVE +0 TO XX00-4472-KVRADER-SHFT-ALL (RS-INDX)                    
069300       ADD +1 TO RS-INDX                                                  
069400     END-PERFORM                                                          
069500     MOVE +1 TO RT-INDX                                                   
069600     PERFORM UNTIL RT-INDX > 30                                           
069700       MOVE +0  TO  XX00-4472-TIRFS (RT-INDX)                             
069800       MOVE +0  TO  XX00-4472-KVRADER (RT-INDX)                           
069900       MOVE +1 TO RS-INDX                                                 
070000       PERFORM UNTIL RS-INDX > 3                                          
070100         MOVE +0 TO XX00-4472-KVRADER-SHFT (RT-INDX, RS-INDX)             
070200         ADD +1 TO RS-INDX                                                
070300       END-PERFORM                                                        
070400       ADD +1 TO RT-INDX                                                  
070500     END-PERFORM                                                          
070600     MOVE +0 TO RT-INDX                                                   
070700     MOVE +0 TO RS-INDX                                                   
070800     .                                                                    
070900     EJECT                                                                
071000 FYB-SUM-XXKW-WDGX4472 SECTION.                                           
071100                                                                          
071200     MOVE XXKH-4448-IDPRC TO W-4471-IDPRC                                 
071300     MOVE XXKH-4448-IDPRC TO W-4448-IDPRC                                 
071400     IF WS-PFI-IDPRC = SPACES                                             
071500         PERFORM IMS-GU-XXKW-WDGX4472                                     
071600         IF SEGMENT-OK                                                    
071700            PERFORM FYBB-TOTAL-WDGX4472                                   
071800            PERFORM FYBA-DATA-WDGX4448                                    
071900         END-IF                                                           
072000     ELSE                                                                 
072100        PERFORM IMS-GU-XXKW-WDGX4472                                      
072200        IF SEGMENT-OK                                                     
072300           PERFORM FYBB-TOTAL-WDGX4472                                    
072400        END-IF                                                            
072500        PERFORM FYBA-DATA-WDGX4448                                        
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 FYBA-DATA-WDGX4448 SECTION.                                              
073000                                                                          
073100     MOVE XXKH-4448-KVBEMAN-ORD TO WS-KVBEMAN                             
073200     ADD  XXKH-4448-KVBEMAN-EXT TO WS-KVBEMAN                             
073300     COMPUTE WS-KVARBTID =                                                
073400          WS-KVARBTID + (XXKH-4448-KVARBTID * WS-KVBEMAN)                 
073500     END-COMPUTE                                                          
073600     .                                                                    
073700     EJECT                                                                
073800 FYBB-TOTAL-WDGX4472 SECTION.                                             
073900                                                                          
074000     MOVE JA                    TO RAD-INFO-SW                            
074100     ADD  XXKW-4472-KVRADER-DAG TO XX00-4472-KVRADER-DAG                  
074200     MOVE +1 TO RS-INDX                                                   
074300     PERFORM UNTIL (RS-INDX > RS-INDX-MAX)                                
074400        IF XXKW-4472-TIRFS  (RS-INDX) > +0                                
074500           ADD  XXKW-4472-KVRADER (RS-INDX) TO                            
074600                         XX00-4472-KVRADER-RFS                            
074700           PERFORM FYBBA-LOC-INDX                                         
074800           ADD  XXKW-4472-KVRADER (RS-INDX) TO                            
074900                         XX00-4472-KVRADER (RW-INDX)                      
075000           MOVE XXKW-4472-TIRFS (RS-INDX) TO                              
075100                         XX00-4472-TIRFS (RW-INDX)                        
075200           PERFORM FYBBB-SUM-SHFT                                         
075300        END-IF                                                            
075400        ADD +1 TO RS-INDX                                                 
075500     END-PERFORM                                                          
075600     .                                                                    
075700     EJECT                                                                
075800 FYBBA-LOC-INDX SECTION.                                                  
075900*                                                                         
076000     MOVE +1 TO RW-INDX                                                   
076100     PERFORM UNTIL                                                        
076200               XXKW-4472-TIRFS (RS-INDX) =                                
076300               XX00-4472-TIRFS (RW-INDX)         OR                       
076400               XX00-4472-TIRFS (RW-INDX) = ZERO  OR                       
076500               RW-INDX                   > 29                             
076600         ADD +1 TO RW-INDX                                                
076700     END-PERFORM                                                          
076800     .                                                                    
076900     EJECT                                                                
077000 FYBBB-SUM-SHFT SECTION.                                                  
077100                                                                          
077200     MOVE +1 TO RT-INDX                                                   
077300     PERFORM UNTIL RT-INDX > 3                                            
077400         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
077500                TO  XX00-4472-KVRADER-SHFT (RW-INDX, RT-INDX)             
077600         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
077700                TO  XX00-4472-KVRADER-SHFT-ALL (RT-INDX)                  
077800         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
077900                TO  XX00-4472-KVRADER-PRAPP                               
078000         ADD +1 TO RT-INDX                                                
078100     END-PERFORM                                                          
078200     .                                                                    
078300     EJECT                                                                
078400 FYC-KVRADER-RFS SECTION.                                                 
078500                                                                          
078600     MOVE +1 TO RS-INDX                                                   
078700     PERFORM UNTIL RS-INDX > RS-INDX-MAX                                  
078800       MOVE XX00-4472-TIRFS (RS-INDX)  TO TMP1-YYMMDDHHMM                 
078900       MOVE WS-TIRFS01                 TO TMP2-YYMMDDHHMM                 
079000       MOVE WS-TIRFS02                 TO TMP3-YYMMDDHHMM                 
079100       PERFORM WY2000QB                                                   
079200       IF  TMP1-YYMMDDHHMM  >=    TMP2-YYMMDDHHMM                         
079300       AND TMP1-YYMMDDHHMM  <=    TMP3-YYMMDDHHMM                         
079400          MOVE +1 TO RT-INDX                                              
079500          PERFORM UNTIL RT-INDX > 3                                       
079600             ADD XX00-4472-KVRADER-SHFT (RS-INDX, RT-INDX) TO             
079700                           WU-KVRADER-SHFT (RT-INDX)                      
079800             ADD +1 TO RT-INDX                                            
079900          END-PERFORM                                                     
080000       END-IF                                                             
080100       ADD  +1 TO RS-INDX                                                 
080200     END-PERFORM                                                          
080300     .                                                                    
080400     EJECT                                                                
080500 FYD-SUM-ORQC-WDQ3C1 SECTION.                                             
080600                                                                          
080700     MOVE WS-FIRST-IDPRC TO W-WDQ3C1KY-MIN-IDPRCBAS                       
080800                            W-WDQ3C1KY-MAX-IDPRCBAS                       
080900     MOVE WS-FIRST-IDPRC (4:1) TO W-WDQ3C1KY-MIN-IDPRCVAR                 
081000                                  W-WDQ3C1KY-MAX-IDPRCVAR                 
081100     PERFORM IMS-GU-ORQD-WDQ3C1                                           
081200     PERFORM UNTIL (NOT SEGMENT-OK)                                       
081300       IF ORQD-SEQC-KDODELSTA = 'U'                                       
081400          ADD  ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-U                   
081500       ELSE                                                               
081600          ADD  ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-R                   
081700       END-IF                                                             
081800        MOVE ORQD-SEQC-DARFS (3:10)   TO TMP1-YYMMDDHHMM                  
081900        MOVE WS-TIRFS01        TO TMP2-YYMMDDHHMM                         
082000        MOVE WS-TIRFS02        TO TMP3-YYMMDDHHMM                         
082100        PERFORM WY2000QB                                                  
082200        IF  TMP1-YYMMDDHHMM  >=    TMP2-YYMMDDHHMM                        
082300        AND TMP1-YYMMDDHHMM  <=    TMP3-YYMMDDHHMM                        
082400          IF ORQD-SEQC-KDODELSTA = 'U'                                    
082500             ADD ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-TIRFS-U           
082600          ELSE                                                            
082700             ADD ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-TIRFS-R           
082800          END-IF                                                          
082900        END-IF                                                            
083000        PERFORM IMS-GN-ORQD-WDQ3C1                                        
083100     END-PERFORM                                                          
083200     .                                                                    
083300     EJECT                                                                
083400 FYE-DSPL-SCRN-LINE SECTION.                                              
083500                                                                          
083600     PERFORM FYEA-COLUM-123                                               
083700     PERFORM FYEB-COLUM-456                                               
083800     PERFORM FYEC-COLUM-78                                                
083900     PERFORM FYED-COLUM-9AB                                               
084000     .                                                                    
084100     EJECT                                                                
084200 FYEA-COLUM-123 SECTION.                                                  
084300                                                                          
084400     MOVE W-4471-IDPRC          TO W-IDPRC-RAD                            
084500     MOVE XX00-4472-KVRADER-RFS TO W-KVRADER-RAD                          
084600     MOVE XX00-4472-KVRADER-DAG TO W-KVRADER-DAG-RAD                      
084700     ADD  XX00-4472-KVRADER-RFS TO WS-KVRADER-TOT (02)                    
084800     ADD  XX00-4472-KVRADER-DAG TO WS-KVRADER-TOT (03)                    
084900     .                                                                    
085000     EJECT                                                                
085100 FYEB-COLUM-456 SECTION.                                                  
085200                                                                          
085300     MOVE XX00-4472-KVRADER-DAG TO WS-KVRADER-CL4                         
085400     IF WS-PFI-KDKALK = 1                                                 
085500        SUBTRACT XX00-ORQC-KVRADER-TIRFS-U FROM WS-KVRADER-CL4            
085600        SUBTRACT XX00-4472-KVRADER-PRAPP   FROM WS-KVRADER-CL4            
085700     ELSE                                                                 
085800        SUBTRACT XX00-ORQC-KVRADER-U       FROM WS-KVRADER-CL4            
085900        SUBTRACT XX00-4472-KVRADER-PRAPP   FROM WS-KVRADER-CL4            
086000     END-IF                                                               
086100     IF WS-KVRADER-CL4   < ZERO                                           
086200         MOVE ZERO       TO WS-KVRADER-CL4                                
086300     END-IF                                                               
086400     MOVE WS-KVRADER-CL4 TO W-KVRADER-RST-RAD                             
086500     ADD  WS-KVRADER-CL4 TO WS-KVRADER-TOT (04)                           
086600     IF WS-PFI-KDKALK = 1                                                 
086700        MOVE XX00-ORQC-KVRADER-TIRFS-R TO WS-KVRADER-REG-RAD              
086800        IF WS-KVRADER-REG-RAD < 0                                         
086900           MOVE 0                 TO WS-KVRADER-REG-RAD                   
087000        END-IF                                                            
087100        MOVE WS-KVRADER-REG-RAD   TO W-KVRADER-REG-RAD                    
087200        ADD  WS-KVRADER-REG-RAD   TO WS-KVRADER-TOT (05)                  
087300     ELSE                                                                 
087400        MOVE XX00-ORQC-KVRADER-R       TO WS-KVRADER-REG-RAD              
087500        IF WS-KVRADER-REG-RAD < 0                                         
087600           MOVE 0                 TO WS-KVRADER-REG-RAD                   
087700        END-IF                                                            
087800        MOVE WS-KVRADER-REG-RAD   TO W-KVRADER-REG-RAD                    
087900        ADD  WS-KVRADER-REG-RAD   TO WS-KVRADER-TOT (05)                  
088000     END-IF                                                               
088100     IF WS-PFI-KDKALK = 1                                                 
088200       MOVE XX00-ORQC-KVRADER-TIRFS-U  TO W-KVRADER-UT-RAD                
088300       ADD  XX00-ORQC-KVRADER-TIRFS-U  TO WS-KVRADER-TOT (06)             
088400     ELSE                                                                 
088500       MOVE XX00-ORQC-KVRADER-U        TO W-KVRADER-UT-RAD                
088600       ADD  XX00-ORQC-KVRADER-U        TO WS-KVRADER-TOT (06)             
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 FYEC-COLUM-78 SECTION.                                                   
089100                                                                          
089200     COMPUTE WS-KVRADER-CL7 =                                             
089300                     WS-KVARBTID * 1                                      
089400     END-COMPUTE                                                          
089500     MOVE WS-KVRADER-CL7    TO   W-KVRADER-KAN-RAD                        
089600     ADD  WS-KVRADER-CL7    TO   WS-KVRADER-TOT (07)                      
089700                                                                          
089800     MOVE 0                 TO WS-KVRADER-CL8                             
089900     MOVE WS-KVRADER-CL8    TO W-KVRADER-PLMI-RAD                         
090000     ADD  WS-KVRADER-CL8    TO WS-KVRADER-TOT (08)                        
090100     .                                                                    
090200     EJECT                                                                
090300 FYED-COLUM-9AB SECTION.                                                  
090400                                                                          
090500     IF WS-PFI-KDKALK = 1                                                 
090600        PERFORM FYEDA-COLUM-9AB-KALK1                                     
090700     ELSE                                                                 
090800        PERFORM FYEDB-COLUM-9AB-KALKX                                     
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200 FYEDA-COLUM-9AB-KALK1 SECTION.                                           
091300                                                                          
091400     MOVE WU-KVRADER-SHFT (1)                                             
091500                          TO W-KVRADER-META-RAD                           
091600     MOVE WU-KVRADER-SHFT (2)                                             
091700                          TO W-KVRADER-METB-RAD                           
091800     MOVE WU-KVRADER-SHFT (3)                                             
091900                          TO W-KVRADER-METC-RAD                           
092000     ADD  WU-KVRADER-SHFT (1) TO WS-KVRADER-TOT (9)                       
092100     ADD  WU-KVRADER-SHFT (2) TO WS-KVRADER-TOT (10)                      
092200     ADD  WU-KVRADER-SHFT (3) TO WS-KVRADER-TOT (11)                      
092300     .                                                                    
092400     EJECT                                                                
092500 FYEDB-COLUM-9AB-KALKX SECTION.                                           
092600                                                                          
092700     MOVE XX00-4472-KVRADER-SHFT-ALL (1)                                  
092800                          TO W-KVRADER-META-RAD                           
092900     MOVE XX00-4472-KVRADER-SHFT-ALL (2)                                  
093000                          TO W-KVRADER-METB-RAD                           
093100     MOVE XX00-4472-KVRADER-SHFT-ALL (3)                                  
093200                          TO W-KVRADER-METC-RAD                           
093300     ADD  XX00-4472-KVRADER-SHFT-ALL (1)                                  
093400                              TO WS-KVRADER-TOT (9)                       
093500     ADD  XX00-4472-KVRADER-SHFT-ALL (2)                                  
093600                              TO WS-KVRADER-TOT (10)                      
093700     ADD  XX00-4472-KVRADER-SHFT-ALL (3)                                  
093800                              TO WS-KVRADER-TOT (11)                      
093900     .                                                                    
094000     EJECT                                                                
094100 FYF-MOVE-W-RAD      SECTION.                                             
094200                                                                          
094300     MOVE W-IDPRC-RAD          TO RAD-IDPRC             (INDX)            
094400     MOVE W-KVRADER-RAD        TO RAD-KVRADER           (INDX)            
094500     MOVE W-KVRADER-DAG-RAD    TO RAD-KVRADER-DAG       (INDX)            
094600     MOVE W-KVRADER-RST-RAD    TO RAD-KVRADER-RST       (INDX)            
094700     MOVE W-KVRADER-REG-RAD    TO RAD-KVRADER-REG       (INDX)            
094800     MOVE W-KVRADER-UT-RAD     TO RAD-KVRADER-UT        (INDX)            
094900     MOVE W-KVRADER-KAN-RAD    TO RAD-KVRADER-KAN       (INDX)            
095000     MOVE W-KVRADER-PLMI-RAD   TO RAD-KVRADER-PLMI      (INDX)            
095100     MOVE W-KVRADER-META-RAD   TO RAD-KVRADER-META      (INDX)            
095200     MOVE W-KVRADER-METB-RAD   TO RAD-KVRADER-METB      (INDX)            
095300     MOVE W-KVRADER-METC-RAD   TO RAD-KVRADER-METC      (INDX)            
095400     .                                                                    
095500     EJECT                                                                
095600 FYG-INIT-W-RAD      SECTION.                                             
095700                                                                          
095800     MOVE ZERO                 TO W-IDPRC-RAD                             
095900                                  W-KVRADER-RAD                           
096000                                  W-KVRADER-DAG-RAD                       
096100                                  W-KVRADER-RST-RAD                       
096200                                  W-KVRADER-REG-RAD                       
096300                                  W-KVRADER-UT-RAD                        
096400                                  W-KVRADER-KAN-RAD                       
096500                                  W-KVRADER-PLMI-RAD                      
096600                                  W-KVRADER-META-RAD                      
096700                                  W-KVRADER-METB-RAD                      
096800                                  W-KVRADER-METC-RAD                      
096900     .                                                                    
097000     EJECT                                                                
097100 FZ-TOTAL-LINE SECTION.                                                   
097200                                                                          
097300       MOVE WS-KVRADER-TOT (2)  TO  TOT-KVRADER                           
097400       MOVE WS-KVRADER-TOT (3)  TO  TOT-KVRADER-DAG                       
097500       MOVE WS-KVRADER-TOT (4)  TO  TOT-KVRADER-RST                       
097600       MOVE WS-KVRADER-TOT (5)  TO  TOT-KVRADER-REG                       
097700       MOVE WS-KVRADER-TOT (6)  TO  TOT-KVRADER-UT                        
097800       MOVE WS-KVRADER-TOT (7)  TO  TOT-KVRADER-KAN                       
097900       MOVE WS-KVRADER-TOT (8)  TO  TOT-KVRADER-PLMI                      
098000       MOVE WS-KVRADER-TOT (9)  TO  TOT-KVRADER-META                      
098100       MOVE WS-KVRADER-TOT (10) TO  TOT-KVRADER-METB                      
098200       MOVE WS-KVRADER-TOT (11) TO  TOT-KVRADER-METC                      
098300       IF WS-KVRADER-TOT (2) = +0                                         
098400          IF WS-KVRADER-TOT (3) = +0                                      
098500             IF WS-KVRADER-TOT (4) = +0                                   
098600                IF WS-KVRADER-TOT (5) = +0                                
098700                   MOVE WS-KVRADER-TOT (7) TO                             
098800                                    TOT-KVRADER-PLMI                      
098900                END-IF                                                    
099000             END-IF                                                       
099100          END-IF                                                          
099200       END-IF                                                             
099300       .                                                                  
099400    EJECT                                                                 
099500 S00-GET-WDGX4448 SECTION.                                                
099600          PERFORM UNTIL                                                   
099700              (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-01)                   
099800         OR   (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-02)                   
099900         OR   (SEGMENT-GE)                                                
100000                 PERFORM IMS-GNP-XXKH-WDGX4448                            
100100         END-PERFORM                                                      
100200     .                                                                    
100300     EJECT                                                                
100400 S10-SKRIV-UTFIL  SECTION.                                                
100500                                                                          
100600     IF PGRP-IX = 1                                                       
100700        PERFORM S11-SKRIV-W41333                                          
100800     END-IF                                                               
100900     IF PGRP-IX = 2                                                       
101000        PERFORM S12-SKRIV-W41334                                          
101100     END-IF                                                               
101200     IF PGRP-IX = 3                                                       
101300        PERFORM S13-SKRIV-W41335                                          
101400     END-IF                                                               
101500     IF PGRP-IX = 4                                                       
101600        PERFORM S14-SKRIV-W41336                                          
101700     END-IF                                                               
101800     IF PGRP-IX = 5                                                       
101900        PERFORM S15-SKRIV-W41337                                          
102000     END-IF                                                               
102100     IF PGRP-IX = 6                                                       
102200        PERFORM S16-SKRIV-W41338                                          
102300     END-IF                                                               
102400     IF PGRP-IX = 7                                                       
102500        PERFORM S17-SKRIV-W41339                                          
102600     END-IF                                                               
102700     IF PGRP-IX = 8                                                       
102800        PERFORM S18-SKRIV-W41340                                          
102900     END-IF                                                               
103000     .                                                                    
103100     SKIP2                                                                
103200 S11-SKRIV-W41333 SECTION.                                                
103300                                                                          
103400     IF WS-ANTAL-RAD = ZERO                                               
103500        WRITE UT1-POST FROM RUBRIK1                                       
103600        WRITE UT1-POST FROM RUBRIK2                                       
103700        WRITE UT1-POST FROM TOMRAD                                        
103800        WRITE UT1-POST FROM RUBRIK4                                       
103900        WRITE UT1-POST FROM RUBRIK5                                       
104000        MOVE +1      TO WS-ANTAL-RAD                                      
104100     END-IF                                                               
104200                                                                          
104300     WRITE UT1-POST FROM RAD(INDX)                                        
104400                                                                          
104500     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
104600     MOVE 'W41333'   TO POSTSUM-FDNAMN                                    
104700     MOVE 'W41332D1' TO POSTSUM-DDNAMN2                                   
104800     CALL POSTSUM USING POSTSUM-PARM                                      
104900     ADD +1          TO WS-ANTAL-RAD                                      
105000     .                                                                    
105100     EJECT                                                                
105200 S12-SKRIV-W41334 SECTION.                                                
105300                                                                          
105400     IF WS-ANTAL-RAD = ZERO                                               
105500        WRITE UT2-POST FROM RUBRIK1                                       
105600        WRITE UT2-POST FROM RUBRIK2                                       
105700        WRITE UT2-POST FROM TOMRAD                                        
105800        WRITE UT2-POST FROM RUBRIK4                                       
105900        WRITE UT2-POST FROM RUBRIK5                                       
106000        MOVE +1      TO WS-ANTAL-RAD                                      
106100     END-IF                                                               
106200                                                                          
106300     WRITE UT2-POST FROM RAD(INDX)                                        
106400                                                                          
106500     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
106600     MOVE 'W41334'   TO POSTSUM-FDNAMN                                    
106700     MOVE 'W41332D2' TO POSTSUM-DDNAMN2                                   
106800     CALL POSTSUM USING POSTSUM-PARM                                      
106900     ADD +1          TO WS-ANTAL-RAD                                      
107000     .                                                                    
107100     EJECT                                                                
107200 S13-SKRIV-W41335 SECTION.                                                
107300                                                                          
107400     IF WS-ANTAL-RAD = ZERO                                               
107500        WRITE UT3-POST FROM RUBRIK1                                       
107600        WRITE UT3-POST FROM RUBRIK2                                       
107700        WRITE UT3-POST FROM TOMRAD                                        
107800        WRITE UT3-POST FROM RUBRIK4                                       
107900        WRITE UT3-POST FROM RUBRIK5                                       
108000        MOVE +1      TO WS-ANTAL-RAD                                      
108100     END-IF                                                               
108200                                                                          
108300     WRITE UT3-POST FROM RAD(INDX)                                        
108400                                                                          
108500     MOVE 'UT3'      TO POSTSUM-TRANSTYP                                  
108600     MOVE 'W41335'   TO POSTSUM-FDNAMN                                    
108700     MOVE 'W41332D3' TO POSTSUM-DDNAMN2                                   
108800     CALL POSTSUM USING POSTSUM-PARM                                      
108900     ADD +1          TO WS-ANTAL-RAD                                      
109000     .                                                                    
109100     EJECT                                                                
109200 S14-SKRIV-W41336 SECTION.                                                
109300                                                                          
109400     IF WS-ANTAL-RAD = ZERO                                               
109500        WRITE UT4-POST FROM RUBRIK1                                       
109600        WRITE UT4-POST FROM RUBRIK2                                       
109700        WRITE UT4-POST FROM TOMRAD                                        
109800        WRITE UT4-POST FROM RUBRIK4                                       
109900        WRITE UT4-POST FROM RUBRIK5                                       
110000        MOVE +1      TO WS-ANTAL-RAD                                      
110100     END-IF                                                               
110200                                                                          
110300     WRITE UT4-POST FROM RAD(INDX)                                        
110400                                                                          
110500     MOVE 'UT4'      TO POSTSUM-TRANSTYP                                  
110600     MOVE 'W41336'   TO POSTSUM-FDNAMN                                    
110700     MOVE 'W41332D4' TO POSTSUM-DDNAMN2                                   
110800     CALL POSTSUM USING POSTSUM-PARM                                      
110900     ADD +1          TO WS-ANTAL-RAD                                      
111000     .                                                                    
111100     EJECT                                                                
111200 S15-SKRIV-W41337 SECTION.                                                
111300                                                                          
111400     IF WS-ANTAL-RAD = ZERO                                               
111500        WRITE UT5-POST FROM RUBRIK1                                       
111600        WRITE UT5-POST FROM RUBRIK2                                       
111700        WRITE UT5-POST FROM TOMRAD                                        
111800        WRITE UT5-POST FROM RUBRIK4                                       
111900        WRITE UT5-POST FROM RUBRIK5                                       
112000        MOVE +1      TO WS-ANTAL-RAD                                      
112100     END-IF                                                               
112200                                                                          
112300     WRITE UT5-POST FROM RAD(INDX)                                        
112400                                                                          
112500     MOVE 'UT5'      TO POSTSUM-TRANSTYP                                  
112600     MOVE 'W41337'   TO POSTSUM-FDNAMN                                    
112700     MOVE 'W41332D5' TO POSTSUM-DDNAMN2                                   
112800     CALL POSTSUM USING POSTSUM-PARM                                      
112900     ADD +1          TO WS-ANTAL-RAD                                      
113000     .                                                                    
113100     EJECT                                                                
113200 S16-SKRIV-W41338 SECTION.                                                
113300                                                                          
113400     IF WS-ANTAL-RAD = ZERO                                               
113500        WRITE UT6-POST FROM RUBRIK1                                       
113600        WRITE UT6-POST FROM RUBRIK2                                       
113700        WRITE UT6-POST FROM TOMRAD                                        
113800        WRITE UT6-POST FROM RUBRIK4                                       
113900        WRITE UT6-POST FROM RUBRIK5                                       
114000        MOVE +1      TO WS-ANTAL-RAD                                      
114100     END-IF                                                               
114200                                                                          
114300     WRITE UT6-POST FROM RAD(INDX)                                        
114400                                                                          
114500     MOVE 'UT6'      TO POSTSUM-TRANSTYP                                  
114600     MOVE 'W41338'   TO POSTSUM-FDNAMN                                    
114700     MOVE 'W41332D6' TO POSTSUM-DDNAMN2                                   
114800     CALL POSTSUM USING POSTSUM-PARM                                      
114900     ADD +1          TO WS-ANTAL-RAD                                      
115000     .                                                                    
115100     EJECT                                                                
115200 S17-SKRIV-W41339 SECTION.                                                
115300                                                                          
115400     IF WS-ANTAL-RAD = ZERO                                               
115500        WRITE UT7-POST FROM RUBRIK1                                       
115600        WRITE UT7-POST FROM RUBRIK2                                       
115700        WRITE UT7-POST FROM TOMRAD                                        
115800        WRITE UT7-POST FROM RUBRIK4                                       
115900        WRITE UT7-POST FROM RUBRIK5                                       
116000        MOVE +1      TO WS-ANTAL-RAD                                      
116100     END-IF                                                               
116200                                                                          
116300     WRITE UT7-POST FROM RAD(INDX)                                        
116400                                                                          
116500     MOVE 'UT7'      TO POSTSUM-TRANSTYP                                  
116600     MOVE 'W41339'   TO POSTSUM-FDNAMN                                    
116700     MOVE 'W41332D7' TO POSTSUM-DDNAMN2                                   
116800     CALL POSTSUM USING POSTSUM-PARM                                      
116900     ADD +1          TO WS-ANTAL-RAD                                      
117000     .                                                                    
117100     EJECT                                                                
117200 S18-SKRIV-W41340 SECTION.                                                
117300                                                                          
117400     IF WS-ANTAL-RAD = ZERO                                               
117500        WRITE UT8-POST FROM RUBRIK1                                       
117600        WRITE UT8-POST FROM RUBRIK2                                       
117700        WRITE UT8-POST FROM TOMRAD                                        
117800        WRITE UT8-POST FROM RUBRIK4                                       
117900        WRITE UT8-POST FROM RUBRIK5                                       
118000        MOVE +1      TO WS-ANTAL-RAD                                      
118100     END-IF                                                               
118200                                                                          
118300     WRITE UT8-POST FROM RAD(INDX)                                        
118400                                                                          
118500     MOVE 'UT8'      TO POSTSUM-TRANSTYP                                  
118600     MOVE 'W41340'   TO POSTSUM-FDNAMN                                    
118700     MOVE 'W41332D8' TO POSTSUM-DDNAMN2                                   
118800     CALL POSTSUM USING POSTSUM-PARM                                      
118900     ADD +1          TO WS-ANTAL-RAD                                      
119000     .                                                                    
119100     EJECT                                                                
119200 RAD-RENSA-FAELT-UT SECTION.                                              
119300                                                                          
119400     MOVE +1 TO INDX                                                      
119500     PERFORM UNTIL INDX > MAX-INDX                                        
119600        MOVE SPACE TO RAD(INDX)                                           
119700****    PERFORM RAD-ERASE-LINE                                            
119800        ADD +1 TO INDX                                                    
119900     END-PERFORM                                                          
120000     .                                                                    
120100     EJECT                                                                
120200 RAD-ERASE-LINE SECTION.                                                  
120300                                                                          
120400           MOVE SPACE           TO RAD-IDPRC           (INDX)             
120500           MOVE ZERO            TO RAD-KVRADER         (INDX)             
120600                                   RAD-KVRADER-DAG     (INDX)             
120700                                   RAD-KVRADER-RST     (INDX)             
120800                                   RAD-KVRADER-REG     (INDX)             
120900                                   RAD-KVRADER-UT      (INDX)             
121000                                   RAD-KVRADER-KAN     (INDX)             
121100                                   RAD-KVRADER-PLMI    (INDX)             
121200                                   RAD-KVRADER-META    (INDX)             
121300                                   RAD-KVRADER-METB    (INDX)             
121400                                   RAD-KVRADER-METC    (INDX)             
121500     .                                                                    
121600     EJECT                                                                
121700* --- IMS SECTION ---                                                     
121800     SKIP3                                                                
121900 IMS-GU-XXKH-WDGX4447 SECTION.                                            
122000*                                                                         
122100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
122200          DELIMITED BY SIZE INTO SSA1                                     
122300     MOVE '  GE' TO GODK-STATUSKODER                                      
122400     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA1 SSA1                     
122500     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
122600     PERFORM IMS-STATUSKONTROLL                                           
122700     .                                                                    
122800*                                                                         
122900 IMS-GNP-XXKH-WDGX4448 SECTION.                                           
123000                                                                          
123100     STRING 'WLXXKH11(KDPRCGRP= ' WS-PFI-KDPRCGRP ')'                     
123200          DELIMITED BY SIZE INTO SSA1                                     
123300     MOVE '  GE' TO GODK-STATUSKODER                                      
123400     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
123500     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800*                                                                         
123900 IMS-GU-XXKH-WDGX4448 SECTION.                                            
124000                                                                          
124100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
124200          DELIMITED BY SIZE INTO SSA1                                     
124300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
124400          DELIMITED BY SIZE INTO SSA2                                     
124500     MOVE '  GE' TO GODK-STATUSKODER                                      
124600     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA1 SSA1 SSA2               
124700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
124800     PERFORM IMS-STATUSKONTROLL                                           
124900     .                                                                    
125000*                                                                         
125100 IMS-LST-XXKH-WDGX4448 SECTION.                                           
125200*                                                                         
125300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY                          
125400                    '&KDPRCGRP =' WS-PFI-KDPRCGRP ')'                     
125500          DELIMITED BY SIZE INTO SSA1                                     
125600     MOVE '  GE' TO GODK-STATUSKODER                                      
125700     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
125800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     EJECT                                                                
126200 IMS-GU-XXKW-WDGX4472 SECTION.                                            
126300                                                                          
126400     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY ')'                      
126500          DELIMITED BY SIZE INTO SSA1                                     
126600     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY ')'                     
126700          DELIMITED BY SIZE INTO SSA2                                     
126800     MOVE '  GE' TO GODK-STATUSKODER                                      
126900     CALL CBLTDLI USING GU  XXKW-PCB DLI-IO-AREA2 SSA1 SSA2               
127000     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
127100     PERFORM IMS-STATUSKONTROLL                                           
127200     .                                                                    
127300     EJECT                                                                
127400 IMS-GU-ORQD-WDQ3C1 SECTION.                                              
127500*                                                                         
127600     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
127700                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX ')'                      
127800          DELIMITED BY SIZE INTO SSA1                                     
127900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
128000     CALL CBLTDLI USING GU  ORQD-PCB DLI-IO-AREA2 SSA1                    
128100     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSKONTROLL                                           
128300     .                                                                    
128400     EJECT                                                                
128500 IMS-GN-ORQD-WDQ3C1 SECTION.                                              
128600*                                                                         
128700     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
128800                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX ')'                      
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
129100     CALL CBLTDLI USING GN  ORQD-PCB DLI-IO-AREA2 SSA1                    
129200     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500     EJECT                                                                
129600 IMS-STATUSKONTROLL SECTION.                                              
129700                                                                          
129800     SET STATUS-IX TO 1                                                   
129900     SEARCH GODK-STATUS                                                   
130000       AT END CALL FELLOG                                                 
130100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
130200     END-SEARCH                                                           
130300     .                                                                    
130400     EJECT                                                                
130500*    -COPY WY2000P1                                                       
130600     EJECT                                                                
130700*    -COPY WY2000QB                                                       
