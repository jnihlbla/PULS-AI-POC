000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038700.                                                
000400 AUTHOR.         CAO-VAN NGU.                                             
000500 DATE-WRITTEN.   90/06/06.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    DISPLAY  WDGX4471-72  SCREEN 4387                                    
001000*                         INPUTED VIA SCREEN 40387                        
001100*    LOOP (ON KDPRCGRP / IDPRC)                                           
001200*    READ SEQUENTIALLY WDGX4447-48 FOR 4448-IDPRCBAS                      
001300*    READ  WDGX4471-72 WITH KEY = 4448-IDPRCBAS                           
001400*    READ DATA FROM WDQ3 WITH SECONDARY INDEX WDQ2C                       
001500*    HANDLE DATA FOR DISPLAY                                              
001600*    END-LOOP                                                             
001700*                                                                         
001800*        TRANSACTION: W4T387                                              
001900*        MID:         W4I38701                                            
002000*        MOD:         W4O38701                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601*    -COPY WY2000WB                                                       
002610     SKIP3                                                                
002620*    -COPY WY2000W1                                                       
002630     SKIP3                                                                
002700 77  IDPGM                       PIC X(08)   VALUE 'W4038700'.            
002800                                                                          
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100                                                                          
003200*    ---                                                                  
003300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003400 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003500                                                                          
003700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003710                                                                          
003720 01      WS-TIRFS                PIC 9(11).                               
003730 01      FILLER REDEFINES WS-TIRFS.                                       
003740   03    FILLER                  PIC X(1).                                
003750   03    WS-RFS-DATE             PIC 9(6).                                
003760   03    WS-RFS-TIME             PIC X(4).                                
003800                                                                          
003810*                                                                         
003820*          DATE FROM DC-LOCAL                                             
003830*                                                                         
003840 01  FILLER.                                                              
003850     05  WS-LOCAL-DATE         PIC  9(06) VALUE 0.                        
003900*    ---                                                                  
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004200     88  INDATA-OK                           VALUE 'J'.                   
004300     88  INDATA-FEL                          VALUE 'N'.                   
004400                                                                          
004500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004600     88  NYCKLAR-OK                          VALUE 'J'.                   
004700     88  NYCKLAR-FEL                         VALUE 'N'.                   
004800                                                                          
004900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005000     88  ALLT-OK                             VALUE 'J'.                   
005100                                                                          
005500 77  RAD-INFO-SW                 PIC X       VALUE 'J'.                   
005600     88  RAD-INFO-OK                         VALUE 'J'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '4387'.                
006000     88  GODK-MID                            VALUE '4386' '4387'.         
006001*                                                                         
006002 01  WS-DCUSER.                                                           
006003     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
006004     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
006005     03 FILLER                   PIC X(1)   VALUE SPACE.                  
006006*      --- VALID IDDC CODES                                               
006007*                                                                         
006008*01    -COPY WWDC99                                                       
006009       EJECT                                                              
006700 01  MID-INPUT.                                                           
006800     05  WS-PFI-KDPRCGRP             PIC X(5) VALUE SPACE.                
006900     05  WS-PFI-KDPRODKL-01          PIC X(1) VALUE SPACE.                
007000         88 GOOD-PFI-KDPRODKL-01       VALUE ' ' 'B' 'C'.                 
007100     05  WS-PFI-KDPRODKL-02          PIC X(1) VALUE SPACE.                
007200         88 GOOD-PFI-KDPRODKL-02       VALUE ' ' 'B' 'C'.                 
007300     05  WS-PFI-IDPRC.                                                    
007400         10  WS-PFI-IDPRCBAS         PIC X(3) VALUE SPACES.               
007500         10  WS-PFI-IDPRCVAR         PIC X(1) VALUE SPACE.                
007600     05  WS-PFI-TIRFS-01             PIC 9(6) VALUE 0.                    
007700     05  WS-PFI-TIRFS-02             PIC 9(6) VALUE 0.                    
007800     05  WS-PFI-KDKALK               PIC 9(1) VALUE 0.                    
007900         88 GOOD-PFI-KDKALK          VALUE 1 2.                           
008100*                                                                         
008200*                 DATA FOR REFERENCES                                     
008300*                                                                         
008400 01  REF-DATA.                                                            
008500     05  WS-KVRADER-REG-RAD      PIC S9(7)      COMP-3 VALUE +0.          
008510     05  WS-KVARBTID             PIC S9(4)V9(1) COMP-3 VALUE +0.          
008600     05  WS-KVBEMAN              PIC S9(4)V9(1) COMP-3 VALUE +0.          
008700     05  WS-TIRFS01              PIC S9(11)     COMP-3 VALUE +0.          
008800     05  WS-TIRFS02              PIC S9(11)     COMP-3 VALUE +0.          
009200                                                                          
009201     05  WS-IDPRCVAR             PIC X(1) VALUE SPACES.                   
009210     05  WS-FIRST-IDPRC          PIC X(4) VALUE SPACES.                   
009300*                                                                         
009400 01  FILLER.                                                              
009500     05  WS-DB-TIRFS            PIC 9(11) VALUE 0.                        
009600     05  FILLER REDEFINES WS-DB-TIRFS.                                    
009700         10  WS-DB-TIRFS-AAMMDD PIC 9(7).                                 
009800         10  WS-DB-TIRFS-HHMM   PIC 9(4).                                 
009900*                                                                         
010000 01  TOTAL-TABLE.                                                         
010100     05  WS-KVRADER-TOT OCCURS 11 PIC S9(7) COMP-3 VALUE +0.              
010200     EJECT                                                                
010300*                                                                         
010400 01  XX00-4472.                                                           
010500         03  XX00-4472-KVRADER-DAG      PIC S9(7) COMP-3.                 
010600         03  XX00-4472-KVRADER-RFS      PIC S9(7) COMP-3.                 
010700         03  XX00-4472-KVRADER-PRAPP    PIC S9(7) COMP-3.                 
010800         03  FILLER OCCURS 3.                                             
010900             07  XX00-4472-KVRADER-SHFT-ALL PIC S9(7) COMP-3.             
011000         03  FILLER OCCURS 30.                                            
011100             07  XX00-4472-TIRFS        PIC S9(11) COMP-3.                
011200             07  XX00-4472-KVRADER      PIC S9(7)  COMP-3.                
011300             07  FILLER OCCURS 3.                                         
011400                 11  XX00-4472-KVRADER-SHFT PIC S9(7) COMP-3.             
011500*                                                                         
011600 01  KVRADER-SHFT-TIRFS-00.                                               
011700     05  FILLER                  PIC S9(7) COMP-3 VALUE +0.               
011800     05  FILLER.                                                          
011900         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
012000         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
012100         10  FILLER              PIC S9(7) COMP-3 VALUE +0.               
012200*                                                                         
012300 01  KVRADER-SHFT-TIRFS.                                                  
012400     05  WU-KVRADER              PIC S9(7) COMP-3.                        
012500     05  FILLER OCCURS 3.                                                 
012600         10  WU-KVRADER-SHFT     PIC S9(7) COMP-3.                        
012700*                                                                         
012800 01  FILLER.                                                              
012900     05  XX00-ORQC-KVRADER-U         PIC S9(7) COMP-3 VALUE +0.           
013000     05  XX00-ORQC-KVRADER-TIRFS-U   PIC S9(7) COMP-3 VALUE +0.           
013100*                                                                         
013110 01  FILLER.                                                              
013120     05  XX00-ORQC-KVRADER-R         PIC S9(7) COMP-3 VALUE +0.           
013130     05  XX00-ORQC-KVRADER-TIRFS-R   PIC S9(7) COMP-3 VALUE +0.           
013140*                                                                         
013200 01  FILLER.                                                              
013300     05  WS-KVRADER-CL4  PIC S9(7)   COMP-3 VALUE +0.                     
013400     05  WS-KVRADER-CL7  PIC S9(7)   COMP-3 VALUE +0.                     
013500     05  WS-KVRADER-CL8  PIC S9(7)   COMP-3 VALUE +0.                     
013600*                                                                         
013700     EJECT                                                                
014300 01  FILLER-INDX.                                                         
014400     05  RS-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
014500     05  RT-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
014600     05  RU-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
014700     05  RW-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
014800     05  MD-INDX          PIC S9(3)  COMP-3 VALUE +0.                     
014900     05  RS-INDX-MAX      PIC S9(3)  COMP-3 VALUE +30.                    
015000*                                                                         
015100     EJECT                                                                
015200 01  W-MOD-RAD.                                                           
015300     05 W-IDPRC-RAD.                                                      
015400        07 W-IDPRCBAS         PIC X(3)         VALUE ZERO.                
015500        07 W-IDPRCVAR         PIC X            VALUE ZERO.                
015600     05 W-KVRADER-RAD         PIC Z(4)9        VALUE ZERO.                
015700     05 W-KVRADER-DAG-RAD     PIC Z(4)9        VALUE ZERO.                
015800     05 W-KVRADER-RST-RAD     PIC Z(4)9        VALUE ZERO.                
015900     05 W-KVRADER-REG-RAD     PIC Z(4)9        VALUE ZERO.                
016000     05 W-KVRADER-UT-RAD      PIC Z(4)9        VALUE ZERO.                
016100     05 W-KVRADER-KAN-RAD     PIC Z(4)9        VALUE ZERO.                
016200     05 W-KVRADER-PLMI-RAD    PIC -Z(3)9.9     VALUE ZERO.                
016300     05 W-KVRADER-META-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
016400     05 W-KVRADER-METB-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
016500     05 W-KVRADER-METC-RAD    PIC Z(3)9.9(1)   VALUE ZERO.                
016600     EJECT                                                                
016700 01  GENERELLA-SUBPROGRAM.                                                
016800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
016900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
017100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017201     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017203*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017210*01 -COPY WMSGINIT                                                        
017211     SKIP3                                                                
017220*01 -COPY WMSGINIT -PRE DC-                                               
017350     SKIP3                                                                
017400*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
017500*   -COPY WORKAREA                                                        
017700     EJECT                                                                
017800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017900*   -COPY WMEDAREA                                                        
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
018300*   -COPY WDECAREA                                                        
018500     EJECT                                                                
018600*    --- AREA FOR IMS                                                     
018700*                                                                         
018800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018900     SKIP3                                                                
019000*01  MID -COPY W4I38701                                                   
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400     SKIP3                                                                
019500*01  -COPY WMSGAREA                                                       
019700     EJECT                                                                
019800*    03  MOD -COPY W4O38701   -RED MSG-AREA.                              
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020200     SKIP3                                                                
020300*01  -COPY WMFSAREA                                                       
020500     EJECT                                                                
020600*                                                                         
020700*                                                                         
020800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020900     SKIP3                                                                
021000*                                                                         
021100 01  NYCKLAR-TILL-DLI.                                                    
021200*                                                                         
021300     03  W-4447-WDGXKEY.                                                  
021400         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
021500         07 W-4447-IDDC          PIC X(02).                               
021600         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
021700*                                                                         
021800     03  W-4448-WDGXKEY.                                                  
021900         07 W-4448-IDPRC.                                                 
022000            11 W-4448-IDPRCBAS   PIC X(3).                                
022100            11 W-4448-IDPRCVAR   PIC X(1).                                
022200         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
022300*                                                                         
022400*                                                                         
022500     03  W-4471-WDGXKEY.                                                  
022600         07 FILLER               PIC X(4)    VALUE '4471'.                
022700         07 W-4471-IDDC          PIC X(02).                               
022800         07 W-4471-IDPRC.                                                 
022900            11 W-4471-IDPRCBAS   PIC X(3).                                
023000            11 W-4471-IDPRCVAR   PIC X(1).                                
023100         07 FILLER               PIC X(20)   VALUE LOW-VALUE.             
023200*                                                                         
023300     03  W-4472-KDSEGKEY.                                                 
023400         07 FILLER               PIC X(1)    VALUE '1'.                   
023500*                                                                         
023600     03  W-WDQ3C1KY-MIN.                                                  
023700         07 W-WDQ3C1KY-MIN-IDDC      PIC X(02).                           
023800         07 W-WDQ3C1KY-MIN-IDPRCBAS  PIC X(3).                            
023810         07 W-WDQ3C1KY-MIN-IDPRCVAR  PIC X(1).                            
023900         07 W-WDQ3C1KY-MIN-FILLER    PIC X(34) VALUE LOW-VALUE.           
024000*                                                                         
024100     03  W-WDQ3C1KY-MAX.                                                  
024200         07 W-WDQ3C1KY-MAX-IDDC      PIC X(02).                           
024300         07 W-WDQ3C1KY-MAX-IDPRCBAS  PIC X(3).                            
024310         07 W-WDQ3C1KY-MAX-IDPRCVAR  PIC X(1).                            
024400         07 W-WDQ3C1KY-MAX-FILLER    PIC X(34) VALUE HIGH-VALUE.          
024500*                                                                         
024600*    03  W-WDQ3C1KY-KDODELST-U       PIC X VALUE 'U'.                     
024700*                                                                         
024800 01  STATUS-WS                   PIC XX.                                  
024900     88  SEGMENT-OK                          VALUE '  '.                  
025000     88  SEGMENT-II                          VALUE 'II'.                  
025100     88  SEGMENT-GE                          VALUE 'GE'.                  
025200     88  SEGMENT-GB                          VALUE 'GB'.                  
025300     SKIP2                                                                
025400 01  4448-STATUS-WS              PIC XX.                                  
025500     88  4448-SEGMENT-OK                     VALUE '  '.                  
025600     88  4448-SEGMENT-II                     VALUE 'II'.                  
025700     88  4448-SEGMENT-GE                     VALUE 'GE'.                  
025800     88  4448-SEGMENT-GB                     VALUE 'GB'.                  
025900     SKIP2                                                                
026000 01  GODK-STATUSKODER.                                                    
026100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026200     SKIP3                                                                
026300 01  SSA1                        PIC X(128).                              
026400 01  SSA2                        PIC X(128).                              
026500     EJECT                                                                
026600*    --- IMS FUNKTIONSKODER                                               
026700*01  -COPY W0003                                                          
026900     EJECT                                                                
027000*    ---  DLI INPUT-OUTPUT AREA                                           
027100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027200     SKIP3                                                                
027300 01  DLI-IO-AREA1.                                                        
027400     03  IO-AREA1                PIC X(1200)  VALUE SPACE.                
027500     SKIP3                                                                
027600     03  WLXXKH01 REDEFINES IO-AREA1.                                     
027700*        05  -COPY WDGX4447   -PRE XXKH-                                  
027900*                                                                         
028000     03  WLXXKH11 REDEFINES IO-AREA1.                                     
028100*        05  -COPY WDGX4448   -PRE XXKH-                                  
028300     EJECT                                                                
028400 01  DLI-IO-AREA2.                                                        
028500     03  IO-AREA2                PIC X(1200)  VALUE SPACE.                
028600     SKIP3                                                                
028700     03  WLXXKW01 REDEFINES IO-AREA2.                                     
028800*        05  -COPY WDGX4471   -PRE XXKW-                                  
029000*                                                                         
029100     03  WLXXKW11 REDEFINES IO-AREA2.                                     
029200*        05  -COPY WDGX4472   -PRE XXKW-                                  
029400*                                                                         
029500     03  WLORQD01 REDEFINES IO-AREA2.                                     
029600*        05  -COPY WDQ3C1     -PRE ORQD-                                  
029800*                                                                         
029900     EJECT                                                                
030000 LINKAGE SECTION.                                                         
030100                                                                          
030200*01  -COPY W0009      -PRE MSG-                                           
030300     EJECT                                                                
030310*01  -COPY W0008      -PRE USEA-                                          
030320     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008      -PRE XXKH-                                          
030700     05  FILLER                  PIC X.                                   
030830     EJECT                                                                
030900*01  -COPY W0008      -PRE XXKW-                                          
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008      -PRE ORQD-                                          
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700     EJECT                                                                
031800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXKH-PCB XXKW-PCB             
031900                                   ORQD-PCB.                              
032000 W40387 SECTION.                                                          
032100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXKH-PCB XXKW-PCB             
032200                                   ORQD-PCB.                              
032300     PERFORM IMS-GET-MSG                                                  
032400     IF SEGMENT-OK                                                        
032500       PERFORM A-INIT                                                     
032600       PERFORM B-KOLLA-NYCKLAR                                            
032700       IF NYCKLAR-OK                                                      
032800           IF MFS-FIRST                                                   
032900             PERFORM C-FOERSTA-SIDA                                       
033000           ELSE                                                           
033100             IF MFS-NEXT                                                  
033200               PERFORM D-NAESTA-SIDA                                      
033300             ELSE                                                         
033400               PERFORM E-SAMMA-SIDA                                       
033500             END-IF                                                       
033600           END-IF                                                         
033700           IF ALLT-OK                                                     
033800             PERFORM F-LAES-VISA-INFO                                     
033900           END-IF                                                         
034000       END-IF                                                             
034100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38701 + 4                      
034200       PERFORM IMS-INSERT-MSG                                             
034300     END-IF                                                               
034400     MOVE ZERO TO RETURN-CODE                                             
034500     GOBACK                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 A-INIT SECTION.                                                          
034900                                                                          
035000     IF MSG-DUBBLA-TRANSKODER                                             
035100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38701                 
035200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
035300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035400     ELSE                                                                 
035500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38701                  
035600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035800     END-IF                                                               
035900                                                                          
036000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
036100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
036200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036300                                                                          
036400     MOVE LOW-VALUE TO MSG-AREA                                           
036500     MOVE '4387' TO MOD-IDTRANS                                           
036600     MOVE 'W4O387N1' TO MFS-IDMOD                                         
036700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
036710                                                                          
036800     IF NOT EGEN-MID                                                      
036900       MOVE SPACE TO MFS-KDTRTYP                                          
037000       MOVE '7' TO MFS-IDPFK                                              
037010       PERFORM AA-RENSA-MID                                               
037100     END-IF                                                               
037110                                                                          
037200     IF ENGLISH-TEXT                                                      
037300       MOVE +2 TO SPRAK-IX                                                
037500     ELSE                                                                 
037600       MOVE +1 TO SPRAK-IX                                                
037800     END-IF                                                               
037900     .                                                                    
038000     SKIP2                                                                
038100 AA-RENSA-MID    SECTION.                                                 
038101                                                                          
038102     MOVE SPACE             TO MID-PF7-KDPRCGRP                           
038103                               MID-PF7-KDPRODKL-01                        
038104                               MID-PF7-KDPRODKL-02                        
038105                               MID-PF7-IDPRC                              
038108                               MID-PF7-KDKALK                             
038109                               MID-PF7-IDDC                               
038110                               MID-PFE-IDPRC                              
038111                               MID-PF8-IDPRC                              
038112     MOVE ZERO              TO MID-PF7-TIRFS-01                           
038113                               MID-PF7-TIRFS-02                           
038114     .                                                                    
038115     SKIP2                                                                
038120 B-KOLLA-NYCKLAR SECTION.                                                 
038200                                                                          
038201     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038202     MOVE '013'             TO MSGI-KDCALL                                
038203     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
038204     MOVE '4387'            TO MSGI-IDTRANS                               
038205     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
038206     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038207     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
038210                                                                          
038300     MOVE JA  TO NYCKLAR-SW                                               
038400     PERFORM MFS-ERASE-MOD-PFI                                            
038500     PERFORM BA-CHCK-KDPRCGRP                                             
038600     PERFORM BB-CHCK-KDPRODKL                                             
038700                                                                          
039002     MOVE MSGI-IDDC               TO WS-IDDC                              
039003                                                                          
039004     MOVE ALL '+'           TO DC-MSGI-WMSGINIT                           
039005     MOVE '013'             TO DC-MSGI-KDCALL                             
039008     MOVE WS-IDDC           TO WS-DCUSER-IDDC                             
039009     MOVE WS-DCUSER         TO DC-MSGI-IDUSER                             
039010     MOVE '4387'            TO DC-MSGI-IDTRANS                            
039011     MOVE MSG-LTERM-NAME    TO DC-MSGI-IDLTERM-USER                       
039012     CALL W005INIT USING DC-MSGI-WMSGINIT USEA-PCB                        
039020     MOVE DC-MSGI-TILOKDAT  TO WS-LOCAL-DATE                              
039021                                                                          
039040     PERFORM BE-CHCK-TIRFS                                                
039050     PERFORM BF-CHCK-KDKALK                                               
039060     PERFORM BH-CNT-WRKDAYS                                               
039070     PERFORM BC-CHCK-IDPRC                                                
039080                                                                          
039100     PERFORM BG-CHCK-COMBIN                                               
039200     PERFORM BZ-FILL-MOD-PF7                                              
039300     IF NYCKLAR-FEL                                                       
039400       MOVE '401' TO MED-IDMFSFEL                                         
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039700       PERFORM MFS-RENSA-FAELT-UT                                         
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 BA-CHCK-KDPRCGRP SECTION.                                                
040110                                                                          
040200     IF MID-PFI-KDPRCGRP = ALL '+'                                        
040300       MOVE MID-PF7-KDPRCGRP TO WS-PFI-KDPRCGRP                           
040400     ELSE                                                                 
040500       MOVE MID-PFI-KDPRCGRP TO WS-PFI-KDPRCGRP                           
040600       MOVE '7'             TO MFS-IDPFK                                  
040700       MOVE SPACE           TO MFS-KDTRTYP                                
040800     END-IF                                                               
040900     IF  WS-PFI-KDPRCGRP = SPACES OR ALL '+'                              
041000        MOVE NEJ TO NYCKLAR-SW                                            
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 BB-CHCK-KDPRODKL SECTION.                                                
041500                                                                          
041600     IF MID-PFI-KDPRODKL-01 = ALL '+'                                     
041700       MOVE MID-PF7-KDPRODKL-01 TO WS-PFI-KDPRODKL-01                     
041800     ELSE                                                                 
041900       MOVE MID-PFI-KDPRODKL-01 TO WS-PFI-KDPRODKL-01                     
042000       MOVE '7'             TO MFS-IDPFK                                  
042100       MOVE SPACE           TO MFS-KDTRTYP                                
042200     END-IF                                                               
042300     IF MID-PFI-KDPRODKL-02 = ALL '+'                                     
042400       MOVE MID-PF7-KDPRODKL-02 TO WS-PFI-KDPRODKL-02                     
042500     ELSE                                                                 
042600       MOVE MID-PFI-KDPRODKL-02 TO WS-PFI-KDPRODKL-02                     
042700       MOVE '7'             TO MFS-IDPFK                                  
042800       MOVE SPACE           TO MFS-KDTRTYP                                
042900     END-IF                                                               
043000     IF (NOT GOOD-PFI-KDPRODKL-01) OR                                     
043100                    (NOT GOOD-PFI-KDPRODKL-02)                            
043200       MOVE NEJ TO NYCKLAR-SW                                             
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 BC-CHCK-IDPRC SECTION.                                                   
043700                                                                          
043800     IF MID-PFI-IDPRC = ALL '+'                                           
043900       MOVE MID-PF7-IDPRC TO WS-PFI-IDPRC                                 
044000     ELSE                                                                 
044100       MOVE MID-PFI-IDPRC TO WS-PFI-IDPRC                                 
044200       MOVE '7'          TO MFS-IDPFK                                     
044300       MOVE SPACE        TO MFS-KDTRTYP                                   
044400     END-IF                                                               
044410                                                                          
044420     IF WS-PFI-IDPRCBAS NOT = SPACE                                       
044430        IF WS-PFI-IDPRCVAR = SPACE                                        
044440           MOVE NEJ TO NYCKLAR-SW                                         
044450        END-IF                                                            
044460     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 BE-CHCK-TIRFS SECTION.                                                   
044800                                                                          
044900     IF MID-PFI-TIRFS-01 = ALL '+'                                        
045000       INSPECT MID-PF7-TIRFS-01 REPLACING                                 
045100                            LEADING SPACE BY ZERO                         
045200       MOVE MID-PF7-TIRFS-01 TO WS-PFI-TIRFS-01                           
045300     ELSE                                                                 
045400       INSPECT MID-PFI-TIRFS-01 REPLACING                                 
045500                            LEADING SPACE BY ZERO                         
045600       IF MID-PFI-TIRFS-01 IS NUMERIC                                     
045650          MOVE MID-PFI-TIRFS-01 TO WS-PFI-TIRFS-01                        
045800       ELSE                                                               
045900          MOVE 0 TO WS-PFI-TIRFS-01                                       
046000       END-IF                                                             
046100       MOVE '7'          TO MFS-IDPFK                                     
046200       MOVE SPACE        TO MFS-KDTRTYP                                   
046300     END-IF                                                               
046310                                                                          
046400     IF MID-PFI-TIRFS-02 = ALL '+'                                        
046410       INSPECT MID-PF7-TIRFS-02 REPLACING                                 
046420                            LEADING SPACE BY ZERO                         
046430       MOVE MID-PF7-TIRFS-02 TO WS-PFI-TIRFS-02                           
046600     ELSE                                                                 
046700       INSPECT MID-PFI-TIRFS-02 REPLACING                                 
046800                            LEADING SPACE BY ZERO                         
046900       IF MID-PFI-TIRFS-02 IS NUMERIC                                     
047006          MOVE MID-PFI-TIRFS-02 TO WS-PFI-TIRFS-02                        
047100       ELSE                                                               
047200          MOVE 0 TO WS-PFI-TIRFS-02                                       
047300       END-IF                                                             
047400       MOVE '7'          TO MFS-IDPFK                                     
047500       MOVE SPACE        TO MFS-KDTRTYP                                   
047600     END-IF                                                               
047610                                                                          
047700     IF WS-PFI-TIRFS-01 = 0                                               
047800        MOVE WS-LOCAL-DATE TO WS-PFI-TIRFS-01                             
047900     END-IF                                                               
047910                                                                          
048000     IF WS-PFI-TIRFS-02 = 0                                               
048100        MOVE WS-LOCAL-DATE TO WS-PFI-TIRFS-02                             
048200     END-IF                                                               
048210                                                                          
048211     MOVE WS-PFI-TIRFS-01   TO TMP1-YYMMDD                                
048212     MOVE WS-PFI-TIRFS-02   TO TMP2-YYMMDD                                
048220     PERFORM WY2000P1                                                     
048300     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
048400        MOVE NEJ TO NYCKLAR-SW                                            
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BF-CHCK-KDKALK SECTION.                                                  
048900                                                                          
049000     IF MID-PFI-KDKALK = ALL '+'                                          
049100       IF MID-PF7-KDKALK IS NUMERIC                                       
049200          MOVE MID-PF7-KDKALK TO WS-PFI-KDKALK                            
049300       ELSE                                                               
049400          MOVE SPRAK-IX TO WS-PFI-KDKALK                                  
049500       END-IF                                                             
049600     ELSE                                                                 
049700       IF MID-PFI-KDKALK IS NUMERIC                                       
049800          MOVE MID-PFI-KDKALK TO WS-PFI-KDKALK                            
049900       ELSE                                                               
050000          MOVE SPRAK-IX TO WS-PFI-KDKALK                                  
050100       END-IF                                                             
050200       MOVE '7'          TO MFS-IDPFK                                     
050300       MOVE SPACE        TO MFS-KDTRTYP                                   
050400     END-IF                                                               
050500     IF NOT GOOD-PFI-KDKALK                                               
050600       MOVE NEJ TO NYCKLAR-SW                                             
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 BG-CHCK-COMBIN SECTION.                                                  
051100                                                                          
051200     IF (WS-PFI-KDPRODKL-01 NOT = ' ')  AND                               
051300                              (WS-PFI-KDPRODKL-02 = ' ')                  
051400        MOVE WS-PFI-KDPRODKL-01 TO WS-PFI-KDPRODKL-02                     
051500     END-IF                                                               
051600     IF (WS-PFI-KDPRODKL-01 = ' ')  AND                                   
051700                             (WS-PFI-KDPRODKL-02 NOT = ' ')               
051800        MOVE WS-PFI-KDPRODKL-02 TO WS-PFI-KDPRODKL-01                     
051900     END-IF                                                               
052000     IF (WS-PFI-KDPRODKL-01 = ' ')  AND                                   
052100                             (WS-PFI-KDPRODKL-02 = ' ')                   
052200        MOVE 'B' TO WS-PFI-KDPRODKL-01                                    
052300        MOVE 'C' TO WS-PFI-KDPRODKL-02                                    
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 BH-CNT-WRKDAYS SECTION.                                                  
052800                                                                          
052900     MOVE 001             TO WORK-KDCALL                                  
052910     MOVE WS-IDDC         TO WORK-IDDC                                    
053000     MOVE WS-PFI-TIRFS-01 TO WORK-TIAAMMDD-FOM                            
053100     MOVE WS-PFI-TIRFS-02 TO WORK-TIAAMMDD-TOM                            
053200     CALL WORKDAY  USING WORK-KDCALL                                      
053300                         WORK-DATE-AREA                                   
053400                         WORK-KDSVAR                                      
053500     IF (WORK-KDSVAR-FEL)                                                 
053600        MOVE NEJ TO NYCKLAR-SW                                            
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
054030 BZ-FILL-MOD-PF7 SECTION.                                                 
054100                                                                          
054110     IF NOT GODK-MID                                                      
054120        MOVE NEJ TO NYCKLAR-SW                                            
054130     END-IF                                                               
054140                                                                          
054150     MOVE WS-IDDC              TO MOD-PF7-IDDC                            
054160                                                                          
054200     IF GODK-MID OR NYCKLAR-OK                                            
054300       MOVE WS-PFI-KDPRCGRP    TO MOD-PF7-KDPRCGRP                        
054400       MOVE WS-PFI-KDPRODKL-01 TO MOD-PF7-KDPRODKL-01                     
054500       MOVE WS-PFI-KDPRODKL-02 TO MOD-PF7-KDPRODKL-02                     
054600       MOVE WS-PFI-IDPRC       TO MOD-PF7-IDPRC                           
054720       MOVE WS-PFI-TIRFS-01    TO MOD-PF7-TIRFS-01                        
054732       MOVE WS-PFI-TIRFS-02    TO MOD-PF7-TIRFS-02                        
054900       MOVE WS-PFI-KDKALK      TO MOD-PF7-KDKALK                          
055000     ELSE                                                                 
055100       MOVE MFS-RENSA-FAELT    TO MOD-PF7-KDPRCGRP                        
055200                                  MOD-PF7-KDPRODKL-01                     
055300                                  MOD-PF7-KDPRODKL-02                     
055400       MOVE MFS-RENSA-FAELT    TO MOD-PF7-IDPRC                           
055500                                  MOD-PF7-TIRFS-01                        
055600                                  MOD-PF7-TIRFS-02                        
055700                                  MOD-PF7-KDKALK                          
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 C-FOERSTA-SIDA SECTION.                                                  
056200                                                                          
056300     MOVE '006' TO MED-IDMFSFEL                                           
056400     CALL WMEDKONV USING MED-WMEDAREA                                     
056500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
056600     PERFORM MFS-ERASE-MOD-PFI                                            
056700     PERFORM MFS-RENSA-FAELT-UT                                           
056800     MOVE WS-PFI-IDPRC TO W-4448-IDPRC                                    
056900     MOVE JA TO ALLT-SW                                                   
057000     .                                                                    
057100     EJECT                                                                
057200 D-NAESTA-SIDA SECTION.                                                   
057300                                                                          
057400     MOVE MID-PF8-IDPRC TO W-4448-IDPRC                                   
057500     MOVE JA TO ALLT-SW                                                   
057510                                                                          
057520     IF W-4448-IDPRC = 'SLUT'                                             
057530        MOVE '115' TO MED-IDMFSFEL                                        
057540        CALL WMEDKONV USING MED-WMEDAREA                                  
057550        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
057560        PERFORM MFS-RENSA-FAELT-UT                                        
057570        MOVE 'SLUT'     TO MOD-PF8-IDPRC                                  
057580        MOVE NEJ TO ALLT-SW                                               
057590     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057900 E-SAMMA-SIDA SECTION.                                                    
058000                                                                          
058100     IF (MID-PFI-KDPRCGRP = ALL '+') AND                                  
058200            (MID-PFI-KDPRODKL-01 = ALL '+') AND                           
058300            (MID-PFI-KDPRODKL-02 = ALL '+') AND                           
058400                (MID-PFI-IDPRC = ALL '+')                                 
058500       MOVE MID-PFE-IDPRC    TO W-4448-IDPRC                              
058600       MOVE JA  TO ALLT-SW                                                
058700     ELSE                                                                 
058800       MOVE NEJ TO ALLT-SW                                                
058900       MOVE '003' TO MED-IDMFSFEL                                         
059000       CALL WMEDKONV USING MED-WMEDAREA                                   
059100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059200       PERFORM MFS-ROR-EJ-FAELT-IN                                        
059300       PERFORM MFS-ROR-EJ-FAELT-UT                                        
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 F-LAES-VISA-INFO SECTION.                                                
059800                                                                          
059900     PERFORM FA-INIT-WSDATA                                               
060000     PERFORM FB-LOC-WDGX4448                                              
060100     MOVE STATUS-WS            TO 4448-STATUS-WS                          
060200     IF SEGMENT-GE                                                        
060300        MOVE '413' TO MED-IDMFSFEL                                        
060400        CALL WMEDKONV USING MED-WMEDAREA                                  
060500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
060600        PERFORM MFS-RENSA-FAELT-UT                                        
060700     ELSE                                                                 
060800       MOVE XXKH-4448-IDPRC    TO MOD-PFE-IDPRC                           
060900                                  MOD-PF8-IDPRC                           
061000       MOVE +1                  TO INDX                                   
061100       PERFORM UNTIL (INDX > MAX-INDX) OR (NOT 4448-SEGMENT-OK)           
061200         PERFORM FY-CMPT-SCRNLINE                                         
061300       END-PERFORM                                                        
061400       IF 4448-SEGMENT-OK                                                 
061500          MOVE '105' TO MED-IDMFSINF                                      
061600          CALL WMEDKONV USING MED-WMEDAREA                                
061700          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
061800          MOVE XXKH-4448-IDPRC    TO MOD-PF8-IDPRC                        
061810        ELSE                                                              
061820          MOVE '106' TO MED-IDMFSINF                                      
061830          CALL WMEDKONV USING MED-WMEDAREA                                
061840          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
061850          MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                            
061860          MOVE 'SLUT'       TO MOD-PF8-IDPRC                              
061900       END-IF                                                             
062000       IF MFS-FIRST                                                       
062100          PERFORM UNTIL (NOT 4448-SEGMENT-OK)                             
062200              PERFORM FY-CMPT-SCRNLINE                                    
062300          END-PERFORM                                                     
062400          PERFORM FZ-TOTAL-LINE                                           
062500       ELSE                                                               
062600          PERFORM MFS-KEEP-TOTAL-LINE                                     
062700       END-IF                                                             
062800     END-IF                                                               
062900     PERFORM UNTIL (INDX > MAX-INDX)                                      
063000       PERFORM MFS-ERASE-LINE                                             
063100       ADD +1 TO INDX                                                     
063200     END-PERFORM                                                          
063300     .                                                                    
063400     EJECT                                                                
063500 FA-INIT-WSDATA SECTION.                                                  
063600                                                                          
063700     MOVE WS-IDDC         TO W-4447-IDDC                                  
063800                             W-4471-IDDC                                  
063900                             W-WDQ3C1KY-MIN-IDDC                          
064000                             W-WDQ3C1KY-MAX-IDDC                          
064100     MOVE WS-PFI-TIRFS-01 TO WS-DB-TIRFS-AAMMDD                           
064200     MOVE 0000            TO WS-DB-TIRFS-HHMM                             
064300     MOVE WS-DB-TIRFS     TO WS-TIRFS01                                   
064400     MOVE WS-PFI-TIRFS-02 TO WS-DB-TIRFS-AAMMDD                           
064500     MOVE 2359            TO WS-DB-TIRFS-HHMM                             
064600     MOVE WS-DB-TIRFS     TO WS-TIRFS02                                   
064700     .                                                                    
064800     EJECT                                                                
064900 FB-LOC-WDGX4448 SECTION.                                                 
065000     IF WS-PFI-IDPRC NOT = SPACE                                          
065100        PERFORM IMS-GU-XXKH-WDGX4448                                      
065200        IF SEGMENT-OK                                                     
065300           IF XXKH-4448-KDPRODKL = 'B' OR 'C'                             
065400              CONTINUE                                                    
065500           ELSE                                                           
065600              MOVE 'GE' TO STATUS-WS                                      
065700           END-IF                                                         
065800        END-IF                                                            
065900     ELSE                                                                 
066000        PERFORM IMS-GU-XXKH-WDGX4447                                      
066100        IF SEGMENT-OK                                                     
066200           IF MFS-FIRST                                                   
066300              PERFORM IMS-GNP-XXKH-WDGX4448                               
066400           ELSE                                                           
066500              PERFORM IMS-LST-XXKH-WDGX4448                               
066600           END-IF                                                         
066700           PERFORM S00-GET-WDGX4448                                       
066800        END-IF                                                            
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 FY-CMPT-SCRNLINE SECTION.                                                
067300                                                                          
067400     MOVE +0                    TO WS-KVARBTID                            
067500                                   WS-KVBEMAN                             
067700     MOVE XXKH-4448-IDPRC       TO WS-FIRST-IDPRC                         
067900     MOVE NEJ                   TO RAD-INFO-SW                            
068000     MOVE KVRADER-SHFT-TIRFS-00 TO KVRADER-SHFT-TIRFS                     
068010     MOVE +0                    TO XX00-ORQC-KVRADER-U                    
068020                                   XX00-ORQC-KVRADER-TIRFS-U              
068030                                   XX00-ORQC-KVRADER-R                    
068040                                   XX00-ORQC-KVRADER-TIRFS-R              
068100                                                                          
068200     PERFORM FYA-INIT-XX00-4472                                           
068600                                                                          
069100     PERFORM FYB-SUM-XXKW-WDGX4472                                        
069200     IF WS-PFI-IDPRC           NOT = SPACES                               
069300         MOVE 'GE'             TO  STATUS-WS                              
069400      ELSE                                                                
069500         PERFORM IMS-GNP-XXKH-WDGX4448                                    
069600         PERFORM S00-GET-WDGX4448                                         
069700     END-IF                                                               
069800     MOVE STATUS-WS            TO 4448-STATUS-WS                          
070000                                                                          
070100     IF RAD-INFO-OK                                                       
070200         PERFORM FYC-KVRADER-RFS                                          
070300         PERFORM FYD-SUM-ORQC-WDQ3C1                                      
070400         PERFORM FYE-DSPL-SCRN-LINE                                       
070500         IF INDX > MAX-INDX                                               
070600             CONTINUE                                                     
070700          ELSE                                                            
070800             PERFORM FYF-MOVE-W-RAD                                       
070900             ADD +1            TO INDX                                    
071000             PERFORM FYG-INIT-W-RAD                                       
071100         END-IF                                                           
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 FYA-INIT-XX00-4472 SECTION.                                              
071600                                                                          
071700     MOVE +0   TO  XX00-4472-KVRADER-DAG                                  
071800     MOVE +0   TO  XX00-4472-KVRADER-RFS                                  
071900     MOVE +0   TO  XX00-4472-KVRADER-PRAPP                                
072000     MOVE +1 TO RS-INDX                                                   
072100     PERFORM UNTIL RS-INDX > 3                                            
072200       MOVE +0 TO XX00-4472-KVRADER-SHFT-ALL (RS-INDX)                    
072300       ADD +1 TO RS-INDX                                                  
072400     END-PERFORM                                                          
072500     MOVE +1 TO RT-INDX                                                   
072600     PERFORM UNTIL RT-INDX > 30                                           
072700       MOVE +0  TO  XX00-4472-TIRFS (RT-INDX)                             
072800       MOVE +0  TO  XX00-4472-KVRADER (RT-INDX)                           
072900       MOVE +1 TO RS-INDX                                                 
073000       PERFORM UNTIL RS-INDX > 3                                          
073100         MOVE +0 TO XX00-4472-KVRADER-SHFT (RT-INDX, RS-INDX)             
073200         ADD +1 TO RS-INDX                                                
073300       END-PERFORM                                                        
073400       ADD +1 TO RT-INDX                                                  
073500     END-PERFORM                                                          
073700     MOVE +0 TO RT-INDX                                                   
073710     MOVE +0 TO RS-INDX                                                   
073800     .                                                                    
073900     EJECT                                                                
074000 FYB-SUM-XXKW-WDGX4472 SECTION.                                           
074100                                                                          
074200     MOVE XXKH-4448-IDPRC TO W-4471-IDPRC                                 
074300     MOVE XXKH-4448-IDPRC TO W-4448-IDPRC                                 
074400     IF WS-PFI-IDPRC = SPACES                                             
074500         PERFORM IMS-GU-XXKW-WDGX4472                                     
074600         IF SEGMENT-OK                                                    
074700            PERFORM FYBB-TOTAL-WDGX4472                                   
074800            PERFORM FYBA-DATA-WDGX4448                                    
074900         END-IF                                                           
075000     ELSE                                                                 
075100        PERFORM IMS-GU-XXKW-WDGX4472                                      
075200        IF SEGMENT-OK                                                     
075300           PERFORM FYBB-TOTAL-WDGX4472                                    
075310        END-IF                                                            
075400        PERFORM FYBA-DATA-WDGX4448                                        
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 FYBA-DATA-WDGX4448 SECTION.                                              
076000                                                                          
076100     MOVE XXKH-4448-KVBEMAN-ORD TO WS-KVBEMAN                             
076200     ADD  XXKH-4448-KVBEMAN-EXT TO WS-KVBEMAN                             
076300     COMPUTE WS-KVARBTID =                                                
076400          WS-KVARBTID + (XXKH-4448-KVARBTID * WS-KVBEMAN)                 
076500     END-COMPUTE                                                          
076600     .                                                                    
076700     EJECT                                                                
076800 FYBB-TOTAL-WDGX4472 SECTION.                                             
076900                                                                          
077000     MOVE JA                    TO RAD-INFO-SW                            
077100     ADD  XXKW-4472-KVRADER-DAG TO XX00-4472-KVRADER-DAG                  
077200     MOVE +1 TO RS-INDX                                                   
077300     PERFORM UNTIL (RS-INDX > RS-INDX-MAX)                                
077400        IF XXKW-4472-TIRFS  (RS-INDX) > +0                                
077500           ADD  XXKW-4472-KVRADER (RS-INDX) TO                            
077600                         XX00-4472-KVRADER-RFS                            
077700           PERFORM FYBBA-LOC-INDX                                         
077800           ADD  XXKW-4472-KVRADER (RS-INDX) TO                            
077900                         XX00-4472-KVRADER (RW-INDX)                      
078000           MOVE XXKW-4472-TIRFS (RS-INDX) TO                              
078100                         XX00-4472-TIRFS (RW-INDX)                        
078200           PERFORM FYBBB-SUM-SHFT                                         
078300        END-IF                                                            
078400        ADD +1 TO RS-INDX                                                 
078500     END-PERFORM                                                          
078600     .                                                                    
078700     EJECT                                                                
078800 FYBBA-LOC-INDX SECTION.                                                  
078900*                                                                         
079000     MOVE +1 TO RW-INDX                                                   
079100     PERFORM UNTIL                                                        
079200               XXKW-4472-TIRFS (RS-INDX) =                                
079300               XX00-4472-TIRFS (RW-INDX)         OR                       
079400               XX00-4472-TIRFS (RW-INDX) = ZERO  OR                       
079500               RW-INDX                   > 29                             
079600         ADD +1 TO RW-INDX                                                
079700     END-PERFORM                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 FYBBB-SUM-SHFT SECTION.                                                  
080100                                                                          
080200     MOVE +1 TO RT-INDX                                                   
080300     PERFORM UNTIL RT-INDX > 3                                            
080400         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
080500                TO  XX00-4472-KVRADER-SHFT (RW-INDX, RT-INDX)             
080600         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
080700                TO  XX00-4472-KVRADER-SHFT-ALL (RT-INDX)                  
080800         ADD  XXKW-4472-KVRADER-PRAPP (RS-INDX, RT-INDX)                  
080900                TO  XX00-4472-KVRADER-PRAPP                               
081000         ADD +1 TO RT-INDX                                                
081100     END-PERFORM                                                          
081200     .                                                                    
081300     EJECT                                                                
081400 FYC-KVRADER-RFS SECTION.                                                 
081500                                                                          
081600     MOVE +1 TO RS-INDX                                                   
081700     PERFORM UNTIL RS-INDX > RS-INDX-MAX                                  
081701       MOVE XX00-4472-TIRFS (RS-INDX)  TO TMP1-YYMMDDHHMM                 
081702       MOVE WS-TIRFS01                 TO TMP2-YYMMDDHHMM                 
081703       MOVE WS-TIRFS02                 TO TMP3-YYMMDDHHMM                 
081710       PERFORM WY2000QB                                                   
081800       IF  TMP1-YYMMDDHHMM  >=    TMP2-YYMMDDHHMM                         
081810       AND TMP1-YYMMDDHHMM  <=    TMP3-YYMMDDHHMM                         
082000*         ADD XX00-4472-KVRADER (RS-INDX) TO WU-KVRADER                   
082100          MOVE +1 TO RT-INDX                                              
082200          PERFORM UNTIL RT-INDX > 3                                       
082300             ADD XX00-4472-KVRADER-SHFT (RS-INDX, RT-INDX) TO             
082400                           WU-KVRADER-SHFT (RT-INDX)                      
082500             ADD +1 TO RT-INDX                                            
082600          END-PERFORM                                                     
082700       END-IF                                                             
082800       ADD  +1 TO RS-INDX                                                 
082900     END-PERFORM                                                          
083000     .                                                                    
083100     EJECT                                                                
083200 FYD-SUM-ORQC-WDQ3C1 SECTION.                                             
083300                                                                          
083400     MOVE WS-FIRST-IDPRC TO W-WDQ3C1KY-MIN-IDPRCBAS                       
083500                            W-WDQ3C1KY-MAX-IDPRCBAS                       
083510     MOVE WS-FIRST-IDPRC (4:1) TO W-WDQ3C1KY-MIN-IDPRCVAR                 
083520                                  W-WDQ3C1KY-MAX-IDPRCVAR                 
083600     PERFORM IMS-GU-ORQD-WDQ3C1                                           
083700     PERFORM UNTIL (NOT SEGMENT-OK)                                       
083710       IF ORQD-SEQC-KDODELSTA = 'U'                                       
083800          ADD  ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-U                   
083810       ELSE                                                               
083820          ADD  ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-R                   
083830       END-IF                                                             
083831        MOVE ORQD-SEQC-DARFS (3:10)   TO TMP1-YYMMDDHHMM                  
083832        MOVE WS-TIRFS01        TO TMP2-YYMMDDHHMM                         
083833        MOVE WS-TIRFS02        TO TMP3-YYMMDDHHMM                         
083840        PERFORM WY2000QB                                                  
083850        IF  TMP1-YYMMDDHHMM  >=    TMP2-YYMMDDHHMM                        
083860        AND TMP1-YYMMDDHHMM  <=    TMP3-YYMMDDHHMM                        
084010          IF ORQD-SEQC-KDODELSTA = 'U'                                    
084100             ADD ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-TIRFS-U           
084110          ELSE                                                            
084120             ADD ORQD-SEQC-KVRADER TO XX00-ORQC-KVRADER-TIRFS-R           
084130          END-IF                                                          
084200        END-IF                                                            
084300        PERFORM IMS-GN-ORQD-WDQ3C1                                        
084400     END-PERFORM                                                          
084500     .                                                                    
084600     EJECT                                                                
087200 FYE-DSPL-SCRN-LINE SECTION.                                              
087300                                                                          
087400     PERFORM FYEA-COLUM-123                                               
087500     PERFORM FYEB-COLUM-456                                               
087600     PERFORM FYEC-COLUM-78                                                
087700     PERFORM FYED-COLUM-9AB                                               
087800     .                                                                    
087900     EJECT                                                                
088000 FYEA-COLUM-123 SECTION.                                                  
088100                                                                          
088200     MOVE W-4471-IDPRC          TO W-IDPRC-RAD                            
088300     MOVE XX00-4472-KVRADER-RFS TO W-KVRADER-RAD                          
088400     MOVE XX00-4472-KVRADER-DAG TO W-KVRADER-DAG-RAD                      
088500     ADD  XX00-4472-KVRADER-RFS TO WS-KVRADER-TOT (02)                    
088600     ADD  XX00-4472-KVRADER-DAG TO WS-KVRADER-TOT (03)                    
088700     .                                                                    
088800     EJECT                                                                
088900 FYEB-COLUM-456 SECTION.                                                  
089000                                                                          
089100     MOVE XX00-4472-KVRADER-DAG TO WS-KVRADER-CL4                         
089200     IF WS-PFI-KDKALK = 1                                                 
089300        SUBTRACT XX00-ORQC-KVRADER-TIRFS-U FROM WS-KVRADER-CL4            
089310        SUBTRACT XX00-4472-KVRADER-PRAPP   FROM WS-KVRADER-CL4            
089400     ELSE                                                                 
089500        SUBTRACT XX00-ORQC-KVRADER-U       FROM WS-KVRADER-CL4            
089600        SUBTRACT XX00-4472-KVRADER-PRAPP   FROM WS-KVRADER-CL4            
089800     END-IF                                                               
089810     IF WS-KVRADER-CL4   < ZERO                                           
089820         MOVE ZERO       TO WS-KVRADER-CL4                                
089830     END-IF                                                               
089900     MOVE WS-KVRADER-CL4 TO W-KVRADER-RST-RAD                             
090000     ADD  WS-KVRADER-CL4 TO WS-KVRADER-TOT (04)                           
090100     IF WS-PFI-KDKALK = 1                                                 
090101        MOVE XX00-ORQC-KVRADER-TIRFS-R TO WS-KVRADER-REG-RAD              
090110*       COMPUTE WS-KVRADER-REG-RAD = WU-KVRADER               -           
090120*                                    XX00-4472-KVRADER-PRAPP  -           
090130*                                    XX00-ORQC-KVRADER-TIRFS-U            
090140        IF WS-KVRADER-REG-RAD < 0                                         
090150           MOVE 0                 TO WS-KVRADER-REG-RAD                   
090160        END-IF                                                            
090170        MOVE WS-KVRADER-REG-RAD   TO W-KVRADER-REG-RAD                    
090180        ADD  WS-KVRADER-REG-RAD   TO WS-KVRADER-TOT (05)                  
090400     ELSE                                                                 
090401        MOVE XX00-ORQC-KVRADER-R       TO WS-KVRADER-REG-RAD              
090410*       COMPUTE WS-KVRADER-REG-RAD = XX00-4472-KVRADER-RFS    -           
090420*                                    XX00-4472-KVRADER-PRAPP  -           
090430*                                    XX00-ORQC-KVRADER-U                  
090440        IF WS-KVRADER-REG-RAD < 0                                         
090450           MOVE 0                 TO WS-KVRADER-REG-RAD                   
090460        END-IF                                                            
090470        MOVE WS-KVRADER-REG-RAD   TO W-KVRADER-REG-RAD                    
090480        ADD  WS-KVRADER-REG-RAD   TO WS-KVRADER-TOT (05)                  
090900     END-IF                                                               
091000     IF WS-PFI-KDKALK = 1                                                 
091100       MOVE XX00-ORQC-KVRADER-TIRFS-U  TO W-KVRADER-UT-RAD                
091200       ADD  XX00-ORQC-KVRADER-TIRFS-U  TO WS-KVRADER-TOT (06)             
091300     ELSE                                                                 
091400       MOVE XX00-ORQC-KVRADER-U        TO W-KVRADER-UT-RAD                
091500       ADD  XX00-ORQC-KVRADER-U        TO WS-KVRADER-TOT (06)             
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 FYEC-COLUM-78 SECTION.                                                   
092000                                                                          
092100     COMPUTE WS-KVRADER-CL7 =                                             
092200                     WS-KVARBTID * WORK-KVWORKD                           
092300     END-COMPUTE                                                          
092400     MOVE WS-KVRADER-CL7    TO   W-KVRADER-KAN-RAD                        
092500     ADD  WS-KVRADER-CL7    TO   WS-KVRADER-TOT (07)                      
092600                                                                          
092800     MOVE 0                 TO WS-KVRADER-CL8                             
093700     MOVE WS-KVRADER-CL8    TO W-KVRADER-PLMI-RAD                         
093800     ADD  WS-KVRADER-CL8    TO WS-KVRADER-TOT (08)                        
093900     .                                                                    
094000     EJECT                                                                
094100 FYED-COLUM-9AB SECTION.                                                  
094200                                                                          
094300     IF WS-PFI-KDKALK = 1                                                 
094400        PERFORM FYEDA-COLUM-9AB-KALK1                                     
094500     ELSE                                                                 
094600        PERFORM FYEDB-COLUM-9AB-KALKX                                     
094700     END-IF                                                               
094800     .                                                                    
094900     EJECT                                                                
095000 FYEDA-COLUM-9AB-KALK1 SECTION.                                           
095100                                                                          
095200     MOVE WU-KVRADER-SHFT (1)                                             
095300                          TO W-KVRADER-META-RAD                           
095400     MOVE WU-KVRADER-SHFT (2)                                             
095500                          TO W-KVRADER-METB-RAD                           
095600     MOVE WU-KVRADER-SHFT (3)                                             
095700                          TO W-KVRADER-METC-RAD                           
095800     ADD  WU-KVRADER-SHFT (1) TO WS-KVRADER-TOT (9)                       
095900     ADD  WU-KVRADER-SHFT (2) TO WS-KVRADER-TOT (10)                      
096000     ADD  WU-KVRADER-SHFT (3) TO WS-KVRADER-TOT (11)                      
096100     .                                                                    
096200     EJECT                                                                
096300 FYEDB-COLUM-9AB-KALKX SECTION.                                           
096400                                                                          
096500     MOVE XX00-4472-KVRADER-SHFT-ALL (1)                                  
096600                          TO W-KVRADER-META-RAD                           
096700     MOVE XX00-4472-KVRADER-SHFT-ALL (2)                                  
096800                          TO W-KVRADER-METB-RAD                           
096900     MOVE XX00-4472-KVRADER-SHFT-ALL (3)                                  
097000                          TO W-KVRADER-METC-RAD                           
097100     ADD  XX00-4472-KVRADER-SHFT-ALL (1)                                  
097200                              TO WS-KVRADER-TOT (9)                       
097300     ADD  XX00-4472-KVRADER-SHFT-ALL (2)                                  
097400                              TO WS-KVRADER-TOT (10)                      
097500     ADD  XX00-4472-KVRADER-SHFT-ALL (3)                                  
097600                              TO WS-KVRADER-TOT (11)                      
097700     .                                                                    
097800     EJECT                                                                
097900 FYF-MOVE-W-RAD      SECTION.                                             
098000                                                                          
098100     MOVE W-IDPRC-RAD          TO MOD-IDPRC-RAD         (INDX)            
098200     MOVE W-KVRADER-RAD        TO MOD-KVRADER-RAD       (INDX)            
098300     MOVE W-KVRADER-DAG-RAD    TO MOD-KVRADER-DAG-RAD   (INDX)            
098400     MOVE W-KVRADER-RST-RAD    TO MOD-KVRADER-RST-RAD   (INDX)            
098500     MOVE W-KVRADER-REG-RAD    TO MOD-KVRADER-REG-RAD   (INDX)            
098600     MOVE W-KVRADER-UT-RAD     TO MOD-KVRADER-UT-RAD    (INDX)            
098700     MOVE W-KVRADER-KAN-RAD    TO MOD-KVRADER-KAN-RAD   (INDX)            
098800     MOVE W-KVRADER-PLMI-RAD   TO MOD-KVRADER-PLMI-RAD  (INDX)            
098900     MOVE W-KVRADER-META-RAD   TO MOD-KVRADER-META-RAD  (INDX)            
099000     MOVE W-KVRADER-METB-RAD   TO MOD-KVRADER-METB-RAD  (INDX)            
099100     MOVE W-KVRADER-METC-RAD   TO MOD-KVRADER-METC-RAD  (INDX)            
099200     .                                                                    
099300     EJECT                                                                
099400 FYG-INIT-W-RAD      SECTION.                                             
099500                                                                          
099600     MOVE ZERO                 TO W-IDPRC-RAD                             
099700                                  W-KVRADER-RAD                           
099800                                  W-KVRADER-DAG-RAD                       
099900                                  W-KVRADER-RST-RAD                       
100000                                  W-KVRADER-REG-RAD                       
100100                                  W-KVRADER-UT-RAD                        
100200                                  W-KVRADER-KAN-RAD                       
100300                                  W-KVRADER-PLMI-RAD                      
100400                                  W-KVRADER-META-RAD                      
100500                                  W-KVRADER-METB-RAD                      
100600                                  W-KVRADER-METC-RAD                      
100700     .                                                                    
100800     EJECT                                                                
100900 FZ-TOTAL-LINE SECTION.                                                   
101100                                                                          
101200       MOVE WS-KVRADER-TOT (2)  TO  MOD-KVRADER-TOT                       
101210       MOVE WS-KVRADER-TOT (3)  TO  MOD-KVRADER-DAG-TOT                   
101220       MOVE WS-KVRADER-TOT (4)  TO  MOD-KVRADER-RST-TOT                   
101300       MOVE WS-KVRADER-TOT (5)  TO  MOD-KVRADER-REG-TOT                   
101400       MOVE WS-KVRADER-TOT (6)  TO  MOD-KVRADER-UT-TOT                    
101500       MOVE WS-KVRADER-TOT (7)  TO  MOD-KVRADER-KAN-TOT                   
101600       MOVE WS-KVRADER-TOT (8)  TO  MOD-KVRADER-PLMI-TOT                  
101700       MOVE WS-KVRADER-TOT (9)  TO  MOD-KVRADER-META-TOT                  
101800       MOVE WS-KVRADER-TOT (10) TO  MOD-KVRADER-METB-TOT                  
101900       MOVE WS-KVRADER-TOT (11) TO  MOD-KVRADER-METC-TOT                  
102000       IF WS-KVRADER-TOT (2) = +0                                         
102100          IF WS-KVRADER-TOT (3) = +0                                      
102200             IF WS-KVRADER-TOT (4) = +0                                   
102300                IF WS-KVRADER-TOT (5) = +0                                
102400                   MOVE WS-KVRADER-TOT (7) TO                             
102500                                    MOD-KVRADER-PLMI-TOT                  
102600                END-IF                                                    
102700             END-IF                                                       
102800          END-IF                                                          
102900       END-IF                                                             
103000       .                                                                  
103100    EJECT                                                                 
103200 S00-GET-WDGX4448 SECTION.                                                
103300          PERFORM UNTIL                                                   
103400              (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-01)                   
103500         OR   (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-02)                   
103600         OR   (SEGMENT-GE)                                                
103700                 PERFORM IMS-GNP-XXKH-WDGX4448                            
103800         END-PERFORM                                                      
103900     .                                                                    
104095     SKIP2                                                                
104100 MFS-ERASE-MOD-PFI SECTION.                                               
104110                                                                          
104200     MOVE MFS-RENSA-FAELT TO MOD-PFI-KDPRCGRP                             
104300                             MOD-PFI-KDPRODKL-01                          
104400                             MOD-PFI-KDPRODKL-02                          
104500     MOVE MFS-RENSA-FAELT TO MOD-PFI-IDPRC                                
104600                             MOD-PFI-TIRFS-01                             
104700                             MOD-PFI-TIRFS-02                             
104800                             MOD-PFI-KDKALK                               
104810                             MOD-PFI-IDDC                                 
104900     .                                                                    
105000     EJECT                                                                
105100 MFS-RENSA-FAELT-UT SECTION.                                              
105200                                                                          
105300     MOVE +1 TO INDX                                                      
105400     PERFORM UNTIL INDX > MAX-INDX                                        
105500        PERFORM MFS-ERASE-LINE                                            
105600        ADD +1 TO INDX                                                    
105700     END-PERFORM                                                          
105800     MOVE MFS-RENSA-FAELT TO MOD-PFE-IDPRC                                
105900                             MOD-PF8-IDPRC                                
106000     .                                                                    
106100     EJECT                                                                
106200 MFS-ERASE-LINE SECTION.                                                  
106210                                                                          
106300           MOVE MFS-RENSA-FAELT TO MOD-IDPRC-RAD (INDX)                   
106400                                   MOD-KVRADER-RAD (INDX)                 
106500                                   MOD-KVRADER-DAG-RAD (INDX)             
106600                                   MOD-KVRADER-RST-RAD (INDX)             
106700                                   MOD-KVRADER-REG-RAD (INDX)             
106800                                   MOD-KVRADER-UT-RAD (INDX)              
106900                                   MOD-KVRADER-KAN-RAD (INDX)             
107000           MOVE MFS-RENSA-FAELT TO MOD-KVRADER-PLMI-RAD (INDX)            
107100                                   MOD-KVRADER-META-RAD (INDX)            
107200                                   MOD-KVRADER-METB-RAD (INDX)            
107300                                   MOD-KVRADER-METC-RAD (INDX)            
107400     .                                                                    
107500     EJECT                                                                
107600 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
107700                                                                          
107800     MOVE MFS-ROER-EJ-FAELT TO MOD-PF7-KDPRCGRP                           
107900                               MOD-PF7-KDPRODKL-01                        
108000                               MOD-PF7-KDPRODKL-02                        
108100                               MOD-PF7-IDPRC                              
108200                               MOD-PF7-TIRFS-01                           
108300                               MOD-PF7-TIRFS-02                           
108400                               MOD-PF7-KDKALK                             
108500                               MOD-PFE-IDPRC                              
108600                               MOD-PF8-IDPRC                              
108700*                                                                         
108800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
108900     MOVE +1 TO INDX                                                      
109000     PERFORM UNTIL INDX > MAX-INDX                                        
109100        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRC-RAD (INDX)                   
109200                                   MOD-KVRADER-RAD (INDX)                 
109300                                   MOD-KVRADER-DAG-RAD (INDX)             
109400                                   MOD-KVRADER-RST-RAD (INDX)             
109500                                   MOD-KVRADER-REG-RAD (INDX)             
109600                                   MOD-KVRADER-UT-RAD (INDX)              
109700                                   MOD-KVRADER-KAN-RAD (INDX)             
109800       MOVE MFS-ROER-EJ-FAELT   TO MOD-KVRADER-PLMI-RAD (INDX)            
109900                                   MOD-KVRADER-META-RAD (INDX)            
110000                                   MOD-KVRADER-METB-RAD (INDX)            
110100                                   MOD-KVRADER-METC-RAD (INDX)            
110200       ADD +1 TO INDX                                                     
110300     END-PERFORM                                                          
110400     PERFORM MFS-KEEP-TOTAL-LINE                                          
110500     .                                                                    
110600     EJECT                                                                
110700 MFS-KEEP-TOTAL-LINE SECTION.                                             
110800                                                                          
110900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVRADER-TOT                       
111000                                    MOD-KVRADER-DAG-TOT                   
111100                                    MOD-KVRADER-RST-TOT                   
111200                                    MOD-KVRADER-REG-TOT                   
111300                                    MOD-KVRADER-UT-TOT                    
111400                                    MOD-KVRADER-KAN-TOT                   
111500                                    MOD-KVRADER-PLMI-TOT                  
111600                                    MOD-KVRADER-META-TOT                  
111700                                    MOD-KVRADER-METB-TOT                  
111800                                    MOD-KVRADER-METC-TOT                  
111900     .                                                                    
112000     EJECT                                                                
112100*                                                                         
112200 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
112300*                                                                         
112400*    --- ALLA INDATA-FÄLT                                                 
112500     MOVE MFS-ROER-EJ-FAELT TO MOD-PFI-KDPRCGRP                           
112600                               MOD-PFI-KDPRODKL-01                        
112700                               MOD-PFI-KDPRODKL-02                        
112800                               MOD-PFI-IDPRC                              
112900                               MOD-PFI-TIRFS-01                           
113000                               MOD-PFI-TIRFS-02                           
113100                               MOD-PFI-KDKALK                             
113200                               MOD-PF8-IDPRC                              
113300                               MOD-PFE-IDPRC                              
113400     .                                                                    
113500     EJECT                                                                
113600* --- IMS SECTION ---                                                     
113700     SKIP3                                                                
113800 IMS-GET-MSG SECTION.                                                     
113900                                                                          
114000     MOVE '  QC' TO GODK-STATUSKODER                                      
114100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
114200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     .                                                                    
114500     SKIP3                                                                
114600*                                                                         
114700 IMS-INSERT-MSG SECTION.                                                  
114800*                                                                         
114900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
115000       MOVE '0' TO MFS-KDHUVOMR                                           
115100     END-IF                                                               
115200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
115300     MOVE SPACE TO GODK-STATUSKODER                                       
115400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
115500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115600     PERFORM IMS-STATUSKONTROLL                                           
115700     .                                                                    
115800     EJECT                                                                
115900 IMS-GU-XXKH-WDGX4447 SECTION.                                            
116000*                                                                         
116100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
116200          DELIMITED BY SIZE INTO SSA1                                     
116300     MOVE '  GE' TO GODK-STATUSKODER                                      
116400     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA1 SSA1                     
116500     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116800*                                                                         
116900 IMS-GNP-XXKH-WDGX4448 SECTION.                                           
117000                                                                          
117100     STRING 'WLXXKH11(KDPRCGRP= ' WS-PFI-KDPRCGRP ')'                     
117200          DELIMITED BY SIZE INTO SSA1                                     
117300     MOVE '  GE' TO GODK-STATUSKODER                                      
117400     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
117500     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800*                                                                         
117900 IMS-GU-XXKH-WDGX4448 SECTION.                                            
118000                                                                          
118100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
118500          DELIMITED BY SIZE INTO SSA2                                     
118600     MOVE '  GE' TO GODK-STATUSKODER                                      
118700     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA1 SSA1 SSA2               
118800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
118900     PERFORM IMS-STATUSKONTROLL                                           
119000     .                                                                    
119100*                                                                         
119200 IMS-LST-XXKH-WDGX4448 SECTION.                                           
119300*                                                                         
119400     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY                          
119500                    '&KDPRCGRP =' WS-PFI-KDPRCGRP ')'                     
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     MOVE '  GE' TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
119900     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
120000     PERFORM IMS-STATUSKONTROLL                                           
120100     .                                                                    
120200     EJECT                                                                
120300 IMS-GU-XXKW-WDGX4472 SECTION.                                            
120400                                                                          
120500     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY ')'                      
120600          DELIMITED BY SIZE INTO SSA1                                     
120700     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY ')'                     
120800          DELIMITED BY SIZE INTO SSA2                                     
120900     MOVE '  GE' TO GODK-STATUSKODER                                      
121000     CALL CBLTDLI USING GU  XXKW-PCB DLI-IO-AREA2 SSA1 SSA2               
121100     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
121200     PERFORM IMS-STATUSKONTROLL                                           
121300     .                                                                    
121400     EJECT                                                                
121500 IMS-GU-ORQD-WDQ3C1 SECTION.                                              
121600*                                                                         
121700     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
121800                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX ')'                      
121900*                   '&KDODELST =' W-WDQ3C1KY-KDODELST-U ')'               
122000          DELIMITED BY SIZE INTO SSA1                                     
122100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
122200     CALL CBLTDLI USING GU  ORQD-PCB DLI-IO-AREA2 SSA1                    
122300     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122610 IMS-GN-ORQD-WDQ3C1 SECTION.                                              
122620*                                                                         
122630     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
122640                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX ')'                      
122650*                   '&KDODELST =' W-WDQ3C1KY-KDODELST-U ')'               
122670          DELIMITED BY SIZE INTO SSA1                                     
122680     MOVE '  GEGB' TO GODK-STATUSKODER                                    
122690     CALL CBLTDLI USING GN  ORQD-PCB DLI-IO-AREA2 SSA1                    
122691     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
122692     PERFORM IMS-STATUSKONTROLL                                           
122693     .                                                                    
122694     EJECT                                                                
122700 IMS-STATUSKONTROLL SECTION.                                              
122800                                                                          
122900     SET STATUS-IX TO 1                                                   
123000     SEARCH GODK-STATUS                                                   
123100       AT END CALL FELLOG                                                 
123200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
123300     END-SEARCH                                                           
123400     .                                                                    
123410     EJECT                                                                
123500*    -COPY WY2000P1                                                       
123600     EJECT                                                                
123700*    -COPY WY2000QB                                                       
