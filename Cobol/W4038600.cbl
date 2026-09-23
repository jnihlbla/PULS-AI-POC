000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038600.                                                
000400 AUTHOR.         CAO-VAN NGU.                                             
000500 DATE-WRITTEN.   90/05/08.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    DISPLAY  WDGX4471-72  SCREEN 4386                                    
001000*                         INPUTED VIA SCREEN 40386                        
001100*    LOOP (ON KDPRCGRP / IDPRC)                                           
001200*         READ SEQUENTIALLY WDGX4447-48 FOR 4448-IDPRCBAS                 
001300*         READ  WDGX4471-72 WITH KEY = 4448-IDPRCBAS                      
001400*         READ DATA FROM WDQ3 WITH SECONDARY INDEX WDQ2C                  
001500*         HANDLE DATA AND DISPLAY DATA                                    
001600*    END-LOOP                                                             
001700*        TRANSACTION: W4T386                                              
001800*        MID:         W4I38601                                            
001900*        MOD:         W4O38601                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600*    -COPY WY2000WB                                                       
002700     SKIP3                                                                
002800*    -COPY WY2000W1                                                       
002900     SKIP3                                                                
003000 77  IDPGM                       PIC X(08)   VALUE 'W4038600'.            
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500*    ---                                                                  
003600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003700 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003800                                                                          
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000                                                                          
004100*                                                                         
004200*          DATE  FROM DC-LOCAL                                            
004300*                                                                         
004400 01  FILLER.                                                              
004500     05  WS-LOCAL-DATE           PIC  9(06) VALUE 0.                      
004600 01      WS-TIRFS                PIC 9(11).                               
004700 01      FILLER REDEFINES WS-TIRFS.                                       
004800   03    FILLER                  PIC X(1).                                
004900   03    WS-RFS-DATE             PIC 9(6).                                
005000   03    WS-RFS-TIME             PIC X(4).                                
005100                                                                          
005200*    ---                                                                  
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006300     88  ALLT-OK                             VALUE 'J'.                   
006400                                                                          
006500 77  RAD-INFO-SW                 PIC X       VALUE 'J'.                   
006600     88  RAD-INFO-OK                         VALUE 'J'.                   
006700                                                                          
006800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006900     88  EGEN-MID                            VALUE '4386'.                
007000     88  GODK-MID                            VALUE '4386' '4387'.         
007100                                                                          
007200*                                                                         
007300 01  MID-INPUT.                                                           
007400     05  WS-PFI-KDPRCGRP             PIC X(5) VALUE SPACE.                
007500     05  WS-PFI-KDPRODKL-01          PIC X(1) VALUE SPACE.                
007600         88 GOOD-PFI-KDPRODKL-01       VALUE ' ' 'B' 'C'.                 
007700     05  WS-PFI-KDPRODKL-02          PIC X(1) VALUE SPACE.                
007800         88 GOOD-PFI-KDPRODKL-02       VALUE ' ' 'B' 'C'.                 
007900     05  WS-PFI-IDPRC.                                                    
008000         10  WS-PFI-IDPRCBAS         PIC X(3) VALUE SPACES.               
008100         10  WS-PFI-IDPRCVAR         PIC X(1) VALUE SPACE.                
008200     05  WS-PFI-TIRFS-01             PIC 9(6) VALUE 0.                    
008300     05  WS-PFI-TIRFS-02             PIC 9(6) VALUE 0.                    
008400     05  WS-PFI-KDKALK               PIC 9(1) VALUE 0.                    
008500         88 GOOD-PFI-KDKALK          VALUE 1 2.                           
008600*                                                                         
008700 01  WS-DCUSER.                                                           
008800     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
008900     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
009000     03 FILLER                   PIC X(1)   VALUE SPACE.                  
009100*      --- VALID IDDC CODES                                               
009200*                                                                         
009300*01    -COPY WWDC99                                                       
009400       EJECT                                                              
011100 01  REF-DATA.                                                            
011200     05  WS-SUPTID-REG-RAD       PIC S9(5)      COMP-3 VALUE +0.          
011300     05  WS-KVARBTID             PIC S9(2)V9(1) COMP-3 VALUE +0.          
011400     05  WS-KVBEMAN              PIC S9(2)V9(1) COMP-3 VALUE +0.          
011500     05  WS-TIRFS01              PIC S9(11)     COMP-3 VALUE +0.          
011600     05  WS-TIRFS02              PIC S9(11)     COMP-3 VALUE +0.          
011700*                                                                         
011800     05  WS-IDPRCVAR             PIC X(1) VALUE SPACES.                   
011900     05  WS-FIRST-IDPRC          PIC X(4) VALUE SPACES.                   
012000*                                                                         
012100 01  FILLER.                                                              
012200     05  WS-DB-TIRFS              PIC 9(11) VALUE 0.                      
012300     05  FILLER REDEFINES WS-DB-TIRFS.                                    
012400         10 WS-DB-TIRFS-AAMMDD    PIC 9(7).                               
012500         10 WS-DB-TIRFS-HHMM      PIC 9(4).                               
012600*                                                                         
012700*                                                                         
012800 01  TOTAL-TABLE.                                                         
012900     05  WS-SUPTID-TOT OCCURS 11 PIC S9(5) COMP-3 VALUE +0.               
013000     05  WS-SUPTID-TMM OCCURS 11 PIC S9(5) COMP-3 VALUE +0.               
013100     EJECT                                                                
013200*                                                                         
013300 01  XX00-4472.                                                           
013400         03  XX00-4472-SUPTID-DAG-HH    PIC S9(5) COMP-3.                 
013500         03  XX00-4472-SUPTID-DAG-MM    PIC S9(7) COMP-3.                 
013600         03  XX00-4472-SUPTID-RFS-HH    PIC S9(5) COMP-3.                 
013700         03  XX00-4472-SUPTID-RFS-MM    PIC S9(7) COMP-3.                 
013800         03  XX00-4472-SUPTID-SHFT-ALL-HH                                 
013900                                        PIC S9(5) COMP-3.                 
014000         03  XX00-4472-SUPTID-SHFT-ALL-MM                                 
014100                                        PIC S9(7) COMP-3.                 
014200         03  FILLER OCCURS 3.                                             
014300             07  XX00-4472-SUPTID-SHFT-HH-ALL                             
014400                                        PIC S9(5) COMP-3.                 
014500             07  XX00-4472-SUPTID-SHFT-MM-ALL                             
014600                                        PIC S9(5) COMP-3.                 
014700         03  FILLER OCCURS 30.                                            
014800             07  XX00-4472-TIRFS        PIC S9(11) COMP-3.                
014900             07  XX00-4472-SUPTID-HH    PIC S9(5) COMP-3.                 
015000             07  XX00-4472-SUPTID-MM    PIC S9(7) COMP-3.                 
015100             07  FILLER OCCURS 3.                                         
015200                 11  XX00-4472-SUPTID-SHFT-HH                             
015300                                        PIC S9(5) COMP-3.                 
015400                 11  XX00-4472-SUPTID-SHFT-MM                             
015500                                        PIC S9(7) COMP-3.                 
015600*                                                                         
015700 01  WS00-ORQD.                                                           
015800     05  FILLER                       PIC S9(5)  COMP-3 VALUE +0.         
015900     05  FILLER                       PIC S9(7)  COMP-3 VALUE +0.         
016000     05  FILLER                       PIC S9(5)  COMP-3 VALUE +0.         
016100     05  FILLER                       PIC S9(7)  COMP-3 VALUE +0.         
016200*                                                                         
016300 01  XX00-ORQD.                                                           
016400     05  XX00-ORQD-SUPTID-HH          PIC S9(5)  COMP-3 VALUE +0.         
016500     05  XX00-ORQD-SUPTID-MM          PIC S9(7)  COMP-3 VALUE +0.         
016600     05  XX00-ORQD-SUPTID-TIRFS-HH    PIC S9(5)  COMP-3 VALUE +0.         
016700     05  XX00-ORQD-SUPTID-TIRFS-MM    PIC S9(7)  COMP-3 VALUE +0.         
016800*                                                                         
016900 01  FILLER.                                                              
017000     05  WS-SUPTID-CL4                PIC S9(5)  COMP-3 VALUE +0.         
017100     05  WS-SUPTID-CL7                PIC S9(5)  COMP-3 VALUE +0.         
017200     05  WS-SUPTID-CL8                PIC S9(5)  COMP-3 VALUE +0.         
017300*                                                                         
017400 01  SUPTID-SHFT-TIRFS-00.                                                
017500     05  FILLER                       PIC S9(5)  COMP-3 VALUE +0.         
017600     05  FILLER                       PIC S9(5)  COMP-3 VALUE +0.         
017700     05  FILLER.                                                          
017800         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
017900         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
018000         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
018100         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
018200         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
018300         10  FILLER                   PIC S9(5)  COMP-3 VALUE +0.         
018400 01  SUPTID-SHFT-TIRFS.                                                   
018500     05  WU-SUPTID-HH                 PIC S9(5)  COMP-3.                  
018600     05  WU-SUPTID-MM                 PIC S9(5)  COMP-3.                  
018700     05  FILLER OCCURS 3.                                                 
018800         10  WU-SUPTID-SHFT-HH        PIC S9(5)  COMP-3.                  
018900         10  WU-SUPTID-SHFT-MM        PIC S9(5)  COMP-3.                  
019000*                                                                         
019100     EJECT                                                                
019200 01  FILLER-INDX.                                                         
019300     05  RS-INDX                 PIC S9(3)      COMP-3 VALUE +0.          
019400     05  RT-INDX                 PIC S9(3)      COMP-3 VALUE +0.          
019500     05  RU-INDX                 PIC S9(3)      COMP-3 VALUE +0.          
019600     05  RW-INDX                 PIC S9(3)      COMP-3 VALUE +0.          
019700     05  MD-INDX                 PIC S9(3)      COMP-3 VALUE +0.          
019800     05  RS-INDX-MAX             PIC S9(3)      COMP-3 VALUE +30.         
019900*                                                                         
020000 01  FILLER.                                                              
020100     05  WS-HH                   PIC  S9(7)     COMP-3 VALUE +0.          
020200     05  WS-MM                   PIC  S9(7)     COMP-3 VALUE +0.          
020300     05  WS-MMS                  PIC  S9(7)     COMP-3 VALUE +0.          
020400     05  WS-60                   PIC  S9(7)     COMP-3 VALUE +60.         
020500*                                                                         
020600     05  WS-SUPTID-HHMM          PIC  9(5)V9(2).                          
020700     05  FILLER REDEFINES WS-SUPTID-HHMM.                                 
020800         10  WS-SUPTID-HH            PIC  9(5).                           
020900         10  WS-SUPTID-MM            PIC  9(2).                           
021000*                                                                         
021100     EJECT                                                                
021200 01  W-MOD-RAD.                                                           
021300     05 W-IDPRC-RAD.                                                      
021400        07 W-IDPRCBAS        PIC X(3)         VALUE ZERO.                 
021500        07 W-IDPRCVAR        PIC X            VALUE ZERO.                 
021600     05 W-SUPTID-RAD         PIC Z(4)9        VALUE ZERO.                 
021700     05 W-SUPTID-DAG-RAD     PIC Z(4)9        VALUE ZERO.                 
021800     05 W-SUPTID-RST-RAD     PIC Z(4)9        VALUE ZERO.                 
021900     05 W-SUPTID-REG-RAD     PIC Z(4)9        VALUE ZERO.                 
022000     05 W-SUPTID-UT-RAD      PIC Z(4)9        VALUE ZERO.                 
022100     05 W-SUPTID-KAN-RAD     PIC Z(4)9        VALUE ZERO.                 
022200     05 W-SUPTID-PLMI-RAD    PIC -Z(3)9.9     VALUE ZERO.                 
022300     05 W-SUPTID-META-RAD    PIC Z(2)9.9(2)   VALUE ZERO.                 
022400     05 W-SUPTID-METB-RAD    PIC Z(2)9.9(2)   VALUE ZERO.                 
022500     05 W-SUPTID-METC-RAD    PIC Z(2)9.9(2)   VALUE ZERO.                 
022600     EJECT                                                                
022700 01  GENERELLA-SUBPROGRAM.                                                
022800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
022900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
023100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023500*01 -COPY WMSGINIT                                                        
023600     SKIP3                                                                
023700*01 -COPY WMSGINIT -PRE DC-                                               
023800     EJECT                                                                
023900*   -COPY WORKAREA                                                        
024000     EJECT                                                                
024100*   -COPY WMEDAREA                                                        
024200     EJECT                                                                
024300*   -COPY WDECAREA                                                        
024400     EJECT                                                                
024500*    --- AREA FOR IMS                                                     
024600*                                                                         
024700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024800     SKIP3                                                                
024900*01  MID -COPY W4I38601                                                   
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025200     SKIP3                                                                
025300*01  -COPY WMSGAREA                                                       
025400     EJECT                                                                
025500*    03  MOD -COPY W4O38601   -RED MSG-AREA.                              
025600     EJECT                                                                
025700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025800     SKIP3                                                                
025900*01  -COPY WMFSAREA                                                       
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026200     SKIP3                                                                
026300*                                                                         
026400 01  NYCKLAR-TILL-DLI.                                                    
026500*                                                                         
026600     03  W-4447-WDGXKEY.                                                  
026700         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
026800         07 W-4447-IDDC          PIC X(02).                               
026900         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
027000*                                                                         
027100     03  W-4448-WDGXKEY.                                                  
027200         07 W-4448-IDPRC.                                                 
027300            11 W-4448-IDPRCBAS   PIC X(3).                                
027400            11 W-4448-IDPRCVAR   PIC X(1).                                
027500         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
027600*                                                                         
027700     03  W-4471-WDGXKEY.                                                  
027800         07 FILLER               PIC X(4)    VALUE '4471'.                
027900         07 W-4471-IDDC          PIC X(2).                                
028000         07 W-4471-IDPRC.                                                 
028100            11 W-4471-IDPRCBAS   PIC X(3).                                
028200            11 W-4471-IDPRCVAR   PIC X(1).                                
028300         07 FILLER               PIC X(20)   VALUE LOW-VALUE.             
028400*                                                                         
028500     03  W-4472-KDSEGKEY.                                                 
028600         07 FILLER               PIC X(1)    VALUE '1'.                   
028700*                                                                         
028800     03  W-WDQ3C1KY-MIN.                                                  
028900         07 W-SEQC-MIN-IDDC          PIC X(2).                            
029000         07 W-SEQC-MIN-IDPRCBAS      PIC X(3)  VALUE SPACES.              
029100         07 W-SEQC-MIN-IDPRCVAR      PIC X(1)  VALUE SPACES.              
029200         07 W-SEQC-MIN-FILLER        PIC X(34) VALUE LOW-VALUE.           
029300*                                                                         
029400     03  W-WDQ3C1KY-MAX.                                                  
029500         07 W-SEQC-MAX-IDDC          PIC X(2).                            
029600         07 W-SEQC-MAX-IDPRCBAS      PIC X(3)  VALUE SPACES.              
029700         07 W-SEQC-MAX-IDPRCVAR      PIC X(1)  VALUE SPACES.              
029800         07 W-SEQC-MAX-FILLER        PIC X(34) VALUE HIGH-VALUE.          
029900*                                                                         
030000     03  W-WDQ3C1KY-KDODELST-U       PIC X VALUE 'U'.                     
030100*                                                                         
030200 01  STATUS-WS                   PIC XX.                                  
030300     88  SEGMENT-OK                          VALUE '  '.                  
030400     88  SEGMENT-II                          VALUE 'II'.                  
030500     88  SEGMENT-GE                          VALUE 'GE'.                  
030600     88  SEGMENT-GB                          VALUE 'GB'.                  
030700     SKIP2                                                                
030800 01  4448-STATUS-WS                   PIC XX.                             
030900     88  4448-SEGMENT-OK                          VALUE '  '.             
031000     88  4448-SEGMENT-II                          VALUE 'II'.             
031100     88  4448-SEGMENT-GE                          VALUE 'GE'.             
031200     88  4448-SEGMENT-GB                          VALUE 'GB'.             
031300     SKIP2                                                                
031400 01  GODK-STATUSKODER.                                                    
031500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031600     SKIP3                                                                
031700 01  SSA1                        PIC X(128).                              
031800 01  SSA2                        PIC X(128).                              
031900     EJECT                                                                
032000*    --- IMS FUNKTIONSKODER                                               
032100*01  -COPY W0003                                                          
032200     EJECT                                                                
032300*    ---  DLI INPUT-OUTPUT AREA                                           
032400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032500     SKIP3                                                                
032600 01  DLI-IO-AREA1.                                                        
032700     03  IO-AREA1                PIC X(1200)  VALUE SPACE.                
032800     SKIP3                                                                
032900     03  WLXXKH01 REDEFINES IO-AREA1.                                     
033000*        05  -COPY WDGX4447   -PRE XXKH-                                  
033100*                                                                         
033200     03  WLXXKH11 REDEFINES IO-AREA1.                                     
033300*        05  -COPY WDGX4448   -PRE XXKH-                                  
033400     EJECT                                                                
033500 01  DLI-IO-AREA2.                                                        
033600     03  IO-AREA2                PIC X(1200)  VALUE SPACE.                
033700     SKIP3                                                                
033800     03  WLXXKW01 REDEFINES IO-AREA2.                                     
033900*        05  -COPY WDGX4471   -PRE XXKW-                                  
034000*                                                                         
034100     03  WLXXKW11 REDEFINES IO-AREA2.                                     
034200*        05  -COPY WDGX4472   -PRE XXKW-                                  
034300*                                                                         
034400     03  WLORQD01 REDEFINES IO-AREA2.                                     
034500*        05  -COPY WDQ3C1     -PRE ORQD-                                  
034600*                                                                         
034700     EJECT                                                                
034800 LINKAGE SECTION.                                                         
034900                                                                          
035000*01  -COPY W0009      -PRE MSG-                                           
035100     EJECT                                                                
035200*01  -COPY W0008      -PRE USEA-                                          
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008      -PRE XXKH-                                          
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800*01  -COPY W0008      -PRE XXKW-                                          
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100*01  -COPY W0008      -PRE ORQD-                                          
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXKH-PCB XXKW-PCB             
036500                                   ORQD-PCB.                              
036600 W40386 SECTION.                                                          
036700                                                                          
036800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXKH-PCB XXKW-PCB             
036900                                   ORQD-PCB.                              
037000     PERFORM IMS-GET-MSG                                                  
037100     IF SEGMENT-OK                                                        
037200       PERFORM A-INIT                                                     
037300       PERFORM B-KOLLA-NYCKLAR                                            
037400       IF NYCKLAR-OK                                                      
037500           IF MFS-FIRST                                                   
037600             PERFORM C-FOERSTA-SIDA                                       
037700           ELSE                                                           
037800             IF MFS-NEXT                                                  
037900               PERFORM D-NAESTA-SIDA                                      
038000             ELSE                                                         
038100               PERFORM E-SAMMA-SIDA                                       
038200             END-IF                                                       
038300           END-IF                                                         
038400           IF ALLT-OK                                                     
038500             PERFORM F-LAES-VISA-INFO                                     
038600           END-IF                                                         
038700       END-IF                                                             
038800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38601 + 4                      
038900       PERFORM IMS-INSERT-MSG                                             
039000     END-IF                                                               
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 A-INIT SECTION.                                                          
039700                                                                          
039800     IF MSG-DUBBLA-TRANSKODER                                             
039900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38601                 
040000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
040100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
040200     ELSE                                                                 
040300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I38601                 
040400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
040500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
040600     END-IF                                                               
040700                                                                          
040800     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
040900     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
041000     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
041100                                                                          
041200     MOVE LOW-VALUE        TO MSG-AREA                                    
041300     MOVE '4386'           TO MOD-IDTRANS                                 
041400     MOVE 'W4O386N1'       TO MFS-IDMOD                                   
041500     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
041600                                                                          
041700     IF NOT EGEN-MID                                                      
041800       MOVE SPACE          TO MFS-KDTRTYP                                 
041900       MOVE '7'            TO MFS-IDPFK                                   
042000     END-IF                                                               
042100                                                                          
042200     IF NOT GODK-MID                                                      
042300       MOVE SPACE          TO MID-PFI-TIRFS-01                            
042400                              MID-PFI-TIRFS-02                            
042500     END-IF                                                               
042600                                                                          
042700     IF ENGLISH-TEXT                                                      
042800       MOVE +2             TO SPRAK-IX                                    
042900     ELSE                                                                 
043000       MOVE +1             TO SPRAK-IX                                    
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 B-KOLLA-NYCKLAR SECTION.                                                 
043500                                                                          
043600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
043700     MOVE '013'             TO MSGI-KDCALL                                
043800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043900     MOVE '4386'            TO MSGI-IDTRANS                               
044000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
044100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
044300                                                                          
044400     MOVE JA  TO NYCKLAR-SW                                               
044500     PERFORM MFS-ERASE-MOD-PFI                                            
044600     PERFORM BA-CHCK-KDPRCGRP                                             
044700     PERFORM BB-CHCK-KDPRODKL                                             
044800     PERFORM BE-CHCK-TIRFS                                                
044900     PERFORM BF-CHCK-KDKALK                                               
045000     PERFORM BH-CNT-WRKDAYS                                               
045100     PERFORM BC-CHCK-IDPRC                                                
045200                                                                          
045300     MOVE MSGI-IDDC               TO WS-IDDC                              
045400                                                                          
045500     MOVE ALL '+'           TO DC-MSGI-WMSGINIT                           
045600     MOVE '013'             TO DC-MSGI-KDCALL                             
045700     MOVE WS-IDDC           TO WS-DCUSER-IDDC                             
045800     MOVE WS-DCUSER         TO DC-MSGI-IDUSER                             
046100     MOVE '4386'            TO DC-MSGI-IDTRANS                            
046200     MOVE MSG-LTERM-NAME    TO DC-MSGI-IDLTERM-USER                       
046300     CALL W005INIT USING DC-MSGI-WMSGINIT USEA-PCB                        
046400     MOVE DC-MSGI-TILOKDAT  TO WS-LOCAL-DATE                              
046500                                                                          
046600     PERFORM BG-CHCK-COMBIN                                               
046700     PERFORM BZ-FILL-MOD-PF7                                              
046800     IF NYCKLAR-FEL                                                       
046900       MOVE '401'      TO MED-IDMFSFEL                                    
047000       CALL WMEDKONV USING MED-WMEDAREA                                   
047100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
047200       PERFORM MFS-ERASE-FIELD-UT                                         
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 BA-CHCK-KDPRCGRP SECTION.                                                
047700                                                                          
047800     IF MID-PFI-KDPRCGRP = ALL '+'                                        
047900       MOVE MID-PF7-KDPRCGRP TO WS-PFI-KDPRCGRP                           
048000     ELSE                                                                 
048100       MOVE MID-PFI-KDPRCGRP TO WS-PFI-KDPRCGRP                           
048200       MOVE '7'             TO MFS-IDPFK                                  
048300       MOVE SPACE           TO MFS-KDTRTYP                                
048400     END-IF                                                               
048500     IF WS-PFI-KDPRCGRP = SPACES OR ALL '+'                               
048600        MOVE NEJ TO NYCKLAR-SW                                            
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 BB-CHCK-KDPRODKL SECTION.                                                
049100                                                                          
049200     IF MID-PFI-KDPRODKL-01 = ALL '+'                                     
049300       MOVE MID-PF7-KDPRODKL-01 TO WS-PFI-KDPRODKL-01                     
049400     ELSE                                                                 
049500       MOVE MID-PFI-KDPRODKL-01 TO WS-PFI-KDPRODKL-01                     
049600       MOVE '7'             TO MFS-IDPFK                                  
049700       MOVE SPACE           TO MFS-KDTRTYP                                
049800     END-IF                                                               
049900     IF MID-PFI-KDPRODKL-02 = ALL '+'                                     
050000       MOVE MID-PF7-KDPRODKL-02 TO WS-PFI-KDPRODKL-02                     
050100     ELSE                                                                 
050200       MOVE MID-PFI-KDPRODKL-02 TO WS-PFI-KDPRODKL-02                     
050300       MOVE '7'             TO MFS-IDPFK                                  
050400       MOVE SPACE           TO MFS-KDTRTYP                                
050500     END-IF                                                               
050600     IF (NOT GOOD-PFI-KDPRODKL-01) OR                                     
050700                (NOT GOOD-PFI-KDPRODKL-02)                                
050800        MOVE NEJ TO NYCKLAR-SW                                            
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 BC-CHCK-IDPRC SECTION.                                                   
051300                                                                          
051400     IF MID-PFI-IDPRC = ALL '+'                                           
051500       MOVE MID-PF7-IDPRC TO WS-PFI-IDPRC                                 
051600     ELSE                                                                 
051700       MOVE MID-PFI-IDPRC TO WS-PFI-IDPRC                                 
051800       MOVE '7'          TO MFS-IDPFK                                     
051900       MOVE SPACE        TO MFS-KDTRTYP                                   
052000     END-IF                                                               
052100                                                                          
052200     IF WS-PFI-IDPRCBAS NOT = SPACE                                       
052300        IF WS-PFI-IDPRCVAR = SPACE                                        
052400           MOVE NEJ TO NYCKLAR-SW                                         
052500        END-IF                                                            
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 BE-CHCK-TIRFS SECTION.                                                   
053000                                                                          
053100     IF MID-PFI-TIRFS-01 = ALL '+'                                        
053200       INSPECT MID-PF7-TIRFS-01 REPLACING LEADING SPACE BY ZERO           
053300     ELSE                                                                 
053400       INSPECT MID-PFI-TIRFS-01 REPLACING LEADING SPACE BY ZERO           
053500       IF MID-PFI-TIRFS-01 IS NUMERIC                                     
053600          MOVE MID-PFI-TIRFS-01 TO WS-PFI-TIRFS-01                        
053700       ELSE                                                               
053800          MOVE 000000 TO WS-PFI-TIRFS-01                                  
053900       END-IF                                                             
054000       MOVE '7'          TO MFS-IDPFK                                     
054100       MOVE SPACE        TO MFS-KDTRTYP                                   
054200     END-IF                                                               
054300                                                                          
054400     IF MID-PFI-TIRFS-02 = ALL '+'                                        
054500       INSPECT MID-PF7-TIRFS-02 REPLACING LEADING SPACE BY ZERO           
054600       MOVE MID-PF7-TIRFS-02 TO  WS-PFI-TIRFS-02                          
054700     ELSE                                                                 
054800       INSPECT MID-PFI-TIRFS-02 REPLACING LEADING SPACE BY ZERO           
054900       IF MID-PFI-TIRFS-02 IS NUMERIC                                     
055000          MOVE MID-PFI-TIRFS-02 TO WS-PFI-TIRFS-02                        
055100       ELSE                                                               
055200          MOVE 000000 TO WS-PFI-TIRFS-02                                  
055300       END-IF                                                             
055400       MOVE '7'          TO MFS-IDPFK                                     
055500       MOVE SPACE        TO MFS-KDTRTYP                                   
055600     END-IF                                                               
055700                                                                          
055800     IF WS-PFI-TIRFS-01 = 000000                                          
055900        MOVE WS-LOCAL-DATE TO WS-PFI-TIRFS-01                             
056000     END-IF                                                               
056100                                                                          
056200     IF WS-PFI-TIRFS-02 = 000000                                          
056300        MOVE WS-LOCAL-DATE TO WS-PFI-TIRFS-02                             
056400     END-IF                                                               
056500                                                                          
056600     MOVE WS-PFI-TIRFS-02   TO TMP1-YYMMDD                                
056700     MOVE WS-PFI-TIRFS-01   TO TMP2-YYMMDD                                
056800     PERFORM WY2000P1                                                     
056900     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
057000        MOVE NEJ TO NYCKLAR-SW                                            
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 BF-CHCK-KDKALK SECTION.                                                  
057500                                                                          
057600     IF MID-PFI-KDKALK = ALL '+'                                          
057700       IF MID-PF7-KDKALK IS NUMERIC                                       
057800          MOVE MID-PF7-KDKALK TO WS-PFI-KDKALK                            
057900       ELSE                                                               
058000          MOVE SPRAK-IX       TO WS-PFI-KDKALK                            
058100       END-IF                                                             
058200     ELSE                                                                 
058300       IF MID-PFI-KDKALK IS NUMERIC                                       
058400          MOVE MID-PFI-KDKALK TO WS-PFI-KDKALK                            
058500       ELSE                                                               
058600          MOVE SPRAK-IX       TO WS-PFI-KDKALK                            
058700       END-IF                                                             
058800       MOVE '7'               TO MFS-IDPFK                                
058900       MOVE SPACE             TO MFS-KDTRTYP                              
059000     END-IF                                                               
059100     IF NOT (GOOD-PFI-KDKALK)                                             
059200        MOVE NEJ TO NYCKLAR-SW                                            
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 BG-CHCK-COMBIN SECTION.                                                  
059700                                                                          
059800     IF (WS-PFI-KDPRODKL-01 NOT = ' ')  AND                               
059900                              (WS-PFI-KDPRODKL-02 = ' ')                  
060000        MOVE WS-PFI-KDPRODKL-01 TO WS-PFI-KDPRODKL-02                     
060100     END-IF                                                               
060200     IF (WS-PFI-KDPRODKL-01 = ' ')  AND                                   
060300                             (WS-PFI-KDPRODKL-02 NOT = ' ')               
060400        MOVE WS-PFI-KDPRODKL-02 TO WS-PFI-KDPRODKL-01                     
060500     END-IF                                                               
060600     IF (WS-PFI-KDPRODKL-01 = ' ')  AND                                   
060700                             (WS-PFI-KDPRODKL-02 = ' ')                   
060800        MOVE 'B' TO WS-PFI-KDPRODKL-01                                    
060900        MOVE 'C' TO WS-PFI-KDPRODKL-02                                    
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300 BH-CNT-WRKDAYS SECTION.                                                  
061400                                                                          
061500     MOVE 001     TO WORK-KDCALL                                          
061510     MOVE WS-IDDC TO WORK-KDCALL                                          
061600     MOVE WS-PFI-TIRFS-01 TO WORK-TIAAMMDD-FOM                            
061700     MOVE WS-PFI-TIRFS-02 TO WORK-TIAAMMDD-TOM                            
061800     CALL WORKDAY  USING WORK-KDCALL                                      
061900                         WORK-DATE-AREA                                   
062000                         WORK-KDSVAR                                      
062100     IF (WORK-KDSVAR-FEL)                                                 
062200        MOVE NEJ TO NYCKLAR-SW                                            
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 BZ-FILL-MOD-PF7 SECTION.                                                 
062700                                                                          
062800     IF NOT GODK-MID                                                      
062900        MOVE NEJ TO NYCKLAR-SW                                            
063000     END-IF                                                               
063100                                                                          
063200     MOVE WS-IDDC                 TO MOD-PF7-IDDC                         
063300                                                                          
063400     IF GODK-MID OR NYCKLAR-OK                                            
063500          MOVE WS-PFI-KDPRCGRP    TO MOD-PF7-KDPRCGRP                     
063600          MOVE WS-PFI-KDPRODKL-01 TO MOD-PF7-KDPRODKL-01                  
063700          MOVE WS-PFI-KDPRODKL-02 TO MOD-PF7-KDPRODKL-02                  
063800          MOVE WS-PFI-IDPRC       TO MOD-PF7-IDPRC                        
063900                                                                          
064000          MOVE WS-PFI-TIRFS-01    TO MOD-PF7-TIRFS-01                     
064100                                                                          
064200          MOVE WS-PFI-TIRFS-02    TO MOD-PF7-TIRFS-02                     
064300                                                                          
064400          MOVE WS-PFI-KDKALK      TO MOD-PF7-KDKALK                       
064500     ELSE                                                                 
064600          MOVE MFS-ERASE-FIELD    TO MOD-PF7-KDPRCGRP                     
064700                                     MOD-PF7-KDPRODKL-01                  
064800                                     MOD-PF7-KDPRODKL-02                  
064900          MOVE MFS-ERASE-FIELD    TO MOD-PF7-IDPRC                        
065000                                     MOD-PF7-TIRFS-01                     
065100                                     MOD-PF7-TIRFS-02                     
065200                                     MOD-PF7-KDKALK                       
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 C-FOERSTA-SIDA SECTION.                                                  
065700                                                                          
065800     MOVE '006' TO MED-IDMFSFEL                                           
065900     CALL WMEDKONV USING MED-WMEDAREA                                     
066000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
066100     PERFORM MFS-ERASE-MOD-PFI                                            
066200     PERFORM MFS-ERASE-FIELD-UT                                           
066300     MOVE WS-PFI-IDPRC TO W-4448-IDPRC                                    
066400     MOVE JA TO ALLT-SW                                                   
066500     .                                                                    
066600     EJECT                                                                
066700 D-NAESTA-SIDA SECTION.                                                   
066800                                                                          
066900     MOVE MID-PF8-IDPRC TO W-4448-IDPRC                                   
067000     MOVE JA TO ALLT-SW                                                   
067100                                                                          
067200     IF W-4448-IDPRC = 'SLUT'                                             
067300        MOVE '115' TO MED-IDMFSFEL                                        
067400        CALL WMEDKONV USING MED-WMEDAREA                                  
067500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
067600        PERFORM MFS-ERASE-FIELD-UT                                        
067700        MOVE 'SLUT'     TO MOD-PF8-IDPRC                                  
067800        MOVE NEJ TO ALLT-SW                                               
067900     END-IF                                                               
068000     .                                                                    
068100     EJECT                                                                
068200 E-SAMMA-SIDA SECTION.                                                    
068300                                                                          
068400     IF (MID-PFI-KDPRCGRP = ALL '+') AND                                  
068500            (MID-PFI-KDPRODKL-01 = ALL '+') AND                           
068600            (MID-PFI-KDPRODKL-02 = ALL '+') AND                           
068700                (MID-PFI-IDPRC = ALL '+')                                 
068800       MOVE MID-PFE-IDPRC    TO W-4448-IDPRC                              
068900       MOVE JA  TO ALLT-SW                                                
069000     ELSE                                                                 
069100       MOVE NEJ TO ALLT-SW                                                
069200       MOVE '003' TO MED-IDMFSFEL                                         
069300       CALL WMEDKONV USING MED-WMEDAREA                                   
069400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
069500       PERFORM MFS-NOT-TOUCH-FIELD-IN                                     
069600       PERFORM MFS-NOT-TOUCH-FIELD-UT                                     
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 F-LAES-VISA-INFO SECTION.                                                
070100                                                                          
070200     PERFORM FA-INIT-WSDATA                                               
070300     PERFORM FB-LOC-WDGX4448                                              
070400     MOVE STATUS-WS            TO 4448-STATUS-WS                          
070500     IF SEGMENT-GE                                                        
070600        MOVE '413' TO MED-IDMFSFEL                                        
070700        CALL WMEDKONV USING MED-WMEDAREA                                  
070800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070900        PERFORM MFS-ERASE-FIELD-UT                                        
071000     ELSE                                                                 
071100        MOVE XXKH-4448-IDPRC TO MOD-PFE-IDPRC                             
071200                                MOD-PF8-IDPRC                             
071300        MOVE +1                 TO INDX                                   
071400        PERFORM UNTIL INDX > MAX-INDX OR (NOT 4448-SEGMENT-OK)            
071500          PERFORM FY-CMPT-SCRNLNE                                         
071600        END-PERFORM                                                       
071700        IF 4448-SEGMENT-OK                                                
071800          MOVE '105' TO MED-IDMFSINF                                      
071900          CALL WMEDKONV USING MED-WMEDAREA                                
072000          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
072100          MOVE XXKH-4448-IDPRC    TO MOD-PF8-IDPRC                        
072200        ELSE                                                              
072300          MOVE '106' TO MED-IDMFSINF                                      
072400          CALL WMEDKONV USING MED-WMEDAREA                                
072500          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
072600          MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                            
072700          MOVE 'SLUT'       TO MOD-PF8-IDPRC                              
072800        END-IF                                                            
072900        IF MFS-FIRST                                                      
073000          PERFORM UNTIL (NOT 4448-SEGMENT-OK)                             
073100             PERFORM FY-CMPT-SCRNLNE                                      
073200          END-PERFORM                                                     
073300          PERFORM FZ-TOTAL-LINE                                           
073400        ELSE                                                              
073500          PERFORM MFS-KEEP-TOTAL-LINE                                     
073600        END-IF                                                            
073700     END-IF                                                               
073800     PERFORM UNTIL INDX > MAX-INDX                                        
073900         PERFORM MFS-ERASE-LINE                                           
074000         ADD +1 TO INDX                                                   
074100     END-PERFORM                                                          
074200     .                                                                    
074300     EJECT                                                                
074400 FA-INIT-WSDATA SECTION.                                                  
074500                                                                          
074600     MOVE WS-IDDC         TO W-4447-IDDC                                  
074700                             W-4471-IDDC                                  
074800                             W-SEQC-MIN-IDDC                              
074900                             W-SEQC-MAX-IDDC                              
075000     MOVE WS-PFI-TIRFS-01 TO WS-DB-TIRFS-AAMMDD                           
075100     MOVE 0000            TO WS-DB-TIRFS-HHMM                             
075200     MOVE WS-DB-TIRFS     TO WS-TIRFS01                                   
075300     MOVE WS-PFI-TIRFS-02 TO WS-DB-TIRFS-AAMMDD                           
075400     MOVE 2359            TO WS-DB-TIRFS-HHMM                             
075500     MOVE WS-DB-TIRFS     TO WS-TIRFS02                                   
075600     .                                                                    
075700     EJECT                                                                
075800 FB-LOC-WDGX4448 SECTION.                                                 
075900     IF WS-PFI-IDPRC NOT = SPACES                                         
076000        PERFORM IMS-GU-XXKH-WDGX4448                                      
076100        IF SEGMENT-OK                                                     
076200           IF XXKH-4448-KDPRODKL = 'B' OR 'C'                             
076300              CONTINUE                                                    
076400           ELSE                                                           
076500              MOVE 'GE' TO STATUS-WS                                      
076600           END-IF                                                         
076700        END-IF                                                            
076800     ELSE                                                                 
076900        PERFORM IMS-GU-XXKH-WDGX4447                                      
077000        IF SEGMENT-OK                                                     
077100           IF MFS-FIRST                                                   
077200            PERFORM IMS-GNP-XXKH-WDGX4448                                 
077300           ELSE                                                           
077400            PERFORM IMS-LST-XXKH-WDGX4448                                 
077500           END-IF                                                         
077600           PERFORM S00-GET-WDGX4448                                       
077700        END-IF                                                            
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 FY-CMPT-SCRNLNE SECTION.                                                 
078200                                                                          
078300     MOVE +0                   TO WS-KVARBTID                             
078400                                  WS-KVBEMAN                              
078500     MOVE XXKH-4448-IDPRC      TO WS-FIRST-IDPRC                          
078600     MOVE WS00-ORQD            TO XX00-ORQD                               
078700     MOVE SUPTID-SHFT-TIRFS-00 TO SUPTID-SHFT-TIRFS                       
078800     MOVE NEJ                  TO RAD-INFO-SW                             
078900     PERFORM FYA-INIT-XX00-4472                                           
079000                                                                          
079100     PERFORM FYB-SUM-XXKW-WDGX4472                                        
079200     IF WS-PFI-IDPRC           NOT = SPACES                               
079300         MOVE 'GE'             TO  STATUS-WS                              
079400      ELSE                                                                
079500         PERFORM IMS-GNP-XXKH-WDGX4448                                    
079600         PERFORM S00-GET-WDGX4448                                         
079700     END-IF                                                               
079800     MOVE STATUS-WS            TO 4448-STATUS-WS                          
079900                                                                          
080000     IF RAD-INFO-OK                                                       
080100         PERFORM FYC-SUM-SUPTID-RFS                                       
080200         PERFORM FYD-SUM-ORDQ-WDQ3C1                                      
080300         PERFORM FYE-DISPL-SCRN-LINE                                      
080400         IF INDX > MAX-INDX                                               
080500             CONTINUE                                                     
080600          ELSE                                                            
080700             PERFORM FYF-MOVE-W-RAD                                       
080800             ADD +1            TO INDX                                    
080900             PERFORM FYG-INIT-W-RAD                                       
081000         END-IF                                                           
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 FYA-INIT-XX00-4472 SECTION.                                              
081500                                                                          
081600     MOVE +0  TO  XX00-4472-SUPTID-DAG-HH                                 
081700     MOVE +0  TO  XX00-4472-SUPTID-DAG-MM                                 
081800     MOVE +0  TO  XX00-4472-SUPTID-RFS-HH                                 
081900     MOVE +0  TO  XX00-4472-SUPTID-RFS-MM                                 
082000     MOVE +0  TO  XX00-4472-SUPTID-SHFT-ALL-HH                            
082100     MOVE +0  TO  XX00-4472-SUPTID-SHFT-ALL-MM                            
082200     MOVE +1  TO  RS-INDX                                                 
082300     PERFORM UNTIL RS-INDX > 3                                            
082400        MOVE +0  TO  XX00-4472-SUPTID-SHFT-HH-ALL (RS-INDX)               
082500        MOVE +0  TO  XX00-4472-SUPTID-SHFT-MM-ALL (RS-INDX)               
082600        ADD +1 TO RS-INDX                                                 
082700     END-PERFORM                                                          
082800     MOVE +1  TO  RT-INDX                                                 
082900     PERFORM UNTIL RT-INDX > 30                                           
083000      MOVE +0  TO  XX00-4472-TIRFS (RT-INDX)                              
083100      MOVE +0  TO  XX00-4472-SUPTID-HH (RT-INDX)                          
083200      MOVE +0  TO  XX00-4472-SUPTID-MM (RT-INDX)                          
083300      MOVE +1  TO  RS-INDX                                                
083400      PERFORM UNTIL RS-INDX > 3                                           
083500        MOVE +0  TO  XX00-4472-SUPTID-SHFT-HH (RT-INDX, RS-INDX)          
083600        MOVE +0  TO  XX00-4472-SUPTID-SHFT-MM (RT-INDX, RS-INDX)          
083700        ADD +1 TO RS-INDX                                                 
083800      END-PERFORM                                                         
083900      ADD +1 TO RT-INDX                                                   
084000     END-PERFORM                                                          
084100     MOVE +0  TO  RT-INDX                                                 
084200     MOVE +0  TO  RS-INDX                                                 
084300     .                                                                    
084400     EJECT                                                                
084500 FYB-SUM-XXKW-WDGX4472 SECTION.                                           
084600                                                                          
084700     MOVE XXKH-4448-IDPRC TO W-4471-IDPRC                                 
084800     MOVE XXKH-4448-IDPRC TO W-4448-IDPRC                                 
084900     IF WS-PFI-IDPRC = SPACES                                             
085000        PERFORM IMS-GU-XXKW-WDGX4472                                      
085100        IF SEGMENT-OK                                                     
085200            PERFORM FYBB-TOTAL-WDGX4472                                   
085300            PERFORM FYBA-DATA-WDGX4448                                    
085400        END-IF                                                            
085500     ELSE                                                                 
085600        PERFORM IMS-GU-XXKW-WDGX4472                                      
085700        IF SEGMENT-OK                                                     
085800           PERFORM FYBB-TOTAL-WDGX4472                                    
085900        END-IF                                                            
086000        PERFORM FYBA-DATA-WDGX4448                                        
086100     END-IF                                                               
086200     .                                                                    
086300     EJECT                                                                
086400 FYBA-DATA-WDGX4448 SECTION.                                              
086500                                                                          
086600     MOVE XXKH-4448-KVBEMAN-ORD TO WS-KVBEMAN                             
086700     ADD  XXKH-4448-KVBEMAN-EXT TO WS-KVBEMAN                             
086800     COMPUTE WS-KVARBTID =                                                
086900         WS-KVARBTID + (XXKH-4448-KVARBTID * WS-KVBEMAN)                  
087000     END-COMPUTE                                                          
087100     .                                                                    
087200     EJECT                                                                
087300*                                                                         
087400 FYBB-TOTAL-WDGX4472 SECTION.                                             
087500                                                                          
087600     MOVE JA                    TO  RAD-INFO-SW                           
087700     MOVE XXKW-4472-SUPTID-DAG  TO  WS-SUPTID-HHMM                        
087800     ADD  WS-SUPTID-HH TO XX00-4472-SUPTID-DAG-HH                         
087900     ADD  WS-SUPTID-MM TO XX00-4472-SUPTID-DAG-MM                         
088000     MOVE +1 TO RS-INDX                                                   
088100     PERFORM UNTIL (RS-INDX > RS-INDX-MAX)                                
088200       IF XXKW-4472-TIRFS (RS-INDX) > +0                                  
088300         MOVE XXKW-4472-SUPTID (RS-INDX) TO WS-SUPTID-HHMM                
088400         ADD WS-SUPTID-HH  TO XX00-4472-SUPTID-RFS-HH                     
088500         ADD WS-SUPTID-MM  TO XX00-4472-SUPTID-RFS-MM                     
088600         PERFORM FYBBA-LOC-INDX                                           
088700         ADD WS-SUPTID-HH  TO XX00-4472-SUPTID-HH (RW-INDX)               
088800         ADD WS-SUPTID-MM  TO XX00-4472-SUPTID-MM (RW-INDX)               
088900         MOVE XXKW-4472-TIRFS (RS-INDX) TO                                
089000                                XX00-4472-TIRFS (RW-INDX)                 
089100         PERFORM FYBBB-SUM-SHFT                                           
089200       END-IF                                                             
089300       ADD +1 TO RS-INDX                                                  
089400     END-PERFORM                                                          
089500                                                                          
089600     .                                                                    
089700     EJECT                                                                
089800 FYBBA-LOC-INDX SECTION.                                                  
089900                                                                          
090000     MOVE +1 TO RW-INDX                                                   
090100     PERFORM UNTIL                                                        
090200              XXKW-4472-TIRFS (RS-INDX) =                                 
090300                   XX00-4472-TIRFS (RW-INDX)        OR                    
090400                   XX00-4472-TIRFS (RW-INDX) = ZERO OR                    
090500                   RW-INDX                    > 29                        
090600        ADD +1 TO RW-INDX                                                 
090700     END-PERFORM                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 FYBBB-SUM-SHFT SECTION.                                                  
091100                                                                          
091200     MOVE 1 TO RT-INDX                                                    
091300     PERFORM UNTIL RT-INDX > 3                                            
091400        MOVE XXKW-4472-SUPTID-PRAPP (RS-INDX, RT-INDX)                    
091500                               TO  WS-SUPTID-HHMM                         
091600        ADD WS-SUPTID-HH TO                                               
091700             XX00-4472-SUPTID-SHFT-HH (RW-INDX, RT-INDX)                  
091800        ADD WS-SUPTID-MM TO                                               
091900             XX00-4472-SUPTID-SHFT-MM (RW-INDX, RT-INDX)                  
092000        ADD WS-SUPTID-HH TO                                               
092100             XX00-4472-SUPTID-SHFT-HH-ALL (RT-INDX)                       
092200        ADD WS-SUPTID-MM TO                                               
092300             XX00-4472-SUPTID-SHFT-MM-ALL (RT-INDX)                       
092400        ADD WS-SUPTID-HH TO                                               
092500             XX00-4472-SUPTID-SHFT-ALL-HH                                 
092600        ADD WS-SUPTID-MM TO                                               
092700             XX00-4472-SUPTID-SHFT-ALL-MM                                 
092800        ADD +1 TO RT-INDX                                                 
092900     END-PERFORM                                                          
093000     .                                                                    
093100     EJECT                                                                
093200 FYC-SUM-SUPTID-RFS SECTION.                                              
093300                                                                          
093400     MOVE +1 TO RS-INDX                                                   
093500     PERFORM UNTIL RS-INDX > RS-INDX-MAX                                  
093600       MOVE XX00-4472-TIRFS (RS-INDX)   TO TMP1-YYMMDDHHMM                
093700       MOVE WS-TIRFS01                  TO TMP2-YYMMDDHHMM                
093800       MOVE WS-TIRFS02                  TO TMP3-YYMMDDHHMM                
093900       PERFORM WY2000QB                                                   
094000       IF  TMP1-YYMMDDHHMM >=  TMP2-YYMMDDHHMM                            
094100       AND TMP1-YYMMDDHHMM <=  TMP3-YYMMDDHHMM                            
094200          ADD XX00-4472-SUPTID-HH (RS-INDX) TO WU-SUPTID-HH               
094300          ADD XX00-4472-SUPTID-MM (RS-INDX) TO WU-SUPTID-MM               
094400          MOVE +1 TO RT-INDX                                              
094500          PERFORM UNTIL RT-INDX > 3                                       
094600             ADD XX00-4472-SUPTID-SHFT-HH (RS-INDX, RT-INDX) TO           
094700                           WU-SUPTID-SHFT-HH (RT-INDX)                    
094800             ADD XX00-4472-SUPTID-SHFT-MM (RS-INDX, RT-INDX) TO           
094900                           WU-SUPTID-SHFT-MM (RT-INDX)                    
095000             ADD +1 TO RT-INDX                                            
095100          END-PERFORM                                                     
095200     END-IF                                                               
095300     ADD  +1 TO RS-INDX                                                   
095400     END-PERFORM                                                          
095500     .                                                                    
095600     EJECT                                                                
095700 FYD-SUM-ORDQ-WDQ3C1 SECTION.                                             
095800                                                                          
095900     MOVE WS-FIRST-IDPRC TO W-SEQC-MIN-IDPRCBAS                           
096000                            W-SEQC-MAX-IDPRCBAS                           
096100     MOVE WS-FIRST-IDPRC (4:1) TO W-SEQC-MIN-IDPRCVAR                     
096200                                  W-SEQC-MAX-IDPRCVAR                     
096300     PERFORM IMS-GU-ORQD-WDQ3C1                                           
096400     PERFORM UNTIL (NOT SEGMENT-OK)                                       
096500        MOVE ORQD-SEQC-SUPTID TO  WS-SUPTID-HHMM                          
096600        ADD WS-SUPTID-HH      TO  XX00-ORQD-SUPTID-HH                     
096700        ADD WS-SUPTID-MM      TO  XX00-ORQD-SUPTID-MM                     
096800        MOVE ORQD-SEQC-DARFS (3:10)  TO TMP1-YYMMDDHHMM                   
096900        MOVE WS-TIRFS01        TO TMP2-YYMMDDHHMM                         
097000        MOVE WS-TIRFS02        TO TMP3-YYMMDDHHMM                         
097100        PERFORM WY2000QB                                                  
097200        IF  TMP1-YYMMDDHHMM >= TMP2-YYMMDDHHMM                            
097300        AND TMP1-YYMMDDHHMM <= TMP3-YYMMDDHHMM                            
097400           MOVE ORQD-SEQC-SUPTID TO  WS-SUPTID-HHMM                       
097500           ADD WS-SUPTID-HH  TO  XX00-ORQD-SUPTID-TIRFS-HH                
097600           ADD WS-SUPTID-MM  TO  XX00-ORQD-SUPTID-TIRFS-MM                
097700        END-IF                                                            
097800        PERFORM IMS-GN-ORQD-WDQ3C1                                        
097900     END-PERFORM                                                          
098000     .                                                                    
098100     EJECT                                                                
098200 FYE-DISPL-SCRN-LINE SECTION.                                             
098300                                                                          
098400     PERFORM FYEA-ARBTID-SUM                                              
098500     PERFORM FYEB-COLMN-123                                               
098600     PERFORM FYEC-COLMN-456                                               
098700     PERFORM FYED-COLMN-78                                                
098800     PERFORM FYEE-COLMN-9AB                                               
098900     .                                                                    
099000     EJECT                                                                
099100 FYEA-ARBTID-SUM SECTION.                                                 
099200                                                                          
099300     MOVE XX00-4472-SUPTID-DAG-MM      TO   WS-MMS                        
099400     PERFORM FYEAA-ARBTID-SUM-HH                                          
099500     ADD WS-HH                         TO XX00-4472-SUPTID-DAG-HH         
099600     MOVE XX00-4472-SUPTID-RFS-MM      TO   WS-MMS                        
099700     PERFORM FYEAA-ARBTID-SUM-HH                                          
099800     ADD WS-HH                         TO XX00-4472-SUPTID-RFS-HH         
099900     MOVE XX00-4472-SUPTID-SHFT-ALL-MM TO   WS-MMS                        
100000     PERFORM FYEAA-ARBTID-SUM-HH                                          
100100     ADD WS-HH                  TO XX00-4472-SUPTID-SHFT-ALL-HH           
100200     MOVE XX00-ORQD-SUPTID-MM          TO   WS-MMS                        
100300     PERFORM FYEAA-ARBTID-SUM-HH                                          
100400     ADD WS-HH                         TO XX00-ORQD-SUPTID-HH             
100500     MOVE XX00-ORQD-SUPTID-TIRFS-MM    TO   WS-MMS                        
100600     PERFORM FYEAA-ARBTID-SUM-HH                                          
100700     ADD WS-HH                  TO XX00-ORQD-SUPTID-TIRFS-HH              
100800     MOVE WU-SUPTID-MM                 TO   WS-MMS                        
100900     PERFORM FYEAA-ARBTID-SUM-HH                                          
101000     ADD WS-HH                         TO WU-SUPTID-HH                    
101100                                                                          
101200     MOVE +1 TO RT-INDX                                                   
101300     PERFORM UNTIL RT-INDX > 3                                            
101400         MOVE XX00-4472-SUPTID-SHFT-MM-ALL (RT-INDX)  TO  WS-MMS          
101500                                                                          
101600         PERFORM FYEAA-ARBTID-SUM-HH                                      
101700         ADD WS-HH      TO XX00-4472-SUPTID-SHFT-HH-ALL (RT-INDX)         
101800         MOVE WS-MM     TO XX00-4472-SUPTID-SHFT-MM-ALL (RT-INDX)         
101900         MOVE WU-SUPTID-SHFT-MM (RT-INDX)  TO   WS-MMS                    
102000                                                                          
102100         PERFORM FYEAA-ARBTID-SUM-HH                                      
102200         ADD WS-HH           TO WU-SUPTID-SHFT-HH (RT-INDX)               
102300         MOVE WS-MM          TO WU-SUPTID-SHFT-MM (RT-INDX)               
102400         MOVE XX00-4472-SUPTID-MM (RT-INDX)  TO  WS-MMS                   
102500                                                                          
102600         PERFORM FYEAA-ARBTID-SUM-HH                                      
102700         ADD WS-HH           TO XX00-4472-SUPTID-HH (RT-INDX)             
102800         MOVE WS-MM          TO XX00-4472-SUPTID-MM (RT-INDX)             
102900         ADD +1 TO RT-INDX                                                
103000     END-PERFORM                                                          
103100     .                                                                    
103200     EJECT                                                                
103300 FYEAA-ARBTID-SUM-HH SECTION.                                             
103400                                                                          
103500     DIVIDE WS-MMS BY WS-60 GIVING WS-HH                                  
103600     COMPUTE WS-MMS = WS-MMS - (WS-HH * 60)                               
103700     MOVE WS-MMS TO WS-MM                                                 
103800     .                                                                    
103900     EJECT                                                                
104000 FYEB-COLMN-123 SECTION.                                                  
104100                                                                          
104200     MOVE W-4471-IDPRC            TO   W-IDPRC-RAD                        
104300     MOVE XX00-4472-SUPTID-RFS-HH TO   W-SUPTID-RAD                       
104400     MOVE XX00-4472-SUPTID-DAG-HH TO   W-SUPTID-DAG-RAD                   
104500     ADD  XX00-4472-SUPTID-RFS-HH TO   WS-SUPTID-TOT (02)                 
104600     ADD  XX00-4472-SUPTID-DAG-HH TO   WS-SUPTID-TOT (03)                 
104700     .                                                                    
104800     EJECT                                                                
104900 FYEC-COLMN-456 SECTION.                                                  
105000                                                                          
105100     MOVE XX00-4472-SUPTID-DAG-HH TO WS-SUPTID-CL4                        
105200     IF WS-PFI-KDKALK = 1                                                 
105300        SUBTRACT XX00-ORQD-SUPTID-TIRFS-HH FROM WS-SUPTID-CL4             
105400        SUBTRACT XX00-4472-SUPTID-SHFT-ALL-HH                             
105500                                           FROM WS-SUPTID-CL4             
105600     ELSE                                                                 
105700        SUBTRACT XX00-ORQD-SUPTID-HH     FROM WS-SUPTID-CL4               
105800        SUBTRACT XX00-4472-SUPTID-SHFT-ALL-HH                             
105900                                         FROM WS-SUPTID-CL4               
106000     END-IF                                                               
106100     IF WS-SUPTID-CL4           <  ZERO                                   
106200         MOVE ZERO              TO WS-SUPTID-CL4                          
106300     END-IF                                                               
106400     MOVE WS-SUPTID-CL4         TO W-SUPTID-RST-RAD                       
106500     ADD  WS-SUPTID-CL4         TO WS-SUPTID-TOT (04)                     
106600     IF WS-PFI-KDKALK = 1                                                 
106700        COMPUTE WS-SUPTID-REG-RAD = WU-SUPTID-HH                -         
106800                                   XX00-4472-SUPTID-SHFT-ALL-HH -         
106900                                   XX00-ORQD-SUPTID-TIRFS-HH              
107000        IF WS-SUPTID-REG-RAD < 0                                          
107100           MOVE 0                 TO WS-SUPTID-REG-RAD                    
107200        END-IF                                                            
107300        MOVE WS-SUPTID-REG-RAD   TO  W-SUPTID-REG-RAD                     
107400        ADD  WS-SUPTID-REG-RAD   TO  WS-SUPTID-TOT (05)                   
107500     ELSE                                                                 
107600        COMPUTE WS-SUPTID-REG-RAD = XX00-4472-SUPTID-RFS-HH     -         
107700                                   XX00-4472-SUPTID-SHFT-ALL-HH -         
107800                                   XX00-ORQD-SUPTID-HH                    
107900        IF WS-SUPTID-REG-RAD < 0                                          
108000           MOVE 0                 TO WS-SUPTID-REG-RAD                    
108100        END-IF                                                            
108200        MOVE WS-SUPTID-REG-RAD    TO W-SUPTID-REG-RAD                     
108300        ADD  WS-SUPTID-REG-RAD    TO WS-SUPTID-TOT (05)                   
108400     END-IF                                                               
108500     IF WS-PFI-KDKALK = 1                                                 
108600       MOVE XX00-ORQD-SUPTID-TIRFS-HH TO  W-SUPTID-UT-RAD                 
108700       ADD  XX00-ORQD-SUPTID-TIRFS-HH TO  WS-SUPTID-TOT (06)              
108800     ELSE                                                                 
108900       MOVE XX00-ORQD-SUPTID-HH TO  W-SUPTID-UT-RAD                       
109000       ADD  XX00-ORQD-SUPTID-HH TO  WS-SUPTID-TOT (06)                    
109100     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400 FYED-COLMN-78 SECTION.                                                   
109500                                                                          
109600     COMPUTE WS-SUPTID-CL7 =                                              
109700            (WS-KVARBTID * WORK-KVWORKD)                                  
109800     END-COMPUTE                                                          
109900     MOVE WS-SUPTID-CL7      TO   W-SUPTID-KAN-RAD                        
110000     ADD  WS-SUPTID-CL7      TO   WS-SUPTID-TOT (07)                      
110100     IF WS-PFI-KDKALK = 1                                                 
110200        COMPUTE WS-SUPTID-CL8 = WS-SUPTID-CL7 - WU-SUPTID-HH              
110300     ELSE                                                                 
110400        COMPUTE WS-SUPTID-CL8 = WS-SUPTID-CL7 -                           
110500                                XX00-4472-SUPTID-DAG-HH                   
110600     END-IF                                                               
110700     MOVE WS-SUPTID-CL8      TO   W-SUPTID-PLMI-RAD                       
110800     ADD  WS-SUPTID-CL8      TO   WS-SUPTID-TOT (08)                      
110900     .                                                                    
111000     EJECT                                                                
111100 FYEE-COLMN-9AB SECTION.                                                  
111200                                                                          
111300     IF WS-PFI-KDKALK = 1                                                 
111400        PERFORM FYEEA-9AB-KALK1                                           
111500     ELSE                                                                 
111600        PERFORM FYEEB-9AB-KALKX                                           
111700     END-IF                                                               
111800     .                                                                    
111900     EJECT                                                                
112000 FYEEA-9AB-KALK1 SECTION.                                                 
112100                                                                          
112200     MOVE WU-SUPTID-SHFT-HH (1) TO WS-SUPTID-HH                           
112300     MOVE WU-SUPTID-SHFT-MM (1) TO WS-SUPTID-MM                           
112400     MOVE WS-SUPTID-HHMM        TO W-SUPTID-META-RAD                      
112500     ADD  WU-SUPTID-SHFT-HH (1) TO WS-SUPTID-TOT (9)                      
112600     ADD  WU-SUPTID-SHFT-MM (1) TO WS-SUPTID-TMM (9)                      
112700     MOVE WU-SUPTID-SHFT-HH (2) TO WS-SUPTID-HH                           
112800     MOVE WU-SUPTID-SHFT-MM (2) TO WS-SUPTID-MM                           
112900     MOVE WS-SUPTID-HHMM        TO W-SUPTID-METB-RAD                      
113000     ADD  WU-SUPTID-SHFT-HH (2) TO WS-SUPTID-TOT (10)                     
113100     ADD  WU-SUPTID-SHFT-MM (2) TO WS-SUPTID-TMM (10)                     
113200     MOVE WU-SUPTID-SHFT-HH (3) TO WS-SUPTID-HH                           
113300     MOVE WU-SUPTID-SHFT-MM (3) TO WS-SUPTID-MM                           
113400     MOVE WS-SUPTID-HHMM        TO W-SUPTID-METC-RAD                      
113500     ADD  WU-SUPTID-SHFT-HH (3) TO WS-SUPTID-TOT (11)                     
113600     ADD  WU-SUPTID-SHFT-MM (3) TO WS-SUPTID-TMM (11)                     
113700     .                                                                    
113800     EJECT                                                                
113900 FYEEB-9AB-KALKX SECTION.                                                 
114000                                                                          
114100     MOVE XX00-4472-SUPTID-SHFT-HH-ALL (1) TO WS-SUPTID-HH                
114200     MOVE XX00-4472-SUPTID-SHFT-MM-ALL (1) TO WS-SUPTID-MM                
114300     MOVE WS-SUPTID-HHMM      TO W-SUPTID-META-RAD                        
114400     ADD  XX00-4472-SUPTID-SHFT-HH-ALL (1)                                
114500                                    TO WS-SUPTID-TOT (9)                  
114600     ADD  XX00-4472-SUPTID-SHFT-MM-ALL (1)                                
114700                                    TO WS-SUPTID-TMM (9)                  
114800     MOVE XX00-4472-SUPTID-SHFT-HH-ALL (2) TO WS-SUPTID-HH                
114900     MOVE XX00-4472-SUPTID-SHFT-MM-ALL (2) TO WS-SUPTID-MM                
115000     MOVE WS-SUPTID-HHMM      TO W-SUPTID-METB-RAD                        
115100     ADD  XX00-4472-SUPTID-SHFT-HH-ALL (2)                                
115200                                    TO WS-SUPTID-TOT (10)                 
115300     ADD  XX00-4472-SUPTID-SHFT-MM-ALL (2)                                
115400                                    TO WS-SUPTID-TMM (10)                 
115500     MOVE XX00-4472-SUPTID-SHFT-HH-ALL (3) TO WS-SUPTID-HH                
115600     MOVE XX00-4472-SUPTID-SHFT-MM-ALL (3) TO WS-SUPTID-MM                
115700     MOVE WS-SUPTID-HHMM      TO W-SUPTID-METC-RAD                        
115800     ADD  XX00-4472-SUPTID-SHFT-HH-ALL (3)                                
115900                                    TO WS-SUPTID-TOT (11)                 
116000     ADD  XX00-4472-SUPTID-SHFT-MM-ALL (3)                                
116100                                    TO WS-SUPTID-TMM (11)                 
116200     .                                                                    
116300     EJECT                                                                
116400 FYF-MOVE-W-RAD      SECTION.                                             
116500                                                                          
116600     MOVE W-IDPRC-RAD          TO MOD-IDPRC-RAD        (INDX)             
116700     MOVE W-SUPTID-RAD         TO MOD-SUPTID-RAD       (INDX)             
116800     MOVE W-SUPTID-DAG-RAD     TO MOD-SUPTID-DAG-RAD   (INDX)             
116900     MOVE W-SUPTID-RST-RAD     TO MOD-SUPTID-RST-RAD   (INDX)             
117000     MOVE W-SUPTID-REG-RAD     TO MOD-SUPTID-REG-RAD   (INDX)             
117100     MOVE W-SUPTID-UT-RAD      TO MOD-SUPTID-UT-RAD    (INDX)             
117200     MOVE W-SUPTID-KAN-RAD     TO MOD-SUPTID-KAN-RAD   (INDX)             
117300     MOVE W-SUPTID-PLMI-RAD    TO MOD-SUPTID-PLMI-RAD  (INDX)             
117400     MOVE W-SUPTID-META-RAD    TO MOD-SUPTID-META-RAD  (INDX)             
117500     MOVE W-SUPTID-METB-RAD    TO MOD-SUPTID-METB-RAD  (INDX)             
117600     MOVE W-SUPTID-METC-RAD    TO MOD-SUPTID-METC-RAD  (INDX)             
117700     .                                                                    
117800     EJECT                                                                
117900 FYG-INIT-W-RAD      SECTION.                                             
118000                                                                          
118100     MOVE ZERO                 TO W-IDPRC-RAD                             
118200                                  W-SUPTID-RAD                            
118300                                  W-SUPTID-DAG-RAD                        
118400                                  W-SUPTID-RST-RAD                        
118500                                  W-SUPTID-REG-RAD                        
118600                                  W-SUPTID-UT-RAD                         
118700                                  W-SUPTID-KAN-RAD                        
118800                                  W-SUPTID-PLMI-RAD                       
118900                                  W-SUPTID-META-RAD                       
119000                                  W-SUPTID-METB-RAD                       
119100                                  W-SUPTID-METC-RAD                       
119200     .                                                                    
119300     EJECT                                                                
119400 FZ-TOTAL-LINE SECTION.                                                   
119500                                                                          
119600       MOVE WS-SUPTID-TOT (2)    TO  MOD-SUPTID-TOT                       
119700       MOVE WS-SUPTID-TOT (3)    TO  MOD-SUPTID-DAG-TOT                   
119800       MOVE WS-SUPTID-TOT (4)    TO  MOD-SUPTID-RST-TOT                   
119900       MOVE WS-SUPTID-TOT (5)    TO  MOD-SUPTID-REG-TOT                   
120000       MOVE WS-SUPTID-TOT (6)    TO  MOD-SUPTID-UT-TOT                    
120100       MOVE WS-SUPTID-TOT (7)    TO  MOD-SUPTID-KAN-TOT                   
120200       MOVE WS-SUPTID-TOT (8)    TO  MOD-SUPTID-PLMI-TOT                  
120300       IF WS-SUPTID-TOT (2) = +0                                          
120400          IF WS-SUPTID-TOT (3) = +0                                       
120500             IF WS-SUPTID-TOT (4) = +0                                    
120600                IF WS-SUPTID-TOT (5) = +0                                 
120700                   MOVE WS-SUPTID-TOT (7) TO                              
120800                                     MOD-SUPTID-PLMI-TOT                  
120900                END-IF                                                    
121000             END-IF                                                       
121100          END-IF                                                          
121200       END-IF                                                             
121300       DIVIDE WS-60 INTO WS-SUPTID-TMM (9)                                
121400                              GIVING WS-HH REMAINDER WS-MM                
121500       ADD WS-HH TO WS-SUPTID-TOT (9)                                     
121600       MOVE WS-SUPTID-TOT (9) TO WS-SUPTID-HH                             
121700       MOVE WS-MM TO WS-SUPTID-MM                                         
121800       MOVE WS-SUPTID-HHMM       TO  MOD-SUPTID-META-TOT                  
121900       DIVIDE WS-60 INTO WS-SUPTID-TMM (10)                               
122000                              GIVING WS-HH REMAINDER WS-MM                
122100       ADD WS-HH TO WS-SUPTID-TOT (10)                                    
122200       MOVE WS-SUPTID-TOT (10) TO WS-SUPTID-HH                            
122300       MOVE WS-MM TO WS-SUPTID-MM                                         
122400       MOVE WS-SUPTID-HHMM       TO  MOD-SUPTID-METB-TOT                  
122500       DIVIDE WS-60 INTO WS-SUPTID-TMM (11)                               
122600                              GIVING WS-HH REMAINDER WS-MM                
122700       ADD WS-HH TO WS-SUPTID-TOT (11)                                    
122800       MOVE WS-SUPTID-TOT (11) TO WS-SUPTID-HH                            
122900       MOVE WS-MM TO WS-SUPTID-MM                                         
123000       MOVE WS-SUPTID-HHMM       TO  MOD-SUPTID-METC-TOT                  
123100       .                                                                  
123200    EJECT                                                                 
123300 S00-GET-WDGX4448 SECTION.                                                
123400          PERFORM UNTIL                                                   
123500              (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-01)                   
123600         OR   (XXKH-4448-KDPRODKL = WS-PFI-KDPRODKL-02)                   
123700         OR   (SEGMENT-GE)                                                
123800                 PERFORM IMS-GNP-XXKH-WDGX4448                            
123900         END-PERFORM                                                      
124000     .                                                                    
124100     SKIP2                                                                
124200 MFS-ERASE-MOD-PFI SECTION.                                               
124300                                                                          
124400     MOVE MFS-ERASE-FIELD TO MOD-PFI-KDPRCGRP                             
124500                             MOD-PFI-KDPRODKL-01                          
124600                             MOD-PFI-KDPRODKL-02                          
124700     MOVE MFS-ERASE-FIELD TO MOD-PFI-IDPRC                                
124800                             MOD-PFI-TIRFS-01                             
124900                             MOD-PFI-TIRFS-02                             
125000                             MOD-PFI-KDKALK                               
125100                             MOD-PFI-IDDC                                 
125200     .                                                                    
125300     EJECT                                                                
125400*                                                                         
125500 MFS-ERASE-FIELD-UT SECTION.                                              
125600                                                                          
125700     MOVE +1 TO INDX                                                      
125800     PERFORM UNTIL INDX > MAX-INDX                                        
125900        PERFORM MFS-ERASE-LINE                                            
126000        ADD +1 TO INDX                                                    
126100     END-PERFORM                                                          
126200     MOVE MFS-ERASE-FIELD TO MOD-PFE-IDPRC                                
126300                             MOD-PF8-IDPRC                                
126400     .                                                                    
126500     EJECT                                                                
126600 MFS-ERASE-LINE SECTION.                                                  
126700           MOVE MFS-ERASE-FIELD TO MOD-IDPRC-RAD (INDX)                   
126800                                   MOD-SUPTID-RAD (INDX)                  
126900                                   MOD-SUPTID-DAG-RAD (INDX)              
127000                                   MOD-SUPTID-RST-RAD (INDX)              
127100                                   MOD-SUPTID-REG-RAD (INDX)              
127200                                   MOD-SUPTID-UT-RAD (INDX)               
127300                                   MOD-SUPTID-KAN-RAD (INDX)              
127400           MOVE MFS-ERASE-FIELD TO MOD-SUPTID-PLMI-RAD (INDX)             
127500                                   MOD-SUPTID-META-RAD (INDX)             
127600                                   MOD-SUPTID-METB-RAD (INDX)             
127700                                   MOD-SUPTID-METC-RAD (INDX)             
127800     .                                                                    
127900     EJECT                                                                
128000 MFS-NOT-TOUCH-FIELD-UT SECTION.                                          
128100                                                                          
128200     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
128300         MOD-PF7-KDPRCGRP      MOD-PF7-KDPRODKL-01                        
128400         MOD-PF7-KDPRODKL-02   MOD-PF7-IDPRC                              
128500         MOD-PF7-TIRFS-01      MOD-PF7-TIRFS-02                           
128600         MOD-PF7-KDKALK        MOD-PFE-IDPRC                              
128700         MOD-PF8-IDPRC                                                    
128800     MOVE +1 TO INDX                                                      
128900     PERFORM UNTIL INDX > MAX-INDX                                        
129000        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPRC-RAD (INDX)               
129100                                   MOD-SUPTID-RAD (INDX)                  
129200                                   MOD-SUPTID-DAG-RAD (INDX)              
129300                                   MOD-SUPTID-RST-RAD (INDX)              
129400                                   MOD-SUPTID-REG-RAD (INDX)              
129500                                   MOD-SUPTID-UT-RAD (INDX)               
129600                                   MOD-SUPTID-KAN-RAD (INDX)              
129700       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SUPTID-PLMI-RAD (INDX)          
129800                                   MOD-SUPTID-META-RAD (INDX)             
129900                                   MOD-SUPTID-METB-RAD (INDX)             
130000                                   MOD-SUPTID-METC-RAD (INDX)             
130100       ADD +1 TO INDX                                                     
130200     END-PERFORM                                                          
130300     PERFORM MFS-KEEP-TOTAL-LINE                                          
130400     .                                                                    
130500     EJECT                                                                
130600 MFS-KEEP-TOTAL-LINE SECTION.                                             
130700                                                                          
130800     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
130900          MOD-SUPTID-TOT          MOD-SUPTID-DAG-TOT                      
131000          MOD-SUPTID-RST-TOT      MOD-SUPTID-REG-TOT                      
131100          MOD-SUPTID-UT-TOT       MOD-SUPTID-KAN-TOT                      
131200          MOD-SUPTID-PLMI-TOT     MOD-SUPTID-META-TOT                     
131300          MOD-SUPTID-METB-TOT     MOD-SUPTID-METC-TOT                     
131400     .                                                                    
131500     EJECT                                                                
131600 MFS-NOT-TOUCH-FIELD-IN SECTION.                                          
131700                                                                          
131800     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
131900          MOD-PFI-KDPRCGRP     MOD-PFI-KDPRODKL-01                        
132000          MOD-PFI-KDPRODKL-02  MOD-PFI-IDPRC                              
132100          MOD-PFI-TIRFS-01     MOD-PFI-TIRFS-02                           
132200          MOD-PFI-KDKALK       MOD-PF7-KDKALK                             
132300          MOD-PF8-IDPRC        MOD-PFE-IDPRC                              
132400     .                                                                    
132500     EJECT                                                                
132600 IMS-GET-MSG SECTION.                                                     
132700                                                                          
132800     MOVE '  QC' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
133000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300     SKIP3                                                                
133400 IMS-INSERT-MSG SECTION.                                                  
133500                                                                          
133600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
133700       MOVE '0' TO MFS-KDHUVOMR                                           
133800     END-IF                                                               
133900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
134000     MOVE SPACE TO GODK-STATUSKODER                                       
134100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
134200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     EJECT                                                                
134600 IMS-LST-XXKH-WDGX4448 SECTION.                                           
134700                                                                          
134800     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY                          
134900                    '&KDPRCGRP =' WS-PFI-KDPRCGRP ')'                     
135000          DELIMITED BY SIZE INTO SSA1                                     
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
135300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     EJECT                                                                
135700 IMS-GU-ORQD-WDQ3C1 SECTION.                                              
135800                                                                          
135900     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
136000                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX                          
136100                    '&KDODELST =' W-WDQ3C1KY-KDODELST-U ')'               
136200          DELIMITED BY SIZE INTO SSA1                                     
136300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
136400     CALL CBLTDLI USING GU  ORQD-PCB DLI-IO-AREA2 SSA1                    
136500     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     EJECT                                                                
136900 IMS-GN-ORQD-WDQ3C1 SECTION.                                              
137000                                                                          
137100     STRING 'WLORQD01(WDQ3C1KY>=' W-WDQ3C1KY-MIN                          
137200                    '&WDQ3C1KY<=' W-WDQ3C1KY-MAX                          
137300                    '&KDODELST =' W-WDQ3C1KY-KDODELST-U ')'               
137400          DELIMITED BY SIZE INTO SSA1                                     
137500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
137600     CALL CBLTDLI USING GN  ORQD-PCB DLI-IO-AREA2 SSA1                    
137700     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
137800     PERFORM IMS-STATUSKONTROLL                                           
137900     .                                                                    
138000     EJECT                                                                
138100 IMS-GNP-XXKH-WDGX4448 SECTION.                                           
138200                                                                          
138300     STRING 'WLXXKH11(KDPRCGRP= ' WS-PFI-KDPRCGRP ')'                     
138400          DELIMITED BY SIZE INTO SSA1                                     
138500     MOVE '  GE' TO GODK-STATUSKODER                                      
138600     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA1 SSA1                    
138700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
138800     PERFORM IMS-STATUSKONTROLL                                           
138900     .                                                                    
139000                                                                          
139100 IMS-GU-XXKH-WDGX4447 SECTION.                                            
139200                                                                          
139300     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
139400          DELIMITED BY SIZE INTO SSA1                                     
139500     MOVE '  GE' TO GODK-STATUSKODER                                      
139600     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA1 SSA1                     
139700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
139800     PERFORM IMS-STATUSKONTROLL                                           
139900     .                                                                    
140000                                                                          
140100 IMS-GU-XXKH-WDGX4448 SECTION.                                            
140200                                                                          
140300     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
140400          DELIMITED BY SIZE INTO SSA1                                     
140500     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
140600          DELIMITED BY SIZE INTO SSA2                                     
140700     MOVE '  GE' TO GODK-STATUSKODER                                      
140800     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA1 SSA1 SSA2               
140900     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
141000     PERFORM IMS-STATUSKONTROLL                                           
141100     .                                                                    
141200     EJECT                                                                
141300 IMS-GU-XXKW-WDGX4472 SECTION.                                            
141400                                                                          
141500     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY ')'                      
141600          DELIMITED BY SIZE INTO SSA1                                     
141700     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY  ')'                    
141800          DELIMITED BY SIZE INTO SSA2                                     
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GU  XXKW-PCB DLI-IO-AREA2 SSA1 SSA2               
142100     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-STATUSKONTROLL SECTION.                                              
142600                                                                          
142700     SET STATUS-IX TO 1                                                   
142800     SEARCH GODK-STATUS                                                   
142900       AT END CALL FELLOG                                                 
143000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
143100     END-SEARCH                                                           
143200     .                                                                    
143300     EJECT                                                                
143400*    -COPY WY2000P1                                                       
143500     EJECT                                                                
143600*    -COPY WY2000QB                                                       
